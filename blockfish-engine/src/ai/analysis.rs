use super::{a_star::Search, state::State, Stats, Suggestion};
use crate::{finesse::FinesseFinder, place::PlaceFinder, shape::ShapeTable, Config, Input};
use std::{
    collections::HashMap,
    sync::{mpsc, Arc, RwLock},
};

// Analysis handle

/// A handle to a blockfish analysis running in the background.
pub struct Analysis {
    moves: HashMap<MoveId, Move>,
    trace_inputs: Box<TraceInputsFn>,
    stats: Arc<RwLock<Option<Stats>>>,
    rx: Rx,
}

/// References a move discovered by the analysis. The input sequence and rating for the
/// move may change as the analysis progresses.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct MoveId(usize);

/// Condition indicating that the analysis has finished and no new updates to any moves
/// will happen.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AnalysisDone;

/// Holds information pertaining to a given move.
#[derive(Debug, Eq, PartialEq)]
struct Move {
    iteration: usize,
    rating: i64,
    trace: Vec<usize>,
}

/// Message type sent from worker thread to `Analysis` handle.
#[derive(Debug, Eq, PartialEq)]
struct Msg(MoveId, Move);

type Rx = mpsc::Receiver<Msg>;
type Tx = mpsc::SyncSender<Msg>;

/// Handle used by the worker thread to send information back to the handle.
#[derive(Clone)]
struct AnalysisSink {
    stats: Arc<RwLock<Option<Stats>>>,
    tx: Tx,
}

impl Analysis {
    /// Constructs a `(sink, handle)` pair. The analysis handle will use `trace_inputs` as
    /// the algorithm for computing inputs from a trace.
    fn new<TraceInputs>(trace_inputs: TraceInputs) -> (AnalysisSink, Self)
    where
        TraceInputs: Fn(&[usize]) -> Vec<Input> + 'static,
    {
        let (tx, rx) = mpsc::sync_channel(256);
        let stats = Arc::new(RwLock::new(None));
        (
            AnalysisSink {
                stats: stats.clone(),
                tx,
            },
            Analysis {
                moves: HashMap::with_capacity(128),
                trace_inputs: Box::new(trace_inputs),
                stats,
                rx,
            },
        )
    }

    /// Processes a message recieved from the worker thread.
    fn recv(&mut self, msg: Msg) -> MoveId {
        self.moves.insert(msg.0, msg.1);
        msg.0
    }

    /// Returns all known moves in arbitrary order. To get the best move, use `.min_by()`
    /// in conjunction with `Analysis::cmp`.
    pub fn all_moves<'a>(&'a self) -> impl Iterator<Item = MoveId> + 'a {
        self.moves.keys().cloned()
    }

    /// Compares two moves. The `Less` move is preferred. Returns `Equal` if and only if
    /// `lhs == rhs`; therefore this comparison is a total order with no absolute ties.
    pub fn cmp(&self, lhs: MoveId, rhs: MoveId) -> std::cmp::Ordering {
        let lhs = self.moves.get(&lhs).expect("invalid id");
        let rhs = self.moves.get(&rhs).expect("invalid id");
        // settle ties by using the one that was discovered in an earlier iteration.
        lhs.rating
            .cmp(&rhs.rating)
            .then(lhs.iteration.cmp(&rhs.iteration))
    }

    /// Polls the analysis for any progress. Returns `Ok(Some(m))` if move `m`'s rating
    /// changed. Returns `Ok(None)` if no progress was made since the last poll. Returns
    /// `Err(AnalysisDone)` if the analysis is over.
    pub fn poll(&mut self) -> Result<Option<MoveId>, AnalysisDone> {
        match self.rx.try_recv() {
            Ok(msg) => Ok(Some(self.recv(msg))),
            Err(mpsc::TryRecvError::Empty) => Ok(None),
            Err(mpsc::TryRecvError::Disconnected) => Err(AnalysisDone),
        }
    }

    /// Blocks until the analysis thread finishes. This is a non-spinning version of
    /// `while !self.poll().is_err() {}`.
    pub fn wait(&mut self) {
        while let Ok(msg) = self.rx.recv() {
            self.recv(msg);
        }
    }

    /// Returns the `Suggestion` for the given move, containing at most `len`
    /// placements. `len` should be `std::usize::MAX` in order to get the inputs for the
    /// entire sequence. `len` may be `0` to just get the move's rating.
    pub fn suggestion(&self, m_id: MoveId, len: usize) -> Suggestion {
        let mov = self.moves.get(&m_id).expect("invalid id");
        let len = std::cmp::min(len, mov.trace.len());
        Suggestion {
            inputs: (self.trace_inputs)(&mov.trace[..len]),
            rating: mov.rating,
        }
    }

    /// Returns the statistics gathered about the analysis, if any. Should be `Some` only
    /// after the analysis finishes.
    pub fn stats(&self) -> Option<Stats> {
        self.stats.read().map_or(None, |s| s.clone())
    }
}

impl AnalysisSink {
    /// Tries to send `msg` to the analysis handle. Returns `false` if it failed because
    /// the handle was dropped.
    fn send(&self, msg: Msg) -> bool {
        self.tx.send(msg).is_ok()
    }

    fn finish(self, stats: Stats) {
        if let Ok(mut s) = self.stats.write() {
            *s = Some(stats);
        }
    }
}

// Analysis thread

fn analysis(shtb: Arc<ShapeTable>, cfg: Config, root: State, sink: AnalysisSink) {
    let start_time = std::time::Instant::now();

    let mut min_scores = HashMap::with_capacity(128);
    let mut global_min = std::i64::MAX;
    let mut depth_count = Vec::with_capacity(16);
    let mut iters = 0;
    let mut search = Search::new(&shtb, cfg.parameters, cfg.search_limit, root);

    while let Some(node) = search.next() {
        iters += 1;

        if log::log_enabled!(log::Level::Debug) {
            let depth = node.trace().count();
            if depth_count.len() <= depth {
                depth_count.resize(depth + 1, 0usize);
            }
            depth_count[depth] += 1;
        }

        let m_id = match node.trace().nth(0) {
            // move ID is determined by the first placement in the trace
            Some(idx) => MoveId(idx),
            None => continue,
        };

        // update the min entry
        let score = node.score();
        let min_score = min_scores.entry(m_id).or_insert(std::i64::MAX);
        if score < *min_score {
            log::debug!(
                "#{:<2} --> {:>3?}{} iter {}",
                m_id.0,
                score,
                if score < global_min { "*" } else { " " },
                iters
            );

            *min_score = score;
            if score < global_min {
                global_min = score;
            }

            // send msg back to analysis handle on every update
            let mov = Move {
                iteration: iters,
                rating: score,
                trace: node.trace().collect(),
            };
            if !sink.send(Msg(m_id, mov)) {
                log::error!("analysis disconnected");
                return;
            }
        }

        // stop when memory limit hit
        if search.node_count() >= cfg.search_limit {
            break;
        }
    }

    log::debug!("ran {} iterations of A*", iters);
    for (depth, &count) in depth_count.iter().enumerate() {
        log::debug!("  nodes at depth {}: {}", depth, count);
    }

    sink.finish(Stats {
        iterations: iters,
        fringe_nodes: search.node_count(),
        time_taken: std::time::Instant::now() - start_time,
    });
}

// Computing inputs

type TraceInputsFn = dyn Fn(&[usize]) -> Vec<Input>;

fn reconstruct_inputs(shtb: &ShapeTable, state0: State, trace: &[usize]) -> Vec<Input> {
    let mut pfind = PlaceFinder::new(&shtb);
    let mut ffind = FinesseFinder::new();
    let mut state = state0;
    let mut inputs = vec![];
    for &idx in trace {
        let pl = state
            .placements(&mut pfind)
            .nth(idx)
            .expect("trace idx out of range");
        if pl.did_hold {
            inputs.push(Input::Hold);
        }
        inputs.extend(
            ffind
                .find(state.matrix(), pl.shape, pl.normal())
                .expect("finesse finder failed"),
        );
        inputs.push(Input::HD);
        state.place(&pl);
    }
    inputs
}

// Putting it all together

/// Spawns a new analysis, returning a handle to it.
pub fn spawn(shtb: Arc<ShapeTable>, cfg: Config, root: State) -> Analysis {
    let trace_inputs = {
        let shtb = shtb.clone();
        let state0 = root.clone();
        move |t: &[usize]| reconstruct_inputs(&shtb, state0.clone(), t)
    };
    let (sink, handle) = Analysis::new(trace_inputs);
    std::thread::spawn(move || analysis(shtb, cfg, root, sink));
    handle
}

#[cfg(test)]
mod test {
    use super::*;

    fn spam_hd_traces(trace: &[usize]) -> Vec<Input> {
        trace.iter().map(|_| Input::HD).collect()
    }

    #[test]
    fn test_analysis_poll() {
        let (sink, mut handle) = Analysis::new(spam_hd_traces);
        assert_eq!(handle.poll(), Ok(None));
        let mov = Move {
            iteration: 1,
            rating: 1234,
            trace: vec![6, 7, 8],
        };
        assert!(sink.send(Msg(MoveId(6), mov)));
        assert_eq!(handle.poll(), Ok(Some(MoveId(6))));
        assert_eq!(handle.suggestion(MoveId(6), 0).rating, 1234);
        assert_eq!(handle.poll(), Ok(None));
        sink.finish(Stats::default());
        assert_eq!(handle.poll(), Err(AnalysisDone));
    }

    fn example_analysis(sink: AnalysisSink) {
        let mov1 = Move {
            iteration: 1,
            rating: 1234,
            trace: vec![6, 7, 8],
        };
        let mov2 = Move {
            iteration: 2,
            rating: 1233,
            trace: vec![7, 8, 9, 10],
        };
        let mov3 = Move {
            iteration: 3,
            rating: 1233,
            trace: vec![6, 7, 9],
        };
        assert!(sink.send(Msg(MoveId(6), mov1)));
        assert!(sink.send(Msg(MoveId(7), mov2)));
        assert!(sink.send(Msg(MoveId(6), mov3)));
    }

    #[test]
    fn test_analysis_wait() {
        let (sink, mut handle) = Analysis::new(spam_hd_traces);
        assert_eq!(handle.poll(), Ok(None));
        example_analysis(sink);
        handle.wait();
        assert_eq!(handle.poll(), Err(AnalysisDone));
    }

    #[test]
    fn test_analysis_suggestion() {
        let (sink, mut handle) = Analysis::new(spam_hd_traces);
        example_analysis(sink);
        handle.wait();
        assert_eq!(
            handle.suggestion(MoveId(6), std::usize::MAX),
            Suggestion {
                rating: 1233,
                inputs: vec![Input::HD; 3],
            }
        );
        assert_eq!(
            handle.suggestion(MoveId(7), std::usize::MAX),
            Suggestion {
                rating: 1233,
                inputs: vec![Input::HD; 4],
            }
        );
        assert_eq!(handle.suggestion(MoveId(7), 1).inputs, vec![Input::HD; 1]);
        assert_eq!(handle.suggestion(MoveId(7), 2).inputs, vec![Input::HD; 2]);
    }

    #[test]
    fn test_analysis_cmp() {
        use std::cmp::Ordering::*;
        let (sink, mut handle) = Analysis::new(spam_hd_traces);
        example_analysis(sink);
        handle.wait();
        assert_eq!(handle.cmp(MoveId(6), MoveId(6)), Equal);
        assert_eq!(handle.cmp(MoveId(6), MoveId(7)), Greater);
        assert_eq!(handle.cmp(MoveId(7), MoveId(6)), Less);
    }

    #[test]
    fn test_analysis_statistics() {
        let (sink, handle) = Analysis::new(spam_hd_traces);
        assert_eq!(handle.stats(), None);
        let s = Stats {
            iterations: 1,
            fringe_nodes: 2,
            time_taken: std::time::Duration::from_millis(300),
        };
        sink.finish(s.clone());
        assert_eq!(handle.stats(), Some(s));
    }
}
