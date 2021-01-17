use super::{state::State, Stats, Suggestion};
use crate::{finesse::FinesseFinder, place::PlaceFinder, shape::ShapeTable, Config, Input};
use std::{
    collections::HashMap,
    sync::{mpsc, Arc, RwLock},
};

use super::b_star::{Search, Step};

// Analysis handle

// Re-exports
pub use super::b_star::MoveId;

/// A handle to a blockfish analysis running in the background.
pub struct Analysis {
    moves: HashMap<MoveId, Move>,
    trace_inputs: Box<TraceInputsFn>,
    stats: Arc<RwLock<Option<Stats>>>,
    rx: mpsc::Receiver<Msg>,
    all_tx: Option<mpsc::Sender<Suggestion>>,
}

/// Indicates that the analysis has finished and no new updates to any moves will happen.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AnalysisDone;

/// Holds the latest information about a move.
#[derive(Debug, Eq, PartialEq)]
struct Move {
    iteration: usize,
    rating: i64,
    trace: Vec<usize>,
}

/// Message type sent from worker thread to `Analysis` handle.
#[derive(Debug, Eq, PartialEq)]
struct Msg {
    changed_move_id: Option<MoveId>,
    mov: Move,
}

/// Used by the worker thread to send information to the `Analysis` handle.
struct AnalysisSink {
    stats: Arc<RwLock<Option<Stats>>>,
    tx: mpsc::SyncSender<Msg>,
}

impl Analysis {
    /// Constructs a `(sink, handle)` pair. The analysis handle will use `trace_inputs` as
    /// the algorithm for computing inputs from a trace.
    fn new(trace_inputs: impl Fn(&[usize]) -> Vec<Input> + Send + 'static) -> (AnalysisSink, Self) {
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
                all_tx: None,
                stats,
                rx,
            },
        )
    }

    /// Processes a message recieved from the worker thread.
    fn recv(&mut self, msg: Msg) -> Option<MoveId> {
        // send to all-suggestions channel if listening
        if let Some(all_tx) = self.all_tx.as_ref() {
            let inputs = (self.trace_inputs)(&msg.mov.trace);
            let rating = msg.mov.rating;
            if all_tx.send(Suggestion { inputs, rating }).is_err() {
                log::warn!("all-suggestions channel dropped");
                self.all_tx = None;
            }
        }

        // update moves
        if let Some(move_id) = msg.changed_move_id {
            self.moves.insert(move_id, msg.mov);
        }

        msg.changed_move_id
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
        (lhs.rating, lhs.iteration).cmp(&(rhs.rating, rhs.iteration))
    }

    /// Polls the analysis for any progress. Returns `Ok(Some(m))` if move `m`'s rating
    /// changed. Returns `Ok(None)` if no progress was made since the last poll. Returns
    /// `Err(AnalysisDone)` if the analysis is over.
    pub fn poll(&mut self) -> Result<Option<MoveId>, AnalysisDone> {
        loop {
            match self.rx.try_recv() {
                Ok(msg) => {
                    if let Some(move_id) = self.recv(msg) {
                        return Ok(Some(move_id));
                    }
                }
                Err(mpsc::TryRecvError::Empty) => return Ok(None),
                Err(mpsc::TryRecvError::Disconnected) => return Err(AnalysisDone),
            }
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

    /// Finishes the analysis after first setting the collected stats to `stats`.
    fn finish(self, stats: Stats) {
        if let Ok(mut s) = self.stats.write() {
            *s = Some(stats);
        }
    }
}

// Analysis thread

fn analysis(shtb: Arc<ShapeTable>, cfg: Config, root: State, sink: AnalysisSink) {
    let start_time = std::time::Instant::now();
    let mut iteration = 0;
    let mut global_min = std::i64::MAX;

    let mut search = Search::new(&shtb, cfg.parameters);
    search.start(root);

    while search.node_count() < cfg.search_limit {
        let msg;
        match search.step() {
            Ok(Step::RatingChanged {
                move_id,
                rating,
                trace,
            }) => {
                iteration += 1;
                log::debug!(
                    "{:<2?} --> {:>3?}{} iter {}",
                    move_id,
                    rating,
                    if rating < global_min { "*" } else { " " },
                    iteration
                );
                global_min = std::cmp::min(rating, global_min);
                msg = Some(Msg {
                    changed_move_id: Some(move_id),
                    mov: Move {
                        iteration,
                        rating,
                        trace,
                    },
                });
            }

            Ok(Step::SequenceRejected { trace, rating }) => {
                iteration += 1;
                msg = Some(Msg {
                    changed_move_id: None,
                    mov: Move {
                        iteration,
                        rating,
                        trace,
                    },
                });
            }

            Ok(Step::Other) => {
                msg = None;
            }

            Err(_) => break,
        }

        if let Some(msg) = msg {
            if !sink.send(msg) {
                log::warn!("handle disconnected mid-analysis");
                return;
            }
        }
    }

    sink.finish(Stats {
        iterations: iteration,
        nodes: search.node_count(),
        time_taken: std::time::Instant::now() - start_time,
    });
}

// Computing inputs

type TraceInputsFn = dyn Fn(&[usize]) -> Vec<Input> + Send;

fn reconstruct_inputs(shtb: &ShapeTable, state0: State, trace: &[usize]) -> Vec<Input> {
    let mut pfind = PlaceFinder::new(&shtb);
    let mut ffind = FinesseFinder::new();
    let mut state = state0;
    let mut inputs = vec![];
    for &idx in trace {
        let pl = state
            .placements(&mut pfind)
            .find(|pl| pl.idx == idx)
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
pub fn spawn(
    shtb: Arc<ShapeTable>,
    cfg: Config,
    root: State,
    all_suggestions_tx: Option<mpsc::Sender<Suggestion>>,
) -> Analysis {
    let trace_inputs = {
        let shtb = shtb.clone();
        let state0 = root.clone();
        move |t: &[usize]| reconstruct_inputs(&shtb, state0.clone(), t)
    };
    let (sink, mut handle) = Analysis::new(trace_inputs);
    handle.all_tx = all_suggestions_tx;
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
        assert!(sink.send(Msg {
            changed_move_id: Some(MoveId::n(6)),
            mov
        }));
        assert_eq!(handle.poll(), Ok(Some(MoveId::n(6))));
        assert_eq!(handle.suggestion(MoveId::n(6), 0).rating, 1234);
        assert_eq!(handle.poll(), Ok(None));
        sink.finish(Stats::default());
        assert_eq!(handle.poll(), Err(AnalysisDone));
    }

    fn example_analysis(sink: AnalysisSink) {
        assert!(sink.send(Msg {
            changed_move_id: Some(MoveId::n(6)),
            mov: Move {
                iteration: 1,
                rating: 1234,
                trace: vec![6, 7, 8],
            }
        }));
        assert!(sink.send(Msg {
            changed_move_id: Some(MoveId::n(7)),
            mov: Move {
                iteration: 2,
                rating: 1233,
                trace: vec![7, 8, 9, 10],
            }
        }));
        assert!(sink.send(Msg {
            changed_move_id: Some(MoveId::n(6)),
            mov: Move {
                iteration: 3,
                rating: 1233,
                trace: vec![6, 7, 9],
            }
        }));
        assert!(sink.send(Msg {
            changed_move_id: None,
            mov: Move {
                iteration: 3,
                rating: 1239,
                trace: vec![6, 7],
            }
        }));
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
            handle.suggestion(MoveId::n(6), std::usize::MAX),
            Suggestion {
                rating: 1233,
                inputs: vec![Input::HD; 3],
            }
        );
        assert_eq!(
            handle.suggestion(MoveId::n(7), std::usize::MAX),
            Suggestion {
                rating: 1233,
                inputs: vec![Input::HD; 4],
            }
        );
        assert_eq!(
            handle.suggestion(MoveId::n(7), 1).inputs,
            vec![Input::HD; 1]
        );
        assert_eq!(
            handle.suggestion(MoveId::n(7), 2).inputs,
            vec![Input::HD; 2]
        );
    }

    #[test]
    fn test_analysis_cmp() {
        use std::cmp::Ordering::*;
        let (sink, mut handle) = Analysis::new(spam_hd_traces);
        example_analysis(sink);
        handle.wait();
        assert_eq!(handle.cmp(MoveId::n(6), MoveId::n(6)), Equal);
        assert_eq!(handle.cmp(MoveId::n(6), MoveId::n(7)), Greater);
        assert_eq!(handle.cmp(MoveId::n(7), MoveId::n(6)), Less);
    }

    #[test]
    fn test_analysis_statistics() {
        let (sink, handle) = Analysis::new(spam_hd_traces);
        assert_eq!(handle.stats(), None);
        let s = Stats {
            iterations: 1,
            nodes: 2,
            time_taken: std::time::Duration::from_millis(300),
        };
        sink.finish(s.clone());
        assert_eq!(handle.stats(), Some(s));
    }

    #[test]
    fn test_analysis_all_suggestions() {
        let (sink, mut handle) = Analysis::new(spam_hd_traces);
        let (all_tx, all_rx) = mpsc::channel();
        handle.all_tx = Some(all_tx);
        example_analysis(sink);
        handle.wait();
        std::mem::drop(handle); // drop all_tx
        assert_eq!(
            all_rx.iter().collect::<Vec<_>>(),
            vec![
                Suggestion {
                    rating: 1234,
                    inputs: vec![Input::HD; 3],
                },
                Suggestion {
                    rating: 1233,
                    inputs: vec![Input::HD; 4],
                },
                Suggestion {
                    rating: 1233,
                    inputs: vec![Input::HD; 3],
                },
                Suggestion {
                    rating: 1239,
                    inputs: vec![Input::HD; 2],
                },
            ]
        );
    }

    #[test]
    fn test_analysis_is_send() {
        let (_, handle) = Analysis::new(spam_hd_traces);
        // only needs to typecheck
        std::thread::spawn(move || handle);
    }
}
