mod a_star;
mod node;
mod place;
mod score;

use self::{
    a_star::AStar,
    node::{Node, State},
    place::placements,
};
use crate::{
    finesse,
    shape::{srs, ShapeTable},
    BasicMatrix, Color, Input,
};

use std::{
    cell::Cell,
    convert::TryInto,
    sync::{atomic, mpsc, Arc},
};
use thiserror::Error;

// Config

pub use score::ScoreParams;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Config {
    pub search_limit: usize,
    pub scoring: ScoreParams,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            search_limit: 50_000,
            scoring: ScoreParams::default(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SuggestionFilter {
    All,
    LocalBest,
    GlobalBest,
}

impl Default for SuggestionFilter {
    fn default() -> Self {
        SuggestionFilter::All
    }
}

#[derive(Debug, Error)]
pub enum ParseConfigError {
    #[error("invalid integer format")]
    Int(#[from] std::num::ParseIntError),
    #[error("invalid score parameters: {0}")]
    ScoreParams(#[from] score::ParseScoreParamsError),
    #[error("expected '<heap-size>' or '<heap-size>/<score-params>'")]
    Other,
}

impl std::str::FromStr for Config {
    type Err = ParseConfigError;
    fn from_str(s: &str) -> Result<Self, ParseConfigError> {
        let mut ss = s.split('/');
        let search_limit = ss.next().ok_or(ParseConfigError::Other)?;
        let search_limit = search_limit.parse::<usize>()? * 1_000;
        let scoring = match ss.next() {
            Some(s) => s
                .split(',')
                .map(|s| s.parse())
                .collect::<Result<Vec<_>, _>>()?
                .as_slice()
                .try_into()?,
            None => ScoreParams::default(),
        };
        if ss.next().is_some() {
            Err(ParseConfigError::Other)
        } else {
            Ok(Config {
                search_limit,
                scoring,
            })
        }
    }
}

impl std::fmt::Display for Config {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let search_limit = (self.search_limit + 999) / 1_000;
        write!(f, "{}/", search_limit)?;
        for (i, v) in self.scoring.to_vec().into_iter().enumerate() {
            if i > 0 {
                f.write_str(",")?;
            }
            write!(f, "{}", v)?;
        }
        Ok(())
    }
}

// Input / output types

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Snapshot {
    pub hold: Option<Color>,
    pub queue: Vec<Color>,
    pub matrix: BasicMatrix,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Suggestion {
    pub inputs: Vec<Input>,
    pub score: i64,
}

impl Suggestion {
    /// Returns the max depth of this suggestion (number of pieces placed).
    pub fn depth(&self) -> usize {
        self.inputs.iter().filter(|&&inp| inp == Input::HD).count()
    }

    /// Returns the initial inputs up to the given depth; that is, the number of `HD`'s in
    /// the result will be less than or equal to the depth. Particularly, if `depth` is
    /// `0` it will return all the inputs before the first drop.
    pub fn inputs_prefix<'a>(&'a self, depth: usize) -> impl Iterator<Item = Input> + 'a {
        self.inputs.iter().scan(0, move |n, &inp| {
            if inp == Input::HD {
                if *n >= depth {
                    return None;
                } else {
                    *n += 1;
                }
            }
            Some(inp)
        })
    }
}

// AI

/// An instance of the Blockfish AI. Holds engine configuration and can be used to spawn
/// an analysis.
pub struct AI {
    config: Config,
    shape_table: Arc<ShapeTable>,
    filter: SuggestionFilter,
}

/// A handle to a blockfish analysis running in the background. Can be polled for new
/// suggestions from the engine. Particularly, an `Analysis` is an `Iterator` of
/// `Suggestion`s.
pub struct Analysis {
    // recieve end of communication channel with the worker thread.
    rx: Rx,
    // may hold an incoming message if a nonblocking poll was performed.
    inbox: Cell<Option<Msg>>,
    // holds statistics about the worker thread, after the thread finishes
    stats: AtomicStats,
    // function to compute inputs for a trace
    get_trace_inputs: Box<TraceInputsFn>,
}

type TraceInputsFn = dyn FnMut(&[usize], &mut Vec<Input>);

const ANALYSIS_CHANNEL_CAPACITY: usize = 512;

// Message type passed from worker threads to Analysis's.
#[derive(Clone, Debug)]
struct Msg {
    score: i64,
    trace: Vec<usize>,
}

type Rx = mpsc::Receiver<Msg>;
type Tx = mpsc::SyncSender<Msg>;
type AtomicStats = Arc<(atomic::AtomicBool, atomic::AtomicUsize, atomic::AtomicUsize)>;

impl AI {
    /// Constructs a new Blockfish AI instance with the given engine configuration.
    pub fn new(config: Config) -> Self {
        Self {
            config,
            shape_table: srs().into(),
            filter: SuggestionFilter::default(),
        }
    }

    pub fn config(&self) -> Config {
        self.config.clone()
    }

    /// Configure the filter rule for the next analysis.
    pub fn set_suggestion_filter(&mut self, filter: SuggestionFilter) {
        self.filter = filter;
    }

    /// Begins a new analysis of `snapshot`, returning a handle to it.
    pub fn analyze(&mut self, snapshot: Snapshot) -> Analysis {
        Analysis::start(
            self.config.clone(),
            self.filter,
            self.shape_table.clone(),
            snapshot.into(),
        )
    }
}

impl Analysis {
    /// Constructs a new `Analysis` that communicates with a worker thread via `rx`.
    fn new<F>(rx: Rx, f: F) -> Self
    where
        F: FnMut(&[usize], &mut Vec<Input>) + 'static,
    {
        Self {
            rx,
            inbox: Cell::new(None),
            stats: Arc::new((false.into(), 0.into(), 0.into())),
            get_trace_inputs: Box::new(f),
        }
    }

    /// Starts an analysis of initial state `init_state`, returning a handle to it.
    fn start(cfg: Config, filter: SuggestionFilter, shtb: Arc<ShapeTable>, init: State) -> Self {
        let (tx, rx) = mpsc::sync_channel(ANALYSIS_CHANNEL_CAPACITY);
        let analysis = Analysis::new(rx, reconstruct_traces_from(shtb.clone(), init.clone()));
        let stats = analysis.stats.clone();
        std::thread::spawn(move || Analysis::work(cfg, filter, shtb, Node::new(init), tx, stats));
        analysis
    }

    /// Performs the work of an analysis, starting with root node `Node`. New potential
    /// suggestions are pushed to channel `tx`, and final stats are updated to `stats`.
    fn work(
        cfg: Config,
        filter: SuggestionFilter,
        shtb: Arc<ShapeTable>,
        root: Node,
        tx: Tx,
        stats: AtomicStats,
    ) {
        assert!(root.depth() == 0);
        let mut search = AStar::new(&shtb, &cfg.scoring, cfg.search_limit, root);
        let mut root_score = vec![];
        let mut best = None::<Node>;
        let mut iters = 0;

        while let Some(node) = search.next() {
            iters += 1;

            let root_idx = match node.trace().nth(0) {
                Some(idx) => idx,
                None => continue,
            };
            if root_score.len() <= root_idx {
                root_score.resize(root_idx + 1, std::i64::MAX);
            }

            let score = node.score();
            let suggest = match filter {
                SuggestionFilter::All => true,
                SuggestionFilter::GlobalBest => false,
                SuggestionFilter::LocalBest => {
                    if score < root_score[root_idx] {
                        root_score[root_idx] = score;
                        true
                    } else {
                        false
                    }
                }
            };

            if suggest {
                let trace = node.trace().collect();
                if tx.send(Msg { score, trace }).is_err() {
                    log::warn!("analysis dropped; quitting early");
                    break;
                }
            }

            if best
                .as_ref()
                .map(|best| score < best.score())
                .unwrap_or(true)
            {
                log::debug!(
                    "root {} @ depth {}, score => {} (iteration #{})",
                    root_idx,
                    node.depth(),
                    score,
                    iters,
                );
                best = Some(node);
            }

            if search.node_count() >= cfg.search_limit {
                break;
            }
        }

        if filter == SuggestionFilter::GlobalBest {
            if let Some(node) = best {
                let trace = node.trace().collect();
                let score = node.score();
                if tx.send(Msg { score, trace }).is_err() {
                    log::warn!("analysis dropped; best could not be sent");
                }
            }
        }

        log::info!("ran {} iterations of A*", iters);
        let nodes = search.node_count() + iters;
        stats.1.store(nodes, atomic::Ordering::Release);
        stats.2.store(iters, atomic::Ordering::Release);
        stats.0.store(true, atomic::Ordering::Release);
        std::mem::drop(tx);
    }

    /// Computes the list of inputs to perform the sequence given by `root_idx`.
    fn inputs(&mut self, trace: &[usize]) -> Vec<Input> {
        let mut inputs = Vec::with_capacity(trace.len() * 8);
        (self.get_trace_inputs)(&trace, &mut inputs);
        inputs
    }

    /// If the worker process has finished (as indicated by `next()` returning `None`),
    /// returns `Some((node_count, iterations))`. Otherwise returns `None`.
    pub fn stats(&self) -> Option<(usize, usize)> {
        if self.stats.0.load(atomic::Ordering::Acquire) {
            let node_count = self.stats.1.load(atomic::Ordering::Acquire);
            let iters = self.stats.2.load(atomic::Ordering::Acquire);
            Some((node_count, iters))
        } else {
            None
        }
    }

    /// Returns `true` if calling `next()` would block while waiting for the next
    /// suggestion, or `false` if `next()` will immediately return a result.  When the AI
    /// process is finished, this function will always return `false`.
    pub fn would_block(&self) -> bool {
        let msg = match self.inbox.take() {
            Some(m) => m,
            None => match self.rx.try_recv() {
                Ok(m) => m,
                Err(e) => return e == mpsc::TryRecvError::Empty,
            },
        };
        self.inbox.set(Some(msg));
        false
    }
}

impl Iterator for Analysis {
    type Item = Suggestion;
    fn next(&mut self) -> Option<Suggestion> {
        let msg = match self.inbox.take() {
            Some(m) => m,
            None => match self.rx.recv() {
                Ok(m) => m,
                Err(_) => return None,
            },
        };
        Some(Suggestion {
            score: msg.score,
            inputs: self.inputs(&msg.trace),
        })
    }
}

fn reconstruct_traces_from(
    shtb: Arc<ShapeTable>,
    init_state: State,
) -> impl FnMut(&[usize], &mut Vec<Input>) {
    let mut ffind = finesse::FinesseFinder::new();
    move |trace, inputs| {
        let mut state = init_state.clone();
        for &idx in trace {
            // get the same placement by its index
            let pl = placements(&shtb, &state)
                .nth(idx)
                .expect("root_idx out of bounds");

            // compute inputs for this placements (via ffind)
            if pl.did_hold {
                inputs.push(Input::Hold);
            }
            inputs.extend(
                // TODO: ffind takes an out-parameter
                ffind
                    .find(state.matrix(), pl.shape, pl.normal())
                    .expect("finesse not found"),
            );
            inputs.push(Input::HD);

            // apply the placement
            state.place(&pl);
        }
    }
}

//////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_config() {
        assert_eq!(
            "15".parse::<Config>().unwrap(),
            Config {
                search_limit: 15_000,
                scoring: ScoreParams::default()
            }
        );
        assert_eq!(
            "15/1,2,3,4".parse::<Config>().unwrap(),
            Config {
                search_limit: 15_000,
                scoring: ScoreParams {
                    row_factor: 1,
                    piece_estimate_factor: 2,
                    i_dependency_factor: 3,
                    piece_penalty: 4,
                },
            }
        );
    }

    #[test]
    fn test_display_config() {
        assert_eq!(
            format!(
                "{}",
                Config {
                    search_limit: 15_000,
                    scoring: ScoreParams {
                        row_factor: 1,
                        piece_estimate_factor: 2,
                        i_dependency_factor: 3,
                        piece_penalty: 4,
                    },
                }
            ),
            "15/1,2,3,4"
        );
    }

    #[test]
    fn test_suggestion_depth() {
        use Input::*;
        let sugg = Suggestion {
            inputs: vec![Left, Left, CW, HD, Hold, Left, HD, Right, Right, HD],
            score: 1234,
        };
        assert_eq!(sugg.depth(), 3);
        let sugg = Suggestion {
            inputs: vec![Left, Left, CW, HD],
            score: 1234,
        };
        assert_eq!(sugg.depth(), 1);
    }

    #[test]
    fn test_suggestion_inputs_prefix() {
        use Input::*;
        let sugg = Suggestion {
            inputs: vec![Left, Left, CW, HD, Hold, Left, HD, Right, Right, HD],
            score: 1234,
        };
        assert_eq!(sugg.inputs_prefix(0).collect::<Vec<_>>(), [Left, Left, CW]);
        assert_eq!(
            sugg.inputs_prefix(1).collect::<Vec<_>>(),
            [Left, Left, CW, HD, Hold, Left]
        );
        assert_eq!(
            sugg.inputs_prefix(2).collect::<Vec<_>>(),
            [Left, Left, CW, HD, Hold, Left, HD, Right, Right]
        );
        assert_eq!(
            sugg.inputs_prefix(3).collect::<Vec<_>>(),
            [Left, Left, CW, HD, Hold, Left, HD, Right, Right, HD]
        );
        assert_eq!(
            sugg.inputs_prefix(4).collect::<Vec<_>>(),
            [Left, Left, CW, HD, Hold, Left, HD, Right, Right, HD]
        );
    }

    fn spam_hd_traces(trace: &[usize], inputs: &mut Vec<Input>) {
        inputs.extend(trace.iter().map(|_| Input::HD));
    }

    #[test]
    fn test_analysis_would_block() {
        let (tx, rx) = mpsc::sync_channel(0);
        let analysis = Analysis::new(rx, spam_hd_traces);
        assert!(analysis.would_block());
        std::mem::drop(tx);
        assert!(!analysis.would_block());
    }

    #[test]
    fn test_analysis_next() {
        let (tx, rx) = mpsc::sync_channel(1);
        let mut analysis = Analysis::new(rx, spam_hd_traces);
        assert!(analysis.would_block());

        let m = Msg {
            trace: vec![1, 2],
            score: 5,
        };
        tx.send(m).unwrap();
        assert!(!analysis.would_block());
        assert_eq!(
            analysis.next(),
            Some(Suggestion {
                score: 5,
                inputs: vec![Input::HD, Input::HD],
            })
        );
        assert!(analysis.would_block());

        let m = Msg {
            trace: vec![1, 2, 3],
            score: 8,
        };
        tx.send(m).unwrap();
        assert_eq!(
            analysis.next(),
            Some(Suggestion {
                score: 8,
                inputs: vec![Input::HD; 3],
            })
        );

        std::mem::drop(tx);
        assert_eq!(analysis.next(), None);
        assert_eq!(analysis.next(), None);
    }

    #[test]
    fn test_analysis_statistics() {
        let (_, rx) = mpsc::sync_channel(0);
        let analysis = Analysis::new(rx, spam_hd_traces);
        assert_eq!(analysis.stats(), None);
        let stats = analysis.stats.clone();
        stats.1.store(500, atomic::Ordering::Release);
        stats.2.store(123, atomic::Ordering::Release);
        stats.0.store(true, atomic::Ordering::Release);
        assert_eq!(analysis.stats(), Some((500, 123)));
    }
}
