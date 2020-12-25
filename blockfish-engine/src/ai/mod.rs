mod a_star;
mod node;
mod place;
mod score;

#[cfg(test)]
#[allow(unused)]
mod dfs;

use self::{
    a_star::AStar,
    node::{Node, State},
    place::{placements, Place},
};
use crate::{
    shape::{srs, ShapeTable},
    BasicMatrix, Color, Input,
};

use place::Placements;
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

// AI

/// An instance of the Blockfish AI. Holds engine configuration and can be used to spawn
/// an analysis.
pub struct AI {
    config: Config,
    shape_table: Arc<ShapeTable>,
}

/// A handle to a blockfish analysis running in the background. Can be polled for new
/// suggestions from the engine. Particularly, an `Analysis` is an `Iterator` over
/// suggestions.
pub struct Analysis {
    // recieve end of communication channel with the worker thread.
    rx: Option<Rx>,
    // may hold an incoming message if a nonblocking poll was performed.
    inbox: Cell<Option<Msg>>,
    // `root_inputs[i]` holds the input list for placement sequences identified by root
    // index `i`. this list is populated when the analysis starts by a finesse pass over
    // all the initial valid placements.
    root_inputs: Vec<Vec<Input>>,
    // holds statistics about the worker thread, after the thread finishes
    stats: AtomicStats,
}

// Message type passed from worker threads to Analysis's.
struct Msg {
    score: i64,
    root_idx: usize,
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
        }
    }

    pub fn config(&self) -> Config {
        self.config.clone()
    }

    /// Begins a new analysis of `snapshot`, returning a handle to it.
    pub fn analyze(&mut self, snapshot: Snapshot) -> Analysis {
        Analysis::start(
            self.config.clone(),
            self.shape_table.clone(),
            snapshot.into(),
        )
    }
}

impl Analysis {
    /// Constructs a new `Analysis` with given channel reciever and initial placements.
    fn new(rx: Rx, roots: Placements) -> Self {
        Self {
            rx: Some(rx),
            inbox: Cell::new(None),
            root_inputs: roots.map(Place::into_inputs).collect(),
            stats: Arc::new((false.into(), 0.into(), 0.into())),
        }
    }

    /// Starts an analysis.
    fn start(cfg: Config, shtb: Arc<ShapeTable>, init_state: State) -> Self {
        let (tx, rx) = mpsc::sync_channel(1024);
        let analysis = Analysis::new(rx, placements(&shtb, &init_state));
        let stats = analysis.stats.clone();
        std::thread::spawn(move || Analysis::work(cfg, shtb, Node::new(init_state), tx, stats));
        analysis
    }

    /// Performs the work of an analysis, starting with root node `Node`. New potential
    /// suggestions are pushed to channel `tx`.
    fn work(cfg: Config, shtb: Arc<ShapeTable>, root: Node, tx: Tx, stats: AtomicStats) {
        assert!(root.root_idx().is_none()); // must be a root node
        let mut search = AStar::new(&shtb, &cfg.scoring, cfg.search_limit, root);
        let mut root_score = vec![];
        let mut best = std::i64::MAX;
        let mut iters = 0;
        while let Some(node) = search.next() {
            iters += 1;

            if let Some(root_idx) = node.root_idx() {
                if root_score.len() <= root_idx {
                    root_score.resize(root_idx + 1, std::i64::MAX);
                }

                let score = node.score();
                if score < root_score[root_idx] {
                    root_score[root_idx] = score;
                    if tx.send(Msg { root_idx, score }).is_err() {
                        log::warn!("analysis dropped; quitting early");
                        break;
                    }
                }

                if score < best {
                    log::trace!(
                        "root {} @ depth {}, score => {} (iteration #{})",
                        root_idx,
                        node.depth(),
                        score,
                        iters,
                    );
                    best = score;
                }
            }

            if search.node_count() >= cfg.search_limit {
                break;
            }
        }

        log::info!("ran {} iterations of A*", iters);
        let nodes = search.node_count();
        stats.1.store(nodes, atomic::Ordering::Release);
        stats.2.store(iters, atomic::Ordering::Release);
        stats.0.store(true, atomic::Ordering::Release);
        std::mem::drop(tx);
    }

    /// Returns `true` if calling `next()` would block while waiting for the next
    /// suggestion, or `false` if `next()` will immediately return a result.  When the AI
    /// process is finished, this function will always return `false`.
    pub fn would_block(&self) -> bool {
        if let Some(msg) = self.inbox.take() {
            self.inbox.set(Some(msg));
            return false;
        }
        let rx = match self.rx.as_ref() {
            Some(rx) => rx,
            None => return false,
        };
        match rx.try_recv() {
            Err(mpsc::TryRecvError::Empty) => true,
            Err(mpsc::TryRecvError::Disconnected) => false,
            Ok(msg) => {
                self.inbox.set(Some(msg));
                false
            }
        }
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

    /// Returns the next upcoming `Msg`, or `None` if the worker thread has completed.
    fn next_msg(&mut self) -> Option<Msg> {
        if let Some(m) = self.inbox.take() {
            return Some(m);
        }
        let rx = match self.rx.as_ref() {
            Some(rx) => rx,
            None => return None,
        };
        match rx.recv() {
            Err(mpsc::RecvError) => {
                self.rx = None;
                None
            }
            Ok(m) => Some(m),
        }
    }
}

impl Iterator for Analysis {
    type Item = Suggestion;
    fn next(&mut self) -> Option<Suggestion> {
        let Msg { score, root_idx } = self.next_msg()?;
        let inputs = self.root_inputs[root_idx].clone();
        Some(Suggestion { score, inputs })
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
    fn test_analysis_would_block() {
        let (tx, rx) = mpsc::sync_channel(0);
        let mut analysis = Analysis::new(rx, Placements::new(&srs()));
        analysis.root_inputs.push(vec![]);
        assert!(analysis.would_block());
        std::mem::drop(tx);
        assert!(!analysis.would_block());
    }

    #[test]
    fn test_analysis_next() {
        let (tx, rx) = mpsc::sync_channel(1);
        let mut analysis = Analysis::new(rx, Placements::new(&srs()));
        analysis.root_inputs.push(vec![Input::Left, Input::Right]);
        analysis.root_inputs.push(vec![Input::CW, Input::CCW]);
        assert!(analysis.would_block());

        let m = Msg {
            root_idx: 0,
            score: 5,
        };
        tx.send(m).unwrap();
        assert!(!analysis.would_block());
        assert_eq!(
            analysis.next(),
            Some(Suggestion {
                score: 5,
                inputs: vec![Input::Left, Input::Right],
            })
        );
        assert!(analysis.would_block());

        let m = Msg {
            root_idx: 1,
            score: 8,
        };
        tx.send(m).unwrap();
        assert_eq!(
            analysis.next(),
            Some(Suggestion {
                score: 8,
                inputs: vec![Input::CW, Input::CCW],
            })
        );

        std::mem::drop(tx);
        assert_eq!(analysis.next(), None);
        assert_eq!(analysis.next(), None);
    }

    #[test]
    fn test_analysis_statistics() {
        let (_, rx) = mpsc::sync_channel(0);
        let analysis = Analysis::new(rx, Placements::new(&srs()));
        assert_eq!(analysis.stats(), None);
        let stats = analysis.stats.clone();
        stats.1.store(500, atomic::Ordering::Release);
        stats.2.store(123, atomic::Ordering::Release);
        stats.0.store(true, atomic::Ordering::Release);
        assert_eq!(analysis.stats(), Some((500, 123)));
    }
}
