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
    place::placements,
};
use crate::{
    shape::{srs, ShapeTable},
    BasicMatrix, Color, Input,
};
use std::{
    convert::TryInto,
    sync::{mpsc, Arc},
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

// Snapshot, suggestion

#[derive(Clone, Debug)]
pub struct Snapshot {
    pub hold: Option<Color>,
    pub queue: Vec<Color>,
    pub matrix: BasicMatrix,
}

#[derive(Clone, Debug)]
pub struct Suggestion {
    pub inputs: Vec<Input>,
    pub score: i64,
}

// AI

/// An AI process which can be used to suggest moves from a snapshot, or evaluate a game
/// state.
pub struct AI {
    cfg: Config,
    shtb: Arc<ShapeTable>,
    rx: Option<mpsc::Receiver<Suggestion>>,
}

/// A result from polling the AI for suggestions.
#[derive(Debug)]
pub enum AIPoll {
    /// A new solution was suggested (or the score of an earlier suggestion improved).
    Suggest(Suggestion),
    /// The AI is still running but no new suggestions were found.
    Pending,
    /// The AI process ended and no new suggestions will be returned until it is started
    /// again.
    Done,
}

impl AI {
    /// Constructs a new AI from the configuration `cfg`.
    pub fn new(cfg: Config) -> Self {
        AI {
            cfg,
            shtb: Arc::new(srs()),
            rx: None,
        }
    }

    /// Starts searching for suggestions from the game state in `snapshot`. Immediately
    /// cancels whatever search was happening prior.
    pub fn start(&mut self, snapshot: Snapshot) {
        self.rx = None;
        let cfg = self.cfg.clone();
        let shtb = self.shtb.clone();

        let (tx, rx) = mpsc::sync_channel(100);
        std::thread::spawn(move || {
            let init_state = snapshot.into();
            let mut roots = placements(&shtb, &init_state)
                .map(|place| Root::new(place.into_inputs()))
                .collect::<Vec<_>>();

            let mut best = std::i64::MAX;

            let mut worker = Worker::new(&shtb, &cfg.scoring, cfg.search_limit, init_state);
            let mut iterations = 0;
            while let Some((root_idx, depth, score)) = worker.step() {
                iterations += 1;

                let root = &mut roots[root_idx];
                let prev_score = root.best_score;
                if !root.update(depth, score) {
                    // TODO: send periodic "heartbeats" thru tx, with engine statistics
                    continue;
                }

                if score <= best {
                    log::trace!(
                        "{} root {} @ depth {}, score => {} (iteration #{})",
                        if root.best_score < prev_score {
                            "improved"
                        } else {
                            "DEGRADED"
                        },
                        root_idx,
                        root.furthest_depth,
                        root.best_score,
                        iterations
                    );
                    best = score;
                }

                if tx.send(root.to_suggestion()).is_err() {
                    log::warn!("ai channel dropped; stopping early");
                    return;
                }
            }
            log::info!("ran {} iterations of A*", iterations);
        });

        self.rx = Some(rx);
    }

    /// Returns the AI's static evaluation score of the game state in `snapshot`.
    pub fn static_eval(&self, snapshot: &Snapshot) -> i64 {
        score::score(&self.cfg.scoring, &snapshot.matrix)
    }

    /// Polls the AI to see if it has made any progress in finding moves.
    pub fn poll(&mut self) -> AIPoll {
        let rx = match self.rx.as_mut() {
            Some(rx) => rx,
            None => return AIPoll::Done,
        };
        match rx.try_recv() {
            Ok(sugg) => AIPoll::Suggest(sugg),
            Err(mpsc::TryRecvError::Empty) => AIPoll::Pending,
            Err(mpsc::TryRecvError::Disconnected) => {
                self.rx = None;
                AIPoll::Done
            }
        }
    }

    /// Returns an interator over all incoming suggestions from the AI, until it completes.
    pub fn into_iter(mut self) -> impl Iterator<Item = Suggestion> {
        self.rx.take().into_iter().flatten()
    }
}

struct Worker<'s> {
    node_limit: usize,
    search: AStar<'s>,
}

impl<'s> Worker<'s> {
    fn new(
        shape_table: &'s ShapeTable,
        scoring: &'s ScoreParams,
        node_limit: usize,
        state0: State,
    ) -> Self {
        let search = AStar::new(shape_table, scoring, node_limit, Node::new(state0));
        Self { search, node_limit }
    }

    fn step(&mut self) -> Option<(usize, usize, i64)> {
        loop {
            if self.search.node_count() > self.node_limit {
                return None;
            }

            let node = self.search.next()?;
            if let Some(idx) = node.root_idx() {
                return Some((idx, node.depth(), node.score()));
            }
        }
    }
}

/// Roots are updated when a new best sequence is found, and used to suggest the series of
/// inputs to make the first placement in that sequence.
struct Root {
    furthest_depth: isize,
    best_score: i64,
    inputs: Vec<Input>,
}

impl Root {
    fn new(inputs: impl IntoIterator<Item = Input>) -> Self {
        Self {
            furthest_depth: -1,
            best_score: std::i64::MAX,
            inputs: inputs.into_iter().collect(),
        }
    }

    /// Returns `true` if the given rating improves this root's score.
    fn update(&mut self, depth: usize, score: i64) -> bool {
        let depth = depth as isize;
        if depth < self.furthest_depth || score >= self.best_score {
            return false;
        }
        self.furthest_depth = depth;
        self.best_score = score;
        true
    }

    fn to_suggestion(&self) -> Suggestion {
        assert!(self.best_score != std::i64::MAX);
        Suggestion {
            score: self.best_score,
            inputs: self.inputs.clone(),
        }
    }
}

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
}
