use crate::{
    config::Config,
    shape::{srs, ShapeTable},
    BasicMatrix, Color, Input,
};

mod analysis;
mod b_star;
mod eval;
mod state;

// Input / output types

/// A game state snapshot to begin an analysis from.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Snapshot {
    pub hold: Option<Color>,
    pub queue: Vec<Color>,
    pub matrix: BasicMatrix,
}

/// A suggested sequence and its rating.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Suggestion {
    /// List of inputs to perform the suggested move.
    pub inputs: Vec<Input>,
    // The "rating" is an abstract measurement for how good a move is (lower is better).
    pub rating: i64,
}

/// Statistics about the analysis after it has finished.
#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct Stats {
    /// Number of iterations of the search algorithm.
    pub iterations: usize,
    /// Number of nodes generated.
    pub nodes: usize,
    /// Total time taken to do the analysis.
    pub time_taken: std::time::Duration,
}

// Evaluation function interface

pub use eval::Eval;

/// Performs the static analysis function on a snapshot.
pub fn static_eval(snapshot: &Snapshot) -> Eval {
    eval::eval(&snapshot.matrix)
}

// AI interface

// Re-export
pub use analysis::{Analysis, AnalysisDone, MoveId};

/// An instance of the Blockfish AI. Holds engine configuration and can be used to spawn
/// an analysis.
///
/// `AI` can currently be seen as just a sort of builder-pattern type for creating
/// `Analysis`'s.  However, in the future it could be extended to handle more things such
/// as a reusable thread pool and/or the place to call `static_eval`.
pub struct AI {
    config: Config,
    shape_table: std::sync::Arc<ShapeTable>,
}

impl AI {
    /// Constructs a new Blockfish AI instance with the given engine configuration.
    pub fn new(config: Config) -> Self {
        Self {
            config,
            shape_table: std::sync::Arc::new(srs()),
        }
    }

    pub fn config(&self) -> Config {
        self.config.clone()
    }

    /// Begins a new analysis of `snapshot`, returning a handle to it.
    pub fn analyze(&mut self, snapshot: Snapshot) -> Analysis {
        analysis::spawn(
            self.shape_table.clone(),
            self.config.clone(),
            snapshot.into(),
        )
    }
}
