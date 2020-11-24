mod ai;
mod common;
mod matrix;
mod repl;
mod shape;

pub use ai::{ai, Snapshot, Suggestion, SuggestionsIter};
pub use common::{Color, Input};
pub use matrix::BasicMatrix;
pub use repl::repl;
