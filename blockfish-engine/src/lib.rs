mod ai;
mod common;
mod matrix;
mod shape;

pub use ai::{ai, Snapshot, Suggestion, SuggestionsIter};
pub use common::{Color, Input};
pub use matrix::BasicMatrix;

#[cfg(feature = "repl")]
mod repl;

#[cfg(feature = "repl")]
pub use repl::{repl, Error as ReplError};
