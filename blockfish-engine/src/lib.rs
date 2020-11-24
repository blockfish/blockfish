mod ai;
mod common;
mod matrix;
mod repl;
mod shape;

pub use matrix::BasicMatrix;
pub use ai::{ai, Snapshot, Suggestion};
pub use common::{Color, Input};
pub use repl::repl;
