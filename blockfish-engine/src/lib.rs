mod ai;
mod common;
mod matrix;
mod shape;

pub use ai::{ai, Config, Snapshot, Suggestion, SuggestionsIter};
pub use common::{Color, Input, Orientation};
pub use matrix::BasicMatrix;

#[cfg(feature = "repl")]
mod repl;

#[cfg(feature = "repl")]
pub use repl::{repl, Error as ReplError};

#[cfg(feature = "block-stacker")]
mod stacker_util;

#[cfg(feature = "block-stacker")]
pub use stacker_util::StackerExt;
