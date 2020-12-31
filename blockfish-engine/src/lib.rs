mod ai;
mod common;
mod finesse;
mod matrix;
mod shape;

pub use ai::{
    static_eval, Analysis, Config, Eval, ScoreParams, Snapshot, Suggestion, SuggestionFilter, AI,
};
pub use common::{Color, Input, Orientation};
pub use matrix::BasicMatrix;

#[cfg(feature = "block-stacker")]
mod stacker_util;

#[cfg(feature = "block-stacker")]
pub use stacker_util::StackerExt;

#[cfg(feature = "gen-shtb")]
pub use shape::ShapeTable;
