mod common;
mod config;
mod finesse;
mod matrix;
mod place;
mod shape;

pub mod ai;

pub use common::{Color, Input, Orientation};
pub use config::{Config, Parameters};
pub use matrix::BasicMatrix;

#[cfg(feature = "block-stacker")]
mod stacker_util;

#[cfg(feature = "block-stacker")]
pub use stacker_util::StackerExt;

#[cfg(feature = "gen-shtb")]
pub use shape::ShapeTable;

/// Returns Blockfish's build version string.
pub fn version() -> &'static str {
    let v = include_str!("../../support/version");
    v.trim()
}
