use crate::{BasicMatrix, Color, Input, Snapshot};
use std::convert::TryInto;

pub trait StackerExt {
    /// Returns a snapshot of the game state. Returns `None` if the snapshot would
    /// be invalid (e.g., there is no current piece).
    fn snapshot(&self) -> Option<Snapshot>;

    /// Simulates input `inp` on the game state.
    fn input(&mut self, inp: Input);

    /// Simulates all of the inputs in `iter` on the game state.
    fn run<I: IntoIterator<Item = Input>>(&mut self, iter: I) {
        for inp in iter {
            self.input(inp);
        }
    }
}

impl StackerExt for block_stacker::Stacker {
    fn snapshot(&self) -> Option<Snapshot> {
        fn to_color(typ: block_stacker::PieceType) -> Color {
            typ.try_into().expect("bug: invalid color")
        }
        let cur = to_color(self.current_piece_type()?);
        let next = self.next().iter().map(|&typ| to_color(typ));
        let queue = std::iter::once(cur).chain(next).collect();
        let hold = self.held().map(to_color);
        let cols = self.ruleset().cols;
        let mut matrix = BasicMatrix::with_cols(cols as u16);
        for (coord, _) in self.matrix() {
            matrix.set(coord);
        }
        Some(Snapshot {
            queue,
            hold,
            matrix,
        })
    }

    fn input(&mut self, inp: Input) {
        match inp {
            Input::Left => {
                self.move_horizontal(-1);
            }
            Input::Right => {
                self.move_horizontal(1);
            }
            Input::CCW => {
                self.rotate(-1);
            }
            Input::CW => {
                self.rotate(1);
            }
            Input::Hold => {
                self.hold();
            }
            Input::HD => {
                self.hard_drop();
            }
            Input::SD => {
                self.sonic_drop();
            }
        }
    }
}
