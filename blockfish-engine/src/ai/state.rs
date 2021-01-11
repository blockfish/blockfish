use super::Snapshot;
use crate::{
    common::Color,
    matrix::BasicMatrix,
    place::{Place, PlaceFinder},
};

/// A game state: matrix plus queue/hold.
///
/// Represention should prioritize efficiently taking pieces from the queue/hold slot.
#[derive(Clone)]
pub struct State {
    matrix: BasicMatrix,
    queue_rev: Vec<Color>,
    has_held: bool,
    reached_goal: bool,
}

impl State {
    pub fn matrix(&self) -> &BasicMatrix {
        &self.matrix
    }

    pub fn reached_goal(&self) -> bool {
        self.reached_goal
    }

    pub fn is_terminal(&self) -> bool {
        self.reached_goal || self.queue_rev.is_empty()
    }

    /// Returns the next available piece colors, as pair `(next_piece, hold_piece)`.
    /// Either may be `None` if not available (e.g. the next queue is empty).
    ///
    /// Note: `hold_piece` is not exactly the current piece in hold, rather the piece you
    /// will get if you press hold, i.e. if hold is currently empty then it refers to the
    /// 2nd piece in the queue.
    pub fn next(&self) -> (Option<Color>, Option<Color>) {
        let from_top = |i| {
            self.queue_rev
                .len()
                .checked_sub(i)
                .and_then(|i| self.queue_rev.get(i))
                .cloned()
        };
        let c1 = from_top(1);
        let c2 = from_top(2);
        if self.has_held {
            (c2, c1)
        } else {
            (c1, c2)
        }
    }

    /// Configures the placements iterator `pls` to produce the set of valid placements
    /// for this state.
    pub fn placements<'p, 's>(&self, pfind: &'p mut PlaceFinder<'s>) -> &'p mut PlaceFinder<'s> {
        pfind.reset_matrix(&self.matrix);
        let (color_nh, color_h) = self.next();
        if let Some(c) = color_nh {
            pfind.push_shape(c, false);
        }
        if let Some(c) = color_h {
            // don't use hold piece if its identical to current piece
            if color_h != color_nh {
                pfind.push_shape(c, true);
            }
        }
        pfind
    }

    /// Applies the given placement to this state, modifying the queue and matrix.
    pub fn place(&mut self, pl: &Place) {
        pl.shape.blit_to(&mut self.matrix, pl.tf);
        self.reached_goal |= self.matrix.sift_rows();
        self.pop(pl.did_hold);
    }

    /// Removes a piece from the next queue, or hold slot if `hold` is `true`.
    fn pop(&mut self, hold: bool) {
        //  | has_held | hold  | pos
        // -+----------+-------+-----
        //  | true     | false | 2
        //  | true     | true  | 1
        //  | false    | false | 1
        //  | false    | true  | 2
        let pos = if self.has_held == hold { 1 } else { 2 };
        self.queue_rev.remove(self.queue_rev.len() - pos);
        self.has_held |= hold;
    }
}

impl From<Snapshot> for State {
    fn from(snapshot: Snapshot) -> Self {
        let matrix = snapshot.matrix;
        // the queue is represented in reverse order, so the next item can easily be
        // removed. the hold piece (if any) is stored on top, after the previews.
        let mut queue_rev = snapshot.queue;
        queue_rev.reverse();
        let mut has_held = false;
        if let Some(hold_color) = snapshot.hold {
            has_held = true;
            queue_rev.push(hold_color);
        }
        Self {
            matrix,
            queue_rev,
            has_held,
            reached_goal: false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{shape::srs, Orientation::*};

    #[test]
    fn test_state_operations() {
        let queue = || "LTJI".chars().map(Color::n);

        let mut s: State = Snapshot {
            hold: None,
            queue: queue().collect(),
            matrix: BasicMatrix::with_cols(10),
        }
        .into();
        assert!(!s.is_terminal());
        assert_eq!(s.matrix.rows(), 0);
        assert_eq!(s.matrix.cols(), 10);
        assert_eq!(s.next(), (Some(Color::n('L')), Some(Color::n('T'))));

        let srs = srs();
        for (i, color) in queue().enumerate() {
            let shape = srs.shape(color).unwrap();
            let tf = ((i * 2) as i16 - 1, 0, R0);
            s.place(&Place::new(shape, tf, false));
        }
        assert!(s.is_terminal());
        assert_eq!(s.matrix.rows(), 7);
        assert_eq!(s.next(), (None, None));
    }

    #[test]
    fn test_state_use_hold() {
        // something already in hold
        let mut s: State = Snapshot {
            hold: Some(Color::n('S')),
            queue: "LTJI".chars().map(Color::n).collect(),
            matrix: BasicMatrix::with_cols(10),
        }
        .into();
        assert_eq!(s.next(), (Some(Color::n('L')), Some(Color::n('S'))));
        s.pop(true);
        assert_eq!(s.next(), (Some(Color::n('T')), Some(Color::n('L'))));
        s.pop(false);
        assert_eq!(s.next(), (Some(Color::n('J')), Some(Color::n('L'))));
        // nothing previously in hold
        s = Snapshot {
            hold: None,
            queue: "LTJI".chars().map(Color::n).collect(),
            matrix: BasicMatrix::with_cols(10),
        }
        .into();
        assert_eq!(s.next(), (Some(Color::n('L')), Some(Color::n('T'))));
        s.pop(true);
        assert_eq!(s.next(), (Some(Color::n('J')), Some(Color::n('L'))));
    }

    #[test]
    fn test_state_nearly_empty_queue() {
        let mut s: State = Snapshot {
            hold: None,
            queue: vec![Color::n('I')],
            matrix: BasicMatrix::with_cols(10),
        }
        .into();
        assert_eq!(s.next(), (Some(Color::n('I')), None));
        s = Snapshot {
            hold: Some(Color::n('O')),
            queue: vec![],
            matrix: BasicMatrix::with_cols(10),
        }
        .into();
        assert_eq!(s.next(), (None, Some(Color::n('O'))));
    }
}
