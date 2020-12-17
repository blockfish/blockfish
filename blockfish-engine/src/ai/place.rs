use crate::{
    ai::State,
    shape::{NormalizedShapeTransform, ShapeRef, ShapeTable, Transform},
    BasicMatrix, Color, Input, Orientation,
};
use std::collections::{HashSet, VecDeque};

/// Represents a piece placement, with data about the shape as well as the input sequence
/// to get it into place.
#[derive(Clone)]
pub struct Place<'s> {
    /// The shape being placed.
    shape: ShapeRef<'s>,
    /// The final location / orientation for this placement.
    tf: Transform,
    /// List of inputs to move this piece from spawn to `tf`, including `Hold` in some
    /// cases.
    inputs: Vec<Input>,
    /// Flag indicating that `inputs` actually moves this piece to a location in
    /// mid-air, *above* the row indicated in `tf`. This will mean that the next input
    /// needs to be preceeded by a sonic-drop.
    mid_air: bool,
}

impl<'s> Place<'s> {
    /// Constructs a new `Place` from shape, transform. If `did_hold` is `true`, then adds
    /// `Hold` to the input list before any other inputs.
    pub fn new(shape: ShapeRef<'s>, tf: Transform, did_hold: bool) -> Self {
        let (_i, j, r) = tf;
        let inputs = if did_hold { Some(Input::Hold) } else { None };
        let inputs = inputs.into_iter().chain(initial_inputs(shape, j, r));
        Self {
            tf,
            shape,
            inputs: inputs.collect(),
            // any further inputs will automatically require sd
            mid_air: true,
        }
    }

    /// Returns a simplified view of this placement, as just a `(color, transform)` pair
    /// (particularly, discarding the inputs list).
    pub fn simple(&self) -> (Color, Transform) {
        (self.shape.color(), self.tf)
    }

    /// Returns a "normalized" view of this placement, only taking the final cells into
    /// account, not the exact rotation state.
    pub fn normal(&self) -> NormalizedShapeTransform {
        self.shape.normalize(self.tf)
    }

    /// Returns the input sequence for this placement.
    pub fn into_inputs(self) -> Vec<Input> {
        self.inputs
    }

    // accessor functions

    pub fn did_hold(&self) -> bool {
        self.inputs.get(0).cloned() == Some(Input::Hold)
    }

    pub fn shape(&self) -> ShapeRef<'s> {
        self.shape
    }

    pub fn transform(&self) -> Transform {
        self.tf
    }

    /// Simulates the input `inp` on this placement. If the input succeeds without being
    /// blocked by matrix `mat`, then returns `Some(updated_input)`. If the input is
    /// invalid, returns `None`.
    fn input(&self, mat: &BasicMatrix, inp: Input) -> Option<Self> {
        // get a list of potential offsets to try (particularly, from the kick table in
        // case of rotation).
        let (i0, j0, r0) = self.tf;
        let (r, offsets): (Orientation, &[(i16, i16)]) = match inp {
            Input::Left => (r0, &[(0, -1)]),
            Input::Right => (r0, &[(0, 1)]),
            Input::CCW => (r0.ccw(), self.shape.kicks(r0, r0.ccw())),
            Input::CW => (r0.cw(), self.shape.kicks(r0, r0.cw())),
            _ => panic!("invalid input to Shape::input(): {:?}", inp),
        };

        // run SRS algorithm: find the first valid offset if any.
        let (i, j) = offsets
            .iter()
            .map(|&(i_off, j_off)| (i0 + i_off, j0 + j_off))
            .find(|&(i, j)| !self.shape.intersects(mat, (i, j, r)))?;

        // create copy and push new inputs
        let mut copy = self.clone();
        if self.mid_air {
            copy.inputs.push(Input::SD);
            copy.mid_air = false;
        }
        copy.inputs.push(inp);

        // fall with gravity & update transform on copy
        let mut i = i;
        while !self.shape.intersects(mat, (i - 1, j, r)) {
            i -= 1;
            // since this piece falls with gravity, the next placement requires soft-drop
            // pressed.
            copy.mid_air = true;
        }
        copy.tf = (i, j, r);
        Some(copy)
    }
}

/// Iterator over placements as they are discovered on a matrix. This struct has a mutable
/// interface so that the data structures can be reused for further placements.
pub struct Placements<'s> {
    shtb: &'s ShapeTable,
    matrix: BasicMatrix,
    // BFS queue
    // TODO: to do finesse properly, we need to implement djikstra's via priority queue
    //       sorting by inputs.len()
    queue: VecDeque<Place<'s>>,
    // prevent search cycles
    places_seen: HashSet<(Color, Transform)>,
    // prevent returning identical (normalized) shapes
    normals_seen: HashSet<NormalizedShapeTransform>,
}

impl<'s> Placements<'s> {
    /// Returns a new placements iterator using the given shape table.
    ///
    /// Initially this will produce no placements; needs to be fed an initial state with
    /// `set_state`. Use `placements` function to automatically do this process.
    pub fn new(shtb: &'s ShapeTable) -> Self {
        Placements {
            shtb,
            matrix: BasicMatrix::with_cols(0),
            queue: VecDeque::with_capacity(64),
            places_seen: HashSet::with_capacity(64),
            normals_seen: HashSet::with_capacity(32),
        }
    }

    /// Reset this iterator to search for placements from the given node state, `st`.
    pub fn set_state(&mut self, st: &State) {
        self.matrix.clone_from(st.matrix());
        self.places_seen.clear();
        self.normals_seen.clear();
        self.queue.clear();
        for (color, did_hold) in available_colors(st) {
            match self.shtb.shape(color) {
                Some(shape) => self.push_basic_placements(shape, did_hold),
                None => log::error!("color {:?} has no shape!", color),
            }
        }
    }

    fn push_basic_placements(&mut self, shape: ShapeRef<'s>, did_hold: bool) {
        for r in Orientation::iter_all() {
            for j in shape.valid_cols(r, self.matrix.cols()) {
                let i = shape.peak(&self.matrix, j, r);
                self.queue.push_back(Place::new(shape, (i, j, r), did_hold));
            }
        }
    }

    fn push_successors(&mut self, pl: &Place<'s>) {
        let matrix = &self.matrix;
        self.queue.extend(
            [Input::Left, Input::Right, Input::CW, Input::CCW]
                .iter()
                .flat_map(|&inp| pl.input(matrix, inp)),
        );
    }

    fn dequeue(&mut self) -> Option<Place<'s>> {
        self.queue.pop_front()
    }

    /// Returns `true` if `pl` has already been visited, otherwise marks it as visited.
    fn is_cycle(&mut self, pl: &Place) -> bool {
        !self.places_seen.insert(pl.simple())
    }

    /// Returns `true` if `pl` has already been yielded from the iterator, otherwise marks
    /// it as a repeat.
    fn is_repeat(&mut self, pl: &Place) -> bool {
        !self.normals_seen.insert(pl.normal())
    }
}

impl<'s> Iterator for Placements<'s> {
    type Item = Place<'s>;
    fn next(&mut self) -> Option<Place<'s>> {
        loop {
            let node = self.dequeue()?;
            if self.is_cycle(&node) {
                continue;
            }
            self.push_successors(&node);
            if !self.is_repeat(&node) {
                return Some(node);
            }
        }
    }
}

/// Returns a `Placements` iterator already "primed" with node state `st`.
pub fn placements<'s>(shtb: &'s ShapeTable, st: &State) -> Placements<'s> {
    let mut places = Placements::new(shtb);
    places.set_state(st);
    places
}

/// Returns `(color, hold)` for the next shape colors available for `state`, where `hold`
/// is `true` if the it requires hold key to use.
fn available_colors(state: &State) -> impl Iterator<Item = (Color, bool)> {
    let (col_nh, mut col_h) = state.next();
    if col_nh == col_h {
        // don't use hold if identical to current piece
        col_h = None;
    }
    let nh = col_nh.into_iter().map(|c| (c, false));
    let h = col_h.into_iter().map(|c| (c, true));
    nh.chain(h)
}

fn initial_inputs<'a>(shape: ShapeRef<'a>, j: i16, r: Orientation) -> impl Iterator<Item = Input> {
    let h = j - shape.spawn_col();
    let h_input = if h < 0 { Input::Left } else { Input::Right };
    let h_inputs = std::iter::repeat(h_input).take(h.abs() as usize);
    let r_inputs = match r {
        Orientation::R0 => &[] as &[_],
        Orientation::R1 => &[Input::CW],
        Orientation::R2 => &[Input::CW, Input::CW],
        Orientation::R3 => &[Input::CCW],
    }
    .iter()
    .cloned();
    r_inputs.chain(h_inputs)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{basic_matrix, shape::srs, Color, Input::*, Orientation::*, Snapshot};

    #[test]
    fn test_available_colors() {
        fn colors(hold: char, queue: &str) -> Vec<(char, bool)> {
            let snapshot = Snapshot {
                hold: std::convert::TryFrom::try_from(hold).ok(),
                queue: queue.chars().map(Color::n).collect(),
                matrix: BasicMatrix::with_cols(1),
            };
            available_colors(&snapshot.into())
                .map(|(c, h)| (c.as_char(), h))
                .collect()
        }
        assert_eq!(colors('.', "TJZ"), [('T', false), ('J', true)]);
        assert_eq!(colors('I', "TJZ"), [('T', false), ('I', true)]);
        assert_eq!(colors('I', "IJZ"), [('I', false)]);
        assert_eq!(colors('.', "T"), [('T', false)]);
        assert_eq!(colors('O', ""), [('O', true)]);
        assert_eq!(colors('.', ""), []);
    }

    #[test]
    fn test_inputs() {
        let snapshot = Snapshot {
            queue: vec![Color::n('I')],
            hold: Some(Color::n('J')),
            matrix: BasicMatrix::with_cols(10),
        };
        for pl in placements(&srs(), &snapshot.into()) {
            let (col, tf) = pl.simple();
            match (col.as_char(), tf) {
                ('I', (-2, 0, R0)) => assert_eq!(pl.inputs, [Left, Left, Left]),
                ('I', (-2, 6, R0)) => assert_eq!(pl.inputs, [Right, Right, Right]),
                ('I', (0, 4, R1)) => assert_eq!(pl.inputs, [CW, Right]),
                ('J', (-1, 2, R3)) => assert_eq!(pl.inputs, [Hold, CCW, Left]),
                _ => {}
            }
        }
    }

    #[test]
    fn test_overlapping_placements() {
        let snapshot = Snapshot {
            queue: vec![Color::n('O')],
            hold: Some(Color::n('S')),
            matrix: BasicMatrix::with_cols(10),
        };
        let mut places: Vec<_> = placements(&srs(), &snapshot.into())
            .map(|pl| {
                let (c, (i, j, r)) = pl.simple();
                (c.as_char(), r, i, j)
            })
            .collect();
        places.sort();
        assert_eq!(
            places,
            [
                ('O', R0, -1, -1),
                ('O', R0, -1, 0),
                ('O', R0, -1, 1),
                ('O', R0, -1, 2),
                ('O', R0, -1, 3),
                ('O', R0, -1, 4),
                ('O', R0, -1, 5),
                ('O', R0, -1, 6),
                ('O', R0, -1, 7),
                ('S', R0, -1, 0),
                ('S', R0, -1, 1),
                ('S', R0, -1, 2),
                ('S', R0, -1, 3),
                ('S', R0, -1, 4),
                ('S', R0, -1, 5),
                ('S', R0, -1, 6),
                ('S', R0, -1, 7),
                ('S', R1, 0, -1),
                ('S', R1, 0, 0),
                ('S', R1, 0, 1),
                ('S', R1, 0, 2),
                ('S', R1, 0, 3),
                ('S', R1, 0, 4),
                ('S', R1, 0, 5),
                ('S', R1, 0, 6),
                ('S', R1, 0, 7),
            ]
        );
    }

    #[test]
    fn test_placements_w_hold() {
        let (xx, __) = (true, false);
        let snapshot = Snapshot {
            matrix: basic_matrix![[__, __, xx]],
            queue: vec![Color::n('T')],
            hold: Some(Color::n('L')),
        };

        let mut places: Vec<_> = placements(&srs(), &snapshot.into())
            .map(|pl| {
                let (c, (i, j, r)) = pl.simple();
                (c.as_char(), r, i, j, pl.did_hold())
            })
            .collect();
        places.sort();
        assert_eq!(
            places,
            [
                ('L', R0, 0, 0, true),
                ('L', R1, 0, -1, true),
                ('L', R1, 1, 0, true),
                ('L', R2, 0, 0, true),
                ('L', R3, 0, 0, true),
                ('L', R3, 1, 1, true),
                ('T', R0, 0, 0, false),
                ('T', R1, 0, -1, false),
                ('T', R1, 0, 0, false),
                ('T', R2, 0, 0, false),
                ('T', R3, 0, 0, false),
                ('T', R3, 1, 1, false),
            ]
        );
    }

    #[test]
    fn test_place_input() {
        let srs = srs();
        let (xx, __) = (true, false);
        //         T
        //       T T T
        // . . . . . x
        // . . . . . x
        // . . . x . x
        let mat = basic_matrix![
            [__, __, __, xx, __, xx],
            [__, __, __, __, __, xx],
            [__, __, __, __, __, xx],
        ];
        let t = srs.shape(Color::n('T')).unwrap();
        let pl = Place::new(t, (2, 3, R0), false);

        assert_eq!(pl.inputs, []);
        // right movement fails
        assert!(pl.input(&mat, Right).is_none());
        // left movement succeeds
        let pl = pl.input(&mat, Left).unwrap();
        assert_eq!(pl.tf, (0, 2, R0));
        assert_eq!(pl.inputs, [SD, Left]);
        // 2nd left movement succeeds
        let pl = pl.input(&mat, Left).unwrap();
        assert_eq!(pl.tf, (0, 1, R0));
        assert_eq!(pl.inputs, [SD, Left, SD, Left]);
        // 3rd left movement succeeds
        let pl = pl.input(&mat, Left).unwrap();
        assert_eq!(pl.tf, (-1, 0, R0));
        assert_eq!(pl.inputs, [SD, Left, SD, Left, /* no SD */ Left]);
        // 4th left movement fails
        assert!(pl.input(&mat, Left).is_none());
    }

    fn all_places(
        matrix: BasicMatrix,
        (color_char, r): (char, Orientation),
    ) -> Vec<(i16, i16, Vec<Input>)> {
        let snapshot = Snapshot {
            hold: None,
            queue: vec![Color::n(color_char)],
            matrix,
        };
        let mut places: Vec<_> = placements(&srs(), &snapshot.into())
            .filter(|pl| pl.tf.2 == r)
            .map(|pl| (pl.tf.0, pl.tf.1, pl.inputs))
            .collect();
        places.sort();
        places
    }

    #[test]
    fn test_tuck_easy() {
        let (xx, __) = (true, false);
        assert_eq!(
            all_places(
                basic_matrix![[__, __, __, __, __], [xx, __, __, __, __]],
                ('T', R0)
            ),
            [
                // x . . . .      x T . . .
                // . . . . .  ->  T T T . .
                //                  (-1,0)
                // NOTE: it only finds this optimal input sequence because of some subtle
                //       implementation details. finesse is NOT optimized in general!
                (-1, 0, vec![Left, Left, SD, Left]),
                (-1, 1, vec![Left, Left]),
                (-1, 2, vec![Left]),
                (1, 0, vec![Left, Left, Left]),
            ]
        );

        assert_eq!(
            all_places(
                basic_matrix![
                    [__, __, __, __, __],
                    [__, __, __, xx, __],
                    [__, __, __, xx, __],
                ],
                ('T', R0)
            ),
            [
                // . . . x .      . . . x .
                // . . . x .      . . T x .
                // . . . . .  ->  . T T T .
                //                  (-1,1)
                (-1, 0, vec![Left, Left, Left]),
                // same as "NOTE" above
                (-1, 1, vec![Left, Left, Left, SD, Right]),
                (2, 1, vec![Left, Left]),
                (2, 2, vec![Left]),
            ]
        );
    }

    #[test]
    fn test_tuck_double_sd() {
        let (xx, __) = (true, false);
        assert_eq!(
            all_places(
                basic_matrix![
                    [__, __, __, xx, __],
                    [__, __, __, xx, xx],
                    [__, __, __, __, __],
                    [__, __, __, __, __],
                    [xx, xx, xx, __, __],
                ],
                ('O', R0)
            ),
            [
                // x x x . .      x x x . .      x x x . .      x x x . .
                // . . . . .      . . O O .      . . . . .      . . . . .
                // . . . . .  ->  . . O O .  ->  . . . . .  ->  . . . . .
                // . . . x x      . . . x x      . O O x x      O O . x x
                // . . . x .      . . . x .      . O O x .      O O . x .
                //                  (1,1)          (-1,0)         (-1,-1)
                //                SD,L           SD,L,L         SD,L,L,SD,L
                (-1, -1, vec![Left, SD, Left, Left, SD, Left]),
                (-1, 0, vec![Left, SD, Left, Left]),
                (1, 1, vec![Left, SD, Left]),
                (1, 2, vec![Left]),
                (4, -1, vec![Left, Left, Left, Left]),
                (4, 0, vec![Left, Left, Left]),
                (4, 1, vec![Left, Left]),
            ]
        );
    }

    #[test]
    fn test_tuck_ambiguous() {
        let (xx, __) = (true, false);
        assert_eq!(
            all_places(
                basic_matrix![
                    [__, __, __, __, __],
                    [__, __, __, __, __],
                    [__, __, xx, __, __],
                ],
                ('O', R0)
            ),
            [
                // . . x . .      . . x . .      . . x . .
                // . . . . .  ->  . O O . .  ->  . . O O .
                // . . . . .      . O O . .      . . O O .
                //                  (-1,0)         (-1,1)
                //                SD,R           SD,R,R
                //                SD,L,L         SD,L
                (-1, -1, vec![Left, Left, Left, Left]),
                (-1, 0, vec![Left, Left, Left, Left, SD, Right]),
                (-1, 1, vec![Left, SD, Left]),
                (-1, 2, vec![Left]),
                (2, 0, vec![Left, Left, Left]),
                (2, 1, vec![Left, Left]),
            ]
        );
        // NOTE: these input sequences are biased by implementation details of the
        //       algorithm, and not necessarily emblematic of correct finesse.
    }

    #[test]
    fn test_tspeen() {
        let (xx, __) = (true, false);
        assert_eq!(
            all_places(
                // x . . .
                // . . . x
                // x . x x
                basic_matrix![[xx, __, xx, xx], [__, __, __, xx], [xx, __, __, __],],
                ('T', R2)
            ),
            [
                (0, 0, vec![CW, Left, Left, Left, SD, CW]),
                (1, 1, vec![CW, CW, Left, Left]),
                (2, 0, vec![CW, CW, Left, Left, Left]),
            ]
        );
    }

    #[test]
    fn test_lspeen() {
        let (xx, __) = (true, false);
        assert_eq!(
            all_places(
                basic_matrix![[__, __, __, __, __], [xx, xx, __, xx, xx]],
                ('L', R0)
            ),
            [
                // . . . . .      . L L . .      . . . . .
                // x x . x x      x x L x x      x x L x x
                // . . . . .  ->  . . L . .  ->  L L L . .
                //                                 (-1,0)
                (-1, 0, vec![CCW, Left, Left, SD, CW]),
                (1, 0, vec![Left, Left, Left]),
                (1, 1, vec![Left, Left]),
                (1, 2, vec![Left]),
            ]
        );
    }

    #[test]
    fn test_s_spin_triple() {
        let (xx, __) = (true, false);
        assert_eq!(
            all_places(
                basic_matrix![
                    [xx, xx, __, xx],
                    [xx, __, __, xx],
                    [xx, __, xx, xx],
                    [__, __, __, __],
                    [xx, __, __, __],
                ],
                ('S', R1)
            ),
            [
                // x . . .
                // . . . .
                // x . x x
                // x . . x
                // x x . x
                (0, 0, vec![Left, Left, SD, Left, CW]),
                (3, 0, vec![CW, Left, Left, Left]),
                (3, 1, vec![CW, Left, Left]),
                (4, -1, vec![CW, Left, Left, Left, Left]),
            ]
        );
    }

    #[test]
    fn test_s_spin_triple_overhangless() {
        let (xx, __) = (true, false);
        assert_eq!(
            all_places(
                basic_matrix![
                    [xx, xx, __, xx],
                    [xx, __, __, xx],
                    [xx, __, xx, xx],
                    [__, __, __, xx],
                ],
                ('S', R3)
            ),
            [
                // . . . .
                // . . . x
                // x . x x
                // x . . x
                // x x . x
                (0, 1, vec![Left, Left, SD, CCW]),
            ]
        );
    }

    #[test]
    fn test_pierce() {
        let (xx, __) = (true, false);
        assert_eq!(
            all_places(
                basic_matrix![
                    [xx, xx, __, xx, xx, xx],
                    [xx, __, __, __, __, xx],
                    [__, __, __, __, __, __],
                    [__, __, __, xx, __, __],
                ],
                ('I', R0)
            ),
            [
                // . . . x . .   . . I x . .   . . . x . .
                // . . . . . .   . . I . . .   . . . . . .
                // x . . . . x   x . I . . x   x I I I I x
                // x x . x x x   x x I x x x   x x . x x x
                (0, 0, vec![CW, Left, Left, Left, SD, CCW]), // ??
                (2, 0, vec![Left, Left, Left]),
                (2, 1, vec![Left, Left]),
                (2, 2, vec![Left]),
            ]
        );
    }
}
