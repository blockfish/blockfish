use crate::{
    ai::State,
    shape::{NormalizedShapeTransform, ShapeRef, ShapeTable, Transform},
    BasicMatrix, Color, Input, Orientation,
};
use std::collections::HashSet;

/// Represents a piece placement, with data about the shape as well as the input sequence
/// to get it into place.
#[derive(Clone)]
pub struct Place<'s> {
    /// The shape being placed.
    pub shape: ShapeRef<'s>,
    /// The final location / orientation for this placement.
    pub tf: Transform,
    /// `true` if hold was required for this placement.
    pub did_hold: bool,
}

impl<'s> Place<'s> {
    /// Constructs a new `Place` from shape, transform. If `did_hold` is `true`, then adds
    /// `Hold` to the input list before any other inputs.
    pub fn new(shape: ShapeRef<'s>, tf: Transform, did_hold: bool) -> Self {
        Self {
            shape,
            tf,
            did_hold,
        }
    }

    /// Returns a "normalized" view of this placement, only taking the final cells into
    /// account, not the exact rotation state.
    pub fn normal(&self) -> NormalizedShapeTransform {
        self.shape.normalize(self.tf)
    }

    /// Simulates the input `inp` on this placement. If the input succeeds without being
    /// blocked by matrix `mat`, then returns `Some(updated_input)`. If the input is
    /// invalid, returns `None`.
    fn input(&self, matrix: &BasicMatrix, input: Input) -> Option<Self> {
        let tf = self.shape.try_input(matrix, self.tf, input)?;
        let tf = self.shape.sonic_drop(matrix, tf);
        Some(Place { tf, ..self.clone() })
    }
}

/// Iterator over placements as they are discovered on a matrix. This struct has a mutable
/// interface so that the data structures can be reused for further placements.
pub struct Placements<'s> {
    shtb: &'s ShapeTable,
    matrix: BasicMatrix,
    // next placements to scan
    queue: Vec<Place<'s>>,
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
            queue: Vec::with_capacity(64),
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
                self.queue.push(Place::new(shape, (i, j, r), did_hold));
            }
        }
    }

    fn push_successors(&mut self, pl: &Place<'s>) {
        let matrix = &self.matrix;
        self.queue.extend(
            [Input::Left, Input::Right, Input::CW, Input::CCW]
                .iter()
                .filter_map(|&inp| pl.input(matrix, inp)),
        );
    }

    fn pop(&mut self) -> Option<Place<'s>> {
        self.queue.pop()
    }

    /// Returns `true` if `pl` has already been visited, otherwise marks it as visited.
    fn is_cycle(&mut self, pl: &Place) -> bool {
        !self.places_seen.insert((pl.shape.color(), pl.tf))
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
            let node = self.pop()?;
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
    fn test_overlapping_placements() {
        let snapshot = Snapshot {
            queue: vec![Color::n('O')],
            hold: Some(Color::n('S')),
            matrix: BasicMatrix::with_cols(10),
        };
        let mut o_count = 0;
        let mut s02_count = 0;
        let mut s13_count = 0;
        for pl in placements(&srs(), &snapshot.into()) {
            let c = pl.shape.color();
            let r = pl.tf.2;
            match c.as_char() {
                'O' => o_count += 1,
                'S' if r == R0 || r == R2 => s02_count += 1,
                'S' => s13_count += 1,
                _ => panic!("unexpected color {:?}", c),
            }
        }
        assert_eq!(o_count, 9);
        assert_eq!(s02_count, 8);
        assert_eq!(s13_count, 9);
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
                let c = pl.shape.color();
                let (i, j, r) = pl.tf;
                (c.as_char(), r, i, j, pl.did_hold)
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

        // right movement fails
        assert!(pl.input(&mat, Right).is_none());
        // left movement succeeds
        let pl = pl.input(&mat, Left).unwrap();
        assert_eq!(pl.tf, (0, 2, R0));
        // 2nd left movement succeeds
        let pl = pl.input(&mat, Left).unwrap();
        assert_eq!(pl.tf, (0, 1, R0));
        // 3rd left movement succeeds
        let pl = pl.input(&mat, Left).unwrap();
        assert_eq!(pl.tf, (-1, 0, R0));
        // 4th left movement fails
        assert!(pl.input(&mat, Left).is_none());
    }

    fn all_places(matrix: BasicMatrix, (color_char, r): (char, Orientation)) -> Vec<(i16, i16)> {
        let snapshot = Snapshot {
            hold: None,
            queue: vec![Color::n(color_char)],
            matrix,
        };
        let mut places: Vec<_> = placements(&srs(), &snapshot.into())
            .filter(|pl| pl.tf.2 == r)
            .map(|pl| (pl.tf.0, pl.tf.1))
            .collect();
        places.sort();
        places
    }

    fn all_o_places(matrix: BasicMatrix) -> Vec<(i16, i16)> {
        let snapshot = Snapshot {
            hold: None,
            queue: vec![Color::n('O')],
            matrix,
        };
        let mut places: Vec<_> = placements(&srs(), &snapshot.into())
            .map(|pl| (pl.tf.0, pl.tf.1))
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
                (-1, 0),
                (-1, 1),
                (-1, 2),
                (1, 0),
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
                (-1, 0),
                (-1, 1),
                (2, 1),
                (2, 2),
            ]
        );
    }

    #[test]
    fn test_tuck_double_sd() {
        let (xx, __) = (true, false);
        assert_eq!(
            all_o_places(basic_matrix![
                [__, __, __, xx, __],
                [__, __, __, xx, xx],
                [__, __, __, __, __],
                [__, __, __, __, __],
                [xx, xx, xx, __, __],
            ]),
            [
                // x x x . .      x x x . .      x x x . .      x x x . .
                // . . . . .      . . O O .      . . . . .      . . . . .
                // . . . . .  ->  . . O O .  ->  . . . . .  ->  . . . . .
                // . . . x x      . . . x x      . O O x x      O O . x x
                // . . . x .      . . . x .      . O O x .      O O . x .
                //                  (1,1)          (-1,0)         (-1,-1)
                //                SD,L           SD,L,L         SD,L,L,SD,L
                (-1, -1),
                (-1, 0),
                (1, 1),
                (1, 2),
                (4, -1),
                (4, 0),
                (4, 1),
            ]
        );
    }

    #[test]
    fn test_tuck_ambiguous() {
        let (xx, __) = (true, false);
        assert_eq!(
            all_o_places(basic_matrix![
                [__, __, __, __, __],
                [__, __, __, __, __],
                [__, __, xx, __, __],
            ]),
            [
                // . . x . .      . . x . .      . . x . .
                // . . . . .  ->  . O O . .  ->  . . O O .
                // . . . . .      . O O . .      . . O O .
                //                  (-1,0)         (-1,1)
                //                SD,R           SD,R,R
                //                SD,L,L         SD,L
                (-1, -1),
                (-1, 0),
                (-1, 1),
                (-1, 2),
                (2, 0),
                (2, 1),
            ]
        );
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
            [(0, 0), (1, 1), (2, 0),]
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
                (-1, 0),
                (1, 0),
                (1, 1),
                (1, 2),
            ]
        );
    }

    #[test]
    fn test_s_spin_triple() {
        let (xx, __) = (true, false);
        let places = all_places(
            basic_matrix![
                [xx, xx, __, xx],
                [xx, __, __, xx],
                [xx, __, xx, xx],
                [__, __, __, __],
                [xx, __, __, __],
            ],
            ('S', R1),
        );
        assert!(places.contains(&(0, 0)), "{:?}", places);
    }

    #[test]
    fn test_s_spin_triple_overhangless() {
        let (xx, __) = (true, false);
        let places = all_places(
            basic_matrix![
                [xx, xx, __, xx],
                [xx, __, __, xx],
                [xx, __, xx, xx],
                [__, __, __, xx],
            ],
            ('S', R3),
        );
        assert!(places.contains(&(0, 1)), "{:?}", places);
    }

    #[test]
    fn test_pierce() {
        let (xx, __) = (true, false);
        // . . . x . .   . . . x . .
        // . . . . . .   . . . . . .
        // x . . . . x   x I I I I x
        // x x . x x x   x x . x x x
        let mat = basic_matrix![
            [xx, xx, __, xx, xx, xx],
            [xx, __, __, __, __, xx],
            [__, __, __, __, __, __],
            [__, __, __, xx, __, __],
        ];
        let r0_places = all_places(mat.clone(), ('I', R0));
        let r2_places = all_places(mat.clone(), ('I', R2));
        assert!(
            r0_places.contains(&(-1, 1)) || r2_places.contains(&(0, 1)),
            "R0: {:?}, R2: {:?}",
            r0_places,
            r2_places
        );
    }
}
