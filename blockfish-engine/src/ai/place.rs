use crate::{
    ai::node::State,
    shape::{NormalShapeId, ShapeTable},
    BasicMatrix, Color, Input,
};
use std::collections::{HashSet, VecDeque};

/// Represents a placement of a particular shape.
#[derive(Clone)]
pub struct Place {
    pub norm_id: NormalShapeId,
    pub coord: (u16, u16),
    pub did_hold: bool,
    pub mid_air: bool,
    pub initial_col: u16,
    pub final_inputs: Vec<Input>,
}

impl Place {
    /// Create a new placement with shape given by `norm_id` and position `coord`.
    pub fn new(norm_id: NormalShapeId, coord: (u16, u16)) -> Self {
        let (_, initial_col) = coord;
        Place {
            norm_id,
            coord,
            did_hold: false,
            mid_air: true,
            initial_col,
            final_inputs: vec![],
        }
    }

    pub fn row(&self) -> u16 {
        self.coord.0
    }

    pub fn col(&self) -> u16 {
        self.coord.1
    }

    fn intersects(&self, stbl: &ShapeTable, mat: &BasicMatrix) -> bool {
        let norm = &stbl[self.norm_id];
        self.col() + norm.cols() > mat.cols() || norm.intersects(mat, self.coord)
    }

    fn fall(&mut self, stbl: &ShapeTable, mat: &BasicMatrix) {
        let norm = &stbl[self.norm_id];
        let (mut row, col) = self.coord;
        while row > 0 && !norm.intersects(mat, (row - 1, col)) {
            row -= 1;
        }
        self.mid_air = row < self.row();
        self.coord = (row, col);
    }

    fn tap(&mut self, input: Input) {
        let (_row, col) = &mut self.coord;
        match input {
            Input::Left => *col = col.saturating_sub(1),
            Input::Right => *col += 1,
            Input::SD => self.mid_air = false,
            _ => panic!("invalid argument to Place::tap(): {:?}", input),
        }
        self.final_inputs.push(input);
    }
}

impl std::fmt::Debug for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?} at {:?}", self.norm_id, self.coord)
    }
}

/// Data structure used to search for placements and aggregating the results.
pub struct PlacementSearch<'s> {
    stbl: &'s ShapeTable,
    placements: Vec<Place>,
    matrix: BasicMatrix,
    bfs_queue: VecDeque<Place>,
    discovered: HashSet<(u16, u16)>,
}

impl<'s> PlacementSearch<'s> {
    /// Construct a new placement search using shape table `stbl`.
    pub fn new(stbl: &'s ShapeTable) -> Self {
        Self {
            stbl,
            placements: Vec::with_capacity(128),
            matrix: BasicMatrix::with_cols(0),
            discovered: HashSet::with_capacity(64),
            bfs_queue: VecDeque::with_capacity(64),
        }
    }

    /// Returns the shape table held by this placement search.
    pub fn shape_table(&self) -> &'s ShapeTable {
        self.stbl
    }

    /// Returns an iterator over the placements previously computed.
    pub fn placements<'a>(&'a self) -> impl Iterator<Item = &'a Place> {
        self.placements.iter()
    }

    /// Recomputes all possible next placements for `state`, which can be later accessed via
    /// `self.placements()`.
    pub fn compute(&mut self, state: &State) {
        self.placements.clear();
        self.set_matrix(state.matrix());
        for (color, did_hold) in avail_colors(state) {
            for norm_id in self.stbl.iter_norms_by_color(color) {
                self.set_shape(norm_id, did_hold);
                self.compute_shape();
            }
        }
    }

    fn set_matrix(&mut self, mat: &BasicMatrix) {
        self.matrix.clone_from(mat);
    }

    fn set_shape(&mut self, norm_id: NormalShapeId, did_hold: bool) {
        let norm = &self.stbl[norm_id];
        let matrix = &self.matrix;
        let rightmost_col = matrix.cols() - norm.cols();
        self.discovered.clear();
        self.bfs_queue.clear();
        self.bfs_queue.extend((0..=rightmost_col).map(|col| {
            // use bottom-surface-of-shape + top-surface-of-matrix to quickly determine
            // where it will land due to gravity.
            let row = (0..norm.cols())
                .map(|j| matrix.col_height(col + j).saturating_sub(norm.bottom(j)))
                .max()
                .expect("bug: 0 column shape");
            let mut pm = Place::new(norm_id, (row, col));
            pm.did_hold = did_hold;
            pm
        }));
    }

    fn compute_shape(&mut self) {
        while let Some(pm) = self.bfs_queue.pop_front() {
            if self.discovered.contains(&pm.coord) || pm.intersects(self.stbl, &self.matrix) {
                continue;
            }

            for &input in &[Input::Left, Input::Right] {
                let mut pm = pm.clone();
                if pm.mid_air {
                    pm.tap(Input::SD);
                }
                pm.tap(input);
                pm.fall(self.stbl, &self.matrix);
                self.bfs_queue.push_back(pm);
            }

            self.discovered.insert(pm.coord);
            self.placements.push(pm);
        }
    }
}

fn avail_colors(state: &State) -> impl Iterator<Item = (Color, bool)> {
    let (col_nh, mut col_h) = state.next();
    if col_nh == col_h {
        // don't use hold if identical to current piece
        col_h = None;
    }
    let nh = col_nh.into_iter().map(|c| (c, false));
    let h = col_h.into_iter().map(|c| (c, true));
    nh.chain(h)
}

//////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use super::*;
    use crate::{basic_matrix, shape::srs, Color, Snapshot};

    fn colors(hold: char, queue: &str) -> Vec<(char, bool)> {
        let snapshot = Snapshot {
            hold: std::convert::TryFrom::try_from(hold).ok(),
            queue: queue.chars().map(Color).collect(),
            matrix: BasicMatrix::with_cols(1),
        };
        avail_colors(&snapshot.into())
            .map(|(c, h)| (c.0, h))
            .collect()
    }

    #[test]
    fn test_avail_colors() {
        assert_eq!(colors('.', "TJZ"), [('T', false), ('J', true)]);
        assert_eq!(colors('I', "TJZ"), [('T', false), ('I', true)]);
        assert_eq!(colors('I', "IJZ"), [('I', false)]);
        assert_eq!(colors('.', "T"), [('T', false)]);
        assert_eq!(colors('O', ""), [('O', true)]);
        assert_eq!(colors('.', ""), []);
    }

    #[test]
    fn test_sonic_drop() {
        // . x x . .
        // . x x x x
        let mat = basic_matrix![
            [false, true, true, true, true],
            [false, true, true, false, false]
        ];
        let srs = srs();

        let mut ps = PlacementSearch::new(&srs);
        ps.set_matrix(&mat);
        let mut sonic_drop = |norm_id, col| {
            ps.set_shape(norm_id, false);
            ps.bfs_queue
                .iter()
                .find(|pm| pm.col() == col)
                .expect("did not drop to requested column")
                .row()
        };

        let i0 = srs.iter_norms_by_color(Color('I')).nth(0).unwrap();
        let i1 = srs.iter_norms_by_color(Color('I')).nth(1).unwrap();
        assert_eq!(sonic_drop(i0, 0), 2);
        assert_eq!(sonic_drop(i1, 0), 0);
        assert_eq!(sonic_drop(i1, 1), 2);

        let z0 = srs.iter_norms_by_color(Color('Z')).nth(0).unwrap();
        let z1 = srs.iter_norms_by_color(Color('Z')).nth(1).unwrap();
        assert_eq!(sonic_drop(z0, 2), 1);
        assert_eq!(sonic_drop(z0, 1), 2);
        assert_eq!(sonic_drop(z1, 0), 1);
        assert_eq!(sonic_drop(z1, 1), 2);
        assert_eq!(sonic_drop(z1, 2), 2);
        assert_eq!(sonic_drop(z1, 3), 1);

        let s1 = srs.iter_norms_by_color(Color('S')).nth(1).unwrap();
        assert_eq!(sonic_drop(s1, 0), 2);

        let l1 = srs.iter_norms_by_color(Color('L')).nth(1).unwrap();
        let l2 = srs.iter_norms_by_color(Color('L')).nth(2).unwrap();
        assert_eq!(sonic_drop(l1, 1), 2);
        assert_eq!(sonic_drop(l2, 0), 1);
        assert_eq!(sonic_drop(l2, 2), 2);
    }

    fn coords(s: &mut PlacementSearch, norm_id: NormalShapeId) -> Vec<(u16, u16)> {
        s.placements.clear();
        s.set_shape(norm_id, false);
        s.compute_shape();
        let mut coords: Vec<_> = s.placements.iter().map(|pm| pm.coord).collect();
        coords.sort();
        coords
    }

    fn inputs(p: &Place) -> Vec<Input> {
        let srs = srs();
        crate::ai::finesse::inputs(&srs, p).collect()
    }

    #[test]
    fn test_basic_placements() {
        let (xx, __) = (true, false);
        let srs = srs();
        let t0 = srs.iter_norms_by_color(Color('T')).nth(0).unwrap();
        let t2 = srs.iter_norms_by_color(Color('T')).nth(2).unwrap();
        let i0 = srs.iter_norms_by_color(Color('I')).nth(0).unwrap();
        let i1 = srs.iter_norms_by_color(Color('I')).nth(1).unwrap();
        let s1 = srs.iter_norms_by_color(Color('S')).nth(1).unwrap();
        let mut s = PlacementSearch::new(&srs);
        // . . . . . x
        // . . . . . x
        // . . . x . x
        s.set_matrix(&basic_matrix![
            [__, __, __, xx, __, xx],
            [__, __, __, __, __, xx],
            [__, __, __, __, __, xx],
        ]);
        assert_eq!(coords(&mut s, t0), [(0, 0), (1, 1), (1, 2), (3, 3)]);
        assert_eq!(coords(&mut s, t2), [(0, 0), (0, 1), (1, 2), (2, 3)]);
        assert_eq!(coords(&mut s, i0), [(1, 0), (1, 1), (3, 2)]);
        assert_eq!(
            coords(&mut s, i1),
            [(0, 0), (0, 1), (0, 2), (0, 4), (1, 3), (3, 5)]
        );
        assert_eq!(coords(&mut s, s1), [(0, 0), (0, 1), (0, 3), (1, 2), (3, 4)]);
    }

    #[test]
    fn test_basic_placements_w_hold() {
        let (xx, __) = (true, false);
        let srs = srs();
        let t0 = srs.iter_norms_by_color(Color('T')).nth(0).unwrap();
        let t1 = srs.iter_norms_by_color(Color('T')).nth(1).unwrap();
        let t2 = srs.iter_norms_by_color(Color('T')).nth(2).unwrap();
        let t3 = srs.iter_norms_by_color(Color('T')).nth(3).unwrap();
        let o = srs.iter_norms_by_color(Color('O')).nth(0).unwrap();
        let mut s = PlacementSearch::new(&srs);

        s.compute(
            &Snapshot {
                matrix: basic_matrix![[__, __, xx]],
                queue: vec![Color('T')],
                hold: Some(Color('O')),
            }
            .into(),
        );

        let mut places: Vec<_> = s
            .placements()
            .map(|pm| (pm.norm_id, pm.row(), pm.col(), pm.did_hold))
            .collect();
        places.sort();
        assert_eq!(
            places,
            [
                (o, 0, 0, true),
                (o, 1, 1, true),
                (t0, 1, 0, false),
                (t1, 0, 0, false),
                (t1, 0, 1, false),
                (t2, 0, 0, false),
                (t3, 0, 0, false),
                (t3, 1, 1, false),
            ]
        );
    }

    #[test]
    fn test_tuck_easy() {
        use Input::*;
        let (xx, __) = (true, false);
        let srs = srs();
        let t0 = srs.iter_norms_by_color(Color('T')).nth(0).unwrap();
        let l0 = srs.iter_norms_by_color(Color('L')).nth(0).unwrap();

        let mut s = PlacementSearch::new(&srs);
        s.set_matrix(&basic_matrix![[__, __, __, __, __], [xx, __, __, __, __],]);
        assert_eq!(
            coords(&mut s, t0),
            [
                // x . . . .      x T . . .
                // . . . . .  ->  T T T . .
                //                  (0,0)
                (0, 0),
                (0, 1),
                (0, 2),
                (2, 0),
            ]
        );

        let pm = s.placements().find(|pm| pm.coord == (0, 1)).unwrap();
        assert_eq!(inputs(pm), [Left, Left]);

        let pm = s.placements().find(|pm| pm.coord == (0, 0)).unwrap();
        assert_eq!(inputs(pm), [Left, Left, SD, Left]);

        s.set_matrix(&basic_matrix![
            [__, __, __, __, __],
            [__, __, __, xx, __],
            [__, __, __, xx, __],
        ]);
        assert_eq!(
            coords(&mut s, t0),
            [
                (0, 0),
                // . . . x .      . . . x .
                // . . . x .      . . T x .
                // . . . . .  ->  . T T T .
                //                  (0,1)
                (0, 1),
                (3, 1),
                (3, 2),
            ]
        );
        let pm = s.placements().find(|pm| pm.coord == (0, 1)).unwrap();
        assert_eq!(inputs(pm), [Left, Left, Left, SD, Right]);

        // no tuck
        assert_eq!(coords(&mut s, l0), [(0, 0), (3, 1), (3, 2)]);
    }

    #[test]
    fn test_tuck_2_taps() {
        use Input::*;
        let (xx, __) = (true, false);
        let srs = srs();
        let l0 = srs.iter_norms_by_color(Color('L')).nth(0).unwrap();

        let mut s = PlacementSearch::new(&srs);
        s.set_matrix(&basic_matrix![[__, __, __, __, __], [xx, xx, __, __, __],]);
        assert_eq!(
            coords(&mut s, l0),
            [
                // x x . . .      x x . L .      x x L . .
                // . . . . .  ->  . L L L .  ->  L L L . .
                //                  (0,1)          (0,0)
                (0, 0),
                (0, 1),
                (0, 2),
                (2, 0),
                (2, 1),
            ]
        );

        let pm = s.placements().find(|pm| pm.coord == (0, 1)).unwrap();
        assert_eq!(inputs(pm), [Left, SD, Left]);
        let pm = s.placements().find(|pm| pm.coord == (0, 0)).unwrap();
        assert_eq!(inputs(pm), [Left, SD, Left, Left]);
    }

    #[test]
    fn test_tuck_double_sd() {
        use Input::*;
        let (xx, __) = (true, false);
        let srs = srs();
        let o = srs.iter_norms_by_color(Color('O')).nth(0).unwrap();

        let mut s = PlacementSearch::new(&srs);
        s.set_matrix(&basic_matrix![
            [__, __, __, xx, __],
            [__, __, __, xx, xx],
            [__, __, __, __, __],
            [__, __, __, __, __],
            [xx, xx, xx, __, __],
        ]);
        assert_eq!(
            coords(&mut s, o),
            [
                // x x x . .      x x x . .      x x x . .      x x x . .
                // . . . . .      . . O O .      . . . . .      . . . . .
                // . . . . .  ->  . . O O .  ->  . . . . .  ->  . . . . .
                // . . . x x      . . . x x      . O O x x      O O . x x
                // . . . x .      . . . x .      . O O x .      O O . x .
                //                  (2,2)          (0,1)          (0,0)
                //                SD,L           SD,L,L         SD,L,L,SD,L
                (0, 0),
                (0, 1),
                (2, 2),
                (2, 3),
                (5, 0),
                (5, 1),
                (5, 2),
            ]
        );

        let pm = s.placements().find(|pm| pm.coord == (2, 2)).unwrap();
        assert_eq!(inputs(pm), [Left, SD, Left]);
        let pm = s.placements().find(|pm| pm.coord == (0, 1)).unwrap();
        assert_eq!(inputs(pm), [Left, SD, Left, Left]);
        let pm = s.placements().find(|pm| pm.coord == (0, 0)).unwrap();
        assert_eq!(inputs(pm), [Left, SD, Left, Left, SD, Left]);
    }

    #[test]
    fn test_tuck_ambiguous() {
        use Input::*;
        let (xx, __) = (true, false);
        let srs = srs();
        let o = srs.iter_norms_by_color(Color('O')).nth(0).unwrap();

        let mut s = PlacementSearch::new(&srs);
        s.set_matrix(&basic_matrix![
            [__, __, __, __, __],
            [__, __, __, __, __],
            [__, __, xx, __, __],
        ]);
        assert_eq!(
            coords(&mut s, o),
            [
                // . . x . .      . . x . .      . . x . .
                // . . . . .  ->  . O O . .  ->  . . O O .
                // . . . . .      . O O . .      . . O O .
                //                  (0,1)          (0,2)
                //                SD,R           SD,R,R
                //                SD,L,L         SD,L
                (0, 0),
                (0, 1),
                (0, 2),
                (0, 3),
                (3, 1),
                (3, 2),
            ]
        );

        // TODO: use BFS so we get to the placement with least number of taps first;
        // however we aren't taking into account the number of finesse moves to get to
        // 'initial_col'.

        let pm = s.placements().find(|pm| pm.coord == (0, 1)).unwrap();
        assert_eq!(inputs(pm), [Left, Left, Left, Left, SD, Right]);

        let pm = s.placements().find(|pm| pm.coord == (0, 2)).unwrap();
        assert_eq!(inputs(pm), [Left, SD, Left]);
    }
}
