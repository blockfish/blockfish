use crate::{
    ai::State,
    shape::{NormalShapeId, ShapeTable},
    BasicMatrix, Color,
};

/// Represents a placement of a particular shape.
#[derive(Clone)]
pub struct Place {
    pub norm_id: NormalShapeId,
    pub did_hold: bool,
    pub row: u16,
    pub col: u16,
}

impl Place {
    pub fn new(norm_id: NormalShapeId, (row, col): (u16, u16)) -> Self {
        Place {
            norm_id,
            row,
            col,
            did_hold: false,
        }
    }
}

/// Data structure used to search for placements and aggregating the results.
pub struct PlacementSearch<'s> {
    stbl: &'s ShapeTable,
    placements: Vec<Place>,
    col_height: Vec<u16>,
    top_row: Vec<u16>,
}

impl<'s> PlacementSearch<'s> {
    /// Construct a new placement search using shape table `stbl`.
    pub fn new(stbl: &'s ShapeTable) -> Self {
        Self {
            stbl,
            placements: Vec::with_capacity(128),
            col_height: Vec::with_capacity(10),
            top_row: Vec::with_capacity(10),
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
                self.compute_shape(norm_id, did_hold);
            }
        }
    }

    fn set_matrix(&mut self, mat: &BasicMatrix) {
        let heights = (0..mat.cols()).map(|j| mat.col_height(j));
        self.col_height.clear();
        self.col_height.extend(heights);
    }

    fn set_shape(&mut self, norm_id: NormalShapeId) {
        let norm = &self.stbl[norm_id];
        assert!(norm.cols() <= self.matrix_cols(), "matrix too small");
        // compute "top" rows (all spaces above unobstructed)
        let rightmost_col = self.matrix_cols() - norm.cols();
        let col_height = &self.col_height;
        let rows = (0..=rightmost_col).map(|col| {
            (0..norm.cols())
                .map(|j| col_height[(col + j) as usize].saturating_sub(norm.bottom(j)))
                .max()
                .expect("bug: 0 column shape")
        });
        self.top_row.clear();
        self.top_row.extend(rows);
    }

    fn matrix_cols(&self) -> u16 {
        self.col_height.len() as u16
    }

    fn rightmost_col(&self) -> u16 {
        (self.top_row.len() - 1) as u16
    }

    fn compute_shape(&mut self, norm_id: NormalShapeId, did_hold: bool) {
        self.set_shape(norm_id);
        let top_row = &self.top_row;
        for col in 0..=self.rightmost_col() {
            let row = top_row[col as usize];
            let mut place = Place::new(norm_id, (row, col));
            place.did_hold = did_hold;
            self.placements.push(place.clone());
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
            ps.set_shape(norm_id);
            ps.top_row[col as usize]
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
        s.compute_shape(norm_id, false);
        let mut pms: Vec<_> = s.placements.iter().map(|p| (p.row, p.col)).collect();
        pms.sort();
        pms
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
}
