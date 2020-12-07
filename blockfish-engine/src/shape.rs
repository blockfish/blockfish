use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

use crate::{
    common::{Color, Orientation},
    matrix::BasicMatrix,
};

/// Represents the "normalized" form of a polymino, which can be achieved
/// by one or more rotations. In particular, the S, Z and I pieces have two distinct
/// normal forms, the O piece has only 1 normal form, and the T, J, and L pieces have 4
/// distinct normal forms.
#[derive(Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct NormalShape {
    /// The matrix representation of the shape.
    matrix: BasicMatrix,
    /// Bit mask for each of the orientations that lead to this normalized shape. For
    /// instance, the S piece shares a norm for the R0/R2 orientations, as well as R1/R3.
    oris: u8,
    /// Initial spawn column for this shape at each orientation.
    spawn_col: [u16; 4],
}

const ALL_ORIENTATIONS: [Orientation; 4] = {
    use Orientation::*;
    [R0, R1, R2, R3]
};

impl NormalShape {
    /// Construct a new `NormalShape`.
    fn new(matrix: BasicMatrix) -> Self {
        Self {
            matrix,
            oris: 0u8,
            spawn_col: [0; 4],
        }
    }

    /// Sets the spawn location of this shape to be at `col` for orientation `ori`. Also
    /// adds `ori` to the list of valid orientations for this norm (i.e., returned by
    /// `.orientations()`).
    fn set_spawn(&mut self, ori: Orientation, col: u16) {
        self.oris |= 1 << ori as u8;
        self.spawn_col[ori as usize] = col;
    }

    /// Returns the width of this shape.
    pub fn cols(&self) -> u16 {
        self.matrix.cols()
    }

    /// Returns the height of this shape.
    pub fn rows(&self) -> u16 {
        self.matrix.rows()
    }

    /// Returns the row corresponding to the lowest occupied square at column `j` for this
    /// shape.
    pub fn bottom(&self, j: u16) -> u16 {
        (0..self.rows())
            .find(|&i| self.matrix.get((i, j)))
            .expect("bug: unoccupied column")
    }

    /// Returns the orientations that lead to this normalized form.
    pub fn orientations<'a>(&'a self) -> impl Iterator<Item = Orientation> + 'a {
        let oris = self.oris;
        ALL_ORIENTATIONS
            .iter()
            .cloned()
            .filter(move |&ori| oris & (1 << (ori as u8)) != 0)
    }

    /// Returns the absolute column of this piece after rotating to the given orientation
    /// directly after spawn. For instance, the S and Z pieces are at different columns in
    /// the R1 and R3 orientations.
    pub fn spawn_col(&self, ori: Orientation) -> u16 {
        self.spawn_col[ori as usize]
    }

    /// Blits this shape onto the matrix `tgt` at position with origin `pos`.
    pub fn blit_to(&self, tgt: &mut BasicMatrix, pos: (u16, u16)) {
        tgt.blit(&self.matrix, pos)
    }

    /// Returnst rue if this shape intersects with matrix matrix `tgt` at position with
    /// origin `pos`.
    pub fn intersects(&self, tgt: &BasicMatrix, pos: (u16, u16)) -> bool {
        tgt.overlaps(&self.matrix, pos)
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Tables

/// Table containing all of the available shapes in the rotation system, in their
/// normalized forms.
#[derive(Serialize, Deserialize)]
#[serde(transparent)]
pub struct ShapeTable {
    by_color: BTreeMap<Color, Vec<NormalShape>>,
}

impl ShapeTable {
    /// Lists all `NormalShapeId`s for color `Color`.
    pub fn iter_norms_by_color(&self, color: Color) -> impl Iterator<Item = NormalShapeId> {
        let len = self.by_color.get(&color).map(Vec::len).unwrap_or(0);
        (0..len).map(move |idx| NormalShapeId(color, idx))
    }
}

impl std::ops::Index<NormalShapeId> for ShapeTable {
    type Output = NormalShape;
    fn index(&self, id: NormalShapeId) -> &NormalShape {
        self.by_color
            .get(&id.0)
            .and_then(|norms| norms.get(id.1))
            .expect("invalid shape id")
    }
}

/// Lightweight reference to a normalized shape in a `ShapeTable`. This ID can be used to
/// uniquely represent a particular shape without passing around the pointer to the shape
/// itself. It is an error to mix shape ID's between tables that are not identical.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct NormalShapeId(Color, usize);

impl NormalShapeId {
    // pub fn color(&self) -> Color {
    //     self.0
    // }
}

impl std::fmt::Debug for NormalShapeId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}:{}", self.0, self.1)
    }
}

/// Returns `(origin_row, origin_col, mat)` where `mat` is a normalized (no empty rows or
/// columns on the far sides) view of the coords, with `(origin_row, origin_col)` as the
/// origin.
fn normalize_coords(coords: &[(u16, u16)]) -> (u16, u16, BasicMatrix) {
    assert!(!coords.is_empty());
    let min_row = coords.iter().map(|&(i, _)| i).min().unwrap();
    let min_col = coords.iter().map(|&(_, j)| j).min().unwrap();
    let max_col = coords.iter().map(|&(_, j)| j).max().unwrap();
    let mut mat = BasicMatrix::with_cols(max_col + 1 - min_col);
    for &(row, col) in coords.iter() {
        mat.set((row - min_row, col - min_col));
    }
    (min_row, min_col, mat)
}

/// Returns the set of normalized shapes resulting from viewing a shape by its at
/// different rotations. The provided closure `get_coords` is called to get the
/// coordinates at a particular rotation.
fn normal_shapes<F, I>(get_coords: F) -> Vec<NormalShape>
where
    F: Fn(Orientation) -> I,
    I: IntoIterator<Item = (u16, u16)>,
{
    let mut norms = vec![];
    let mut norm_idx = std::collections::HashMap::new();
    let mut coords = vec![];

    for &ori in ALL_ORIENTATIONS.iter() {
        coords.clear();
        coords.extend(get_coords(ori));
        let (_, col, mat) = normalize_coords(&coords);
        let idx = *norm_idx.entry(mat.clone()).or_insert_with(|| {
            let norm = NormalShape::new(mat);
            norms.push(norm);
            norms.len() - 1
        });
        norms[idx].set_spawn(ori, col);
    }

    norms
}

#[cfg(feature = "block-stacker")]
impl ShapeTable {
    #[allow(unused)]
    pub fn from_ruleset(rules: &block_stacker::Ruleset) -> Self {
        use std::convert::TryFrom;
        let by_color = rules
            .types()
            .flat_map(|typ| {
                let (i0, j0) = rules.spawn(typ);
                let color = Color::try_from(typ).ok()?;
                let shapes = normal_shapes(|rot| {
                    rules
                        .coords(typ, rot.as_i32())
                        .map(|(i, j)| (i + i0 as u16, j + j0 as u16))
                });
                Some((color, shapes))
            })
            .collect();

        Self { by_color }
    }
}

static SRS_BYTES: &[u8] = include_bytes!("../../support/test/srs-shape-table.json");

/// Returns a new copy of the SRS shape table.
pub fn srs() -> ShapeTable {
    serde_json::from_slice(SRS_BYTES).expect("BUG: SRS data is malformed!")
}

//////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    pub use super::*;
    use crate::basic_matrix;

    #[test]
    fn test_srs_count() {
        let srs = srs();
        assert_eq!(srs.iter_norms_by_color(Color('L')).count(), 4);
        assert_eq!(srs.iter_norms_by_color(Color('J')).count(), 4);
        assert_eq!(srs.iter_norms_by_color(Color('T')).count(), 4);
        assert_eq!(srs.iter_norms_by_color(Color('O')).count(), 1);
        assert_eq!(srs.iter_norms_by_color(Color('I')).count(), 2);
        assert_eq!(srs.iter_norms_by_color(Color('S')).count(), 2);
        assert_eq!(srs.iter_norms_by_color(Color('Z')).count(), 2);
    }

    #[test]
    fn test_bottom() {
        let srs = srs();
        let t2 = srs.iter_norms_by_color(Color('T')).nth(2).unwrap();
        assert_eq!(srs[t2].bottom(0), 1);
        assert_eq!(srs[t2].bottom(1), 0);
        assert_eq!(srs[t2].bottom(2), 1);

        let s1 = srs.iter_norms_by_color(Color('S')).nth(1).unwrap();
        assert_eq!(srs[s1].bottom(0), 1);
        assert_eq!(srs[s1].bottom(1), 0);

        let z1 = srs.iter_norms_by_color(Color('Z')).nth(1).unwrap();
        assert_eq!(srs[z1].bottom(0), 0);
        assert_eq!(srs[z1].bottom(1), 1);
    }

    #[test]
    fn test_normalize_coords() {
        let (i, j, mat) = normalize_coords(&[
            // - - x x x
            // - - . . x
            // - - - - -
            (2, 2),
            (1, 4),
            (2, 4),
            (2, 3),
        ]);
        assert_eq!((i, j), (1, 2));
        assert_eq!(mat, basic_matrix![[false, false, true], [true, true, true]]);
    }

    #[test]
    fn test_normal_shapes_z() {
        let shapes = normal_shapes(|o| match o {
            // . . . Z Z . . . . .
            // . . . . Z Z . . . .
            // . . . . . . . . . .
            Orientation::R0 => vec![(1, 4), (1, 5), (2, 3), (2, 4)],
            // . . . . . Z . . . .
            // . . . . Z Z . . . .
            // . . . . Z . . . . .
            Orientation::R1 => vec![(0, 4), (1, 4), (1, 5), (2, 5)],
            // . . . . . . . . . .
            // . . . Z Z . . . . .
            // . . . . Z Z . . . .
            Orientation::R2 => vec![(0, 4), (0, 5), (1, 3), (1, 4)],
            // . . . . Z . . . . .
            // . . . Z Z . . . . .
            // . . . Z . . . . . .
            Orientation::R3 => vec![(0, 3), (1, 3), (1, 4), (2, 4)],
        });
        let srs = srs();
        let srs_zs = srs.by_color.get(&Color('Z')).unwrap();
        assert_eq!(&shapes, srs_zs);
    }

    #[cfg(feature = "block-stacker")]
    #[test]
    fn test_block_stacker_srs() {
        let stbl = ShapeTable::from_ruleset(&block_stacker::Ruleset::guideline());
        let srs = srs();
        for color in "OILJTSZ".chars().map(Color) {
            assert_eq!(
                stbl.by_color.get(&color),
                srs.by_color.get(&color),
                "same as SRS: {:?}",
                color
            );
        }
    }
}
