use std::{collections::BTreeMap, convert::TryInto as _};

use crate::{
    basic_matrix,
    common::{Color, Orientation},
    matrix::BasicMatrix,
};

//////////////////////////////////////////////////////////////////////////////////////////
// Shapes

/// Represents the "normalized" form of a polymino, which can be achieved
/// by one or more rotations. In particular, the S, Z and I pieces have two distinct
/// normal forms, the O piece has only 1 normal form, and the T, J, and L pieces have 4
/// distinct normal forms.
// TODO: Eq and Hash
#[derive(Clone, Debug)]
pub struct NormalShape {
    /// The matrix representation of the shape.
    matrix: BasicMatrix,
    /// Mapping from each of the orientations that lead to this normalized shape, to the
    /// column it spawns at, at that orientation. For instance, the S and Z pieces are at
    /// different columns in the R1 and R3 orientations, even though these orientations
    /// share a normalized form.
    // TODO: lighter representation of this mapping, esp. consider there are only 4
    // possible orientations.
    spawn_col: BTreeMap<Orientation, u16>,
}

impl NormalShape {
    /// Construct a new `NormalShape`.
    fn new(matrix: BasicMatrix, spawn_col: BTreeMap<Orientation, u16>) -> Self {
        assert!(!spawn_col.is_empty());
        NormalShape { matrix, spawn_col }
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
        self.spawn_col.keys().cloned()
    }

    /// Returns the absolute column of this piece after rotating to the given orientation
    /// directly after spawn. For instance, the S and Z pieces are at different columns in
    /// the R1 and R3 orientations.
    pub fn spawn_col(&self, ori: Orientation) -> u16 {
        *self
            .spawn_col
            .get(&ori)
            .expect("invalid orientation for this shape")
    }

    /// Blit this shape onto the matrix `tgt` with origin `(i,j)`.
    pub fn blit_to(&self, tgt: &mut BasicMatrix, i: u16, j: u16) {
        tgt.blit(&self.matrix, (i, j))
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Tables

/// Table containing all of the available shapes in the rotation system, in their
/// normalized forms.
pub struct ShapeTable {
    by_color: BTreeMap<Color, Vec<NormalShape>>,
}

impl ShapeTable {
    /// Constructs an empty new `ShapeTable`.
    fn new() -> Self {
        ShapeTable {
            by_color: BTreeMap::new(),
        }
    }

    /// Inserts a `NormalShape` into the table, returning an ID that can be used to refer
    /// to the shape later.
    fn insert_norm(&mut self, color: Color, norm: NormalShape) -> NormalShapeId {
        let norms = self.by_color.entry(color).or_default();
        let idx = norms.len();
        norms.push(norm);
        NormalShapeId(color, idx)
    }

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

//////////////////////////////////////////////////////////////////////////////////////////
// SRS

/// Returns a new copy of the shape table corresponding to the standard rotation system.
pub fn srs() -> ShapeTable {
    use Orientation::*;

    let mut shapes = ShapeTable::new();
    macro_rules! shape {
        ($col:expr => [$(($rots:expr, $mat:expr)),* $(,)?]) => {
            $({
                shapes.insert_norm(
                    ($col).try_into().unwrap(),
                    NormalShape::new(
                        $mat,
                        $rots.into_iter().collect(),
                    ),
                );
            })*
        };
    }

    let xx = true;
    let __ = false;
    shape!('L' => [
        (vec![(R0, 3)], basic_matrix![[xx, xx, xx], [__, __, xx]]),
        (vec![(R1, 4)], basic_matrix![[xx, xx], [xx, __], [xx, __]]),
        (vec![(R2, 3)], basic_matrix![[xx, __, __], [xx, xx, xx]]),
        (vec![(R3, 3)], basic_matrix![[__, xx], [__, xx], [xx, xx]]),
    ]);
    shape!('J' => [
        (vec![(R0, 3)], basic_matrix![[xx, xx, xx], [xx, __, __]]),
        (vec![(R1, 4)], basic_matrix![[xx, __], [xx, __], [xx, xx]]),
        (vec![(R2, 3)], basic_matrix![[__, __, xx], [xx, xx, xx]]),
        (vec![(R3, 3)], basic_matrix![[xx, xx], [__, xx], [__, xx]]),
    ]);
    shape!('T' => [
        (vec![(R0, 3)], basic_matrix![[xx, xx, xx], [__, xx, __]]),
        (vec![(R1, 4)], basic_matrix![[xx, __], [xx, xx], [xx, __]]),
        (vec![(R2, 3)], basic_matrix![[__, xx, __], [xx, xx, xx]]),
        (vec![(R3, 3)], basic_matrix![[__, xx], [xx, xx], [__, xx]]),
    ]);
    shape!('O' => [
        (vec![(R0, 4), (R1, 4), (R2, 4), (R3, 4)],
         basic_matrix![[xx, xx], [xx, xx]])
    ]);
    shape!('I' => [
        (vec![(R0, 3), (R2, 3)], basic_matrix![[xx, xx, xx, xx]]),
        (vec![(R1, 5), (R3, 4)], basic_matrix![[xx], [xx], [xx], [xx]]),
    ]);
    shape!('S' => [
        (vec![(R0, 3), (R2, 3)], basic_matrix![[xx, xx, __], [__, xx, xx]]),
        (vec![(R1, 4), (R3, 3)], basic_matrix![[__, xx], [xx, xx], [xx, __]]),
    ]);
    shape!('Z' => [
        (vec![(R0, 3), (R2, 3)], basic_matrix![[__, xx, xx], [xx, xx, __]]),
        (vec![(R1, 4), (R3, 3)], basic_matrix![[xx, __], [xx, xx], [__, xx]]),
    ]);

    shapes
}

//////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    pub use super::*;

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
}
