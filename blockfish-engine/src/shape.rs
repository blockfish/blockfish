use std::{collections::BTreeMap, convert::TryInto as _};

use crate::{
    basic_matrix,
    common::{Color, Input, Orientation, RepeatedInputs},
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

    /// Computes the minimum input sequence to move this shape from spawn to column
    /// `targ_col`.
    // TODO: DAS-finesse
    pub fn finesse(&self, targ_col: u16) -> impl Iterator<Item = Input> {
        self.spawn_col
            .iter()
            .map(|(&ori, &spawn_col)| {
                (
                    RepeatedInputs::rotate(ori),
                    RepeatedInputs::horizontal(spawn_col, targ_col),
                )
            })
            .min_by_key(|(r, h)| r.len() + h.len())
            .map(|(r, h)| r.chain(h))
            .expect("unreachable: shape w/o orientations")
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

    fn all_finesse(norm: &NormalShape) -> Vec<Vec<Input>> {
        let num_cols = 10 - norm.cols() + 1;
        (0..num_cols)
            .map(|col| norm.finesse(col).collect())
            .collect()
    }

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
    fn test_i_finesse() {
        use Input::*;
        let srs = srs();
        let mut i_ids = srs.iter_norms_by_color(Color('I'));
        let i0 = &srs[i_ids.next().unwrap()];
        let i1 = &srs[i_ids.next().unwrap()];
        assert_eq!(i0.matrix.rows(), 1);
        assert_eq!(i0.matrix.cols(), 4);
        assert_eq!(i1.matrix.rows(), 4);
        assert_eq!(i1.matrix.cols(), 1);
        assert_eq!(
            all_finesse(i0),
            vec![
                vec![Left, Left, Left],
                vec![Left, Left],
                vec![Left],
                vec![],
                vec![Right],
                vec![Right, Right],
                vec![Right, Right, Right],
            ]
        );
        assert_eq!(
            all_finesse(i1),
            vec![
                vec![CCW, Left, Left, Left, Left],
                vec![CCW, Left, Left, Left],
                vec![CCW, Left, Left],
                vec![CCW, Left],
                vec![CCW],
                vec![CW],
                vec![CW, Right],
                vec![CW, Right, Right],
                vec![CW, Right, Right, Right],
                vec![CW, Right, Right, Right, Right],
            ]
        );
    }

    #[test]
    fn test_sz_finesse() {
        use Input::*;
        let srs = srs();
        for &color in [Color('S'), Color('Z')].iter() {
            let mut sz_ids = srs.iter_norms_by_color(color);
            let sz0 = &srs[sz_ids.next().unwrap()];
            let sz1 = &srs[sz_ids.next().unwrap()];
            assert_eq!(sz0.matrix.rows(), 2);
            assert_eq!(sz0.matrix.cols(), 3);
            assert_eq!(sz1.matrix.rows(), 3);
            assert_eq!(sz1.matrix.cols(), 2);
            assert_eq!(
                all_finesse(sz0),
                vec![
                    vec![Left, Left, Left],
                    vec![Left, Left],
                    vec![Left],
                    vec![],
                    vec![Right],
                    vec![Right, Right],
                    vec![Right, Right, Right],
                    vec![Right, Right, Right, Right],
                ]
            );
            assert_eq!(
                all_finesse(sz1),
                vec![
                    vec![CCW, Left, Left, Left],
                    vec![CCW, Left, Left],
                    vec![CCW, Left],
                    vec![CCW],
                    vec![CW],
                    vec![CW, Right],
                    vec![CW, Right, Right],
                    vec![CW, Right, Right, Right],
                    vec![CW, Right, Right, Right, Right],
                ]
            );
        }
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
