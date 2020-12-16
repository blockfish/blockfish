use crate::{BasicMatrix, Color, Orientation};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, ops::RangeInclusive};
use thiserror::Error;

/// Holds all of the shapes associated with some set of game rules.
#[derive(Serialize, Deserialize)]
pub struct ShapeTable {
    shapes: HashMap<Color, ShapeData>,
    matrices: Vec<BasicMatrix>,
    kick_tables: Vec<KickTable>,
}

impl ShapeTable {
    /// Finds the shape for color `color`, if it exists in the table.
    pub fn shape(&self, color: Color) -> Option<ShapeRef> {
        let data = self.shapes.get(&color)?;
        Some(ShapeRef(&self, data, color))
    }
}

#[derive(Serialize, Deserialize)]
struct ShapeData {
    #[serde(rename = "j0")]
    spawn_col: i16,
    #[serde(rename = "rs")]
    orientations: [OrientationInfo; 4],
    #[serde(rename = "kt")]
    kick_table_index: usize,
}

#[derive(Default, Copy, Clone, Serialize, Deserialize)]
struct OrientationInfo {
    #[serde(rename = "o")]
    offset: (i16, i16),
    #[serde(rename = "m")]
    matrix_index: usize,
}

/// References a shape in a shape table
// TODO: make this repr smaller?
#[derive(Copy, Clone)]
pub struct ShapeRef<'a>(&'a ShapeTable, &'a ShapeData, Color);

/// A transformation (offset + rotation) to apply to a shape.
pub type Transform = (i16, i16, Orientation);

/// An application of a transform to a shape, normalized so that they are considered eq()
/// if they result in the same cells, even if they involved different orientations or
/// coordinates.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NormalizedShapeTransform(usize, i16, i16);

impl<'a> ShapeRef<'a> {
    /// Returns `true` if this shape intersects matrix `tgt` if transformed by `tf`.
    pub fn intersects(&self, tgt: &BasicMatrix, tf: Transform) -> bool {
        let (i, j, mat) = self.transform(tf);
        if i >= 0 && j >= 0 && (j as u16) + mat.cols() <= tgt.cols() {
            tgt.overlaps(mat, (i as u16, j as u16))
        } else {
            true
        }
    }

    /// Fills in the matrix `tgt` with this shape transformed by `tf`.
    ///
    /// Panics if the designed transformation puts this shape out of bounds. Note that
    /// this will never occur if `intersects()` was previously checked and returned
    /// `false`.
    pub fn blit_to(&self, tgt: &mut BasicMatrix, tf: Transform) {
        let (i, j, mat) = self.transform(tf);
        assert!(
            i >= 0 && j >= 0 && (j as u16) + mat.cols() <= tgt.cols(),
            "cannot blit shape to out-of-bounds location"
        );
        tgt.blit(mat, (i as u16, j as u16));
    }

    /// Returns the initial spawn column for this shape.
    pub fn spawn_col(&self) -> i16 {
        self.1.spawn_col
    }

    /// Returns the color of this shape.
    pub fn color(&self) -> Color {
        self.2
    }

    /// Returns the range of valid columns this shape can be placed at without going out
    /// of bounds.
    pub fn valid_cols(&self, r: Orientation, cols: u16) -> RangeInclusive<i16> {
        let (_, off, mat) = self.matrix(r);
        -(off as i16)..=((cols - mat.cols()) as i16 - off)
    }

    /// Returns the highest row position for this shape to rest on top of matrix `tgt`,
    /// when at column `j` and orientation `r`.
    pub fn peak(&self, tgt: &BasicMatrix, j: i16, r: Orientation) -> i16 {
        let mut i = tgt.rows() as i16;
        while !self.intersects(tgt, (i - 1, j, r)) {
            i -= 1;
        }
        i
    }

    /// Returns the normalized representation for this shape when transforming by `tf`.
    pub fn normalize(&self, tf: Transform) -> NormalizedShapeTransform {
        let mat_idx = self.1.orientations[tf.2 as usize].matrix_index;
        let (i, j, _) = self.transform(tf);
        NormalizedShapeTransform(mat_idx, i, j)
    }

    /// Returns the array of kick offsets for rotating from `r0` to `r1`.
    pub fn kicks(&self, r0: Orientation, r1: Orientation) -> &[(i16, i16)] {
        let kick_table = &self.0.kick_tables[self.1.kick_table_index];
        match kick_table.kicks.get(&Kick(r0, r1)) {
            Some(offsets) => &offsets,
            None => &[],
        }
    }

    /// Returns the matrix and its column/row offset for this shape at orientation `r`.
    #[inline(always)]
    fn matrix(&self, r: Orientation) -> (i16, i16, &'a BasicMatrix) {
        let r_info = &self.1.orientations[r as usize];
        let (i_off, j_off) = r_info.offset;
        let mat = &self.0.matrices[r_info.matrix_index];
        (i_off, j_off, mat)
    }

    /// Returns the absolute position (row, col) and corresponding matrix from rotating
    /// this shape by transform `tf`.
    #[inline(always)]
    fn transform(&self, tf: Transform) -> (i16, i16, &'a BasicMatrix) {
        let (i0, j0, r) = tf;
        let (i_off, j_off, mat) = self.matrix(r);
        (i0 + i_off, j0 + j_off, mat)
    }
}

#[derive(Default, Clone, Serialize, Deserialize)]
#[serde(transparent)]
struct KickTable {
    kicks: HashMap<Kick, Vec<(i16, i16)>>,
}

#[derive(Default, Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct Kick(Orientation, Orientation);

#[derive(Debug, Error)]
#[error("invalid kick entry format")]
struct ParseKickError;

impl Serialize for Kick {
    fn serialize<T>(&self, ser: T) -> Result<T::Ok, T::Error>
    where
        T: serde::Serializer,
    {
        format!("{}{}", self.0 as u8, self.1 as u8).serialize(ser)
    }
}

impl std::str::FromStr for Kick {
    type Err = ParseKickError;
    fn from_str(s: &str) -> Result<Self, ParseKickError> {
        if s.len() != 2 || s.chars().count() != 2 {
            return Err(ParseKickError);
        }
        let r0 = s[0..1].parse().map_err(|_| ParseKickError)?;
        let r0 = Orientation::from_u8(r0).ok_or(ParseKickError)?;
        let r1 = s[1..2].parse().map_err(|_| ParseKickError)?;
        let r1 = Orientation::from_u8(r1).ok_or(ParseKickError)?;
        Ok(Kick(r0, r1))
    }
}

impl<'de> Deserialize<'de> for Kick {
    fn deserialize<T>(de: T) -> Result<Self, T::Error>
    where
        T: serde::Deserializer<'de>,
    {
        std::borrow::Cow::<str>::deserialize(de)?
            .parse()
            .map_err(serde::de::Error::custom)
    }
}

/// Returns `(origin_row, origin_col, mat)` where `mat` is a normalized (no empty rows or
/// columns on the far sides) view of the coords, with `(origin_row, origin_col)` as the
/// origin.
#[cfg(any(test, feature = "gen-stbl"))]
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

#[cfg(feature = "gen-stbl")]
impl KickTable {
    /// Generate a kick table for a particular shape given a ruleset.
    fn from_ruleset(rules: &block_stacker::Ruleset, typ: block_stacker::PieceType) -> Self {
        let mut kicks = HashMap::new();
        for r in 0..4i32 {
            let ori = Orientation::from_u8(r as u8).unwrap();
            kicks.insert(Kick(ori, ori.ccw()), rules.kicks(typ, r, r - 1).collect());
            kicks.insert(Kick(ori, ori.cw()), rules.kicks(typ, r, r + 1).collect());
        }
        Self { kicks }
    }

    /// Convert to a `BTreeMap` (for normalized key order).
    fn to_btree_map(&self) -> std::collections::BTreeMap<Kick, Vec<(i16, i16)>> {
        self.kicks
            .iter()
            .map(|(&kick, offsets)| (kick, offsets.clone()))
            .collect()
    }
}

#[cfg(feature = "gen-stbl")]
impl ShapeTable {
    /// Generate a shape table from the given game ruleset.
    pub fn from_ruleset(rules: &block_stacker::Ruleset) -> Self {
        use std::convert::TryInto;

        let mut shapes = HashMap::new();
        let mut matrices = vec![];
        let mut matrix_lookup = HashMap::new();
        let mut kick_tables = vec![];
        let mut kick_table_lookup = HashMap::new();
        let mut coords = Vec::with_capacity(4);

        for typ in rules.types() {
            let (_, spawn_col) = rules.spawn(typ);

            // get matrix representations for each orientation
            let mut orientations = [OrientationInfo::default(); 4];
            for r in 0..4 {
                coords.clear();
                coords.extend(rules.coords(typ, r));
                let (i0, j0, matrix) = normalize_coords(&coords);
                let mut rs = &mut orientations[r as usize];
                rs.offset = (i0 as i16, j0 as i16);
                // deduplicate identical matrices
                rs.matrix_index = *matrix_lookup.entry(matrix.clone()).or_insert_with(|| {
                    matrices.push(matrix);
                    matrices.len() - 1
                });
            }

            // generate kick table representation
            let kick_table = KickTable::from_ruleset(rules, typ);
            // deduplicate identical kick tables
            let kick_table_index = *kick_table_lookup
                .entry(kick_table.to_btree_map())
                .or_insert_with(|| {
                    kick_tables.push(kick_table);
                    kick_tables.len() - 1
                });

            // convert PieceType to our color representation
            let color = typ
                .try_into()
                .expect("bug: ruleset has invalid shape color");

            let shape = ShapeData {
                spawn_col,
                orientations,
                kick_table_index,
            };
            shapes.insert(color, shape);
        }

        Self {
            shapes,
            matrices,
            kick_tables,
        }
    }
}

static SRS_BYTES: &[u8] = include_bytes!("../../support/test/srs-shape-table.json");

/// Returns a new copy of the SRS shape table.
pub fn srs() -> ShapeTable {
    serde_json::from_slice(SRS_BYTES).expect("BUG: SRS data is malformed!")
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::basic_matrix;

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
    fn test_srs_col_range() {
        let srs = srs();
        for col in "SZTJ".chars().map(Color) {
            // . 0 1 2 3 4 5 6 7 8 9 .
            // -----------------------
            //   x x .         x x .
            //   . x x         . x x
            // -----------------------
            // . . x           . . x
            // . x x           . x x
            // . x .           . x .
            // -----------------------
            //   . x .           . x .
            //   x x .           x x .
            //   x . .           x . .
            let sh = srs.shape(col).unwrap();
            assert_eq!(sh.valid_cols(Orientation::R0, 10), 0..=7);
            assert_eq!(sh.valid_cols(Orientation::R1, 10), -1..=7);
            assert_eq!(sh.valid_cols(Orientation::R2, 10), 0..=7);
            assert_eq!(sh.valid_cols(Orientation::R3, 10), 0..=8);
        }

        let i = srs.shape(Color('I')).unwrap();
        assert_eq!(i.valid_cols(Orientation::R0, 10), 0..=6);
        assert_eq!(i.valid_cols(Orientation::R1, 10), -2..=7);
        assert_eq!(i.valid_cols(Orientation::R2, 10), 0..=6);
        assert_eq!(i.valid_cols(Orientation::R3, 10), -1..=8);

        // . 0 1 2 3 4 5 6 7 8 9 .
        // -----------------------
        // . x x .         . x x .
        // . x x .         . x x .
        let o = srs.shape(Color('O')).unwrap();
        assert_eq!(o.valid_cols(Orientation::R0, 10), -1..=7);
        assert_eq!(o.valid_cols(Orientation::R1, 10), -1..=7);
        assert_eq!(o.valid_cols(Orientation::R2, 10), -1..=7);
        assert_eq!(o.valid_cols(Orientation::R3, 10), -1..=7);
    }

    #[test]
    fn test_parse_kick() {
        use serde::de::value::*;
        assert_eq!(
            "01".parse::<Kick>().unwrap(),
            Kick(Orientation::R0, Orientation::R1)
        );
        assert_eq!(
            "32".parse::<Kick>().unwrap(),
            Kick(Orientation::R3, Orientation::R2)
        );
        let de = BorrowedStrDeserializer::<Error>::new("30");
        assert_eq!(
            Kick::deserialize(de).unwrap(),
            Kick(Orientation::R3, Orientation::R0)
        );
    }

    #[test]
    fn test_srs_peak() {
        let (xx, __) = (true, false);
        let mat = basic_matrix![
            // . . . . . . .
            // . . x . . . .
            // x x x . . x .
            [xx, xx, xx, __, __, xx, __],
            [__, __, xx, __, __, __, __],
        ];
        let srs = srs();
        let s = srs.shape(Color('S')).unwrap();
        let peaks = |js: RangeInclusive<_>, r| js.map(|j| s.peak(&mat, j, r)).collect::<Vec<_>>();
        assert_eq!(peaks(0..=4, Orientation::R0), [0, 1, 1, -1, 0]);
        assert_eq!(peaks(-1..=4, Orientation::R1), [1, 2, 1, 0, 1, 0]);
        assert_eq!(peaks(0..=4, Orientation::R2), [1, 2, 2, 0, 1]);
        assert_eq!(peaks(0..=5, Orientation::R3), [1, 2, 1, 0, 1, 0]);
    }

    #[test]
    fn test_srs_peak_o() {
        let (xx, __) = (true, false);
        let mat = basic_matrix![
            [__, __, __, xx, __],
            [__, __, __, xx, xx],
            [__, __, __, __, __],
            [__, __, __, __, __],
            [xx, xx, xx, __, __],
        ];
        let srs = srs();
        let o = srs.shape(Color('O')).unwrap();
        let peaks = |js: RangeInclusive<_>, r| js.map(|j| o.peak(&mat, j, r)).collect::<Vec<_>>();
        assert!(o.intersects(&mat, (3, 0, Orientation::R0)));
        assert!(!o.intersects(&mat, (4, 0, Orientation::R0)));
        assert_eq!(peaks(-1..=2, Orientation::R0), [4, 4, 4, 1]);
    }

    #[test]
    fn test_srs_peak_cheese() {
        let (xx, __) = (true, false);
        let mat = basic_matrix![
            [xx, xx, xx, xx, xx, xx, xx, xx, __, xx],
            [xx, __, xx, xx, xx, xx, xx, xx, xx, xx],
            [xx, xx, xx, xx, xx, xx, xx, __, xx, xx],
            [xx, xx, xx, xx, xx, xx, xx, xx, xx, __],
            [xx, xx, xx, xx, xx, __, xx, xx, xx, xx],
            [xx, xx, xx, xx, xx, xx, xx, __, xx, xx],
        ];
        let srs = srs();
        let s = srs.shape(Color('S')).unwrap();
        assert_eq!(
            (0..=7)
                .map(|j| s.peak(&mat, j, Orientation::R0))
                .collect::<Vec<_>>(),
            [5, 5, 5, 5, 5, 5, 5, 5]
        );
    }

    #[test]
    fn test_normalize() {
        use crate::Orientation::*;
        let srs = srs();
        for color in "SZ".chars().map(Color) {
            let s = srs.shape(color).unwrap();
            assert_eq!(s.normalize((5, 5, R0)), s.normalize((6, 5, R2)));
            assert_eq!(s.normalize((5, 5, R1)), s.normalize((5, 6, R3)));
        }
        let o = srs.shape(Color('O')).unwrap();
        for &r in &[R0, R1, R2, R3] {
            assert_eq!(o.normalize((5, 5, R0)), o.normalize((5, 5, r)));
        }
        let i = srs.shape(Color('I')).unwrap();
        assert_eq!(i.normalize((5, 5, R0)), i.normalize((6, 5, R2)));
        assert_eq!(i.normalize((5, 5, R1)), i.normalize((5, 6, R3)));
    }
}
