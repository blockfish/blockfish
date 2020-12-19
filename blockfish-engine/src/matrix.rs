use serde::{de::Error, Deserialize, Serialize};
use std::{
    convert::{TryFrom, TryInto},
    ops::Range,
};
use thiserror::Error;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct BasicMatrix {
    /// Number of columns.
    cols: u16,
    /// Row data. Each row is represented by a bit mask of the occupied cells. All high
    /// bits after the first `cols` bits are set as well. This way e.g. a completely full
    /// row is represented by `std::u16::MAX`.
    data: Vec<u16>,
}

#[cfg(test)]
#[macro_export]
macro_rules! basic_matrix {
    [ $r0:tt ] => {
        basic_matrix![ $r0, ]
    };

    [ $r0:tt, $( $ri:tt ),* $(,)? ] => {
        {
            let row0 = $r0;
            let mut m = BasicMatrix::with_cols(row0.len() as u16);
            m.push_row(row0.iter().cloned());
            $({
                let rowi = $ri;
                m.push_row(rowi.iter().cloned());
            })*
            m
        }
    };
}

#[inline(always)]
fn empty_row_bits(cols: u16) -> u16 {
    std::u16::MAX.overflowing_shl(cols.into()).0
}

#[inline(always)]
fn full_row_bits(_cols: u16) -> u16 {
    std::u16::MAX
}

impl BasicMatrix {
    /// Constructs an empty matrix with the given number of columns.
    pub fn with_cols(cols: u16) -> Self {
        BasicMatrix { cols, data: vec![] }
    }

    /// Returns the number of columns.
    #[inline(always)]
    pub fn cols(&self) -> u16 {
        self.cols
    }

    /// Returns the number of (occupied) rows.
    #[inline(always)]
    pub fn rows(&self) -> u16 {
        self.data.len() as u16
    }

    /// Ensure that row `i` is present by appending empty rows to the top of the matrix.
    fn ensure_row(&mut self, i: u16) {
        let min_len = (i as usize) + 1;
        let len = std::cmp::max(self.data.len(), min_len);
        self.data.resize(len, empty_row_bits(self.cols));
    }

    /// Returns true if the coordinate `coord` is occupied. Out of bounds coordinates are
    /// considered occupied, unless the coordinate is above the highest row, in which case
    /// it is considered empty. This is to emulate an infinitely tall matrix.
    // TODO: infinite height configurable?
    pub fn get(&self, (i, j): (u16, u16)) -> bool {
        self.data
            .get(i as usize)
            .map(|row_bits| row_bits & (1 << j) != 0)
            .unwrap_or_else(|| j >= self.cols)
    }

    /// Set the given coordinate to become occupied. If the coordinate is out of bounds,
    /// does nothing.
    pub fn set(&mut self, (i, j): (u16, u16)) {
        if j < self.cols {
            self.ensure_row(i);
            self.data[i as usize] |= 1 << j;
        }
    }

    /// Blit matrix `mat` onto this matrix with origin `(i0, j0)`, setting all corresponing
    /// occupied cells.
    pub fn blit(&mut self, mat: &BasicMatrix, (i0, j0): (u16, u16)) {
        if mat.rows() == 0 {
            return;
        }
        let mask = (1 << mat.cols()) - 1;
        self.ensure_row(i0 + mat.rows() - 1);
        for i in 0..mat.rows() {
            self.data[(i0 + i) as usize] |= (mat.data[i as usize] & mask) << j0;
        }
    }

    /// Returns true if `mat` overlaps with this matrix, when offset by `(i0, j0)`.
    pub fn overlaps(&self, mat: &BasicMatrix, (i0, j0): (u16, u16)) -> bool {
        let mask = (1 << mat.cols()) - 1;
        // i < mat.rows() && (i0 + i) < self.rows()
        // i < mat.rows() && i < (self.rows() - i0)
        // i < min(mat.rows(), self.rows() - i0)
        let n = std::cmp::min(mat.rows(), self.rows().saturating_sub(i0));
        (0..n).any(|i| {
            let here = self.data[(i0 + i) as usize];
            let there = (mat.data[i as usize] & mask) << j0;
            (here & there) != 0
        })
    }

    /// Removes all rows that are either entirely occupied or entirely empty. Returns
    /// `true` if the bottom row was removed.
    pub fn sift_rows(&mut self) -> bool {
        let mut bottom_removed = false;
        let mut dst_idx = 0;
        for src_idx in 0..self.data.len() {
            let row_bits = self.data[src_idx];
            if row_bits == full_row_bits(self.cols) || row_bits == empty_row_bits(self.cols) {
                if src_idx == 0 {
                    bottom_removed = true;
                }
            } else {
                self.data[dst_idx] = row_bits;
                dst_idx += 1;
            }
        }
        self.data.resize_with(dst_idx, || unreachable!());
        bottom_removed
    }

    pub fn remove_rows(&mut self, range: Range<u16>) {
        let Range { start, end } = range;
        std::mem::drop(self.data.drain(start as usize..end as usize));
    }

    /// Returns the height of column `j`, counting only occupied cells.
    pub fn col_height(&self, j: u16) -> u16 {
        (0..self.rows())
            .rev()
            .find(|&i| self.get((i, j)))
            .map(|i| i + 1)
            .unwrap_or(0)
    }

    /// Returns the extents of every gap in row `i`.
    pub fn gaps(&self, i: u16) -> impl Iterator<Item = Range<u16>> {
        let cols = self.cols();
        let row_bits = self.data[i as usize];
        // iterator *inclusive* so we hit the rightmost wall
        (0..=cols)
            .scan(None, move |gap, j| {
                if row_bits & (1 << j) != 0 {
                    Some(gap.take())
                } else {
                    gap.get_or_insert(j..j).end += 1;
                    Some(None)
                }
            })
            .flatten()
    }

    /// Appends a row to the top of the matrix,
    // NOTE: currently, only used by `basic_matrix!` macro
    // TODO: can we remove this entirely?
    #[cfg(test)]
    pub fn push_row(&mut self, cells: impl IntoIterator<Item = bool>) {
        let i = self.rows();
        self.ensure_row(i);
        for (j, cell) in cells.into_iter().enumerate().take(self.cols as usize) {
            if cell {
                self.set((i, j as u16));
            }
        }
    }

    /// Inserts an empty row to the bottom of the matrix.
    pub fn insert_empty_bottom_row(&mut self) {
        self.data.insert(0, empty_row_bits(self.cols));
    }
}

impl std::fmt::Debug for BasicMatrix {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("[|")?;
        let mut space = "";
        for i in 0..self.rows() {
            f.write_str(space)?;
            space = " ";
            for j in 0..self.cols() {
                f.write_str(match self.get((i, j)) {
                    true => "x",
                    false => ".",
                })?;
            }
        }
        f.write_str("|]")
    }
}

#[derive(Serialize, Deserialize)]
struct MatrixWireFormat {
    cols: u16,
    data: String,
}

#[derive(Debug, Error)]
#[error("encountered invald byte in matrix specification")]
struct MatrixFormatError;

impl<'a> From<&'a BasicMatrix> for MatrixWireFormat {
    fn from(bm: &'a BasicMatrix) -> Self {
        Self {
            cols: bm.cols(),
            data: (0..bm.rows())
                .flat_map(move |i| {
                    let sep = if i == 0 { None } else { Some(' ') };
                    let row = (0..bm.cols()).map(move |j| if bm.get((i, j)) { 'x' } else { '.' });
                    sep.into_iter().chain(row)
                })
                .collect(),
        }
    }
}

impl TryFrom<MatrixWireFormat> for BasicMatrix {
    type Error = MatrixFormatError;
    fn try_from(m: MatrixWireFormat) -> Result<Self, MatrixFormatError> {
        let mut bm = Self::with_cols(m.cols);
        let (mut i, mut j) = (0u16, 0u16);
        for ch in m.data.chars() {
            match ch {
                'x' => {
                    bm.set((i, j));
                    j += 1;
                }
                '.' => {
                    j += 1;
                }
                ' ' => {
                    j = 0;
                    i += 1;
                }
                _ => return Err(MatrixFormatError),
            }
        }
        Ok(bm)
    }
}

impl Serialize for BasicMatrix {
    fn serialize<S>(&self, ser: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        MatrixWireFormat::from(self).serialize(ser)
    }
}

impl<'de> Deserialize<'de> for BasicMatrix {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        MatrixWireFormat::deserialize(de)?
            .try_into()
            .map_err(D::Error::custom)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bm_get() {
        let m = basic_matrix![[true, false, false], [false, true, true],];
        assert_eq!(m.get((0, 0)), true);
        assert_eq!(m.get((0, 1)), false);
        assert_eq!(m.get((0, 2)), false);
        assert_eq!(m.get((1, 0)), false);
        assert_eq!(m.get((1, 1)), true);
        assert_eq!(m.get((1, 2)), true);
        assert_eq!(m.get((1, 3)), true);
        assert_eq!(m.get((3, 0)), false);
        assert_eq!(m.get((3, 4)), true);
    }

    #[test]
    fn test_bm_set() {
        let mut m = BasicMatrix::with_cols(5);
        let c03 = (0, 3);
        assert_eq!(m.get(c03), false);
        m.set(c03);
        assert_eq!(m.get(c03), true);
        assert_eq!(m.rows(), 1);
        let c31 = (3, 1);
        assert_eq!(m.get(c31), false);
        m.set(c31);
        assert_eq!(m.get(c31), true);
        assert_eq!(m.rows(), 4);
        let c77 = (7, 7); // out of bounds
        assert_eq!(m.get(c77), true);
        m.set(c77);
        assert_eq!(m.rows(), 4);
    }

    #[test]
    fn test_bm_macro() {
        let mut m1 = BasicMatrix::with_cols(4);
        m1.set((0, 0));
        m1.set((0, 3));
        m1.set((2, 2));
        m1.set((2, 3));
        let m2 = basic_matrix![
            [true, false, false, true],
            [false, false, false, false],
            [false, false, true, true],
        ];
        assert_eq!(m1, m2);
    }

    fn heights(m: &BasicMatrix) -> Vec<u16> {
        (0..m.cols()).map(|j| m.col_height(j)).collect()
    }

    #[test]
    fn test_bm_sift() {
        let xx = true;
        let __ = false;

        let mut m = basic_matrix![
            [xx, xx, xx, xx, __],
            [xx, xx, __, __, xx],
            [__, __, __, __, __],
        ];
        assert_eq!(heights(&m), vec![2, 2, 1, 1, 2]);

        assert_eq!(m.sift_rows(), false);
        assert_eq!(
            m,
            basic_matrix![[xx, xx, xx, xx, __], [xx, xx, __, __, xx],]
        );
        assert_eq!(heights(&m), vec![2, 2, 1, 1, 2]);

        m.set((2, 3));
        m.set((1, 2));
        m.set((1, 3));
        assert_eq!(m.sift_rows(), false);
        assert_eq!(
            m,
            basic_matrix![[xx, xx, xx, xx, __], [__, __, __, xx, __],]
        );
        assert_eq!(heights(&m), vec![1, 1, 1, 2, 0]);

        m.set((0, 4));
        assert_eq!(m.sift_rows(), true);

        m.set((0, 2));
        m.set((0, 0));
        m.set((0, 1));
        m.set((0, 4));
        assert_eq!(m.sift_rows(), true);
        assert_eq!(m.rows(), 0);
        assert_eq!(heights(&m), vec![0, 0, 0, 0, 0]);
    }

    #[test]
    fn test_bm_blit() {
        let xx = true;
        let __ = false;
        let mut m = BasicMatrix::with_cols(6);

        m.blit(&basic_matrix![[xx, __, xx], [__, xx, __],], (1, 2));
        assert_eq!(
            m,
            basic_matrix![
                [__, __, __, __, __, __],
                [__, __, xx, __, xx, __],
                [__, __, __, xx, __, __],
            ]
        );

        m.blit(&basic_matrix![[xx, xx, xx, xx], [__, __, xx, __],], (0, 3));
        assert_eq!(
            m,
            basic_matrix![
                [__, __, __, xx, xx, xx],
                [__, __, xx, __, xx, xx],
                [__, __, __, xx, __, __],
            ]
        );

        m.set((0, 0));
        m.set((0, 1));
        m.set((0, 2));
        m.sift_rows();
    }

    #[test]
    fn test_bm_gaps() {
        let (xx, __) = (true, false);
        let mat = basic_matrix![
            [__, __, __, __, __, __, __, __],
            [xx, xx, xx, xx, xx, xx, xx, xx],
            [xx, xx, __, __, xx, xx, xx, xx],
            [xx, xx, __, __, xx, xx, __, xx],
            [__, __, __, __, xx, xx, __, __],
        ];
        let gaps = |row| mat.gaps(row).collect::<Vec<_>>();
        assert_eq!(gaps(0), [0..8]);
        assert_eq!(gaps(1), []);
        assert_eq!(gaps(2), [2..4]);
        assert_eq!(gaps(3), [2..4, 6..7]);
        assert_eq!(gaps(4), [0..4, 6..8]);
    }

    #[test]
    fn test_bm_overlaps() {
        let (xx, __) = (true, false);
        let mat = basic_matrix![
            [__, __, __, __, __, __, __, __],
            [xx, xx, xx, xx, xx, xx, xx, xx],
            [xx, xx, __, __, xx, xx, xx, xx],
            [xx, xx, __, __, xx, xx, __, xx],
            [__, __, __, __, __, xx, __, __],
        ];
        let mat2 = basic_matrix![[xx, xx, __, __], [__, __, __, __], [__, __, xx, __]];
        assert!(!mat.overlaps(&mat2, (0, 0)));
        assert!(!mat.overlaps(&mat2, (0, 1)));
        assert!(mat.overlaps(&mat2, (0, 2)));
        assert!(mat.overlaps(&mat2, (1, 0)));
        assert!(mat.overlaps(&mat2, (2, 0)));
        assert!(mat.overlaps(&mat2, (2, 1)));
        assert!(!mat.overlaps(&mat2, (2, 2)));
        assert!(mat.overlaps(&mat2, (2, 3)));
        assert!(mat.overlaps(&mat2, (3, 3)));
        assert!(!mat.overlaps(&mat2, (4, 3)));
        assert!(!mat.overlaps(&mat2, (5, 3)));
    }

    #[test]
    fn test_bm_overlaps_oob() {
        let (xx, __) = (true, false);
        let mat = basic_matrix![
            [__, __, __, __, __, __, __, __],
            [xx, xx, xx, xx, xx, xx, xx, xx],
            [xx, xx, __, __, xx, xx, xx, xx],
            [xx, xx, __, __, xx, xx, __, xx],
            [__, __, __, __, __, xx, __, __],
        ];
        let mat2 = basic_matrix![
            [xx, xx, __, __],
            [__, __, __, __],
            [__, __, __, __],
            [xx, __, __, __]
        ];
        assert!(!mat.overlaps(&mat2, (0, 6)));
        assert!(mat.overlaps(&mat2, (0, 7)));
        assert!(mat.overlaps(&mat2, (0, 8)));
    }

    #[test]
    fn test_remove_rows() {
        let (xx, __) = (true, false);
        let mut mat = basic_matrix![
            [xx, xx, xx, __],
            [xx, __, __, __],
            [xx, xx, __, __],
            [__, xx, xx, __],
            [__, xx, xx, xx],
        ];
        mat.remove_rows(1..3);
        assert_eq!(
            mat,
            basic_matrix![[xx, xx, xx, __], [__, xx, xx, __], [__, xx, xx, xx]]
        );
    }

    #[test]
    fn test_insert_empty_bottom_row() {
        let (xx, __) = (true, false);
        let mut mat = basic_matrix![[xx, xx, xx, __], [xx, __, __, __], [xx, xx, __, __],];
        mat.insert_empty_bottom_row();
        assert_eq!(
            mat,
            basic_matrix![
                [__, __, __, __],
                [xx, xx, xx, __],
                [xx, __, __, __],
                [xx, xx, __, __],
            ]
        );
    }
}
