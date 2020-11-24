#[derive(Clone, Eq, PartialEq, Hash)]
pub struct BasicMatrix {
    // Number of columns.
    cols: u16,
    // Row data.
    data: Vec<u16>,
}

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

impl BasicMatrix {
    /// Constructs an empty matrix with the given number of columns.
    pub fn with_cols(cols: u16) -> Self {
        BasicMatrix { cols, data: vec![] }
    }

    /// Returns the number of columns.
    pub fn cols(&self) -> u16 {
        self.cols
    }

    /// Returns the number of (occupied) rows.
    pub fn rows(&self) -> u16 {
        self.data.len() as u16
    }

    /// Returns true if the coordinate `coord` is occupied. Out of bounds coordinates are
    /// considered occupied, unless the coordinate is above the highest row, in which case
    /// it is considered empty. This is to emulate an infinitely tall matrix.
    // TODO: infinite height configurable?
    #[inline(always)]
    pub fn get(&self, coord: impl Coord) -> bool {
        if let Some((i, j)) = coord.check_coord(self.cols) {
            self.get_(i, j)
        } else {
            true
        }
    }

    fn get_(&self, i: u16, j: u16) -> bool {
        self.data
            .get(i as usize)
            .map(|row_bits| row_bits & (1 << j) != 0)
            .unwrap_or(false)
    }

    /// Set the given coordinate to become occupied. If the coordinate is out of bounds,
    /// does nothing.
    #[inline(always)]
    pub fn set(&mut self, coord: impl Coord) {
        if let Some((i, j)) = coord.check_coord(self.cols) {
            self.set_(i, j);
        }
    }

    fn ensure_row(&mut self, i: u16) {
        let min_len = (i as usize) + 1;
        let len = std::cmp::max(self.data.len(), min_len);
        self.data.resize(len, 0u16);
    }

    fn set_(&mut self, i: u16, j: u16) {
        self.ensure_row(i);
        self.data[i as usize] |= 1 << j;
    }

    /// Blit matrix `src` onto this matrix with origin `dst`, setting all corresponing
    /// occupied cells.
    #[inline(always)]
    pub fn blit(&mut self, src: &BasicMatrix, dst: impl Coord) {
        if let Some((i, j)) = dst.check_coord(self.cols) {
            self.blit_(src, i, j);
        }
    }

    pub fn blit_(&mut self, mat: &BasicMatrix, i0: u16, j0: u16) {
        if mat.rows() == 0 {
            return;
        }
        self.ensure_row(i0 + mat.rows() - 1);
        let row_bits_mask = (1 << self.cols()) - 1;
        for i in 0..mat.rows() {
            let src = mat.data[i as usize];
            let dst = &mut self.data[(i0 + i) as usize];
            *dst |= src << j0;
            *dst &= row_bits_mask;
        }
    }

    /// Appends a new row to the top of the matrix.
    ///
    /// # Arguments
    ///
    /// * `cells` - list of cell data for the new row. `true` indicates a cell is present.
    pub fn push_row(&mut self, cells: impl IntoIterator<Item = bool>) {
        let mut row_bits = 0u16;
        for (j, cell) in cells.into_iter().enumerate().take(self.cols as usize) {
            if cell {
                row_bits |= 1 << j;
            }
        }
        self.data.push(row_bits);
    }

    /// Removes all rows that are either entirely occupied or entirely empty. Returns the
    /// number of rows that were removed this way because they were full.
    pub fn sift_rows(&mut self) -> usize {
        let empty_row_bits = 0u16;
        let full_row_bits = (1 << self.cols()) - 1;

        let mut dst_idx = 0;
        let mut full_cnt = 0;
        for src_idx in 0..self.data.len() {
            let row_bits = self.data[src_idx];
            if row_bits == full_row_bits {
                full_cnt += 1;
            } else if row_bits != empty_row_bits {
                self.data[dst_idx] = row_bits;
                dst_idx += 1;
            }
        }

        self.data.resize(dst_idx, 0);
        full_cnt
    }

    /// Returns the height of column `j`, counting only occupied cells.
    pub fn col_height(&self, j: u16) -> u16 {
        (0..self.rows())
            .rev()
            .find(|&i| self.get((i, j)))
            .map(|i| i + 1)
            .unwrap_or(0)
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

pub trait Coord {
    /// Check if this coordinate is bounded by the given number of columns. If so,
    /// returns `Some((i, j))` such that this coordinate represents the `i`th row and
    /// `j`th column.
    fn check_coord(&self, cols: u16) -> Option<(u16, u16)>;
}

impl Coord for (u16, u16) {
    fn check_coord(&self, cols: u16) -> Option<(u16, u16)> {
        if self.1 < cols {
            Some(*self)
        } else {
            None
        }
    }
}

impl Coord for (i16, i16) {
    fn check_coord(&self, cols: u16) -> Option<(u16, u16)> {
        if self.0 < 0 || self.1 < 0 {
            None
        } else if (self.1 as u16) < cols {
            Some((self.0 as u16, self.1 as u16))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bm_get() {
        let mut m = BasicMatrix::with_cols(3);
        m.push_row(vec![true, false, false]);
        m.push_row(vec![false, true, true]);
        assert_eq!(m.get((0u16, 0u16)), true);
        assert_eq!(m.get((0u16, 1u16)), false);
        assert_eq!(m.get((0u16, 2u16)), false);
        assert_eq!(m.get((1u16, 0u16)), false);
        assert_eq!(m.get((1u16, 1u16)), true);
        assert_eq!(m.get((1u16, 2u16)), true);
        assert_eq!(m.get((-1i16, 0i16)), true);
        assert_eq!(m.get((0i16, -1i16)), true);
        assert_eq!(m.get((0i16, 3i16)), true);
        assert_eq!(m.get((1i16, -1i16)), true);
        assert_eq!(m.get((1u16, 3u16)), true);
        assert_eq!(m.get((3i16, 0i16)), false);
        assert_eq!(m.get((3u16, 0u16)), false);
        assert_eq!(m.get((3i16, -1i16)), true);
    }

    #[test]
    fn test_bm_set() {
        let mut m = BasicMatrix::with_cols(5);
        let c03 = (0u16, 3u16);
        assert_eq!(m.get(c03), false);
        m.set(c03);
        assert_eq!(m.get(c03), true);
        assert_eq!(m.rows(), 1);
        let c31 = (3u16, 1u16);
        assert_eq!(m.get(c31), false);
        m.set(c31);
        assert_eq!(m.get(c31), true);
        assert_eq!(m.rows(), 4);
        let c77 = (7u16, 7u16); // out of bounds
        assert_eq!(m.get(c77), true);
        m.set(c77);
        assert_eq!(m.rows(), 4);
    }

    #[test]
    fn test_bm_macro() {
        let mut m1 = BasicMatrix::with_cols(4);
        m1.push_row(vec![true, false, false, true]);
        m1.push_row(vec![false, false, true, true]);
        let m2 = basic_matrix![[true, false, false, true], [false, false, true, true],];
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

        assert_eq!(m.sift_rows(), 0);
        assert_eq!(
            m,
            basic_matrix![[xx, xx, xx, xx, __], [xx, xx, __, __, xx],]
        );
        assert_eq!(heights(&m), vec![2, 2, 1, 1, 2]);

        m.set((2u16, 3u16));
        m.set((1u16, 2u16));
        m.set((1u16, 3u16));
        assert_eq!(m.sift_rows(), 1);
        assert_eq!(
            m,
            basic_matrix![[xx, xx, xx, xx, __], [__, __, __, xx, __],]
        );
        assert_eq!(heights(&m), vec![1, 1, 1, 2, 0]);

        m.set((0u16, 4u16));
        m.set((1u16, 2u16));
        m.set((1u16, 0u16));
        m.set((1u16, 1u16));
        m.set((1u16, 4u16));
        assert_eq!(m.sift_rows(), 2);
        assert_eq!(m.rows(), 0);
        assert_eq!(heights(&m), vec![0, 0, 0, 0, 0]);
    }

    #[test]
    fn test_bm_blit() {
        let xx = true;
        let __ = false;
        let mut m = BasicMatrix::with_cols(6);

        m.blit(&basic_matrix![[xx, __, xx], [__, xx, __],], (1u16, 2u16));
        assert_eq!(
            m,
            basic_matrix![
                [__, __, __, __, __, __],
                [__, __, xx, __, xx, __],
                [__, __, __, xx, __, __],
            ]
        );

        m.blit(
            &basic_matrix![[xx, xx, xx, xx], [__, __, xx, __],],
            (0u16, 3u16),
        );
        assert_eq!(
            m,
            basic_matrix![
                [__, __, __, xx, xx, xx],
                [__, __, xx, __, xx, xx],
                [__, __, __, xx, __, __],
            ]
        );

        m.set((0u16, 0u16));
        m.set((0u16, 1u16));
        m.set((0u16, 2u16));
        assert_eq!(m.sift_rows(), 1);
    }
}
