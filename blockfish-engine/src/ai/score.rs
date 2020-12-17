use crate::BasicMatrix;
use red_union_find::UF;
use std::ops::Range;

// Parameters

#[derive(Clone, Debug)]
pub struct ScoreParams {
    pub row_factor: i64,
    pub nspace_factor: i64,
    pub piece_penalty: i64,
}

impl Default for ScoreParams {
    fn default() -> Self {
        Self {
            row_factor: 50,
            nspace_factor: 10,
            piece_penalty: 30,
        }
    }
}

/// Computes the "score" for the given snapshot. Lower is better.
///
/// Note: used by A* to compute "h" value (remaining cost heuristic).
pub fn score(params: &ScoreParams, matrix: &BasicMatrix) -> i64 {
    let rs = (matrix.rows() as i64) * params.row_factor;
    let nss = negative_space_score(&matrix) * params.nspace_factor;
    rs + nss
}

/// Computes the "penalty" for placing the given number of pieces.
///
/// Note: used in A* to compute "g" value (path cost).
pub fn penalty(params: &ScoreParams, depth: usize) -> i64 {
    (depth as i64) * params.piece_penalty
}

fn negative_space_score(matrix: &BasicMatrix) -> i64 {
    negative_spaces(matrix, 0..matrix.rows())
        .map(|area| ceil_4(area) as i64)
        .sum()
}

/// Returns the area of each disjoint contiguous negative space in the given matrix.
fn negative_spaces<'a>(
    matrix: &'a BasicMatrix,
    row_range: Range<u16>,
) -> impl Iterator<Item = u16> + 'a {
    gaps_contiguous_areas(row_range.map(move |i| matrix.gaps(i)))
}

/// Returns the size of each contiguous area given by the overlapping, neighboring ranges
/// in `iter`.
///
/// # Example
///
/// If `iter` is `[[0..5, 7..10], [0..4], [2..3]]` returns `[10, 3]`. `10` is given by the
/// set of intervals `{0..5,0..4,2..3}` which all overlap; `3` is given by the last
/// interval `7..10`.
fn gaps_contiguous_areas<I>(iter: I) -> impl Iterator<Item = u16> + 'static
where
    I: IntoIterator,
    I::Item: IntoIterator<Item = Range<u16>>,
{
    let iter = iter.into_iter();
    let size_hint = iter.size_hint().1.unwrap_or(0);
    let mut gaps = Vec::with_capacity(size_hint * 2);
    let mut row_end_idxs = Vec::with_capacity(size_hint);
    for row_gaps in iter {
        gaps.extend(row_gaps);
        row_end_idxs.push(gaps.len());
    }

    let mut uf = UF::new_reflexive(gaps.len());
    let mut idx0 = std::usize::MAX;
    let mut idx1 = 0;
    for idx2 in row_end_idxs {
        if idx0 < std::usize::MAX {
            let row1 = &gaps[idx0..idx1];
            let row2 = &gaps[idx1..idx2];
            for (i0, i1) in intersecting_ranges(row1, row2) {
                uf.union(idx0 + i0, idx1 + i1);
            }
        }
        idx0 = idx1;
        idx1 = idx2;
    }

    let mut areas = vec![0; gaps.len()];
    for (i, gap) in gaps.into_iter().enumerate() {
        areas[uf.find(i)] += gap.end - gap.start;
    }

    areas.into_iter().filter(|&a| a > 0)
}

/// Rounds `x` to the nearest multiple of 4.
fn ceil_4(x: u16) -> u16 {
    if x & 3 == 0 {
        x
    } else {
        (x | 3) + 1
    }
}

/// Given `xs` and `ys` both ordered lists of non-overlapping ranges, returns every pair
/// of indices `(i, j)` such that `xs[i]` intersects with `ys[j]`.
fn intersecting_ranges<'a, T: Ord>(
    xs: &'a [Range<T>],
    ys: &'a [Range<T>],
) -> impl Iterator<Item = (usize, usize)> + 'a {
    let (mut i1, mut i2) = (0, 0);
    std::iter::from_fn(move || loop {
        let r1 = xs.get(i1)?;
        let r2 = ys.get(i2)?;
        if r2.start >= r1.end {
            i1 += 1;
        } else if r1.start >= r2.end {
            i2 += 1;
        } else if r2.end >= r1.end {
            i1 += 1;
            return Some((i1 - 1, i2));
        } else {
            i2 += 1;
            return Some((i1, i2 - 1));
        }
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::basic_matrix;

    #[test]
    fn test_ceil_4() {
        assert_eq!(
            (0..10).map(ceil_4).collect::<Vec<_>>(),
            [0, 4, 4, 4, 4, 8, 8, 8, 8, 12]
        );
    }

    #[test]
    fn test_intersecting_ranges() {
        let irs = |xs: &[Range<i32>], ys| intersecting_ranges(xs, ys).collect::<Vec<_>>();
        // 0 1 2 3 4 5 6 7 8 9 10 11 12 13
        // {-----}     {-----------}    {----
        //     {-----} {-}   {--------}
        //     {#}     {#}   {#####}
        let xs = [0..3, 6..11, 13..20];
        let ys = [2..5, 6..7, 9..12];
        assert_eq!(irs(&xs, &ys), [(0, 0), (1, 1), (1, 2)]);
        assert_eq!(irs(&ys, &xs), [(0, 0), (1, 1), (2, 1)]);
        assert_eq!(irs(&xs, &[]), []);
        assert_eq!(irs(&[], &xs), []);
        assert_eq!(irs(&xs, &[10..15]), [(1, 0), (2, 0)]);
        assert_eq!(irs(&xs, &[11..15]), [(2, 0)]);
    }

    fn neg_space(mat: BasicMatrix) -> Vec<u16> {
        let mut nss = negative_spaces(&mat, 0..mat.rows()).collect::<Vec<_>>();
        nss.sort();
        nss
    }

    #[test]
    fn test_negative_spaces_1() {
        let (xx, __) = (true, false);
        assert_eq!(
            neg_space(basic_matrix![
                [xx, xx, xx, xx, xx, __],
                [xx, __, xx, xx, xx, xx],
                [xx, xx, __, xx, xx, xx],
            ]),
            [1, 1, 1],
            "cheese"
        );
        assert_eq!(
            neg_space(basic_matrix![
                [xx, __, xx, xx, xx, xx],
                [xx, xx, __, __, xx, xx],
            ]),
            [1, 2],
            "2w gap"
        );
    }

    #[test]
    fn test_negative_spaces_2() {
        let (xx, __) = (true, false);
        assert_eq!(
            neg_space(basic_matrix![
                [xx, __, __, __, xx, xx],
                [xx, __, xx, xx, xx, xx],
                [xx, xx, xx, __, __, xx],
            ]),
            [2, 4],
            "overlap (depth=1)"
        );
        assert_eq!(
            neg_space(basic_matrix![
                [xx, xx, xx, __, __, xx],
                [xx, __, xx, xx, xx, xx],
                [xx, __, __, __, xx, xx],
            ]),
            [2, 4],
            "overlap (depth=1, inverted)"
        );
        assert_eq!(
            neg_space(basic_matrix![
                [xx, __, __, __, xx, xx],
                [xx, __, xx, __, __, xx],
                [__, xx, __, xx, xx, xx],
            ]),
            [1, 1, 6],
            "overlap (depth=2)"
        );
        assert_eq!(
            neg_space(basic_matrix![
                [__, xx, __, xx, xx, xx],
                [xx, __, xx, __, __, xx],
                [xx, __, __, __, xx, xx],
            ]),
            [1, 1, 6],
            "overlap (depth=2, inverted)"
        );
    }

    #[test]
    fn test_negative_spaces_3() {
        let (xx, __) = (true, false);
        assert_eq!(
            neg_space(basic_matrix![
                [xx, __, __, __, xx, __],
                [__, __, xx, __, xx, xx],
                [xx, __, xx, __, __, xx],
            ]),
            [1, 9],
            "overlap (depth=3)"
        );
        assert_eq!(
            neg_space(basic_matrix![
                [xx, __, xx, __, __, xx],
                [__, __, xx, __, xx, xx],
                [xx, __, __, __, xx, __],
            ]),
            [1, 9],
            "overlap (depth=3, ivnerted)"
        );
    }

    #[test]
    fn test_negative_spaces_all_clear() {
        assert_eq!(neg_space(BasicMatrix::with_cols(5)), [0u16; 0]);
    }
}
