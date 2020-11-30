use crate::{BasicMatrix, Snapshot};
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
            piece_penalty: 40,
        }
    }
}

/// Computes the "score" for the given snapshot. Lower is better.
///
/// Note: used by A* to compute "h" value (remaining cost heuristic).
pub fn score(params: &ScoreParams, snapshot: &Snapshot) -> i64 {
    let rs = (snapshot.matrix.rows() as i64) * params.row_factor;
    let nss = negative_space_score(&snapshot.matrix) * params.nspace_factor;
    rs + nss
}

/// Computes the "penalty" for placing the given number of pieces.
///
/// Note: used in A* to compute "g" value (path cost).
pub fn penalty(params: &ScoreParams, depth: usize) -> i64 {
    (depth as i64) * params.piece_penalty
}

fn negative_space_score(matrix: &BasicMatrix) -> i64 {
    negative_spaces(matrix)
        .map(|area| ceil_4(area) as i64)
        .sum()
}

/// Returns the area of each disjoint contiguous negative spaces in the given matrix.
fn negative_spaces(matrix: &BasicMatrix) -> impl Iterator<Item = u16> {
    let mut gaps = vec![];
    let mut row_idxs = vec![];
    for i in 0..matrix.rows() {
        let start = gaps.len();
        for gap in matrix.gaps(i) {
            gaps.push(gap);
        }
        let end = gaps.len();
        row_idxs.push(start..end);
    }

    let mut uf = UF::new_reflexive(gaps.len());
    for (idxs, prev_idxs) in row_idxs.iter().zip(row_idxs[1..].iter()) {
        for idx1 in idxs.clone() {
            for idx2 in prev_idxs.clone() {
                if range_intersects(&gaps[idx1], &gaps[idx2]) {
                    uf.union(idx1, idx2);
                }
            }
        }
    }

    let mut areas = vec![0; gaps.len()];
    for (idx, gap) in gaps.into_iter().enumerate() {
        areas[uf.find(idx)] += gap.end - gap.start;
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

/// Returns `true` if the ranges intersect.
fn range_intersects<T: Ord>(r1: &Range<T>, r2: &Range<T>) -> bool {
    (r1.start < r2.end) && (r2.start < r1.end)
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
    fn test_range_intersects() {
        assert!(range_intersects(&(0..5), &(3..7)));
        assert!(range_intersects(&(3..7), &(0..5)));
        assert!(!range_intersects(&(0..5), &(5..6)));
        assert!(!range_intersects(&(0..5), &(7..10)));
        assert!(!range_intersects(&(7..10), &(0..5)));
    }

    fn neg_space(mat: BasicMatrix) -> Vec<u16> {
        let mut nss = negative_spaces(&mat).collect::<Vec<_>>();
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
}
