use std::collections::{BinaryHeap, HashMap, HashSet};
use thiserror::Error;

use crate::{
    common::{Input, Orientation},
    matrix::BasicMatrix,
    shape::{NormalizedShapeTransform, ShapeRef, Transform},
};

#[derive(Debug, Error)]
#[error("no valid input sequences found")]
pub struct FinesseNotFoundError;

/// Data structure for computing (minimum-)inputs to accomplish a placement. This type has
/// a mutable interface so that the internal data structures may be reused for performing
/// the algorithm multiple times.
///
/// Current implementation uses Djikstra's algorithm to ensure a shortest path is found.
/// It's perhaps not as efficient as A* but it does the job simply.
pub struct FinesseFinder {
    frontier: BinaryHeap<FinesseNode>,
    explored: HashSet<Transform>,
    back_links: HashMap<Transform, (Transform, Input)>,
}

impl FinesseFinder {
    /// Constructs a new `FinesseFinder`.
    pub fn new() -> Self {
        Self {
            frontier: BinaryHeap::new(),
            explored: HashSet::new(),
            back_links: HashMap::new(),
        }
    }

    /// Runs the finesse finder algorithm to find a path for `shape` to get from its spawn
    /// location to `target`.
    pub fn find(
        &mut self,
        matrix: &BasicMatrix,
        shape: ShapeRef,
        target: NormalizedShapeTransform,
    ) -> Result<Vec<Input>, FinesseNotFoundError> {
        self.explored.clear();
        self.back_links.clear();
        self.frontier.clear();
        self.frontier.push(FinesseNode::new((
            matrix.rows() as i16,
            shape.spawn_col(),
            Orientation::R0,
        )));
        loop {
            if let Some(tf) = self.step(matrix, shape, target)? {
                return Ok(self.reconstruct_inputs(tf));
            }
        }
    }

    /// Run one step of Djikstra's. Returns `Ok(Some(tf))` if the algorithm successfully
    /// found a path to `target` via final transform `tf` (this can be used as the
    /// argument to `reconstruct_inputs()`). Returns `Ok(None)` if the algorithm is still
    /// searching.  Returns `Err(FinesseNotFoundError)` if the algorithm gave up.
    fn step(
        &mut self,
        matrix: &BasicMatrix,
        shape: ShapeRef,
        target: NormalizedShapeTransform,
    ) -> Result<Option<Transform>, FinesseNotFoundError> {
        // pop next node in priority queue with least KPP
        let node = self.frontier.pop().ok_or(FinesseNotFoundError)?;
        let tf0 = node.tf;
        self.explored.insert(tf0);

        // halt if the placement would hard-drop at the target location (normalized)
        let sd_tf = shape.sonic_drop(matrix, tf0);
        let norm = shape.normalize(sd_tf);
        if norm == target {
            return Ok(Some(tf0));
        }

        // try moving in every direction (including sonic-drop)
        let neighbors = [Input::CW, Input::CCW, Input::Left, Input::Right, Input::SD]
            .iter()
            .filter_map(|&input| {
                match input {
                    Input::SD => Some(sd_tf),
                    _ => shape.try_input(matrix, tf0, input),
                }
                .map(|tf| (tf, input))
            });

        for (tf, input) in neighbors {
            // push if placement is newly discovered
            if self.explored.contains(&tf) {
                continue;
            }
            self.frontier.push(node.succ(tf, input));
            self.back_links.insert(tf, (tf0, input));
        }

        Ok(None)
    }

    /// Constructs the optimal input sequence arriving at `tf` by following the backwards
    /// links created during Djikstra's algorithm.
    fn reconstruct_inputs(&self, mut tf: Transform) -> Vec<Input> {
        let mut inputs = vec![];
        while let Some(&(prev_tf, input)) = self.back_links.get(&tf) {
            tf = prev_tf;
            inputs.push(input);
        }
        inputs.reverse();
        inputs
    }
}

/// Wrapper around Transforms for use in Djikstra's algorithm. `FinesseNode`s are
/// `Ord`ered by most KPP, so that the binary (max-)heap in `FinesseFinder` orders them by
/// least-KPP first.
#[derive(Debug)]
struct FinesseNode {
    tf: Transform,
    kpp: usize,
}

impl FinesseNode {
    fn new(tf: Transform) -> Self {
        Self { tf, kpp: 0 }
    }

    fn succ(&self, new_tf: Transform, _: Input) -> Self {
        Self {
            tf: new_tf,
            kpp: self.kpp + 1,
        }
    }
}

impl Eq for FinesseNode {}

impl PartialEq for FinesseNode {
    fn eq(&self, rhs: &Self) -> bool {
        rhs.kpp == self.kpp
    }
}

impl PartialOrd for FinesseNode {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}

impl Ord for FinesseNode {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        rhs.kpp.cmp(&self.kpp)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        common::{Color, Input::*, Orientation::*},
        shape::srs,
    };

    #[test]
    fn test_ord_kpp() {
        let tf = (0, 0, R0);
        let n0 = FinesseNode::new(tf);
        let n1 = n0.succ(tf, CW); // kpp = 1
        let n2 = n0.succ(tf, Left).succ(tf, Right); // kpp = 2
        let n2_alt = n0.succ(tf, Left).succ(tf, CW); // kpp = 2
        assert!(n1 > n2); // order reversed to create a min-heap in FinesseFinder
        assert!(n0 > n1);
        assert!(n0 == n0);
        assert!(n2 == n2_alt);
        assert!(n2 != n0);
    }

    fn all_finesse(color: char, i: i16, r: Orientation) -> Vec<Vec<Input>> {
        let srs = srs();
        let color = Color::n(color);
        let matrix = BasicMatrix::with_cols(10);
        let shape = srs.shape(color).unwrap();
        let mut ffind = FinesseFinder::new();
        shape
            .valid_cols(r, matrix.cols())
            .map(|j| {
                let tgt = shape.normalize((i, j, r));
                ffind.find(&matrix, shape, tgt).map_err(|e| (j, e))
            })
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
    }

    #[test]
    fn test_o_finesse() {
        assert_eq!(
            all_finesse('O', -1, R0),
            vec![
                // 0 1 2 3 4 5 6 7 8 9
                // o o . . . . . . . .
                // o o . . . . . . . .
                vec![Left, Left, Left, Left],
                vec![Left, Left, Left],
                vec![Left, Left],
                vec![Left],
                // . . . . o o . . . .
                // . . . . o o . . . .
                vec![],
                vec![Right],
                vec![Right, Right],
                vec![Right, Right, Right],
                vec![Right, Right, Right, Right],
                // . . . . . . . . o o
                // . . . . . . . . o o
            ]
        );
    }

    #[test]
    fn test_s_finesse() {
        // CW happens in the middle for unknown reason. however, biasing rotations first
        // without fucking over the algorithm is going to be difficult. so this test is
        // going to stay for the time being, since it does put the piece in the right
        // location anyways.
        assert_eq!(
            all_finesse('S', 0, R1),
            vec![
                // 0 1 2 3 4 5 6 7 8 9
                // s . . . . . . . . .
                // s s . . . . . . . .
                // . s . . . . . . . .
                vec![CCW, Left, Left, Left],
                vec![CCW, Left, Left],
                vec![CCW, Left],
                // . . . s . . . . . .
                // . . . s s . . . . .
                // . . . . s . . . . .
                vec![CCW],
                // . . . . s . . . . .
                // . . . . s s . . . .
                // . . . . . s . . . .
                vec![CW],
                vec![Right, CW],
                vec![Right, Right, CW],
                vec![Right, Right, CW, Right],
                vec![Right, Right, CW, Right, Right],
                // . . . . . . . . s .
                // . . . . . . . . s s
                // . . . . . . . . . s
            ]
        );
        assert_eq!(all_finesse('S', -1, R0), all_finesse('S', 0, R2));
        assert_eq!(all_finesse('S', 0, R1), all_finesse('S', 0, R3));
    }
}
