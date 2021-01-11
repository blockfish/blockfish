use super::state::State;
use crate::{
    config::Parameters,
    place::{Place, PlaceFinder},
    shape::ShapeTable,
};

use std::collections::BinaryHeap;

/// An instance of the A* search algorithm.
pub struct Search<'s> {
    fringe: BinaryHeap<Node>,
    pfind: PlaceFinder<'s>,
    parameters: Parameters,
}

/// A node in the A* algorithm. Contains the computed score as well as traceback
/// information.
pub struct Node {
    // node's state
    state: State,
    // stored as u8's because placement indices are small
    trace: Vec<u8>,
    // cost to move to this node
    g: i64,
    // heuristic estimate cost to the goal
    h: i64,
}

impl<'s> Search<'s> {
    /// Start a new instance of A* with initial state `root`, using ruleset in
    /// `shape_table` to find placements and `parameters` to evaluate nodes. Space for
    /// `capacity` nodes is reserved in the fringe set.
    ///
    /// TODO: generate *exactly* `capacity` nodes then stop instead of putting `analysis`
    /// in charge of checking that
    pub fn new(
        shape_table: &'s ShapeTable,
        parameters: Parameters,
        capacity: usize,
        root: State,
    ) -> Self {
        let mut fringe = BinaryHeap::with_capacity(capacity * 3 / 2);
        fringe.push(Node::new(root));
        Self {
            fringe,
            parameters,
            pfind: PlaceFinder::new(shape_table),
        }
    }

    /// Current number of nodes in the fringe.
    pub fn node_count(&self) -> usize {
        self.fringe.len()
    }

    /// Expands the given node, adding its successors to the fringe set.
    fn expand(&mut self, node: &Node) {
        if node.state.is_terminal() {
            return;
        }
        for (idx, place) in node.state.placements(&mut self.pfind).enumerate() {
            let succ = node.successor(&self.parameters, idx, &place);
            self.fringe.push(succ);
        }
    }
}

impl<'s> Iterator for Search<'s> {
    type Item = Node;
    fn next(&mut self) -> Option<Node> {
        let node = self.fringe.pop()?;
        self.expand(&node);
        Some(node)
    }
}

impl Node {
    /// Constructs a new root node with the given state.
    fn new(state: State) -> Self {
        Self {
            state,
            trace: Vec::with_capacity(8),
            g: 0,
            h: std::i64::MAX,
        }
    }

    /// Returns the evaluation score for this node.
    pub fn score(&self) -> i64 {
        self.h
    }

    /// Returns the `f(n)` value used to pick nodes in A*.
    pub fn f_val(&self) -> i64 {
        self.g + self.h
    }

    /// Returns the placement trace leading to this node. TODO: rename to 'path'?
    pub fn trace<'a>(&'a self) -> impl Iterator<Item = usize> + 'a {
        self.trace.iter().map(|&idx| idx as usize)
    }

    /// Builds and returns a successor node derived from this node and the placement
    /// `pl`, using `scoring` to score the returned node. `idx` is used to update the
    /// traceback.
    fn successor(&self, params: &Parameters, idx: usize, pl: &Place) -> Self {
        use super::eval::{eval, penalty};

        let mut state = self.state.clone();
        state.place(pl);
        let mut trace = self.trace.clone();
        trace.push(idx as u8);

        let g = penalty(params, trace.len());
        let h = if state.reached_goal() {
            // TODO: this is a bad way to do this, but currently necessary in order to
            // make `analysis.rs` pick the shortest path to finishing the cheese race.
            //
            // the proper way to do this would be to use g(n) values as ratings but that
            // currently falls apart really hard because analysis doesn't take node depth
            // into account. this whole part is going to be scrapped when the search algo
            // is replaced.
            (trace.len() as i64) - 1000
        } else {
            eval(state.matrix()).score(params)
        };

        Self { state, trace, g, h }
    }
}

impl PartialEq<Node> for Node {
    fn eq(&self, rhs: &Self) -> bool {
        self.f_val() == rhs.f_val()
    }
}

impl Eq for Node {}

impl PartialOrd<Node> for Node {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}

impl Ord for Node {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        self.f_val().cmp(&rhs.f_val()).reverse()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{ai::Snapshot, basic_matrix, shape::srs, BasicMatrix, Color, Orientation::*};

    #[test]
    fn test_node_successor() {
        let srs = srs();
        let par = Parameters::default();
        let (xx, __) = (true, false);

        // x . . . .
        // x x . . .
        let matrix = basic_matrix![[xx, xx, __, __, __], [xx, __, __, __, __]];
        let mut node = Node::new(
            Snapshot {
                hold: None,
                queue: vec![Color::n('L'), Color::n('O')],
                matrix,
            }
            .into(),
        );
        assert_eq!(node.trace().count(), 0);
        assert_eq!(node.state.is_terminal(), false);

        // x . . . L
        // x x L L L  ==>  x . . . L
        let l = srs.shape(Color::n('L')).unwrap();
        let tf = (-1, 2, R0);
        node = node.successor(&par, 3, &Place::new(l, tf, false));
        assert_eq!(node.state.next().0, Some(Color::n('O')));
        assert_eq!(node.state.next().1, None);
        assert_eq!(node.state.matrix(), &basic_matrix![[xx, __, __, __, xx]]);
        assert_eq!(node.trace().collect::<Vec<_>>(), [3]);
        assert_eq!(node.state.is_terminal(), true);

        // O O . . .
        // O O . . .
        // x . . . L
        let o = srs.shape(Color::n('O')).unwrap();
        let tf = (0, -1, R0);
        node = node.successor(&par, 4, &Place::new(o, tf, false));
        assert_eq!(node.trace().collect::<Vec<_>>(), [3, 4]);
        assert_eq!(
            node.state.matrix(),
            &basic_matrix![
                [xx, __, __, __, xx],
                [xx, xx, __, __, __],
                [xx, xx, __, __, __],
            ]
        );
        assert!(node.state.is_terminal(), true);
    }
}
