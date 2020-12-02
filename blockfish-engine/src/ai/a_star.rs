use super::{place::PlacementSearch, Node, ScoreParams};
use std::collections::BinaryHeap;

pub(super) struct AStar<'s> {
    nodes: BinaryHeap<AStarNode>,
    place_search: PlacementSearch<'s>,
    scoring: &'s ScoreParams,
}

pub(super) fn a_star<'s>(
    place_search: PlacementSearch<'s>,
    scoring: &'s ScoreParams,
    initial_nodes: Vec<Node>,
) -> AStar<'s> {
    let mut nodes = BinaryHeap::with_capacity(1000);
    nodes.extend(initial_nodes.into_iter().map(AStarNode));
    AStar {
        nodes,
        place_search,
        scoring,
    }
}

impl<'s> AStar<'s> {
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }
}

impl<'s> Iterator for AStar<'s> {
    type Item = Node;
    fn next(&mut self) -> Option<Node> {
        let node = self.nodes.pop()?.0;
        self.place_search.compute(&node.state);
        let succ = node.successors(&self.place_search, self.scoring);
        self.nodes.extend(succ.map(AStarNode));
        Some(node)
    }
}

struct AStarNode(Node);

impl AStarNode {
    fn f_val(&self) -> i64 {
        let g = self.0.penalty;
        let h = self.0.score;
        g + h
    }
}

impl PartialEq<AStarNode> for AStarNode {
    fn eq(&self, rhs: &Self) -> bool {
        self.f_val() == rhs.f_val()
    }
}

impl Eq for AStarNode {}

impl PartialOrd<AStarNode> for AStarNode {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}

impl Ord for AStarNode {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        self.f_val().cmp(&rhs.f_val()).reverse()
    }
}
