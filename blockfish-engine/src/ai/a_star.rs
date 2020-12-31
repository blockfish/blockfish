use super::{node::Node, place::Placements, score::ScoreParams};
use crate::shape::ShapeTable;

use std::collections::BinaryHeap;

pub(super) struct AStar<'s> {
    nodes: BinaryHeap<AStarNode>,
    placements: Placements<'s>,
    scoring: &'s ScoreParams,
}

impl<'s> AStar<'s> {
    pub fn new(
        shape_table: &'s ShapeTable,
        scoring: &'s ScoreParams,
        capacity: usize,
        initial_node: Node,
    ) -> Self {
        let mut nodes = BinaryHeap::with_capacity(capacity);
        nodes.push(AStarNode(initial_node));
        let placements = Placements::new(shape_table);
        Self {
            nodes,
            placements,
            scoring,
        }
    }

    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }
}

impl<'s> Iterator for AStar<'s> {
    type Item = Node;
    fn next(&mut self) -> Option<Node> {
        let node = self.nodes.pop()?.0;
        if !node.state().is_goal() {
            self.placements.set_state(node.state());
            for (idx, place) in (&mut self.placements).enumerate() {
                let succ = node.successor(self.scoring, idx, &place);
                self.nodes.push(AStarNode(succ));
            }
        }
        Some(node)
    }
}

struct AStarNode(Node);

impl AStarNode {
    fn f_val(&self) -> i64 {
        self.0.score() + self.0.penalty()
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
