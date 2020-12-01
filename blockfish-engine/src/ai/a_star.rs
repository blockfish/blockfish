use super::{Node, ScoreParams};
use crate::shape::ShapeTable;
use std::collections::BinaryHeap;

pub(super) struct AStar<'s> {
    nodes: BinaryHeap<AStarNode>,
    stbl: &'s ShapeTable,
    scoring: &'s ScoreParams,
}

pub(super) fn a_star<'s, I>(stbl: &'s ShapeTable, scoring: &'s ScoreParams, iter: I) -> AStar<'s>
where
    I: IntoIterator<Item = Node>,
{
    let mut nodes = BinaryHeap::with_capacity(1000);
    nodes.extend(iter.into_iter().map(AStarNode));
    AStar {
        nodes,
        stbl,
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
        let succ = node.successors(self.stbl, self.scoring);
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
