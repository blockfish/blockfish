use super::{node::Node, place::Placements, score::ScoreParams};
use crate::shape::ShapeTable;

pub(super) struct DFS<'s> {
    nodes: Vec<Node>,
    placements: Placements<'s>,
    scoring: &'s ScoreParams,
    max_depth: usize,
}

impl<'s> DFS<'s> {
    pub fn new(
        shape_table: &'s ShapeTable,
        scoring: &'s ScoreParams,
        max_depth: usize,
        initial_node: Node,
    ) -> Self {
        let placements = Placements::new(shape_table);
        let mut nodes = Vec::with_capacity(15usize.pow(max_depth as u32));
        nodes.push(initial_node);
        Self {
            nodes,
            placements,
            scoring,
            max_depth,
        }
    }
}

impl<'s> Iterator for DFS<'s> {
    type Item = Node;

    fn next(&mut self) -> Option<Node> {
        let node = self.nodes.pop()?;
        if node.depth() < self.max_depth {
            let scoring = self.scoring;
            self.placements.set_state(node.state());
            for (idx, place) in (&mut self.placements).enumerate() {
                let succ = node.successor(scoring, idx, &place);
                self.nodes.push(succ);
            }
        }
        Some(node)
    }
}
