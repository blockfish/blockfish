use super::{Node, ScoreParams};
use crate::shape::ShapeTable;

pub(super) struct DFS<'s> {
    nodes: Vec<Node>,
    max_depth: usize,
    stbl: &'s ShapeTable,
    scoring: &'s ScoreParams,
}

pub(super) fn dfs<'s, I>(
    stbl: &'s ShapeTable,
    scoring: &'s ScoreParams,
    max_depth: usize,
    iter: I,
) -> DFS<'s>
where
    I: IntoIterator<Item = Node>,
{
    let mut nodes = Vec::with_capacity(15usize.pow(max_depth as u32));
    nodes.extend(iter);
    DFS {
        nodes,
        max_depth,
        stbl,
        scoring,
    }
}

impl<'s> Iterator for DFS<'s> {
    type Item = Node;

    fn next(&mut self) -> Option<Node> {
        let node = self.nodes.pop()?;
        if node.depth < self.max_depth {
            self.nodes.extend(node.successors(self.stbl, self.scoring));
        }
        Some(node)
    }
}
