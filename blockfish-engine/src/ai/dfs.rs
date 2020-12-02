use super::{place::PlacementSearch, Node, ScoreParams};

pub(super) struct DFS<'s> {
    nodes: Vec<Node>,
    max_depth: usize,
    place_search: PlacementSearch<'s>,
    scoring: &'s ScoreParams,
}

pub(super) fn dfs<'s, I>(
    place_search: PlacementSearch<'s>,
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
        place_search,
        scoring,
    }
}

impl<'s> Iterator for DFS<'s> {
    type Item = Node;

    fn next(&mut self) -> Option<Node> {
        let node = self.nodes.pop()?;
        if node.depth < self.max_depth {
            self.place_search.compute(&node.state);
            let succ = node.successors(&self.place_search, self.scoring);
            self.nodes.extend(succ);
        }
        Some(node)
    }
}
