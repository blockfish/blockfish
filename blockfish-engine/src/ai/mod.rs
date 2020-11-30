mod a_star;
mod finesse;
mod place;
mod score;

#[cfg(test)]
mod dfs;
#[cfg(test)]
mod histogram;

use self::{
    a_star::a_star,
    finesse::inputs,
    place::{placements, Place},
    score::{penalty, score},
};
use crate::{
    shape::{srs, ShapeTable},
    BasicMatrix, Color, Input,
};
use std::sync::mpsc;

// Input / output data

pub use score::ScoreParams;

#[derive(Clone, Debug)]
pub struct Config {
    pub max_depth: usize,
    pub search_limit: usize,
    pub scoring: ScoreParams,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            max_depth: 6,
            search_limit: 10_000,
            scoring: ScoreParams::default(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Snapshot {
    pub hold: Option<Color>,
    pub queue: Vec<Color>,
    pub matrix: BasicMatrix,
}

#[derive(Clone, Debug)]
pub struct Suggestion {
    pub inputs: Vec<Input>,
    pub score: i64,
}

pub type SuggestionsIter = mpsc::Receiver<Suggestion>;

// AI thread

pub fn ai(cfg: Config, snapshot: Snapshot) -> SuggestionsIter {
    let Config {
        max_depth,
        search_limit,
        scoring,
    } = cfg;

    let srs = srs();
    let (tx, rx) = mpsc::sync_channel(100);
    std::thread::spawn(move || {
        let initial_placements: Vec<_> = placements(&srs, &snapshot).collect();
        let mut roots: Vec<_> = initial_placements
            .iter()
            .map(|place| Root::new(&srs, place))
            .collect();
        let initial_nodes = initial_placements
            .into_iter()
            .enumerate()
            .map(|(root_idx, place)| {
                let mut root_node = Node::new(root_idx, snapshot.clone());
                root_node.place(&srs, &scoring, place);
                root_node
            });

        let mut count = 0;
        let mut search = a_star(&srs, &scoring, initial_nodes);
        let mut best_score = std::i64::MAX;
        while let Some(node) = search.next() {
            count += 1;

            if node.depth < max_depth {
                continue;
            }

            if node.score < best_score {
                best_score = node.score;
                log::trace!(
                    "best: score {:>3}, root {:>2}, iteration {}",
                    best_score,
                    node.root_idx,
                    count
                );
            }

            let root = &mut roots[node.root_idx];
            root.best_score = std::cmp::min(node.score, root.best_score);

            if search.node_count() >= search_limit {
                break;
            }
        }
        log::info!("searched {} nodes to depth {}", count, max_depth);

        for root in roots.into_iter().filter(|r| r.best_score < std::i64::MAX) {
            let sugg = root.into_suggestion();
            if tx.send(sugg).is_err() {
                log::warn!("ai channel dropped; stopping early");
                return;
            }
        }
    });

    rx
}

// Roots of the search tree (for comparing best placement)

struct Root {
    inputs: Vec<Input>,
    best_score: i64,
}

impl Root {
    fn new(stb: &ShapeTable, place: &Place) -> Self {
        Self {
            inputs: inputs(stb, place.norm_id, place.col).collect(),
            best_score: std::i64::MAX,
        }
    }

    fn into_suggestion(self) -> Suggestion {
        Suggestion {
            inputs: self.inputs,
            score: self.best_score,
        }
    }
}

// Search nodes

#[derive(Clone)]
struct Node {
    root_idx: usize,
    snapshot: Snapshot,
    depth: usize,
    score: i64,
    penalty: i64,
}

impl Node {
    /// Constructs a new root node from `snapshot` with root index `root_idx`.
    ///
    /// Note: the score is not calculated initially, since this node will most likely be
    /// immediately used to derive new nodes; the initial score would be discarded
    /// anyways.
    fn new(root_idx: usize, snapshot: Snapshot) -> Self {
        Self {
            root_idx,
            snapshot,
            depth: 0,
            score: std::i64::MAX,
            penalty: 0,
        }
    }

    fn place(&mut self, stb: &ShapeTable, sp: &ScoreParams, place: Place) {
        // remove first from queue
        assert_eq!(self.snapshot.queue.get(0), Some(&place.norm_id.color()));
        self.snapshot.queue.remove(0);
        // place shape onto matrix
        let norm = &stb[place.norm_id];
        norm.blit_to(&mut self.snapshot.matrix, place.row, place.col);
        self.snapshot.matrix.sift_rows();
        // update score
        self.depth += 1;
        self.score = score(sp, &self.snapshot);
        self.penalty = penalty(sp, self.depth);
    }

    fn neighbors<'a>(
        &'a self,
        stb: &'a ShapeTable,
        sp: &'a ScoreParams,
    ) -> impl Iterator<Item = Node> + 'a {
        placements(&stb, &self.snapshot).map(move |place| {
            let mut node = self.clone();
            node.place(stb, sp, place);
            node
        })
    }
}

//////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    #[allow(unused)]
    use super::dfs::dfs;
    use super::*;
    use crate::basic_matrix;

    #[test]
    fn test_node_place() {
        let srs = srs();
        let sp = ScoreParams::default();
        let (xx, __) = (true, false);

        // x . . . .
        // x x . . .
        let matrix = basic_matrix![[xx, xx, __, __, __], [xx, __, __, __, __]];
        let mut node = Node::new(
            0,
            Snapshot {
                hold: None,
                queue: vec![Color('L'), Color('O')],
                matrix,
            },
        );
        assert_eq!(node.depth, 0);

        // x . . . L
        // x x L L L  ==>  x . . . L
        let l0 = srs.iter_norms_by_color(Color('L')).nth(0).unwrap();
        node.place(
            &srs,
            &sp,
            Place {
                norm_id: l0,
                row: 0,
                col: 2,
            },
        );
        assert_eq!(node.depth, 1);
        assert_eq!(node.snapshot.queue, [Color('O')]);
        assert_eq!(node.snapshot.matrix, basic_matrix![[xx, __, __, __, xx]]);

        // O O . . .                              . . . O O
        // O O . . .    . O O . .    . . O O .    . . . O O
        // x . . . L    x O O . L    x . O O L    x . . . L
        //    (1)          (2)          (3)          (4)
        assert_eq!(
            node.neighbors(&srs, &sp)
                .map(|node| {
                    assert_eq!(node.depth, 2);
                    assert_eq!(node.snapshot.queue, []);
                    node.snapshot.matrix
                })
                .collect::<Vec<_>>(),
            [
                basic_matrix![
                    [xx, __, __, __, xx],
                    [xx, xx, __, __, __],
                    [xx, xx, __, __, __],
                ],
                basic_matrix![[xx, xx, xx, __, xx], [__, xx, xx, __, __],],
                basic_matrix![[xx, __, xx, xx, xx], [__, __, xx, xx, __],],
                basic_matrix![
                    [xx, __, __, __, xx],
                    [__, __, __, xx, xx],
                    [__, __, __, xx, xx],
                ],
            ]
        );

        // queue empty
        let node = node.neighbors(&srs, &sp).nth(0).unwrap();
        assert_eq!(node.neighbors(&srs, &sp).count(), 0);
    }

    struct Stats(Vec<i64>);
    impl std::fmt::Display for Stats {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            let xs = self.0.as_slice();
            let len = xs.len() as f64;
            let avg = (xs.iter().cloned().sum::<i64>() as f64) / len;
            let sd = (xs.iter().map(|&x| (x as f64 - avg).powi(2)).sum::<f64>() / len).sqrt();
            write!(f, "avg: {:.2}, sd: {:.2}", avg, sd)
        }
    }

    fn snapshot_ex_4l_cheese() -> Snapshot {
        let (xx, __) = (true, false);
        Snapshot {
            hold: None,
            queue: "LTJIZSO".chars().map(Color).collect(),
            matrix: basic_matrix![
                [xx, xx, xx, xx, __, xx, xx, xx, xx, xx],
                [xx, xx, __, xx, xx, xx, xx, xx, xx, xx],
                [xx, xx, xx, xx, xx, xx, xx, xx, __, xx],
                [xx, __, xx, xx, xx, xx, xx, xx, xx, xx],
            ],
        }
    }

    #[cfg(feature = "slow-tests")]
    #[test]
    fn test_dfs() {
        let srs = srs();
        let sp = ScoreParams::default();
        let root = Node::new(0, snapshot_ex_4l_cheese());
        let dfs_nodes = dfs(&srs, &sp, 2, std::iter::once(root.clone())).collect::<Vec<_>>();
        assert_eq!(dfs_nodes.len(), 1 + 1 * 34 + 1 * 34 * 34);
        assert!(dfs_nodes.iter().all(|n| n.depth <= 2));
    }

    #[cfg(feature = "slow-tests")]
    #[test]
    fn test_a_star() {
        let srs = srs();
        let sp = ScoreParams::default();
        let root = Node::new(0, snapshot_ex_4l_cheese());

        static MAX_DEPTH: usize = 4;
        let mut best_score = std::i64::MAX;

        let mut dfs_traversed = 0u64;
        dfs(&srs, &sp, MAX_DEPTH, std::iter::once(root.clone())).for_each(|n| {
            dfs_traversed += 1;
            best_score = std::cmp::min(best_score, n.score);
        });

        let mut ast_traversed = 0u64;
        a_star(&srs, &sp, std::iter::once(root))
            .take_while(|n| n.depth < MAX_DEPTH || n.score > best_score)
            .for_each(|n| {
                println!("--| {:<3} {:?}", n.score, n.snapshot.matrix);
                ast_traversed += 1;
            });

        println!("");
        println!("best score @ depth {}: {}", MAX_DEPTH, best_score);
        println!("dfs nodes traversed: {}", dfs_traversed);
        println!("a* nodes traversed: {}", ast_traversed);

        assert!((ast_traversed * 1000) < dfs_traversed);
    }

    // #[test]
    #[allow(dead_code)]
    fn test_cheese_score_histogram() {
        use super::histogram::Histogram;
        let srs = srs();
        let sp = ScoreParams::default();

        let deeper = |nodes: &[Node]| -> Vec<Node> {
            nodes.iter().flat_map(|n| n.neighbors(&srs, &sp)).collect()
        };
        let s_hist = |nodes: &[Node]| nodes.iter().map(|n| n.score).collect::<Histogram>();
        let stats = |nodes: &[Node]| Stats(nodes.iter().map(|n| n.score).collect());

        let root = Node::new(0, snapshot_ex_4l_cheese());
        let depth1_nodes = deeper(&[root]);
        let depth2_nodes = deeper(&depth1_nodes);
        let depth3_nodes = deeper(&depth2_nodes);

        assert_eq!(depth1_nodes.len(), 34);
        assert_eq!(depth2_nodes.len(), 34 * 34);
        assert_eq!(depth3_nodes.len(), 34 * 34 * 34);

        print!(
            "----------\nDEPTH 1\n{} NODES\nSCORE:\n{}{}\n\n",
            depth1_nodes.len(),
            s_hist(&depth1_nodes),
            stats(&depth1_nodes),
        );
        print!(
            "----------\nDEPTH 2\n{} NODES\n\nSCORE:\n{}{}\n",
            depth2_nodes.len(),
            s_hist(&depth2_nodes),
            stats(&depth2_nodes),
        );
        print!(
            "----------\nDEPTH 3\n{} NODES\n\nSCORE:\n{}{}\n",
            depth3_nodes.len(),
            s_hist(&depth3_nodes),
            stats(&depth3_nodes),
        );
    }
}
