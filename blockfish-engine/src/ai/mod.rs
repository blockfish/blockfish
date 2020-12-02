mod a_star;
mod finesse;
mod place;
mod score;

#[cfg(test)]
mod dfs;

use self::{
    a_star::a_star,
    place::{Place, PlacementSearch},
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
    pub search_limit: usize,
    pub scoring: ScoreParams,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            search_limit: 25_000,
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
        search_limit,
        scoring,
    } = cfg;

    let srs = srs();
    let (tx, rx) = mpsc::sync_channel(100);
    std::thread::spawn(move || {
        let initial_state: State = snapshot.into();
        let mut place_search = PlacementSearch::new(&srs);
        place_search.compute(&initial_state);

        let placement_inputs = place_search
            .placements()
            .map(|place| finesse::inputs(place_search.shape_table(), place).collect())
            .collect::<Vec<Vec<_>>>();
        let mut placement_scores = place_search
            .placements()
            .map(|_| std::i64::MAX)
            .collect::<Vec<_>>();

        let nodes = place_search
            .placements()
            .enumerate()
            .map(|(root_idx, place)| {
                let mut root_node = Node::new(root_idx, initial_state.clone());
                root_node.place(&srs, &scoring, place);
                root_node
            })
            .collect();

        let mut count = 0;
        let mut search = a_star(place_search, &scoring, nodes);
        let mut best_score = std::i64::MAX;

        while let Some(node) = search.next() {
            count += 1;

            if !node.state.is_max_depth() {
                continue;
            }

            if node.score < best_score {
                best_score = node.score;
                log::trace!(
                    "best: score {:>3}, root {:>2}, depth {}, iteration #{}",
                    best_score,
                    node.root_idx,
                    node.depth,
                    count
                );
            }

            let best = &mut placement_scores[node.root_idx];
            *best = std::cmp::min(node.score, *best);

            if search.node_count() >= search_limit {
                break;
            }
        }

        log::info!("searched {} nodes", count);

        for (inputs, score) in placement_inputs.into_iter().zip(placement_scores) {
            if score < std::i64::MAX {
                if tx.send(Suggestion { inputs, score }).is_err() {
                    log::warn!("ai channel dropped; stopping early");
                    return;
                }
            }
        }
    });

    rx
}

// Board state

#[derive(Clone)]
pub struct State {
    matrix: BasicMatrix,
    queue_rev: Vec<Color>,
    has_held: bool,
}

impl State {
    fn matrix(&self) -> &BasicMatrix {
        &self.matrix
    }

    /// Returns `true` if this state is at the max depth, so no further placements are
    /// possible.
    fn is_max_depth(&self) -> bool {
        self.queue_rev.is_empty()
    }

    /// Returns `(next_piece, hold_piece)`, where either may be `None` if not available.
    ///
    /// Note: `hold_piece` is not exactly the current piece in hold, rather the piece you
    /// will get if you press hold, i.e. if hold is currently empty then it refers to the
    /// 2nd piece in the queue.
    fn next(&self) -> (Option<Color>, Option<Color>) {
        let from_top = |i| {
            self.queue_rev
                .len()
                .checked_sub(i)
                .and_then(|i| self.queue_rev.get(i))
                .cloned()
        };
        let c1 = from_top(1);
        let c2 = from_top(2);
        if self.has_held {
            (c2, c1)
        } else {
            (c1, c2)
        }
    }

    /// Applies the given placement to this state, modifying the queue and matrix.
    fn place(&mut self, stbl: &ShapeTable, p: &Place) {
        stbl[p.norm_id].blit_to(&mut self.matrix, p.row, p.col);
        self.matrix.sift_rows();
        self.pop(p.did_hold);
    }

    /// Removes a piece from the next queue, or hold slot if `hold` is `true`.
    fn pop(&mut self, hold: bool) {
        //  | has_held | hold  | pos
        // -+----------+----------+----
        //  | true     | false | 2
        //  | true     | true  | 1
        //  | false    | false | 1
        //  | false    | true  | 2
        let pos = if self.has_held == hold { 1 } else { 2 };
        self.queue_rev.remove(self.queue_rev.len() - pos);
        self.has_held |= hold;
    }
}

impl From<Snapshot> for State {
    fn from(snapshot: Snapshot) -> Self {
        let matrix = snapshot.matrix;
        let mut queue_rev = snapshot.queue;
        queue_rev.reverse();
        let mut has_held = false;
        if let Some(hold_color) = snapshot.hold {
            has_held = true;
            queue_rev.push(hold_color);
        }
        Self {
            matrix,
            queue_rev,
            has_held,
        }
    }
}

// Search nodes

#[derive(Clone)]
pub struct Node {
    root_idx: usize,
    state: State,
    depth: usize,
    score: i64,
    penalty: i64,
}

impl Node {
    /// Constructs a new root node from state `state`. The root index `root_idx` indicates
    /// which initial placement this node arose from.
    ///
    /// Note: the score is not calculated initially, since this node will most likely be
    /// immediately used to derive new nodes; the initial score would be discarded
    /// anyways.
    fn new(root_idx: usize, state: State) -> Self {
        Self {
            root_idx,
            state,
            depth: 0,
            score: std::i64::MAX,
            penalty: 0,
        }
    }

    fn place(&mut self, stbl: &ShapeTable, sp: &ScoreParams, place: &Place) {
        self.state.place(&stbl, &place);
        self.depth += 1;
        self.score = score(sp, &self.state.matrix);
        self.penalty = penalty(sp, self.depth);
    }

    /// Returns a list of the "successor" to this node, using placements computed by the
    /// placement search `search`, and scores the successor using the parameters specified
    /// in `scoring`.
    fn successors<'a>(
        &'a self,
        search: &'a PlacementSearch,
        scoring: &'a ScoreParams,
    ) -> impl Iterator<Item = Node> + 'a {
        search.placements().map(move |place| {
            let mut node = self.clone();
            node.place(search.shape_table(), scoring, &place);
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

    fn place(stbl: &ShapeTable, color: Color, rot: usize, row: u16, col: u16) -> Place {
        let norm_id = stbl.iter_norms_by_color(color).nth(rot).unwrap();
        Place::new(norm_id, (row, col))
    }

    #[test]
    fn test_state_operations() {
        let queue = || "LTJI".chars().map(Color);

        let mut s: State = Snapshot {
            hold: None,
            queue: queue().collect(),
            matrix: BasicMatrix::with_cols(10),
        }
        .into();
        assert!(!s.is_max_depth());
        assert_eq!(s.matrix.rows(), 0);
        assert_eq!(s.matrix.cols(), 10);
        assert_eq!(s.next(), (Some(Color('L')), Some(Color('T'))));

        let srs = srs();
        for (i, color) in queue().enumerate() {
            s.place(&srs, &place(&srs, color, 0, (i * 2) as u16, 0));
        }
        assert!(s.is_max_depth());
        assert_eq!(s.matrix.rows(), 7);
        assert_eq!(s.next(), (None, None));
    }

    #[test]
    fn test_state_use_hold() {
        // something already in hold
        let mut s: State = Snapshot {
            hold: Some(Color('S')),
            queue: "LTJI".chars().map(Color).collect(),
            matrix: BasicMatrix::with_cols(10),
        }
        .into();
        assert_eq!(s.next(), (Some(Color('L')), Some(Color('S'))));
        s.pop(true);
        assert_eq!(s.next(), (Some(Color('T')), Some(Color('L'))));
        s.pop(false);
        assert_eq!(s.next(), (Some(Color('J')), Some(Color('L'))));
        // nothing previously in hold
        s = Snapshot {
            hold: None,
            queue: "LTJI".chars().map(Color).collect(),
            matrix: BasicMatrix::with_cols(10),
        }
        .into();
        assert_eq!(s.next(), (Some(Color('L')), Some(Color('T'))));
        s.pop(true);
        assert_eq!(s.next(), (Some(Color('J')), Some(Color('L'))));
    }

    #[test]
    fn test_state_nearly_empty_queue() {
        let mut s: State = Snapshot {
            hold: None,
            queue: vec![Color('I')],
            matrix: BasicMatrix::with_cols(10),
        }
        .into();
        assert_eq!(s.next(), (Some(Color('I')), None));
        s = Snapshot {
            hold: Some(Color('O')),
            queue: vec![],
            matrix: BasicMatrix::with_cols(10),
        }
        .into();
        assert_eq!(s.next(), (None, Some(Color('O'))));
    }

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
            }
            .into(),
        );
        assert_eq!(node.depth, 0);

        // x . . . L
        // x x L L L  ==>  x . . . L
        node.place(&srs, &sp, &place(&srs, Color('L'), 0, 0, 2));
        assert_eq!(node.depth, 1);
        assert_eq!(node.state.next().0, Some(Color('O')));
        assert_eq!(node.state.matrix(), &basic_matrix![[xx, __, __, __, xx]]);

        // O O . . .                              . . . O O
        // O O . . .    . O O . .    . . O O .    . . . O O
        // x . . . L    x O O . L    x . O O L    x . . . L
        //    (1)          (2)          (3)          (4)
        let mut ps = PlacementSearch::new(&srs);
        ps.compute(&node.state);
        assert_eq!(
            node.successors(&ps, &sp)
                .map(|node| {
                    assert_eq!(node.depth, 2);
                    assert!(node.state.is_max_depth());
                    node.state.matrix().clone()
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
    }

    #[cfg(feature = "slow-tests")]
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
        let root = Node::new(0, snapshot_ex_4l_cheese().into());
        let dfs_nodes = dfs(
            PlacementSearch::new(&srs),
            &sp,
            3,
            std::iter::once(root.clone()),
        )
        .collect::<Vec<_>>();
        assert_eq!(
            dfs_nodes.len(),
            //  | sequence        | placements
            // -+-----------------+--------------
            //  | .               | 1
            //  | L,      T       | 34
            //  | LT, LJ, TJ, TL  | 34^2
            //  | LTJ,LJT,TJL,TLJ | 34^3
            //  | LTI,LJI,TJI,TLI | 34^2 * 17
            1 + (2 * 34) + (4 * 34 * 34) + (4 * 34 * 34 * 34) + (4 * 34 * 34 * 17)
        );
        assert!(dfs_nodes.iter().all(|n| n.depth <= 3));
    }

    #[cfg(feature = "slow-tests")]
    #[test]
    fn test_a_star() {
        let srs = srs();
        let sp = ScoreParams::default();
        let root = Node::new(0, snapshot_ex_4l_cheese().into());

        static MAX_DEPTH: usize = 4;
        let mut best_score = std::i64::MAX;

        let mut dfs_traversed = 0u64;
        dfs(
            PlacementSearch::new(&srs),
            &sp,
            MAX_DEPTH,
            std::iter::once(root.clone()),
        )
        .for_each(|n| {
            dfs_traversed += 1;
            best_score = std::cmp::min(best_score, n.score);
        });

        let mut ast_traversed = 0u64;
        let mut ast = a_star(PlacementSearch::new(&srs), &sp, vec![root]);
        (&mut ast)
            .take_while(|n| n.depth < MAX_DEPTH || n.score > best_score)
            .for_each(|n| {
                println!("--| {:<3} {:?}", n.score, n.state.matrix);
                ast_traversed += 1;
            });

        println!("");
        println!("best score @ depth {}: {}", MAX_DEPTH, best_score);
        println!("dfs nodes traversed: {}", dfs_traversed);
        println!("a* nodes traversed: {}", ast_traversed);
        println!("a* heap size: {}", ast.node_count());

        assert!((ast_traversed * 5000) < dfs_traversed);
    }
}
