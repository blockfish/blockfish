use crate::{
    controls::{Action, EngineOp, GameOp, TreeOp},
    view::View,
};
use block_stacker::Stacker;
use blockfish::{ai, Parameters, StackerExt as _};

use bitflags::bitflags;

// limit on number of times to poll per frame -- prevents lagging out the UI thread.
const MAX_POLLS_PER_FRAME: u32 = 64;

/// Controls a view according to changes in a `Stacker` state, incoming
/// engine suggestions, and user actions.
pub struct Controller<'v> {
    ai: ai::AI,
    view: View<'v>,
    stacker: Stacker,
    progress: Progress,
    undo_list: Vec<(Stacker, Progress)>,
    analysis: Option<Analysis>,
    trie: Option<Trie>,
}

bitflags! {
    struct Update: u8 {
        const QUEUE   = 0b00000001; // just hold & next queue
        const PIECE   = 0b00000010; // just current piece
        const MATRIX  = 0b00000100; // just matrix cells
        const STATS   = 0b00001000; // just stats labels
        const STACKER = 0b00001111; // anything stacker related

        const AI      = 0b00010000; // just analysis suggestions
        const TRIE    = 0b00100000; // just tr(e/i)e sidebar
        const ENGINE  = 0b00110000; // anything engine related
    }
}

impl<'v> Controller<'v> {
    /// Constructs a new `Controller`.
    pub fn new(ai: ai::AI, view: View<'v>, stacker: Stacker) -> Self {
        let mut ctl = Self {
            ai,
            view,
            stacker,
            trie: None,
            undo_list: Vec::with_capacity(100),
            progress: Progress::new(),
            analysis: None,
        };

        ctl.consult_engine();
        ctl.undo_save();
        ctl.update_view(Update::all());
        ctl
    }

    pub fn view(&self) -> &View<'v> {
        &self.view
    }

    fn update_view(&mut self, upd: Update) {
        if upd.contains(Update::QUEUE) {
            let next = self.stacker.next().iter().cloned();
            self.view.set_queue(next, self.stacker.held());
        }
        if upd.contains(Update::PIECE) {
            self.view.set_piece(self.stacker.current_piece());
        }
        if upd.contains(Update::MATRIX) {
            self.view.set_matrix(self.stacker.matrix());
        }
        if upd.contains(Update::STATS) {
            self.view.set_stats(
                self.progress.pieces,
                self.progress.lines,
                self.progress.color_clears,
                self.progress.downstack,
                self.stacker.config().garbage.total_lines,
            );
        }
        if upd.contains(Update::AI) {
            if let Some(an) = self.analysis.as_mut() {
                an.update_view(&mut self.view)
            } else {
                self.view.set_engine_disabled();
            }
        }
        if upd.contains(Update::TRIE) {
            if let Some(trie) = self.trie.as_mut() {
                self.view.set_tree_enabled();
                trie.update_view(&mut self.view);
            } else {
                self.view.set_tree_disabled();
            }
        }
    }

    /// Ends the previous engine process and starts an analysis of the current stacker
    /// state.
    fn consult_engine(&mut self) {
        if let Some(trie) = self.trie.as_mut() {
            // TODO: configure `self.ai` to push nodes to some channel that trie can see??
            trie.reroot(self.stacker.clone(), &self.ai.config().parameters);
        }
        self.analysis = Some(Analysis::new(&mut self.ai, self.stacker.clone()));
    }

    /// Disables the engine.
    fn disable_engine(&mut self) {
        self.analysis = None;
        if let Some(trie) = self.trie.as_mut() {
            trie.clear();
        }
    }

    /// Polls the engine process for any updates to its progress.
    pub fn poll_engine(&mut self) {
        if let Some(an) = self.analysis.as_mut() {
            if an.poll() {
                self.update_view(Update::ENGINE);
            }
        }
    }

    /// Save the current state to the undo list.
    fn undo_save(&mut self) {
        let stacker = self.stacker.clone();
        let progress = self.progress.clone();
        self.undo_list.push((stacker, progress));
    }

    /// Restore the current state from the undo list.
    fn undo_restore(&mut self) {
        let (stacker, progress) = self.undo_list.pop().expect("undo list cannot be empty!");
        self.stacker = stacker;
        self.progress = progress;
    }

    /// Hard drops the current piece and updates `progress` as a result.
    fn hard_drop(&mut self) {
        let (lc, ds) = self.stacker.hard_drop();
        let cc = self.stacker.is_matrix_colorless();
        self.progress.incr(lc, ds, cc);
    }

    /// Handles the user action `action`.
    pub fn handle(&mut self, action: Action) {
        match action {
            Action::Game(op) => self.handle_game_op(op),
            Action::Engine(op) => self.handle_engine_op(op),
            Action::Tree(op) => self.handle_tree_op(op),
        }
    }

    /// Handles a game input, updating both the stacker state and view accordingly.
    fn handle_game_op(&mut self, op: GameOp) {
        let mut upd = Update::empty();
        match op {
            GameOp::MoveLeft | GameOp::MoveRight => {
                let dx = if op == GameOp::MoveLeft { -1 } else { 1 };
                if self.stacker.move_horizontal(dx) {
                    upd |= Update::PIECE;
                }
            }
            GameOp::RotateCCW | GameOp::RotateCW => {
                let dr = if op == GameOp::RotateCCW { -1 } else { 1 };
                if self.stacker.rotate(dr) {
                    upd |= Update::PIECE;
                }
            }
            GameOp::SonicDrop => {
                if self.stacker.sonic_drop() {
                    upd |= Update::PIECE;
                }
            }
            GameOp::Hold => {
                if self.stacker.hold() {
                    upd |= Update::PIECE | Update::QUEUE;
                }
            }
            GameOp::HardDrop => {
                self.undo_save();
                self.hard_drop();
                upd |= Update::STACKER;
            }
            GameOp::Reset => {
                let ruleset = self.stacker.ruleset().clone();
                let mut cfg = self.stacker.config();
                cfg.prng_seed = None; // don't use the same RNG seed
                self.stacker = Stacker::new(ruleset, cfg);
                self.progress = Progress::new();
                self.undo_list.clear();
                self.undo_save();
                upd |= Update::STACKER;
            }
            GameOp::Undo => {
                self.undo_restore();
                if self.undo_list.is_empty() {
                    self.undo_save();
                }
                upd |= Update::STACKER;
            }
        }

        // run a new analysis if the matrix contents were changed
        if self.analysis.is_some() && upd.intersects(Update::MATRIX) {
            self.consult_engine();
            upd |= Update::ENGINE;
        }

        self.update_view(upd);
    }

    /// Handles an engine related user action.
    fn handle_engine_op(&mut self, op: EngineOp) {
        if let Some(an) = self.analysis.take() {
            self.handle_engine_op_enabled(op, an);
        } else {
            match op {
                EngineOp::Toggle => {
                    self.consult_engine();
                    self.update_view(Update::ENGINE);
                }
                _ => {}
            }
        }
    }

    /// Handles an engine related user action, when engine is enabled. This function is
    /// responsible for setting `self.analysis` before returning.
    fn handle_engine_op_enabled(&mut self, op: EngineOp, mut an: Analysis) {
        let mut upd = Update::empty();
        match op {
            EngineOp::Toggle => {
                upd |= Update::ENGINE;
                self.disable_engine();
            }
            EngineOp::Next | EngineOp::Prev => {
                let delta = if op == EngineOp::Prev { -1 } else { 1 };
                if an.nav(delta, 0) {
                    upd |= Update::AI;
                }
                self.analysis = Some(an);
            }
            EngineOp::StepForward | EngineOp::StepBackward => {
                let step = if op == EngineOp::StepBackward { -1 } else { 1 };
                if an.nav(0, step) {
                    upd |= Update::AI;
                }
                self.analysis = Some(an);
            }
            EngineOp::Goto => {
                if an.go_to(&mut self.stacker) {
                    upd |= Update::STACKER;
                }
                self.analysis = Some(an);
            }
            EngineOp::AutoPlay => {
                if an.go_to(&mut self.stacker) {
                    self.undo_save();
                    self.hard_drop();
                    self.consult_engine();
                    upd |= Update::STACKER | Update::ENGINE;
                } else {
                    self.analysis = Some(an);
                }
            }
        }
        self.update_view(upd);
    }

    /// Handles a tree related user action.
    fn handle_tree_op(&mut self, op: TreeOp) {
        if let Some(trie) = self.trie.take() {
            self.handle_tree_op_enabled(op, trie);
        } else {
            match op {
                TreeOp::Toggle => {
                    self.trie = Some(Trie::new());
                    self.update_view(Update::TRIE);
                }
                _ => {}
            }
        }
    }

    /// Handles a tree related user action, when tree is enabled. This function is
    /// responsible for setting `self.trie` before returning.
    fn handle_tree_op_enabled(&mut self, op: TreeOp, mut trie: Trie) {
        let mut upd = Update::empty();
        match op {
            TreeOp::Toggle => {
                upd |= Update::TRIE;
                self.trie = None;
            }
            TreeOp::OverEntry(idx) => {
                trie.set_hover(Some(idx));
                upd |= Update::TRIE;
                self.trie = Some(trie);
            }
            TreeOp::Out => {
                trie.set_hover(None);
                // display the analysis preview again after hovering away from trie
                upd |= Update::AI;
                self.trie = Some(trie);
            }
            TreeOp::ClickEntry(idx) => {
                if trie.expand(idx) {
                    upd |= Update::TRIE;
                }
                self.trie = Some(trie);
            }
            TreeOp::ScrollBy(dy) => {
                trie.scroll_by(dy);
                upd |= Update::TRIE;
                self.trie = Some(trie);
            }
        }
        self.update_view(upd);
    }
}

/// Holds the current statistics of this cheese race.
#[derive(Clone)]
struct Progress {
    pieces: usize,
    lines: usize,
    downstack: usize,
    color_clears: usize,
}

impl Progress {
    /// Constructs a new `Progress` representing the very beginning of a race (no pieces
    /// placed).
    fn new() -> Self {
        Self {
            pieces: 0,
            lines: 0,
            downstack: 0,
            color_clears: 0,
        }
    }

    /// Increment the piece counter by one, and the other counters according to what the
    /// effect of the placement.
    ///
    /// - `lc`: number of lines cleared by the placement
    /// - `ds`: number of garbage lines cleared by the placement
    /// - `cc`: `true` if the placement was a color clear
    fn incr(&mut self, lc: usize, ds: usize, cc: bool) {
        self.pieces += 1;
        self.lines += lc;
        self.downstack += ds;
        self.color_clears += if cc { 1 } else { 0 };
    }
}

/// Holds the current state of a background analysis, the suggested moves, and which move
/// is selected.
struct Analysis {
    // analysis, if any. if `None`, then the analysis is finished (or never ran).
    analysis: Option<ai::Analysis>,
    // `(cfg_string, Some((time, nodes, iters)))`. the triple is just sent to the view.
    status: (String, Option<(f64, usize, usize)>),
    // initial game state when the analysis began
    src: Stacker,
    // current game state preview, for showing a step in a sequence
    preview: Stacker,
    // all moves collected from the analysis, sorted best -> worst
    moves: Vec<Move>,
    // currently selected move index
    sel_idx: usize,
    // current position in move sequence
    sel_pos: usize,
}

/// A move suggested by the analysis.
struct Move {
    id: ai::MoveId,
    rating: i64,
    inputs: Vec<blockfish::Input>,
}

impl Analysis {
    /// Starts an analysis using blockfish instance `ai`, taking a snapshot of game state
    /// `src`.
    fn new(ai: &mut ai::AI, mut src: Stacker) -> Self {
        // `reset_piece()` is required so simulating the input list works
        src.reset_piece();
        // show just the snapshot in the initial preview (before any suggestion arrives)
        let mut preview = src.clone();
        preview.freeze();
        Self {
            analysis: src.snapshot().map(|ss| ai.analyze(ss)),
            status: (format!("{}", ai.config()), None),
            moves: vec![],
            sel_idx: 0,
            sel_pos: 0,
            src,
            preview,
        }
    }

    /// Polls the background analysis for updates. Returns `true` if anything changed as a
    /// result of new analysis results.
    fn poll(&mut self) -> bool {
        let mut an = match self.analysis.take() {
            Some(an) => an,
            None => return false,
        };
        let mut updated = false;
        for _ in 0..MAX_POLLS_PER_FRAME {
            match an.poll() {
                Ok(Some(m_id)) => updated |= self.update(m_id, &an),
                Ok(None) => break,
                Err(ai::AnalysisDone) => {
                    log::info!("analysis finished");
                    self.status.1 = an.stats().map(|stats| {
                        let time = stats.time_taken.as_secs_f64();
                        let nodes = stats.nodes;
                        let iters = stats.iterations;
                        (time, nodes, iters)
                    });
                    // early return ends analysis
                    return true;
                }
            }
        }
        self.analysis = Some(an);
        updated
    }

    /// Updates the stored moves by bringing move `m_id` up to date according to
    /// `analysis`.
    fn update(&mut self, m_id: ai::MoveId, analysis: &ai::Analysis) -> bool {
        let mov = match self.moves.iter_mut().find(|m| m.id == m_id) {
            Some(m) => m,
            None => {
                self.moves.push(Move::new(m_id));
                self.moves.last_mut().unwrap()
            }
        };
        let sugg = analysis.suggestion(m_id, std::usize::MAX);
        mov.rating = sugg.rating;
        mov.inputs = sugg.inputs;
        // unstable sort OK because `cmp` does not cause ties.
        let cmp = |m1: &Move, m2: &Move| analysis.cmp(m1.id, m2.id);
        self.moves.sort_unstable_by(cmp);
        self.nav(0, 0)
    }

    /// Performs some kind of nagivation through the visible moves. `idx_off` is the
    /// amount to change the currently selected index (i.e. `1` for next, `-1` for
    /// previous), and `step_off` is the amount to change the current position in the
    /// sequence.
    fn nav(&mut self, idx_off: isize, step_off: isize) -> bool {
        if self.moves.is_empty() {
            return false;
        }
        // offset index (wrapping)
        let len = self.moves.len();
        self.sel_idx = (self.sel_idx + (len as isize + idx_off) as usize) % len;
        let mov = &self.moves[self.sel_idx];
        if idx_off == 0 {
            // offset position (wrapping)
            let steps = mov.steps();
            self.sel_pos = (self.sel_pos + (steps as isize + step_off) as usize) % steps;
        } else {
            // if index changed, then always go to first position
            self.sel_pos = 0;
        }
        // update the stacker preview
        self.preview.clone_from(&self.src);
        self.preview.freeze();
        self.preview.run(mov.inputs_prefix(self.sel_pos));
        true
    }

    /// Sets the given game state to the selected move. Returns `true` if the game state
    /// was modified as a result, or `false` if it wasn't (e.g. analysis hasn't finished so
    /// the move shouldn't be committed to).
    fn go_to(&self, dst: &mut Stacker) -> bool {
        if self.analysis.is_some() || self.moves.is_empty() {
            return false;
        }
        dst.clone_from(&self.src);
        dst.run(self.moves[self.sel_idx].inputs_prefix(0));
        true
    }

    /// Updates the engine elements of `view`.
    fn update_view(&mut self, view: &mut View) {
        // update status text
        view.set_engine_status(&self.status.0, self.status.1);

        // update overlay
        let show_piece;
        if let Some(sel) = &self.moves.get(self.sel_idx) {
            let len = self.moves.len();
            let steps = sel.steps();
            view.set_engine_overlay((self.sel_idx, len), (self.sel_pos, steps), sel.rating);
            show_piece = self.sel_pos < steps - 1;
        } else {
            view.clear_engine_overlay();
            show_piece = false;
        }

        // update matrix/piece indicator
        view.set_engine_matrix(self.preview.matrix());
        view.set_engine_piece(if show_piece {
            self.preview.current_piece_ghost()
        } else {
            None
        });
    }
}

impl Move {
    fn new(id: ai::MoveId) -> Self {
        Self {
            id,
            inputs: vec![],
            rating: std::i64::MAX,
        }
    }

    /// Returns the number of "steps" (piece placements) in this move's input sequence.
    fn steps(&self) -> usize {
        self.inputs
            .iter()
            .filter(|&&i| i == blockfish::Input::HD)
            .count()
            + 1
    }

    /// Returns a prefix of the input sequence up to the first `pos` piece placements. The
    /// last piece will be moved to but not placed; e.g. `.inputs_prefix(0)` moves the
    /// piece to the first placement but does not lock in.
    fn inputs_prefix<'a>(&'a self, mut pos: usize) -> impl Iterator<Item = blockfish::Input> + 'a {
        self.inputs
            .iter()
            .take_while(move |&&i| {
                if i == blockfish::Input::HD {
                    pos = match pos.checked_sub(1) {
                        Some(pos) => pos,
                        None => return false,
                    };
                }
                true
            })
            .cloned()
    }
}

/// Currently defunct -- needs fixing!
///
/// Holds a trie representation of all discovered nodes, with a linearized view for the
/// tree sidebar. Implements UI actions such as hovering, scrolling, and collapsing nodes.
struct Trie {
    parameters: Parameters,
    nodes: Vec<TrieNode>,
    linear: Vec<TrieNodeId>,
    hover: Option<usize>,
    scroll: i32,
}

struct TrieNode {
    key: Vec<blockfish::Input>,
    stacker: Stacker,
    depth: usize,
    eval: Option<ai::Eval>,
    rating: Option<i64>,
    best_rating: i64,
    count: usize,
    children: Vec<TrieNodeId>,
    expanded: bool,
}

type TrieNodeId = usize;

impl Trie {
    fn new() -> Self {
        Self {
            parameters: Parameters::default(),
            nodes: vec![],
            linear: vec![],
            hover: None,
            scroll: 0,
        }
    }

    fn clear(&mut self) {
        self.nodes.clear();
        self.linear.clear();
    }

    fn reroot(&mut self, mut stacker: Stacker, params: &Parameters) {
        stacker.freeze();
        stacker.reset_piece();
        let root = TrieNode::new(vec![], stacker, std::usize::MAX);
        self.nodes.clear();
        self.nodes.push(root);
        self.linearize();
        self.parameters.clone_from(params);
    }

    fn set_hover(&mut self, hover: Option<usize>) {
        self.hover = hover;
    }

    #[allow(unused)]
    fn insert(&mut self, inputs: &[blockfish::Input], rating: i64) {
        if self.nodes.is_empty() {
            // cannot insert if no root
            return;
        }

        let mut stacker = self[0].stacker.clone();
        let mut node_id = 0; // root
        for (depth, prefix) in inputs.split(|&i| i == blockfish::Input::HD).enumerate() {
            stacker.run(prefix.iter().cloned());

            // create new child if needed
            let mut children = std::mem::take(&mut self[node_id].children);
            let child_id = children
                .iter()
                .find(|&&id| &*self[id].key == prefix)
                .cloned();
            let child_id = child_id.unwrap_or_else(|| {
                let node = TrieNode::new(prefix.to_vec(), stacker.clone(), depth);
                self.nodes.push(node);
                let id = self.nodes.len() - 1;
                children.push(id);
                id
            });

            // update rating & leaf count
            self[child_id].best_rating = std::cmp::min(self[child_id].best_rating, rating);
            self[child_id].count += 1;
            // re-sort child nodes
            children.sort_by_key(|&id| self[id].best_rating);
            self[node_id].children = children;
            node_id = child_id;
            stacker.hard_drop();
        }

        // fixup final node
        self[node_id].eval = stacker.snapshot().as_ref().map(ai::static_eval);
        self[node_id].rating = Some(rating);
        // re-build the linear tree repr
        // TODO: optimization: dont linearize if the update changed an unexpanded node
        self.linearize();
    }

    fn expand(&mut self, entry_idx: usize) -> bool {
        if let Some(&id) = self.linear.get(entry_idx) {
            let node = &mut self[id];
            node.expanded = !node.expanded;
            self.linearize();
            true
        } else {
            false
        }
    }

    fn linearize(&mut self) {
        self.linear.clear();
        // initialize with root nodes
        let mut stack = self[0].children.clone();
        stack.reverse();
        while let Some(id) = stack.pop() {
            let node = &self[id];
            if node.expanded {
                // push in reverse order, so we `pop()` in order
                stack.extend(node.children.iter().rev().cloned());
            }
            self.linear.push(id);
        }
    }

    fn scroll_by(&mut self, dy: i32) {
        self.scroll += dy;
    }

    fn update_view(&mut self, view: &mut View) {
        view.set_tree_nodes(self.linear.iter().map(|&id| self[id].view()));

        view.set_tree_hover(self.hover);
        if let Some(&hover_id) = self.hover.and_then(|idx| self.linear.get(idx)) {
            let preview = &self[hover_id].stacker;
            view.set_engine_matrix(preview.matrix());
            view.set_engine_piece(preview.current_piece_ghost());
            if let Some(eval) = self[hover_id].eval.as_ref() {
                view.set_engine_overlay_score(eval, &self.parameters);
            } else {
                view.clear_engine_overlay();
            }
        }

        let (scroll_min, scroll_max) = view.tree_scroll_bounds().into_inner();
        self.scroll = std::cmp::min(std::cmp::max(self.scroll, scroll_min), scroll_max);
        view.set_tree_scroll(self.scroll);
    }
}

impl std::ops::Index<TrieNodeId> for Trie {
    type Output = TrieNode;
    fn index(&self, id: TrieNodeId) -> &TrieNode {
        &self.nodes[id]
    }
}

impl std::ops::IndexMut<TrieNodeId> for Trie {
    fn index_mut(&mut self, id: TrieNodeId) -> &mut TrieNode {
        &mut self.nodes[id]
    }
}

impl TrieNode {
    fn new(key: Vec<blockfish::Input>, stacker: Stacker, depth: usize) -> Self {
        Self {
            key,
            stacker,
            depth,
            eval: None,
            rating: None,
            best_rating: std::i64::MAX,
            count: 0,
            children: vec![],
            expanded: false,
        }
    }

    fn view(&self) -> crate::view::TreeNode {
        crate::view::TreeNode {
            piece: self.stacker.current_piece_type().unwrap_or('H'),
            depth: self.depth,
            count: self.count,
            rating: self.rating,
            best_rating: self.best_rating,
        }
    }
}
