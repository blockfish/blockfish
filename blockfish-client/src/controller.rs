use crate::{
    controls::{Action, EngineOp, GameOp, TreeOp},
    view::View,
};
use block_stacker::Stacker;
use blockfish::{Config as BFConfig, StackerExt as _};

use bitflags::bitflags;

/// Controls a view according to changes in a `Stacker` state, incoming
/// engine suggestions, and user actions.
pub struct Controller<'v> {
    view: Box<View<'v>>,
    stacker: Stacker,
    progress: Progress,
    undo_list: Vec<(Stacker, Progress)>,
    ai_config: BFConfig,
    engine: Option<Engine>,
    trie: Option<Trie>,
}

bitflags! {
    struct Update: u8 {
        const QUEUE   = 0b00000001; // just hold & next queue
        const PIECE   = 0b00000010; // just current piece
        const MATRIX  = 0b00000100; // just matrix cells
        const STATS   = 0b00001000; // just stats labels
        const STACKER = 0b00001111; // anything stacker related

        const AI      = 0b00010000; // just ai info/preview
        const TRIE    = 0b00100000; // just tr(e/i)e sidebar
        const ENGINE  = 0b00110000; // anything engine related
    }
}

impl<'v> Controller<'v> {
    /// Constructs a new `Controller`.
    pub fn new(stacker: Stacker, view: Box<View<'v>>, ai_config: BFConfig) -> Self {
        let mut ctl = Self {
            view,
            ai_config,
            stacker,
            trie: None,
            undo_list: Vec::with_capacity(100),
            progress: Progress::new(),
            engine: None,
        };

        ctl.consult_engine();
        ctl.undo_save();
        ctl.update_view(Update::all());
        ctl
    }

    pub fn view(&self) -> &View<'v> {
        &self.view
    }

    pub fn view_mut(&mut self) -> &mut View<'v> {
        &mut self.view
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
            if let Some(eng) = self.engine.as_ref() {
                eng.update_view(&mut self.view)
            } else {
                self.view.set_engine_disabled();
            }
        }
        if upd.contains(Update::TRIE) {
            if let Some(trie) = self.trie.as_mut() {
                self.view.enable_tree();
                trie.update_view(&mut self.view);
            } else {
                self.view.disable_tree();
            }
        }
    }

    /// Ends the previous engine process and starts an analysis of the current stacker
    /// state.
    fn consult_engine(&mut self) {
        self.engine = Some(Engine::new(
            self.ai_config.clone(),
            self.stacker.clone(),
            self.trie.is_some(),
        ));
        if let Some(trie) = self.trie.as_mut() {
            trie.reroot(self.stacker.clone(), &self.ai_config.scoring);
        }
    }

    /// Disables the engine
    fn disable_engine(&mut self) {
        self.engine = None;
        if let Some(trie) = self.trie.as_mut() {
            trie.clear();
        }
    }

    /// Polls the engine process for any updates to its progress.
    pub fn poll_engine(&mut self) {
        if let Some(eng) = self.engine.as_mut() {
            if eng.poll(self.trie.as_mut()) {
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
                let (lc, ds) = self.stacker.hard_drop();
                let cc = self.stacker.is_matrix_colorless();
                self.progress.incr(lc, ds, cc);
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

        if self.engine.is_some() && upd.intersects(Update::MATRIX) {
            self.consult_engine();
            upd |= Update::ENGINE;
        }

        self.update_view(upd);
    }

    /// Handles an engine related user action.
    fn handle_engine_op(&mut self, op: EngineOp) {
        if let Some(eng) = self.engine.take() {
            self.handle_engine_op_enabled(op, eng);
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
    /// responsible for setting `self.engine` before returning.
    fn handle_engine_op_enabled(&mut self, op: EngineOp, mut eng: Engine) {
        match op {
            EngineOp::Toggle => {
                self.disable_engine();
                self.update_view(Update::ENGINE);
            }
            EngineOp::Next | EngineOp::Prev => {
                let delta = if op == EngineOp::Prev { -1 } else { 1 };
                eng.select(delta, 0);
                self.engine = Some(eng);
                self.update_view(Update::AI);
            }
            EngineOp::Goto => {
                // TODO: maybe this should be a GameOp? since it exclusively affects .stacker
                //       and not .engine
                let updated = eng.go_to_selection(&mut self.stacker);
                self.engine = Some(eng);
                if updated {
                    self.update_view(Update::STACKER);
                }
            }
            EngineOp::StepForward | EngineOp::StepBackward => {
                let step = if op == EngineOp::StepBackward { -1 } else { 1 };
                eng.select(0, step);
                self.engine = Some(eng);
                self.update_view(Update::AI);
            }
        }
    }

    /// Handles an tree related user action, when tree is enabled. This function is
    /// responsible for setting `self.trie` before returning.
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

    /// Handles an tree related user action, when tree is enabled. This function is
    /// responsible for setting `self.trie` before returning.
    fn handle_tree_op_enabled(&mut self, op: TreeOp, mut trie: Trie) {
        match op {
            TreeOp::Toggle => {
                self.update_view(Update::TRIE);
            }
            TreeOp::OverEntry(idx) => {
                trie.set_hover(Some(idx));
                self.trie = Some(trie);
                self.update_view(Update::TRIE);
            }
            TreeOp::Out => {
                trie.set_hover(None);
                self.trie = Some(trie);
                self.update_view(Update::ENGINE);
            }
            TreeOp::ClickEntry(idx) => {
                let updated = trie.expand(idx);
                self.trie = Some(trie);
                if updated {
                    self.update_view(Update::TRIE);
                }
            }
            TreeOp::ScrollBy(dy) => {
                trie.scroll_by(dy);
                self.trie = Some(trie);
                self.update_view(Update::TRIE);
            }
        }
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
    /// Constructs a new `Progress` represented the very beginning of a race (no pieces
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

/// Holds a handle to a Blockfish AI process, the top suggestions produced by that
/// process, and the current suggestion being selected by the user.
struct Engine {
    start_time: std::time::Instant,
    params: String,
    source: Stacker,
    ai: Option<blockfish::Analysis>,
    suggestions: Vec<Suggestion>,
    selection: Option<Selection>,
    stats: Option<(f64, usize, usize)>,
}

struct Suggestion(blockfish::Suggestion);

struct Selection {
    idx: usize,
    pos: usize,
    preview: Stacker,
}

impl Engine {
    /// Constructs a new `Engine` by starting a new Blockfish process with configuration
    /// parameters `config`, starting at the stacker state `source`. If `collect_all` is
    /// `true`, then all suggestions will be collected. Otherwise only the ones that win
    /// their individual branches. The latter is faster but the former is better for
    /// diagnostics (ie. when the tree sidebar is visible).
    fn new(config: BFConfig, mut source: Stacker, collect_all: bool) -> Self {
        source.reset_piece();
        let params = format!("{}", config);
        Self {
            start_time: std::time::Instant::now(),
            ai: match source.snapshot() {
                Some(ss) => {
                    let mut ai = blockfish::AI::new(config);
                    ai.set_suggestion_filter(if collect_all {
                        blockfish::SuggestionFilter::All
                    } else {
                        blockfish::SuggestionFilter::LocalBest
                    });
                    Some(ai.analyze(ss))
                }
                None => None,
            },
            suggestions: vec![],
            selection: None,
            stats: None,
            params,
            source,
        }
    }

    /// Updates `view` to show the current state of the engine process, and the selected
    /// suggestion.
    // TODO: partial updates for the engine matrix
    fn update_view(&self, view: &mut View) {
        view.set_engine_status(&self.params, self.stats);
        if let Some(sel) = self.selection.as_ref() {
            view.set_engine_matrix(sel.preview.matrix());
            view.set_engine_piece(sel.preview.current_piece_ghost());
            if self.finished() {
                let sugg = &self.suggestions[sel.idx];
                let seq = (sel.idx, self.suggestions.len());
                let pos = (sel.pos, sugg.steps());
                let rating = sugg.rating();
                view.set_engine_overlay(seq, pos, rating);
                let is_last_step = pos.0 == pos.1 - 1;
                if is_last_step {
                    view.set_engine_piece(None);
                }
            } else {
                view.clear_engine_overlay();
            }
        } else {
            view.set_engine_matrix(self.source.matrix().map(|(ij, _)| (ij, 'H')));
            view.set_engine_piece(None);
            view.clear_engine_overlay();
        }
    }

    /// Adds `sugg` to the list of suggestions, or replaces an existing suggestion, then
    /// re-sorts the suggestions.
    fn insert(&mut self, new_sugg: blockfish::Suggestion) {
        let mut insert = true;
        for sugg in self.suggestions.iter_mut() {
            if sugg.update_from(&new_sugg) {
                insert = false;
                break;
            }
        }
        if insert {
            self.suggestions.push(new_sugg.into());
        }
        self.suggestions.sort_by_key(Suggestion::rating);
        self.select(0, 0);
    }

    fn finished(&self) -> bool {
        self.ai.is_none()
    }

    /// Polls the Blockfish process, returning `true` if the suggestions were updated as a
    /// result. Suggestions are inserted into `trie` as well.
    fn poll(&mut self, mut trie: Option<&mut Trie>) -> bool {
        let mut ai = match self.ai.take() {
            Some(ai) => ai,
            None => return false,
        };

        const MAX_POLLS_PER_FRAME: usize = 100;
        let mut polls = 0;
        while !ai.would_block() && polls < MAX_POLLS_PER_FRAME {
            match ai.next() {
                Some(sugg) => {
                    if let Some(trie) = trie.as_mut() {
                        let inputs = &sugg.inputs[..sugg.inputs.len() - 1];
                        trie.insert(inputs, sugg.score);
                    }
                    self.insert(sugg);
                    polls += 1;
                }
                None => {
                    let time = std::time::Instant::now() - self.start_time;
                    let time = time.as_secs_f64();
                    if let Some((nodes, iters)) = ai.stats() {
                        self.stats = Some((time, nodes, iters));
                    }
                    return true;
                }
            }
        }

        self.ai = Some(ai);
        polls > 0
    }

    /// Selects a different suggestion, moving by `delta` in the list of suggestions or
    /// `step` in the sequence for the suggestion. I.e., to move to the next suggestion,
    /// `delta` should be `1`, and to move to the previous selection `delta` should be
    /// `-1`. To step forward or backwards, `step` should be `1` or `-1` respectively.
    fn select(&mut self, delta: isize, step: isize) {
        if self.suggestions.is_empty() {
            return;
        }

        let source = &self.source;
        let sel = self.selection.get_or_insert_with(|| Selection {
            idx: 0,
            pos: 0,
            preview: source.clone(),
        });

        let seqs = self.suggestions.len();
        sel.idx = (sel.idx + (seqs as isize + delta) as usize) % seqs;

        let sugg = &self.suggestions[sel.idx];
        let steps = sugg.steps();
        sel.pos = if delta == 0 {
            (sel.pos + (steps as isize + step) as usize) % steps
        } else {
            0
        };

        let preview = &mut sel.preview;
        preview.clone_from(&self.source);
        preview.freeze();
        sugg.run(preview, sel.pos);
    }

    /// Tries to modify `dst` to go to the first step of the current selected suggestion.
    /// Returns `true` if `dst` was modified, or `false` and leaves `dst` untouched if
    /// there is no valid suggestion.
    fn go_to_selection(&self, dst: &mut Stacker) -> bool {
        if !self.finished() {
            return false;
        }
        let sugg = match self.selection.as_ref() {
            Some(sel) => &self.suggestions[sel.idx],
            None => return false,
        };
        dst.clone_from(&self.source);
        sugg.run(dst, 0);
        true
    }
}

impl Suggestion {
    fn steps(&self) -> usize {
        self.0.depth() + 1
    }

    fn rating(&self) -> i64 {
        self.0.score
    }

    fn run(&self, stacker: &mut Stacker, pos: usize) {
        stacker.run(self.0.inputs_prefix(pos));
    }

    fn update_from(&mut self, src: &blockfish::Suggestion) -> bool {
        if src.inputs_prefix(0).eq(self.0.inputs_prefix(0)) {
            if src.score < self.0.score {
                self.0.clone_from(src);
            }
            true
        } else {
            false
        }
    }
}

impl From<blockfish::Suggestion> for Suggestion {
    fn from(src: blockfish::Suggestion) -> Self {
        Self(src)
    }
}

struct Trie {
    score_params: blockfish::ScoreParams,
    nodes: Vec<TrieNode>,
    linear: Vec<TrieNodeId>,
    hover: Option<usize>,
    scroll: i32,
}

struct TrieNode {
    key: Vec<blockfish::Input>,
    stacker: Stacker,
    depth: usize,
    eval: Option<blockfish::Eval>,
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
            score_params: blockfish::ScoreParams::default(),
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

    fn reroot(&mut self, mut stacker: Stacker, params: &blockfish::ScoreParams) {
        stacker.freeze();
        stacker.reset_piece();
        let root = TrieNode::new(vec![], stacker, std::usize::MAX);
        self.nodes.clear();
        self.nodes.push(root);
        self.linearize();
        self.score_params.clone_from(params);
    }

    fn set_hover(&mut self, hover: Option<usize>) {
        self.hover = hover;
    }

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
        self[node_id].eval = stacker.snapshot().as_ref().map(blockfish::static_eval);
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
                view.set_engine_overlay_score(eval, &self.score_params);
            } else {
                view.clear_engine_overlay();
            }
        }

        let scroll_bounds = view.tree_scroll_bounds();
        self.scroll = std::cmp::max(scroll_bounds.start, self.scroll);
        self.scroll = std::cmp::min(scroll_bounds.end, self.scroll);
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
