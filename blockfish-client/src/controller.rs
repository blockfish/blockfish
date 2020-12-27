use crate::{
    controls::{Action, Controls, EngineOp, GameOp},
    view::View,
};
use block_stacker::Stacker;
use blockfish::{Config as BFConfig, StackerExt as _};

/// Controls a view according to changes in a `Stacker` state, incoming
/// engine suggestions, and user actions.
pub struct Controller<'v> {
    view: Box<View<'v>>,
    controls: Controls,
    stacker: Stacker,
    progress: Progress,
    undo_list: Vec<(Stacker, Progress)>,
    ai_config: BFConfig,
    engine: Option<Engine>,
}

impl<'v> Controller<'v> {
    /// Constructs a new `Controller`.
    pub fn new(
        stacker: Stacker,
        view: Box<View<'v>>,
        controls: Controls,
        ai_config: BFConfig,
    ) -> Self {
        let mut ctl = Self {
            view,
            controls,
            ai_config,
            stacker,
            undo_list: Vec::with_capacity(100),
            progress: Progress::new(),
            engine: None,
        };
        ctl.view.set_controls(&ctl.controls);
        ctl.consult_engine();
        ctl.undo_save();
        ctl.update_view(true, true, true, true);
        ctl
    }

    pub fn controls(&self) -> &Controls {
        &self.controls
    }

    pub fn view(&self) -> &View<'v> {
        &self.view
    }

    pub fn view_mut(&mut self) -> &mut View<'v> {
        &mut self.view
    }

    /// Updates parts of the view according to what changed since the last update.
    fn update_view(
        &mut self,
        piece_changed: bool,
        matrix_changed: bool,
        queue_changed: bool,
        engine_changed: bool,
    ) {
        let stacker = &self.stacker;
        let view = self.view.as_mut();

        if piece_changed {
            match stacker.current_piece() {
                Some((ty, i, j, r, g)) => view.set_piece(ty, i, j, r, g),
                None => view.clear_piece(),
            }
        }
        if matrix_changed {
            view.set_matrix(stacker.matrix());
        }
        if queue_changed {
            view.set_queue(stacker.next().to_vec(), stacker.held());
        }

        if engine_changed {
            if let Some(eng) = self.engine.as_ref() {
                eng.update_view(view);
            } else {
                view.set_engine_disabled();
            }
        }

        view.set_stats(
            self.progress.pieces,
            self.progress.lines,
            self.progress.color_clears,
            self.progress.downstack,
            stacker.config().garbage.total_lines,
        );
    }

    /// Ends the previous engine process and starts an analysis of the current stacker
    /// state.
    fn consult_engine(&mut self) {
        self.engine = Some(Engine::new(self.ai_config.clone(), self.stacker.clone()));
    }

    /// Polls the engine process for any updates to its progress.
    pub fn poll_engine(&mut self) {
        let e_chg = if let Some(eng) = self.engine.as_mut() {
            eng.poll()
        } else {
            false
        };
        if e_chg {
            self.update_view(false, false, false, true);
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
        }
    }

    /// Handles a game input, updating both the stacker state and view accordingly.
    fn handle_game_op(&mut self, op: GameOp) {
        let (p_chg, m_chg, q_chg) = match op {
            GameOp::MoveLeft | GameOp::MoveRight => {
                let dx = if op == GameOp::MoveLeft { -1 } else { 1 };
                (self.stacker.move_horizontal(dx), false, false)
            }
            GameOp::RotateCCW | GameOp::RotateCW => {
                let dr = if op == GameOp::RotateCCW { -1 } else { 1 };
                (self.stacker.rotate(dr), false, false)
            }
            GameOp::SonicDrop => (self.stacker.sonic_drop(), false, false),
            GameOp::Hold => {
                let chg = self.stacker.hold();
                (chg, false, chg)
            }
            GameOp::HardDrop => {
                self.undo_save();
                let (lc, ds) = self.stacker.hard_drop();
                let cc = self.stacker.is_matrix_colorless();
                self.progress.incr(lc, ds, cc);
                (true, true, true)
            }
            GameOp::Reset => {
                let ruleset = self.stacker.ruleset().clone();
                let mut cfg = self.stacker.config();
                cfg.prng_seed = None; // don't use the same RNG seed
                self.stacker = Stacker::new(ruleset, cfg);
                self.progress = Progress::new();
                self.undo_list.clear();
                self.undo_save();
                (true, true, true)
            }
            GameOp::Undo => {
                self.undo_restore();
                if self.undo_list.is_empty() {
                    self.undo_save();
                }
                (true, true, true)
            }
        };

        // if the matrix changed, we need to reconsult the engine
        let e_chg = if m_chg && self.engine.is_some() {
            self.consult_engine();
            true
        } else {
            false
        };

        // TODO: re-request piece rating if piece changed

        self.update_view(p_chg, m_chg, q_chg, e_chg);
    }

    /// Handles an engine related user action.
    fn handle_engine_op(&mut self, op: EngineOp) {
        if let Some(eng) = self.engine.take() {
            self.handle_engine_op_enabled(op, eng);
        } else {
            match op {
                EngineOp::Toggle => {
                    self.consult_engine();
                    self.update_view(false, false, false, true);
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
                self.engine = None;
                self.update_view(false, false, false, true);
            }
            EngineOp::Next | EngineOp::Prev => {
                let delta = if op == EngineOp::Prev { -1 } else { 1 };
                eng.select(delta, 0);
                self.engine = Some(eng);
                self.update_view(false, false, false, true);
            }
            EngineOp::Goto => {
                // TODO: maybe this should be a GameOp? since it exclusively affects .stacker
                //       and not .engine
                let chg = eng.go_to_selection(&mut self.stacker);
                self.engine = Some(eng);
                self.update_view(chg, chg, chg, false);
            }
            EngineOp::StepForward | EngineOp::StepBackward => {
                let step = if op == EngineOp::StepBackward { -1 } else { 1 };
                eng.select(0, step);
                self.engine = Some(eng);
                self.update_view(false, false, false, true);
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
    /// parameters `config`, starting at the stacker state `source`.
    fn new(config: BFConfig, mut source: Stacker) -> Self {
        source.reset_piece();
        let params = format!("{}", config);
        Self {
            start_time: std::time::Instant::now(),
            ai: match source.snapshot() {
                Some(ss) => Some(blockfish::AI::new(config).analyze(ss)),
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
    fn update_view(&self, view: &mut View) {
        view.set_engine_status(&self.params, self.stats);

        // TODO: partial updates for the engine matrix
        view.clear_engine_piece();
        view.clear_engine_suggestion();

        if let Some(sel) = self.selection.as_ref() {
            view.set_engine_matrix(sel.preview.matrix());
            if let Some((ty, _, j, r, i)) = sel.preview.current_piece() {
                view.set_engine_piece(ty, i, j, r);
            }
            if self.finished() {
                let sugg = &self.suggestions[sel.idx];
                let seq = (sel.idx, self.suggestions.len());
                let pos = (sel.pos, sugg.steps());
                let rating = sugg.rating();
                view.set_engine_overlay(seq, pos, rating);

                let is_last_step = pos.0 == pos.1 - 1;
                if is_last_step {
                    view.clear_engine_piece();
                }
            }
        } else {
            view.set_engine_matrix(self.source.matrix().map(|(ij, _)| (ij, 'H')));
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
    /// result.
    fn poll(&mut self) -> bool {
        let mut ai = match self.ai.take() {
            Some(ai) => ai,
            None => return false,
        };
        let mut did_change = false;
        while !ai.would_block() {
            match ai.next() {
                Some(sugg) => {
                    self.insert(sugg);
                    did_change = true;
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
        did_change
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
            self.0.clone_from(src);
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
