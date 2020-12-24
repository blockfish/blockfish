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
    undo_list: Vec<(Stacker, Progress)>,
    progress: Progress,
    ai_config: BFConfig,
    engine: Option<Box<Engine>>,
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
        let eng = Engine::new(self.ai_config.clone(), self.stacker.clone());
        self.engine = Some(Box::new(eng));
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
        self.undo_list.push((self.stacker.clone(), self.progress.clone()));
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
        if let Some(eng) = self.engine.as_mut() {
            let mut stacker_chg = false;
            match op {
                EngineOp::Toggle => {
                    self.engine = None;
                }
                EngineOp::Next | EngineOp::Prev => {
                    let delta = if op == EngineOp::Prev { -1 } else { 1 };
                    eng.select(delta);
                }
                EngineOp::Goto => {
                    if let Some(new_stacker) = eng.go_to_selection() {
                        self.stacker = new_stacker;
                        stacker_chg = true;
                    }
                }
                _ => {
                    log::warn!("unimplemented: {:?}", op);
                }
            }
            self.update_view(stacker_chg, stacker_chg, stacker_chg, true);
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
    ai: blockfish::AI,
    source: Stacker,
    source_frozen: Stacker,
    suggestions: Vec<blockfish::Suggestion>,
    selected: Option<(usize, Stacker)>,
    done: bool,
}

// TODO: make this dynamic?
const MAX_SUGGESTIONS: usize = 5;

impl Engine {
    /// Constructs a new `Engine` by starting a new Blockfish process with configuration
    /// parameters `config`, starting at the stacker state `source`.
    fn new(config: BFConfig, source: Stacker) -> Self {
        let mut ai = blockfish::AI::new(config);
        if let Some(snapshot) = source.snapshot() {
            ai.start(snapshot);
        }
        let mut source_frozen = source.clone();
        source_frozen.freeze();
        Self {
            ai,
            source,
            source_frozen,
            suggestions: vec![],
            selected: None,
            done: false,
        }
    }

    /// Updates `view` to show the current state of the engine process, and the selected
    /// suggestion.
    fn update_view(&self, view: &mut View) {
        view.set_engine_status(&format!("{}", self.ai.config()), None);

        // TODO: partial updates for the engine matrix
        view.clear_engine_piece();
        view.clear_engine_suggestion();

        if let Some((num, stacker)) = self.selected.as_ref() {
            view.set_engine_matrix(stacker.matrix());
            if let Some((ty, _, j, r, i)) = stacker.current_piece() {
                view.set_engine_piece(ty, i, j, r);
            }
            if self.done {
                let num = *num;
                let seq = (0, 1);
                let score = self.suggestions[num].score;
                view.set_engine_suggestion(num, seq, score);
            }
        } else {
            view.set_engine_matrix(self.source_frozen.matrix());
        }
    }

    /// Adds `sugg` to the list of suggestions, or replaces an existing suggestion, then
    /// re-sorts the suggestions.
    fn insert(&mut self, sugg: blockfish::Suggestion) {
        let mut insert = true;
        for prev_sugg in self.suggestions.iter_mut() {
            if sugg.inputs == prev_sugg.inputs {
                prev_sugg.score = sugg.score;
                insert = false;
                break;
            }
        }
        if insert {
            self.suggestions.push(sugg);
        }
        self.suggestions.sort_by_key(|sugg| sugg.score);
        if self.suggestions.len() > MAX_SUGGESTIONS {
            self.suggestions.pop();
        }
        self.select(0);
    }

    /// Polls the Blockfish process, returning `true` if the suggestions were updated as a
    /// result.
    fn poll(&mut self) -> bool {
        let mut did_change = false;
        while !self.done {
            use blockfish::AIPoll;
            match self.ai.poll() {
                AIPoll::Pending => break,
                AIPoll::Done => {
                    did_change = true;
                    self.done = true;
                }
                AIPoll::Suggest(sugg) => {
                    did_change = true;
                    self.insert(sugg);
                }
            }
        }
        did_change
    }

    /// Selects a different suggestion, moving by `delta` in the list of
    /// suggestions. I.e., to move to the next suggestion, `delta` should be `1`, and to
    /// move to the previous selection `delta` should be `-1`.
    fn select(&mut self, delta: isize) {
        let idx = match self.selected.as_ref() {
            Some((idx, _)) => *idx,
            None => 0,
        };
        let len = self.suggestions.len();
        let idx = (idx + (len as isize + delta) as usize) % len;
        let stacker = self.stacker_from_suggestion(true, idx);
        self.selected = Some((idx, stacker));
    }

    /// Returns the stacker state obtained by applying the selected suggestion. Returns
    /// `None` if there are no valid suggestions to go to.
    fn go_to_selection(&self) -> Option<Stacker> {
        if !self.done {
            None
        } else {
            if let Some((idx, _)) = self.selected {
                Some(self.stacker_from_suggestion(false, idx))
            } else {
                None
            }
        }
    }

    /// Computers the stacker state derived from the `idx`'th best suggestion. If `freeze`
    /// is `true`, then `freeze()`s the stacker state before applying the suggestion.
    ///
    /// The engine matrix display should show frozen states, and the main matrix should
    /// show non-frozen states.
    fn stacker_from_suggestion(&self, freeze: bool, idx: usize) -> Stacker {
        let mut stacker = if freeze {
            self.source_frozen.clone()
        } else {
            self.source.clone()
        };
        stacker.reset_piece();
        stacker.run(self.suggestions[idx].inputs.iter().cloned());
        stacker
    }
}
