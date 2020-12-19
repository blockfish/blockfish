use crate::view::{self, View};
use block_stacker::{PieceType, Stacker};
use blockfish::{Config, Input, StackerExt as _, Suggestion, AI};
use std::collections::HashMap;

/// Holds both the view state and the game state, and bridges the gap between them by
/// handling input events and updating the states accordingly.
pub struct Controller<'v> {
    view: View<'v>,
    stacker: Stacker,
    stats: Stats,
    suggestions: Suggestions,
    evals: Evals,
    ai_config: Config,
    engine: Option<EngineProcess>,
}

/// Represents a kind of user input.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum UserInput {
    NextSuggestion,
    PrevSuggestion,
}

impl<'v> Controller<'v> {
    /// Constructs a new controller with the game state `stacker` and view `view`.
    pub fn new(stacker: Stacker, ai_config: Config, view: View<'v>) -> Self {
        let mut ctl = Controller {
            view,
            stacker,
            stats: Stats::new(),
            suggestions: Suggestions::new(),
            evals: Evals::new(),
            ai_config,
            engine: None,
        };
        ctl.consult_engine();
        ctl.update_view();
        ctl
    }

    /// Returns the view being controlled by this controller.
    pub fn view(&self) -> &View {
        &self.view
    }

    // TODO: don't do this; do updates incrementally
    fn update_view(&mut self) {
        log::trace!("view updated");
        // matrix
        self.view.set_cells(self.stacker.matrix());
        // current piece
        if let Some((typ, i, j, r, g)) = self.stacker.current_piece() {
            self.view.set_piece(typ, i, j, r, g);
        }
        // suggested piece
        if let Some((typ, i, j, r)) = self.suggestions.selected_piece() {
            self.view.set_suggested(typ, i, j, r);
        } else {
            self.view.clear_suggested();
        }
        // hold
        if let Some(typ) = self.stacker.held() {
            self.view.set_hold(typ);
        } else {
            self.view.clear_hold();
        }
        // next
        self.view.set_next(self.stacker.next().iter().cloned());
        // hud
        self.view.set_hud_labels(view::LabelInfo {
            pace: view::PaceLabelInfo {
                pieces: self.stats.pieces,
                downstack: self.stats.downstack,
                cleared: self.stats.cleared,
                dpp: self.stats.dpp(),
                pace: self.stats.pace(),
            },
            engine: match self.suggestions.selected {
                // TODO: "failed" state?
                None => view::EngineLabelInfo::Thinking,
                Some(index) => view::EngineLabelInfo::Suggesting {
                    index,
                    total: self.suggestions.infos.len(),
                    score: self.suggestions.infos[index].score,
                    base_eval: self.evals.base_eval(),
                    piece_eval: self.evals.piece_eval(),
                },
            },
        });
    }

    fn update_evals(&mut self) {
        self.evals.set(&self.stacker);
        if let Some(eng) = self.engine.as_ref() {
            self.evals.poll(&eng.ai);
        }
    }

    /// Responds to a user input event.
    pub fn on_user_input(&mut self, inp: UserInput) {
        match inp {
            UserInput::NextSuggestion => {
                if self.suggestions.toggle(1) {
                    self.update_view();
                }
            }
            UserInput::PrevSuggestion => {
                if self.suggestions.toggle(-1) {
                    self.update_view();
                }
            }
        }
    }

    /// Responds to a game input event.
    pub fn on_game_input(&mut self, inp: Input) {
        match inp {
            Input::Left => {
                if self.stacker.move_horizontal(-1) {
                    self.update_evals();
                    self.update_view();
                }
            }
            Input::Right => {
                if self.stacker.move_horizontal(1) {
                    self.update_evals();
                    self.update_view();
                }
            }
            Input::CCW => {
                if self.stacker.rotate(-1) {
                    self.update_evals();
                    self.update_view();
                }
            }
            Input::CW => {
                if self.stacker.rotate(1) {
                    self.update_evals();
                    self.update_view();
                }
            }
            Input::Hold => {
                if self.stacker.hold() {
                    self.update_evals();
                    // TODO: ? engine result should be the same after you press hold
                    // self.consult_engine();
                    self.update_view();
                }
            }
            Input::HD => {
                let (n, g) = self.stacker.hard_drop();
                self.stats.piece(n, g);
                self.consult_engine();
                self.update_view();
            }
            Input::SD => {
                if self.stacker.sonic_drop() {
                    self.update_view();
                }
            }
        }
    }

    /// Check if the engine has made any progress and potentially update the view if it
    /// has.
    pub fn poll_engine(&mut self) {
        if let Some(eng) = self.engine.as_mut() {
            match eng.poll(&mut self.suggestions) {
                Some(n) if n > 0 => self.update_view(),
                _ => {}
            }
        }
    }

    /// BEGIN running the engine in the background.
    fn consult_engine(&mut self) {
        self.suggestions.clear();
        self.evals.clear();
        self.engine = engine(&self.stacker, self.ai_config.clone());
        log::trace!("consulting engine...");
        self.poll_engine();
        self.update_evals();
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Stats

/// Keeps track of statistics for the players' placements.
struct Stats {
    pieces: u32,
    cleared: u32,
    downstack: u32,
}

impl Stats {
    /// Constructs an empty `Stats`.
    pub fn new() -> Self {
        Stats {
            pieces: 0,
            cleared: 0,
            downstack: 0,
        }
    }

    /// Advance the statistics after a piece has been placed which cleared `n` total lines
    /// and `g` lines of garbage.
    pub fn piece(&mut self, n: usize, g: usize) {
        self.pieces += 1;
        self.cleared += n as u32;
        self.downstack += g as u32;
    }

    /// Computes downstack-per-piece. Returns `None` if unavailable due to not enough
    /// pieces placed.
    pub fn dpp(&self) -> Option<f32> {
        if self.pieces == 0 {
            None
        } else {
            Some((self.downstack as f32) / (self.pieces as f32))
        }
    }

    /// Computes downstack "pace", the projected piece count clearing 100L of
    /// garbage. Returns `None` if unavailable due to not enough garbage lines cleared.
    pub fn pace(&self) -> Option<u32> {
        if self.downstack == 0 {
            None
        } else {
            Some(self.pieces * 100 / self.downstack)
        }
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Board eval

/// Keeps track of and handles computing static evaluations of board states.
struct Evals {
    base: Option<Box<Stacker>>,
    base_eval: Option<i64>,
    piece: Option<(PieceType, i16, i16, i32)>,
    piece_evals: HashMap<(PieceType, i16, i16, i32), i64>,
}

impl Evals {
    fn new() -> Self {
        Self {
            base: None,
            base_eval: None,
            piece: None,
            piece_evals: HashMap::with_capacity(256),
        }
    }

    fn base_eval(&self) -> Option<i64> {
        self.base_eval
    }

    fn piece_eval(&self) -> Option<i64> {
        self.piece.and_then(|p| self.piece_evals.get(&p).cloned())
    }

    /// Clears all of the evaluations calculated.
    fn clear(&mut self) {
        self.base = None;
        self.base_eval = None;
        self.piece = None;
        self.piece_evals.clear();
    }

    /// Sets the game state in `stacker` to be the initial state to base evaluations of off.
    fn set(&mut self, stacker: &Stacker) {
        self.base = Some(Box::new(stacker.clone()));
        self.piece = stacker
            .current_piece()
            .map(|(typ, _, j, r, i)| (typ, i, j, r)); // ghost coordinate
    }

    /// Polls `ai` to update any evaluation scores that need to be computed.
    fn poll(&mut self, ai: &blockfish::AI) {
        let base = match self.base.as_ref() {
            Some(stacker) => &*stacker,
            None => return,
        };

        if self.base_eval.is_none() {
            // TODO: make this concurrent somehow in case its time consuming to consult
            // the ai for static eval's, e.g. if the ai is in a separate process.
            self.base_eval = base.static_eval(ai);
        }

        if let Some(piece) = self.piece.clone() {
            if !self.piece_evals.contains_key(&piece) {
                let mut stacker = base.clone();
                stacker.hard_drop();
                if let Some(eval) = stacker.static_eval(ai) {
                    self.piece_evals.insert(piece, eval);
                }
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Suggestions

/// Maintains the current best suggestions found by the engine.
struct Suggestions {
    selected: Option<usize>,
    infos: Vec<SuggestionInfo>,
}

/// Represents a single suggestion by the engine.
// NOTE: the list of inputs is discarded rn because we don't have any use for it; the
// piece location is enough to display to the view. however it might be useful to put it
// back.
struct SuggestionInfo {
    score: i64,
    piece: (PieceType, i16, i16, i32),
}

impl Suggestions {
    /// Construct a new `Suggestions` that is initially empty
    fn new() -> Self {
        Self {
            infos: vec![],
            selected: None,
        }
    }

    /// Returns the info `(typ, row, col, rotation)` of the current suggestion being
    /// viewed, if any.
    fn selected_piece(&self) -> Option<(PieceType, i16, i16, i32)> {
        self.selected.map(|i| self.infos[i].piece)
    }

    /// Clears all of the suggestions.
    fn clear(&mut self) {
        self.infos.clear();
        self.selected = None;
    }

    /// Toggle the currently viewed suggestion, moving forwards or backwards in the list according
    /// to the sign of `dir`. Returns `true` if the selection was modified as a result.
    fn toggle(&mut self, dir: isize) -> bool {
        if let Some(sel) = self.selected.as_mut() {
            let len = self.infos.len() as isize;
            *sel = (((*sel) as isize + dir + len) % len) as usize;
            true
        } else {
            false
        }
    }
}

impl Extend<SuggestionInfo> for Suggestions {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = SuggestionInfo>,
    {
        for info in iter {
            let mut infos = self.infos.iter().enumerate();
            match infos.find(|(_, prev)| prev.piece == info.piece) {
                Some((idx, prev)) => {
                    log::trace!(
                        "adjusted #{} score: {} => {}",
                        idx + 1,
                        prev.score,
                        info.score
                    );
                    self.infos[idx] = info;
                }
                None => {
                    self.infos.push(info);
                }
            }
        }
        if !self.infos.is_empty() {
            self.infos.sort_by_key(|s| s.score);
            self.selected.get_or_insert(0);
        }
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Engine background process

/// Represents the state of an engine running in the background.
struct EngineProcess {
    /// The handle to the AI.
    ai: blockfish::AI,
    /// The game state at the time of suggestions being generated.
    stacker: Box<Stacker>,
}

/// Run the AI in the background from the initial state given by `stacker`.
/// Returns `None` if a snapshot could not be produced by the state.
// TODO: look into deriving a Stacker from a Snapshot?
fn engine(stacker: &Stacker, cfg: Config) -> Option<EngineProcess> {
    let snapshot = stacker.snapshot()?;
    let mut ai = AI::new(cfg);
    ai.start(snapshot);
    let stacker = Box::new(stacker.clone());
    Some(EngineProcess { ai, stacker })
}

impl EngineProcess {
    /// Polls the engine for completion. Returns the number of new suggestions found, or
    /// `None` if the AI process is finished.
    fn poll(&mut self, sink: &mut impl Extend<SuggestionInfo>) -> Option<usize> {
        use blockfish::AIPoll;
        let mut count = 0;
        loop {
            match self.ai.poll() {
                AIPoll::Suggest(sugg) => {
                    let info = self.suggestion_info(sugg);
                    sink.extend(std::iter::once(info));
                    count += 1;
                }
                AIPoll::Done if count == 0 => return None,
                AIPoll::Pending | AIPoll::Done => return Some(count),
            }
        }
    }

    /// Converts `sugg` into a `SuggestionInfo` by simulating the inputs on the internal
    /// game state.
    fn suggestion_info(&self, sugg: Suggestion) -> SuggestionInfo {
        let mut stacker = (*self.stacker).clone();
        stacker.run(sugg.inputs);
        // NOTE: we're using the ghost piece row, not the current row.
        let (typ, _, j, r, i) = stacker
            .current_piece()
            .expect("bug: suggestion leads to no piece");
        SuggestionInfo {
            score: sugg.score,
            piece: (typ, i, j, r),
        }
    }
}
