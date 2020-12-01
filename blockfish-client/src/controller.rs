use crate::view::View;
use block_stacker::{PieceType, Stacker};
use blockfish::{Input, StackerExt as _, Suggestion};

/// Holds both the view state and the game state, and bridges the gap between them by
/// handling input events and updating the states accordingly.
pub struct Controller<'v> {
    view: View<'v>,
    stacker: Stacker,
    stats: Stats,
    suggestions: Suggestions,
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
    pub fn new(stacker: Stacker, view: View<'v>) -> Self {
        let mut ctl = Controller {
            view,
            stacker,
            stats: Stats::new(),
            suggestions: Suggestions::new(),
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
        self.view.set_hud_labels(
            self.stats
                .hud()
                .chain(linebreak())
                .chain(self.suggestions.hud()),
        );
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
                    self.update_view();
                }
            }
            Input::Right => {
                if self.stacker.move_horizontal(1) {
                    self.update_view();
                }
            }
            Input::CCW => {
                if self.stacker.rotate(-1) {
                    self.update_view();
                }
            }
            Input::CW => {
                if self.stacker.rotate(1) {
                    self.update_view();
                }
            }
            Input::Hold => {
                if self.stacker.hold() {
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
            if eng.poll_done(&mut self.suggestions) {
                self.engine = None;
                log::trace!("engine process finished");
            }
            self.update_view();
        }
    }

    /// Begin running the engine in the background.
    fn consult_engine(&mut self) {
        self.suggestions.clear();
        self.engine = ai(&self.stacker);
        log::trace!("consulting engine...");
        self.poll_engine();
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Stats

/// Keeps track of statistics for the players' placements.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Stats {
    pub pieces: usize,
    pub cleared: usize,
    pub downstack: usize,
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
    pub fn pace(&self) -> Option<usize> {
        if self.downstack == 0 {
            None
        } else {
            Some(self.pieces * 100 / self.downstack)
        }
    }

    /// Advance the statistics after a piece has been placed which cleared `n` total lines
    /// and `g` lines of garbage.
    pub fn piece(&mut self, n: usize, g: usize) {
        self.pieces += 1;
        self.cleared += n;
        self.downstack += g;
    }

    /// Returns the HUD labels to display for these statistics.
    fn hud<'a>(&'a self) -> impl Iterator<Item = HUD<'a>> {
        (0..STATS_LINES).map(move |idx| HUD::Stats(StatsDisplay(self, idx)))
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

    /// Returns `true` if no suggestions have been found yet.
    fn no_suggestions(&self) -> bool {
        self.infos.len() == 0
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

    /// Returns the HUD labels to display for these suggestions.
    fn hud<'a>(&'a self) -> impl Iterator<Item = HUD<'a>> {
        let len = if self.no_suggestions() {
            NO_SUGGESTIONS_LINES
        } else {
            SOME_SUGGESTIONS_LINES
        };
        (0..len).map(move |idx| HUD::Suggestions(SuggestionsDisplay(self, idx)))
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
    /// The incoming stream of suggestions.
    stream: blockfish::SuggestionsIter,
    /// The game state at the time of suggestions being generated.
    stacker: Box<Stacker>,
}

/// Run the AI in the background from the initial state given by `stacker`.
/// Returns `None` if a snapshot could not be produced by the state.
// TODO: look into deriving a Stacker from a Snapshot?
fn ai(stacker: &Stacker) -> Option<EngineProcess> {
    let snapshot = stacker.snapshot()?;
    let cfg = blockfish::Config::default();
    Some(EngineProcess {
        stream: blockfish::ai(cfg, snapshot),
        stacker: Box::new(stacker.clone()),
    })
}

impl EngineProcess {
    /// Polls the engine for completion, returning `true` if it is done. In the meantime,
    /// appends any suggestions received to `sink`.
    fn poll_done(&mut self, sink: &mut impl Extend<SuggestionInfo>) -> bool {
        loop {
            match self.stream.try_recv() {
                Ok(sugg) => {
                    let info = self.suggestion_info(sugg);
                    sink.extend(std::iter::once(info));
                }
                Err(status) => {
                    return status == std::sync::mpsc::TryRecvError::Disconnected;
                }
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

//////////////////////////////////////////////////////////////////////////////////////////
// HUD text

// NOTE TO READER: below is kind of an awful micro optimization in order to create an
// `Iterator` of `HUD`s which can be individually formatted with `"{}"` in order to
// produce the text of each label, entirely without generating a single intermediate
// `String` or `Vec` on the heap. `view::Label` then uses double-buffers to basically
// prevent strings from ever being allocated except for on initialization of the view.
//
// In hindsight you should just allocate instead because it's not a big deal and it would
// be a lot cleaner. But at the moment this does work so we're keeping it for the time
// being.

enum HUD<'a> {
    Stats(StatsDisplay<'a>),
    Suggestions(SuggestionsDisplay<'a>),
    Blank,
}

fn linebreak<'a>() -> impl Iterator<Item = HUD<'a>> {
    std::iter::once(HUD::Blank)
}

impl<'a> std::fmt::Display for HUD<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            HUD::Stats(x) => write!(f, "{}", x),
            HUD::Suggestions(x) => write!(f, "{}", x),
            HUD::Blank => write!(f, ""),
        }
    }
}

const STATS_LINES: usize = 5;
const NO_SUGGESTIONS_LINES: usize = 1;
const SOME_SUGGESTIONS_LINES: usize = 2;

struct StatsDisplay<'a>(&'a Stats, usize);
impl<'a> std::fmt::Display for StatsDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let StatsDisplay(stats, idx) = self;
        match idx {
            0 => write!(f, "{}", Plural(stats.pieces, "piece")),
            1 => write!(f, "{}", Plural(stats.cleared, "line")),
            2 => write!(f, "{} garbage", stats.downstack),
            3 => write!(f, "{} DPP", MaybeF32(stats.dpp())),
            4 => write!(f, "{} pace", MaybeInt(stats.pace())),
            _ => unreachable!(),
        }
    }
}

struct SuggestionsDisplay<'a>(&'a Suggestions, usize);
impl<'a> std::fmt::Display for SuggestionsDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let SuggestionsDisplay(sug, idx) = self;
        let infos = &sug.infos;
        let sel = sug.selected.unwrap_or(0);
        match idx {
            0 if sug.no_suggestions() => write!(f, "Engine: running..."),
            0 => write!(f, "Engine: [{}/{}]", sel + 1, infos.len()),
            1 => write!(f, "Score: {}", infos[sel].score),
            _ => unreachable!(),
        }
    }
}

struct Plural<'a>(usize, &'a str);
impl<'a> std::fmt::Display for Plural<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.0 == 1 {
            write!(f, "1 {}", self.1)
        } else {
            write!(f, "{} {}s", self.0, self.1)
        }
    }
}

struct MaybeF32(Option<f32>);
impl std::fmt::Display for MaybeF32 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            Some(x) => write!(f, "{:.3}", x),
            None => write!(f, "?"),
        }
    }
}

struct MaybeInt(Option<usize>);
impl std::fmt::Display for MaybeInt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            Some(x) => write!(f, "{}", x),
            None => write!(f, "?"),
        }
    }
}
