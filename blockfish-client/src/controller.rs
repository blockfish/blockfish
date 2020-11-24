use crate::{ruleset::Ruleset, stacker::Stacker, view::View};

/// Holds both the view state and the game state, and bridges the gap between them by
/// handling input events and updating the states accordingly.
pub struct Controller<'v> {
    view: View<'v>,
    stacker: Stacker,
    stats: Stats,
}

/// Represents a kind of user input.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
#[repr(u8)]
pub enum Input {
    HardDrop,
    SonicDrop,
    Left,
    Right,
    CW,
    CCW,
    Hold,
}

impl<'v> Controller<'v> {
    /// Constructs a new controller with the given ruleset.
    pub fn new(rules: Ruleset, view: View<'v>) -> Self {
        let stacker = Stacker::new(rules);
        let stats = Stats::new();
        let mut ctl = Controller {
            view,
            stacker,
            stats,
        };
        ctl.update_view();
        ctl
    }

    /// Returns the view being controlled by this controller.
    pub fn view(&self) -> &View {
        &self.view
    }

    // TODO: don't do this; do updates incrementally
    fn update_view(&mut self) {
        self.view.set_cells(self.stacker.matrix());
        if let Some((typ, i, j, r, g)) = self.stacker.current_piece() {
            self.view.set_piece(typ, i, j, r, g);
        }
        //self.view.set_suggested('L', 2, -1, 1);
        self.view.clear_suggested();
        // TODO: if no piece, then game over (probably)
        if let Some(typ) = self.stacker.held() {
            self.view.set_hold(typ);
        } else {
            self.view.clear_hold();
        }
        self.view.set_next(self.stacker.next().iter().cloned());

        let stats = self.stats.display_iter();
        self.view.set_hud_labels(stats);
    }

    /// Responds to a user input event.
    pub fn on_input(&mut self, inp: Input) {
        match inp {
            Input::HardDrop => {
                let (n, ds) = self.stacker.hard_drop();
                self.stats.piece(n, ds);
                self.update_view();
            }
            Input::SonicDrop => {
                if self.stacker.sonic_drop() {
                    self.update_view();
                }
            }
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
                    self.update_view();
                }
            }
        }
    }
}

pub struct Stats {
    pub pieces: usize,
    pub cleared: usize,
    pub downstack: usize,
}

impl Stats {
    pub fn new() -> Self {
        Stats {
            pieces: 0,
            cleared: 0,
            downstack: 0,
        }
    }

    pub fn dpp(&self) -> Option<f32> {
        if self.pieces == 0 {
            None
        } else {
            Some((self.downstack as f32) / (self.pieces as f32))
        }
    }

    pub fn pace(&self) -> Option<usize> {
        if self.downstack == 0 {
            None
        } else {
            Some(self.pieces * 100 / self.downstack)
        }
    }

    fn piece(&mut self, cleared: usize, downstack: usize) {
        self.pieces += 1;
        self.cleared += cleared;
        self.downstack += downstack;
    }

    fn display_iter(&self) -> StatsDisplayIter {
        StatsDisplayIter(self, 0)
    }
}

const STATS_DISPLAY_LINES: usize = 5;

struct StatsDisplayIter<'a>(&'a Stats, usize);
struct StatsDisplay<'a>(&'a Stats, usize);

impl<'a> Iterator for StatsDisplayIter<'a> {
    type Item = StatsDisplay<'a>;

    fn next(&mut self) -> Option<StatsDisplay<'a>> {
        let idx = self.1;
        if idx < STATS_DISPLAY_LINES {
            self.1 += 1;
            Some(StatsDisplay(self.0, idx))
        } else {
            None
        }
    }
}

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
