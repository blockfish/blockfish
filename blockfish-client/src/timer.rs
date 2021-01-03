use crate::controls::{self, Action, GameOp, Handling};
use std::time::{Duration, Instant};

pub struct Timer {
    latest: Instant,
    autoplay: DAR<()>,
    autoshift: DAR<GameOp>,
}

impl Timer {
    /// Constructs a new timer with given handling settings.
    pub fn new(handling: &Handling) -> Self {
        Self {
            latest: Instant::now(),
            autoplay: DAR::new(handling.autoplay_delay(), handling.autoplay_repeat()),
            autoshift: DAR::new(handling.autoshift_delay(), handling.autoshift_repeat()),
        }
    }

    /// Brings the timer up-to-date with the current time (`Instant::now()`).
    pub fn update(&mut self) {
        self.latest = Instant::now();
    }

    /// Polls the timer to see if there are any new actions to perform since the last
    /// call to `update()`.
    pub fn poll(&mut self) -> Option<Action> {
        if self.autoplay.trigger(self.latest).is_some() {
            return Some(Action::Engine(controls::EngineOp::AutoPlay));
        }
        if let Some(&op) = self.autoshift.trigger(self.latest) {
            return Some(Action::Game(op));
        }
        None
    }

    /// Begins autoplaying, returning `true` if the autoplay delay just began as a result,
    /// or `false` if the timer was already started.
    pub fn begin_autoplay(&mut self) -> bool {
        self.autoplay.start(self.latest, ())
    }

    /// Stops autoplaying (usually triggered by letting go of the autoplay key).
    pub fn end_autoplay(&mut self) {
        self.autoplay.stop(&());
    }

    /// Begins DASing the given `GameOp`, returning `true` if the delay just began as a
    /// result, or `false` if the timer for the op already started. If this function
    /// returns `true` then then `op` should still be performed this frame, if `false`
    /// then it should be supressed.
    pub fn begin_autoshift(&mut self, op: GameOp) -> bool {
        assert!(op.can_das());
        self.autoshift.start(self.latest, op)
    }

    /// Stops DASing the given `GameOp`.
    pub fn end_autoshift(&mut self, op: GameOp) {
        self.autoshift.stop(&op);
    }
}

/// Delayed-auto-repeat timer abstraction. The type `A` is type of actions that get
/// repeatedly triggered.
#[derive(Clone, Debug)]
struct DAR<A> {
    delay: Duration,
    repeat: Duration,
    load: Option<(A, Instant)>,
}

impl<A: Eq> DAR<A> {
    /// Constructs a new delayed-auto-repeat timer. The duration `delay` is the length of
    /// time between `start()`ing the timer and the first trigger. The duration `repeat`
    /// is the length of time between subsequent triggers if the timer is never
    /// `stop()`ed.
    fn new(delay: Duration, repeat: Duration) -> Self {
        Self {
            delay,
            repeat,
            load: None,
        }
    }

    fn is_loaded(&self, a: &A) -> bool {
        if let Some((a0, _)) = self.load.as_ref() {
            a0 == a
        } else {
            false
        }
    }

    /// Attempts to start the timer, where the current time is instant `t`, and `a` is the
    /// action to start triggering after a delay. Returns `true` unless the timer is
    /// already loaded with the same action.
    fn start(&mut self, t: Instant, a: A) -> bool {
        if !self.is_loaded(&a) {
            self.load = Some((a, t + self.delay));
            true
        } else {
            false
        }
    }

    /// Stops the timer as long as `a` is the same action that this timer is loaded with.
    fn stop(&mut self, a: &A) {
        if self.is_loaded(a) {
            self.load = None;
        }
    }

    /// Polls the timer for any triggers of the action, where the current time is instant
    /// `t`. Returns `None` when the timer is no longer triggered. May return `Some`
    /// multiple times before returning `None` in case auto-repeat is triggered more than
    /// once this frame.
    fn trigger(&mut self, t: Instant) -> Option<&A> {
        if let Some((a, t0)) = self.load.as_mut() {
            if t >= *t0 {
                *t0 += self.repeat;
                return Some(a);
            }
        }
        None
    }
}
