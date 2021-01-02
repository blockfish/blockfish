use crate::controls::{self, Action, GameOp, Handling};
use std::time::{Duration, Instant};

pub struct Timer {
    latest: Instant,
    autoplay: DAR<()>,
    autoshift: DAR<GameOp>,
}

impl Timer {
    pub fn new(handling: Handling) -> Self {
        Self {
            latest: Instant::now(),
            autoplay: DAR::new(handling.autoplay_delay(), handling.autoplay_repeat()),
            autoshift: DAR::new(handling.autoshift_delay(), handling.autoshift_repeat()),
        }
    }

    pub fn update(&mut self) {
        self.latest = Instant::now();
    }

    pub fn poll(&mut self) -> Option<Action> {
        if self.autoplay.trigger(self.latest).is_some() {
            return Some(Action::Engine(controls::EngineOp::AutoPlay));
        }
        if let Some(&op) = self.autoshift.trigger(self.latest) {
            return Some(Action::Game(op));
        }
        None
    }

    pub fn begin_autoplay(&mut self) -> bool {
        self.autoplay.start(self.latest, ())
    }

    pub fn end_autoplay(&mut self) {
        self.autoplay.stop(&());
    }

    pub fn begin_autoshift(&mut self, op: GameOp) -> bool {
        assert!(op.can_das());
        self.autoshift.start(self.latest, op)
    }

    pub fn end_autoshift(&mut self, op: GameOp) {
        self.autoshift.stop(&op);
    }
}

// Delayed auto-repeat timer.
#[derive(Clone, Debug)]
struct DAR<A> {
    delay: Duration,
    repeat: Duration,
    load: Option<(A, Instant)>,
}

impl<A: Eq> DAR<A> {
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

    fn start(&mut self, t: Instant, a: A) -> bool {
        if !self.is_loaded(&a) {
            self.load = Some((a, t + self.delay));
            true
        } else {
            false
        }
    }

    fn stop(&mut self, a: &A) {
        if self.is_loaded(a) {
            self.load = None;
        }
    }

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
