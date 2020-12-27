use std::collections::HashMap;

pub use sdl2::keyboard::{Keycode, Mod};

pub const DEFAULT_BINDINGS: &[(Action, KeyStroke)] = {
    use Action::*;
    use EngineOp::*;
    use GameOp::{Undo, *};
    use KeyStroke::*;
    use Keycode::*;
    &[
        (Game(MoveLeft), Only(Left)),
        (Game(MoveRight), Only(Right)),
        (Game(RotateCCW), Only(Z)),
        (Game(RotateCW), Only(X)),
        (Game(Hold), Shift),
        (Game(SonicDrop), Only(Down)),
        (Game(HardDrop), Only(Space)),
        (Game(Reset), Control(R)),
        (Game(Undo), Control(Z)),
        (Engine(Toggle), Control(E)),
        (Engine(Next), Only(Tab)),
        (Engine(Prev), Control(Tab)),
        (Engine(StepForward), Control(F)),
        (Engine(StepBackward), Control(B)),
        (Engine(Goto), Only(Return)),
    ]
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Action {
    Game(GameOp),
    Engine(EngineOp),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u8)]
#[allow(dead_code)]
pub enum GameOp {
    MoveLeft,
    MoveRight,
    RotateCCW,
    RotateCW,
    Hold,
    SonicDrop,
    HardDrop,
    Reset,
    Undo,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(u8)]
#[allow(dead_code)]
pub enum EngineOp {
    Toggle,
    Next,
    Prev,
    StepForward,
    StepBackward,
    Goto,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum KeyStroke {
    Only(Keycode),
    Control(Keycode),
    Shift,
}

impl std::fmt::Display for KeyStroke {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let fmt_keycode = |f: &mut std::fmt::Formatter, kc: Keycode| {
            let string = format!("{:?}", kc);
            f.write_str(&string.to_lowercase())
        };
        match *self {
            KeyStroke::Only(kc) => fmt_keycode(f, kc),
            KeyStroke::Control(kc) => {
                f.write_str("C-")?;
                fmt_keycode(f, kc)
            }
            KeyStroke::Shift => f.write_str("shift"),
        }
    }
}

/// Represents a controls configuration, which can be used to look up which `Action` is
/// triggered by a given key press.
pub struct Controls {
    from_keycode: HashMap<(Keycode, bool), Action>,
    from_action: HashMap<Action, KeyStroke>,
}

impl Controls {
    pub fn new<I>(bindings: I) -> Self
    where
        I: IntoIterator<Item = (Action, KeyStroke)>,
    {
        let mut from_keycode = HashMap::new();
        let mut from_action = HashMap::new();
        for (action, ks) in bindings {
            from_action.insert(action, ks);
            match ks {
                KeyStroke::Only(kc) => {
                    from_keycode.insert((kc, false), action);
                }
                KeyStroke::Control(kc) => {
                    from_keycode.insert((kc, true), action);
                }
                KeyStroke::Shift => {
                    from_keycode.insert((Keycode::LShift, false), action);
                    from_keycode.insert((Keycode::RShift, false), action);
                }
            }
        }
        Self {
            from_keycode,
            from_action,
        }
    }

    /// Returns the key-stroke associated with the given action, if bound.
    pub fn key_stroke(&self, action: Action) -> Option<KeyStroke> {
        self.from_action.get(&action).cloned()
    }

    /// Parses the given keycode + keymod sequence into an `Action`, if that sequence does
    /// anything accoring to the controls configuration.
    pub fn parse(&self, keycode: Keycode, keymod: Mod) -> Option<Action> {
        let control = keymod.contains(Mod::LCTRLMOD) || keymod.contains(Mod::RCTRLMOD);
        self.from_keycode.get(&(keycode, control)).cloned()
    }
}

impl Default for Controls {
    fn default() -> Self {
        Self::new(DEFAULT_BINDINGS.iter().cloned())
    }
}
