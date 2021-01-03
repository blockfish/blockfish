use sdl2::keyboard::{Keycode, Mod};
use serde::Deserialize;
use std::collections::HashMap;
use thiserror::Error;

const BINDING_TOGGLE_TREE: KeyStroke = KeyStroke::Only(Keycode::Backquote);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Action {
    Game(GameOp),
    Engine(EngineOp),
    Tree(TreeOp),
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

impl GameOp {
    /// Only horizontal movement can be DAS'd.
    pub fn can_das(&self) -> bool {
        match self {
            GameOp::MoveLeft | GameOp::MoveRight => true,
            _ => false,
        }
    }
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
    AutoPlay,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[allow(dead_code)]
pub enum TreeOp {
    Toggle,
    Out,
    OverEntry(usize),
    ClickEntry(usize),
    ScrollBy(i32),
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

#[derive(Debug, Error)]
#[error("invalid keystroke description")]
pub struct ParseKeyStrokeError;

impl std::str::FromStr for KeyStroke {
    type Err = ParseKeyStrokeError;
    fn from_str(s: &str) -> Result<Self, ParseKeyStrokeError> {
        if s.eq_ignore_ascii_case("shift") {
            Ok(KeyStroke::Shift)
        } else if c_prefix(s) {
            Keycode::from_name(&s[2..])
                .map(KeyStroke::Control)
                .ok_or(ParseKeyStrokeError)
        } else {
            Keycode::from_name(s)
                .map(KeyStroke::Only)
                .ok_or(ParseKeyStrokeError)
        }
    }
}

fn c_prefix(s: &str) -> bool {
    let bs = s.as_bytes();
    bs.len() >= 2 && (bs[0] == b'C' || bs[0] == b'c') && bs[1] == b'-'
}

/// Represents a controls configuration, which can be used to look up which `Action` is
/// triggered by a given key press, as well as timing values for handling.
#[derive(Clone)]
pub struct Controls {
    /// `(keycode, requires_ctrl_mod)` => `action` mapping.
    from_keycode: HashMap<(Keycode, bool), Action>,
    /// `action` => `binding` mapping.
    from_action: HashMap<Action, KeyStroke>,
    /// Handling settings.
    pub handling: Handling,
}

impl Controls {
    pub fn new<I>(key_bindings: I, handling: Handling) -> Self
    where
        I: IntoIterator<Item = (Action, KeyStroke)>,
    {
        let mut from_keycode = HashMap::new();
        let mut from_action = HashMap::new();
        for (action, ks) in key_bindings {
            from_action.insert(action, ks);
            match ks {
                KeyStroke::Only(kc) => {
                    from_keycode.insert((kc, false), action);
                    if kc == Keycode::RCtrl || kc == Keycode::LCtrl {
                        // rctrl/lctrl needs entries for both with & without ctrl mod,
                        // since the keys are themselves ctrl!
                        from_keycode.insert((kc, true), action);
                    }
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
            handling,
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

/// Represents handling configuration, which defines timing delays for different actions.
#[derive(Clone, Debug, Deserialize)]
pub struct Handling {
    /// Autoplay delay, in milliseconds. First delay is the initial delay before autoplay
    /// triggers; second delay is the time between repeated triggers afterwards.
    autoplay: (u64, u64),
    /// Delayed autoshift, in milliseconds.
    das: u64,
    /// Autoshift repeat rate, in milliseconds.
    arr: u64,
}

impl Handling {
    pub fn autoplay_delay(&self) -> std::time::Duration {
        std::time::Duration::from_millis(self.autoplay.0)
    }

    pub fn autoplay_repeat(&self) -> std::time::Duration {
        std::time::Duration::from_millis(std::cmp::max(self.autoplay.1, 1))
    }

    pub fn autoshift_delay(&self) -> std::time::Duration {
        std::time::Duration::from_millis(self.das)
    }

    pub fn autoshift_repeat(&self) -> std::time::Duration {
        std::time::Duration::from_millis(std::cmp::max(self.arr, 1))
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Deserialization

#[derive(Deserialize)]
struct ControlsSpec {
    game: GameControlsSpec,
    engine: EngineControlsSpec,
    handling: Handling,
}

#[derive(Deserialize)]
struct GameControlsSpec {
    left: KeyStroke,
    right: KeyStroke,
    ccw: KeyStroke,
    cw: KeyStroke,
    hold: KeyStroke,
    sd: KeyStroke,
    hd: KeyStroke,
    reset: KeyStroke,
    undo: KeyStroke,
}

#[derive(Deserialize)]
struct EngineControlsSpec {
    toggle: KeyStroke,
    next: KeyStroke,
    prev: KeyStroke,
    forward: KeyStroke,
    backward: KeyStroke,
    goto: KeyStroke,
}

impl ControlsSpec {
    fn to_controls(&self) -> Controls {
        let key_bindings = &[
            (Action::Game(GameOp::MoveLeft), self.game.left),
            (Action::Game(GameOp::MoveRight), self.game.right),
            (Action::Game(GameOp::RotateCCW), self.game.ccw),
            (Action::Game(GameOp::RotateCW), self.game.cw),
            (Action::Game(GameOp::Hold), self.game.hold),
            (Action::Game(GameOp::SonicDrop), self.game.sd),
            (Action::Game(GameOp::HardDrop), self.game.hd),
            (Action::Game(GameOp::Reset), self.game.reset),
            (Action::Game(GameOp::Undo), self.game.undo),
            (Action::Engine(EngineOp::Toggle), self.engine.toggle),
            (Action::Engine(EngineOp::Next), self.engine.next),
            (Action::Engine(EngineOp::Prev), self.engine.prev),
            (Action::Engine(EngineOp::StepForward), self.engine.forward),
            (Action::Engine(EngineOp::StepBackward), self.engine.backward),
            (Action::Engine(EngineOp::Goto), self.engine.goto),
            (Action::Tree(TreeOp::Toggle), BINDING_TOGGLE_TREE),
        ];
        Controls::new(key_bindings.iter().cloned(), self.handling.clone())
    }
}

impl<'de> Deserialize<'de> for Controls {
    fn deserialize<T: serde::Deserializer<'de>>(de: T) -> Result<Self, T::Error> {
        ControlsSpec::deserialize(de).map(|c| c.to_controls())
    }
}

impl<'de> Deserialize<'de> for KeyStroke {
    fn deserialize<T>(de: T) -> Result<Self, T::Error>
    where
        T: serde::Deserializer<'de>,
    {
        std::borrow::Cow::<str>::deserialize(de)?
            .parse()
            .map_err(serde::de::Error::custom)
    }
}

static DEFAULT_CONTROLS: &[u8] = include_bytes!("../../support/default-controls.json");

impl Default for Controls {
    fn default() -> Self {
        serde_json::from_slice(DEFAULT_CONTROLS).expect("BUG: default controls are malformed!")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_keycode() {
        let key = |s: &str| s.parse::<KeyStroke>().unwrap();
        assert_eq!(key("c-left"), KeyStroke::Control(Keycode::Left));
        assert_eq!(key("up"), KeyStroke::Only(Keycode::Up));
        assert_eq!(key("shift"), KeyStroke::Shift);
        assert_eq!(key("C-Left"), KeyStroke::Control(Keycode::Left));
        assert_eq!(key("UP"), KeyStroke::Only(Keycode::Up));
        assert_eq!(key("Shift"), KeyStroke::Shift);
        assert_eq!(key("Z"), KeyStroke::Only(Keycode::Z));
    }
}
