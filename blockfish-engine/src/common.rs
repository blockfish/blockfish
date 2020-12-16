use serde::{Deserialize, Serialize};
use std::convert::TryFrom;
use thiserror::Error;

#[cfg(not(test))]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Color(char);

#[cfg(test)]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Color(pub char);

#[derive(Debug, Error)]
#[error("not a valid character to represent a block color")]
pub struct InvalidColorChar;

impl Color {
    pub fn as_char(&self) -> char {
        self.0
    }
}

impl TryFrom<char> for Color {
    type Error = InvalidColorChar;

    fn try_from(c: char) -> Result<Self, InvalidColorChar> {
        if c.is_alphabetic() {
            Ok(Self(c))
        } else {
            Err(InvalidColorChar)
        }
    }
}

impl std::fmt::Debug for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_char())
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, Ord, PartialOrd, Serialize, Deserialize)]
#[repr(u8)]
#[allow(dead_code)]
pub enum Orientation {
    R0,
    R1,
    R2,
    R3,
}

impl Default for Orientation {
    fn default() -> Self {
        Orientation::R0
    }
}

impl Orientation {
    pub fn cw(self) -> Orientation {
        match self {
            Orientation::R0 => Orientation::R1,
            Orientation::R1 => Orientation::R2,
            Orientation::R2 => Orientation::R3,
            Orientation::R3 => Orientation::R0,
        }
    }

    pub fn cw_acyclic(self) -> Option<Orientation> {
        match self {
            Orientation::R0 => Some(Orientation::R1),
            Orientation::R1 => Some(Orientation::R2),
            Orientation::R2 => Some(Orientation::R3),
            Orientation::R3 => None,
        }
    }

    pub fn ccw(self) -> Orientation {
        match self {
            Orientation::R0 => Orientation::R3,
            Orientation::R1 => Orientation::R0,
            Orientation::R2 => Orientation::R1,
            Orientation::R3 => Orientation::R2,
        }
    }

    pub fn from_u8(x: u8) -> Option<Self> {
        match x {
            0 => Some(Orientation::R0),
            1 => Some(Orientation::R1),
            2 => Some(Orientation::R2),
            3 => Some(Orientation::R3),
            _ => None,
        }
    }

    pub fn iter_all() -> impl Iterator<Item = Orientation> {
        std::iter::successors(Some(Orientation::R0), |r| r.cw_acyclic())
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, Ord, PartialOrd, Serialize, Deserialize)]
#[repr(u8)]
pub enum Input {
    Left,
    Right,
    CW,
    CCW,
    Hold,
    SD,
    HD,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_all_orientations() {
        assert_eq!(
            Orientation::iter_all().collect::<Vec<_>>(),
            [
                Orientation::R0,
                Orientation::R1,
                Orientation::R2,
                Orientation::R3,
            ]
        );
    }
}
