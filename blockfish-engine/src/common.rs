use serde::{Deserialize, Serialize};
use std::{
    convert::{TryFrom, TryInto},
    num::NonZeroU8,
};
use thiserror::Error;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Color(NonZeroU8);

#[derive(Debug, Error)]
#[error("not a valid character to represent a block color")]
pub struct InvalidColorChar;

impl Color {
    #[cfg(test)]
    pub fn n(ch: char) -> Self {
        ch.try_into().unwrap()
    }

    pub fn as_char(&self) -> char {
        self.0.get() as char
    }
}

impl TryFrom<char> for Color {
    type Error = InvalidColorChar;

    fn try_from(c: char) -> Result<Self, InvalidColorChar> {
        if c.is_alphabetic() {
            // SAFETY: alphabetic chars are not null
            Ok(Color(unsafe { NonZeroU8::new_unchecked(c as u8) }))
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

impl Serialize for Color {
    fn serialize<T>(&self, ser: T) -> Result<T::Ok, T::Error>
    where
        T: serde::Serializer,
    {
        self.as_char().serialize(ser)
    }
}

impl<'de> Deserialize<'de> for Color {
    fn deserialize<T>(de: T) -> Result<Self, T::Error>
    where
        T: serde::Deserializer<'de>,
    {
        char::deserialize(de)?
            .try_into()
            .map_err(serde::de::Error::custom)
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
    fn test_parse_color() {
        assert_eq!(Color::try_from('T').ok(), Some(Color::n('T')));
        assert_eq!(Color::try_from('.').ok(), None);
        assert_eq!(Color::try_from(' ').ok(), None);
        assert_eq!(Color::try_from('\0').ok(), None);
    }

    #[test]
    fn test_sizeof() {
        use std::mem::size_of;
        assert_eq!(size_of::<Orientation>(), 1);
        assert_eq!(size_of::<Input>(), 1);
        assert_eq!(size_of::<Color>(), 1);
        assert_eq!(size_of::<Option<Color>>(), 1);
        assert_eq!(size_of::<Option<Orientation>>(), 1);
        assert_eq!(size_of::<(Color, Orientation)>(), 2);
        assert_eq!(size_of::<(u16, u16, Orientation)>(), 6);
        assert_eq!(size_of::<(Color, (u16, u16, Orientation))>(), 8);
    }

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
