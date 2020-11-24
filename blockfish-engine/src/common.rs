use std::convert::TryFrom;
use thiserror::Error;

#[cfg(not(test))]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Color(char);

#[cfg(test)]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
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

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
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

#[allow(dead_code)]
impl Orientation {
    pub fn cw(self) -> Orientation {
        match self {
            Orientation::R0 => Orientation::R1,
            Orientation::R1 => Orientation::R2,
            Orientation::R2 => Orientation::R3,
            Orientation::R3 => Orientation::R0,
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

    pub fn flip(self) -> Orientation {
        match self {
            Orientation::R0 => Orientation::R2,
            Orientation::R1 => Orientation::R3,
            Orientation::R2 => Orientation::R0,
            Orientation::R3 => Orientation::R1,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
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

pub struct RepeatedInputs(Input, usize, usize);

impl RepeatedInputs {
    /// Iterator of inputs to get from initial orientation to `ori`.
    // TODO: get from one orientation to another
    pub fn rotate(ori: Orientation) -> Self {
        let (inp, cnt) = match ori {
            Orientation::R0 => (Input::CW, 0),
            Orientation::R1 => (Input::CW, 1),
            Orientation::R2 => (Input::CW, 2),
            Orientation::R3 => (Input::CCW, 1),
        };
        Self(inp, 0, cnt)
    }

    /// Iterator of inputs to move horizontally from `col0` to `col1`.
    pub fn horizontal(col0: u16, col1: u16) -> Self {
        let dif = (col1 as i32) - (col0 as i32);
        let inp = if dif < 0 { Input::Left } else { Input::Right };
        Self(inp, 0, dif.abs() as usize)
    }

    /// Returns the total number of `Input`s produced by this iterator.
    pub fn len(&self) -> usize {
        self.2
    }
}

impl Iterator for RepeatedInputs {
    type Item = Input;

    fn next(&mut self) -> Option<Input> {
        if self.1 >= self.2 {
            None
        } else {
            self.1 += 1;
            Some(self.0)
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some((self.2).saturating_sub(self.1)))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_rotate() {
        use self::Input::*;
        use self::Orientation::*;
        assert_eq!(RepeatedInputs::rotate(R0).collect::<Vec<_>>(), vec![]);
        assert_eq!(RepeatedInputs::rotate(R1).collect::<Vec<_>>(), vec![CW]);
        assert_eq!(RepeatedInputs::rotate(R2).collect::<Vec<_>>(), vec![CW, CW]);
        assert_eq!(RepeatedInputs::rotate(R3).collect::<Vec<_>>(), vec![CCW]);
    }

    #[test]
    fn test_horizontal() {
        use self::Input::*;
        assert_eq!(RepeatedInputs::horizontal(2, 2).collect::<Vec<_>>(), vec![]);
        assert_eq!(
            RepeatedInputs::horizontal(0, 2).collect::<Vec<_>>(),
            vec![Right, Right]
        );
        assert_eq!(
            RepeatedInputs::horizontal(5, 2).collect::<Vec<_>>(),
            vec![Left, Left, Left]
        );
    }
}
