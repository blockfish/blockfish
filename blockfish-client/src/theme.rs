use sdl2::pixels::{Color, PixelFormat};
use serde::Deserialize;
use std::collections::HashMap;
use thiserror::Error;

use block_stacker::CellColor;

/// Stores the colors to use for a particular theme.
pub struct Colors {
    pub background: Color,
    pub grid_background: (Color, Color),
    pub text: (Color, Color),
    pub good_bad: (Color, Color),
    pub cell: HashMap<CellColor, Color>,
}

/// A specification of a theme.
#[derive(Clone, Debug, Deserialize)]
pub struct Theme {
    bg: HexColor,
    text: TextTheme,
    matrix: MatrixTheme,
    mino: MinoTheme,
}

#[derive(Clone, Debug, Deserialize)]
struct MatrixTheme {
    bg: HexColor,
    alt: HexColor,
}

#[derive(Clone, Debug, Deserialize)]
struct TextTheme {
    primary: HexColor,
    secondary: HexColor,
    good: HexColor,
    bad: HexColor,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
struct MinoTheme {
    l: HexColor,
    o: HexColor,
    j: HexColor,
    i: HexColor,
    s: HexColor,
    z: HexColor,
    t: HexColor,
    g1: HexColor,
    g2: HexColor,
}

impl Theme {
    pub fn to_colors(&self) -> Colors {
        use std::convert::TryFrom;
        let rgb24 = PixelFormat::try_from(sdl2::pixels::PixelFormatEnum::RGB888).unwrap();
        Colors {
            background: self.bg.color(&rgb24),
            grid_background: (self.matrix.bg.color(&rgb24), self.matrix.alt.color(&rgb24)),
            text: (
                self.text.primary.color(&rgb24),
                self.text.secondary.color(&rgb24),
            ),
            good_bad: (self.text.good.color(&rgb24), self.text.bad.color(&rgb24)),
            cell: {
                let mut tbl = HashMap::new();
                tbl.insert('L', self.mino.l.color(&rgb24));
                tbl.insert('O', self.mino.o.color(&rgb24));
                tbl.insert('J', self.mino.j.color(&rgb24));
                tbl.insert('I', self.mino.i.color(&rgb24));
                tbl.insert('S', self.mino.s.color(&rgb24));
                tbl.insert('Z', self.mino.z.color(&rgb24));
                tbl.insert('T', self.mino.t.color(&rgb24));
                tbl.insert('G', self.mino.g1.color(&rgb24));
                tbl.insert('H', self.mino.g2.color(&rgb24));
                tbl
            },
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct HexColor(u32);

impl HexColor {
    fn color(&self, fmt: &PixelFormat) -> Color {
        Color::from_u32(&fmt, self.0)
    }
}

#[derive(Debug, Error)]
#[error("invalid hex-color string")]
pub struct ParseHexColorError;

impl std::str::FromStr for HexColor {
    type Err = ParseHexColorError;
    fn from_str(s: &str) -> Result<Self, ParseHexColorError> {
        if s.len() != 7 || !s.starts_with("#") {
            return Err(ParseHexColorError);
        }
        u32::from_str_radix(&s[1..], 16)
            .map(HexColor)
            .map_err(|_| ParseHexColorError)
    }
}

impl<'de> Deserialize<'de> for HexColor {
    fn deserialize<T>(de: T) -> Result<Self, T::Error>
    where
        T: serde::Deserializer<'de>,
    {
        std::borrow::Cow::<str>::deserialize(de)?
            .parse()
            .map_err(serde::de::Error::custom)
    }
}

static DEFAULT_THEME: &[u8] = include_bytes!("../../support/themes/dark.json");

impl Default for Theme {
    fn default() -> Self {
        serde_json::from_slice(DEFAULT_THEME).expect("BUG: default theme is malformed!")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_hex_color() {
        assert_eq!("#1abf3e".parse::<HexColor>().unwrap(), HexColor(0x1abf3e));
        assert_eq!("#1ABF3E".parse::<HexColor>().unwrap(), HexColor(0x1abf3e));
        assert!("#1ab".parse::<HexColor>().is_err());
        assert!("!1abf3e".parse::<HexColor>().is_err());
        assert!("1abf3e3".parse::<HexColor>().is_err());
    }
}
