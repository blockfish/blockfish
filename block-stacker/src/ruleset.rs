use super::PieceType;
use serde::Deserialize;
use std::collections::HashMap;

/// Represents a specification of the game rules, such as the matrix size, the number of
/// previews, and the shape information for every mino.
#[derive(Deserialize)]
pub struct Ruleset {
    /// Number of columns in the matrix.
    pub cols: usize,
    /// Number of rows in the matrix.
    pub rows: usize,
    /// Number of rows in the matrix that are visible to the player.
    pub visible_rows: usize,
    /// Number of preview pieces the player sees.
    pub previews: usize,
    /// The number of garbage rows continually present.
    pub garbage_height: usize,
    /// The available poly-mino shapes in this ruleset.
    minos: HashMap<PieceType, PolyMino>,
}

/// Represents the specification of one poly-mino.
#[derive(Deserialize)]
struct PolyMino {
    /// The width/height of the bounding box surrounding `coords` (used for rotating).
    width: u16,
    /// The list of coords making up the shape.
    coords: Vec<(u16, u16)>,
    /// The initial spawn coordinate.
    spawn: (i16, i16),
    /// The kick table.
    kicks: HashMap<Kick, Vec<(i16, i16)>>,
}

/// Specifies a type of kick.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
enum Kick {
    /// `CW(r)` indicates a clockwise rotation ending at rotation `r`
    CW(u8),
    /// `CCW(r)` indicates a counter-clockwise rotation ending at rotation `r`.
    CCW(u8),
}

impl<'de> Deserialize<'de> for Kick {
    fn deserialize<D>(de: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::Error;
        let s = String::deserialize(de)?;
        match s.chars().nth(0) {
            Some('>') => s[1..].parse().map(Kick::CW).map_err(D::Error::custom),
            Some('<') => s[1..].parse().map(Kick::CCW).map_err(D::Error::custom),
            _ => Err(D::Error::custom("bad kick specification")),
        }
    }
}

static GUIDELINE_BYTES: &[u8] = include_bytes!("../../support/guideline.json");

impl Ruleset {
    /// Returns a copy of the guideline rules.
    pub fn guideline() -> Ruleset {
        serde_json::from_slice(GUIDELINE_BYTES).expect("BUG: guideline data is malformed!")
    }

    fn mino(&self, typ: PieceType) -> &PolyMino {
        self.minos.get(&typ).expect("BUG: no such mino")
    }

    /// Returns a list of every color of shape per this ruleset.
    pub fn types<'a>(&'a self) -> impl Iterator<Item = PieceType> + 'a {
        self.minos.keys().cloned()
    }

    /// Returns the relative coords of the shape specified by `color`, at orientation
    /// `rot`.
    pub fn coords<'a>(&'a self, typ: PieceType, rot: i32) -> impl Iterator<Item = (u16, u16)> + 'a {
        let PolyMino { coords, width, .. } = self.mino(typ);
        coords
            .iter()
            .map(move |&coord| rotate_coord(coord, *width, rot))
    }

    /// Returns the list of kick offsets for shape specified by `color`, when rotating
    /// from orientation `rot0` to `rot`.
    pub fn kicks<'a>(
        &'a self,
        typ: PieceType,
        rot0: i32,
        rot: i32,
    ) -> impl Iterator<Item = (i16, i16)> + 'a {
        let kick = if rot0 < rot {
            Kick::CW(normalize_rot(rot))
        } else {
            Kick::CCW(normalize_rot(rot))
        };
        match self.mino(typ).kicks.get(&kick) {
            Some(kicks) => kicks.as_slice(),
            // in SRS the O kick table is empty, so this fallback case is used.
            None => &[(0, 0)],
        }
        .iter()
        .cloned()
    }

    /// Returns the initial spawn coordinates for shape specified by `color`.
    pub fn spawn(&self, typ: PieceType) -> (i16, i16) {
        self.mino(typ).spawn
    }
}

/// Rotates `coord`, `r` times, such that it is contained in a box with width/height `w`.
///
/// For example: `rotate_coord((1, 3), 4, 3)` does the following:
///
/// -----------       -----------
/// | . . . . |       | . . x . |
/// | . . . . |       | . . . . |
/// | . . . x |  -->  | . . . . |
/// | . . . . |       | . . . . |
/// -----------       -----------
///
/// returning `(3, 2)`.
fn rotate_coord(mut coord: (u16, u16), w: u16, r: i32) -> (u16, u16) {
    let mut r = normalize_rot(r);
    while r > 0 {
        let (x, y) = coord;
        coord = (w - y - 1, x);
        r -= 1;
    }
    coord
}

/// "Normalizes" rotation `r` to be in the range [0, 3).
fn normalize_rot(r: i32) -> u8 {
    let r = r % 4;
    if r < 0 {
        (r + 4) as u8
    } else {
        r as u8
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_rotate_coord() {
        assert_eq!(rotate_coord((1, 3), 4, 3), (3, 2));
    }

    #[test]
    fn test_normalize_rot() {
        assert_eq!(normalize_rot(1), 1);
        assert_eq!(normalize_rot(4), 0);
        assert_eq!(normalize_rot(-1), 3);
        assert_eq!(normalize_rot(-6), 2);
    }

    #[test]
    fn test_guideline_o_kicks() {
        let rules = Ruleset::guideline();
        for &r in &[-2, -1, 0, 1, 2, 3] {
            assert_eq!(rules.kicks('O', 1, r).collect::<Vec<_>>(), [(0, 0)]);
        }
    }
}
