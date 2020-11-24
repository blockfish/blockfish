use std::{collections::HashMap, rc::Rc};

/// Represents a specification of the game rules, such as the matrix size, the number of
/// previews, and the shape information for every mino.
#[derive(Debug)]
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
    minos: HashMap<char, PolyMino>,
}

/// Represents the specification of one poly-mino.
#[derive(Clone, Debug)]
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

impl Ruleset {
    // TODO: parsing rules from text specification

    /// Returns a copy of the guideline rules.
    pub fn guideline() -> Rc<Ruleset> {
        let mut minos = HashMap::new();

        // J, L, S, T, Z

        let mut jlstz_kicks = HashMap::with_capacity(16);
        macro_rules! jlstz_kick {
            ($dir:ident($offs:expr), $kicks:expr) => {
                jlstz_kicks.insert(Kick::$dir($offs), $kicks)
            };
        }
        macro_rules! jlstz {
            ($col:expr, $coords:expr) => {
                minos.insert(
                    $col,
                    PolyMino {
                        width: 3,
                        coords: $coords,
                        spawn: (18, 3),
                        kicks: jlstz_kicks.clone(),
                    },
                )
            };
        }
        jlstz_kick!(CW(0), vec![(0, 0), (0, -1), (-1, -1), (2, 0), (2, -1)]);
        jlstz_kick!(CW(1), vec![(0, 0), (0, -1), (1, -1), (-2, 0), (-2, -1)]);
        jlstz_kick!(CW(2), vec![(0, 0), (0, 1), (-1, 1), (2, 0), (2, 1)]);
        jlstz_kick!(CW(3), vec![(0, 0), (0, 1), (1, 1), (-2, 0), (-2, 1)]);
        jlstz_kick!(CCW(0), vec![(0, 0), (0, 1), (-1, 1), (2, 0), (2, 1)]);
        jlstz_kick!(CCW(1), vec![(0, 0), (0, -1), (1, -1), (-2, 0), (-2, -1)]);
        jlstz_kick!(CCW(2), vec![(0, 0), (0, -1), (-1, -1), (2, 0), (2, -1)]);
        jlstz_kick!(CCW(3), vec![(0, 0), (0, 1), (1, 1), (-2, 0), (-2, 1)]);
        jlstz!('J', vec![(1, 0), (1, 1), (1, 2), (2, 0)]);
        jlstz!('L', vec![(1, 0), (1, 1), (1, 2), (2, 2)]);
        jlstz!('S', vec![(1, 0), (1, 1), (2, 1), (2, 2)]);
        jlstz!('T', vec![(1, 0), (1, 1), (1, 2), (2, 1)]);
        jlstz!('Z', vec![(1, 1), (1, 2), (2, 0), (2, 1)]);

        // I

        let mut i_kicks = HashMap::with_capacity(16);
        macro_rules! i_kick {
            ($dir:ident($offs:expr), $kicks:expr) => {
                i_kicks.insert(Kick::$dir($offs), $kicks)
            };
        }
        i_kick!(CW(0), vec![(0, 0), (0, 1), (0, -2), (-2, 1), (1, -2)]);
        i_kick!(CW(1), vec![(0, 0), (0, -2), (0, 1), (-1, -2), (2, 1)]);
        i_kick!(CW(2), vec![(0, 0), (0, -1), (0, 2), (2, -1), (-1, -2)]);
        i_kick!(CW(3), vec![(0, 0), (0, 2), (0, -1), (1, 2), (-2, -1)]);
        i_kick!(CCW(0), vec![(0, 0), (0, 2), (0, -1), (1, 2), (-2, -1)]);
        i_kick!(CCW(1), vec![(0, 0), (0, 1), (0, -2), (-2, 1), (1, -2)]);
        i_kick!(CCW(2), vec![(0, 0), (0, -2), (0, 1), (-1, -2), (2, 1)]);
        i_kick!(CCW(3), vec![(0, 0), (0, -1), (0, 2), (2, -1), (-1, 2)]);
        minos.insert(
            'I',
            PolyMino {
                width: 4,
                coords: vec![(2, 0), (2, 1), (2, 2), (2, 3)],
                spawn: (17, 3),
                kicks: i_kicks,
            },
        );

        // O

        minos.insert(
            'O',
            PolyMino {
                width: 4,
                coords: vec![(1, 1), (1, 2), (2, 1), (2, 2)],
                spawn: (18, 3),
                kicks: HashMap::new(),
            },
        );

        Rc::new(Ruleset {
            cols: 10,
            rows: 25,
            visible_rows: 20,
            garbage_height: 8,
            previews: 5,
            minos,
        })
    }

    fn mino(&self, color: char) -> &PolyMino {
        self.minos.get(&color).expect("bug: no such mino")
    }

    /// Returns a list of every color of shape per this ruleset.
    pub fn types<'a>(&'a self) -> impl Iterator<Item = char> + 'a {
        self.minos.keys().cloned()
    }

    /// Returns the relative coords of the shape specified by `color`, at orientation
    /// `rot`.
    pub fn coords<'a>(&'a self, color: char, rot: i32) -> impl Iterator<Item = (u16, u16)> + 'a {
        let PolyMino { coords, width, .. } = self.mino(color);
        coords
            .iter()
            .map(move |&coord| rotate_coord(coord, *width, rot))
    }

    /// Returns the list of kick offsets for shape specified by `color`, when rotating
    /// from orientation `rot0` to `rot`.
    pub fn kicks<'a>(
        &'a self,
        color: char,
        rot0: i32,
        rot: i32,
    ) -> impl Iterator<Item = (i16, i16)> + 'a {
        let kick = if rot0 < rot {
            Kick::CW(normalize_rot(rot))
        } else {
            Kick::CCW(normalize_rot(rot))
        };
        match self.mino(color).kicks.get(&kick) {
            Some(kicks) => kicks.as_slice(),
            // in SRS the O kick table is empty, so this fallback case is used.
            None => &[(0, 0)],
        }
        .iter()
        .cloned()
    }

    /// Returns the initial spawn coordinates for shape specified by `color`.
    pub fn spawn(&self, color: char) -> (i16, i16) {
        self.mino(color).spawn
    }
}

/// Rotates `coord`, `r` times, such that it is contained in a box with width/height `w`.
///
/// For example: `rotate_coord((1, 3), 4, 3)` does the following:
///
///     -----------       -----------
///     | . . . . |       | . . x . |
///     | . . . . |       | . . . . |
///     | . . . x |  -->  | . . . . |
///     | . . . . |       | . . . . |
///     -----------       -----------
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
///
/// `normalize_rot(1)` = `1`
/// `normalize_rot(4)` = `0`
/// `normalize_rot(-1)` = `3`
/// `normalize_rot(-6)` = `2`
fn normalize_rot(r: i32) -> u8 {
    let r = r % 4;
    if r < 0 {
        (r + 4) as u8
    } else {
        r as u8
    }
}
