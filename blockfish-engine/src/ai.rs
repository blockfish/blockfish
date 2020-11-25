use crate::{
    common::{Color, Input},
    matrix::BasicMatrix,
    shape::{srs, NormalShape, NormalShapeId, ShapeTable},
};
use rand::Rng as _;
use std::cell::RefCell;

// Data types

#[derive(Clone, Debug)]
pub struct Snapshot {
    pub hold: Option<Color>,
    pub queue: Vec<Color>,
    pub matrix: BasicMatrix,
}

#[derive(Clone, Debug)]
pub struct Suggestion {
    pub inputs: Vec<Input>,
    pub score: i64,
}

pub type SuggestionsIter = std::sync::mpsc::Receiver<Suggestion>;

// AI algorithm

pub fn ai(snap: Snapshot) -> SuggestionsIter {
    let srs = srs();
    let current = *snap.queue.get(0).expect("bug: empty queue");
    let (tx, rx) = std::sync::mpsc::sync_channel(0);
    std::thread::spawn(move || {
        let ai = AI {
            rng: RefCell::new(rand::thread_rng()),
            srs: &srs,
            matrix: &snap.matrix,
            piece: current,
        };
        for _ in 0..2 {
            for place in ai.placements() {
                let sugg = Suggestion {
                    inputs: ai.finesse_inputs(&place),
                    score: ai.score(&place),
                };
                if tx.send(sugg).is_err() {
                    log::warn!("rx dropped; stopping iteration");
                    return;
                }
            }
        }
    });

    rx
}

struct AI<'m> {
    rng: RefCell<rand::rngs::ThreadRng>,
    srs: &'m ShapeTable,
    matrix: &'m BasicMatrix,
    piece: Color,
}

impl<'m> AI<'m> {
    fn placements(&self) -> impl Iterator<Item = Place> + 'm {
        let srs = self.srs;
        let matrix = self.matrix;
        srs.iter_norms_by_color(self.piece)
            .flat_map(move |norm_id| {
                let norm = srs.get_norm(norm_id);
                (0..(matrix.cols() + 1 - norm.cols())).map(move |col| {
                    let row = sonic_drop(matrix, norm, col);
                    Place { norm_id, row, col }
                })
            })
    }

    fn finesse_inputs(&self, place: &Place) -> Vec<Input> {
        let norm = self.srs.get_norm(place.norm_id);
        norm.finesse(place.col).collect()
    }

    fn score(&self, place: &Place) -> i64 {
        let norm = self.srs.get_norm(place.norm_id);
        let mut matrix = self.matrix.clone();
        norm.blit_to(&mut matrix, place.row, place.col);
        matrix.sift_rows();
        let ns = negative_space(&matrix);
        let rs = self.rng.borrow_mut().gen_range(-20, 20);
        ns + rs
    }
}

/// Represents a placement of a particular shape.
struct Place {
    norm_id: NormalShapeId,
    row: u16,
    col: u16,
}

impl std::fmt::Debug for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Place { norm_id, row, col } = self;
        write!(f, "{:?}, column {}, row {}", norm_id, col, row)
    }
}

/// Returns the row after sonic-dropping `shape` onto `matrix` at column `j0`.
fn sonic_drop(matrix: &BasicMatrix, shape: &NormalShape, j0: u16) -> u16 {
    (0..shape.cols())
        .map(|j| matrix.col_height(j0 + j).saturating_sub(shape.bottom(j)))
        .max()
        .expect("bug: no columns")
}

/// Counts the amount of "negative space" in the given matrix: the minimum number of
/// blocks needed to be filled to clear the entire matrix.
fn negative_space(matrix: &BasicMatrix) -> i64 {
    let mut acc = matrix.cols() as i64 * matrix.rows() as i64;
    for i in 0..matrix.rows() {
        for j in 0..matrix.cols() {
            if matrix.get((i, j)) {
                acc -= 1;
            }
        }
    }
    acc
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::basic_matrix;

    #[test]
    fn test_sonic_drop() {
        let srs = srs();
        let mat = basic_matrix![
            [false, true, true, true, true],
            [false, true, true, false, false]
        ];

        let i0 = srs.iter_norms_by_color(Color('I')).nth(0).unwrap();
        let i1 = srs.iter_norms_by_color(Color('I')).nth(1).unwrap();
        assert_eq!(sonic_drop(&mat, srs.get_norm(i0), 0), 2);
        assert_eq!(sonic_drop(&mat, srs.get_norm(i1), 0), 0);
        assert_eq!(sonic_drop(&mat, srs.get_norm(i1), 1), 2);

        let z0 = srs.iter_norms_by_color(Color('Z')).nth(0).unwrap();
        let z1 = srs.iter_norms_by_color(Color('Z')).nth(1).unwrap();
        assert_eq!(sonic_drop(&mat, srs.get_norm(z0), 2), 1);
        assert_eq!(sonic_drop(&mat, srs.get_norm(z0), 1), 2);
        assert_eq!(sonic_drop(&mat, srs.get_norm(z1), 0), 1);
        assert_eq!(sonic_drop(&mat, srs.get_norm(z1), 1), 2);
        assert_eq!(sonic_drop(&mat, srs.get_norm(z1), 2), 2);
        assert_eq!(sonic_drop(&mat, srs.get_norm(z1), 3), 1);

        let s1 = srs.iter_norms_by_color(Color('S')).nth(1).unwrap();
        assert_eq!(sonic_drop(&mat, srs.get_norm(s1), 0), 2);

        let l1 = srs.iter_norms_by_color(Color('L')).nth(1).unwrap();
        let l2 = srs.iter_norms_by_color(Color('L')).nth(2).unwrap();
        assert_eq!(sonic_drop(&mat, srs.get_norm(l1), 1), 2);
        assert_eq!(sonic_drop(&mat, srs.get_norm(l2), 0), 1);
        assert_eq!(sonic_drop(&mat, srs.get_norm(l2), 2), 2);
    }

    #[test]
    fn test_negative_space() {
        assert_eq!(negative_space(&BasicMatrix::with_cols(5)), 0);
        assert_eq!(
            negative_space(&basic_matrix![
                [true, true, false, false],
                [false, false, false, true],
            ]),
            5
        );
        assert_eq!(
            negative_space(&basic_matrix![
                [false, false, false, false],
                [false, false, false, false],
            ]),
            8
        );
    }
}
