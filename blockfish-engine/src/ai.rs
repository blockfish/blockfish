use crate::{
    common::{Color, Input},
    matrix::BasicMatrix,
    shape::{srs, NormalShape, NormalShapeId, ShapeTable},
};

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
    let mut queue = snap.queue.into_iter();
    let current = queue.next().expect("bug: empty queue");
    let matrix = snap.matrix;

    let (tx, rx) = std::sync::mpsc::sync_channel(0);
    std::thread::spawn(move || {
        let places = all_valid_places(&srs, current, &matrix);
        let mut tmp_mat = matrix.clone();
        for place in places {
            let norm = srs.get_norm(place.norm_id);
            tmp_mat.clone_from(&matrix);
            norm.blit_to(&mut tmp_mat, place.row, place.col);
            tmp_mat.sift_rows();
            let sugg = Suggestion {
                inputs: norm.finesse(place.col).collect(),
                score: negative_space(&tmp_mat),
            };
            std::thread::sleep(std::time::Duration::from_millis(10));
            if tx.send(sugg).is_err() {
                log::warn!("rx dropped; stopping iteration");
                break;
            }
        }
    });

    rx
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

/// Returns all valid `Place`s for pieces of `color` onto `matrix`.
fn all_valid_places<'a>(
    srs: &'a ShapeTable,
    color: Color,
    matrix: &'a BasicMatrix,
) -> impl Iterator<Item = Place> + 'a {
    srs.iter_norms_by_color(color).flat_map(move |norm_id| {
        let norm = srs.get_norm(norm_id);
        (0..(matrix.cols() + 1 - norm.cols())).map(move |col| {
            let row = sonic_drop(matrix, norm, col);
            Place { norm_id, row, col }
        })
    })
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
