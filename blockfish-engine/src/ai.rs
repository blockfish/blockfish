use crate::{
    common::{Color, Input},
    matrix::BasicMatrix,
    shape::{srs, NormalShapeId, ShapeTable},
};
use std::collections::VecDeque;

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

pub fn ai(snapshot: Snapshot) -> SuggestionsIter {
    let srs = srs();
    let (tx, rx) = std::sync::mpsc::sync_channel(100);
    std::thread::spawn(move || {
        let initial_places = placements(&srs, &snapshot).collect::<Vec<_>>();
        let num_roots = initial_places.len();
        let mut root_inputs = Vec::with_capacity(num_roots);
        let mut root_best_scores = Vec::with_capacity(num_roots);
        let mut jobs = VecDeque::with_capacity(4000);
        for (i, place) in initial_places.into_iter().enumerate() {
            root_inputs.push(place.inputs(&srs));
            root_best_scores.push(std::i64::MAX);
            jobs.push_back(Job {
                snapshot: snapshot.clone(),
                placement: place,
                root: i,
                depth: 1,
                score: 0,
            });
        }

        let mut completed = 0;
        let mut total = jobs.len();
        while let Some(job) = jobs.pop_front() {
            completed += 1;
            if completed % 1000 == 0 {
                log::info!("pending: {:5}  {}/{}", jobs.len(), completed, total);
            }

            if jobs.len() > 20000 {
                log::info!("truncating job queue");
                jobs.make_contiguous().sort_by_key(|j| j.score);
                jobs.truncate(15000);
            }

            let mut snapshot = job.snapshot;
            snapshot.queue.remove(0);
            let Place { norm_id, row, col } = job.placement;
            srs[norm_id].blit_to(&mut snapshot.matrix, row, col);
            snapshot.matrix.sift_rows();

            let score = score(&snapshot, job.depth);
            let best_score = &mut root_best_scores[job.root];
            if score < *best_score {
                *best_score = score;
                let inputs = root_inputs[job.root].clone();
                if tx.send(Suggestion { inputs, score }).is_err() {
                    log::warn!("engine process interrupted, quitting");
                    return;
                }
            }

            if job.depth < 5 {
                for place in placements(&srs, &snapshot) {
                    total += 1;
                    jobs.push_back(Job {
                        snapshot: snapshot.clone(),
                        placement: place,
                        root: job.root,
                        depth: job.depth + 1,
                        score,
                    });
                }
            }
        }
    });

    rx
}

/// Returns all valid placements given the snapshot `snap`.
fn placements<'a>(srs: &'a ShapeTable, snap: &'a Snapshot) -> impl Iterator<Item = Place> + 'a {
    let current = snap.queue.get(0);
    let matrix = &snap.matrix;
    current
        .into_iter()
        .flat_map(move |&color| srs.iter_norms_by_color(color))
        .flat_map(move |norm_id| {
            let shape_width = &srs[norm_id].cols();
            let cols = matrix.cols() + 1 - shape_width;
            (0..cols).map(move |col| Place::sonic_drop(srs, matrix, norm_id, col))
        })
}

struct Job {
    snapshot: Snapshot,
    placement: Place,
    depth: usize,
    root: usize,
    score: i64,
}

/// Represents a placement of a particular shape.
#[derive(Clone)]
struct Place {
    norm_id: NormalShapeId,
    row: u16,
    col: u16,
}

impl Place {
    /// Returns the placement by sonic dropping the shape given by `norm_id` onto `matrix`
    /// at column `col`.
    fn sonic_drop(
        srs: &ShapeTable,
        matrix: &BasicMatrix,
        norm_id: NormalShapeId,
        col: u16,
    ) -> Self {
        let norm = &srs[norm_id];
        let row = (0..norm.cols())
            .map(|j| matrix.col_height(col + j).saturating_sub(norm.bottom(j)))
            .max()
            .expect("bug: no columns");
        Place { norm_id, row, col }
    }

    fn inputs(&self, srs: &ShapeTable) -> Vec<Input> {
        srs[self.norm_id].finesse(self.col).collect()
    }
}

impl std::fmt::Debug for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Place { norm_id, row, col } = self;
        write!(f, "{:?}, column {}, row {}", norm_id, col, row)
    }
}

fn score(snapshot: &Snapshot, depth: usize) -> i64 {
    let bs = block_count(&snapshot.matrix) as i64;
    let rs = (snapshot.matrix.rows() as i64) * 40;
    let ds = (depth as i64) * -10;
    bs + rs + ds
}

fn block_count(matrix: &BasicMatrix) -> usize {
    (0..matrix.rows())
        .flat_map(|i| (0..matrix.cols()).filter(move |&j| matrix.get((i, j))))
        .count()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::basic_matrix;

    #[test]
    fn test_sonic_drop() {
        // . x x . .
        // . x x x x
        let mat = basic_matrix![
            [false, true, true, true, true],
            [false, true, true, false, false]
        ];
        let srs = srs();

        let sonic_drop = |norm_id, col| Place::sonic_drop(&srs, &mat, norm_id, col).row;

        let i0 = srs.iter_norms_by_color(Color('I')).nth(0).unwrap();
        let i1 = srs.iter_norms_by_color(Color('I')).nth(1).unwrap();
        assert_eq!(sonic_drop(i0, 0), 2);
        assert_eq!(sonic_drop(i1, 0), 0);
        assert_eq!(sonic_drop(i1, 1), 2);

        let z0 = srs.iter_norms_by_color(Color('Z')).nth(0).unwrap();
        let z1 = srs.iter_norms_by_color(Color('Z')).nth(1).unwrap();
        assert_eq!(sonic_drop(z0, 2), 1);
        assert_eq!(sonic_drop(z0, 1), 2);
        assert_eq!(sonic_drop(z1, 0), 1);
        assert_eq!(sonic_drop(z1, 1), 2);
        assert_eq!(sonic_drop(z1, 2), 2);
        assert_eq!(sonic_drop(z1, 3), 1);

        let s1 = srs.iter_norms_by_color(Color('S')).nth(1).unwrap();
        assert_eq!(sonic_drop(s1, 0), 2);

        let l1 = srs.iter_norms_by_color(Color('L')).nth(1).unwrap();
        let l2 = srs.iter_norms_by_color(Color('L')).nth(2).unwrap();
        assert_eq!(sonic_drop(l1, 1), 2);
        assert_eq!(sonic_drop(l2, 0), 1);
        assert_eq!(sonic_drop(l2, 2), 2);
    }

    #[test]
    fn test_block_count() {
        assert_eq!(block_count(&BasicMatrix::with_cols(5)), 0);
        assert_eq!(
            block_count(&basic_matrix![
                [true, true, false, false],
                [false, false, false, true],
            ]),
            3
        );
        assert_eq!(
            block_count(&basic_matrix![
                [false, false, false, false],
                [false, false, false, false],
            ]),
            0
        );
    }
}
