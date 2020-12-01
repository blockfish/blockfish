use crate::{
    ai::State,
    shape::{NormalShape, NormalShapeId, ShapeTable},
    BasicMatrix, Color,
};

// Data type for a single placement

/// Represents a placement of a particular shape.
#[derive(Clone)]
pub struct Place {
    pub norm_id: NormalShapeId,
    pub did_hold: bool,
    pub row: u16,
    pub col: u16,
}

// Computing all placements

/// Returns all valid placements given the snapshot `snap`.
pub fn placements<'a>(stbl: &'a ShapeTable, state: &'a State) -> impl Iterator<Item = Place> + 'a {
    let matrix = state.matrix();
    avail_colors(state).flat_map(move |(color, did_hold)| {
        stbl.iter_norms_by_color(color).flat_map(move |norm_id| {
            avail_coords(matrix, &stbl[norm_id]).map(move |(row, col)| Place {
                norm_id,
                row,
                col,
                did_hold,
            })
        })
    })
}

fn avail_colors(state: &State) -> impl Iterator<Item = (Color, bool)> {
    let (col_nh, mut col_h) = state.next();
    if col_nh == col_h {
        // don't use hold if identical to current piece
        col_h = None;
    }
    let nh = col_nh.into_iter().map(|c| (c, false));
    let h = col_h.into_iter().map(|c| (c, true));
    nh.chain(h)
}

fn avail_coords<'a>(
    matrix: &'a BasicMatrix,
    norm: &'a NormalShape,
) -> impl Iterator<Item = (u16, u16)> + 'a {
    let cols = matrix.cols() + 1 - norm.cols();
    (0..cols).map(move |col| {
        let row = sonic_drop(matrix, norm, col);
        (row, col)
    })
}

/// Returns the row from sonic-dropping the shape `norm` from column `col` onto `matrix`.
fn sonic_drop(matrix: &BasicMatrix, norm: &NormalShape, col: u16) -> u16 {
    (0..norm.cols())
        .map(|j| matrix.col_height(col + j).saturating_sub(norm.bottom(j)))
        .max()
        .expect("bug: no columns")
}

//////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use super::*;
    use crate::{basic_matrix, shape::srs, Color, Snapshot};

    fn colors(hold: char, queue: &str) -> Vec<(char, bool)> {
        let snapshot = Snapshot {
            hold: std::convert::TryFrom::try_from(hold).ok(),
            queue: queue.chars().map(Color).collect(),
            matrix: BasicMatrix::with_cols(1),
        };
        avail_colors(&snapshot.into())
            .map(|(c, h)| (c.0, h))
            .collect()
    }

    #[test]
    fn test_avail_colors() {
        assert_eq!(colors('.', "TJZ"), [('T', false), ('J', true)]);
        assert_eq!(colors('I', "TJZ"), [('T', false), ('I', true)]);
        assert_eq!(colors('I', "IJZ"), [('I', false)]);
        assert_eq!(colors('.', "T"), [('T', false)]);
        assert_eq!(colors('O', ""), [('O', true)]);
        assert_eq!(colors('.', ""), []);
    }

    #[test]
    fn test_sonic_drop() {
        // . x x . .
        // . x x x x
        let mat = basic_matrix![
            [false, true, true, true, true],
            [false, true, true, false, false]
        ];
        let srs = srs();

        let sonic_drop = |norm_id, col| sonic_drop(&mat, &srs[norm_id], col);

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
}
