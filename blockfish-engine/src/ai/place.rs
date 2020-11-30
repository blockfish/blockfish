use crate::{
    shape::{NormalShapeId, ShapeTable},
    BasicMatrix, Snapshot,
};

//////////////////////////////////////////////////////////////////////////////////////////
// Data type for a single placement

/// Represents a placement of a particular shape.
#[derive(Clone)]
pub struct Place {
    pub norm_id: NormalShapeId,
    pub row: u16,
    pub col: u16,
}

impl Place {
    /// Returns the placement by sonic dropping the shape specified by `norm_id` onto `matrix`
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
}

impl std::fmt::Debug for Place {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Place { norm_id, row, col } = self;
        write!(f, "{:?}, column {}, row {}", norm_id, col, row)
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Computing all placements

/// Returns all valid placements given the snapshot `snap`.
pub fn placements<'a>(srs: &'a ShapeTable, snap: &'a Snapshot) -> impl Iterator<Item = Place> + 'a {
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

//////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use super::*;
    use crate::{basic_matrix, shape::srs, Color};

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
}
