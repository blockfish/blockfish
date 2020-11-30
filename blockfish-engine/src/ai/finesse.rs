use crate::{
    shape::{NormalShapeId, ShapeTable},
    Input, Orientation,
};

/// Returns optimal finesse inputs to get shape specified by `norm_id` to column `targ_col` from
/// the spawn location.
// TODO: tucks/ spins?
pub fn inputs<'a>(
    srs: &'a ShapeTable,
    norm_id: NormalShapeId,
    targ_col: u16,
) -> impl Iterator<Item = Input> {
    let norm = &srs[norm_id];
    norm.orientations()
        .map(|ori| {
            let spawn_col = norm.spawn_col(ori);
            (
                RepeatedInputs::rotate(ori),
                RepeatedInputs::horizontal(spawn_col, targ_col),
            )
        })
        .min_by_key(|(r, h)| r.len() + h.len())
        .map(|(r, h)| r.chain(h))
        .expect("unreachable: shape has no orientations?")
}

struct RepeatedInputs(Input, usize, usize);

impl RepeatedInputs {
    /// Iterator of inputs to get from initial orientation to `ori`.
    fn rotate(ori: Orientation) -> Self {
        let (inp, cnt) = match ori {
            Orientation::R0 => (Input::CW, 0),
            Orientation::R1 => (Input::CW, 1),
            Orientation::R2 => (Input::CW, 2),
            Orientation::R3 => (Input::CCW, 1),
        };
        Self(inp, 0, cnt)
    }

    /// Iterator of inputs to move horizontally from `col0` to `col1`.
    fn horizontal(col0: u16, col1: u16) -> Self {
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
    use crate::{shape::srs, Color};

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

    fn all_finesse(srs: &ShapeTable, id: NormalShapeId) -> Vec<Vec<Input>> {
        let norm = &srs[id];
        let num_cols = 10 - norm.cols() + 1;
        (0..num_cols)
            .map(|col| inputs(srs, id, col).collect())
            .collect()
    }

    #[test]
    fn test_i_finesse() {
        use Input::*;
        let srs = srs();
        let mut i_ids = srs.iter_norms_by_color(Color('I'));
        let i0 = i_ids.next().unwrap();
        let i1 = i_ids.next().unwrap();
        assert_eq!(
            all_finesse(&srs, i0),
            vec![
                vec![Left, Left, Left],
                vec![Left, Left],
                vec![Left],
                vec![],
                vec![Right],
                vec![Right, Right],
                vec![Right, Right, Right],
            ]
        );
        assert_eq!(
            all_finesse(&srs, i1),
            vec![
                vec![CCW, Left, Left, Left, Left],
                vec![CCW, Left, Left, Left],
                vec![CCW, Left, Left],
                vec![CCW, Left],
                vec![CCW],
                vec![CW],
                vec![CW, Right],
                vec![CW, Right, Right],
                vec![CW, Right, Right, Right],
                vec![CW, Right, Right, Right, Right],
            ]
        );
    }

    #[test]
    fn test_sz_finesse() {
        use Input::*;
        let srs = srs();
        for &color in [Color('S'), Color('Z')].iter() {
            let mut sz_ids = srs.iter_norms_by_color(color);
            let sz0 = sz_ids.next().unwrap();
            let sz1 = sz_ids.next().unwrap();
            assert_eq!(
                all_finesse(&srs, sz0),
                vec![
                    vec![Left, Left, Left],
                    vec![Left, Left],
                    vec![Left],
                    vec![],
                    vec![Right],
                    vec![Right, Right],
                    vec![Right, Right, Right],
                    vec![Right, Right, Right, Right],
                ]
            );
            assert_eq!(
                all_finesse(&srs, sz1),
                vec![
                    vec![CCW, Left, Left, Left],
                    vec![CCW, Left, Left],
                    vec![CCW, Left],
                    vec![CCW],
                    vec![CW],
                    vec![CW, Right],
                    vec![CW, Right, Right],
                    vec![CW, Right, Right, Right],
                    vec![CW, Right, Right, Right, Right],
                ]
            );
        }
    }
}
