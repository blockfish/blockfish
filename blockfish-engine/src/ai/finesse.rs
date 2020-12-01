use crate::{ai::Place, shape::ShapeTable, Input, Orientation};

/// Returns optimal finesse inputs to perform the given placement.
// TODO: tucks/ spins?
pub fn inputs(stbl: &ShapeTable, place: &Place) -> impl Iterator<Item = Input> {
    let norm = &stbl[place.norm_id];
    let targ_col = place.col;
    let did_hold = place.did_hold;
    norm.orientations()
        .map(|ori| Inputs::new(did_hold, ori, norm.spawn_col(ori), targ_col))
        .min_by_key(|i| i.len())
        .expect("unreachable: shape has no orientations?")
}

struct Inputs {
    hold: bool,
    ori: Orientation,
    taps: i16,
}

impl Inputs {
    fn new(hold: bool, ori: Orientation, col0: u16, col1: u16) -> Self {
        let taps = (col1 as i16) - (col0 as i16);
        Self { hold, ori, taps }
    }
}

impl Iterator for Inputs {
    type Item = Input;
    fn next(&mut self) -> Option<Input> {
        if self.hold {
            self.hold = false;
            return Some(Input::Hold);
        }

        match self.ori {
            Orientation::R0 => {
                if self.taps > 0 {
                    self.taps -= 1;
                    Some(Input::Right)
                } else if self.taps < 0 {
                    self.taps += 1;
                    Some(Input::Left)
                } else {
                    None
                }
            }
            Orientation::R1 => {
                self.ori = Orientation::R0;
                Some(Input::CW)
            }
            Orientation::R2 => {
                self.ori = Orientation::R1;
                Some(Input::CW)
            }
            Orientation::R3 => {
                // for czsmall mode, switch to R2
                self.ori = Orientation::R0;
                Some(Input::CCW)
            }
        }
    }
}

impl ExactSizeIterator for Inputs {
    fn len(&self) -> usize {
        let h_count = if self.hold { 1 } else { 0 };
        let r_count = match self.ori {
            Orientation::R0 => 0,
            Orientation::R1 => 1,
            Orientation::R2 => 2,
            Orientation::R3 => 1,
        };
        let t_count = self.taps.abs() as usize;
        h_count + r_count + t_count
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        shape::{srs, NormalShapeId},
        Color,
    };

    #[test]
    fn test_inputs() {
        use Input::*;
        let inps1 = Inputs::new(false, Orientation::R1, 3, 5);
        let inps2 = Inputs::new(false, Orientation::R2, 4, 2);
        let inps3 = Inputs::new(false, Orientation::R3, 8, 8);
        let inps4 = Inputs::new(true, Orientation::R0, 8, 9);
        assert_eq!(inps1.len(), 3);
        assert_eq!(inps1.collect::<Vec<_>>(), [CW, Right, Right]);
        assert_eq!(inps2.len(), 4);
        assert_eq!(inps2.collect::<Vec<_>>(), [CW, CW, Left, Left]);
        assert_eq!(inps3.len(), 1);
        assert_eq!(inps3.collect::<Vec<_>>(), [CCW]);
        assert_eq!(inps4.len(), 2);
        assert_eq!(inps4.collect::<Vec<_>>(), [Hold, Right]);
    }

    fn all_finesse(srs: &ShapeTable, norm_id: NormalShapeId) -> Vec<Vec<Input>> {
        let num_cols = 10 - srs[norm_id].cols() + 1;
        (0..num_cols)
            .map(|col| {
                let place = Place {
                    norm_id,
                    did_hold: false,
                    row: 0,
                    col,
                };
                inputs(srs, &place).collect()
            })
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

    #[test]
    fn test_did_hold_inputs() {
        use Input::*;
        let srs = srs();
        let z1 = srs.iter_norms_by_color(Color('Z')).nth(1).unwrap();
        let place = Place {
            norm_id: z1,
            row: 0,
            col: 2,
            did_hold: true,
        };
        assert_eq!(inputs(&srs, &place).collect::<Vec<_>>(), [Hold, CCW, Left]);
    }
}
