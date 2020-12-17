use crate::{ruleset::Ruleset, CellColor, PieceType};
use rand::{rngs::ThreadRng, Rng as _, RngCore};
use std::rc::Rc;

//////////////////////////////////////////////////////////////////////////////////////////
// Game logic

#[derive(Clone)]
pub struct Stacker {
    rules: Rc<Ruleset>,
    rng: ThreadRng,
    matrix: Matrix,
    cheese: Cheese,
    current: Option<Piece>,
    queue: BagRandomizer,
    held: Option<PieceType>,
}

#[derive(Clone, Debug, Default)]
pub struct Config {
    pub garbage: GarbageConfig,
}

#[derive(Clone, Debug)]
pub struct GarbageConfig {
    pub min_height: usize,
    pub max_height: usize,
    pub total_lines: Option<usize>,
}

impl Default for GarbageConfig {
    fn default() -> Self {
        GarbageConfig {
            min_height: 4,
            max_height: 9,
            total_lines: None,
        }
    }
}

impl Stacker {
    /// Constructs a new stacker game state with ruleset `rules`.
    pub fn new(rules: Rc<Ruleset>, cfg: Config) -> Self {
        let mut stacker = Stacker {
            rng: rand::thread_rng(),
            matrix: Matrix::new(rules.cols as u16),
            cheese: Cheese::new(cfg.garbage, rules.cols as u16),
            current: None,
            queue: BagRandomizer::new(rules.types().collect()),
            held: None,
            rules,
        };
        stacker.cheese(false);
        stacker.new_piece();
        stacker
    }

    /// Returns the ruleset used by this stacker.
    pub fn ruleset(&self) -> &Rc<Ruleset> {
        &self.rules
    }

    /// Returns a list of all occupied cells in the matrix.
    pub fn matrix<'a>(&'a self) -> impl Iterator<Item = ((u16, u16), PieceType)> + 'a {
        self.matrix.iter()
    }

    /// Inserts the appropriate number of cheese rows to the bottom of the matrix.
    fn cheese(&mut self, combo: bool) {
        self.cheese.generate(&mut self.rng, &mut self.matrix, combo)
    }

    /// Returns the current piece's typ, if any.
    pub fn current_piece_type(&self) -> Option<PieceType> {
        self.current.map(|pc| pc.typ)
    }

    /// Returns `(color, row, col, rot, ghost_row)` for the current piece, if any.
    pub fn current_piece(&self) -> Option<(PieceType, i16, i16, i32, i16)> {
        self.current.map(|pc| {
            let ghost_row = {
                let mut ghost = pc;
                ghost.sonic_drop(&self.matrix, &self.rules);
                ghost.origin.0
            };
            (pc.typ, pc.origin.0, pc.origin.1, pc.rot, ghost_row)
        })
    }

    /// Returns the current piece type being held, if any.
    pub fn held(&self) -> Option<PieceType> {
        self.held
    }

    /// Returns the list of next previews.
    pub fn next(&self) -> &[PieceType] {
        &self.queue[..self.rules.previews]
    }

    /// Move the current piece horizontally by `dx` squares. Returns `true` if the piece
    /// moved without being obstructed.
    pub fn move_horizontal(&mut self, dx: i16) -> bool {
        if let Some(pc) = self.current.as_mut() {
            pc.try_move_by(&self.matrix, &self.rules, 0, dx)
        } else {
            false
        }
    }

    /// Rotate the current piece by `dr` rotations (negative means CCW). Returns `true` if
    /// the piece rotated without being obstructed.
    // TODO: different return value for non-kick success & kicked success.
    pub fn rotate(&mut self, dr: i32) -> bool {
        if let Some(pc) = self.current.as_mut() {
            pc.try_rot_by(&self.matrix, &self.rules, dr)
        } else {
            false
        }
    }

    /// Drop the current piece all the way to the floor. Returns `true` if the piece moved
    /// at all as a result.
    pub fn sonic_drop(&mut self) -> bool {
        if let Some(pc) = self.current.as_mut() {
            pc.sonic_drop(&self.matrix, &self.rules) > 0
        } else {
            false
        }
    }

    /// Hard drop the current piece. Returns `(lines_cleared, garbage_cleared)`.
    pub fn hard_drop(&mut self) -> (usize, usize) {
        let (n, ds) = match self.current.take() {
            Some(mut pc) => {
                pc.sonic_drop(&self.matrix, &self.rules);
                for coord in pc.coords(&self.rules) {
                    self.matrix.set(coord, pc.typ);
                }
                self.matrix.sift()
            }
            None => (0, 0),
        };
        self.cheese(n > 0);
        self.new_piece();
        (n, ds)
    }

    /// Hold the current piece. Returns `true` if the piece was held, or `false` if the
    /// hold queue was just used and cannot be swapped again.
    pub fn hold(&mut self) -> bool {
        let prev = self.current_piece_type();
        if let Some(held) = std::mem::replace(&mut self.held, prev) {
            // TODO: prevent repeat holds
            self.current = Some(Piece::new(&self.rules, held));
        } else {
            // no hold piece, so grab one from the queue
            self.new_piece();
        }
        true
    }

    /// Spawns a new piece with the next piece type from the queue.
    fn new_piece(&mut self) {
        self.queue.refill(&mut self.rng, self.rules.previews + 1);
        self.current = self.queue.next().map(|typ| Piece::new(&self.rules, typ));
    }
}

impl std::fmt::Debug for Stacker {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "queue: ")?;
        for &c in self.next() {
            write!(f, "{}", c)?;
        }
        if let Some(c) = self.held() {
            write!(f, ", hold: {}", c)?;
        }
        write!(f, "\n{:?}", self.matrix)?;
        Ok(())
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Matrix

/// Represents a matrix of colored cells.
#[derive(Clone)]
struct Matrix {
    /// Number of columns per line.
    cols: u16,
    /// Matrix row data.
    lines: Vec<Line>,
}

impl Matrix {
    /// Construct a new matrix with `cols` number of columns.
    fn new(cols: u16) -> Self {
        let lines = vec![];
        Matrix { cols, lines }
    }

    /// Returns the cell at coordinate `(i, j)`, if present.
    fn get(&self, (i, j): (i16, i16)) -> Option<CellColor> {
        if i < 0 || j < 0 || j >= self.cols as i16 {
            Some('H')
        } else if i >= self.lines.len() as i16 {
            None
        } else {
            self.lines[i as usize].get(j as u16)
        }
    }

    /// Fills the cell at `(i, j)` to contain color `cc`.
    fn set(&mut self, (i, j): (i16, i16), cc: CellColor) {
        if i < 0 || j < 0 || j >= self.cols as i16 {
            return;
        } else if i >= self.lines.len() as i16 {
            let cols = self.cols;
            let new_line = || Line::new(cols);
            self.lines.resize_with((i + 1) as usize, new_line);
        }
        self.lines[i as usize].set(j as u16, cc);
    }

    /// Removes full rows from the matrix. Returns `(lines_cleared, garbage_cleared)`.
    fn sift(&mut self) -> (usize, usize) {
        let mut lines_cleared = 0;
        let mut garbage_cleared = 0;
        let mut dst_idx = 0;
        for src_idx in 0..self.lines.len() {
            let line = &self.lines[src_idx];
            if line.is_full() {
                lines_cleared += 1;
                if line.is_garbage() {
                    garbage_cleared += 1;
                }
            } else {
                self.lines.swap(src_idx, dst_idx);
                dst_idx += 1;
            }
        }
        self.lines.resize_with(dst_idx, || unreachable!());
        (lines_cleared, garbage_cleared)
    }

    /// Inserts a line, `line` onto the bottom of the matrix.
    fn insert_below(&mut self, line: Line) {
        self.lines.insert(0, line);
    }

    /// Returns the number of rows that are garbage lines.
    fn garbage_rows(&self) -> usize {
        self.lines.iter().take_while(|ln| ln.is_garbage()).count()
    }

    /// Returns `((i, j), cc)` for each coordinate `(i, j)` occupied with color `cc`.
    fn iter<'a>(&'a self) -> impl Iterator<Item = ((u16, u16), CellColor)> + 'a {
        let lines = self.lines.iter().enumerate();
        lines.flat_map(|(i, ln)| ln.iter().map(move |(j, cc)| ((i as u16, j), cc)))
    }
}

/// Represents a single line in a matrix.
#[derive(Clone)]
struct Line {
    /// The cells in this line.
    cells: Vec<CellColor>,
}

impl Line {
    /// Construct an empty line with the given number of columns.
    fn new(cols: u16) -> Self {
        let cells = vec![' '; cols as usize];
        Line { cells }
    }

    /// Constructs a "garbage" line that is `cols` columns wide, with empty column at
    /// `hole`.
    fn garbage(cols: u16, hole: u16) -> Self {
        let cells = (0..cols)
            .map(|col| if col == hole { ' ' } else { 'G' })
            .collect();
        Line { cells }
    }

    /// Returns the cell at column `j`, if present.
    fn get(&self, j: u16) -> Option<PieceType> {
        match self.cells[j as usize] {
            ' ' => None,
            c => Some(c),
        }
    }

    /// Fills the cell at column `j` to contain color `cc`.
    fn set(&mut self, j: u16, cc: CellColor) {
        assert!(cc != ' ');
        self.cells[j as usize] = cc;
    }

    /// Returns `true` if every cell in this row is filled.
    fn is_full(&self) -> bool {
        self.cells.iter().all(|&c| c != ' ')
    }

    /// Returns `true` if this is a garbage line.
    fn is_garbage(&self) -> bool {
        // only need to look at 2 cells because at most one of them will be empty and the
        // other garbage.
        self.cells.iter().take(2).any(|&c| c == 'G')
    }

    /// Returns `(j, cc)` for each column `j` occupied with color `cc`.
    fn iter<'a>(&'a self) -> impl Iterator<Item = (u16, CellColor)> + 'a {
        let cells = self.cells.iter().cloned().enumerate();
        cells
            .filter(|&(_, cc)| cc != ' ')
            .map(|(j, cc)| (j as u16, cc))
    }
}

impl std::fmt::Debug for Matrix {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (i, line) in self.lines.iter().rev().enumerate() {
            if i > 0 {
                write!(f, "\n|")?;
            } else {
                write!(f, "|")?;
            }
            for cc in line.cells.iter() {
                write!(f, "{}", cc)?;
            }
            write!(f, "|")?;
        }
        Ok(())
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Current piece

/// Represents a poly-mino being controlled by the player.
#[derive(Copy, Clone)]
struct Piece {
    /// The poly-mino type.
    typ: PieceType,
    /// The origin coordinate of the piece. Note that row/column may be negative while the
    /// piece is still entirely within the matrix, since shapes may contain empty squares
    /// on the sides.
    origin: (i16, i16),
    /// The orientation of the piece. Negative indicates counter-clockwise rotations.
    rot: i32,
}

impl Piece {
    /// Constructs a new `typ` type piece, with ruleset `rules`.
    fn new(rules: &Ruleset, typ: PieceType) -> Self {
        let origin = rules.spawn(typ);
        let rot = 0;
        Piece { typ, origin, rot }
    }

    /// Returns the coordinates of squares occupied by the piece. `rules` should be the
    /// same ruleset used to initialize this piece.
    fn coords<'r>(self, rules: &'r Ruleset) -> impl Iterator<Item = (i16, i16)> + 'r {
        rules.coords(self.typ, self.rot).map(move |(i, j)| {
            let i = (i as i16) + self.origin.0;
            let j = (j as i16) + self.origin.1;
            (i, j)
        })
    }

    /// Attempts to move the piece by offset `(i_off, j_off)`. Returns `true` if the piece
    /// was moved without colliding with `matrix`. `rules` should be the same ruleset used
    /// to initialize this piece.
    fn try_move_by(&mut self, matrix: &Matrix, rules: &Ruleset, i_off: i16, j_off: i16) -> bool {
        let mut tmp = *self;
        tmp.origin.0 += i_off;
        tmp.origin.1 += j_off;
        if tmp.coords(rules).any(|coord| matrix.get(coord).is_some()) {
            false
        } else {
            *self = tmp;
            true
        }
    }

    /// Attempts to rotate the piece by `r_off`. Returns `true` if the piece was rotated
    /// without colliding with `matrix`. The piece may be offset in the process according
    /// to the kick table. `rules` should be the same ruleset used to initialize this
    /// piece.
    fn try_rot_by(&mut self, matrix: &Matrix, rules: &Ruleset, r_off: i32) -> bool {
        for (i_off, j_off) in rules.kicks(self.typ, self.rot, self.rot + r_off) {
            let mut tmp = *self;
            tmp.origin.0 += i_off;
            tmp.origin.1 += j_off;
            tmp.rot += r_off;
            if !tmp.coords(rules).any(|coord| matrix.get(coord).is_some()) {
                *self = tmp;
                return true;
            }
        }
        false
    }

    /// "Sonic drops" the piece to the floor. Returns the number of rows that the piece
    /// fell before colliding with `matrix`. `rules` should be the same ruleset used to
    /// initialize this piece.
    fn sonic_drop(&mut self, matrix: &Matrix, rules: &Ruleset) -> u16 {
        let mut dy = 0;
        let mut tmp = *self;
        while tmp.try_move_by(matrix, rules, -1, 0) {
            dy += 1;
            *self = tmp;
        }
        dy
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Randomizer

#[derive(Clone)]
struct BagRandomizer {
    bag: Vec<PieceType>,
    queue: Vec<PieceType>,
}

impl BagRandomizer {
    fn new(bag: Vec<PieceType>) -> Self {
        let queue = Vec::with_capacity(bag.len() * 2);
        Self { bag, queue }
    }

    fn refill(&mut self, rng: &mut impl RngCore, n: usize) {
        while self.queue.len() < n {
            // shuffle bag
            for i in 1..self.bag.len() {
                let j = rng.gen_range(0, i);
                self.bag.swap(i, j);
            }
            // push onto end of queue
            self.queue.extend(self.bag.iter().cloned());
        }
    }
}

impl std::ops::Deref for BagRandomizer {
    type Target = [PieceType];
    fn deref(&self) -> &[PieceType] {
        &self.queue
    }
}

impl Iterator for BagRandomizer {
    type Item = PieceType;
    fn next(&mut self) -> Option<PieceType> {
        let &next = self.queue.get(0)?;
        self.queue.remove(0);
        Some(next)
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Cheese rows

#[derive(Clone)]
struct Cheese {
    cfg: GarbageConfig,
    cols: u16,
    prev_hole: Option<u16>,
    count: usize,
}

impl Cheese {
    fn new(cfg: GarbageConfig, cols: u16) -> Self {
        Cheese {
            cols,
            cfg,
            prev_hole: None,
            count: 0,
        }
    }

    /// Generate cheese lines, adding them to the bottom of matrix `tgt`. Different number
    /// of lines are added depending on if the last line clear was part of a combo,
    /// indicated by `comboing`.
    fn generate(&mut self, rng: &mut impl RngCore, tgt: &mut Matrix, comboing: bool) {
        // target garbage height depending on if last piece was a combo
        let height = if comboing {
            std::cmp::min(self.cfg.min_height, self.cfg.max_height)
        } else {
            self.cfg.max_height
        };

        // compute number of lines to add, from garbage rows present & total lines
        let mut num = height.saturating_sub(tgt.garbage_rows());
        if let Some(total) = self.cfg.total_lines {
            num = std::cmp::min(num, total);
        }

        let cols = self.cols;
        for _ in 0..num {
            self.count += 1;
            // don't pick previous garbage column if possible
            let hole = match self.prev_hole {
                None => rng.gen_range(0, cols),
                Some(prev) => {
                    let col = rng.gen_range(0, cols - 1);
                    (col + prev + 1) % cols
                }
            };
            self.prev_hole = Some(hole);
            tgt.insert_below(Line::garbage(cols, hole));
        }
    }
}
