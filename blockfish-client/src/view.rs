use crate::{controls::Controls, resources::Resources};
use block_stacker::{CellColor, PieceType, Ruleset};
use sdl2::{pixels::Color, rect::Rect, render::Texture};
use std::{collections::HashMap, rc::Rc};

/// Default window size for view.
pub static DEFAULT_SIZE: (u32, u32) = (1200, 720);

/// Holds all of the graphical elements to draw on the view, and where to draw them.
pub struct View<'r> {
    resources: Resources<'r>,
    ruleset: Rc<Ruleset>,
    colors: Colors,
    geom: Geometry,
    queue: Cells,
    matrix: Cells,
    piece: Cells,
    ghost: Cells,
    eng_matrix: Cells,
    eng_ghost: Cells,
    motd: Label<'r>,
    stats: Vec<Label<'r>>,
    controls: [(Label<'r>, Vec<Label<'r>>); 2],
    progress: (Label<'r>, bool),
    piece_rating: (Label<'r>, Label<'r>, bool),
    eng_overlay: [Label<'r>; 3],
    eng_status: Label<'r>,
}

/// Canvas type that `View` is able to paint to.
pub type Canvas = sdl2::render::Canvas<sdl2::video::Window>;

/// Used to build `Texture`'s; annoying artefact of SDL2 API.
pub type TextureCreator = sdl2::render::TextureCreator<sdl2::video::WindowContext>;

impl<'r> View<'r> {
    /// Constructs a new view.
    pub fn new(ruleset: Rc<Ruleset>, resources: Resources<'r>, version_string: &str) -> Box<Self> {
        let mut motd = Label::new();
        motd.set(&format!("Blockfish {}", version_string));

        let mut hd0 = Label::new();
        let mut hd1 = Label::new();
        hd0.set("game controls");
        hd1.set("engine controls");

        let rows = ruleset.visible_rows as u16;
        let cols = ruleset.cols as u16;

        Box::new(Self {
            ruleset,
            resources,
            colors: Colors::dark_theme(),
            geom: Geometry::new(rows, cols, DEFAULT_SIZE),
            queue: Cells::new(None),
            matrix: Cells::new(Some(rows)),
            piece: Cells::new(Some(rows)),
            ghost: Cells::new(Some(rows)),
            eng_matrix: Cells::new(Some(rows)),
            eng_ghost: Cells::new(Some(rows)),
            stats: Vec::with_capacity(4),
            controls: [(hd0, vec![]), (hd1, vec![])],
            progress: (Label::new(), false),
            piece_rating: (Label::new(), Label::new(), true),
            eng_overlay: [Label::new(), Label::new(), Label::new()],
            eng_status: Label::new(),
            motd,
        })
    }

    /// Sets the controls configuration to be displayed on the side of the view.
    pub fn set_controls(&mut self, controls: &Controls) {
        use crate::controls::{Action, EngineOp::*, GameOp::*};

        let label_text = |prefix: &str, actions: &[Action]| {
            let mut buf = String::with_capacity(16);
            buf.push_str(prefix);
            for (i, &action) in actions.iter().enumerate() {
                if i > 0 {
                    buf.push_str(", ");
                }
                if let Some(ks) = controls.key_stroke(action) {
                    use std::fmt::Write as _;
                    write!(&mut buf, "{}", ks).unwrap();
                } else {
                    buf.push_str("--");
                }
            }
            buf
        };

        let left_right = &[Action::Game(MoveLeft), Action::Game(MoveRight)];
        let ccw_cw = &[Action::Game(RotateCCW), Action::Game(RotateCW)];
        let sd_hd = &[Action::Game(SonicDrop), Action::Game(HardDrop)];
        let hold = &[Action::Game(Hold)];
        let undo = &[Action::Game(Undo)];
        let reset = &[Action::Game(Reset)];
        let game_ctrls = &mut self.controls[0].1;
        game_ctrls.resize_with(6, Label::new);
        game_ctrls[0].set(&label_text("\u{2190}, \u{2192}:         ", left_right));
        game_ctrls[1].set(&label_text("ccw, cw:      ", ccw_cw));
        game_ctrls[2].set(&label_text("sd, hd:       ", sd_hd));
        game_ctrls[3].set(&label_text("hold:         ", hold));
        game_ctrls[4].set(&label_text("undo:         ", undo));
        game_ctrls[5].set(&label_text("reset:        ", reset));

        let toggle = &[Action::Engine(Toggle)];
        // let step = &[Action::Engine(StepForward), Action::Engine(StepBackward)];
        let switch = &[Action::Engine(Next), Action::Engine(Prev)];
        let go_to = &[Action::Engine(Goto)];
        let eng_ctrls = &mut self.controls[1].1;
        eng_ctrls.resize_with(3, Label::new);
        eng_ctrls[0].set(&label_text("toggle:       ", toggle));
        // eng_ctrls[1].set(&label_text("step sugg:    ", step));
        eng_ctrls[1].set(&label_text("switch sugg:  ", switch));
        eng_ctrls[2].set(&label_text("go to sugg:   ", go_to));
    }

    /// Sets the next previews to contain each piece type in `previews`, and the hold
    /// space to contain `hold`.
    pub fn set_queue<P>(&mut self, previews: P, hold: Option<PieceType>)
    where
        P: IntoIterator<Item = PieceType>,
    {
        self.queue.clear();
        let ruleset = self.ruleset.as_ref();
        let geom = &self.geom;
        let hold = hold.into_iter();
        let cells = hold.flat_map(|ty| ruleset.coords(ty, 0).map(move |ij| (ij, ty)));
        self.queue.insert(cells, |i, j| geom.hold_cell(i, j));
        for (idx, ty) in previews.into_iter().enumerate() {
            let cells = ruleset.coords(ty, 0).map(|ij| (ij, ty));
            self.queue.insert(cells, |i, j| geom.next_cell(idx, i, j));
        }
    }

    /// Sets the contents of the user's matrix, given a list of cells represented by
    /// `((row, col), cell_color)`.
    pub fn set_matrix<I>(&mut self, cells: I)
    where
        I: IntoIterator<Item = ((u16, u16), CellColor)>,
    {
        let geom = &self.geom;
        self.matrix.clear();
        self.matrix.insert(cells, |i, j| geom.main_cell(i, j));
    }

    /// Sets the contents of the engine's matrix.
    pub fn set_engine_matrix<I>(&mut self, cells: I)
    where
        I: IntoIterator<Item = ((u16, u16), CellColor)>,
    {
        let geom = &self.geom;
        self.eng_matrix.clear();
        self.eng_matrix.insert(cells, |i, j| geom.eng_cell(i, j));
    }

    /// Sets the user's current piece to be type `ty` at row `i`, column `j`, rotation
    /// `r`, with ghost piece shown at row `g`.
    pub fn set_piece(&mut self, ty: PieceType, i: i16, j: i16, r: i32, g: i16) {
        let ruleset = self.ruleset.as_ref();
        let geom = &self.geom;
        let cells = |i: i16| {
            ruleset.coords(ty, r).map(move |(i_off, j_off)| {
                let i = (i + i_off as i16) as u16;
                let j = (j + j_off as i16) as u16;
                ((i, j), ty)
            })
        };
        self.piece.clear();
        self.piece.insert(cells(i), |i, j| geom.main_cell(i, j));
        self.ghost.clear();
        self.ghost.insert(cells(g), |i, j| geom.main_cell(i, j));
    }

    /// Clears the user's current piece, so no piece or ghost is shown.
    pub fn clear_piece(&mut self) {
        self.piece.clear();
        self.ghost.clear();
    }

    /// Sets the engine's suggested piece to be type `ty` at row `i`, column `j`, rotation
    /// `r`.
    pub fn set_engine_piece(&mut self, ty: PieceType, i: i16, j: i16, r: i32) {
        let geom = &self.geom;
        let cells = self.ruleset.coords(ty, r).map(move |(i_off, j_off)| {
            let i = (i + i_off as i16) as u16;
            let j = (j + j_off as i16) as u16;
            ((i, j), ty)
        });
        self.eng_ghost.clear();
        self.eng_ghost.insert(cells, |i, j| geom.eng_cell(i, j));
    }

    /// Clears the engine's suggested piece, so no piece is shown.
    pub fn clear_engine_piece(&mut self) {
        self.eng_ghost.clear();
    }

    /// Sets the rating shown for the user's current piece. If `rating` is
    /// `Some((static_eval, score))`, indicates that the static evaluation after placing
    /// the current piece is `static_eval`, and the score for the best sequence following
    /// that placement is `score`.  If `rating` is `None`, indicates that the engine is
    /// still computing the rating.
    #[allow(unused)]
    pub fn set_piece_rating(&mut self, rating: Option<(i64, i64)>) {
        let (lbl0, lbl1, working) = &mut self.piece_rating;
        if let Some((static_eval, score)) = rating {
            lbl0.set(&format!("engine rating: {} ", score));
            lbl1.set(&format!("static eval: {}", static_eval));
            *working = true;
        } else {
            lbl0.clear();
            lbl1.clear();
            *working = false;
        }
    }

    /// Sets the game statistics to be shown according to the given values:
    ///
    /// - `pc`: number of pieces.
    /// - `lc`: number of total lines cleared.
    /// - `cc`: number of color clears.
    /// - `ds`: number of garbage lines cleared ("downstack").
    /// - `ds_goal`: total number of garbage lines to clear.
    pub fn set_stats(
        &mut self,
        pc: usize,
        lc: usize,
        cc: usize,
        ds: usize,
        ds_goal: Option<usize>,
    ) {
        let mut pace = None;
        if ds > 0 {
            pace = Some((100 * pc + ds - 1) / ds);
        }

        let mut dpp = None;
        if pc > 0 {
            dpp = Some((ds as f32) / (pc as f32));
        }

        let progress_label;
        let finished;
        if let Some(goal) = ds_goal {
            progress_label = format!("{}/{}L", ds, goal);
            finished = ds >= goal;
        } else {
            progress_label = format!("{}L", ds);
            finished = false;
        };

        use crate::util::text_fmt::*;
        self.stats.resize_with(5, Label::new);
        self.stats[0].set(&format!("pieces:        {}", pc));
        self.stats[1].set(&format!("lines:         {}", lc));
        self.stats[2].set(&format!("color clears:  {}", cc));
        self.stats[3].set(&format!("dpp:           {}", maybe_f32(dpp, "?")));
        self.stats[4].set(&format!("100L pace:     {}", maybe(pace, "\u{221e}")));
        self.progress.0.set(&progress_label);
        self.progress.1 = finished;
    }

    /// Sets the information about the current engine suggestion.
    ///
    /// - `num`: the 'place' of the current suggestions, i.e. `0` for best placement, `4`
    ///   for 5th best placement.
    /// - `seq`: the position along the sequence of this suggestion, i.e. `(1, 4)` for the
    ///   2nd position in a sequence of 4 pieces.
    /// - `rating`: the engine rating for this sequence.
    pub fn set_engine_suggestion(&mut self, num: usize, seq: (usize, usize), rating: i64) {
        let lbl = &mut self.eng_overlay;
        if seq.1 == 1 {
            // sequence contains only one placement
            lbl[0].set(&format!("#{}", num + 1));
        } else {
            lbl[0].set(&format!("#{} ({}/{})", num + 1, seq.0 + 1, seq.1));
        }
        lbl[1].set(&format!("rating: {}", rating));
    }

    /// Clears the engine suggestion information.
    pub fn clear_engine_suggestion(&mut self) {
        for lbl in self.eng_overlay.iter_mut() {
            lbl.clear();
        }
    }

    /// Sets the text showing the current status of engine process, where `params` is a
    /// string representing the parameters passed to the engine. If `search` if
    /// `Some((time, nodes, iters))`, indicates that the engine has processed `nodes`
    /// total nodes over `iters` iterations in `time` seconds.
    pub fn set_engine_status(&mut self, params: &str, search: Option<(f64, usize, usize)>) {
        use std::fmt::Write;
        let mut text = format!("engine settings: {:?}", params);
        if let Some((time, nodes, iters)) = search {
            write!(
                &mut text,
                " [{} nodes ({} iterations) in {:.3}s]",
                nodes, iters, time
            )
            .unwrap();
        }
        self.eng_status.set(&text);
    }

    /// Sets the engine to indicate it is not enabled.
    pub fn set_engine_disabled(&mut self) {
        self.eng_matrix.clear();
        self.eng_ghost.clear();
        self.clear_engine_suggestion();
        self.eng_status.set("engine not enabled");
    }

    /// Renders any labels whos text has recently been updated, and needs to be
    /// rendered. This function should be called whenever components of the view were
    /// possibly updated.
    pub fn render_labels(&mut self, tc: &'r TextureCreator) {
        let colors = &self.colors;
        let hud_font = &self.resources.hud_font;
        let hud_font_bold = &self.resources.hud_font_bold;
        let hud_font_small = &self.resources.hud_font_small;
        let hud_font_small_bold = &self.resources.hud_font_small_bold;

        self.motd.render(tc, hud_font_bold, colors.text.0);
        for lbl in self.stats.iter_mut() {
            lbl.render(tc, hud_font, colors.text.0);
        }
        for (hd, lns) in self.controls.iter_mut() {
            hd.render(tc, hud_font_bold, colors.text.0);
            for ln in lns.iter_mut() {
                ln.render(tc, hud_font, colors.text.0);
            }
        }

        self.eng_overlay[0].render(tc, hud_font_small_bold, colors.text.0);
        self.eng_overlay[1].render(tc, hud_font_small_bold, colors.text.0);
        self.eng_overlay[2].render(tc, hud_font_small_bold, colors.text.0);
        self.eng_status.render(tc, hud_font_small, colors.text.1);

        let piece_rating_color = if self.piece_rating.2 {
            colors.text.0
        } else {
            colors.text.1
        };
        self.piece_rating.0.render(tc, hud_font, piece_rating_color);
        self.piece_rating.1.render(tc, hud_font, piece_rating_color);

        let progress_font = &self.resources.progress_font;
        let progress_color = if self.progress.1 {
            colors.good_bad.0
        } else {
            colors.text.0
        };
        self.progress.0.render(tc, progress_font, progress_color);
    }

    /// Paints this view onto SDL2 canvas `cv`.
    pub fn paint(&self, cv: &mut Canvas) {
        cv.set_draw_color(self.colors.background);
        cv.clear();

        // draw matrix backgrounds
        cv.set_draw_color(self.colors.grid_background.0);
        cv.fill_rect(self.geom.matrix).unwrap();
        cv.fill_rect(self.geom.eng_matrix).unwrap();
        // checkerboard
        cv.set_draw_color(self.colors.grid_background.1);
        for i in 0..self.ruleset.visible_rows {
            for j in 0..self.ruleset.cols {
                if (i + j) % 2 == 0 {
                    continue;
                }
                let (i, j) = (i as u16, j as u16);
                cv.fill_rect(self.geom.main_cell(i, j)).unwrap();
                cv.fill_rect(self.geom.eng_cell(i, j)).unwrap();
            }
        }

        // draw matrix cells
        self.queue.paint(cv, &self.colors, CellStyle::Solid);
        self.matrix.paint(cv, &self.colors, CellStyle::Solid);
        self.ghost.paint(cv, &self.colors, CellStyle::Outline);
        self.piece.paint(cv, &self.colors, CellStyle::Solid);
        self.eng_matrix.paint(cv, &self.colors, CellStyle::Solid);
        self.eng_ghost.paint(cv, &self.colors, CellStyle::Outline);

        // draw left-side HUD labels, which need some extra machinery to calculate their
        // positions. `group` is the index of the current label group, and `line` is the
        // line number for the next label.
        let (mut group, mut line) = (0, 0);
        self.motd.paint(cv, self.geom.hud(group, line));
        line += 1;
        group += 1;

        for lbl in self.stats.iter() {
            lbl.paint(cv, self.geom.hud(group, line));
            line += 1;
        }
        group += 1;

        for (hd, lns) in self.controls.iter() {
            hd.paint(cv, self.geom.hud(group, line));
            line += 1;
            for ln in lns.iter() {
                ln.paint(cv, self.geom.hud(group, line));
                line += 1;
            }
            group += 1;
        }

        // draw other labels
        let progress = &self.progress.0;
        progress.paint(cv, self.geom.progress(progress));
        for (i, lbl) in self.eng_overlay.iter().enumerate() {
            lbl.paint(cv, self.geom.engine_overlay(i));
        }
        self.piece_rating.0.paint(cv, self.geom.piece_rating(0));
        self.piece_rating.1.paint(cv, self.geom.piece_rating(1));
        self.eng_status.paint(cv, self.geom.engine_status());
    }
}

/// Represents a collection of colored cells to be drawn. Helps save on allocations and
/// drawing calls.
struct Cells {
    rows: u16,
    rects: HashMap<CellColor, Vec<Rect>>,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum CellStyle {
    Solid,
    Outline,
}

impl Cells {
    /// Construct a new `Cells`. If given, `rows` specifies the max number of rows
    /// displayed, and cells above that row will not be displayed.
    fn new(rows: Option<u16>) -> Self {
        Self {
            rows: rows.unwrap_or(std::u16::MAX),
            rects: HashMap::with_capacity(32),
        }
    }

    fn clear(&mut self) {
        for rs in self.rects.values_mut() {
            rs.clear();
        }
    }

    /// Insert cells to be drawn, where `cells` specifies the coordinate and colors of the
    /// cells, and `geom` computes the rectangle for each cell.
    fn insert<I, G>(&mut self, cells: I, geom: G)
    where
        I: IntoIterator<Item = ((u16, u16), CellColor)>,
        G: Fn(u16, u16) -> Rect,
    {
        for ((i, j), cc) in cells {
            if i < self.rows {
                self.rects.entry(cc).or_default().push(geom(i, j));
            }
        }
    }

    /// Paints these cells onto canvas `cv`, using `colors` to deterimne the color of each
    /// cell, and `style` to determine how the cells are drawn.
    fn paint(&self, cv: &mut Canvas, colors: &Colors, style: CellStyle) {
        for (&cc, rects) in self.rects.iter() {
            if rects.is_empty() {
                continue;
            }
            if let Some(&color) = colors.cell.get(&cc) {
                cv.set_draw_color(color);
                for &rect in rects.iter() {
                    match style {
                        CellStyle::Solid => cv.fill_rect(rect),
                        CellStyle::Outline => cv.draw_rect(rect),
                    }
                    .unwrap();
                }
            }
        }
    }
}

/// Represents a label to be drawn. Helps save on allocations and font rendering
/// operations.
struct Label<'a> {
    text: String,
    texture: Option<Texture<'a>>,
    width: u32,
    height: u32,
}

impl<'a> Label<'a> {
    /// Constructs a new empty label.
    fn new() -> Self {
        Self {
            text: String::new(),
            texture: None,
            width: 0,
            height: 0,
        }
    }

    /// Clears the text on this label (equivalent to `self.set("");`).
    fn clear(&mut self) {
        self.text.clear();
        self.texture = None;
        self.width = 0;
        self.height = 0;
    }

    /// Sets the label text to the provided string.
    fn set(&mut self, s: &str) {
        if s != self.text {
            self.clear();
            self.text.push_str(s);
        }
    }

    /// Renders the label so that its text will be drawn on the next call to
    /// `paint`. Returns `true` if the text had to be re-rendered from the given font,
    /// `false` if not since the text had not changed since the last call to `render()`.
    fn render(
        &mut self,
        tc: &'a TextureCreator,
        font: &sdl2::ttf::Font<'a, 'static>,
        color: Color,
    ) -> bool {
        if self.texture.is_some() || self.text.is_empty() {
            return false;
        }
        let surf = font
            .render(&self.text)
            .blended(color)
            .expect("text render failed");
        let texture = tc
            .create_texture_from_surface(&surf)
            .expect("texture creation failed");
        let query = texture.query();
        self.width = query.width;
        self.height = query.height;
        self.texture = Some(texture);
        true
    }

    /// Paints this label onto canvas `cv` with top-left origin `(x, y)`.
    fn paint(&self, cv: &mut Canvas, (x, y): (i32, i32)) {
        if let Some(texture) = self.texture.as_ref() {
            let rect = (x, y, self.width, self.height).into();
            cv.copy(texture, None, Some(rect)).unwrap();
        }
    }
}

/// Caches geometry computations for the on-screen elements.
#[derive(Clone)]
struct Geometry {
    // number of visible rows/cols
    rows: u32,
    cols: u32,
    // line height for primary font
    line_height: i32,
    // line height for small font
    line_height_small: i32,
    // text padding for primary font
    text_pad: i32,
    // text padding for small font
    text_pad_small: i32,
    // position of the bottom of the window
    bottom: i32,
    // extent of the main matrix
    matrix: Rect,
    // size of main matrix cells
    cell: u32,
    // extent of the engine matrix
    eng_matrix: Rect,
    // size of engine matrix cells
    eng_cell: u32,
}

impl Geometry {
    fn new(rows: u16, cols: u16, win: (u32, u32)) -> Self {
        let mut geom = Self {
            // constant
            rows: rows as u32,
            cols: cols as u32,
            // TODO: derive these from Resources
            line_height: 19,
            line_height_small: 15,
            text_pad: 6,
            text_pad_small: 3,
            // calculated on every resize
            bottom: 0,
            cell: 0,
            eng_cell: 0,
            matrix: Rect::new(0, 0, 0, 0),
            eng_matrix: Rect::new(0, 0, 0, 0),
        };
        geom.set(win);
        geom
    }

    fn set(&mut self, (win_w, win_h): (u32, u32)) {
        self.bottom = win_h as i32;
        self.cell = (win_h * 3 / 4) / self.rows;
        self.eng_cell = self.cell / 2;

        let main_w = self.cell * self.cols;
        let main_h = self.cell * self.rows;
        let main_x = (win_w as i32 - main_w as i32) / 2;
        let main_y = (win_h as i32 - main_h as i32) / 2 - self.line_height_small;
        self.matrix = (main_x, main_y, main_w, main_h).into();

        let eng_w = self.eng_cell * self.cols;
        let eng_h = self.eng_cell * self.rows;
        let eng_x = main_x - (self.cell as i32) - eng_w as i32;
        let eng_y = main_y + main_h as i32 - eng_h as i32;
        self.eng_matrix = (eng_x, eng_y, eng_w, eng_h).into();
    }

    fn cell(&self, x0: i32, y0: i32, size: u32, i: u16, j: u16) -> Rect {
        Rect::new(
            x0 + (j as i32) * (size as i32),
            y0 - ((i + 1) as i32) * (size as i32),
            size,
            size,
        )
    }

    fn main_cell(&self, i: u16, j: u16) -> Rect {
        let (x0, y0) = (self.matrix.left(), self.matrix.bottom());
        self.cell(x0, y0, self.cell, i, j)
    }

    fn hold_cell(&self, i: u16, j: u16) -> Rect {
        let x0 = self.matrix.left() - (self.cell as i32) * 5;
        let y0 = self.matrix.top() + (self.cell as i32) * 3;
        self.cell(x0, y0, self.cell, i, j)
    }

    fn next_cell(&self, idx: usize, i: u16, j: u16) -> Rect {
        let x0 = self.matrix.right() + (self.cell as i32);
        let mut y0 = self.matrix.top();
        y0 += (idx as i32 + 1) * (self.cell as i32) * 3;
        self.cell(x0, y0, self.cell, i, j)
    }

    fn eng_cell(&self, i: u16, j: u16) -> Rect {
        let (x0, y0) = (self.eng_matrix.left(), self.eng_matrix.bottom());
        self.cell(x0, y0, self.eng_cell, i, j)
    }

    fn hud(&self, group: usize, line: usize) -> (i32, i32) {
        let (x, mut y) = (self.text_pad, self.text_pad);
        y += (line as i32) * self.line_height;
        y += (group as i32) * self.line_height;
        (x, y)
    }

    fn progress(&self, label: &Label) -> (i32, i32) {
        let mut x = self.matrix.right() + (self.cell as i32) * 3;
        let mut y = self.matrix.bottom() - self.cell as i32;
        let min_dx = (self.cell as i32) * 3 - self.text_pad;
        x -= std::cmp::min(min_dx, (label.width as i32) / 2);
        y -= label.height as i32;
        (x, y)
    }

    fn piece_rating(&self, line: usize) -> (i32, i32) {
        let x = self.matrix.left();
        let mut y = self.matrix.bottom() + self.text_pad;
        y += (line as i32) * self.line_height;
        (x, y)
    }

    fn engine_overlay(&self, line: usize) -> (i32, i32) {
        let x = self.eng_matrix.left() + self.text_pad_small;
        let mut y = self.eng_matrix.top() + self.text_pad_small;
        y += (line as i32) * self.line_height_small;
        (x, y)
    }

    fn engine_status(&self) -> (i32, i32) {
        let x = self.text_pad;
        let y = self.bottom - self.line_height_small - self.text_pad;
        (x, y)
    }
}

/// Stores the colors to use for a particular theme.
struct Colors {
    background: Color,
    grid_background: (Color, Color),
    text: (Color, Color),
    good_bad: (Color, Color),
    cell: HashMap<CellColor, Color>,
}

impl Colors {
    fn dark_theme() -> Self {
        let fmt_rgb24 = {
            use sdl2::pixels::{PixelFormat, PixelFormatEnum};
            use std::convert::TryFrom;
            PixelFormat::try_from(PixelFormatEnum::RGB888).unwrap()
        };
        let rgb24 = |v| Color::from_u32(&fmt_rgb24, v);

        let mut cell = HashMap::new();
        cell.insert('G', rgb24(0x888888));
        cell.insert('H', rgb24(0x666666));
        cell.insert('L', rgb24(0xff9900));
        cell.insert('O', rgb24(0xffff00));
        cell.insert('I', rgb24(0x00ffff));
        cell.insert('J', rgb24(0x0022ff));
        cell.insert('S', rgb24(0x00ff00));
        cell.insert('Z', rgb24(0xff0000));
        cell.insert('T', rgb24(0x990099));

        Self {
            background: rgb24(0x2e2e2e),
            grid_background: (rgb24(0), rgb24(0x222222)),
            text: (rgb24(0xeeeeee), rgb24(0x888888)),
            good_bad: (rgb24(0x55ff55), rgb24(0xff3333)),
            cell,
        }
    }
}
