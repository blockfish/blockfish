use crate::resources::Resources;
use block_stacker::Ruleset;
use sdl2::{
    pixels::Color,
    rect::Rect,
    render::{Canvas, RenderTarget, Texture},
};
use std::{collections::HashMap, rc::Rc};

//////////////////////////////////////////////////////////////////////////////////////////
// View

/// Encapsulates the view logic.
pub struct View<'h> {
    rules: Rc<Ruleset>,
    resources: Resources<'h>,
    texture_creator: &'h TextureCreator,
    theme: Theme,
    geom: Geometry,
    matrix: MatrixCells,
    piece: PieceCells,
    ghost: PieceCells,
    suggested: PieceCells,
    hold: PieceCells,
    next: Vec<PieceCells>,
    hud: Vec<Label<'h>>,
}

/// Contains information to be displayed by the HUD labels.
#[derive(Clone, Debug)]
pub struct LabelInfo {
    pub pace: PaceLabelInfo,
    pub engine: EngineLabelInfo,
}

/// HUD label info related to the user's downstacking pace.
#[derive(Clone, Debug)]
pub struct PaceLabelInfo {
    pub pieces: u32,
    pub cleared: u32,
    pub downstack: u32,
    pub dpp: Option<f32>,
    pub pace: Option<u32>,
}

/// HUD label info related to the engine status.
#[derive(Clone, Debug)]
pub enum EngineLabelInfo {
    Thinking,
    Suggesting {
        index: usize,
        total: usize,
        score: i64,
        base_eval: Option<i64>,
        piece_eval: Option<i64>,
    },
    #[allow(dead_code)]
    Failed,
}

type TextureCreator = sdl2::render::TextureCreator<sdl2::video::WindowContext>;

impl<'h> View<'h> {
    /// Construct a new view with given ruleset and resource data.
    pub fn new(
        rules: Rc<Ruleset>,
        resources: Resources<'h>,
        canvas: &Canvas<sdl2::video::Window>,
        texture_creator: &'h TextureCreator,
    ) -> Self {
        let mut view = View {
            rules,
            resources,
            texture_creator,
            theme: Default::default(),
            geom: Default::default(),
            matrix: MatrixCells::new(),
            piece: PieceCells::new(),
            ghost: PieceCells::new(),
            suggested: PieceCells::new(),
            hold: PieceCells::new(),
            next: Vec::with_capacity(6),
            hud: Vec::with_capacity(20),
        };
        view.set_size(canvas.window().size());
        view
    }

    /// Resizes the view to fit the window dimensions `(w, h)`.
    pub fn set_size(&mut self, (w, h): (u32, u32)) {
        self.geom = Geometry::new(&self.rules, &self.resources, w, h);
    }

    /// Sets the contents of the matrix to be `cells`, given by each `(coord, color)`
    /// pair.
    pub fn set_cells(&mut self, cells: impl IntoIterator<Item = ((u16, u16), char)>) {
        self.matrix.clear();
        for (coord, color) in cells {
            self.matrix.add(color, coord);
        }
    }

    /// Sets the contents of the next queue.
    pub fn set_next(&mut self, pcs: impl IntoIterator<Item = char>) {
        for (i, color) in pcs.into_iter().enumerate() {
            if i == self.next.len() {
                self.next.push(PieceCells::new());
            }
            self.next[i].set(color, self.rules.coords(color, 0));
        }
    }

    /// Sets the hold piece.
    pub fn set_hold(&mut self, color: char) {
        self.hold.set(color, self.rules.coords(color, 0));
    }

    /// Clears the hold piece.
    pub fn clear_hold(&mut self) {
        self.hold.clear();
    }

    /// Sets the current piece and the ghost piece's row.
    pub fn set_piece(&mut self, color: char, row: i16, col: i16, rot: i32, ghost_row: i16) {
        let rules = &self.rules;
        let get_coords = move |row| {
            rules.coords(color, rot).map(move |(i, j)| {
                let i = ((i as i16) + row) as u16;
                let j = ((j as i16) + col) as u16;
                (i, j)
            })
        };

        let coords = get_coords(row);
        let ghost_coords = get_coords(ghost_row);
        self.piece.set(color, coords);
        self.ghost.set(color, ghost_coords);
    }

    /// Sets the suggested piece.
    pub fn set_suggested(&mut self, color: char, row: i16, col: i16, rot: i32) {
        let rules = &self.rules;
        let coords = rules.coords(color, rot).map(move |(i, j)| {
            let i = ((i as i16) + row) as u16;
            let j = ((j as i16) + col) as u16;
            (i, j)
        });
        self.suggested.set(color, coords);
    }

    /// Removes the suggested piece.
    pub fn clear_suggested(&mut self) {
        self.suggested.clear();
    }

    /// Sets the HUD label text based on the provided info.
    pub fn set_hud_labels(&mut self, info: LabelInfo) {
        use self::TextStyle::*;
        use crate::util::text_fmt::*;

        // label!(...) sets the next label text according to a format string.
        let mut count = 0;
        let mut set_next = |style: TextStyle, args: std::fmt::Arguments| {
            if count >= self.hud.len() {
                self.hud.push(Label::new());
            }
            count += 1;
            self.hud[count - 1].set(style, args);
        };

        macro_rules! label {
            ($s:expr, $($fmt:tt)*) => {{
                set_next($s, format_args!($($fmt)*))
            }}
        }

        label!(Normal, "{}", plural(info.pace.pieces, "piece"));
        label!(Normal, "{}", plural(info.pace.downstack, "garbage line"));
        label!(Normal, "{}", plural(info.pace.cleared, "total line"));
        label!(Normal, "{} dpp", maybe_f32(info.pace.dpp, "?"));
        label!(Normal, "{} pace", maybe(info.pace.pace, "?"));
        label!(Normal, "");

        label!(Highlight, "* engine enabled *");
        match info.engine {
            EngineLabelInfo::Thinking => label!(Normal, "thinking ..."),
            EngineLabelInfo::Failed => {
                label!(Bad, "failed :(");
            }
            EngineLabelInfo::Suggesting {
                index,
                total,
                score,
                base_eval,
                piece_eval,
            } => {
                label!(Normal, "suggestion {} of {}", index + 1, total);
                if let Some(base_eval) = base_eval {
                    label!(Normal, "base eval: {}", base_eval);
                    let score_diff = score - base_eval;
                    label!(goodbad(score_diff), "suggestion: {:+}", score_diff);
                    if let Some(piece_score) = piece_eval {
                        let piece_diff = piece_score - base_eval;
                        label!(goodbad(piece_diff), "this piece: {:+}", piece_diff);
                    }
                }
            }
        }

        for (i, label) in self.hud.iter_mut().enumerate() {
            if i >= count {
                label.clear();
            } else {
                label.repaint(
                    &self.texture_creator,
                    &self.theme.text,
                    &self.resources.hud_font,
                );
            }
        }
    }

    /// Displays the view onto the canvas `dc`.
    pub fn draw(&self, dc: &mut Canvas<impl RenderTarget>) {
        let geom = &self.geom;
        let rows = self.rules.visible_rows as u16;
        let solid_colors = &self.theme.cell_colors;
        let ghost_colors = &self.theme.ghost_colors;

        dc.set_blend_mode(sdl2::render::BlendMode::Blend);
        dc.set_draw_color(self.theme.background);
        dc.clear();

        self.matrix.draw(
            SolidCanvasDraw(dc, solid_colors),
            MatrixCellGeom(geom, rows),
        );

        self.suggested.draw(
            OutlineCanvasDraw(dc, ghost_colors),
            MatrixCellGeom(geom, rows),
        );

        self.ghost.draw(
            SolidCanvasDraw(dc, ghost_colors),
            MatrixCellGeom(geom, rows),
        );

        self.piece.draw(
            SolidCanvasDraw(dc, solid_colors),
            MatrixCellGeom(geom, rows),
        );

        dc.set_draw_color(self.theme.matrix_border);
        dc.draw_rect(self.geom.matrix_rect().into())
            .expect("border failed");

        self.hold
            .draw(SolidCanvasDraw(dc, solid_colors), HoldCellGeom(geom));

        for (i, pc) in self.next.iter().enumerate() {
            pc.draw(SolidCanvasDraw(dc, solid_colors), NextCellGeom(geom, i));
        }

        for (i, label) in self.hud.iter().enumerate() {
            label.draw(self.geom.hud_text_pos(i), dc);
        }
    }
}

/// Facilitates drawing a matrix consisting of many cells of various colors.
#[derive(Clone)]
struct MatrixCells(HashMap<char, Vec<(u16, u16)>>);

impl MatrixCells {
    fn new() -> Self {
        Self(HashMap::with_capacity(10))
    }

    fn clear(&mut self) {
        for coords in self.0.values_mut() {
            coords.clear();
        }
    }

    fn add(&mut self, color: char, coord: (u16, u16)) {
        self.0.entry(color).or_default().push(coord);
    }

    fn draw(&self, mut cs: impl CellStyle, cg: impl CellGeom) {
        for (&color, coords) in self.0.iter() {
            cs.draw_cells(&cg, color, &coords);
        }
    }
}

/// Facilitates drawing a piece with a few cells and a single color.
#[derive(Clone)]
struct PieceCells(char, Vec<(u16, u16)>);

impl PieceCells {
    fn new() -> Self {
        Self('G', Vec::with_capacity(4))
    }

    fn clear(&mut self) {
        self.1.clear();
    }

    fn set(&mut self, color: char, coords: impl IntoIterator<Item = (u16, u16)>) {
        self.0 = color;
        self.1.clear();
        self.1.extend(coords);
    }

    fn draw(&self, mut cs: impl CellStyle, cg: impl CellGeom) {
        cs.draw_cells(&cg, self.0, &self.1);
    }
}

/// Trait for drawing cells of different colors.
trait CellStyle {
    fn prepare(&mut self, color: char) -> bool;
    fn draw_rect(&mut self, rect: Rect);

    fn draw_cells(&mut self, geom: &impl CellGeom, color: char, cells: &[(u16, u16)]) {
        if cells.is_empty() {
            return;
        }
        if !self.prepare(color) {
            return;
        }
        for &coord in cells.iter() {
            if let Some(rect) = geom.cell_rect(coord) {
                self.draw_rect(rect);
            }
        }
    }
}

struct SolidCanvasDraw<'dc, 'th, T: RenderTarget>(&'dc mut Canvas<T>, &'th HashMap<char, Color>);

impl<'dc, 'th, T: RenderTarget> CellStyle for SolidCanvasDraw<'dc, 'th, T> {
    fn prepare(&mut self, color: char) -> bool {
        if let Some(&color) = self.1.get(&color) {
            self.0.set_draw_color(color);
            true
        } else {
            false
        }
    }

    fn draw_rect(&mut self, rect: Rect) {
        self.0.fill_rect(Some(rect)).expect("fill failed");
    }
}

struct OutlineCanvasDraw<'dc, 'th, T: RenderTarget>(&'dc mut Canvas<T>, &'th HashMap<char, Color>);

impl<'dc, 'th, T: RenderTarget> CellStyle for OutlineCanvasDraw<'dc, 'th, T> {
    fn prepare(&mut self, color: char) -> bool {
        if let Some(&color) = self.1.get(&color) {
            self.0.set_draw_color(color);
            true
        } else {
            false
        }
    }

    fn draw_rect(&mut self, rect: Rect) {
        self.0.draw_rect(rect).expect("outline failed");
    }
}

/// Trait for computing cell geometry from a coordinate.
trait CellGeom {
    fn cell_rect(&self, coord: (u16, u16)) -> Option<Rect>;
}

struct MatrixCellGeom<'a>(&'a Geometry, u16);
impl<'a> CellGeom for MatrixCellGeom<'a> {
    fn cell_rect(&self, coord: (u16, u16)) -> Option<Rect> {
        if coord.0 < self.1 {
            Some(self.0.matrix_cell_rect(coord).into())
        } else {
            None
        }
    }
}

struct NextCellGeom<'a>(&'a Geometry, usize);
impl<'a> CellGeom for NextCellGeom<'a> {
    fn cell_rect(&self, coord: (u16, u16)) -> Option<Rect> {
        Some(self.0.next_cell_rect(self.1, coord).into())
    }
}

struct HoldCellGeom<'a>(&'a Geometry);
impl<'a> CellGeom for HoldCellGeom<'a> {
    fn cell_rect(&self, coord: (u16, u16)) -> Option<Rect> {
        Some(self.0.hold_cell_rect(coord).into())
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Labels

struct Label<'tx> {
    buf: String,
    tmp: String,
    texture: Option<Texture<'tx>>,
    width: u32,
    height: u32,
    style: TextStyle,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum TextStyle {
    Normal,
    Highlight,
    Good,
    Bad,
}

impl Default for TextStyle {
    fn default() -> Self {
        TextStyle::Normal
    }
}

fn goodbad(x: i64) -> TextStyle {
    if x < 0 {
        TextStyle::Good
    } else {
        TextStyle::Bad
    }
}

impl<'tx> Label<'tx> {
    fn new() -> Self {
        Label {
            buf: String::with_capacity(80),
            tmp: String::with_capacity(80),
            texture: None,
            width: 0,
            height: 0,
            style: TextStyle::Normal,
        }
    }

    fn clear(&mut self) {
        self.buf.clear();
        self.texture = None;
    }

    fn set(&mut self, style: TextStyle, text: impl std::fmt::Display) {
        use std::fmt::Write;
        self.style = style;
        self.tmp.clear();
        write!(&mut self.tmp, "{}", text).unwrap();
        if self.tmp != self.buf {
            self.buf.clone_from(&self.tmp);
            self.texture = None;
        }
    }

    fn repaint(
        &mut self,
        tc: &'tx TextureCreator,
        theme: &TextTheme,
        font: &sdl2::ttf::Font<'tx, 'static>,
    ) {
        if self.texture.is_none() && self.buf.len() > 0 {
            let surf = font
                .render(&self.buf)
                .blended(match self.style {
                    TextStyle::Normal => theme.normal,
                    TextStyle::Highlight => theme.highlight,
                    TextStyle::Good => theme.good,
                    TextStyle::Bad => theme.bad,
                })
                .expect("text render failed");
            let texture = tc
                .create_texture_from_surface(&surf)
                .expect("texture creation failed");
            let query = texture.query();
            self.width = query.width;
            self.height = query.height;
            self.texture = Some(texture);
        }
    }

    fn draw(&self, (x, y): (i32, i32), dc: &mut Canvas<impl RenderTarget>) {
        if let Some(tx) = self.texture.as_ref() {
            let dst = (x, y, self.width, self.height);
            dc.copy(tx, None, Some(dst.into())).expect("draw failed");
        }
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Geometry

#[derive(Default, Clone)]
struct Geometry {
    rows: u32,
    cols: u32,
    cell_size: u32,
    mat_top: i32,
    mat_left: i32,
    hold_left: i32,
    hold_top: i32,
    next_top: i32,
    next_left: i32,
    next_offset: i32,
    hud_top: i32,
    hud_left: i32,
    hud_offset: i32,
}

const PADDING: u32 = 10;

impl Geometry {
    fn new(rules: &Ruleset, res: &Resources, w: u32, h: u32) -> Self {
        let cols = rules.cols as u32;
        let rows = rules.visible_rows as u32;

        let cell_size = std::cmp::min((w - PADDING * 2) / (cols + 10), (h - PADDING * 2) / rows);

        // hold piece
        let hold_left = PADDING as i32;
        let hold_top = PADDING as i32;

        // matrix
        let mat_top = PADDING as i32;
        let mat_left = hold_left + (cell_size * 5) as i32;

        // next queue
        let next_top = PADDING as i32;
        let next_left = mat_left + (cell_size * (cols + 1)) as i32;
        let next_offset = (cell_size * 3) as i32;

        // hud
        let hud_top = PADDING as i32;
        let hud_left = next_left + (cell_size * 5) as i32;
        let hud_offset = res.hud_font.height() + 2;

        Self {
            rows,
            cols,
            cell_size,
            mat_top,
            mat_left,
            hold_left,
            hold_top,
            next_top,
            next_left,
            next_offset,
            hud_top,
            hud_left,
            hud_offset,
        }
    }

    fn matrix_rect(&self) -> (i32, i32, u32, u32) {
        let x = self.mat_left as i32;
        let y = self.mat_top as i32;
        (x, y, self.cell_size * self.cols, self.cell_size * self.rows)
    }

    fn matrix_cell_rect(&self, (row, col): (u16, u16)) -> (i32, i32, u32, u32) {
        let x = self.mat_left + ((col as u32) * self.cell_size) as i32;
        let y = self.mat_top + ((self.rows - (row as u32) - 1) * self.cell_size) as i32;
        (x, y, self.cell_size, self.cell_size)
    }

    fn hold_cell_rect(&self, (row, col): (u16, u16)) -> (i32, i32, u32, u32) {
        let x = self.hold_left + ((col as u32) * self.cell_size) as i32;
        let y = self.hold_top + ((3 - (row as u32) - 1) * self.cell_size) as i32;
        (x, y, self.cell_size, self.cell_size)
    }

    fn next_cell_rect(&self, i: usize, (row, col): (u16, u16)) -> (i32, i32, u32, u32) {
        let x = self.next_left + ((col as u32) * self.cell_size) as i32;
        let y = self.next_top + ((3 - (row as u32)) * self.cell_size) as i32;
        let y = y + (i as i32) * (self.next_offset as i32);
        (x, y, self.cell_size, self.cell_size)
    }

    fn hud_text_pos(&self, i: usize) -> (i32, i32) {
        let x = self.hud_left;
        let y = self.hud_top + (i as i32) * self.hud_offset;
        (x, y)
    }
}

//////////////////////////////////////////////////////////////////////////////////////////
// Theme

/// Holds configuration parameters for the theme, particularly the colors of various
/// objects to be drawn.
#[derive(Clone, Debug)]
pub struct Theme {
    pub background: Color,
    pub matrix_border: Color,
    pub cell_colors: HashMap<char, Color>,
    pub ghost_colors: HashMap<char, Color>,
    pub text: TextTheme,
}

/// Holds configuration for just the text colors.
#[derive(Clone, Debug)]
pub struct TextTheme {
    normal: Color,
    highlight: Color,
    good: Color,
    bad: Color,
}

impl Default for Theme {
    fn default() -> Self {
        let fmt_rgb24 = {
            use sdl2::pixels::{PixelFormat, PixelFormatEnum};
            use std::convert::TryFrom;
            PixelFormat::try_from(PixelFormatEnum::RGB888).unwrap()
        };
        let rgb24 = |v| Color::from_u32(&fmt_rgb24, v);

        let mut cell_colors = HashMap::new();
        cell_colors.insert('G', rgb24(0x999999)); // #999
        cell_colors.insert('H', rgb24(0x666666)); // #666
        cell_colors.insert('L', rgb24(0xff9900)); // #f90
        cell_colors.insert('O', rgb24(0xffff00)); // #ff0
        cell_colors.insert('I', rgb24(0x00ffff)); // #0ff
        cell_colors.insert('J', rgb24(0x0022ff)); // #00f
        cell_colors.insert('S', rgb24(0x00ff00)); // #0f0
        cell_colors.insert('Z', rgb24(0xff0000)); // #f00
        cell_colors.insert('T', rgb24(0x990099)); // #909

        let mut ghost_colors = cell_colors.clone();
        for color in ghost_colors.values_mut() {
            let (r, g, b) = color.rgb();
            *color = Color::RGBA(r, g, b, 128);
        }

        let text = TextTheme {
            normal: rgb24(0x888888),    // #888
            highlight: rgb24(0xcccccc), // #ccc
            good: rgb24(0x44aa44),      // #4a4
            bad: rgb24(0xcc4444),       // #c44
        };

        Self {
            background: rgb24(0x111111),    // #111
            matrix_border: rgb24(0x333333), // #333
            cell_colors,
            ghost_colors,
            text,
        }
    }
}
