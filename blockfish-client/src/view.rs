use crate::{
    controls::{self, Action, Controls},
    resources::Resources,
    theme::{Colors, Theme},
    timer::Timer,
};
use block_stacker::{CellColor, PieceType, Ruleset};
use sdl2::{pixels::Color, rect::Point, rect::Rect, render::Texture};
use std::{collections::HashMap, ops::Range, rc::Rc};

/// Default window size for view.
pub static DEFAULT_SIZE: (u32, u32) = (1200, 720);

/// Pixels to scroll for every 'tick' of the scroll wheel.
pub static SCROLL_WHEEL_SPEED: i32 = 13;

/// Holds all of the graphical elements to draw on the view, and where to draw them.
pub struct View<'r> {
    resources: Resources<'r>,
    ruleset: Rc<Ruleset>,
    colors: Colors,
    geom: Geometry,
    controls: Controls,
    queue: Cells,
    matrix: Cells,
    piece: Cells,
    ghost: Cells,
    eng_matrix: Cells,
    eng_ghost: Cells,
    motd: Label<'r>,
    stats: Vec<Label<'r>>,
    help: [Vec<Label<'r>>; 2],
    progress: (Label<'r>, bool),
    eng_overlay: [Label<'r>; 4],
    eng_status: Label<'r>,
    tree_sidebar: Option<TreeSidebar<'r>>,
}

/// Data for entries in the tree sidebar.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TreeNode {
    pub piece: PieceType,
    pub depth: usize,
    pub count: usize,
    pub rating: Option<i64>,
    pub best_rating: i64,
}

/// Represents window close "event".
///
/// NOTE: used in `Err` return values in `View::handle`, but `Quit` is not really an
/// error, just a signal to close the application.
#[derive(Copy, Clone, Debug)]
pub struct Quit;

/// Canvas type that `View` is able to paint to.
pub type Canvas = sdl2::render::Canvas<sdl2::video::Window>;

/// Used to build `Texture`'s; annoying artefact of SDL2 API.
pub type TextureCreator = sdl2::render::TextureCreator<sdl2::video::WindowContext>;

impl<'r> View<'r> {
    /// Constructs a new view.
    pub fn new(
        resources: Resources<'r>,
        ruleset: Rc<Ruleset>,
        controls: Controls,
        theme: &Theme,
        version_string: &str,
    ) -> Self {
        let rows = ruleset.visible_rows as u16;
        let cols = ruleset.cols as u16;

        let mut motd = Label::new();
        let mut help = [vec![], vec![]];
        Self::set_motd(&mut motd, version_string);
        Self::set_help_labels_controls(&mut help, &controls);

        Self {
            resources,
            ruleset,
            controls,
            motd,
            help,
            colors: theme.to_colors(),
            geom: Geometry::new(rows, cols, DEFAULT_SIZE),
            queue: Cells::new(None),
            matrix: Cells::new(Some(rows)),
            piece: Cells::new(Some(rows)),
            ghost: Cells::new(Some(rows)),
            eng_matrix: Cells::new(Some(rows)),
            eng_ghost: Cells::new(Some(rows)),
            stats: Vec::with_capacity(4),
            progress: (Label::new(), false),
            eng_status: Label::new(),
            eng_overlay: [Label::new(), Label::new(), Label::new(), Label::new()],
            tree_sidebar: None,
        }
    }

    /// Generates the version-string heading text, setting `label` appropriately.
    fn set_motd(label: &mut Label<'r>, version_string: &str) {
        label.set(&format!("Blockfish {}", version_string));
    }

    /// Generates help text labels to display help for control scheme `controls`, setting
    /// the `Label`'s in `help` appropriately.
    fn set_help_labels_controls(help: &mut [Vec<Label<'r>>], controls: &Controls) {
        use crate::controls::{EngineOp::*, GameOp::*};

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
        let game_ctrls = &mut help[0];
        game_ctrls.resize_with(7, Label::new);
        game_ctrls[0].set("game controls");
        game_ctrls[1].set(&label_text("\u{2190}, \u{2192}:         ", left_right));
        game_ctrls[2].set(&label_text("ccw, cw:      ", ccw_cw));
        game_ctrls[3].set(&label_text("sd, hd:       ", sd_hd));
        game_ctrls[4].set(&label_text("hold:         ", hold));
        game_ctrls[5].set(&label_text("undo:         ", undo));
        game_ctrls[6].set(&label_text("reset:        ", reset));

        let toggle = &[Action::Engine(Toggle)];
        let switch = &[Action::Engine(Next), Action::Engine(Prev)];
        let step = &[Action::Engine(StepForward), Action::Engine(StepBackward)];
        let go_to = &[Action::Engine(Goto)];
        let eng_ctrls = &mut help[1];
        eng_ctrls.resize_with(5, Label::new);
        eng_ctrls[0].set("engine controls");
        eng_ctrls[1].set(&label_text("toggle:       ", toggle));
        eng_ctrls[2].set(&label_text("switch sugg:  ", switch));
        eng_ctrls[3].set(&label_text("step sugg:    ", step));
        eng_ctrls[4].set(&label_text("go to sugg:   ", go_to));
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

    /// Sets or clears the user's current piece. If given `Some((ty, i, j, r, g))`, set
    /// piece type `ty` at row `i`, column `j`, rotation `r`, with ghost piece at row `g`.
    pub fn set_piece(&mut self, piece: Option<(PieceType, i16, i16, i32, i16)>) {
        self.piece.clear();
        self.ghost.clear();
        if let Some((ty, i, j, r, g)) = piece {
            let ruleset = self.ruleset.as_ref();
            let cells = |i: i16| {
                ruleset.coords(ty, r).map(move |(i_off, j_off)| {
                    let i = (i + i_off as i16) as u16;
                    let j = (j + j_off as i16) as u16;
                    ((i, j), ty)
                })
            };
            let geom = &self.geom;
            self.piece.insert(cells(i), |i, j| geom.main_cell(i, j));
            self.ghost.insert(cells(g), |i, j| geom.main_cell(i, j));
        }
    }

    /// Sets or clears the engine's suggested piece. If given `Some((ty, i, j, r))`, set
    /// piece type `ty` at row `i`, column `j`, rotation `r`.
    pub fn set_engine_piece(&mut self, piece: Option<(PieceType, i16, i16, i32)>) {
        self.eng_ghost.clear();
        if let Some((ty, i, j, r)) = piece {
            let geom = &self.geom;
            let cells = self.ruleset.coords(ty, r).map(move |(i_off, j_off)| {
                let i = (i + i_off as i16) as u16;
                let j = (j + j_off as i16) as u16;
                ((i, j), ty)
            });
            self.eng_ghost.insert(cells, |i, j| geom.eng_cell(i, j));
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
    /// `seq`: `(idx, len)` of this sequence among other suggestions
    /// `pos`: `(idx, len)` of the step in the sequence being displayed
    /// `rating`: score at end of sequence
    pub fn set_engine_overlay(&mut self, seq: (usize, usize), pos: (usize, usize), rating: i64) {
        use std::fmt::Write;
        let mut line = format!("#{} of {}", seq.0 + 1, seq.1);
        self.eng_overlay[0].set(&line);
        line.clear();
        write!(&mut line, "rating {}", rating).unwrap();
        self.eng_overlay[1].set(&line);
        line.clear();
        write!(&mut line, "{}/{}", pos.0 + 1, pos.1).unwrap();
        self.eng_overlay[2].set(&line);
        self.eng_overlay[3].clear();
    }

    /// Sets the information about the static evaluation of the current preview.
    pub fn set_engine_overlay_score(
        &mut self,
        eval: &blockfish::ai::Eval,
        params: &blockfish::Parameters,
    ) {
        self.eng_overlay[0].set(&format!("eval:  {:<5}= ", eval.score(params)));
        self.eng_overlay[1].set(&format!(
            "rows   {:<2}*{:<2}+ ",
            eval.rows, params.row_factor
        ));
        self.eng_overlay[2].set(&format!(
            "pc est {:<2}*{:<2}+ ",
            eval.piece_estimate, params.piece_estimate_factor
        ));
        self.eng_overlay[3].set(&format!(
            "i deps {:<2}*{:<2}+ ",
            eval.i_dependencies, params.i_dependency_factor
        ));
    }

    /// Clears the engine suggestion information.
    pub fn clear_engine_overlay(&mut self) {
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

    /// Sets the view to indicate that the engine is not enabled.
    pub fn set_engine_disabled(&mut self) {
        self.eng_matrix.clear();
        self.eng_ghost.clear();
        self.clear_engine_overlay();
        self.eng_status.set("engine not enabled");
    }

    /// Enables the tree sidebar in the view. Note that this function isn't always
    /// necessary to call, since other tree functions (e.g. `set_tree_nodes`) will
    /// automatically enable the sidebar.
    pub fn set_tree_enabled(&mut self) {
        let _ = self.enable_tree_();
    }

    /// Disables the tree sidebar.
    pub fn set_tree_disabled(&mut self) {
        self.tree_sidebar = None;
    }

    fn enable_tree_(&mut self) -> &mut TreeSidebar<'r> {
        let geom = &mut self.geom;
        self.tree_sidebar.get_or_insert_with(|| {
            geom.tree_scroll = 0;
            TreeSidebar::new()
        })
    }

    /// Sets the list of nodes to display in the tree sidebar.
    pub fn set_tree_nodes(&mut self, iter: impl IntoIterator<Item = TreeNode>) {
        self.enable_tree_().set_nodes(iter);
    }

    /// Sets which node is being hovered over in the tree sidebar, by its index in the
    /// list (in the same order as supplied to `set_tree_nodes`). If `None`, then no node
    /// is hovered over.
    pub fn set_tree_hover(&mut self, idx: Option<usize>) {
        self.enable_tree_().hover = idx;
    }

    /// Sets the scroll offset of the tree sidebar. Larger values indicate scrolling down.
    pub fn set_tree_scroll(&mut self, scroll: i32) {
        self.enable_tree_();
        self.geom.tree_scroll = scroll;
    }

    /// Returns the valid range of values that should be passed to `set_tree_scroll`.
    pub fn tree_scroll_bounds(&self) -> std::ops::RangeInclusive<i32> {
        match self.tree_sidebar.as_ref() {
            Some(tree) => 0..=tree.max_scroll(&self.geom),
            None => 0..=0,
        }
    }

    /// Renders any labels whos text has recently been updated, and needs to be
    /// rendered. This function should be called whenever components of the view were
    /// possibly updated.
    pub fn render_labels(&self, tc: &'r TextureCreator) {
        let colors = &self.colors;
        let hud_font = &self.resources.hud_font;
        let hud_font_bold = &self.resources.hud_font_bold;
        let hud_font_small = &self.resources.hud_font_small;
        let hud_font_small_bold = &self.resources.hud_font_small_bold;

        self.motd.render(tc, hud_font_bold, colors.text.0);
        for lbl in self.stats.iter() {
            lbl.render(tc, hud_font, colors.text.0);
        }
        for lns in self.help.iter() {
            let mut font = hud_font_bold; // header line is bold
            for ln in lns.iter() {
                ln.render(tc, font, colors.text.0);
                font = hud_font; // other lines are not bold
            }
        }

        for lbl in self.eng_overlay.iter() {
            lbl.render(tc, hud_font_small_bold, colors.text.0);
        }
        self.eng_status.render(tc, hud_font_small, colors.text.1);

        let progress_font = &self.resources.progress_font;
        let progress_color = if self.progress.1 {
            colors.good_bad.0
        } else {
            colors.text.0
        };
        self.progress.0.render(tc, progress_font, progress_color);

        if let Some(tree) = self.tree_sidebar.as_ref() {
            tree.render_labels(colors, &self.resources, tc);
        }
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

        for lns in self.help.iter() {
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
        self.eng_status.paint(cv, self.geom.engine_status());

        // draw tree sidebar
        if let Some(tree) = self.tree_sidebar.as_ref() {
            tree.paint(cv, &self.geom, &self.colors);
        }
    }

    /// Creates a timer based on the handling settings configured for this view.
    pub fn make_timer(&self) -> Timer {
        Timer::new(&self.controls.handling)
    }

    /// Handle SDL event `evt`. If an action should be performed as a result, returns
    /// `Ok(Some(action))`. If the application should quit, returns `Err(Quit)`. If no
    /// more actions are available, returns `Ok(None)`.
    pub fn handle(&self, evt: sdl2::event::Event, tmr: &mut Timer) -> Result<Option<Action>, Quit> {
        use sdl2::{event::Event, mouse::MouseButton};
        Ok(match evt {
            Event::Quit { .. } => return Err(Quit),

            Event::KeyDown {
                keycode: Some(keycode),
                keymod,
                ..
            } => self.handle_key_down(keycode, keymod, tmr),

            Event::KeyUp {
                keycode: Some(keycode),
                keymod,
                ..
            } => self.handle_key_up(keycode, keymod, tmr),

            Event::MouseMotion { x, y, .. } => self.handle_mouse_motion((x as i32, y as i32)),

            Event::MouseWheel { y, .. } => self.handle_mouse_scroll(y),

            Event::MouseButtonDown {
                x,
                y,
                mouse_btn: MouseButton::Left,
                ..
            } => self.handle_left_click((x as i32, y as i32)),

            _ => None,
        })
    }

    fn handle_key_down(
        &self,
        keycode: sdl2::keyboard::Keycode,
        keymod: sdl2::keyboard::Mod,
        tmr: &mut Timer,
    ) -> Option<Action> {
        let action = self.controls.parse(keycode, keymod);
        match action {
            Some(Action::Engine(controls::EngineOp::Goto)) => {
                if tmr.begin_autoplay() {
                    action
                } else {
                    // autoplay has already been activated so don't trigger further Goto
                    // actions.  this would usually happen due to your system's DAS
                    // triggering multiple key presses.
                    None
                }
            }

            Some(Action::Game(op)) if op.can_das() => {
                if tmr.begin_autoshift(op) {
                    action
                } else {
                    // similar as comment above; supress system DAS triggers.
                    None
                }
            }

            _ => action,
        }
    }

    fn handle_key_up(
        &self,
        keycode: sdl2::keyboard::Keycode,
        keymod: sdl2::keyboard::Mod,
        tmr: &mut Timer,
    ) -> Option<Action> {
        let action = self.controls.parse(keycode, keymod);
        match action {
            Some(Action::Engine(controls::EngineOp::Goto)) => tmr.end_autoplay(),
            Some(Action::Game(op)) => tmr.end_autoshift(op),
            _ => {}
        }
        None
    }

    fn handle_mouse_motion(&self, pos: (i32, i32)) -> Option<Action> {
        let tree = self.tree_sidebar.as_ref()?;
        let idx = tree.entry_idx(&self.geom, pos);
        if tree.hover == idx {
            return None;
        }
        let op = match idx {
            Some(idx) => controls::TreeOp::OverEntry(idx),
            None => controls::TreeOp::Out,
        };
        Some(Action::Tree(op))
    }

    fn handle_mouse_scroll(&self, dy: i32) -> Option<Action> {
        let _tree = self.tree_sidebar.as_ref()?;
        let op = controls::TreeOp::ScrollBy(-dy * SCROLL_WHEEL_SPEED);
        Some(Action::Tree(op))
    }

    fn handle_left_click(&self, pos: (i32, i32)) -> Option<Action> {
        let tree = self.tree_sidebar.as_ref()?;
        let idx = tree.entry_idx(&self.geom, pos)?;
        let op = controls::TreeOp::ClickEntry(idx);
        Some(Action::Tree(op))
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
    texture: std::cell::Cell<Option<Texture<'a>>>,
    dim: std::cell::Cell<(u32, u32)>,
}

impl<'a> Label<'a> {
    /// Constructs a new empty label.
    fn new() -> Self {
        Self {
            text: String::new(),
            texture: None.into(),
            dim: (0, 0).into(),
        }
    }

    fn width(&self) -> u32 {
        self.dim.get().0
    }

    fn height(&self) -> u32 {
        self.dim.get().1
    }

    /// Clears the text on this label (equivalent to `self.set("");`).
    fn clear(&mut self) {
        self.text.clear();
        self.texture.set(None);
        self.dim.set((0, 0));
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
        &self,
        tc: &'a TextureCreator,
        font: &sdl2::ttf::Font<'a, 'static>,
        color: Color,
    ) -> bool {
        if self.text.is_empty() {
            return false;
        }
        if let Some(tx) = self.texture.take() {
            self.texture.set(Some(tx));
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
        self.texture.set(Some(texture));
        self.dim.set((query.width, query.height));
        true
    }

    /// Paints this label onto canvas `cv` with top-left origin `(x, y)`.
    fn paint(&self, cv: &mut Canvas, (x, y): (i32, i32)) {
        if let Some(texture) = self.texture.take() {
            let (w, h) = self.dim.get();
            let rect = (x, y, w, h).into();
            cv.copy(&texture, None, Some(rect)).unwrap();
            self.texture.set(Some(texture));
        }
    }
}

struct TreeSidebar<'r> {
    nodes: Vec<TreeNode>,
    labels: Vec<TreeNodeLabels<'r>>,
    hover: Option<usize>,
}

struct TreeNodeLabels<'r> {
    rating: Label<'r>,
    count: Label<'r>,
}

const TREE_MAX_DEPTH: usize = 10;

impl<'r> TreeSidebar<'r> {
    fn new() -> Self {
        Self {
            nodes: vec![],
            labels: vec![],
            hover: None,
        }
    }

    /// Returns the index of the entry in the tree at position `pos`, if any.
    fn entry_idx(&self, geom: &Geometry, pos: (i32, i32)) -> Option<usize> {
        if let Some(idx) = geom.tree_node_entry_at(pos) {
            if idx < self.nodes.len() {
                return Some(idx);
            }
        }
        None
    }

    /// Returns the max valid scroll offset. May be `0`, if the tree is too short to be
    /// scrolled.
    fn max_scroll(&self, geom: &Geometry) -> i32 {
        let height = (self.nodes.len() as i32) * geom.tree_node_height;
        std::cmp::max(0, height - geom.bottom)
    }

    /// Sets the nodes in the tree to the given list of `TreeNode`'s
    fn set_nodes(&mut self, iter: impl IntoIterator<Item = TreeNode>) {
        self.nodes.clear();
        self.nodes.extend(iter);

        let labels = &mut self.labels;
        labels.resize_with(self.nodes.len(), TreeNodeLabels::new);
        for (lbl, node) in labels.iter_mut().zip(self.nodes.iter()) {
            lbl.set(node);
        }

        if self.hover.map_or(false, |idx| idx >= self.nodes.len()) {
            self.hover = None;
        }
    }

    fn render_labels(&self, colors: &Colors, resources: &Resources<'r>, tc: &'r TextureCreator) {
        for lbl in self.labels.iter() {
            lbl.render(colors, resources, tc);
        }
    }

    fn paint(&self, cv: &mut Canvas, geom: &Geometry, colors: &Colors) {
        cv.set_draw_color(colors.grid_background.0);
        cv.fill_rect(geom.tree_sidebar).unwrap();

        // draw background for hovered item
        if let Some(idx) = self.hover {
            cv.set_draw_color(colors.grid_background.1);
            cv.fill_rect(geom.tree_node_entry(idx)).unwrap();
        }

        // draw tree branches
        cv.set_draw_color(colors.text.1);
        // `vertical[d]` contains range `i0..i1` if vertical line from `(i0,d)` to
        // `(i1,d)` should be drawn
        let mut vertical: [Option<Range<usize>>; TREE_MAX_DEPTH] = Default::default();
        for (i, node) in self.nodes.iter().enumerate() {
            let d = node.depth;
            if d > 0 {
                // update vertical line for parent
                vertical[d - 1].get_or_insert((i - 1)..i).end = i;
                // draw horizontal line
                cv.draw_line(
                    geom.tree_node_center((i, d - 1)),
                    geom.tree_node_center((i, d)),
                )
                .unwrap();
            }
            // flush vertical lines for parent-of-parents
            for d in d..TREE_MAX_DEPTH {
                if let Some(v) = vertical[d].take() {
                    cv.draw_line(
                        geom.tree_node_center((v.start, d)),
                        geom.tree_node_center((v.end, d)),
                    )
                    .unwrap();
                }
            }
        }
        // flush remaining lines
        for d in 0..TREE_MAX_DEPTH {
            if let Some(v) = vertical[d].take() {
                cv.draw_line(
                    geom.tree_node_center((v.start, d)),
                    geom.tree_node_center((v.end, d)),
                )
                .unwrap();
            }
        }

        // draw node icons & labels
        for (idx, node) in self.nodes.iter().enumerate() {
            let loc = (idx, node.depth);
            let color = colors.cell.get(&node.piece).unwrap();
            let node_rect = geom.tree_node_icon(loc);
            cv.set_draw_color(*color);
            cv.fill_rect(node_rect).unwrap();
            self.labels[idx].paint(cv, geom, loc);
        }
    }
}

impl<'r> TreeNodeLabels<'r> {
    fn new() -> Self {
        Self {
            rating: Label::new(),
            count: Label::new(),
        }
    }

    fn set(&mut self, node: &TreeNode) {
        use crate::util::text_fmt::*;
        self.rating.set(&{
            if node.rating == Some(node.best_rating) {
                format!("{}", node.best_rating)
            } else {
                format!("{}\u{2192}{}", maybe(node.rating, "?"), node.best_rating)
            }
        });
        self.count.set(&format!("{}", plural(node.count, "node")));
    }

    fn render(&self, colors: &Colors, resources: &Resources<'r>, tc: &'r TextureCreator) {
        let font = &resources.hud_font_small_bold;
        self.rating.render(tc, font, colors.text.0);
        self.count.render(tc, font, colors.text.1);
    }

    fn paint(&self, cv: &mut Canvas, geom: &Geometry, loc: (usize, usize)) {
        let (x_rate, x_cnt, y) = geom.tree_node_labels(loc, &self.count);
        self.rating.paint(cv, (x_rate, y));
        self.count.paint(cv, (x_cnt, y));
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
    // height of entries in tree sidebar
    tree_node_height: i32,
    // padding around node icons in tree sidebar
    tree_node_pad: i32,
    // x offset between nodes at adjacent depths
    tree_node_dx: i32,
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
    // extent of the tree-view sidebar
    tree_sidebar: Rect,
    // scroll offset of tree-view
    tree_scroll: i32,
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
            text_pad_small: 2,
            tree_node_height: 20,
            tree_node_pad: 2,
            tree_node_dx: 25,
            // calculated on every resize
            bottom: 0,
            cell: 0,
            eng_cell: 0,
            matrix: Rect::new(0, 0, 0, 0),
            eng_matrix: Rect::new(0, 0, 0, 0),
            tree_sidebar: Rect::new(0, 0, 0, 0),
            tree_scroll: 0,
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

        let tsb_w = win_w * 1 / 4;
        let tsb_x = (win_w - tsb_w) as i32;
        self.tree_sidebar = (tsb_x, 0, tsb_w, win_h).into();
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
        x -= std::cmp::min(min_dx, (label.width() as i32) / 2);
        y -= label.height() as i32;
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

    fn tree_origin(&self) -> (i32, i32) {
        let x0 = self.tree_sidebar.left();
        let y0 = self.tree_sidebar.top() - self.tree_scroll;
        (x0, y0)
    }

    fn tree_node_entry(&self, idx: usize) -> Rect {
        let (x, mut y) = self.tree_origin();
        y += (idx as i32) * self.tree_node_height;
        let w = self.tree_sidebar.width();
        let h = self.tree_node_height as u32;
        (x, y, w, h).into()
    }

    fn tree_node_entry_at(&self, (x, y): (i32, i32)) -> Option<usize> {
        let (x0, y0) = self.tree_origin();
        if x < x0 || y < y0 {
            None
        } else {
            let idx = std::cmp::max(0, (y - y0) / self.tree_node_height);
            Some(idx as usize)
        }
    }

    fn tree_node_origin(&self, (idx, depth): (usize, usize)) -> (i32, i32) {
        let (mut x, mut y) = self.tree_origin();
        x += (depth as i32) * self.tree_node_dx;
        y += (idx as i32) * self.tree_node_height;
        (x, y)
    }

    fn tree_node_icon(&self, loc: (usize, usize)) -> Rect {
        let (mut x, mut y) = self.tree_node_origin(loc);
        x += self.tree_node_pad;
        y += self.tree_node_pad;
        let w = (self.tree_node_height - self.tree_node_pad * 2) as u32;
        (x, y, w, w).into()
    }

    fn tree_node_center(&self, loc: (usize, usize)) -> Point {
        let (x, y) = self.tree_node_origin(loc);
        (x + self.tree_node_height / 2, y + self.tree_node_height / 2).into()
    }

    fn tree_node_labels(&self, loc: (usize, usize), count: &Label) -> (i32, i32, i32) {
        let (mut x_rate, mut y) = self.tree_node_origin(loc);
        y += self.text_pad_small;
        x_rate += self.tree_node_height + self.text_pad_small;
        let mut x_count = self.tree_sidebar.right();
        x_count -= count.width() as i32 + self.text_pad_small;
        (x_rate, x_count, y)
    }
}
