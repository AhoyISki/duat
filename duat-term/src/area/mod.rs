mod iter;
mod print_info;

use std::{cell::RefCell, fmt::Alignment, rc::Rc, sync::Arc};

use crossterm::cursor;
use duat_core::{
    cfg::PrintCfg,
    form::{CONTROL_CHAR_ID, Painter},
    text::{FwdIter, Item, Part, Point, RevIter, Text, txt},
    ui::{self, Axis, Caret, MutArea, PushSpecs, SpawnSpecs},
};
use iter::{print_iter, print_iter_indented, rev_print_iter};

pub use self::print_info::PrintInfo;
use crate::{
    AreaId, CStyle, Mutex,
    layout::{Layout, Rect, transfer_vars},
    print_style, queue,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Coord {
    pub y: u32,
    pub x: u32,
}

impl std::fmt::Debug for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("x: {}, y: {}", self.x, self.y))
    }
}

impl Coord {
    pub fn new(x: u32, y: u32) -> Coord {
        Coord { x, y }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Coords {
    pub tl: Coord,
    pub br: Coord,
}
impl Coords {
    pub fn new(tl: Coord, br: Coord) -> Self {
        Coords { tl, br }
    }

    pub fn width(&self) -> u32 {
        self.br.x - self.tl.x
    }

    pub fn height(&self) -> u32 {
        self.br.y - self.tl.y
    }

    pub fn intersects(&self, other: Self) -> bool {
        self.tl.x < other.br.x
            && self.br.x > other.tl.x
            && self.tl.y < other.br.y
            && self.br.y > other.tl.y
    }
}

#[derive(Clone)]
pub struct Area {
    layouts: Rc<RefCell<Vec<Layout>>>,
    pub id: AreaId,
    ansi_codes: Arc<Mutex<micromap::Map<CStyle, String, 16>>>,
}

impl PartialEq for Area {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Area {
    pub(crate) fn new(id: AreaId, layouts: Rc<RefCell<Vec<Layout>>>) -> Self {
        Self { layouts, id, ansi_codes: Arc::default() }
    }

    fn print<'a>(
        &self,
        text: &mut Text,
        cfg: PrintCfg,
        mut painter: Painter,
        mut f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        let layouts = self.layouts.borrow();
        let mut ansi_codes = self.ansi_codes.lock().unwrap();

        let layout = get_layout(&layouts, self.id).unwrap();
        let is_active = layout.active_id() == self.id;

        let (mut lines, iter, is_floating) = {
            let rect = layout.get(self.id).unwrap();
            let max = layout.printer.max_value();
            let (coords, has_changed) = layout.printer.coords(rect.var_points(), true);

            if coords.width() == 0 || coords.height() == 0 {
                return;
            }

            let (s_points, x_shift) = {
                let mut info = rect.print_info().unwrap().get();
                let s_points = info.start_points(coords, text, cfg, has_changed);
                rect.print_info().unwrap().set(info);
                (s_points, info.x_shift())
            };

            let lines = layout.printer.lines(coords, max, x_shift, cfg);

            let iter = {
                let line_start = text.visual_line_start(s_points);
                let iter = text.iter_fwd(line_start);
                print_iter(iter, lines.cap(), cfg, s_points)
            };

            (lines, iter, rect.is_floating())
        };

        let mut style_was_set = false;
        enum Cursor {
            Main,
            Extra,
        }

        let lines_left = {
            // The y here represents the bottom of the current row of cells.
            let tl_y = lines.coords().tl.y;
            let (lines, ansi_codes) = (&mut lines, &mut ansi_codes);
            let mut y = tl_y;
            let mut cursor = None;

            for (caret, item) in iter {
                f(&caret, &item);

                let Caret { x, len, wrap } = caret;
                let Item { part, .. } = item;

                if wrap {
                    if y > lines.coords().tl.y {
                        lines.end_line(ansi_codes, &painter);
                    }
                    if y == lines.coords().br.y {
                        break;
                    }
                    (0..x).for_each(|_| lines.push_char(' ', 1));
                    print_style(lines, painter.absolute_style(), ansi_codes);
                    y += 1
                }

                match part {
                    Part::Char(char) => {
                        match char {
                            '\t' => {
                                if style_was_set {
                                    print_style(lines, painter.relative_style(), ansi_codes);
                                    style_was_set = false;
                                }
                                (0..len).for_each(|_| lines.push_char(' ', 1));
                            }
                            '\n' | '\r' => {}
                            char => {
                                if let Some(str) = get_control_str(char) {
                                    painter.apply(CONTROL_CHAR_ID, 100);
                                    let style = painter.relative_style();
                                    duat_core::context::info!("{CONTROL_CHAR_ID:?}, {style:#?}");
                                    print_style(lines, style, ansi_codes);
                                    str.chars().for_each(|c| lines.push_char(c, 1));
                                    painter.remove(CONTROL_CHAR_ID)
                                } else {
                                    if style_was_set {
                                        print_style(lines, painter.relative_style(), ansi_codes);
                                        style_was_set = false;
                                    }
                                    lines.push_char(char, len)
                                }
                            }
                        }

                        if let Some(cursor) = cursor.take() {
                            match cursor {
                                Cursor::Main => painter.remove_main_caret(),
                                Cursor::Extra => painter.remove_extra_caret(),
                            }
                            print_style(lines, painter.relative_style(), ansi_codes)
                        }
                    }
                    Part::PushForm(id, prio) => {
                        painter.apply(id, prio);
                        style_was_set = true;
                    }
                    Part::PopForm(id) => {
                        painter.remove(id);
                        style_was_set = true;
                    }
                    Part::MainCaret => {
                        if let Some(shape) = painter.main_cursor()
                            && is_active
                        {
                            lines.show_real_cursor();
                            queue!(lines, shape, cursor::SavePosition);
                        } else {
                            cursor = Some(Cursor::Main);
                            lines.hide_real_cursor();
                            painter.apply_main_cursor();
                            style_was_set = true;
                        }
                    }
                    Part::ExtraCaret => {
                        cursor = Some(Cursor::Extra);
                        painter.apply_extra_cursor();
                        style_was_set = true;
                    }
                    Part::AlignLeft if !cfg.wrap_method.is_no_wrap() => {
                        lines.realign(Alignment::Left)
                    }
                    Part::AlignCenter if !cfg.wrap_method.is_no_wrap() => {
                        lines.realign(Alignment::Center)
                    }
                    Part::AlignRight if !cfg.wrap_method.is_no_wrap() => {
                        lines.realign(Alignment::Right)
                    }
                    Part::Spacer if !cfg.wrap_method.is_no_wrap() => {
                        lines.add_spacer();
                    }
                    Part::ResetState => print_style(lines, painter.reset(), ansi_codes),
                    Part::ToggleStart(_) | Part::ToggleEnd(_) => todo!(),
                    _ => {}
                }
            }

            lines.end_line(ansi_codes, &painter);

            lines.coords().br.y - y
        };

        for _ in 0..lines_left {
            lines.end_line(&mut ansi_codes, &painter);
        }

        if is_floating {
            layout.printer.send_floating_lines(self.id, lines);
        } else {
            layout.printer.send_lines(self.id, lines);
        }
    }
}

impl ui::Area for Area {
    type Cache = PrintInfo;
    type PrintInfo = PrintInfo;
    type Ui = crate::Ui;

    /////////// Modification

    fn push(
        area: MutArea<Self>,
        specs: PushSpecs,
        on_files: bool,
        cache: PrintInfo,
    ) -> (Area, Option<Area>) {
        let mut layouts = area.layouts.borrow_mut();
        let layout = get_layout_mut(&mut layouts, area.id).unwrap();

        let (child, parent) = layout.bisect(area.id, specs, on_files, cache);

        (
            Self::new(child, area.layouts.clone()),
            parent.map(|parent| Self::new(parent, area.layouts.clone())),
        )
    }

    fn delete(area: MutArea<Self>) -> Option<Self> {
        let mut layouts = area.layouts.borrow_mut();
        // This Area may have already been deleted, so a Layout may not be
        // found.
        let layout = get_layout_mut(&mut layouts, area.id)?;
        layout
            .delete(area.id)
            .map(|id| Self::new(id, area.layouts.clone()))
    }

    fn swap(lhs: MutArea<Self>, rhs: &Self) {
        let mut layouts = lhs.layouts.borrow_mut();
        let lhs_lay = get_layout_pos(&layouts, lhs.id).unwrap();
        let rhs_lay = get_layout_pos(&layouts, rhs.id).unwrap();

        if lhs_lay == rhs_lay {
            let layout = &mut layouts[lhs_lay];
            let lhs_id = layout.rects.get_cluster_master(lhs.id).unwrap_or(lhs.id);
            let rhs_id = layout.rects.get_cluster_master(rhs.id).unwrap_or(rhs.id);
            if lhs_id == rhs_id {
                return;
            }
            layout.swap(lhs_id, rhs_id);
        } else {
            let [lhs_lay, rhs_lay] = layouts.get_disjoint_mut([lhs_lay, rhs_lay]).unwrap();
            let lhs_id = lhs_lay.rects.get_cluster_master(lhs.id).unwrap_or(lhs.id);
            let rhs_id = rhs_lay.rects.get_cluster_master(rhs.id).unwrap_or(rhs.id);

            let lhs_rect = lhs_lay.rects.get_mut(lhs_id).unwrap();
            let rhs_rect = rhs_lay.rects.get_mut(rhs_id).unwrap();

            let lhs_p = &lhs_lay.printer;
            let rhs_p = &rhs_lay.printer;
            transfer_vars(lhs_p, rhs_p, lhs_rect);
            transfer_vars(rhs_p, lhs_p, rhs_rect);

            std::mem::swap(lhs_rect, rhs_rect);

            lhs_lay.reset_eqs(rhs_id);
            rhs_lay.reset_eqs(lhs_id);
        }

        for lay in [lhs_lay, rhs_lay] {
            layouts[lay].printer.update(false, false);
        }
    }

    fn spawn(area: MutArea<Self>, specs: SpawnSpecs, cache: Self::Cache) -> Self {
        let mut layouts = area.layouts.borrow_mut();
        let layout = get_layout_mut(&mut layouts, area.id).unwrap();

        Self::new(
            layout.new_floating(area.id, specs, cache),
            area.layouts.clone(),
        )
    }

    fn spawn_floating_at(
        _area: MutArea<Self>,
        _specs: SpawnSpecs,
        _at: impl duat_core::text::TwoPoints,
        _text: &Text,
        _cfg: PrintCfg,
    ) -> Result<Self, Text> {
        todo!()
    }

    fn set_width(&self, width: f32) -> Result<(), Text> {
        let mut layouts = self.layouts.borrow_mut();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();
        let old_cons = layout
            .rects
            .get_constraints_mut(self.id)
            .ok_or_else(|| txt!("Area has no parents, so it can't be constrained"))?
            .clone();

        if let Some(old) = old_cons.on(Axis::Horizontal)
            && old == width
        {
            return Ok(());
        };

        *layout.rects.get_constraints_mut(self.id).unwrap() = {
            let (cons, old_eqs) = old_cons.replace(width, Axis::Horizontal);

            let (_, parent) = layout.get_parent(self.id).unwrap();
            let rect = layout.get(self.id).unwrap();

            let (cons, new_eqs) = cons.apply(rect, parent.id(), &layout.rects);
            layout.printer.replace_and_update(old_eqs, new_eqs, false);
            cons
        };

        Ok(())
    }

    fn set_height(&self, height: f32) -> Result<(), Text> {
        let mut layouts = self.layouts.borrow_mut();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();
        let old_cons = layout
            .rects
            .get_constraints_mut(self.id)
            .ok_or_else(|| txt!("Area has no parents, so it can't be constrained"))?
            .clone();

        if let Some(old) = old_cons.on(Axis::Vertical)
            && old == height
        {
            return Ok(());
        };

        *layout.rects.get_constraints_mut(self.id).unwrap() = {
            let (cons, old_eqs) = old_cons.replace(height, Axis::Vertical);

            let (_, parent) = layout.get_parent(self.id).unwrap();
            let rect = layout.get(self.id).unwrap();

            let (cons, new_eqs) = cons.apply(rect, parent.id(), &layout.rects);
            layout.printer.replace_and_update(old_eqs, new_eqs, false);
            cons
        };

        Ok(())
    }

    fn hide(&self) -> Result<(), Text> {
        let mut layouts = self.layouts.borrow_mut();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();
        let mut old_cons = layout
            .rects
            .get_constraints_mut(self.id)
            .ok_or_else(|| txt!("Area has no parents, so it can't be constrained"))?
            .clone();

        if old_cons.is_hidden {
            return Ok(());
        };

        *layout.rects.get_constraints_mut(self.id).unwrap() = {
            let old_eqs = old_cons.get_eqs();
            old_cons.is_hidden = true;

            let (_, parent) = layout.get_parent(self.id).unwrap();
            let rect = layout.get(self.id).unwrap();

            let (cons, new_eqs) = old_cons.apply(rect, parent.id(), &layout.rects);
            layout.printer.replace_and_update(old_eqs, new_eqs, false);
            cons
        };

        Ok(())
    }

    fn reveal(&self) -> Result<(), Text> {
        let mut layouts = self.layouts.borrow_mut();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();
        let mut old_cons = layout
            .rects
            .get_constraints_mut(self.id)
            .ok_or_else(|| txt!("Area has no parents, so it can't be constrained"))?
            .clone();

        if !old_cons.is_hidden {
            return Ok(());
        };

        *layout.rects.get_constraints_mut(self.id).unwrap() = {
            let old_eqs = old_cons.get_eqs();
            old_cons.is_hidden = false;

            let (_, parent) = layout.get_parent(self.id).unwrap();
            let rect = layout.get(self.id).unwrap();

            let (cons, new_eqs) = old_cons.apply(rect, parent.id(), &layout.rects);
            layout.printer.replace_and_update(old_eqs, new_eqs, false);
            cons
        };

        Ok(())
    }

    fn request_width_to_fit(&self, cfg: PrintCfg, text: &Text) -> Result<(), Text> {
        let layouts = self.layouts.borrow();
        let max = get_layout(&layouts, self.id).unwrap().printer.max_value();
        let iter = iter::print_iter(
            text.iter_fwd(Point::default()),
            cfg.wrap_width(max.x),
            cfg,
            (Point::default(), None),
        );

        // It can be None if there is total concalment.
        let len = match iter.filter(|(_, item)| item.part.is_char()).last() {
            Some((caret, _)) => caret.x as f32 + caret.len as f32,
            _ => 0.0,
        };

        self.set_width(len)
    }

    fn scroll_ver(&self, text: &Text, by: i32, cfg: PrintCfg) {
        if by == 0 {
            return;
        }

        let layouts = self.layouts.borrow();
        let layout = get_layout(&layouts, self.id).unwrap();
        let rect = layout.get(self.id).unwrap();
        let (coords, _) = layout.printer.coords(rect.var_points(), false);

        let mut info = rect.print_info().unwrap().get();
        info.scroll_ver(by, coords, text, cfg);
        rect.print_info().unwrap().set(info);
    }

    ////////// Printing

    fn scroll_around_point(&self, text: &Text, p: Point, cfg: PrintCfg) {
        let layouts = self.layouts.borrow();
        let layout = get_layout(&layouts, self.id).unwrap();
        let rect = layout.get(self.id).unwrap();
        let (coords, _) = layout.printer.coords(rect.var_points(), false);

        let mut info = rect.print_info().unwrap().get();
        info.scroll_around(p, coords, text, cfg);
        rect.print_info().unwrap().set(info);
    }

    fn scroll_to_points(
        &self,
        text: &Text,
        points: impl duat_core::text::TwoPoints,
        cfg: PrintCfg,
    ) {
        let layouts = self.layouts.borrow();
        let layout = get_layout(&layouts, self.id).unwrap();
        let rect = layout.get(self.id).unwrap();
        let (coords, _) = layout.printer.coords(rect.var_points(), false);

        let mut info = rect.print_info().unwrap().get();
        info.scroll_to_points(points, coords, text, cfg);
        rect.print_info().unwrap().set(info);
    }

    fn set_as_active(&self) {
        let mut layouts = self.layouts.borrow_mut();
        get_layout_mut(&mut layouts, self.id).unwrap().active_id = self.id;
    }

    fn print(&self, text: &mut Text, cfg: PrintCfg, painter: Painter) {
        self.print(text, cfg, painter, |_, _| {})
    }

    fn print_with<'a>(
        &self,
        text: &mut Text,
        cfg: PrintCfg,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        self.print(text, cfg, painter, f)
    }

    ////////// Queries

    fn set_print_info(&self, info: Self::PrintInfo) {
        let layouts = self.layouts.borrow();
        let layout = get_layout(&layouts, self.id).unwrap();
        layout.get(self.id).unwrap().print_info().unwrap().set(info);
    }

    fn print_iter<'a>(
        &self,
        iter: FwdIter<'a>,
        cfg: PrintCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        let points = iter.points();
        print_iter(iter, cfg.wrap_width(self.width()), cfg, points)
    }

    fn rev_print_iter<'a>(
        &self,
        iter: RevIter<'a>,
        cfg: PrintCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        rev_print_iter(iter, cfg.wrap_width(self.width()), cfg)
    }

    fn has_changed(&self) -> bool {
        let layouts = self.layouts.borrow();
        if let Some(layout) = get_layout(&layouts, self.id)
            && let Some(rect) = layout.get(self.id)
        {
            rect.has_changed(layout)
        } else {
            true
        }
    }

    fn is_master_of(&self, other: &Self) -> bool {
        if other.id == self.id {
            return true;
        }
        let layouts = self.layouts.borrow();
        let Some(layout) = get_layout(&layouts, self.id) else {
            return false;
        };

        let mut parent_id = other.id;
        while let Some((_, parent)) = layout.get_parent(parent_id) {
            parent_id = parent.id();
            if parent.id() == self.id {
                return true;
            }
        }

        parent_id == self.id
    }

    fn get_cluster_master(&self) -> Option<Self> {
        let layouts = self.layouts.borrow();
        get_layout(&layouts, self.id)?
            .rects
            .get_cluster_master(self.id)
            .map(|id| Self::new(id, self.layouts.clone()))
    }

    fn cache(&self) -> Option<Self::Cache> {
        let layouts = self.layouts.borrow();
        get_rect(&layouts, self.id)?
            .print_info()
            .map(|cell| cell.get().for_caching())
    }

    fn width(&self) -> u32 {
        let layouts = self.layouts.borrow();
        let Some(layout) = get_layout(&layouts, self.id) else {
            return 0;
        };

        let rect = layout.get(self.id).unwrap();
        let (coords, _) = layout.printer.coords(rect.var_points(), false);
        coords.width()
    }

    fn height(&self) -> u32 {
        let layouts = self.layouts.borrow();
        let Some(layout) = get_layout(&layouts, self.id) else {
            return 0;
        };

        let rect = layout.get(self.id).unwrap();
        let (coords, _) = layout.printer.coords(rect.var_points(), false);
        coords.height()
    }

    fn start_points(&self, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>) {
        let layouts = self.layouts.borrow();
        layouted::start_points(self, &layouts, text, cfg)
    }

    fn end_points(&self, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>) {
        let layouts = self.layouts.borrow();
        layouted::end_points(self, &layouts, text, cfg)
    }

    fn print_info(&self) -> Self::PrintInfo {
        let layouts = self.layouts.borrow();
        get_rect(&layouts, self.id)
            .and_then(|rect| rect.print_info())
            .map(|pi| pi.get())
            .unwrap_or_default()
    }

    fn is_active(&self) -> bool {
        if let Some(layout) = get_layout(&self.layouts.borrow(), self.id) {
            layout.active_id == self.id
        } else {
            false
        }
    }
}

mod layouted {
    use duat_core::{
        cfg::PrintCfg,
        text::{Point, Text},
    };

    use super::{Area, get_layout};
    use crate::layout::Layout;

    pub fn start_points(
        area: &Area,
        layouts: &[Layout],
        text: &Text,
        cfg: PrintCfg,
    ) -> (Point, Option<Point>) {
        let layout = get_layout(layouts, area.id).unwrap();
        let rect = layout.get(area.id).unwrap();
        let (coords, has_changed) = layout.printer.coords(rect.var_points(), false);

        let mut info = rect.print_info().unwrap().get();
        let s_points = info.start_points(coords, text, cfg, has_changed);
        rect.print_info().unwrap().set(info);
        s_points
    }

    pub fn end_points(
        area: &Area,
        layouts: &[Layout],
        text: &Text,
        cfg: PrintCfg,
    ) -> (Point, Option<Point>) {
        let layout = get_layout(layouts, area.id).unwrap();
        let rect = layout.get(area.id).unwrap();
        let (coords, _) = layout.printer.coords(rect.var_points(), false);

        let mut info = rect.print_info().unwrap().get();
        let e_points = info.end_points(coords, text, cfg);
        rect.print_info().unwrap().set(info);
        e_points
    }
}

fn get_rect(layouts: &[Layout], id: AreaId) -> Option<&Rect> {
    layouts.iter().find_map(|l| l.get(id))
}

fn get_layout(layouts: &[Layout], id: AreaId) -> Option<&Layout> {
    layouts.iter().find(|l| l.get(id).is_some())
}

fn get_layout_mut(layouts: &mut [Layout], id: AreaId) -> Option<&mut Layout> {
    layouts.iter_mut().find(|l| l.get(id).is_some())
}

fn get_layout_pos(layouts: &[Layout], id: AreaId) -> Option<usize> {
    layouts.iter().position(|l| l.get(id).is_some())
}

const fn get_control_str(char: char) -> Option<&'static str> {
    match char {
        '\0' => Some("^@"),
        '\u{01}' => Some("^A"),
        '\u{02}' => Some("^B"),
        '\u{03}' => Some("^C"),
        '\u{04}' => Some("^D"),
        '\u{05}' => Some("^E"),
        '\u{06}' => Some("^F"),
        '\u{07}' => Some("^G"),
        '\u{08}' => Some("^H"),
        '\u{09}' => Some("^I"),
        '\u{0a}' => Some("^J"),
        '\u{0b}' => Some("^K"),
        '\u{0c}' => Some("^L"),
        '\u{0d}' => Some("^M"),
        '\u{0e}' => Some("^N"),
        '\u{0f}' => Some("^O"),
        '\u{10}' => Some("^P"),
        '\u{11}' => Some("^Q"),
        '\u{12}' => Some("^R"),
        '\u{13}' => Some("^S"),
        '\u{14}' => Some("^T"),
        '\u{15}' => Some("^U"),
        '\u{16}' => Some("^V"),
        '\u{17}' => Some("^W"),
        '\u{18}' => Some("^X"),
        '\u{19}' => Some("^Y"),
        '\u{1a}' => Some("^Z"),
        '\u{1b}' => Some("^["),
        '\u{1c}' => Some("^\\"),
        '\u{1d}' => Some("^]"),
        '\u{1e}' => Some("^^"),
        '\u{1f}' => Some("^_"),
        '\u{80}' => Some("<80>"),
        '\u{81}' => Some("<81>"),
        '\u{82}' => Some("<82>"),
        '\u{83}' => Some("<83>"),
        '\u{84}' => Some("<84>"),
        '\u{85}' => Some("<85>"),
        '\u{86}' => Some("<86>"),
        '\u{87}' => Some("<87>"),
        '\u{88}' => Some("<88>"),
        '\u{89}' => Some("<89>"),
        '\u{8a}' => Some("<8a>"),
        '\u{8b}' => Some("<8b>"),
        '\u{8c}' => Some("<8c>"),
        '\u{8d}' => Some("<8d>"),
        '\u{8e}' => Some("<8e>"),
        '\u{8f}' => Some("<8f>"),
        '\u{90}' => Some("<90>"),
        '\u{91}' => Some("<91>"),
        '\u{92}' => Some("<92>"),
        '\u{93}' => Some("<93>"),
        '\u{94}' => Some("<94>"),
        '\u{95}' => Some("<95>"),
        '\u{96}' => Some("<96>"),
        '\u{97}' => Some("<97>"),
        '\u{98}' => Some("<98>"),
        '\u{99}' => Some("<99>"),
        '\u{9a}' => Some("<9a>"),
        '\u{9b}' => Some("<9b>"),
        '\u{9c}' => Some("<9c>"),
        '\u{9d}' => Some("<9d>"),
        '\u{9e}' => Some("<9e>"),
        '\u{9f}' => Some("<9f>"),
        _ => None,
    }
}
