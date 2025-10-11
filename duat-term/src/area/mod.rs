mod iter;
mod print_info;

use std::{fmt::Alignment, sync::Arc};

use crossterm::cursor;
use duat_core::{
    cfg::PrintCfg,
    context,
    form::{CONTROL_CHAR_ID, Painter},
    text::{FwdIter, Item, Part, Point, RevIter, SpawnId, Text, txt},
    ui::{self, Caret, MutArea, PushSpecs, SpawnSpecs},
};
use iter::{print_iter, print_iter_indented, rev_print_iter};

pub use self::print_info::PrintInfo;
use crate::{AreaId, CStyle, Mutex, layout::Layouts, print_style, printer::LinesBuilder, queue};

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

    pub fn area(&self) -> u32 {
        self.width() * self.height()
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
    layouts: Layouts,
    id: AreaId,
    ansi_codes: Arc<Mutex<micromap::Map<CStyle, String, 16>>>,
}

impl PartialEq for Area {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Area {
    pub(crate) fn new(id: AreaId, layouts: Layouts) -> Self {
        Self { layouts, id, ansi_codes: Arc::default() }
    }

    fn print<'a>(
        &self,
        text: &Text,
        cfg: PrintCfg,
        mut painter: Painter,
        mut f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        let (mut lines, iter, line_start) = {
            let Some((coords, has_changed)) = self.layouts.coords_of(self.id, true) else {
                context::warn!("This Area was already deleted");
                return;
            };

            if coords.width() == 0 || coords.height() == 0 {
                return;
            }

            let (s_points, x_shift) = {
                let mut info = self.layouts.get_info_of(self.id).unwrap();
                let s_points = info.start_points(coords, text, cfg, has_changed);
                self.layouts.set_info_of(self.id, info);
                (s_points, info.x_shift())
            };

            let lines = LinesBuilder::new(coords, x_shift, cfg);

            let (line_start, iter) = {
                let line_start = text.visual_line_start(s_points);
                let iter = text.iter_fwd(line_start);
                (line_start, print_iter(iter, lines.cap(), cfg, s_points))
            };

            (lines, iter, line_start)
        };

        let mut observed_spawns = Vec::new();
        let is_floating = self
            .layouts
            .inspect(self.id, |rect, _| rect.is_floating())
            .unwrap();
        let is_active = self.id == self.layouts.get_active_id();

        let mut ansi_codes = self.ansi_codes.lock().unwrap();
        let mut style_was_set = false;

        enum Cursor {
            Main,
            Extra,
        }

        let lines_left = {
            // The y here represents the bottom of the current row of cells.
            let tl_y = lines.coords().tl.y;
            let (painter, lines, ansi_codes) = (&mut painter, &mut lines, &mut ansi_codes);
            let mut y = tl_y;
            let mut cursor = None;

            for (caret, item) in iter {
                f(&caret, &item);

                let Caret { x, len, wrap } = caret;
                let Item { part, .. } = item;

                if wrap {
                    if y > lines.coords().tl.y {
                        lines.end_line(ansi_codes, painter);
                    }
                    if y == lines.coords().br.y {
                        break;
                    }
                    if x > 0 {
                        let default_style = painter.get_default().style;
                        print_style(lines, default_style, ansi_codes);
                        (0..x).for_each(|_| lines.push_char(' ', 1));
                    }
                    painter.reset_prev_style();
                    style_was_set = true;
                    y += 1
                }

                match part {
                    Part::Char(char) => {
                        if let Some(str) = get_control_str(char) {
                            painter.apply(CONTROL_CHAR_ID, 100);
                            if style_was_set && let Some(style) = painter.relative_style() {
                                print_style(lines, style, ansi_codes);
                            }
                            str.chars().for_each(|c| lines.push_char(c, 1));
                            painter.remove(CONTROL_CHAR_ID)
                        } else {
                            if style_was_set && let Some(style) = painter.relative_style() {
                                print_style(lines, style, ansi_codes);
                            }
                            match char {
                                '\t' => (0..len).for_each(|_| lines.push_char(' ', 1)),
                                '\n' | '\r' => {}
                                char => lines.push_char(char, len),
                            }
                        }

                        if let Some(cursor) = cursor.take() {
                            match cursor {
                                Cursor::Main => painter.remove_main_caret(),
                                Cursor::Extra => painter.remove_extra_caret(),
                            }
                            if let Some(style) = painter.relative_style() {
                                print_style(lines, style, ansi_codes)
                            }
                        }
                        style_was_set = false;
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
                    Part::AlignLeft => lines.realign(Alignment::Left),
                    Part::AlignCenter => lines.realign(Alignment::Center),
                    Part::AlignRight => lines.realign(Alignment::Right),
                    Part::Spacer => lines.add_spacer(),
                    Part::ResetState => print_style(lines, painter.reset(), ansi_codes),
                    Part::SpawnedWidget(id) => {
                        observed_spawns.push(id);
                        self.layouts.move_spawn_to(
                            id,
                            Coord::new(line_start.0.byte() as u32 % 50, 20),
                            len,
                        );
                    }
                    Part::ToggleStart(_) | Part::ToggleEnd(_) => {
                        todo!("Toggles have not been implemented yet.")
                    }
                }
            }

            lines.end_line(ansi_codes, painter);

            lines.coords().br.y - y
        };

        for _ in 0..lines_left {
            lines.end_line(&mut ansi_codes, &mut painter);
        }

        self.layouts
            .send_lines(self.id, lines, is_floating, &observed_spawns, &[]);
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
    ) -> Option<(Area, Option<Area>)> {
        let (child, parent) = area.layouts.push(area.id, specs, on_files, cache)?;

        Some((
            Self::new(child, area.layouts.clone()),
            parent.map(|parent| Self::new(parent, area.layouts.clone())),
        ))
    }

    fn delete(area: MutArea<Self>) -> (bool, Vec<Self>) {
        let (do_rm_window, rm_areas) = area.layouts.delete(area.id);
        (
            do_rm_window,
            rm_areas
                .into_iter()
                .map(|id| Self::new(id, area.layouts.clone()))
                .collect(),
        )
    }

    fn swap(lhs: MutArea<Self>, rhs: &Self) -> bool {
        lhs.layouts.swap(lhs.id, rhs.id)
    }

    fn spawn(
        area: MutArea<Self>,
        spawn_id: SpawnId,
        specs: SpawnSpecs,
        cache: Self::Cache,
    ) -> Option<Self> {
        Some(Self::new(
            area.layouts
                .spawn_on_widget(area.id, spawn_id, specs, cache)?,
            area.layouts.clone(),
        ))
    }

    fn set_width(&self, width: f32) -> Result<(), Text> {
        if self
            .layouts
            .set_constraints(self.id, Some(width), None, None)
        {
            Ok(())
        } else {
            Err(txt!("Couldn't set the width to [a]{width}").build())
        }
    }

    fn set_height(&self, height: f32) -> Result<(), Text> {
        if self
            .layouts
            .set_constraints(self.id, None, Some(height), None)
        {
            Ok(())
        } else {
            Err(txt!("Couldn't set the height to [a]{height}").build())
        }
    }

    fn hide(&self) -> Result<(), Text> {
        if self
            .layouts
            .set_constraints(self.id, None, None, Some(true))
        {
            Ok(())
        } else {
            Err(txt!("Couldn't hide the Area").build())
        }
    }

    fn reveal(&self) -> Result<(), Text> {
        if self
            .layouts
            .set_constraints(self.id, None, None, Some(false))
        {
            Ok(())
        } else {
            Err(txt!("Couldn't reveal the Area").build())
        }
    }

    fn request_width_to_fit(&self, cfg: PrintCfg, text: &Text) -> Result<(), Text> {
        let max = self
            .layouts
            .inspect(self.id, |_, layout| layout.max_value())
            .ok_or_else(|| txt!("This Area was already deleted"))?;

        let iter = iter::print_iter(
            text.iter_fwd(Point::default()),
            cfg.wrap_width(max.x),
            cfg,
            (Point::default(), None),
        );

        // It can be None if there is total concalment.
        self.set_width(iter.map(|(c, _)| c.x + c.len).max().unwrap_or(0) as f32)
    }

    fn scroll_ver(&self, text: &Text, by: i32, cfg: PrintCfg) {
        if by == 0 {
            return;
        }

        let Some((coords, _)) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return;
        };

        let mut info = self.layouts.get_info_of(self.id).unwrap();
        info.scroll_ver(by, coords, text, cfg);
        self.layouts.set_info_of(self.id, info);
    }

    ////////// Printing

    fn scroll_around_point(&self, text: &Text, p: Point, cfg: PrintCfg) {
        let Some((coords, _)) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return;
        };

        let mut info = self.layouts.get_info_of(self.id).unwrap();
        info.scroll_around(p, coords, text, cfg);
        self.layouts.set_info_of(self.id, info);
    }

    fn scroll_to_points(
        &self,
        text: &Text,
        points: impl duat_core::text::TwoPoints,
        cfg: PrintCfg,
    ) {
        let Some((coords, _)) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return;
        };

        let mut info = self.layouts.get_info_of(self.id).unwrap();
        info.scroll_to_points(points, coords, text, cfg);
        self.layouts.set_info_of(self.id, info);
    }

    fn set_as_active(&self) {
        self.layouts.set_active_id(self.id);
    }

    fn print(&self, text: &Text, cfg: PrintCfg, painter: Painter) {
        self.print(text, cfg, painter, |_, _| {})
    }

    fn print_with<'a>(
        &self,
        text: &Text,
        cfg: PrintCfg,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        self.print(text, cfg, painter, f)
    }

    ////////// Queries

    fn set_print_info(&self, info: Self::PrintInfo) {
        self.layouts.set_info_of(self.id, info);
    }

    fn print_iter<'a>(
        &self,
        iter: FwdIter<'a>,
        cfg: PrintCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        let points = iter.points();
        print_iter(iter, cfg.wrap_width(self.width() as u32), cfg, points)
    }

    fn rev_print_iter<'a>(
        &self,
        iter: RevIter<'a>,
        cfg: PrintCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        rev_print_iter(iter, cfg.wrap_width(self.width() as u32), cfg)
    }

    fn has_changed(&self) -> bool {
        self.layouts
            .inspect(self.id, |rect, layout| rect.has_changed(layout))
            .unwrap_or(false)
    }

    fn is_master_of(&self, other: &Self) -> bool {
        if other.id == self.id {
            return true;
        }

        let mut parent_id = other.id;

        self.layouts.inspect(self.id, |_, layout| {
            while let Some((_, parent)) = layout.get_parent(parent_id) {
                parent_id = parent.id();
                if parent.id() == self.id {
                    break;
                }
            }
        });

        parent_id == self.id
    }

    fn get_cluster_master(&self) -> Option<Self> {
        let id = self
            .layouts
            .inspect(self.id, |_, layout| layout.get_cluster_master(self.id))??;

        Some(Self {
            layouts: self.layouts.clone(),
            id,
            ansi_codes: Arc::default(),
        })
    }

    fn cache(&self) -> Option<Self::Cache> {
        Some(self.layouts.get_info_of(self.id)?.for_caching())
    }

    fn width(&self) -> f32 {
        self.layouts
            .coords_of(self.id, false)
            .map(|(coords, _)| coords.width() as f32)
            .unwrap_or(0.0)
    }

    fn height(&self) -> f32 {
        self.layouts
            .coords_of(self.id, false)
            .map(|(coords, _)| coords.height() as f32)
            .unwrap_or(0.0)
    }

    fn start_points(&self, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>) {
        let Some((coords, has_changed)) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return Default::default();
        };

        let mut info = self.layouts.get_info_of(self.id).unwrap();
        let start_points = info.start_points(coords, text, cfg, has_changed);
        self.layouts.set_info_of(self.id, info);

        start_points
    }

    fn end_points(&self, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>) {
        let Some((coords, _)) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return Default::default();
        };

        let mut info = self.layouts.get_info_of(self.id).unwrap();
        let end_points = info.end_points(coords, text, cfg);
        self.layouts.set_info_of(self.id, info);

        end_points
    }

    fn print_info(&self) -> Self::PrintInfo {
        self.layouts.get_info_of(self.id).unwrap_or_default()
    }

    fn is_active(&self) -> bool {
        self.layouts.get_active_id() == self.id
    }
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
        '\u{0b}' => Some("^K"),
        '\u{0c}' => Some("^L"),
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
