mod iter;
mod print_info;

use std::{io::Write, sync::Arc};

use crossterm::{
    cursor, queue,
    style::{Attribute, Attributes},
};
use duat_core::{
    context::{self, Decode, Encode},
    form::{CONTROL_CHAR_ID, Painter},
    opts::PrintOpts,
    session::TwoPointsPlace,
    text::{Item, Part, Text, TwoPoints, txt},
    ui::{
        self, Caret, DynSpawnSpecs, PushSpecs, SpawnId,
        traits::{RawArea, UiPass},
    },
};
use iter::{print_iter, rev_print_iter};

pub use self::print_info::PrintInfo;
use crate::{
    AreaId, CStyle, Mutex,
    layout::{Frame, Layouts},
    print_style,
    printer::Lines,
};

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Encode, Decode)]
#[bincode(crate = "duat_core::context::bincode")]
pub struct Coord {
    pub x: u32,
    pub y: u32,
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

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Encode, Decode)]
#[bincode(crate = "duat_core::context::bincode")]
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

    pub fn x_range(&self) -> std::ops::Range<u32> {
        self.tl.x..self.br.x
    }
}

#[derive(Clone)]
pub struct Area {
    layouts: Layouts,
    id: AreaId,
    ansi_codes: Arc<Mutex<micromap::Map<CStyle, String, 16>>>,
    set_frame: fn(&mut Self, Frame),
}

impl PartialEq for Area {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Area {
    /// Returns a new `Area` from raw parts
    pub(crate) fn new(id: AreaId, layouts: Layouts) -> Self {
        Self {
            layouts,
            id,
            ansi_codes: Arc::default(),
            set_frame: |area, frame| {
                if !area.layouts.set_frame(area.id, frame) {
                    context::warn!("This Area was already deleted");
                }
            },
        }
    }

    /// Adds a frame to this [`Area`]
    ///
    /// This function will fail if the `Area` was either deleted or is
    /// not a spawned `Area`.
    pub fn set_frame(&mut self, frame: Frame) {
        (self.set_frame)(self, frame)
    }

    /// Prints the `Text`
    fn print(
        &self,
        text: &Text,
        opts: PrintOpts,
        mut painter: Painter,
        mut f: impl FnMut(&Caret, &Item),
    ) {
        const SPACES: &[u8] = &[b' '; 3000];

        fn end_line(
            lines: &mut Lines,
            painter: &Painter,
            ansi_codes: &mut micromap::Map<CStyle, String, 16>,
            len: u32,
            max_x: u32,
        ) {
            let mut default_style = painter.get_default();
            default_style.style.foreground_color = None;
            default_style.style.underline_color = None;
            default_style.style.attributes = Attributes::from(Attribute::Reset);

            print_style(lines, default_style.style, ansi_codes);
            if lines.coords().br.x == max_x {
                lines.write_all(b"\x1b[0K").unwrap();
            } else {
                lines
                    .write_all(&SPACES[..(lines.coords().width() - len) as usize])
                    .unwrap();
            }
            lines.flush().unwrap();
        }

        let (mut lines, iter, x_shift, max_x) = {
            let Some(coords) = self.layouts.coords_of(self.id, true) else {
                context::warn!("This Area was already deleted");
                return;
            };

            let max = self
                .layouts
                .inspect(self.id, |_, layout| layout.max_value())
                .unwrap();

            if coords.width() == 0 || coords.height() == 0 {
                return;
            }

            let (s_points, x_shift) = {
                let mut info = self.layouts.get_info_of(self.id).unwrap();
                let s_points = info.start_points(coords, text, opts);
                self.layouts.set_info_of(self.id, info);
                (s_points, info.x_shift())
            };

            let lines = Lines::new(coords);
            let width = opts.wrap_width(coords.width()).unwrap_or(coords.width());
            let iter = print_iter(text, s_points, width, opts);

            (lines, iter, x_shift, max.x)
        };

        let mut observed_spawns = Vec::new();
        let is_active = self.id == self.layouts.get_active_id();

        let mut ansi_codes = self.ansi_codes.lock().unwrap();
        let mut style_was_set = false;

        enum Cursor {
            Main,
            Extra,
        }

        // The y here represents the bottom of the current row of cells.
        let tl_y = lines.coords().tl.y;
        let mut y = tl_y;
        let mut cursor = None;
        let mut spawns_for_next: Vec<SpawnId> = Vec::new();
        let mut last_len = 0;

        for (caret, item) in iter {
            let (painter, lines, ansi_codes) = (&mut painter, &mut lines, &mut ansi_codes);
            f(&caret, &item);

            let Caret { x, len, wrap } = caret;
            let Item { part, .. } = item;

            if wrap {
                if y == lines.coords().br.y {
                    break;
                }
                if y > lines.coords().tl.y {
                    end_line(lines, painter, ansi_codes, last_len, max_x);
                }
                let initial_space = x.saturating_sub(x_shift).min(lines.coords().width());
                if initial_space > 0 {
                    let mut default_style = painter.get_default().style;
                    default_style.attributes.set(Attribute::Reset);
                    print_style(lines, default_style, ansi_codes);
                    lines.write_all(&SPACES[..initial_space as usize]).unwrap();
                }
                y += 1;

                // Resetting space to prevent erroneous printing.
                painter.reset_prev_style();
                style_was_set = true;
                last_len = initial_space;
            }

            let is_contained = x + len > x_shift && x < x_shift + lines.coords().width();

            match part {
                Part::Char(char) if is_contained => {
                    if let Some(str) = get_control_str(char) {
                        painter.apply(CONTROL_CHAR_ID, 100);
                        if style_was_set && let Some(style) = painter.relative_style() {
                            print_style(lines, style, ansi_codes);
                        }
                        lines.write_all(str.as_bytes()).unwrap();
                        painter.remove(CONTROL_CHAR_ID)
                    } else {
                        if style_was_set && let Some(style) = painter.relative_style() {
                            print_style(lines, style, ansi_codes);
                        }
                        match char {
                            '\t' => {
                                let truncated_start = x_shift.saturating_sub(x);
                                let truncated_end =
                                    (x + len).saturating_sub(lines.coords().width() + x_shift);
                                let tab_len = len - (truncated_start + truncated_end);
                                lines.write_all(&SPACES[..tab_len as usize]).unwrap()
                            }
                            '\n' if opts.print_new_line => lines.write_all(b" ").unwrap(),
                            '\n' | '\r' => {}
                            char => {
                                let mut bytes = [0; 4];
                                char.encode_utf8(&mut bytes);
                                lines.write_all(&bytes[..char.len_utf8()]).unwrap();
                            }
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
                    for id in spawns_for_next.drain(..) {
                        observed_spawns.push(id);
                        self.layouts.move_spawn_to(
                            id,
                            Coord::new(lines.coords().tl.x + x, y - 1),
                            len,
                        );
                    }

                    last_len = x + len - x_shift;
                    style_was_set = false;
                }
                Part::Char(_) => {
                    match cursor.take() {
                        Some(Cursor::Main) => painter.remove_main_caret(),
                        Some(Cursor::Extra) => painter.remove_extra_caret(),
                        None => {}
                    }
                    spawns_for_next.clear();
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
                        queue!(lines, shape, cursor::SavePosition).unwrap();
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
                Part::Spacer => {
                    let truncated_start = x_shift.saturating_sub(x).min(len);
                    let truncated_end = (x + len)
                        .saturating_sub(lines.coords().width().saturating_sub(x_shift))
                        .min(len);
                    let spacer_len = len - (truncated_start + truncated_end);
                    lines.write_all(&SPACES[0..spacer_len as usize]).unwrap();
                    last_len = (x + len)
                        .saturating_sub(x_shift)
                        .min(lines.coords().width());
                }
                Part::ResetState => print_style(lines, painter.reset(), ansi_codes),
                Part::SpawnedWidget(id) => spawns_for_next.push(id),
                Part::ToggleStart(_) | Part::ToggleEnd(_) => {
                    todo!("Toggles have not been implemented yet.")
                }
                Part::AlignLeft | Part::AlignCenter | Part::AlignRight => {}
            }
        }

        end_line(&mut lines, &painter, &mut ansi_codes, last_len, max_x);

        for _ in 0..lines.coords().br.y - y {
            end_line(&mut lines, &painter, &mut ansi_codes, 0, max_x);
        }

        let spawns = text.get_spawned_ids();

        self.layouts
            .send_lines(self.id, lines, spawns, &observed_spawns);
    }
}

impl RawArea for Area {
    type Cache = PrintInfo;
    type PrintInfo = PrintInfo;

    /////////// Modification

    fn push(
        &self,
        _: UiPass,
        specs: PushSpecs,
        on_files: bool,
        cache: PrintInfo,
    ) -> Option<(Area, Option<Area>)> {
        let (child, parent) = self.layouts.push(self.id, specs, on_files, cache)?;

        Some((
            Self::new(child, self.layouts.clone()),
            parent.map(|parent| Self::new(parent, self.layouts.clone())),
        ))
    }

    fn delete(&self, _: UiPass) -> (bool, Vec<Self>) {
        let (do_rm_window, rm_areas) = self.layouts.delete(self.id);
        (
            do_rm_window,
            rm_areas
                .into_iter()
                .map(|id| Self::new(id, self.layouts.clone()))
                .collect(),
        )
    }

    fn swap(&self, _: UiPass, rhs: &Self) -> bool {
        self.layouts.swap(self.id, rhs.id)
    }

    fn spawn(
        &self,
        _: UiPass,
        spawn_id: SpawnId,
        specs: DynSpawnSpecs,
        cache: Self::Cache,
    ) -> Option<Self> {
        Some(Self::new(
            self.layouts
                .spawn_on_widget(self.id, spawn_id, specs, cache)?,
            self.layouts.clone(),
        ))
    }

    fn set_width(&self, _: UiPass, width: f32) -> Result<(), Text> {
        if self
            .layouts
            .set_constraints(self.id, Some(width), None, None)
        {
            Ok(())
        } else {
            Err(txt!("This Area was already deleted"))
        }
    }

    fn set_height(&self, _: UiPass, height: f32) -> Result<(), Text> {
        if self
            .layouts
            .set_constraints(self.id, None, Some(height), None)
        {
            Ok(())
        } else {
            Err(txt!("This Area was already deleted"))
        }
    }

    fn hide(&self, _: UiPass) -> Result<(), Text> {
        if self
            .layouts
            .set_constraints(self.id, None, None, Some(true))
        {
            Ok(())
        } else {
            Err(txt!("This Area was already deleted"))
        }
    }

    fn reveal(&self, _: UiPass) -> Result<(), Text> {
        if self
            .layouts
            .set_constraints(self.id, None, None, Some(false))
        {
            Ok(())
        } else {
            Err(txt!("This Area was already deleted"))
        }
    }

    fn width_of_text(&self, _: UiPass, opts: PrintOpts, text: &Text) -> Result<f32, Text> {
        let max = self
            .layouts
            .inspect(self.id, |_, layout| layout.max_value())
            .ok_or_else(|| txt!("This Area was already deleted"))?;

        let iter = iter::print_iter(text, TwoPoints::default(), max.x, opts);

        let mut max = 0;
        let mut width = 0;

        for (caret, item) in iter {
            if caret.wrap {
                max = width.max(max);
                width = 0;
            }
            if item.part.is_char() {
                width += caret.len;
            }
        }

        Ok(max.max(width) as f32)
    }

    fn scroll_ver(&self, _: UiPass, text: &Text, by: i32, opts: PrintOpts) {
        if by == 0 {
            return;
        }

        let Some(coords) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return;
        };

        if coords.width() == 0 || coords.height() == 0 {
            return;
        }

        let mut info = self.layouts.get_info_of(self.id).unwrap();
        info.scroll_ver(by, coords, text, opts);
        self.layouts.set_info_of(self.id, info);
    }

    ////////// Printing

    fn scroll_around_points(&self, _: UiPass, text: &Text, points: TwoPoints, opts: PrintOpts) {
        let Some(coords) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return;
        };

        if coords.width() == 0 || coords.height() == 0 {
            return;
        }

        let mut info = self.layouts.get_info_of(self.id).unwrap();
        info.scroll_around(points.real, coords, text, opts);
        self.layouts.set_info_of(self.id, info);
    }

    fn scroll_to_points(&self, _: UiPass, text: &Text, points: TwoPoints, opts: PrintOpts) {
        let Some(coords) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return;
        };

        if coords.width() == 0 || coords.height() == 0 {
            return;
        }

        let mut info = self.layouts.get_info_of(self.id).unwrap();
        info.scroll_to_points(points, coords, text, opts);
        self.layouts.set_info_of(self.id, info);
    }

    fn set_as_active(&self, _: UiPass) {
        self.layouts.set_active_id(self.id);
    }

    fn print(&self, _: UiPass, text: &Text, opts: PrintOpts, painter: Painter) {
        self.print(text, opts, painter, |_, _| {})
    }

    fn print_with<'a>(
        &self,
        _: UiPass,
        text: &Text,
        opts: PrintOpts,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        self.print(text, opts, painter, f)
    }

    ////////// Queries

    fn set_print_info(&self, _: UiPass, info: Self::PrintInfo) {
        self.layouts.set_info_of(self.id, info);
    }

    fn print_iter<'a>(
        &self,
        ca: UiPass,
        text: &'a Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> impl Iterator<Item = (Caret, Item)> + 'a {
        let width = (self.bottom_right(ca).x - self.top_left(ca).x) as u32;
        print_iter(text, points, width, opts)
    }

    fn rev_print_iter<'a>(
        &self,
        ca: UiPass,
        text: &'a Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> impl Iterator<Item = (Caret, Item)> + 'a {
        let width = (self.bottom_right(ca).x - self.top_left(ca).x) as u32;
        rev_print_iter(text, points, opts.wrap_width(width).unwrap_or(width), opts)
    }

    fn has_changed(&self, _: UiPass) -> bool {
        self.layouts
            .inspect(self.id, |rect, layout| rect.has_changed(layout))
            .unwrap_or(false)
    }

    fn is_master_of(&self, _: UiPass, other: &Self) -> bool {
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

    fn get_cluster_master(&self, _: UiPass) -> Option<Self> {
        let id = self
            .layouts
            .inspect(self.id, |_, layout| layout.get_cluster_master(self.id))??;

        Some(Self {
            layouts: self.layouts.clone(),
            id,
            ansi_codes: Arc::default(),
            set_frame: |area, frame| {
                if !area.layouts.set_frame(area.id, frame) {
                    context::warn!("This Area was already deleted");
                }
            },
        })
    }

    fn cache(&self, _: UiPass) -> Option<Self::Cache> {
        let info = self.layouts.get_info_of(self.id)?.for_caching();
        Some(info)
    }

    fn top_left(&self, _: UiPass) -> ui::Coord {
        self.layouts
            .coords_of(self.id, false)
            .map(|coords| ui::Coord {
                x: coords.tl.x as f32,
                y: coords.tl.y as f32,
            })
            .unwrap_or_default()
    }

    fn bottom_right(&self, _: UiPass) -> ui::Coord {
        self.layouts
            .coords_of(self.id, false)
            .map(|coords| ui::Coord {
                x: coords.br.x as f32,
                y: coords.br.y as f32,
            })
            .unwrap_or_default()
    }

    fn coord_at_points(
        &self,
        _: UiPass,
        text: &Text,
        points: TwoPoints,
        opts: PrintOpts,
    ) -> Option<ui::Coord> {
        let Some(coords) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return None;
        };

        if coords.width() == 0 || coords.height() == 0 {
            return None;
        }

        let (s_points, x_shift) = {
            let mut info = self.layouts.get_info_of(self.id).unwrap();
            let s_points = info.start_points(coords, text, opts);
            self.layouts.set_info_of(self.id, info);
            (s_points, info.x_shift())
        };

        let mut row = coords.tl.y;
        for (caret, item) in print_iter(text, s_points, coords.width(), opts) {
            row += caret.wrap as u32;

            if row > coords.br.y {
                break;
            }

            if item.points() == points && item.part.is_char() {
                if caret.x >= x_shift && caret.x <= x_shift + coords.width() {
                    return Some(ui::Coord {
                        x: (coords.tl.x + caret.x - x_shift) as f32,
                        y: (row - 1) as f32,
                    });
                } else {
                    break;
                }
            }
        }

        None
    }

    fn points_at_coord(
        &self,
        _: UiPass,
        text: &Text,
        coord: ui::Coord,
        opts: PrintOpts,
    ) -> Option<TwoPointsPlace> {
        let Some(coords) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return None;
        };

        if coords.width() == 0 || coords.height() == 0 {
            return None;
        } else if !(coords.tl.x..coords.br.x).contains(&(coord.x as u32))
            || !(coords.tl.y..coords.br.y).contains(&(coord.y as u32))
        {
            context::warn!("Coordinate not contained in area");
            return None;
        }

        let (s_points, x_shift) = {
            let mut info = self.layouts.get_info_of(self.id).unwrap();
            let s_points = info.start_points(coords, text, opts);
            self.layouts.set_info_of(self.id, info);
            (s_points, info.x_shift())
        };

        let mut row = coords.tl.y;
        let mut backup = None;
        for (caret, item) in print_iter(text, s_points, coords.width(), opts) {
            row += caret.wrap as u32;

            if row > coord.y as u32 + 1 {
                return backup;
            } else if row == coord.y as u32 + 1
                && let Some(col) = caret.x.checked_sub(x_shift)
            {
                if (coords.tl.x + col..coords.tl.x + col + caret.len).contains(&(coord.x as u32)) {
                    return Some(TwoPointsPlace::Within(item.points()));
                } else if coords.tl.x + col >= coord.x as u32 {
                    return backup;
                }
            }

            if item.part.is_char() {
                backup = Some(TwoPointsPlace::AheadOf(item.points()));
            }
        }

        None
    }

    fn start_points(&self, _: UiPass, text: &Text, opts: PrintOpts) -> TwoPoints {
        let Some(coords) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return Default::default();
        };

        let mut info = self.layouts.get_info_of(self.id).unwrap();
        let start_points = info.start_points(coords, text, opts);
        self.layouts.set_info_of(self.id, info);

        start_points
    }

    fn end_points(&self, _: UiPass, text: &Text, opts: PrintOpts) -> TwoPoints {
        let Some(coords) = self.layouts.coords_of(self.id, false) else {
            context::warn!("This Area was already deleted");
            return Default::default();
        };

        let mut info = self.layouts.get_info_of(self.id).unwrap();
        let end_points = info.end_points(coords, text, opts);
        self.layouts.set_info_of(self.id, info);

        end_points
    }

    fn get_print_info(&self, _: UiPass) -> Self::PrintInfo {
        self.layouts.get_info_of(self.id).unwrap_or_default()
    }

    fn is_active(&self, _: UiPass) -> bool {
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
