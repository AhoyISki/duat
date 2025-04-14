mod iter;

use std::{fmt::Alignment, sync::Arc};

use crossterm::cursor;
use duat_core::{
    cache::bincode::{Decode, Encode},
    cfg::PrintCfg,
    form::Painter,
    text::{FwdIter, Item, Part, Point, RevIter, Text, err},
    ui::{self, Axis, Caret, Constraint, DuatPermission, PushSpecs},
};
use iter::{print_iter, print_iter_indented, rev_print_iter};

use crate::{
    AreaId, Mutex,
    layout::{Layout, Rect, transfer_vars},
    print::Gaps,
    queue, style,
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
        let and_tl = self.tl.max(other.tl);
        let and_br = self.br.min(other.br);
        if and_tl.x > and_br.x || and_tl.y > and_br.y {
            return false;
        }

        let and_coords = Coords::new(and_tl, and_br);
        and_coords.width() > 0 && and_coords.height() > 0
    }
}

#[derive(Clone)]
pub struct Area {
    layouts: Arc<Mutex<Vec<Layout>>>,
    pub id: AreaId,
}

impl PartialEq for Area {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Area {
    pub fn new(id: AreaId, layouts: Arc<Mutex<Vec<Layout>>>) -> Self {
        Self { layouts, id }
    }

    fn print<'a>(
        &self,
        text: &mut Text,
        cfg: PrintCfg,
        mut painter: Painter,
        mut f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        let layouts = self.layouts.lock();
        let start = |_: &Text| layouted::first_points(self, &layouts).0;
        let end = |text: &Text| layouted::last_points(self, &layouts, text, cfg).0;
        text.update_range(start, end);

        let layout = get_layout(&layouts, self.id).unwrap();
        let is_active = layout.active_id() == self.id;

        let (mut lines, iter) = {
            let rect = layout.get(self.id).unwrap();

            let info = {
                let mut info = rect.print_info().unwrap().write();
                info.fix(text);
                *info
            };

            let lines = layout.printer.lines(rect.var_points(), info.x_shift, cfg);
            if lines.coords().width() == 0 || lines.coords().height() == 0 {
                return;
            }

            let iter = {
                let line_start = text.visual_line_start(info.points);
                let iter = text.iter_fwd(line_start);
                print_iter(iter, lines.cap(), cfg, info.points)
            };

            (lines, iter)
        };

        let mut cur_style = None;
        enum Cursor {
            Main,
            Extra,
        }

        let lines_left = {
            // The y here represents the bottom of the current row of cells.
            let tl_y = lines.coords().tl.y;
            let mut y = tl_y;
            let mut cursor = None;

            for (caret, item) in iter {
                f(&caret, &item);

                let Caret { x, len, wrap } = caret;
                let Item { part, .. } = item;

                if wrap {
                    if y > lines.coords().tl.y {
                        lines.end_line(&painter);
                    }
                    if y == lines.coords().br.y {
                        break;
                    }
                    (0..x).for_each(|_| lines.push_char(' ', 1));
                    style!(lines, painter.make_style());
                    y += 1
                }

                match part {
                    Part::Char(char) => {
                        painter.confirm_printing();
                        if let Some(style) = cur_style.take() {
                            style!(lines, style);
                        }
                        match char {
                            '\t' => (0..len).for_each(|_| lines.push_char(' ', 1)),
                            '\n' => {}
                            char => lines.push_char(char, len),
                        }
                        if let Some(cursor) = cursor.take() {
                            style!(lines, match cursor {
                                Cursor::Main => painter.remove_main_cursor(),
                                Cursor::Extra => painter.remove_extra_cursor(),
                            })
                        }
                    }
                    Part::PushForm(id) => {
                        cur_style = Some(painter.apply(id));
                    }
                    Part::PopForm(id) => {
                        cur_style = Some(painter.remove(id));
                    }
                    Part::MainCursor => {
                        if let Some(shape) = painter.main_cursor()
                            && is_active
                        {
                            lines.show_real_cursor();
                            queue!(lines, shape, cursor::SavePosition);
                        } else {
                            cursor = Some(Cursor::Main);
                            lines.hide_real_cursor();
                            cur_style = Some(painter.apply_main_cursor());
                        }
                    }
                    Part::ExtraCursor => {
                        cursor = Some(Cursor::Extra);
                        cur_style = Some(painter.apply_extra_cursor());
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
                    Part::ResetState => {
                        style!(lines, painter.reset())
                    }
                    Part::ToggleStart(_) | Part::ToggleEnd(_) => todo!(),
                    _ => {}
                }
            }

            if !lines.is_empty() {
                lines.end_line(&painter);
            }

            lines.coords().br.y - y
        };

        for _ in 0..lines_left {
            lines.end_line(&painter);
        }

        layout.printer.send(self.id, lines);
    }
}

impl ui::Area for Area {
    type Cache = PrintInfo;
    type PrintInfo = PrintInfo;
    type Ui = crate::Ui;

    /////////// Modification

    fn bisect(
        &self,
        specs: PushSpecs,
        cluster: bool,
        on_files: bool,
        cache: PrintInfo,
        _: DuatPermission,
    ) -> (Area, Option<Area>) {
        let mut layouts = self.layouts.lock();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();

        let (child, parent) = layout.bisect(self.id, specs, cluster, on_files, cache);

        (
            Area::new(child, self.layouts.clone()),
            parent.map(|parent| Area::new(parent, self.layouts.clone())),
        )
    }

    fn delete(&self, _: DuatPermission) -> Option<Self> {
        let mut layouts = self.layouts.lock();
        // This Area may have already been deleted, so a Layout may not be
        // found.
        let layout = get_layout_mut(&mut layouts, self.id)?;
        layout
            .delete(self.id)
            .map(|id| Area::new(id, self.layouts.clone()))
    }

    fn swap(&self, rhs: &Self, _: DuatPermission) {
        let mut layouts = self.layouts.lock();
        let lhs_lay = get_layout_pos(&layouts, self.id).unwrap();
        let rhs_lay = get_layout_pos(&layouts, rhs.id).unwrap();

        if lhs_lay == rhs_lay {
            let layout = &mut layouts[lhs_lay];
            let lhs_id = layout.rects.get_cluster_master(self.id).unwrap_or(self.id);
            let rhs_id = layout.rects.get_cluster_master(rhs.id).unwrap_or(rhs.id);
            if lhs_id == rhs_id {
                return;
            }
            layout.swap(lhs_id, rhs_id);
        } else {
            let [lhs_lay, rhs_lay] = layouts.get_disjoint_mut([lhs_lay, rhs_lay]).unwrap();
            let lhs_id = lhs_lay.rects.get_cluster_master(self.id).unwrap_or(self.id);
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
            layouts[lay].printer.update(false);
        }
    }

    fn spawn_floating(
        &self,
        at: impl duat_core::text::TwoPoints,
        specs: PushSpecs,
        text: &Text,
        cfg: PrintCfg,
        _: DuatPermission,
    ) -> Self {
        let points = at.to_points();
        let mut coord = {
            let layouts = self.layouts.lock();
            let layout = get_layout(&layouts, self.id).unwrap();
            let rect = layout.get(self.id).unwrap();
            layout.printer.coords(rect.var_points(), false).tl
        };
        let mut iter = self.print_iter_from_top(text, cfg);
        while let Some((caret, item)) = iter.next()
            && item.points() <= points
        {
            coord.x = caret.x;
            coord.y += caret.wrap as u32;
        }

        let mut layouts = self.layouts.lock();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();

        let _floating = layout.new_floating(at, specs, text, cfg);

        todo!();
    }

    fn constrain_hor(&self, cons: impl IntoIterator<Item = Constraint>) -> Result<(), Text> {
        let cons = {
            let mut cons: Vec<Constraint> = cons.into_iter().collect();
            cons.sort_unstable();
            cons
        };

        let mut layouts = self.layouts.lock();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();
        let old_cons = layout
            .rects
            .get_constraints_mut(self.id)
            .ok_or_else(|| err!("Area has no parents, so it can't be constrained"))?
            .clone();

        if old_cons.on(Axis::Horizontal).eq(cons.iter().cloned()) {
            return Ok(());
        };

        *layout.rects.get_constraints_mut(self.id).unwrap() = {
            let (cons, old_eqs) = old_cons.replace(cons.into_iter(), Axis::Horizontal);

            let (_, parent) = layout.get_parent(self.id).unwrap();
            let rect = layout.get(self.id).unwrap();

            let (cons, new_eqs) = cons.apply(rect, parent.id(), &layout.rects);
            layout.printer.replace_and_update(old_eqs, new_eqs, false);
            cons
        };

        Ok(())
    }

    fn constrain_ver(&self, cons: impl IntoIterator<Item = Constraint>) -> Result<(), Text> {
        let cons = {
            let mut cons: Vec<Constraint> = cons.into_iter().collect();
            cons.sort_unstable();
            cons
        };

        let mut layouts = self.layouts.lock();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();
        let old_cons = layout
            .rects
            .get_constraints_mut(self.id)
            .ok_or_else(|| err!("Area has no parents, so it can't be constrained"))?
            .clone();

        if old_cons.on(Axis::Vertical).eq(cons.iter().cloned()) {
            return Ok(());
        };

        *layout.rects.get_constraints_mut(self.id).unwrap() = {
            let (cons, old_eqs) = old_cons.replace(cons.into_iter(), Axis::Vertical);

            let (_, parent) = layout.get_parent(self.id).unwrap();
            let rect = layout.get(self.id).unwrap();

            let (cons, new_eqs) = cons.apply(rect, parent.id(), &layout.rects);
            layout.printer.replace_and_update(old_eqs, new_eqs, false);
            cons
        };

        Ok(())
    }

    fn restore_constraints(&self) -> Result<(), Text> {
        todo!();
    }

    fn request_width_to_fit(&self, _text: &str) -> Result<(), Text> {
        todo!();
    }

    fn scroll_around_point(&self, text: &Text, point: Point, cfg: PrintCfg) {
        let layouts = self.layouts.lock();
        let layout = get_layout(&layouts, self.id).unwrap();
        let rect = layout.get(self.id).unwrap();

        let (mut info, w, h) = {
            let coords = layout.printer.coords(rect.var_points(), false);
            let info = rect.print_info().unwrap();
            (info.write(), coords.width(), coords.height())
        };

        if w > 0 && h > 0 {
            scroll_ver_around(&mut info, w, h, point, text, cfg);
            scroll_hor_around(&mut info, w, point, text, cfg);
        }

        info.prev_main = point;
        info.last_points = None;
    }

    fn set_as_active(&self) {
        let mut layouts = self.layouts.lock();
        get_layout_mut(&mut layouts, self.id).unwrap().active_id = self.id;
    }

    ////////// Printing

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

    fn set_print_info(&self, info: Self::PrintInfo) {
        let layouts = self.layouts.lock();
        let layout = get_layout(&layouts, self.id).unwrap();
        *layout.get(self.id).unwrap().print_info().unwrap().write() = info;
    }

    fn print_iter<'a>(
        &self,
        iter: FwdIter<'a>,
        cfg: PrintCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        let points = iter.points();
        print_iter(iter, cfg.wrap_width(self.width()), cfg, points)
    }

    fn print_iter_from_top<'a>(
        &self,
        text: &'a Text,
        cfg: PrintCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        let (info, width) = {
            let layouts = self.layouts.lock();
            let layout = get_layout(&layouts, self.id).unwrap();
            let rect = get_rect(&layouts, self.id).unwrap();

            let info = rect.print_info().unwrap();
            let info = info.read();

            let coords = layout.printer.coords(rect.var_points(), false);

            (*info, coords.width())
        };
        let line_start = text.visual_line_start(info.points);
        let iter = text.iter_fwd(line_start);

        print_iter(iter, cfg.wrap_width(width), cfg, info.points)
    }

    fn rev_print_iter<'a>(
        &self,
        iter: RevIter<'a>,
        cfg: PrintCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        rev_print_iter(iter, cfg.wrap_width(self.width()), cfg)
    }

    fn has_changed(&self) -> bool {
        let layouts = self.layouts.lock();
        let layout = get_layout(&layouts, self.id).unwrap();
        let rect = layout.get(self.id).unwrap();
        rect.has_changed(layout)
    }

    ////////// Queries

    fn is_master_of(&self, other: &Self) -> bool {
        if other.id == self.id {
            return true;
        }
        let layouts = self.layouts.lock();
        let layout = get_layout(&layouts, self.id).unwrap();
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
        let layouts = self.layouts.lock();
        get_layout(&layouts, self.id)
            .unwrap()
            .rects
            .get_cluster_master(self.id)
            .map(|id| Area::new(id, self.layouts.clone()))
    }

    fn cache(&self) -> Option<Self::Cache> {
        let layouts = self.layouts.lock();
        get_rect(&layouts, self.id)
            .unwrap()
            .print_info()
            .map(|info| *info.read())
    }

    fn width(&self) -> u32 {
        let layouts = self.layouts.lock();
        let layout = get_layout(&layouts, self.id).unwrap();
        let rect = layout.get(self.id).unwrap();
        let coords = layout.printer.coords(rect.var_points(), false);
        coords.width()
    }

    fn height(&self) -> u32 {
        let layouts = self.layouts.lock();
        let layout = get_layout(&layouts, self.id).unwrap();
        let rect = layout.get(self.id).unwrap();
        let coords = layout.printer.coords(rect.var_points(), false);
        coords.height()
    }

    fn first_points(&self, _text: &Text, _cfg: PrintCfg) -> (Point, Option<Point>) {
        let layouts = self.layouts.lock();
        layouted::first_points(self, &layouts)
    }

    fn last_points(&self, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>) {
        let layouts = self.layouts.lock();
        layouted::last_points(self, &layouts, text, cfg)
    }

    fn print_info(&self) -> Self::PrintInfo {
        let layouts = self.layouts.lock();
        *get_rect(&layouts, self.id)
            .unwrap()
            .print_info()
            .unwrap()
            .read()
    }

    fn is_active(&self) -> bool {
        get_layout(&self.layouts.lock(), self.id).unwrap().active_id == self.id
    }
}

// NOTE: The defaultness in here, when it comes to `last_main`, may
// cause issues in the future.
/// Information about how to print the file on the `Label`.
#[derive(Default, Debug, Clone, Copy, Encode, Decode)]
#[bincode(crate = "duat_core::cache::bincode")]
pub struct PrintInfo {
    points: (Point, Option<Point>),
    x_shift: u32,
    prev_main: Point,
    last_points: Option<(Point, Option<Point>)>,
}

impl PrintInfo {
    fn fix(&mut self, text: &Text) {
        let max = text.len().min(self.points.0);
        let (_, max_ghost) = text.ghost_max_points_at(max.byte());
        self.points = (max, self.points.1.min(max_ghost))
    }
}

/// Scrolls down until the gap between the main cursor and the
/// bottom of the widget is equal to `config.scrolloff.y_gap`.
fn scroll_ver_around(
    info: &mut PrintInfo,
    width: u32,
    height: u32,
    point: Point,
    text: &Text,
    cfg: PrintCfg,
) {
    if info.prev_main == point {
        return;
    }

    let points = text.ghost_max_points_at(point.byte());
    let after = text.points_after(points).unwrap_or(text.len_points());

    let cap = cfg.wrap_width(width);
    let mut iter = rev_print_iter(text.iter_rev(after), cap, cfg)
        .filter_map(|(caret, item)| caret.wrap.then_some(item.points()));

    let target = if info.prev_main > point {
        cfg.scrolloff.y()
    } else {
        height.saturating_sub(cfg.scrolloff.y() + 1)
    };
    let first = iter.nth(target as usize).unwrap_or_default();

    if (info.prev_main > point && first <= info.points)
        || (info.prev_main < point && first >= info.points)
    {
        info.points = first;
    }
}

/// Scrolls the file horizontally, usually when no wrapping is
/// being used.
fn scroll_hor_around(info: &mut PrintInfo, width: u32, p: Point, text: &Text, cfg: PrintCfg) {
    let cap = cfg.wrap_width(width);
    // Quick shortcut to avoid iteration.
    if cap <= width {
        info.x_shift = 0;
        return;
    }

    let (max_shift, start, end) = {
        let points = text.ghost_max_points_at(p.byte());
        let after = text.points_after(points).unwrap_or(text.len_points());

        let mut iter = rev_print_iter(text.iter_rev(after), cap, cfg);

        let (points, start, end, wrap) = iter
            .find_map(|(Caret { x, len, wrap }, item)| {
                let points = item.points();
                item.part.as_char().and(Some((points, x, x + len, wrap)))
            })
            .unwrap_or(((Point::default(), None), 0, 0, true));

        let (line_len, gaps) = {
            let mut gaps = Gaps::OnRight;
            let (indent, points) = if wrap {
                (start, points)
            } else {
                iter.find_map(|(caret, item)| caret.wrap.then_some((caret.x, item.points())))
                    .unwrap()
            };

            let len = print_iter_indented(text.iter_fwd(points), cap, cfg, indent)
                .inspect(|(_, Item { part, real, .. })| match part {
                    Part::AlignLeft => gaps = Gaps::OnRight,
                    Part::AlignCenter => gaps = Gaps::OnSides,
                    Part::AlignRight => gaps = Gaps::OnLeft,
                    Part::Spacer => gaps.add_spacer(real.byte()),
                    _ => {}
                })
                .take_while(|(caret, item)| !caret.wrap || item.points() == points)
                .last()
                .map(|(Caret { x, len, .. }, _)| x + len)
                .unwrap_or(0);

            (len, gaps)
        };

        let diff = match &gaps {
            Gaps::OnRight => 0,
            Gaps::OnLeft => cap - line_len,
            Gaps::OnSides => (cap - line_len) / 2,
            Gaps::Spacers(bytes) => {
                let spaces = gaps.get_spaces(cap - line_len);
                bytes
                    .iter()
                    .take_while(|b| **b <= p.byte())
                    .zip(spaces)
                    .fold(0, |prev_len, (_, len)| prev_len + len)
            }
        };

        (line_len + diff, start + diff, end + diff)
    };

    info.x_shift = info
        .x_shift
        .min(start.saturating_sub(cfg.scrolloff.x()))
        .max(if cfg.force_scrolloff {
            (end + cfg.scrolloff.x()).saturating_sub(width)
        } else {
            (end + cfg.scrolloff.x())
                .min(max_shift)
                .saturating_sub(width)
        });
}

mod layouted {
    use duat_core::{
        cfg::PrintCfg,
        text::{Item, Point, Text},
        ui::Caret,
    };

    use super::{Area, get_layout, get_rect, iter::print_iter};
    use crate::layout::Layout;

    pub fn first_points(area: &Area, layouts: &[Layout]) -> (Point, Option<Point>) {
        let rect = get_rect(layouts, area.id).unwrap();
        let info = rect.print_info().unwrap();
        let info = info.read();
        info.points
    }

    pub fn last_points(
        area: &Area,
        layouts: &[Layout],
        text: &Text,
        cfg: PrintCfg,
    ) -> (Point, Option<Point>) {
        let (mut info, coords) = {
            let layout = get_layout(layouts, area.id).unwrap();
            let rect = layout.get(area.id).unwrap();
            let info = rect.print_info().unwrap();
            (
                info.write(),
                layout.printer.coords(rect.var_points(), false),
            )
        };

        if let Some(last) = info.last_points {
            return last;
        }

        let line_start = text.visual_line_start(info.points);
        let iter = text.iter_fwd(line_start);

        let iter = print_iter(iter, cfg.wrap_width(coords.width()), cfg, info.points);
        let mut points = info.points;
        let mut y = 0;

        for (Caret { wrap, .. }, Item { part, real, ghost }) in iter {
            if wrap {
                if y == coords.height() {
                    break;
                }
                if part.is_char() {
                    y += 1
                }
            } else {
                points = (real, ghost);
            }
        }

        info.last_points = Some(points);

        points
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
