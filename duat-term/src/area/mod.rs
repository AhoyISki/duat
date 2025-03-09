mod iter;

use std::fmt::Alignment;

use crossterm::cursor;
use duat_core::{
    cache::{Deserialize, Serialize},
    cfg::{IterCfg, PrintCfg},
    data::RwData,
    form::Painter,
    text::{Item, Iter, Part, Point, RevIter, Text},
    ui::{self, Area as UiArea, Axis, Caret, Constraint, DuatPermission, PushSpecs},
};
use iter::{print_iter, print_iter_indented, rev_print_iter};

use crate::{
    AreaId, ConstraintErr,
    layout::{Layout, Rect},
    print::Printer,
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
    tl: Coord,
    br: Coord,
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

    pub fn tl(&self) -> Coord {
        self.tl
    }

    pub fn br(&self) -> Coord {
        self.br
    }
}

#[derive(Clone)]
pub struct Area {
    layouts: RwData<Vec<Layout>>,
    pub id: AreaId,
}

impl PartialEq for Area {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Area {
    pub fn new(id: AreaId, layouts: RwData<Vec<Layout>>) -> Self {
        Self { layouts, id }
    }

    fn print<'a>(
        &self,
        text: &mut Text,
        cfg: PrintCfg,
        mut painter: Painter,
        mut f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        if text.needs_update() {
            let (first_point, _) = self.first_points(text, cfg);
            let (last_point, _) = self.last_points(text, cfg);
            text.update_range((first_point, last_point));
        }

        let layouts = self.layouts.read();
        let layout = get_layout(&layouts, self.id).unwrap();
        let is_active = layout.active_id() == self.id;
        let Some((sender, info)) = layout.get(self.id).and_then(|rect| {
            let sender = rect.sender();
            let info = rect.print_info();
            sender.zip(info).map(|(sender, info)| {
                let mut info = info.write();
                info.fix(text);
                (sender, *info)
            })
        }) else {
            return;
        };

        let (iter, cfg) = {
            let line_start = text.visual_line_start(info.points);
            (text.iter_fwd(line_start), IterCfg::new(cfg).outsource_lfs())
        };

        let cap = cfg.wrap_width(sender.coords().width());
        let iter = print_iter(iter, cap, cfg, info.points);

        let mut lines = sender.lines(info.x_shift, cap);
        let mut cur_style = None;

        enum Cursor {
            Main,
            Extra,
        }

        let lines_left = {
            // The y here represents the bottom of the current row of cells.
            let tl_y = sender.coords().tl.y;
            let mut y = tl_y;
            let mut cursor = None;

            for (caret, item) in iter {
                f(&caret, &item);

                let Caret { x, len, wrap } = caret;
                let Item { part, .. } = item;

                if wrap {
                    if y > sender.coords().tl.y {
                        lines.end_line(&painter);
                    }
                    if y == sender.coords().br.y {
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
                    Part::AlignLeft if !cfg.wrap_method().is_no_wrap() => {
                        lines.realign(Alignment::Left)
                    }
                    Part::AlignCenter if !cfg.wrap_method().is_no_wrap() => {
                        lines.realign(Alignment::Center)
                    }
                    Part::AlignRight if !cfg.wrap_method().is_no_wrap() => {
                        lines.realign(Alignment::Right)
                    }
                    Part::ResetState => {
                        style!(lines, painter.reset())
                    }
                    Part::ToggleStart(_) => todo!(),
                    Part::ToggleEnd(_) => todo!(),
                    _ => {}
                }
            }

            if !lines.is_empty() {
                lines.end_line(&painter);
            }

            sender.coords().br.y - y
        };

        for _ in 0..lines_left {
            lines.end_line(&painter);
        }

        sender.send(lines);
    }
}

impl ui::Area for Area {
    type Cache = PrintInfo;
    type ConstraintChangeErr = ConstraintErr;
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
        let mut layouts = self.layouts.write();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();

        let (child, parent) = layout.bisect(self.id, specs, cluster, on_files, cache);

        (
            Area::new(child, self.layouts.clone()),
            parent.map(|parent| Area::new(parent, self.layouts.clone())),
        )
    }

    fn delete(&self, _: DuatPermission) -> Option<Self> {
        let mut layouts = self.layouts.write();
        // This Area may have already been deleted, so a Layout may not be
        // found.
        get_layout_mut(&mut layouts, self.id)?
            .delete(self.id)
            .map(|id| Area::new(id, self.layouts.clone()))
    }

    fn swap(&self, rhs: &Self, _: DuatPermission) {
        let mut layouts = self.layouts.write();
        let lhs_win = get_layout_pos(&layouts, self.id).unwrap();
        let rhs_win = get_layout_pos(&layouts, rhs.id).unwrap();

        if lhs_win == rhs_win {
            let layout = get_layout_mut(&mut layouts, self.id).unwrap();
            let lhs_id = layout.rects.get_cluster_master(self.id).unwrap_or(self.id);
            let rhs_id = layout.rects.get_cluster_master(rhs.id).unwrap_or(rhs.id);
            if lhs_id == rhs_id {
                return;
            }
            layout.swap(lhs_id, rhs_id);
        } else {
            let [lhs_lay, rhs_lay] = layouts.get_disjoint_mut([lhs_win, rhs_win]).unwrap();
            let lhs_id = lhs_lay.rects.get_cluster_master(self.id).unwrap_or(self.id);
            let rhs_id = rhs_lay.rects.get_cluster_master(rhs.id).unwrap_or(rhs.id);

            let lhs_rect = lhs_lay.rects.get_mut(lhs_id).unwrap();
            let rhs_rect = rhs_lay.rects.get_mut(rhs_id).unwrap();

            let mut lhs_p = lhs_lay.printer.write();
            let mut rhs_p = rhs_lay.printer.write();
            transfer_vars_and_recvs(&mut lhs_p, &mut rhs_p, lhs_rect);
            transfer_vars_and_recvs(&mut rhs_p, &mut lhs_p, rhs_rect);
            drop((lhs_p, rhs_p));

            std::mem::swap(lhs_rect, rhs_rect);

            lhs_lay.reset_eqs(rhs_id);
            rhs_lay.reset_eqs(lhs_id);
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
            let layouts = self.layouts.read();
            let rect = get_rect(&layouts, self.id).unwrap();
            rect.tl()
        };
        let mut iter = self.print_iter_from_top(text, IterCfg::new(cfg));
        while let Some((caret, item)) = iter.next()
            && item.points() <= points
        {
            coord.x = caret.x;
            coord.y += caret.wrap as u32;
        }

        let mut layouts = self.layouts.write();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();

        let floating = layout.new_floating(at, specs, text, cfg);

        todo!();
    }

    fn constrain_hor(&self, con: Constraint) -> Result<(), ConstraintErr> {
        let mut layouts = self.layouts.write();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();
        let cons = layout
            .rects
            .get_constraints_mut(self.id)
            .ok_or(ConstraintErr::NoParent)?
            .clone();

        if let Some(hor) = cons.on(Axis::Horizontal)
            && hor == con
        {
            return Ok(());
        };

        *layout.rects.get_constraints_mut(self.id).unwrap() = {
            let mut p = layout.printer.write();
            let cons = cons.replace(con, Axis::Horizontal, &mut p);

            let (_, parent) = layout.get_parent(self.id).unwrap();
            let rect = layout.get(self.id).unwrap();

            let cons = cons.apply(rect, parent.id(), &layout.rects, &mut p);
            p.flush_equalities().unwrap();
            cons
        };

        Ok(())
    }

    fn constrain_ver(&self, con: Constraint) -> Result<(), ConstraintErr> {
        let mut layouts = self.layouts.write();
        let layout = get_layout_mut(&mut layouts, self.id).unwrap();
        let cons = layout
            .rects
            .get_constraints_mut(self.id)
            .ok_or(ConstraintErr::NoParent)?
            .clone();

        if let Some(ver) = cons.on(Axis::Vertical)
            && ver == con
        {
            return Ok(());
        };

        *layout.rects.get_constraints_mut(self.id).unwrap() = {
            let mut p = layout.printer.write();
            let cons = cons.replace(con, Axis::Vertical, &mut p);

            let (_, parent) = layout.get_parent(self.id).unwrap();
            let rect = layout.get(self.id).unwrap();

            let cons = cons.apply(rect, parent.id(), &layout.rects, &mut p);
            p.flush_equalities().unwrap();
            cons
        };

        Ok(())
    }

    fn restore_constraints(&self) -> Result<(), Self::ConstraintChangeErr> {
        todo!();
    }

    fn request_width_to_fit(&self, _text: &str) -> Result<(), Self::ConstraintChangeErr> {
        todo!();
    }

    fn scroll_around_point(&self, text: &Text, point: Point, cfg: PrintCfg) {
        let (info, w, h) = {
            let layouts = self.layouts.read();
            let rect = get_rect(&layouts, self.id).unwrap();
            let info = rect.print_info().unwrap();
            let info = info.read();
            let width = rect.br().x - rect.tl().x;
            let height = rect.br().y - rect.tl().y;
            (*info, width, height)
        };

        let info = scroll_ver_around(info, w, h, point, text, IterCfg::new(cfg).outsource_lfs());
        let info = scroll_hor_around(info, w, point, text, IterCfg::new(cfg).outsource_lfs());

        let layouts = self.layouts.read();
        let rect = get_rect(&layouts, self.id).unwrap();
        let mut old_info = rect.print_info().unwrap().write();
        *old_info = info;
        old_info.prev_main = point;
        old_info.last_points = None;
    }

    fn set_as_active(&self) {
        let mut layouts = self.layouts.write();
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
        let layouts = self.layouts.read();
        let layout = get_layout(&layouts, self.id).unwrap();
        *layout.get(self.id).unwrap().print_info().unwrap().write() = info;
    }

    fn print_iter<'a>(
        &self,
        iter: Iter<'a>,
        cfg: IterCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        let points = iter.points();
        print_iter(iter, cfg.wrap_width(self.width()), cfg, points)
    }

    fn print_iter_from_top<'a>(
        &self,
        text: &'a Text,
        cfg: IterCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        let (info, width) = {
            let layouts = self.layouts.read();
            let rect = get_rect(&layouts, self.id).unwrap();
            let info = rect.print_info().unwrap();
            let info = info.read();
            (*info, rect.width())
        };
        let line_start = text.visual_line_start(info.points);
        let iter = text.iter_fwd(line_start);

        print_iter(iter, cfg.wrap_width(width), cfg, info.points)
    }

    fn rev_print_iter<'a>(
        &self,
        iter: RevIter<'a>,
        cfg: IterCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        rev_print_iter(iter, cfg.wrap_width(self.width()), cfg)
    }

    fn has_changed(&self) -> bool {
        let layouts = self.layouts.read();
        get_rect(&layouts, self.id).unwrap().has_changed()
    }

    ////////// Queries

    fn is_master_of(&self, other: &Self) -> bool {
        if other.id == self.id {
            return true;
        }
        let layouts = self.layouts.read();
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
        let layouts = self.layouts.read();
        get_layout(&layouts, self.id)
            .unwrap()
            .rects
            .get_cluster_master(self.id)
            .map(|id| Area::new(id, self.layouts.clone()))
    }

    fn cache(&self) -> Option<Self::Cache> {
        let layouts = self.layouts.read();
        get_rect(&layouts, self.id)
            .unwrap()
            .print_info()
            .map(|info| *info.read())
    }

    fn width(&self) -> u32 {
        get_rect(&self.layouts.read(), self.id).unwrap().width()
    }

    fn height(&self) -> u32 {
        get_rect(&self.layouts.read(), self.id).unwrap().height()
    }

    fn first_points(&self, _text: &Text, _cfg: PrintCfg) -> (Point, Option<Point>) {
        let layouts = self.layouts.read();
        let rect = get_rect(&layouts, self.id).unwrap();
        let info = rect.print_info().unwrap();
        let info = info.read();
        info.points
    }

    fn last_points(&self, text: &Text, cfg: PrintCfg) -> (Point, Option<Point>) {
        let layouts = self.layouts.read();
        let rect = get_rect(&layouts, self.id).unwrap();
        let (height, width) = (rect.height(), rect.width());
        let info = rect.print_info().unwrap();
        let mut info = info.write();

        if let Some(last) = info.last_points {
            return last;
        }

        let line_start = text.visual_line_start(info.points);
        let iter = text.iter_fwd(line_start);
        let cfg = IterCfg::new(cfg);

        let iter = print_iter(iter, cfg.wrap_width(width), cfg, info.points);
        let mut points = info.points;
        let mut y = 0;

        for (Caret { wrap, .. }, Item { part, real, ghost }) in iter {
            if wrap {
                if y == height {
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

    fn print_info(&self) -> Self::PrintInfo {
        let layouts = self.layouts.read();
        *get_rect(&layouts, self.id)
            .unwrap()
            .print_info()
            .unwrap()
            .read()
    }

    fn is_active(&self) -> bool {
        get_layout(&self.layouts.read(), self.id).unwrap().active_id == self.id
    }
}

// NOTE: The defaultness in here, when it comes to `last_main`, may
// cause issues in the future.
/// Information about how to print the file on the `Label`.
#[derive(Default, Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(crate = "duat_core::cache::serde")]
pub struct PrintInfo {
    /// The index of the first [`char`] that should be printed on
    /// the screen.
    points: (Point, Option<Point>),
    /// How shifted the text is to the left.
    x_shift: u32,
    /// The last position of the main cursor.
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
    mut info: PrintInfo,
    width: u32,
    height: u32,
    point: Point,
    text: &Text,
    cfg: IterCfg,
) -> PrintInfo {
    let points = text.ghost_max_points_at(point.byte());
    let after = text.points_after(points).unwrap_or(text.len_points());

    let cap = cfg.wrap_width(width);
    let mut iter = rev_print_iter(text.iter_rev(after), cap, cfg)
        .filter_map(|(caret, item)| caret.wrap.then_some(item.points()));

    let target = match info.prev_main > point {
        true => cfg.scrolloff().y(),
        false => height.saturating_sub(cfg.scrolloff().y() + 1),
    };
    let first = iter.nth(target as usize).unwrap_or_default();

    if (info.prev_main > point && first <= info.points)
        || (info.prev_main < point && first >= info.points)
    {
        info.points = first;
    }
    info
}

/// Scrolls the file horizontally, usually when no wrapping is
/// being used.
fn scroll_hor_around(
    mut info: PrintInfo,
    width: u32,
    point: Point,
    text: &Text,
    cfg: IterCfg,
) -> PrintInfo {
    let cap = cfg.wrap_width(width);

    let (max_shift, start, end) = {
        let points = text.ghost_max_points_at(point.byte());
        let after = text.points_after(points).unwrap_or(text.len_points());

        let mut iter = rev_print_iter(text.iter_rev(after), cap, cfg);

        let (points, start, end, wrap) = iter
            .find_map(|(Caret { x, len, wrap }, item)| {
                let points = item.points();
                item.part.as_char().and(Some((points, x, x + len, wrap)))
            })
            .unwrap_or(((Point::default(), None), 0, 0, true));

        let (line_len, align) = {
            let mut align = Alignment::Left;
            let (indent, points) = if wrap {
                (start, points)
            } else {
                iter.find_map(|(caret, item)| caret.wrap.then_some((caret.x, item.points())))
                    .unwrap()
            };

            let len = print_iter_indented(text.iter_fwd(points), cap, cfg, indent)
                .inspect(|(_, Item { part, .. })| match part {
                    Part::AlignLeft => align = Alignment::Left,
                    Part::AlignCenter => align = Alignment::Center,
                    Part::AlignRight => align = Alignment::Right,
                    _ => {}
                })
                .take_while(|(caret, item)| !caret.wrap || item.points() == points)
                .last()
                .map(|(Caret { x, len, .. }, _)| x + len)
                .unwrap_or(0);

            (len, align)
        };

        let diff = match align {
            Alignment::Left => 0,
            Alignment::Right => cap - line_len,
            Alignment::Center => (cap - line_len) / 2,
        };

        (line_len + diff, start + diff, end + diff)
    };

    info.x_shift = info
        .x_shift
        .min(start.saturating_sub(cfg.scrolloff().x()))
        .max(if cfg.forced_scrollof() {
            (end + cfg.scrolloff().x()).saturating_sub(width)
        } else {
            (end + cfg.scrolloff().x())
                .min(max_shift)
                .saturating_sub(width)
        });
    info
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

fn transfer_vars_and_recvs(from_p: &mut Printer, to_p: &mut Printer, rect: &Rect) {
    let (vars, recv) = from_p.take_rect_parts(rect);
    to_p.insert_rect_parts(vars, recv);

    for (child, _) in rect.children().into_iter().flatten() {
        transfer_vars_and_recvs(from_p, to_p, child)
    }
}
