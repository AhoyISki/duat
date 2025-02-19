mod iter;

use std::{fmt::Alignment, io::Write};

use crossterm::cursor;
use duat_core::{
    cache::{Deserialize, Serialize},
    cfg::{IterCfg, PrintCfg},
    data::RwData,
    form::Painter,
    text::{Item, Iter, Part, Point, RevIter, Text},
    ui::{self, Area as UiArea, Axis, Caret, Constraint, PushSpecs},
};
use iter::{print_iter, print_iter_indented, rev_print_iter};

use crate::{AreaId, ConstraintErr, layout::Layout, queue, style};

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
    layout: RwData<Layout>,
    pub id: AreaId,
}

impl PartialEq for Area {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Area {
    pub fn new(index: AreaId, layout: RwData<Layout>) -> Self {
        Self { layout, id: index }
    }

    fn print<'a>(
        &self,
        text: &mut Text,
        cfg: PrintCfg,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        if text.needs_update() {
            let first_point = self.first_point(text, cfg);
            let last_point = self.last_point(text, cfg);
            text.update_range((first_point, last_point));
        }

        let layout = self.layout.read();
        let Some((sender, info)) = layout.rects.get(self.id).and_then(|rect| {
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
        let active = layout.active_id == self.id;
        let iter = print_iter(iter, cap, cfg, info.points);

        let mut lines = sender.lines(info.x_shift, cap);
        let mut cur_style = None;

        enum Cursor {
            Main,
            Extra,
        }

        let lines_left = {
            let (mut painter, mut f) = (painter, f);
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
                        lines.flush().unwrap();
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
                            && active
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
                lines.flush().unwrap();
            }

            sender.coords().br.y - y
        };

        for _ in 0..lines_left {
            lines.flush().unwrap();
        }

        sender.send(lines);
    }
}

impl ui::Area for Area {
    type Cache = PrintInfo;
    type ConstraintChangeErr = ConstraintErr;
    type PrintInfo = PrintInfo;
    type Ui = crate::Ui;

    fn cache(&self) -> Option<Self::Cache> {
        self.layout
            .read()
            .get(self.id)
            .unwrap()
            .print_info()
            .map(|info| *info.read())
    }

    fn width(&self) -> u32 {
        self.layout.inspect(|layout| {
            let rect = layout.get(self.id).unwrap();
            rect.br().x - rect.tl().x
        })
    }

    fn height(&self) -> u32 {
        self.layout.inspect(|window| {
            let rect = window.get(self.id).unwrap();
            rect.br().y - rect.tl().y
        })
    }

    fn scroll_around_point(&self, text: &Text, point: Point, cfg: PrintCfg) {
        let (info, w, h) = {
            let layout = self.layout.read();
            let rect = layout.get(self.id).unwrap();
            let info = rect.print_info().unwrap();
            let info = info.read();
            let width = rect.br().x - rect.tl().x;
            let height = rect.br().y - rect.tl().y;
            (*info, width, height)
        };

        let info = scroll_ver_around(info, w, h, point, text, IterCfg::new(cfg).outsource_lfs());
        let info = scroll_hor_around(info, w, point, text, IterCfg::new(cfg).outsource_lfs());

        let layout = self.layout.read();
        let rect = layout.get(self.id).unwrap();
        let mut old_info = rect.print_info().unwrap().write();
        *old_info = info;
        old_info.last_main = point;
        old_info.last_point = None;
    }

    fn top_left(&self) -> (Point, Option<Point>) {
        let layout = self.layout.read();
        let rect = layout.get(self.id).unwrap();
        let info = rect.print_info().unwrap();
        let info = info.read();
        info.points
    }

    fn set_as_active(&self) {
        self.layout.write().active_id = self.id;
    }

    fn is_active(&self) -> bool {
        self.layout.read().active_id == self.id
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

    fn constrain_ver(&self, con: Constraint) -> Result<(), ConstraintErr> {
        let mut layout = self.layout.write();
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

        let cons = {
            let mut p = layout.printer.write();
            let cons = cons.replace(con, Axis::Vertical, &mut p);

            let (_, parent) = layout.get_parent(self.id).unwrap();
            let rect = layout.get(self.id).unwrap();

            let cons = cons.apply(rect, parent.id(), &layout.rects, &mut p);
            p.flush_equalities().unwrap();
            cons
        };

        *layout.rects.get_constraints_mut(self.id).unwrap() = cons;

        Ok(())
    }

    fn constrain_hor(&self, con: Constraint) -> Result<(), ConstraintErr> {
        let mut layout = self.layout.write();
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

        let cons = {
            let mut p = layout.printer.write();
            let cons = cons.replace(con, Axis::Horizontal, &mut p);

            let (_, parent) = layout.get_parent(self.id).unwrap();
            let rect = layout.get(self.id).unwrap();

            let cons = cons.apply(rect, parent.id(), &layout.rects, &mut p);
            p.flush_equalities().unwrap();
            cons
        };

        *layout.rects.get_constraints_mut(self.id).unwrap() = cons;

        Ok(())
    }

    fn restore_constraints(&self) -> Result<(), Self::ConstraintChangeErr> {
        todo!();
    }

    fn request_width_to_fit(&self, _text: &str) -> Result<(), Self::ConstraintChangeErr> {
        todo!();
    }

    fn has_changed(&self) -> bool {
        self.layout.read().get(self.id).unwrap().has_changed()
    }

    fn is_master_of(&self, other: &Self) -> bool {
        self.layout.inspect(|layout| {
            let mut parent_id = other.id;
            while let Some((_, parent)) = layout.get_parent(parent_id) {
                parent_id = parent.id();
                if parent.id() == self.id {
                    return true;
                }
            }

            parent_id == self.id
        })
    }

    fn get_cluster_master(&self) -> Option<Self> {
        let clone = self.layout.clone();
        self.layout.inspect(|layout| {
            let (_, mut parent) = layout
                .get_parent(self.id)
                .filter(|(_, p)| p.is_clustered())?;

            loop {
                if let Some((_, gp)) = layout.get_parent(parent.id())
                    && gp.is_clustered()
                {
                    parent = gp
                } else {
                    break Some(Area::new(parent.id(), clone));
                }
            }
        })
    }

    fn bisect(
        &self,
        specs: PushSpecs,
        cluster: bool,
        on_files: bool,
        cache: PrintInfo,
    ) -> (Area, Option<Area>) {
        let mut layout = self.layout.write();
        let layout = &mut *layout;

        let (child, parent) = layout.bisect(self.id, specs, cluster, on_files, cache);

        (
            Area::new(child, self.layout.clone()),
            parent.map(|parent| Area::new(parent, self.layout.clone())),
        )
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
        let info = {
            let layout = self.layout.read();
            let rect = layout.get(self.id).unwrap();
            let info = rect.print_info().unwrap();
            let info = info.read();
            *info
        };
        let line_start = text.visual_line_start(info.points);
        let iter = text.iter_fwd(line_start);

        print_iter(iter, cfg.wrap_width(self.width()), cfg, info.points)
    }

    fn rev_print_iter<'a>(
        &self,
        iter: RevIter<'a>,
        cfg: IterCfg,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        rev_print_iter(iter, cfg.wrap_width(self.width()), cfg)
    }

    fn print_info(&self) -> Self::PrintInfo {
        let layout = self.layout.read();
        let info = layout.get(self.id).unwrap().print_info().unwrap().read();
        *info
    }

    fn set_print_info(&self, info: Self::PrintInfo) {
        let layout = self.layout.read();
        *layout.get(self.id).unwrap().print_info().unwrap().write() = info;
    }

    fn first_point(&self, _text: &Text, _cfg: PrintCfg) -> Point {
        let layout = self.layout.read();
        let rect = layout.get(self.id).unwrap();
        let info = rect.print_info().unwrap();
        let info = info.read();
        info.points.0
    }

    fn last_point(&self, text: &Text, cfg: PrintCfg) -> Point {
        let layout = self.layout.read();
        let rect = layout.get(self.id).unwrap();
        let info = rect.print_info().unwrap();
        let mut info = info.write();

        if let Some(last) = info.last_point {
            return last;
        }

        let line_start = text.visual_line_start(info.points);
        let iter = text.iter_fwd(line_start);
        let cfg = IterCfg::new(cfg);

        let iter = print_iter(iter, cfg.wrap_width(self.width()), cfg, info.points);
        let mut point = info.points.0;
        let mut y = 0;
        let height = self.height();

        for (Caret { wrap, .. }, Item { part, real, .. }) in iter {
            if wrap {
                if y == height {
                    break;
                }
                if part.is_char() {
                    y += 1
                }
            } else {
                point = real;
            }
        }

        info.last_point = Some(point);

        point
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
    last_main: Point,
    last_point: Option<Point>,
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

    let target = match info.last_main > point {
        true => cfg.scrolloff().y(),
        false => height.saturating_sub(cfg.scrolloff().y() + 1),
    };
    let first = iter.nth(target as usize).unwrap_or_default();

    if (info.last_main > point && first <= info.points)
        || (info.last_main < point && first >= info.points)
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
