mod iter;
mod line;

use std::{
    fmt::Alignment,
    io::Write,
    sync::{atomic::Ordering, LazyLock},
};

use crossterm::{
    cursor,
    style::{Print, ResetColor, SetStyle},
};
use duat_core::{
    data::RwData,
    palette::{self, FormId, Painter},
    text::{Item, Iter, IterCfg, Part, Point, PrintCfg, RevIter, Text},
    ui::{self, Axis, Caret, Constraint, PushSpecs},
};
use iter::{print_iter, print_iter_indented, rev_print_iter};

use crate::{
    layout::{Brush, Edge, EdgeCoords, Layout},
    AreaId, ConstraintErr, RESIZED,
};

macro_rules! queue {
    ($writer:expr $(, $command:expr)* $(,)?) => {
        unsafe { crossterm::queue!($writer $(, $command)*).unwrap_unchecked() }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Coord {
    pub y: usize,
    pub x: usize,
}

impl std::fmt::Debug for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("x: {}, y: {}", self.x, self.y))
    }
}

impl Coord {
    pub fn new(x: usize, y: usize) -> Coord {
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

    pub fn width(&self) -> usize {
        self.br.x - self.tl.x
    }

    pub fn height(&self) -> usize {
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
    pub layout: RwData<Layout>,
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
        text: &Text,
        cfg: &PrintCfg,
        painter: Painter,
        f: impl FnMut(&Caret, &Item) + 'a,
    ) {
        if RESIZED.fetch_and(false, Ordering::Acquire) {
            let mut layout = self.layout.write();
            layout.resize();
            print_edges(layout.edges());
        }

        let layout = self.layout.read();
        let Some((sender, info)) = layout.rects.get(self.id).and_then(|rect| {
            let sender = rect.sender();
            let info = rect.print_info();
            sender.zip(info).map(|(sender, info)| {
                let info = info.read();
                (sender, *info)
            })
        }) else {
            return;
        };

        let (iter, cfg) = {
            let line_start = text.visual_line_start(info.points);
            (text.iter_at(line_start), IterCfg::new(cfg).outsource_lfs())
        };

        let cap = cfg.wrap_width(sender.coords().width());
        let active = layout.active_id == self.id;
        let iter = print_iter(iter, cap, cfg, info.points);

        let mut lines = sender.lines(info.x_shift, cap);

        let lines_left = {
            let (mut painter, mut f) = (painter, f);
            // The y here represents the bottom of the current row of cells.
            let mut y = sender.coords().tl.y;
            let mut prev_style = None;

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
                    queue!(lines, SetStyle(painter.make_form().style));
                    if part.is_char() {
                        y += 1
                    }
                }

                match part {
                    Part::Char(char) => {
                        match char {
                            '\t' => (0..len).for_each(|_| lines.push_char(' ', 1)),
                            '\n' => {}
                            char => lines.push_char(char, len),
                        }
                        if let Some(style) = prev_style.take() {
                            queue!(lines, ResetColor, SetStyle(style))
                        }
                    }
                    Part::PushForm(id) => {
                        queue!(lines, ResetColor, SetStyle(painter.apply(id).style));
                    }
                    Part::PopForm(id) => {
                        queue!(lines, ResetColor, SetStyle(painter.remove(id).style))
                    }
                    Part::MainCursor => {
                        let (form, shape) = painter.main_cursor();
                        if let (Some(shape), true) = (shape, active) {
                            lines.show_real_cursor();
                            queue!(lines, shape, cursor::SavePosition);
                        } else {
                            lines.hide_real_cursor();
                            queue!(lines, ResetColor, SetStyle(form.style));
                            prev_style = Some(painter.make_form().style);
                        }
                    }
                    Part::ExtraCursor => {
                        queue!(lines, SetStyle(painter.extra_cursor().0.style));
                        prev_style = Some(painter.make_form().style);
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
                    Part::Termination => {
                        queue!(lines, SetStyle(painter.reset().style))
                    }
                    Part::ToggleStart(_) => todo!(),
                    Part::ToggleEnd(_) => todo!(),
                    _ => {}
                }
            }

            if !lines.is_empty() {
                if cfg.ending_space() {
                    lines.push_char(' ', 1)
                }
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
    type ConstraintChangeErr = ConstraintErr;

    fn width(&self) -> usize {
        self.layout.inspect(|layout| {
            let rect = layout.get(self.id).unwrap();
            rect.br().x - rect.tl().x
        })
    }

    fn height(&self) -> usize {
        self.layout.inspect(|window| {
            let rect = window.get(self.id).unwrap();
            rect.br().y - rect.tl().y
        })
    }

    fn scroll_around_point(&self, text: &Text, point: Point, cfg: &PrintCfg) {
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

    fn print(&self, text: &Text, cfg: &PrintCfg, painter: Painter) {
        self.print(text, cfg, painter, |_, _| {})
    }

    fn print_with<'a>(
        &self,
        text: &Text,
        cfg: &PrintCfg,
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

    fn bisect(&self, specs: PushSpecs, cluster: bool, on_files: bool) -> (Area, Option<Area>) {
        let mut layout = self.layout.write();
        let layout = &mut *layout;

        let (child, parent) = layout.bisect(self.id, specs, cluster, on_files);

        print_edges(layout.edges());

        (
            Area::new(child, self.layout.clone()),
            parent.map(|parent| Area::new(parent, self.layout.clone())),
        )
    }

    fn print_iter<'a>(
        &self,
        iter: Iter<'a>,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        let points = iter.points();
        print_iter(iter, cfg.wrap_width(self.width()), cfg, points)
    }

    fn print_iter_from_top<'a>(
        &self,
        text: &'a Text,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        let info = {
            let layout = self.layout.read();
            let rect = layout.get(self.id).unwrap();
            let info = rect.print_info().unwrap();
            let info = info.read();
            *info
        };
        let line_start = text.visual_line_start(info.points);
        let iter = text.iter_at(line_start);

        print_iter(iter, cfg.wrap_width(self.width()), cfg, info.points)
    }

    fn rev_print_iter<'a>(
        &self,
        iter: RevIter<'a>,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        rev_print_iter(iter, cfg.wrap_width(self.width()), cfg)
    }
}

unsafe impl Send for Area {}
unsafe impl Sync for Area {}

// NOTE: The defaultness in here, when it comes to `last_main`, may
// cause issues in the future.
/// Information about how to print the file on the `Label`.
#[derive(Default, Debug, Clone, Copy)]
pub struct PrintInfo {
    /// The index of the first [`char`] that should be printed on the
    /// screen.
    points: (Point, Option<Point>),
    /// How shifted the text is to the left.
    x_shift: usize,
    /// The last position of the main cursor.
    last_main: Point,
}

fn print_edges(edges: &[Edge]) {
    static FRAME_FORM: LazyLock<FormId> =
        LazyLock::new(|| palette::set_weak_ref("Frame", "Default"));
    let frame_form = palette::form_from_id(*FRAME_FORM);

    let mut stdout = std::io::stdout().lock();

    let edges = edges
        .iter()
        .map(|edge| edge.line_coords())
        .collect::<Vec<EdgeCoords>>();

    let mut crossings = Vec::<(
        Coord,
        Option<Brush>,
        Option<Brush>,
        Option<Brush>,
        Option<Brush>,
    )>::new();

    for (index, &coords) in edges.iter().enumerate() {
        if let Axis::Horizontal = coords.axis {
            let char = match coords.line {
                Some(line) => line::horizontal(line, line),
                None => unreachable!(),
            };
            let line = char.to_string().repeat(coords.br.x - coords.tl.x + 1);
            queue!(
                stdout,
                cursor::MoveTo(coords.tl.x as u16, coords.tl.y as u16),
                ResetColor,
                SetStyle(frame_form.style),
                Print(line)
            )
        } else {
            let char = match coords.line {
                Some(line) => line::vertical(line, line),
                None => unreachable!(),
            };

            for y in (coords.tl.y)..=coords.br.y {
                queue!(
                    stdout,
                    cursor::MoveTo(coords.tl.x as u16, y as u16),
                    ResetColor,
                    SetStyle(frame_form.style),
                    Print(char)
                )
            }
        }

        for (other_index, &other_coords) in edges.iter().enumerate() {
            if index == other_index {
                continue;
            }

            if let Some(crossing) = coords.crossing(other_coords) {
                let prev_crossing = crossings
                    .iter_mut()
                    .find(|(coord, ..)| *coord == crossing.0);
                if let Some((_, right, up, left, down)) = prev_crossing {
                    *right = right.or(crossing.1);
                    *up = up.or(crossing.2);
                    *left = left.or(crossing.3);
                    *down = down.or(crossing.4);
                } else {
                    crossings.push(crossing);
                }
            }
        }
    }

    for (coord, right, up, left, down) in crossings {
        queue!(
            stdout,
            cursor::MoveTo(coord.x as u16, coord.y as u16),
            SetStyle(frame_form.style),
            Print(line::crossing(right, up, left, down, true))
        )
    }
}

/// Scrolls down until the gap between the main cursor and the
/// bottom of the widget is equal to `config.scrolloff.y_gap`.
fn scroll_ver_around(
    mut info: PrintInfo,
    width: usize,
    height: usize,
    point: Point,
    text: &Text,
    cfg: IterCfg,
) -> PrintInfo {
    let points = text.ghost_max_points_at(point.byte());
    let after = text.points_after(points).unwrap_or(text.len_points());

    let cap = cfg.wrap_width(width);
    let mut iter = rev_print_iter(text.rev_iter_at(after), cap, cfg)
        .filter_map(|(caret, item)| caret.wrap.then_some(item.points()));

    let target = match info.last_main > point {
        true => cfg.scrolloff().y,
        false => height.saturating_sub(cfg.scrolloff().y + 1),
    };
    let first = iter.nth(target).unwrap_or_default();

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
    width: usize,
    point: Point,
    text: &Text,
    cfg: IterCfg,
) -> PrintInfo {
    let cap = cfg.wrap_width(width);

    let (max_shift, start, end) = {
        let points = text.ghost_max_points_at(point.byte());
        let after = text.points_after(points).unwrap_or(text.len_points());

        let mut iter = rev_print_iter(text.rev_iter_at(after), cap, cfg);

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

            let len = print_iter_indented(text.iter_at(points), cap, cfg, indent)
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
        .min(start.saturating_sub(cfg.scrolloff().x))
        .max(if cfg.forced_scrollof() {
            (end + cfg.scrolloff().x).saturating_sub(width)
        } else {
            (end + cfg.scrolloff().x)
                .min(max_shift)
                .saturating_sub(width)
        });
    info
}
