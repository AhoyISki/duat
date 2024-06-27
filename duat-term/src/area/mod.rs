mod iter;
mod line;

use std::{cell::RefCell, fmt::Alignment, io::Write, sync::atomic::Ordering};

use crossterm::{
    cursor,
    style::{Print, ResetColor, SetStyle},
};
use duat_core::{
    data::RwData,
    palette::Painter,
    position::Point,
    text::{ExactPos, Item, IterCfg, Part, PrintCfg, Text, WrapMethod},
    ui::{self, Area as UiArea, Axis, Caret, Constraint, PushSpecs},
};
use iter::{print_iter, rev_print_iter};

use crate::{
    layout::{Brush, Edge, EdgeCoords, Layout},
    print::Lines,
    AreaId, ConstraintChangeErr, RESIZED,
};

static BLANK: &str = unsafe { std::str::from_utf8_unchecked(&[b' '; 1000]) };

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
    print_info: RefCell<PrintInfo>,
    pub id: AreaId,
}

impl PartialEq for Area {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Area {
    pub fn new(index: AreaId, layout: RwData<Layout>) -> Self {
        Self {
            layout,
            print_info: RefCell::new(PrintInfo::default()),
            id: index,
        }
    }

    /// Scrolls down until the gap between the main cursor and the
    /// bottom of the widget is equal to `config.scrolloff.y_gap`.
    fn scroll_ver_around(&self, point: Point, text: &Text, cfg: IterCfg) {
        let mut info = self.print_info.borrow_mut();

        let mut str = String::new();
        let pos = ExactPos::at_cursor_char(point.char());
        let mut iter = rev_print_iter(text.rev_iter_following(pos), self.width(), cfg)
            .inspect(|(_, item)| {
                item.part.as_byte().inspect(|char| str.insert(0, *char));
            })
            .filter_map(|(caret, item)| caret.wrap.then_some(item))
            .peekable();

        let nl_on_point_was_concealed = iter
            .peek()
            .is_some_and(|item| item.line < point.line() && point.col() == 0);

        let mut iter = iter.map(|item| item.pos);

        let target = if info.last_main > point {
            cfg.scrolloff().y
        } else {
            self.height().saturating_sub(cfg.scrolloff().y + 1)
        };

        let first = if nl_on_point_was_concealed {
            let exact = ExactPos::at_cursor_char(point.char());
            let skipped_nl = std::iter::once(exact);
            skipped_nl
                .chain(iter)
                .nth(target)
                .unwrap_or(ExactPos::default())
        } else {
            iter.nth(target).unwrap_or(ExactPos::default())
        };

        if (info.last_main > point && first <= info.first)
            || (info.last_main < point && first >= info.first)
        {
            info.first = first;
        }
    }

    /// Scrolls the file horizontally, usually when no wrapping is
    /// being used.
    fn scroll_hor_around(&self, point: Point, text: &Text, cfg: IterCfg) {
        let mut info = self.print_info.borrow_mut();

        let width = self.width();
        let max_x_shift = match cfg.wrap_method() {
            WrapMethod::Width | WrapMethod::Word => return,
            WrapMethod::Capped(cap) => {
                if cap > width {
                    cap - width
                } else {
                    return;
                }
            }
            WrapMethod::NoWrap => usize::MAX,
        };

        let (line_start, start, end) = {
            let inclusive_pos = point.char() + 1;
            let mut iter = rev_print_iter(text.rev_iter_at(inclusive_pos), width, cfg).scan(
                false,
                |wrapped, (caret, item)| {
                    let prev_wrapped = *wrapped;
                    *wrapped = caret.wrap;
                    (!prev_wrapped).then_some((caret, item))
                },
            );

            let (pos, start, end) = iter
                .find_map(|(Caret { x, len, .. }, Item { pos, part, .. })| {
                    part.as_byte().and(Some((pos, x, x + len)))
                })
                .unwrap_or((ExactPos::default(), 0, 0));

            let line_start = iter
                .find_map(|(Caret { wrap, .. }, Item { pos, .. })| wrap.then_some(pos))
                .unwrap_or(pos);

            (line_start, start, end)
        };

        let max_dist = width - cfg.scrolloff().x;
        let min_dist = info.x_shift + cfg.scrolloff().x;

        if start < min_dist {
            info.x_shift = info.x_shift.saturating_sub(min_dist - start);
        } else if end < start {
            info.x_shift = info.x_shift.saturating_sub(min_dist - end);
        } else if end > info.x_shift + max_dist {
            let line_width = print_iter(text.iter_at(line_start), width, cfg, *info)
                .try_fold(
                    (0, 0),
                    |(right_end, some_count), (Caret { x, len, wrap }, _)| {
                        let some_count = some_count + wrap as usize;
                        match some_count < 2 {
                            true => std::ops::ControlFlow::Continue((x + len, some_count)),
                            false => std::ops::ControlFlow::Break(right_end),
                        }
                    },
                );

            if let std::ops::ControlFlow::Break(line_width) = line_width
                && line_width <= width
            {
                return;
            }
            info.x_shift = end - max_dist;
        }

        info.x_shift = info.x_shift.min(max_x_shift);
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
        let info = self.print_info.borrow();

        let Some(sender) = layout.rects.get(self.id).and_then(|rect| rect.sender()) else {
            return;
        };
        let mut lines = sender.lines();

        let (iter, cfg) = {
            let line_start = text.visual_line_start(info.first);
            let cfg = IterCfg::new(cfg).outsource_lfs();
            (text.iter_at(line_start), cfg)
        };

        let active = layout.active_id == self.id;
        let iter = print_iter(iter, sender.coords().width(), cfg, *info);
        let y = print_parts(iter, sender.coords(), active, *info, painter, &mut lines, f);

        for _ in (0..y).rev() {
            lines
                .write_all(&BLANK.as_bytes()[..sender.coords().width()])
                .unwrap();

            lines.flush().unwrap();
        }

        sender.send(lines);
    }
}

impl ui::Area for Area {
    type ConstraintChangeErr = ConstraintChangeErr;

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
        self.scroll_ver_around(point, text, IterCfg::new(cfg).outsource_lfs());
        self.scroll_hor_around(point, text, IterCfg::new(cfg).outsource_lfs());

        self.print_info.borrow_mut().last_main = point;
    }

    fn first_char(&self) -> usize {
        self.print_info.borrow().first.real()
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

    fn change_constraint(
        &self,
        constraint: Constraint,
        axis: Axis,
    ) -> Result<(), ConstraintChangeErr> {
        if self
            .layout
            .read()
            .rects
            .get_constraint(self.id)
            .is_some_and(|cmp| cmp == (constraint, axis))
        {
            return Ok(());
        };

        let mut layout = self.layout.write();
        let layout = &mut *layout;
        let mut printer = layout.printer.write();
        let prev_cons = layout
            .rects
            .set_constraint(self.id, constraint, axis, &mut printer);

        if prev_cons.map_or(true, |cmp| cmp != (constraint, axis)) && printer.update(false) {
            drop(printer);
            layout.resize();

            print_edges(layout.edges());
        }

        Ok(())
    }

    fn request_width_to_fit(&self, _text: &str) -> Result<(), Self::ConstraintChangeErr> {
        todo!();
    }

    fn has_changed(&self) -> bool {
        self.layout.read().get(self.id).unwrap().has_changed()
    }

    fn is_senior_of(&self, other: &Self) -> bool {
        self.layout.inspect(|layout| {
            let mut parent_index = other.id;
            while let Some((_, parent)) = layout.get_parent(parent_index) {
                parent_index = parent.index();
                if parent.index() == self.id {
                    return true;
                }
            }

            parent_index == self.id
        })
    }

    fn bisect(&self, specs: PushSpecs, is_glued: bool) -> (Area, Option<Area>) {
        let mut layout = self.layout.write();
        let layout = &mut *layout;

        let (child, parent) = layout.bisect(self.id, specs, is_glued);

        layout.printer.mutate(|printer| {
            printer.update(false);
            layout.rects.set_senders(printer);
        });

        print_edges(layout.edges());

        (
            Area::new(child, self.layout.clone()),
            parent.map(|parent| Area::new(parent, self.layout.clone())),
        )
    }

    fn print_iter<'a>(
        &self,
        iter: impl Iterator<Item = Item> + Clone + 'a,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        print_iter(iter, self.width(), cfg, PrintInfo::default())
    }

    fn print_iter_from_top<'a>(
        &self,
        text: &'a Text,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        let info = self.print_info.borrow();
        let line_start = text.visual_line_start(info.first);
        let iter = text.iter_at(line_start);

        print_iter(iter, self.width(), cfg, *info)
    }

    fn rev_print_iter<'a>(
        &self,
        iter: impl Iterator<Item = Item> + Clone + 'a,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        rev_print_iter(iter, self.width(), cfg)
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
    first: ExactPos,
    /// How shifted the text is to the left.
    x_shift: usize,
    /// The last position of the main cursor.
    last_main: Point,
}

fn print_parts<'a>(
    iter: impl Iterator<Item = (Caret, Item)>,
    coords: Coords,
    is_active: bool,
    info: PrintInfo,
    mut painter: Painter,
    lines: &mut Lines,
    mut f: impl FnMut(&Caret, &Item) + 'a,
) -> usize {
    let mut old_x = coords.tl.x;
    // The y here represents the bottom line of the current row of cells.
    let mut y = coords.tl.y;
    let mut prev_style = None;
    let mut alignment = Alignment::Left;
    let mut line = Vec::new();

    for (caret, item) in iter {
        f(&caret, &item);

        let Caret { x, len, wrap } = caret;
        let Item { part, .. } = item;

        if wrap {
            if y > coords.tl.y {
                let shifted_x = old_x.saturating_sub(info.x_shift);
                print_line(shifted_x, coords, alignment, &mut line, lines);
            }
            if y == coords.br.y {
                break;
            }
            indent_line(&painter, x, info.x_shift, &mut line);
            y += 1;
        }

        old_x = coords.tl.x + x + len;

        match part {
            // Char
            Part::Byte(char) => {
                if len > 0 {
                    write_char(char, coords.tl.x + x, len, coords, info.x_shift, &mut line);
                    if let Some(style) = prev_style.take() {
                        queue!(&mut line, ResetColor, SetStyle(style))
                    }
                }
            }

            // Tags
            Part::PushForm(id) => {
                if let Some(form) = painter.apply(id) {
                    queue!(line, ResetColor, SetStyle(form.style));
                }
            }
            Part::PopForm(id) => {
                if let Some(form) = painter.remove(id) {
                    queue!(line, ResetColor, SetStyle(form.style))
                }
            }
            Part::MainCursor => {
                let (form, shape) = painter.main_cursor();
                if let (Some(shape), true) = (shape, is_active) {
                    lines.show_real_cursor();
                    queue!(line, shape, cursor::SavePosition);
                } else {
                    lines.hide_real_cursor();
                    queue!(line, SetStyle(form.style));
                    prev_style = Some(painter.make_form().style);
                }
            }
            Part::ExtraCursor => {
                queue!(line, SetStyle(painter.extra_cursor().0.style));
                prev_style = Some(painter.make_form().style);
            }
            Part::AlignLeft => alignment = Alignment::Left,
            Part::AlignCenter => alignment = Alignment::Center,
            Part::AlignRight => alignment = Alignment::Right,
            Part::Termination => {
                alignment = Alignment::Left;
                if let Some(form) = painter.reset() {
                    queue!(line, SetStyle(form.style));
                }
            }
            Part::ToggleStart(_) => todo!(),
            Part::ToggleEnd(_) => todo!(),
        }
    }

    if !line.is_empty() {
        line.write_all(b" ").unwrap();
        print_line(old_x + 1, coords, alignment, &mut line, lines);
    }

    coords.br.y - y
}

#[inline(always)]
fn print_line(
    x: usize,
    coords: Coords,
    alignment: Alignment,
    line: &mut Vec<u8>,
    lines: &mut Lines,
) {
    let remainder = coords.br.x.saturating_sub(x.max(coords.tl.x));

    let (left, right) = match alignment {
        Alignment::Left => (0, remainder),
        Alignment::Right => (remainder, 0),
        Alignment::Center => {
            let left = remainder / 2;
            (left, remainder - left)
        }
    };

    queue!(lines, ResetColor);

    lines.write_all(BLANK[..left].as_bytes()).unwrap();
    lines.write_all(line).unwrap();

    queue!(lines, ResetColor);

    lines.write_all(BLANK[..right].as_bytes()).unwrap();

    lines.flush().unwrap();

    line.clear();
}

#[inline(always)]
fn indent_line(form_former: &Painter, x: usize, x_shift: usize, line: &mut Vec<u8>) {
    let prev_style = form_former.make_form().style;
    let indent_count = x.saturating_sub(x_shift);
    let mut indent = Vec::<u8>::from(&BLANK[..indent_count]);
    queue!(indent, SetStyle(prev_style));
    line.splice(0..0, indent);
}

#[inline(always)]
fn write_char(
    char: char,
    x: usize,
    len: usize,
    coords: Coords,
    x_shift: usize,
    line: &mut Vec<u8>,
) {
    // Case where the cursor hasn't yet reached the left edge.
    if x < coords.tl.x + x_shift {
        let len = (x + len).saturating_sub(coords.tl.x + x_shift);
        line.write_all(BLANK[..len].as_bytes()).unwrap();
    // Case where the end of the cursor, after printing, would be
    // located before the right edge.
    } else if x + len <= coords.br.x + x_shift {
        match char {
            '\t' => line.write_all(BLANK[..len].as_bytes()).unwrap(),
            char => {
                let mut bytes = [0; 4];
                char.encode_utf8(&mut bytes);
                line.write_all(&bytes).unwrap();
            }
        };
    // Case where it wouldn't.
    } else if x < coords.br.x + x_shift {
        let len = coords.br.x + x_shift - x;
        line.write_all(BLANK[..len].as_bytes()).unwrap();
    }
}

fn print_edges(edges: &[Edge]) {
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
            Print(line::crossing(right, up, left, down, true))
        )
    }
}
