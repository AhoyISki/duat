mod iter;
mod line;

use std::{
    fmt::Alignment,
    io::{stdout, StdoutLock},
    sync::atomic::{AtomicBool, Ordering}
};

use crossterm::{
    cursor,
    style::{Print, ResetColor, SetStyle}
};
use iter::{print_iter, rev_print_iter};
use parsec_core::{
    data::{ReadableData, RwData},
    forms::{FormFormer, FormPalette},
    log_info,
    position::Pos,
    text::{Part, PrintCfg, Text, WrapMethod},
    ui::{self, Area as UiArea, Axis, Constraint, PushSpecs}
};

use crate::{
    layout::{Edge, Layout, Line, LineCoords},
    AreaIndex, ConstraintChangeErr
};

static SHOW_CURSOR: AtomicBool = AtomicBool::new(false);

macro_rules! queue {
    ($writer:expr $(, $command:expr)* $(,)?) => {
        unsafe { crossterm::queue!($writer $(, $command)*).unwrap_unchecked() }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct Coord {
    pub x: usize,
    pub y: usize
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

#[derive(Debug, Clone, Copy)]
pub struct Coords {
    tl: Coord,
    br: Coord
}
impl Coords {
    pub fn new(tl: Coord, br: Coord) -> Self {
        Coords { tl, br }
    }

    fn width(&self) -> usize {
        self.br.x - self.tl.x
    }
}

#[derive(Clone)]
pub struct Area {
    pub layout: RwData<Layout>,
    pub index: AreaIndex
}

impl PartialEq for Area {
    fn eq(&self, other: &Self) -> bool {
        self.layout.ptr_eq(&other.layout) && self.index == other.index
    }
}

impl Area {
    pub fn new(layout: RwData<Layout>, index: AreaIndex) -> Self {
        Self { layout, index }
    }

    fn is_active(&self) -> bool {
        self.layout.read().active_index == self.index
    }

    fn coords(&self) -> Coords {
        let layout = self.layout.read();
        let rect = layout.fetch_index(self.index).unwrap();
        let rect = rect.read();

        Coords { tl: rect.tl(), br: rect.br() }
    }
}

impl ui::Area for Area {
    type PrintInfo = PrintInfo;
    type ConstraintChangeErr = ConstraintChangeErr;

    fn width(&self) -> usize {
        self.layout.inspect(|layout| {
            let rect = layout.fetch_index(self.index).unwrap();
            let rect = rect.read();
            rect.br().x - rect.tl().x
        })
    }

    fn height(&self) -> usize {
        self.layout.inspect(|window| {
            let rect = window.fetch_index(self.index).unwrap();
            let rect = rect.read();
            rect.br().y - rect.tl().y
        })
    }

    fn set_as_active(&self) {
        self.layout.write().active_index = self.index;
    }

    fn print(&self, text: &Text, info: PrintInfo, cfg: &PrintCfg, palette: &FormPalette) {
        let coords = self.coords();
        let mut stdout = stdout().lock();
        print_edges(self.layout.read().edges(), &mut stdout);

        if self.is_active() {
            SHOW_CURSOR.store(false, Ordering::Release);
        }

        queue!(stdout, cursor::MoveTo(coords.tl.x as u16, coords.tl.y as u16), cursor::Hide);

        if text.len_chars() == 0 {
            for y in coords.tl.y..coords.br.y {
                clear_line(Coord::new(coords.tl.x, coords.br.y - y), coords, 0, &mut stdout);
            }
            return;
        }

        let iter = {
            let start = text
                .rev_iter_at(info.first_char)
                .find_map(|(pos, .., part)| {
                    part.as_char().filter(|char| *char == '\n').and(Some(pos + 1))
                })
                .unwrap_or(0);
            print_iter(text.iter_at(start), info.first_char, coords.width(), cfg)
        };

        let form_former = palette.form_former();
        let y = print_parts(iter, coords, self.is_active(), info, form_former, &mut stdout);

        for y in (0..y).rev() {
            clear_line(Coord::new(coords.tl.x, coords.br.y - y), coords, 0, &mut stdout);
        }

        if SHOW_CURSOR.load(Ordering::Acquire) {
            queue!(stdout, cursor::RestorePosition, cursor::Show);
        }

        crossterm::execute!(stdout, ResetColor).unwrap();
    }

    fn change_constraint(&self, constraint: Constraint) -> Result<(), ConstraintChangeErr> {
        let mut layout = self.layout.write();
        let (parent, index) =
            layout.fetch_parent(self.index).ok_or(ConstraintChangeErr::NoParent)?;
        if parent.write().change_child_constraint(index, constraint, &mut layout.solver) {
            layout.update();
        }

        Ok(())
    }

    fn request_width_to_fit(&self, _text: &str) -> Result<(), Self::ConstraintChangeErr> {
        todo!();
    }

    // NOTE: INCORRECT FUNCTION!!!
    fn visible_rows(
        &self, iter: impl Iterator<Item = (usize, usize, Part)> + Clone, cfg: &PrintCfg
    ) -> usize {
        print_iter(iter, 0, self.width(), cfg).filter_map(|((.., new_line), _)| new_line).count()
    }

    // NOTE: INCORRECT FUNCTION!!!
    fn char_at_wrap(
        &self, iter: impl Iterator<Item = (usize, usize, Part)> + Clone, wrap: usize,
        cfg: &PrintCfg
    ) -> Option<usize> {
        print_iter(iter, 0, self.width(), cfg)
            .filter_map(|((.., new_line), (pos, _))| new_line.map(|_| pos))
            .nth(wrap)
    }

    // NOTE: INCORRECT FUNCTION!!!
    fn get_width(
        &self, iter: impl Iterator<Item = (usize, usize, Part)> + Clone, cfg: &PrintCfg,
        wrap_around: bool
    ) -> usize {
        print_iter(iter, 0, self.width(), cfg)
            .skip_while(|((.., new_line), _)| new_line.is_none())
            .skip(1)
            .take_while(|((.., new_line), _)| new_line.is_none() || wrap_around)
            .map(|((_, len, _), _)| len)
            .sum::<usize>()
    }

    // NOTE: INCORRECT FUNCTION!!!
    fn col_at_dist(
        &self, iter: impl Iterator<Item = (usize, usize, Part)> + Clone, dist: usize,
        cfg: &PrintCfg
    ) -> usize {
        print_iter(iter, 0, self.width(), cfg)
            .enumerate()
            .take_while(|(_, ((x, ..), _))| *x <= dist)
            .map(|(index, _)| index)
            .last()
            .unwrap_or(0)
    }

    fn has_changed(&self) -> bool {
        let rect = self.layout.read().fetch_index(self.index).unwrap();
        let rect = rect.read();
        rect.has_changed()
    }

    fn is_senior_of(&self, other: &Self) -> bool {
        self.layout.inspect(|layout| {
            let mut parent_index = other.index;
            while let Some((parent, _)) = layout.fetch_parent(parent_index) {
                parent_index = parent.read().index();
                if parent_index == self.index {
                    break;
                }
            }

            parent_index == self.index
        })
    }

    fn bisect(&self, specs: PushSpecs, is_glued: bool) -> (Area, Option<Area>) {
        let (child, parent) =
            self.layout.mutate(|layout| layout.bisect(self.index, specs, is_glued));

        (
            Area::new(self.layout.clone(), child),
            parent.map(|parent| Area::new(self.layout.clone(), parent))
        )
    }

    fn print_iter<'a>(
        &self, iter: impl Iterator<Item = (usize, usize, Part)> + Clone + 'a, start: usize,
        cfg: &'a PrintCfg
    ) -> impl Iterator<Item = ((usize, usize, Option<usize>), (usize, Part))> + Clone + 'a {
        print_iter(iter, start, self.width(), cfg)
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
    first_char: usize,
    /// How shifted the text is to the left.
    x_shift: usize,
    /// The last position of the main cursor.
    last_main: Pos
}

impl PrintInfo {
    /// Scrolls down until the gap between the main cursor and the
    /// bottom of the widget is equal to `config.scrolloff.y_gap`.
    fn scroll_ver_to_gap(&mut self, point: Pos, text: &Text, area: &Area, cfg: &PrintCfg) {
        let inclusive_pos = point.true_char() + 1;
        let mut iter = rev_print_iter(text.rev_iter_at(inclusive_pos), area.width(), cfg)
            .filter_map(|((.., new_line), (pos, _))| new_line.zip(Some(pos)))
            .peekable();

        let point_line_nl_was_skipped = iter
            .peek()
            .is_some_and(|(new_line, _)| *new_line < point.true_line() && point.true_col() == 0);

        let mut iter = iter.map(|(_, pos)| pos);

        self.first_char = if self.last_main > point {
            if point_line_nl_was_skipped {
                let skipped_nl = std::iter::once(point.true_char());
                skipped_nl.chain(iter).nth(cfg.scrolloff.y_gap).unwrap_or(0).min(self.first_char)
            } else {
                iter.nth(cfg.scrolloff.y_gap).unwrap_or(0).min(self.first_char)
            }
        } else {
            let target = area.height().saturating_sub(cfg.scrolloff.y_gap + 1);
            if point_line_nl_was_skipped {
                let skipped_nl = std::iter::once(point.true_char());
                skipped_nl.chain(iter).nth(target).unwrap_or(0).max(self.first_char)
            } else {
                iter.nth(target).unwrap_or(0).max(self.first_char)
            }
        };
    }

    /// Scrolls the file horizontally, usually when no wrapping is
    /// being used.
    fn scroll_hor_to_gap(&mut self, point: Pos, text: &Text, area: &Area, cfg: &PrintCfg) {
        let width = area.width();
        let max_x_shift = match cfg.wrap_method {
            WrapMethod::Width | WrapMethod::Word => return,
            WrapMethod::Capped(cap) => {
                if cap > width {
                    cap - width
                } else {
                    return;
                }
            }
            WrapMethod::NoWrap => usize::MAX
        };

        let (line_start, start, end) = {
            let inclusive_pos = point.true_char() + 1;
            let mut iter = rev_print_iter(text.rev_iter_at(inclusive_pos), width, cfg).scan(
                false,
                |last_was_nl, item| {
                    let prev_last_was_nl = *last_was_nl;
                    *last_was_nl = item.0.2.is_some();
                    (!prev_last_was_nl).then_some(item)
                }
            );

            let (pos, start, end) = iter
                .find_map(|((x, len, _), (pos, part))| part.as_char().and(Some((pos, x, x + len))))
                .unwrap_or((0, 0, 0));

            let line_start =
                iter.find_map(|((.., new_line), (pos, _))| new_line.and(Some(pos))).unwrap_or(pos);

            (line_start, start, end)
        };

        let max_dist = width - cfg.scrolloff.x_gap;
        let min_dist = self.x_shift + cfg.scrolloff.x_gap;

        if start < min_dist {
            self.x_shift = self.x_shift.saturating_sub(min_dist - start);
        } else if end < start {
            self.x_shift = self.x_shift.saturating_sub(min_dist - end);
        } else if end > self.x_shift + max_dist {
            let line_width = print_iter(text.iter_at(line_start), 0, width, cfg).try_fold(
                (0, 0),
                |(right_end, some_count), ((x, len, new_line), _)| {
                    let some_count = some_count + new_line.is_some() as usize;
                    match some_count < 2 {
                        true => std::ops::ControlFlow::Continue((x + len, some_count)),
                        false => std::ops::ControlFlow::Break(right_end)
                    }
                }
            );

            if let std::ops::ControlFlow::Break(line_width) = line_width && line_width <= width {
                return;
            }
            self.x_shift = end - max_dist;
        }

        self.x_shift = self.x_shift.min(max_x_shift);
    }
}

impl ui::PrintInfo for PrintInfo {
    type Area = Area;

    fn scroll_to_gap(&mut self, text: &Text, pos: Pos, area: &Area, cfg: &PrintCfg) {
        if self.last_main != pos {
            self.scroll_hor_to_gap(pos, text, area, cfg);
            self.scroll_ver_to_gap(pos, text, area, cfg);
            self.last_main = pos;
        }
    }

    fn first_char(&self) -> usize {
        self.first_char
    }
}

fn print_parts(
    iter: impl Iterator<Item = ((usize, usize, Option<usize>), (usize, Part))>, coords: Coords,
    is_active: bool, info: PrintInfo, mut form_former: FormFormer, stdout: &mut StdoutLock
) -> usize {
    let mut x = coords.tl.x;
    // The y here represents the bottom line of the current row of cells.
    let mut y = coords.tl.y;
    let mut prev_style = None;
    let mut alignment = Alignment::Left;
    let mut line = Vec::new();

    for ((new_x, len, line_num), (_, part)) in iter {
        if line_num.is_some() {
            if y > coords.tl.y {
                let shifted_x = x.saturating_sub(info.x_shift);
                print_line(shifted_x, y, coords, alignment, &mut line, stdout);
            }
            if y == coords.br.y {
                break;
            }
            indent_line(&form_former, new_x, info.x_shift, &mut line);
            y += 1;
        }

        x = coords.tl.x + new_x + len;

        match part {
            // Char
            Part::Char(char) => {
                if len > 0 {
                    write_char(char, coords.tl.x + new_x, len, coords, info.x_shift, &mut line);
                    if let Some(style) = prev_style.take() {
                        queue!(&mut line, ResetColor, SetStyle(style))
                    }
                }
            }

            // Tags
            Part::PushForm(id) => queue!(line, ResetColor, SetStyle(form_former.apply(id).style)),
            Part::PopForm(id) => {
                if let Some(form) = form_former.remove(id) {
                    queue!(line, ResetColor, SetStyle(form.style))
                }
            }
            Part::MainCursor => {
                let cursor_style = form_former.main_cursor();
                if let (Some(caret), true) = (cursor_style.caret, is_active) {
                    SHOW_CURSOR.store(true, Ordering::Release);
                    queue!(line, caret, cursor::SavePosition);
                } else {
                    queue!(line, SetStyle(cursor_style.form.style));
                    prev_style = Some(form_former.make_form().style);
                }
            }
            Part::ExtraCursor => {
                queue!(line, SetStyle(form_former.extra_cursor().form.style));
                prev_style = Some(form_former.make_form().style);
            }
            Part::AlignLeft => alignment = Alignment::Left,
            Part::AlignCenter => alignment = Alignment::Center,
            Part::AlignRight => alignment = Alignment::Right,
            Part::Termination => {
                alignment = Alignment::Left;
                queue!(line, SetStyle(form_former.reset().style));
            }
            Part::HoverStart(_) => todo!(),
            Part::HoverEnd(_) => todo!(),
            Part::LeftButtonStart(_) => todo!(),
            Part::LeftButtonEnd(_) => todo!(),
            Part::RightButtonStart(_) => todo!(),
            Part::RightButtonEnd(_) => todo!(),
            Part::MiddleButtonStart(_) => todo!(),
            Part::MiddleButtonEnd(_) => todo!()
        }
    }

    if !line.is_empty() {
        print_line(x, y, coords, alignment, &mut line, stdout);
    }

    coords.br.y - y
}

#[inline(always)]
fn print_line(
    x: usize, y: usize, coords: Coords, alignment: Alignment, line: &mut Vec<u8>,
    stdout: &mut StdoutLock
) {
    let (left, right) = match alignment {
        Alignment::Left => (0, (coords.br.x).saturating_sub(x)),
        Alignment::Right => ((coords.br.x).saturating_sub(x), 0),
        Alignment::Center => {
            let remainder = (coords.br.x).saturating_sub(x);
            let left = remainder / 2;
            (left, remainder - left)
        }
    };

    queue!(
        stdout,
        ResetColor,
        Print(" ".repeat(left)),
        Print(std::str::from_utf8(line).unwrap()),
        ResetColor,
        Print(" ".repeat(right)),
        cursor::MoveTo(coords.tl.x as u16, y as u16)
    );

    line.clear();
}

#[inline(always)]
fn clear_line(cursor: Coord, coords: Coords, x_shift: usize, stdout: &mut StdoutLock) {
    let len = (coords.br.x + x_shift).saturating_sub(cursor.x);
    let (x, y) = (coords.tl.x, cursor.y);
    queue!(
        stdout,
        ResetColor,
        Print(" ".repeat(len.min(coords.width()))),
        cursor::MoveTo(x as u16, y as u16)
    );
}

#[inline(always)]
fn indent_line(form_former: &FormFormer, x: usize, x_shift: usize, line: &mut Vec<u8>) {
    let prev_style = form_former.make_form().style;
    let indent_count = x.saturating_sub(x_shift);
    let mut indent = Vec::<u8>::from(" ".repeat(indent_count));
    queue!(indent, SetStyle(prev_style));
    line.splice(0..0, indent);
}

#[inline(always)]
fn write_char(
    char: char, x: usize, len: usize, coords: Coords, x_shift: usize, line: &mut Vec<u8>
) {
    // Case where the cursor hasn't yet reached the left edge.
    if x < coords.tl.x + x_shift {
        let len = (x + len).saturating_sub(coords.tl.x + x_shift);
        queue!(line, Print(" ".repeat(len)));
    // Case where the end of the cursor, after printing, would be
    // located before the right edge.
    } else if x + len <= coords.br.x + x_shift {
        match char {
            '\t' => queue!(line, Print(" ".repeat(len))),
            char => queue!(line, Print(char))
        };
    // Case where it wouldn't.
    } else if x < coords.br.x + x_shift {
        let len = coords.br.x + x_shift - x;
        queue!(line, Print(" ".repeat(len)));
    }
}

fn print_edges(edges: &[Edge], stdout: &mut StdoutLock) {
    let edges = edges.iter().map(|edge| edge.line_coords()).collect::<Vec<LineCoords>>();

    let mut crossings =
        Vec::<(Coord, Option<Line>, Option<Line>, Option<Line>, Option<Line>)>::new();

    for (index, &coords) in edges.iter().enumerate() {
        if let Axis::Horizontal = coords.axis {
            let char = match coords.line {
                Some(line) => line::horizontal(line, line),
                None => unreachable!()
            };
            let line = char.to_string().repeat(coords.br.x - coords.tl.x + 1);
            queue!(stdout, cursor::MoveTo(coords.tl.x as u16, coords.tl.y as u16), Print(line))
        } else {
            let char = match coords.line {
                Some(line) => line::vertical(line, line),
                None => unreachable!()
            };

            for y in (coords.tl.y)..=coords.br.y {
                queue!(stdout, cursor::MoveTo(coords.tl.x as u16, y as u16), Print(char))
            }
        }

        for (other_index, &other_coords) in edges.iter().enumerate() {
            if index == other_index {
                continue;
            }

            if let Some(crossing) = coords.crossing(other_coords) {
                let prev_crossing = crossings.iter_mut().find(|(coord, ..)| *coord == crossing.0);
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
