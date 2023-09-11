mod iter;
mod line;

use std::{
    cell::RefCell,
    fmt::Alignment,
    io::{stdout, StdoutLock},
    sync::atomic::{AtomicBool, Ordering},
};

use crossterm::{
    cursor,
    style::{Print, ResetColor, SetStyle},
};
use iter::{print_iter, rev_print_iter};
use parsec_core::{
    data::{ReadableData, RwData},
    forms::{FormFormer, FormPalette},
    position::Point,
    text::{Item, IterCfg, Part, PrintCfg, Text, WrapMethod},
    ui::{self, Area as UiArea, Axis, Caret, Constraint, PushSpecs},
};

use crate::{
    area::iter::counted_print_iter,
    layout::{Edge, EdgeBrush, EdgeCoords, Layout},
    AreaIndex, ConstraintChangeErr,
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
    pub y: usize,
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
    br: Coord,
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
    print_info: RefCell<PrintInfo>,
    pub index: AreaIndex,
}

impl PartialEq for Area {
    fn eq(&self, other: &Self) -> bool {
        self.layout.ptr_eq(&other.layout) && self.index == other.index
    }
}

impl Area {
    pub fn new(index: AreaIndex, layout: RwData<Layout>) -> Self {
        Self {
            layout,
            print_info: RefCell::new(PrintInfo::default()),
            index,
        }
    }

    fn is_active(&self) -> bool {
        self.layout.read().active_index == self.index
    }

    fn coords(&self) -> Coords {
        let layout = self.layout.read();
        let rect = layout.fetch_index(self.index).unwrap();
        let rect = rect.read();

        Coords {
            tl: rect.tl(),
            br: rect.br(),
        }
    }

    /// Scrolls down until the gap between the main cursor and the
    /// bottom of the widget is equal to `config.scrolloff.y_gap`.
    fn scroll_ver_to_gap(&self, point: Point, text: &Text, cfg: IterCfg) {
        let mut info = self.print_info.borrow_mut();

        let inclusive_pos = point.true_char() + 1;
        let mut iter = rev_print_iter(text.rev_iter_at(inclusive_pos), self.width(), cfg)
            .filter_map(|(caret, item)| caret.wrap.then_some(item))
            .peekable();

        let point_line_nl_was_concealed = iter
            .peek()
            .is_some_and(|item| item.line < point.true_line() && point.true_col() == 0);

        let mut iter = iter.map(|item| (item.ghost_pos, item.pos));

        let target = if info.last_main > point {
            cfg.scrolloff().y_gap
        } else {
            self.height().saturating_sub(cfg.scrolloff().y_gap + 1)
        };

        let (ghost_pos, first_char) = if point_line_nl_was_concealed {
            let skipped_nl = std::iter::once((None, point.true_char()));
            skipped_nl.chain(iter).nth(target).unwrap_or((None, 0))
        } else {
            iter.nth(target).unwrap_or((None, 0))
        };

        if info.last_main > point {
            if first_char <= info.first_char {
                info.first_char = first_char;
                info.ghost_pos = ghost_pos;
            }
        } else if first_char >= info.first_char {
            info.first_char = first_char;
            info.ghost_pos = ghost_pos;
        }
    }

    /// Scrolls the file horizontally, usually when no wrapping is
    /// being used.
    fn scroll_hor_to_gap(&self, point: Point, text: &Text, cfg: IterCfg) {
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
            let inclusive_pos = point.true_char() + 1;
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
                    part.as_char().and(Some((pos, x, x + len)))
                })
                .unwrap_or((0, 0, 0));

            let line_start = iter
                .find_map(|(Caret { wrap, .. }, Item { pos, .. })| wrap.then_some(pos))
                .unwrap_or(pos);

            (line_start, start, end)
        };

        let max_dist = width - cfg.scrolloff().x_gap;
        let min_dist = info.x_shift + cfg.scrolloff().x_gap;

        if start < min_dist {
            info.x_shift = info.x_shift.saturating_sub(min_dist - start);
        } else if end < start {
            info.x_shift = info.x_shift.saturating_sub(min_dist - end);
        } else if end > info.x_shift + max_dist {
            let line_width = print_iter(text.iter_at(line_start), width, cfg).try_fold(
                (0, 0),
                |(right_end, some_count), (Caret { x, len, wrap }, _)| {
                    let some_count = some_count + wrap as usize;
                    match some_count < 2 {
                        true => std::ops::ControlFlow::Continue((x + len, some_count)),
                        false => std::ops::ControlFlow::Break(right_end),
                    }
                },
            );

            if let std::ops::ControlFlow::Break(line_width) = line_width && line_width <= width {
                return;
            }
            info.x_shift = end - max_dist;
        }

        info.x_shift = info.x_shift.min(max_x_shift);
    }
}

impl ui::Area for Area {
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

    fn scroll_around_point(&self, text: &Text, point: Point, cfg: &PrintCfg) {
        self.scroll_hor_to_gap(point, text, IterCfg::new(cfg).outsource_lfs());
        self.scroll_ver_to_gap(point, text, IterCfg::new(cfg).outsource_lfs());

        self.print_info.borrow_mut().last_main = point;
    }

    fn first_char(&self) -> usize {
        self.print_info.borrow().first_char
    }

    fn set_as_active(&self) {
        self.layout.write().active_index = self.index;
    }

    fn print(&self, text: &Text, cfg: &PrintCfg, palette: &FormPalette) {
        let info = self.print_info.borrow();

        let coords = self.coords();
        let mut stdout = stdout().lock();
        print_edges(self.layout.read().edges(), &mut stdout);

        if self.is_active() {
            SHOW_CURSOR.store(false, Ordering::Release);
        }

        queue!(
            stdout,
            cursor::MoveTo(coords.tl.x as u16, coords.tl.y as u16),
            cursor::Hide
        );

        if text.len_chars() == 0 {
            for y in coords.tl.y..coords.br.y {
                clear_line(
                    Coord::new(coords.tl.x, coords.br.y - y),
                    coords,
                    0,
                    &mut stdout,
                );
            }
            return;
        }

        let (iter, cfg) = if let Some(start) = text.close_visual_line_start(info.first_char) {
            let cfg = IterCfg::new(cfg).chars_at(info.first_char).outsource_lfs();
            (text.iter_at(start), cfg)
        } else {
            let cfg = IterCfg::new(cfg)
                .outsource_lfs()
                .no_word_wrap()
                .no_indent_wrap();
            (text.iter_at(info.first_char), cfg)
        };

        let form_former = palette.form_former();
        let y = if let Some(pos) = info.ghost_pos && pos > 0 {
            let iter = counted_print_iter(iter, coords.width(), cfg, pos);
            print_parts(iter, coords, self.is_active(), *info, form_former, &mut stdout)
        } else {
            let iter = print_iter(iter, coords.width(), cfg);
            print_parts(iter, coords, self.is_active(), *info, form_former, &mut stdout)
        };

        for y in (0..y).rev() {
            clear_line(
                Coord::new(coords.tl.x, coords.br.y - y),
                coords,
                0,
                &mut stdout,
            );
        }

        if SHOW_CURSOR.load(Ordering::Acquire) {
            queue!(stdout, cursor::RestorePosition, cursor::Show);
        }

        crossterm::execute!(stdout, ResetColor).unwrap();
    }

    fn change_constraint(&self, constraint: Constraint) -> Result<(), ConstraintChangeErr> {
        let mut layout = self.layout.write();
        let (parent, index) = layout
            .fetch_parent(self.index)
            .ok_or(ConstraintChangeErr::NoParent)?;
        if parent
            .write()
            .change_child_constraint(index, constraint, &mut layout.solver)
        {
            layout.update();
        }

        Ok(())
    }

    fn request_width_to_fit(&self, _text: &str) -> Result<(), Self::ConstraintChangeErr> {
        todo!();
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
        let (child, parent) = self
            .layout
            .mutate(|layout| layout.bisect(self.index, specs, is_glued));

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
        print_iter(iter, self.width(), cfg)
    }

    fn precise_print_iter<'a>(
        &self,
        iter: impl Iterator<Item = Item> + Clone + 'a,
        cfg: IterCfg<'a>,
    ) -> impl Iterator<Item = (Caret, Item)> + Clone + 'a {
        counted_print_iter(
            iter,
            self.width(),
            cfg,
            self.print_info.borrow().ghost_pos.unwrap_or(usize::MAX),
        )
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
    first_char: usize,
    /// The index of the first [`char`] that should be printed,
    /// relative to the beginning of a ghost text.
    ghost_pos: Option<usize>,
    /// How shifted the text is to the left.
    x_shift: usize,
    /// The last position of the main cursor.
    last_main: Point,
}

fn print_parts(
    iter: impl Iterator<Item = (Caret, Item)>,
    coords: Coords,
    is_active: bool,
    info: PrintInfo,
    mut form_former: FormFormer,
    stdout: &mut StdoutLock,
) -> usize {
    let mut x = coords.tl.x;
    // The y here represents the bottom line of the current row of cells.
    let mut y = coords.tl.y;
    let mut prev_style = None;
    let mut alignment = Alignment::Left;
    let mut line = Vec::new();

    for (
        Caret {
            x: new_x,
            len,
            wrap,
        },
        Item { part, .. },
    ) in iter
    {
        if wrap {
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
                    write_char(
                        char,
                        coords.tl.x + new_x,
                        len,
                        coords,
                        info.x_shift,
                        &mut line,
                    );
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
            Part::MiddleButtonEnd(_) => todo!(),
        }
    }

    if !line.is_empty() {
        print_line(x, y, coords, alignment, &mut line, stdout);
    }

    coords.br.y - y
}

#[inline(always)]
fn print_line(
    x: usize,
    y: usize,
    coords: Coords,
    alignment: Alignment,
    line: &mut Vec<u8>,
    stdout: &mut StdoutLock,
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
        queue!(line, Print(" ".repeat(len)));
    // Case where the end of the cursor, after printing, would be
    // located before the right edge.
    } else if x + len <= coords.br.x + x_shift {
        match char {
            '\t' => queue!(line, Print(" ".repeat(len))),
            char => queue!(line, Print(char)),
        };
    // Case where it wouldn't.
    } else if x < coords.br.x + x_shift {
        let len = coords.br.x + x_shift - x;
        queue!(line, Print(" ".repeat(len)));
    }
}

fn print_edges(edges: &[Edge], stdout: &mut StdoutLock) {
    let edges = edges
        .iter()
        .map(|edge| edge.line_coords())
        .collect::<Vec<EdgeCoords>>();

    let mut crossings = Vec::<(
        Coord,
        Option<EdgeBrush>,
        Option<EdgeBrush>,
        Option<EdgeBrush>,
        Option<EdgeBrush>,
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
