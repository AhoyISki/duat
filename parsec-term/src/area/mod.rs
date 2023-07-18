mod line;

use std::{
    fmt::Alignment,
    io::{stdout, StdoutLock},
    sync::atomic::{AtomicBool, Ordering}
};

use crossterm::{
    cursor,
    style::{ContentStyle, Print, ResetColor, SetStyle}
};
use parsec_core::{
    data::{ReadableData, RwData},
    forms::{FormFormer, FormPalette},
    position::Pos,
    text::{NewLine, PrintCfg, TabStops, Text, TextBit, WrapMethod},
    ui::{self, Area as UiArea, Axis, Constraint}};
use unicode_width::UnicodeWidthChar;

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
    pub x: u16,
    pub y: u16
}

impl std::fmt::Debug for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("x: {}, y: {}", self.x, self.y))
    }
}

impl Coord {
    pub fn new(x: u16, y: u16) -> Coord {
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
        (self.br.x - self.tl.x) as usize
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

    fn wrapping_coords(&self, cfg: &PrintCfg) -> Coords {
        let layout = self.layout.read();
        let rect = layout.fetch_index(self.index).unwrap();
        let rect = rect.read();

        let width = match cfg.wrap_method {
            WrapMethod::Capped(cap) => cap as u16,
            _ => rect.br().x - rect.tl().x
        };

        Coords { tl: rect.tl(), br: Coord::new(rect.tl().x + width, rect.br().y) }
    }
}

impl ui::Area for Area {
    type PrintInfo = PrintInfo;
    fn width(&self) -> usize {
        self.layout.inspect(|layout| {
            let rect = layout.fetch_index(self.index).unwrap();
            let rect = rect.read();
            rect.br().x as usize - rect.tl().x as usize
        })
    }

    fn height(&self) -> usize {
        self.layout.inspect(|window| {
            let rect = window.fetch_index(self.index).unwrap();
            let rect = rect.read();
            rect.br().y as usize - rect.tl().y as usize
        })
    }

    fn set_as_active(&self) {
        self.layout.write().active_index = self.index;
    }

    fn print(&self, text: &Text, info: PrintInfo, cfg: PrintCfg, palette: &FormPalette) {
        if self.is_active() {
            SHOW_CURSOR.store(false, Ordering::Release);
        }

        let mut stdout = stdout().lock();
        print_edges(self.layout.read().edges(), &mut stdout);

        let coords = self.coords();
        queue!(stdout, cursor::MoveTo(coords.tl.x, coords.tl.y), cursor::Hide);

        let width = cfg.wrap_method.wrapping_cap(coords.width());

        let indents = {
            let line_char = {
                let Some(line) = text.get_char_to_line(info.first_char) else {
                    clear_line(coords.tl, coords, 0, &mut stdout);
                    return;
                };
                text.line_to_char(line)
            };
            let iter = text.iter_range(line_char..);
            indents(iter, &cfg.tab_stops, width).filter(move |(_, index, bit)| {
                *index >= info.first_char || matches!(bit, TextBit::Tag(_))
            })
        };

        let form_former = palette.form_former();
        let y_left = if let WrapMethod::Word = cfg.wrap_method {
            let words = words(indents, width, &cfg);
            print_bits(words, coords, self.is_active(), info, &cfg, form_former, &mut stdout)
        } else {
            let dont_wrap = cfg.wrap_method.is_no_wrap();
            let bits = bits(indents, width, &cfg.tab_stops, dont_wrap);
            print_bits(bits, coords, self.is_active(), info, &cfg, form_former, &mut stdout)
        };

        for y in (0..y_left).rev() {
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

    fn visible_rows(&self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg) -> usize {
        let coords = self.wrapping_coords(cfg);
        let indents = indents(iter, &cfg.tab_stops, coords.width());
        match cfg.wrap_method {
            WrapMethod::Width | WrapMethod::Capped(_) => {
                bits(indents, coords.width(), &cfg.tab_stops, false)
                    .filter_map(|(new_line, ..)| new_line)
                    .count()
            }
            WrapMethod::Word => {
                words(indents, coords.width(), cfg).filter_map(|(new_line, ..)| new_line).count()
            }
            WrapMethod::NoWrap => 1
        }
    }

    fn char_at_wrap(
        &self, mut iter: impl Iterator<Item = (usize, TextBit)>, wrap: usize, cfg: &PrintCfg
    ) -> Option<usize> {
        let width = self.wrapping_coords(cfg).width();
        match cfg.wrap_method {
            WrapMethod::Width | WrapMethod::Capped(_) => {
                let indents = indents(iter, &cfg.tab_stops, width);
                bits(indents, width, &cfg.tab_stops, false)
                    .filter_map(|(new_line, index, _)| new_line.map(|_| index))
                    .nth(wrap)
            }
            WrapMethod::Word => {
                let indents = indents(iter, &cfg.tab_stops, width);
                words(indents, width, cfg)
                    .filter_map(|(new_line, index, _)| new_line.map(|_| index))
                    .nth(wrap)
            }
            WrapMethod::NoWrap => iter.next().map(|(index, _)| index)
        }
    }

    fn get_width(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg, wrap_around: bool
    ) -> usize {
        let width = self.wrapping_coords(cfg).width();
        let max_width = if wrap_around { width as u16 } else { u16::MAX };
        let indents = indents(iter, &cfg.tab_stops, width);
        let fold_fn = |mut width, (new_line, _, bit)| {
            if let (true, Some(indent)) = (wrap_around, new_line) {
                width = indent;
            };

            if let TextBit::Char(char) = bit {
                width += len_from(char, width, max_width, &cfg.tab_stops)
            }

            width
        };

        if let WrapMethod::Word = cfg.wrap_method {
            words(indents, width, cfg).fold(0, fold_fn) as usize
        } else {
            let is_no_wrap = cfg.wrap_method.is_no_wrap();
            bits(indents, width, &cfg.tab_stops, is_no_wrap).fold(0, fold_fn) as usize
        }
    }

    fn col_at_dist(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, dist: usize, cfg: &PrintCfg
    ) -> usize {
        iter.filter_map(|(_, bit)| bit.as_char())
            .enumerate()
            .scan(0, |width, (index, char)| {
                let old_width = *width as usize;
                *width += len_from(char, *width, u16::MAX, &cfg.tab_stops);
                if old_width < dist { Some(index) } else { None }
            })
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

    type ConstraintChangeErr = ConstraintChangeErr;
}

unsafe impl Send for Area {}
unsafe impl Sync for Area {}

// NOTE: The defaultness in here, when it comes to `last_main`, may
// cause issues in the future.
/// Information about how to print the file on the `Label`.
#[derive(Default, Debug, Clone, Copy)]
pub struct PrintInfo {
    /// The index of the first [char] that should be printed on the
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
    fn scroll_ver_to_gap(&mut self, pos: Pos, text: &Text, area: &Area, cfg: &PrintCfg) {
        let width = cfg.wrap_method.wrapping_cap(area.width());
        let limit = if self.last_main > pos {
            cfg.scrolloff.y_gap + 1
        } else {
            area.height().saturating_sub(cfg.scrolloff.y_gap)
        };

        let mut indices = Vec::with_capacity(limit);
        let mut line_indices = Vec::new();
        for line_index in (0..=pos.true_row()).rev() {
            let line = text.iter_line(line_index);

            let iter = indents(line, &cfg.tab_stops, width);
            if let WrapMethod::Word = cfg.wrap_method {
                words(iter, width, cfg)
                    .filter_map(|(new_line, index, _)| new_line.map(|_| index))
                    .take_while(|index| *index <= pos.true_char())
                    .collect_into(&mut line_indices);
            } else {
                bits(iter, width, &cfg.tab_stops, cfg.wrap_method.is_no_wrap())
                    .filter_map(|(new_line, index, _)| new_line.map(|_| index))
                    .take_while(|index| *index <= pos.true_char())
                    .collect_into(&mut line_indices);
            };

            line_indices.reverse();
            indices.append(&mut line_indices);
            if indices.len() >= limit {
                break;
            }
        }

        if let Some(&index) =
            limit.checked_sub(1).and_then(|index| indices.get(index)).or_else(|| indices.last())
        {
            if (index < self.first_char && self.last_main > pos)
                || (index > self.first_char && self.last_main < pos)
            {
                self.first_char = index;
            }
        }
    }

    /// Scrolls the file horizontally, usually when no wrapping is
    /// being used.
    fn scroll_hor_to_gap(&mut self, pos: Pos, text: &Text, area: &Area, cfg: &PrintCfg) {
        let width = area.width();
        let (max_x_shift, cap) = match cfg.wrap_method {
            WrapMethod::Width | WrapMethod::Word => return,
            WrapMethod::Capped(cap) => {
                if cap > width {
                    (cap - width, cap)
                } else {
                    return;
                }
            }
            WrapMethod::NoWrap => (usize::MAX, usize::MAX)
        };

        let line = text.iter_line(pos.true_row());
        let Some(char) = text.get_char(pos.true_char()) else {
            return;
        };

        let end = {
            let line = line.clone().take_while(|(index, _)| *index < pos.true_char() + 1);
            area.get_width(line, cfg, true)
        };
        let start = match char {
            '\n' => end - 1,
            '\t' => {
                let line = line.take_while(|(index, _)| *index < pos.true_char());
                let start = area.get_width(line.clone(), cfg, true);
                // If `start > end`, the start happens before wrapping, which can only
                // happen on '\t' characters. Since the start needs to be on the same
                // line as the end, and the end is the first wrapped character, it is
                // also the start.
                if start > end {
                    indents(line, &cfg.tab_stops, cap)
                        .scan(0, |old_indent, (indent, ..)| {
                            if *old_indent == indent {
                                None
                            } else {
                                *old_indent = indent;
                                Some(indent)
                            }
                        })
                        .last()
                        .unwrap_or(0) as usize
                } else {
                    start
                }
            }
            char => end - UnicodeWidthChar::width(char).unwrap_or(0)
        };

        let max_dist = width - cfg.scrolloff.x_gap;
        let min_dist = self.x_shift + cfg.scrolloff.x_gap;

        if start < min_dist {
            self.x_shift = self.x_shift.saturating_sub(min_dist - start);
        } else if end < start {
            self.x_shift = self.x_shift.saturating_sub(min_dist - end);
        } else if end > self.x_shift + max_dist {
            let line = text.iter_line(pos.true_row());

            if area.get_width(line, cfg, false) < width {
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

    fn first_char(&self, _text: &Text) -> usize {
        self.first_char
    }
}

fn len_from(char: char, start: u16, max_width: u16, tab_stops: &TabStops) -> u16 {
    match char {
        '\t' => {
            (tab_stops.spaces_at(start as usize) as u16).min(max_width.saturating_sub(start)).max(1)
        }
        '\n' => 1,
        _ => UnicodeWidthChar::width(char).unwrap_or(0) as u16
    }
}

/// Returns an [`Iterator`] that also shows the current level of
/// indentation.
fn indents<'a>(
    iter: impl Iterator<Item = (usize, TextBit)> + 'a, tab_stops: &'a TabStops, width: usize
) -> impl Iterator<Item = (u16, usize, TextBit)> + 'a {
    iter.scan((0, true), move |(indent, on_indent), (index, bit)| {
        let old_indent = if *indent < width { *indent } else { 0 };
        (*indent, *on_indent) = match (&bit, *on_indent) {
            (&TextBit::Char('\t'), true) => (*indent + tab_stops.spaces_at(*indent), true),
            (&TextBit::Char(' '), true) => (*indent + 1, true),
            (&TextBit::Char('\n'), _) => (0, true),
            (&TextBit::Char(_), _) => (*indent, false),
            (&TextBit::Tag(_), on_indent) => (*indent, on_indent)
        };

        Some((old_indent as u16, index, bit))
    })
}

pub fn bits<'a>(
    iter: impl Iterator<Item = (u16, usize, TextBit)> + 'a, width: usize, tab_stops: &'a TabStops,
    no_wrap: bool
) -> impl Iterator<Item = (Option<u16>, usize, TextBit)> + 'a {
    let width = width as u16;
    iter.scan((0, true, false), move |(x, next_line, char_printed), (indent, index, bit)| {
        let len = bit
            .as_char()
            .map(|char| {
                *char_printed = true;
                len_from(char, *x, width, tab_stops)
            })
            .unwrap_or(0);
        *x += len;

        let surpassed_width = *x > width || (*x == width && len == 0);
        let nl = if *next_line && *char_printed || (!no_wrap && surpassed_width) {
            *x = indent + len;
            *next_line = false;
            Some(indent)
        } else {
            None
        };

        if let TextBit::Char('\n') = bit {
            *x = 0;
            *next_line = true;
        }

        Some((nl, index, bit))
    })
}

/// Returns an [`Iterator`] over the sequences of [`WordChars`].
fn words<'a>(
    iter: impl Iterator<Item = (u16, usize, TextBit)> + 'a, width: usize, cfg: &'a PrintCfg
) -> impl Iterator<Item = (Option<u16>, usize, TextBit)> + 'a {
    let mut iter = iter.peekable();
    let width = width as u16;
    let mut indent = 0;
    let mut word = Vec::new();

    let mut finished_word = Vec::new();
    let mut x = 0;
    let mut next_is_nl = true;
    std::iter::from_fn(move || {
        if let Some((index, bit)) = finished_word.pop() {
            return words_bit(index, bit, indent, &mut x, &mut next_is_nl, width, cfg);
        }

        let mut word_len = 0;
        while let Some((new_indent, _, bit)) = iter.peek() {
            if let &TextBit::Char(char) = bit {
                indent = *new_indent;
                if cfg.word_chars.contains(char) {
                    word_len += len_from(char, x + word_len, width, &cfg.tab_stops)
                } else {
                    word.push(iter.next().map(|(_, index, bit)| (index, bit)).unwrap());
                    break;
                }
            }
            word.push(iter.next().map(|(_, index, bit)| (index, bit)).unwrap());
        }

        next_is_nl |= x + word_len > width;

        std::mem::swap(&mut word, &mut finished_word);
        finished_word.reverse();
        finished_word.pop().and_then(|(index, bit)| {
            words_bit(index, bit, indent, &mut x, &mut next_is_nl, width, cfg)
        })
    })
}

fn words_bit(
    index: usize, bit: TextBit, indent: u16, x: &mut u16, next_is_nl: &mut bool, width: u16,
    cfg: &PrintCfg
) -> Option<(Option<u16>, usize, TextBit)> {
    let nl = if *next_is_nl {
        *next_is_nl = false;
        *x = indent;
        Some(indent)
    } else if let TextBit::Char(char) = bit {
        let len = len_from(char, *x, width, &cfg.tab_stops);
        let ret = if *x + len > width { Some(indent) } else { None };
        *x = ret.map(|indent| indent + len).unwrap_or(*x + len);
        ret
    } else if *x >= width {
        *x = indent;
        Some(indent)
    } else {
        None
    };

    if let Some(char) = bit.as_char() {
        *next_is_nl = char == '\n'
    }

    Some((nl, index, bit))
}

fn print_bits(
    iter: impl Iterator<Item = (Option<u16>, usize, TextBit)>, coords: Coords, is_active: bool,
    info: PrintInfo, cfg: &PrintCfg, mut form_former: FormFormer, stdout: &mut StdoutLock
) -> u16 {
    let mut passed_first_indent = false;
    let mut last_char = 'a';
    let mut cursor = coords.tl;
    let mut prev_style = None;
    let mut alignment = Alignment::Left;
    let mut line = Vec::new();

    for (nl, _, bit) in iter {
        if let Some(indent) = nl {
            if passed_first_indent {
                cursor.y += 1;
                print_line(cursor, coords, alignment, &mut line, stdout);
            } else {
                passed_first_indent = true;
            }
            if cursor.y == coords.br.y {
                break;
            }
            cursor.x = coords.tl.x + indent;
            indent_line(&form_former, cursor, coords, info.x_shift, &mut line);
        }

        if let &TextBit::Char(char) = &bit {
            if char == '\n' && let Alignment::Right | Alignment::Center = alignment {
                continue;
            }
            let char = real_char_from(char, &cfg.new_line, last_char);
            cursor.x += write_char(char, cursor, coords, info.x_shift, &cfg.tab_stops, &mut line);
            last_char = char;
            if let Some(style) = prev_style.take() {
                queue!(&mut line, ResetColor, SetStyle(style))
            }
        } else if let TextBit::Tag(tag) = bit {
            prev_style = trigger_tag(tag, is_active, &mut alignment, &mut form_former, &mut line);
        }
    }

    if !line.is_empty() {
        cursor.y += 1;
        print_line(cursor, coords, alignment, &mut line, stdout);
    }

    coords.br.y - cursor.y
}

fn print_line(
    cursor: Coord, coords: Coords, alignment: Alignment, line: &mut Vec<u8>,
    stdout: &mut StdoutLock
) {
    let (left, right) = match alignment {
        Alignment::Left => (0, coords.br.x.saturating_sub(cursor.x) as usize),
        Alignment::Right => (coords.br.x.saturating_sub(cursor.x) as usize, 0),
        Alignment::Center => {
            let remainder = coords.br.x.saturating_sub(cursor.x) as usize;
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
        cursor::MoveTo(coords.tl.x, cursor.y)
    );

    line.clear();
}

fn clear_line(cursor: Coord, coords: Coords, x_shift: usize, stdout: &mut StdoutLock) {
    let len = (coords.br.x + x_shift as u16).saturating_sub(cursor.x) as usize;
    let (x, y) = (coords.tl.x, cursor.y);
    queue!(stdout, ResetColor, Print(" ".repeat(len.min(coords.width()))), cursor::MoveTo(x, y));
}

fn indent_line(
    form_former: &FormFormer, cursor: Coord, coords: Coords, x_shift: usize, line: &mut Vec<u8>
) {
    let prev_style = form_former.make_form().style;
    let mut indent = Vec::<u8>::from(
        " ".repeat((cursor.x.saturating_sub(coords.tl.x + x_shift as u16)) as usize)
    );
    queue!(indent, SetStyle(prev_style));
    line.splice(0..0, indent);
}

fn write_char(
    char: char, cursor: Coord, coords: Coords, x_shift: usize, tab_stops: &TabStops,
    line: &mut Vec<u8>
) -> u16 {
    let len = len_from(char, cursor.x - coords.tl.x, (coords.width() + x_shift) as u16, tab_stops);

    // Case where the cursor hasn't yet reached the left edge.
    if cursor.x < coords.tl.x + x_shift as u16 {
        let len = (cursor.x + len).saturating_sub(coords.tl.x + x_shift as u16) as usize;
        queue!(line, Print(" ".repeat(len)));
    // Case where the end of the cursor, after printing, would be
    // located before the right edge.
    } else if cursor.x + len <= coords.br.x + x_shift as u16 {
        match char {
            '\t' => queue!(line, Print(" ".repeat(len as usize))),
            char => queue!(line, Print(char))
        };
    // Case where it wouldn't.
    } else if cursor.x < coords.br.x + x_shift as u16 {
        let len = coords.br.x as usize + x_shift - cursor.x as usize;
        queue!(line, Print(" ".repeat(len)));
    }

    len
}

fn trigger_tag(
    tag: parsec_core::text::Tag, is_active: bool, alignment: &mut Alignment,
    form_former: &mut FormFormer, line: &mut Vec<u8>
) -> Option<ContentStyle> {
    use parsec_core::text::Tag::*;
    match tag {
        PushForm(id) => queue!(line, ResetColor, SetStyle(form_former.apply(id).style)),
        PopForm(id) => queue!(line, ResetColor, SetStyle(form_former.remove(id).style)),
        MainCursor => {
            let cursor_style = form_former.main_cursor();
            if let (Some(caret), true) = (cursor_style.caret, is_active) {
                SHOW_CURSOR.store(true, Ordering::Release);
                queue!(line, caret, cursor::SavePosition);
            } else {
                queue!(line, SetStyle(cursor_style.form.style));
                return Some(form_former.make_form().style);
            }
        }
        ExtraCursor => queue!(line, SetStyle(form_former.extra_cursor().form.style)),
        AlignLeft => *alignment = Alignment::Left,
        AlignRight => *alignment = Alignment::Right,
        AlignCenter => *alignment = Alignment::Center,
        _ => {}
    }

    None
}

fn real_char_from(char: char, new_line: &NewLine, last_char: char) -> char {
    match char {
        '\n' => match *new_line {
            NewLine::Blank => ' ',
            NewLine::AlwaysAs(char) => char,
            NewLine::AfterSpaceAs(char) => {
                if last_char == ' ' {
                    char
                } else {
                    ' '
                }
            }
        },
        char => char
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
            let line = char.to_string().repeat((coords.br.x - coords.tl.x + 1) as usize);
            queue!(stdout, cursor::MoveTo(coords.tl.x, coords.tl.y), Print(line))
        } else {
            let char = match coords.line {
                Some(line) => line::vertical(line, line),
                None => unreachable!()
            };

            for y in (coords.tl.y)..=coords.br.y {
                queue!(stdout, cursor::MoveTo(coords.tl.x, y), Print(char))
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
            cursor::MoveTo(coord.x, coord.y),
            Print(line::crossing(right, up, left, down, true))
        )
    }
}
