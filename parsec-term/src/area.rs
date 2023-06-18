use std::io::{self, StdoutLock};

use crossterm::{
    cursor::{self, MoveTo, SavePosition},
    execute, queue as crossterm_queue,
    style::{ContentStyle, Print, ResetColor, SetStyle}
};
use parsec_core::{
    data::RwData,
    position::Pos,
    tags::{
        form::{FormFormer, FormPalette},
        Tag
    },
    text::{NewLine, PrintCfg, TabStops, Text, TextBit, WrapMethod},
    ui::{self, Area as UiArea, Constraint, Ui}
};
use unicode_width::UnicodeWidthChar;

use crate::layout::Layout;

macro_rules! queue {
    ($writer:expr $(, $command:expr)* $(,)?) => {
        unsafe { crossterm_queue!($writer $(, $command)*).unwrap_unchecked() }
    }
}

#[derive(Clone, Copy)]
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
    fn new(x: u16, y: u16) -> Coord {
        Coord { x, y }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Coords {
    tl: Coord,
    br: Coord
}
impl Coords {
    fn width(&self) -> usize {
        (self.br.x - self.tl.x) as usize
    }
}

/// When to show the side of a [`Frame`].
///
/// - [`Never`][Show::Never] never shows it.
/// - [`Border`][Show::Border] hides when on the edge of the window.
/// - [`Always`][Show::Always] always shows it.
/// - [`Negate`][Show::Negate] stops other [`Frame`]s from bordering.
#[derive(Clone, Copy, Debug)]
pub enum Show {
    Never,
    Border,
    Always,
    Negate
}

enum Side {
    Right,
    Up,
    Left,
    Down
}

impl Show {
    fn frame_shift(&self, on_edge_of_screen: bool, side: Side) -> u16 {
        // TODO: Take neighboring edges into account.
        match self {
            Show::Never | Show::Negate => 0,
            Show::Border => match (on_edge_of_screen, side) {
                (false, Side::Right | Side::Down) => 1,
                _ => 0
            },
            Show::Always => 1
        }
    }
}

/// Configuration about how and wether to frame a [`Rect`].
///
/// All options follow an anti-clockwise ordering, starting from
/// right:
///
/// - edges: (horizontal, vertical),
/// - corners: (up right, up left, down left, down right),
/// - joints: (right ver, up hor, left ver, down hor, ver hor)
/// - sides_to_show: (right, up, left, down)
#[derive(Clone, Debug)]
pub struct Frame {
    pub edges: (char, char),
    pub corners: (char, char, char, char),
    pub joints: (char, char, char, char, char),
    pub sides_to_show: (Show, Show, Show, Show)
}

impl Default for Frame {
    fn default() -> Self {
        Frame {
            edges: ('─', '│'),
            corners: ('┐', '┌', '└', '┘'),
            joints: ('├', '┴', '┤', '┬', '┼'),
            sides_to_show: (Show::Never, Show::Never, Show::Never, Show::Never)
        }
    }
}

impl Frame {
    pub fn normal() -> Self {
        Frame {
            edges: ('─', '│'),
            corners: ('┐', '┌', '└', '┘'),
            joints: ('├', '┴', '┤', '┬', '┼'),
            sides_to_show: (Show::Border, Show::Border, Show::Border, Show::Border)
        }
    }

    /// Returns a new instance of [`Frame`], using only ASCII
    /// characters.
    pub fn ascii() -> Self {
        Frame {
            edges: ('-', '|'),
            corners: ('+', '+', '+', '+'),
            joints: ('+', '+', '+', '+', '+'),
            sides_to_show: (Show::Border, Show::Border, Show::Border, Show::Border)
        }
    }

    pub fn heavy() -> Self {
        Frame {
            edges: ('━', '┃'),
            corners: ('┓', '┏', '┗', '┛'),
            joints: ('┣', '┻', '┫', '┳', '╋'),
            sides_to_show: (Show::Border, Show::Border, Show::Border, Show::Border)
        }
    }

    pub fn dashed() -> Self {
        Frame {
            edges: ('╌', '╎'),
            corners: ('┐', '┌', '└', '┘'),
            joints: ('├', '┴', '┤', '┬', '┼'),
            sides_to_show: (Show::Border, Show::Border, Show::Border, Show::Border)
        }
    }

    pub fn heavy_dashed() -> Self {
        Frame {
            edges: ('╍', '╏'),
            corners: ('┓', '┏', '┗', '┛'),
            joints: ('┣', '┻', '┫', '┳', '╋'),
            sides_to_show: (Show::Border, Show::Border, Show::Border, Show::Border)
        }
    }

    pub fn double() -> Self {
        Frame {
            edges: ('═', '║'),
            corners: ('╗', '╔', '╚', '╝'),
            joints: ('╠', '╩', '╣', '╦', '╬'),
            sides_to_show: (Show::Border, Show::Border, Show::Border, Show::Border)
        }
    }
}

pub struct Area {
    pub layout: RwData<Layout>,
    pub index: usize
}

impl Area {
    fn is_active(&self) -> bool {
        self.layout.read().active_index == self.index
    }

    fn coords(&self) -> Coords {
        let layout = self.layout.read();
        let rect = layout.fetch_index(self.index).unwrap();
        let rect = rect.read();

        let mut coords = Coords {
            tl: rect.tl(),
            br: rect.br()
        };

        let (right, up, left, down) = rect.frame.sides_to_show;
        coords.br.x -= right.frame_shift(coords.br.x == layout.width(), Side::Right);
        coords.tl.y += up.frame_shift(coords.tl.y == 0, Side::Up);
        coords.tl.x += left.frame_shift(coords.tl.x == 0, Side::Left);
        coords.br.y -= down.frame_shift(coords.tl.y == layout.height(), Side::Down);

        coords
    }

    fn wrapping_coords(&self, cfg: &PrintCfg) -> Coords {
        let layout = self.layout.read();
        let rect = layout.fetch_index(self.index).unwrap();
        let rect = rect.read();

        let width = match cfg.wrap_method {
            WrapMethod::Capped(cap) => cap as u16,
            _ => rect.br().x - rect.tl().x as u16
        };

        let mut coords = Coords {
            tl: rect.tl(),
            br: Coord::new(rect.tl().x + width, rect.br().y)
        };

        let (_, up, left, down) = rect.frame.sides_to_show;
        coords.tl.y += up.frame_shift(coords.tl.y == 0, Side::Up);
        coords.br.y -= down.frame_shift(coords.tl.y == layout.height(), Side::Down);

        let x_shift = left.frame_shift(coords.br.x == layout.width(), Side::Right);
        coords.tl.x += x_shift;
        coords.br.x += x_shift;

        coords
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

    fn set_as_active(&mut self) {
        self.layout.write().active_index = self.index;
    }

    fn print<U>(&mut self, text: &Text<U>, info: PrintInfo, cfg: PrintCfg, palette: &FormPalette)
    where
        U: Ui + ?Sized
    {
        let mut stdout = io::stdout().lock();
        let coords = self.coords();
        queue!(stdout, MoveTo(coords.tl.x, coords.tl.y), cursor::Hide);

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
        let (mut cursor, show_cursor) = if let WrapMethod::Word = cfg.wrap_method {
            let words = words(indents, width, &cfg);
            print(words, coords, self.is_active(), info, &cfg, form_former, &mut stdout)
        } else {
            let bits = bits(indents, width, &cfg.tab_stops, cfg.wrap_method.is_no_wrap());
            print(bits, coords, self.is_active(), info, &cfg, form_former, &mut stdout)
        };

        let mut stdout = io::stdout().lock();
        while coords.br.y >= cursor.y {
            clear_line(cursor, coords, 0, &mut stdout);
            cursor.y += 1;
            cursor.x = coords.tl.x;
        }

        if show_cursor {
            queue!(stdout, cursor::Show);
        }

        execute!(stdout, ResetColor).unwrap();
    }

    fn change_constraint(&mut self, constraint: Constraint) -> Result<(), ()> {
        let mut layout = self.layout.write();
        let (parent, index, _) = layout.fetch_parent(self.index).ok_or(())?;
        parent.write().change_child_constraints(index, constraint, &mut layout.solver);

        layout.update();

        Ok(())
    }

    fn vis_rows(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg, max_index: usize
    ) -> usize {
        let coords = self.wrapping_coords(cfg);
        let indents = indents(iter, &cfg.tab_stops, coords.width());
        match cfg.wrap_method {
            WrapMethod::Width | WrapMethod::Capped(_) => {
                bits(indents, coords.width(), &cfg.tab_stops, false)
                    .filter_map(|(new_line, index, _)| new_line.map(|_| index))
                    .take_while(|index| *index <= max_index)
                    .count()
            }
            WrapMethod::Word => words(indents, coords.width(), &cfg)
                .filter_map(|(new_line, index, _)| new_line.map(|_| index))
                .take_while(|index| *index <= max_index)
                .count(),
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
        &self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg, max_index: usize,
        wrap_around: bool
    ) -> usize {
        let width = self.wrapping_coords(cfg).width();
        let indents = indents(iter, &cfg.tab_stops, width);
        let fold_fn = |mut width, (new_line, _, bit)| {
            if let (true, Some(indent)) = (wrap_around, new_line) {
                width = indent;
            };

            if let TextBit::Char(char) = bit {
                width += len_from(char, width, &cfg.tab_stops)
            }

            width
        };

        if let WrapMethod::Word = cfg.wrap_method {
            let is_no_wrap = cfg.wrap_method.is_no_wrap();
            bits(indents, width, &cfg.tab_stops, is_no_wrap)
                .take_while(|(_, index, _)| *index < max_index)
                .fold(0, fold_fn) as usize
        } else {
            words(indents, width, &cfg)
                .take_while(|(_, index, _)| *index < max_index)
                .fold(0, fold_fn) as usize
        }
    }

    fn col_at_dist(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, dist: usize, cfg: &PrintCfg
    ) -> usize {
        iter.filter_map(|(_, bit)| bit.as_char())
            .enumerate()
            .scan(0, |width, (index, char)| {
                let old_width = *width as usize;
                *width += len_from(char, *width, &cfg.tab_stops);
                if old_width < dist { Some(index) } else { None }
            })
            .last()
            .unwrap_or(0)
    }

    fn index(&self) -> usize {
        self.index
    }
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
    fn scroll_ver_to_gap<U>(&mut self, pos: Pos, text: &Text<U>, area: &U::Area, cfg: &PrintCfg)
    where
        U: Ui
    {
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
                words(iter, width, &cfg)
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

        if let Some(&index) = indices.get(limit - 1).or_else(|| indices.last()) {
            if (index < self.first_char && self.last_main > pos)
                || (index > self.first_char && self.last_main < pos)
            {
                self.first_char = index;
            }
        }
    }

    /// Scrolls the file horizontally, usually when no wrapping is
    /// being used.
    fn scroll_hor_to_gap<U>(&mut self, pos: Pos, text: &Text<U>, area: &U::Area, cfg: &PrintCfg)
    where
        U: Ui
    {
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

        let line = text.iter_line(pos.true_row());
        let target_dist = area.get_width(line, cfg, pos.true_char(), true);
        let max_dist = width - (cfg.scrolloff.x_gap + 1);
        let min_dist = self.x_shift + cfg.scrolloff.x_gap;
        if target_dist < min_dist {
            self.x_shift = self.x_shift.saturating_sub(min_dist - target_dist);
        } else if target_dist > self.x_shift + max_dist {
            let line = text.iter_line(pos.true_row());

            if area.get_width(line, cfg, usize::MAX, false) < width {
                return;
            }
            self.x_shift = target_dist - max_dist;
        }

        self.x_shift = self.x_shift.min(max_x_shift);
    }
}

impl ui::PrintInfo for PrintInfo {
    fn scroll_to_gap<U>(&mut self, text: &Text<U>, pos: Pos, area: &U::Area, cfg: &PrintCfg)
    where
        U: Ui
    {
        if self.last_main != pos {
            self.scroll_hor_to_gap::<U>(pos, text, area, cfg);
            self.scroll_ver_to_gap::<U>(pos, text, area, cfg);
            self.last_main = pos;
        }
    }

    fn first_char<U>(&self, _text: &Text<U>) -> usize
    where
        U: Ui
    {
        self.first_char
    }
}

fn len_from(char: char, start: u16, tab_stops: &TabStops) -> u16 {
    match char {
        '\t' => tab_stops.spaces_at(start as usize) as u16,
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

fn bits<'a>(
    iter: impl Iterator<Item = (u16, usize, TextBit)> + 'a, width: usize, tab_stops: &'a TabStops,
    no_wrap: bool
) -> impl Iterator<Item = (Option<u16>, usize, TextBit)> + 'a {
    let width = width as u16;
    iter.scan((0, true), move |(x, next_line), (indent, index, bit)| {
        let len = bit.as_char().map(|char| len_from(char, *x, tab_stops)).unwrap_or(0);
        *x += len;

        let surpassed_width = *x > width || (*x == width && len == 0);
        let tag_on_indent = indent > *x - len && bit.is_tag();
        let nl = if (*next_line && !tag_on_indent) || (!no_wrap && surpassed_width) {
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
    let mut next_line = true;
    std::iter::from_fn(move || {
        if let Some((index, bit)) = finished_word.pop() {
            let nl = if let TextBit::Char(char) = bit {
                let len = len_from(char, x, &cfg.tab_stops);
                let ret = if x + len > width { Some(indent) } else { None };
                x = ret.map(|indent| indent + len).unwrap_or(x + len);
                ret
            } else if x >= width {
                x = indent;
                Some(indent)
            } else {
                None
            };

            return Some((nl, index, bit));
        }

        let mut word_len = 0;
        while let Some((new_indent, _, bit)) = iter.peek() {
            if let &TextBit::Char(char) = bit {
                let is_word_char = cfg.word_chars.contains(char);
                indent = *new_indent;
                if word.is_empty() || is_word_char {
                    word_len += len_from(char, x + word_len, &cfg.tab_stops)
                }
                if !is_word_char {
                    break;
                }
            }
            word.push(iter.next().map(|(_, index, bit)| (index, bit)).unwrap());
        }

        let nl = if next_line || x + word_len > width {
            next_line = false;
            x = indent;
            Some(indent)
        // Wrapping not necessary.
        } else {
            None
        };

        if word.is_empty() {
            iter.next().map(|(_, index, bit)| {
                x += word_len;
                next_line = matches!(bit, TextBit::Char('\n'));
                (nl, index, bit)
            })
        } else {
            std::mem::swap(&mut word, &mut finished_word);
            finished_word.reverse();
            finished_word.pop().map(|(index, bit)| {
                if let TextBit::Char(char) = bit {
                    x += len_from(char, x, &cfg.tab_stops);
                }
                (nl, index, bit)
            })
        }
    })
}

fn print(
    iter: impl Iterator<Item = (Option<u16>, usize, TextBit)>, coords: Coords, is_active: bool,
    info: PrintInfo, cfg: &PrintCfg, mut form_former: FormFormer, stdout: &mut StdoutLock
) -> (Coord, bool) {
    let x_shift = info.x_shift;
    let mut last_char = 'a';
    let mut cursor = coords.tl;
    let mut show_cursor = false;
    let mut prev_style = None;

    let mut iter = iter
        .map(|(nl, _, bit)| (nl, bit))
        .chain(std::iter::once((None, TextBit::Char(' '))));
    while let Some((nl, bit)) = iter.next() {
        if let Some(indent) = nl {
            clear_line(cursor, coords, info.x_shift, stdout);
            if cursor.y + 1 > coords.br.y {
                cursor.y += 1;
                break;
            }
            cursor.x = coords.tl.x + indent;
            indent_line(&form_former, cursor, coords, x_shift, stdout);
            cursor.y += 1;
        }

        if let &TextBit::Char(char) = &bit {
            last_char = real_char_from(char, &cfg.new_line, last_char);
            cursor.x += print_char(cursor, last_char, coords, x_shift, &cfg.tab_stops, stdout);
            if let Some(style) = prev_style.take() {
                queue!(stdout, ResetColor, SetStyle(style));
            }
        } else if let TextBit::Tag(tag) = bit {
            (show_cursor, prev_style) =
                trigger_tag(tag, show_cursor, is_active, &mut form_former, stdout);
        }
    }

    (cursor, show_cursor)
}

fn indent_line(
    form_former: &FormFormer, cursor: Coord, coords: Coords, x_shift: usize,
    stdout: &mut StdoutLock
) {
    let prev_style = form_former.make_form().style;
    let indent = " ".repeat((cursor.x.saturating_sub(coords.tl.x + x_shift as u16)) as usize);
    queue!(stdout, Print(indent), SetStyle(prev_style));
}

fn print_char(
    cursor: Coord, char: char, coords: Coords, x_shift: usize, tab_stops: &TabStops,
    stdout: &mut StdoutLock
) -> u16 {
    let len = len_from(char, cursor.x - coords.tl.x, tab_stops);

    if cursor.x < coords.tl.x + x_shift as u16 {
        let len = (cursor.x + len).saturating_sub(coords.tl.x + x_shift as u16) as usize;
        queue!(stdout, Print(" ".repeat(len)));
    } else if cursor.x + len <= coords.br.x + x_shift as u16 {
        match char {
            '\t' => queue!(stdout, Print(" ".repeat(len as usize))),
            char => queue!(stdout, Print(char))
        };
    } else if cursor.x < coords.br.x + x_shift as u16 {
        let len = (coords.br.x as usize + x_shift).saturating_sub(cursor.x as usize);
        queue!(stdout, Print(" ".repeat(len as usize)));
    }

    len
}

fn trigger_tag(
    tag: Tag, show_cursor: bool, is_active: bool, form_former: &mut FormFormer,
    stdout: &mut StdoutLock
) -> (bool, Option<ContentStyle>) {
    let mut prev_style = None;
    let mut cursor_shown = false;

    match tag {
        Tag::PushForm(id) => {
            queue!(stdout, ResetColor, SetStyle(form_former.apply(id).style));
        }
        Tag::PopForm(id) => {
            queue!(stdout, ResetColor, SetStyle(form_former.remove(id).style));
        }
        Tag::MainCursor => {
            let cursor_style = form_former.main_cursor();
            if let (Some(caret), true) = (cursor_style.caret, is_active) {
                queue!(stdout, caret, SavePosition);
                cursor_shown = true || show_cursor;
            } else {
                prev_style = Some(form_former.make_form().style);
                queue!(stdout, SetStyle(cursor_style.form.style));
            }
        }
        Tag::ExtraCursor => {
            queue!(stdout, SetStyle(form_former.extra_cursor().form.style));
        }
        Tag::HoverBound => todo!(),
        Tag::PermanentConceal { .. } => todo!()
    }

    (cursor_shown, prev_style)
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

fn clear_line(cursor: Coord, coords: Coords, x_shift: usize, stdout: &mut StdoutLock) {
    let len = (coords.br.x + x_shift as u16).saturating_sub(cursor.x) as usize;
    let (x, y) = (coords.tl.x, cursor.y);
    queue!(stdout, ResetColor, Print(" ".repeat(len.min(coords.width()))), MoveTo(x, y));
}
