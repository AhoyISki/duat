use std::io::{self, StdoutLock};

use crossterm::{
    cursor::{self, MoveTo, SavePosition},
    execute, queue,
    style::{ContentStyle, Print, ResetColor, SetStyle}
};
use parsec_core::{
    tags::{
        form::{FormFormer, FormPalette},
        Tag
    },
    text::{NewLine, PrintCfg, PrintInfo, TabStops, TextBit, WordChars, WrapMethod},
    ui::{self, Area as UiArea}
};
use smallvec::{smallvec, SmallVec};
use unicode_width::UnicodeWidthChar;

use crate::{Area, Coord, Coords};

pub struct Label {
    area: Area,
    is_active: bool
}

impl Label {
    pub fn new(area: Area) -> Self {
        Label {
            area,
            is_active: false
        }
    }

    fn wrapping_coords(&self, cfg: &PrintCfg) -> Coords {
        match cfg.wrap_method {
            WrapMethod::Capped(cap) => self.area.coords().from_tl(cap, self.area.height()),
            _ => self.area.coords()
        }
    }
}

impl ui::Label<Area> for Label {
    fn print(
        &mut self, iter: impl Iterator<Item = (usize, TextBit)>, info: PrintInfo, cfg: PrintCfg,
        palette: &FormPalette
    ) {
        let mut stdout = io::stdout().lock();
        let _ = queue!(stdout, MoveTo(self.area.tl().x, self.area.tl().y), cursor::Hide);

        let no_wraps = cfg.wrap_method.is_no_wrap();
        let mod_coords = self.wrapping_coords(&cfg);
        let indents = indents(iter, &cfg.tab_stops, mod_coords.width())
            .filter(|(_, (index, bit))| *index >= info.first_char() || bit.as_char().is_none());

        let form_former = palette.form_former();
        let (mut cursor, show_cursor) = if let WrapMethod::Word = cfg.wrap_method {
            let words = words(indents, &cfg.word_chars, self.area.width());
            let iter = width_iter(words, mod_coords, info.x_shift(), &cfg.tab_stops, no_wraps);
            print(iter, self.area.coords(), self.is_active, info, &cfg, form_former, &mut stdout)
        } else {
            let iter = width_iter(indents, mod_coords, info.x_shift(), &cfg.tab_stops, no_wraps);
            print(iter, self.area.coords(), self.is_active, info, &cfg, form_former, &mut stdout)
        };

        let mut stdout = io::stdout().lock();
        while mod_coords.br.y > cursor.y {
            clear_line(cursor, mod_coords, 0, &mut stdout);
            cursor.y += 1;
        }

        if show_cursor {
            let _ = queue!(stdout, cursor::Show);
        }

        let _ = execute!(stdout, ResetColor);
    }

    fn set_as_active(&mut self) {
        self.is_active = true;
    }

    fn area(&self) -> &Area {
        &self.area
    }

    fn wrap_count(&self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg) -> usize {
        let coords = self.wrapping_coords(cfg);
        let indents = indents(iter, &cfg.tab_stops, coords.width());
        match cfg.wrap_method {
            WrapMethod::Width | WrapMethod::Capped(_) => {
                width_iter(indents, coords, 0, &cfg.tab_stops, false)
                    .filter_map(|(new_line, _)| new_line)
                    .count()
                    - 1
            }
            WrapMethod::Word => {
                let words = words(indents, &cfg.word_chars, coords.width());
                width_iter(words, coords, 0, &cfg.tab_stops, false)
                    .filter_map(|(new_line, _)| new_line)
                    .count()
                    - 1
            }
            WrapMethod::NoWrap => 0
        }
    }

    fn char_at_wrap(
        &self, mut iter: impl Iterator<Item = (usize, TextBit)>, wrap: usize, cfg: &PrintCfg
    ) -> Option<usize> {
        let coords = self.wrapping_coords(cfg);
        match cfg.wrap_method {
            WrapMethod::Width | WrapMethod::Capped(_) => {
                let indents = indents(iter, &cfg.tab_stops, coords.width());
                width_iter(indents, coords, 0, &cfg.tab_stops, false)
                    .filter_map(|(new_line, ti)| {
                        new_line.map(|_| ti.bits().next().map(|(index, _)| index))
                    })
                    .flatten()
                    .nth(wrap)
            }

            WrapMethod::Word => {
                let indents = indents(iter, &cfg.tab_stops, coords.width());
                let words = words(indents, &cfg.word_chars, coords.width());
                width_iter(words, coords, 0, &cfg.tab_stops, false)
                    .filter_map(|(new_line, ti)| new_line.map(|_| ti))
                    .nth(wrap)
                    .map(|ti| ti.bits().next().map(|(index, _)| index))
                    .flatten()
            }
            WrapMethod::NoWrap => iter.next().map(|(index, _)| index)
        }
    }

    fn get_width(&self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg) -> usize {
        let coords = self.wrapping_coords(cfg);
        let indents = indents(iter, &cfg.tab_stops, coords.width());
        let iter = width_iter(indents, coords, 0, &cfg.tab_stops, cfg.wrap_method.is_no_wrap());
        iter.fold(0, |mut width, (cr, (_, bit))| {
            if let Some(indent) = cr {
                width = indent;
            };

            if let TextBit::Char(char) = bit {
                width += len_from(char, width, 0, &cfg.tab_stops)
            }

            width
        }) as usize
    }

    fn col_at_dist(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, dist: usize, cfg: &PrintCfg
    ) -> usize {
        iter.filter_map(|(_, bit)| bit.as_char())
            .enumerate()
            .scan(0, |width, (index, char)| {
                let old_width = *width as usize;
                *width += len_from(char, *width, 0, &cfg.tab_stops);
                if old_width < dist { Some(index) } else { None }
            })
            .last()
            .unwrap_or(0)
    }

    fn area_index(&self) -> usize {
        self.area.index
    }
}

unsafe impl Send for Label {}
unsafe impl Sync for Label {}

fn len_from(char: char, start: u16, x_shift: usize, tab_stops: &TabStops) -> u16 {
    match char {
        '\t' => tab_stops.spaces_at(start as usize + x_shift) as u16,
        '\n' => 1,
        _ => UnicodeWidthChar::width(char).unwrap_or(0) as u16
    }
}

fn indent_from(char: char, start: u16, x_shift: usize, tab_stops: &TabStops) -> u16 {
    match char {
        '\t' | ' ' => len_from(char, start, x_shift, tab_stops),
        _ => 0
    }
}

trait TextIterable {
    type Iterator: Iterator<Item = (usize, TextBit)>;
    fn len_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> u16;

    fn is_new_line(&self) -> bool;

    fn indent_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> Option<u16>;

    fn bits(self) -> Self::Iterator;
}

impl TextIterable for (usize, TextBit) {
    type Iterator = std::iter::Once<(usize, TextBit)>;

    fn len_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> u16 {
        self.1
            .as_char()
            .map(|char| len_from(char, start, x_shift, tab_stops))
            .unwrap_or(0)
    }

    fn is_new_line(&self) -> bool {
        self.1.as_char().is_some_and(|char| char == '\n')
    }

    fn indent_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> Option<u16> {
        self.1.as_char().map(|char| indent_from(char, start, x_shift, tab_stops))
    }

    fn bits(self) -> Self::Iterator {
        std::iter::once(self)
    }
}

impl TextIterable for SmallVec<[(usize, TextBit); 32]> {
    type Iterator = smallvec::IntoIter<[(usize, TextBit); 32]>;

    fn len_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> u16 {
        self.iter()
            .filter_map(|(_, bit)| bit.as_char())
            .scan(0, |x, char| {
                *x += len_from(char, start + *x, x_shift, tab_stops);
                Some(*x)
            })
            .last()
            .unwrap_or(0)
    }

    fn is_new_line(&self) -> bool {
        self.first()
            .is_some_and(|(_, bit)| bit.as_char().is_some_and(|char| char == '\n'))
    }

    fn indent_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> Option<u16> {
        self.iter()
            .filter_map(|(_, bit)| bit.as_char())
            .scan(0, |x, char| {
                *x = indent_from(char, start + *x, x_shift, tab_stops);
                Some(*x)
            })
            .last()
    }

    fn bits(self) -> Self::Iterator {
        self.into_iter()
    }
}

/// Returns an [`Iterator`] that also shows the current level of
/// indentation.
fn indents<'a>(
    iter: impl Iterator<Item = (usize, TextBit)> + 'a, tab_stops: &'a TabStops, width: usize
) -> impl Iterator<Item = (u16, (usize, TextBit))> + 'a {
    iter.scan((0, true), move |(indent, on_indent), (index, bit)| {
        let old_indent = if *indent < width { *indent } else { 0 };
        (*indent, *on_indent) = match (&bit, *on_indent) {
            (&TextBit::Char('\t'), true) => (*indent + tab_stops.spaces_at(*indent), true),
            (&TextBit::Char(' '), true) => (*indent + 1, true),
            (&TextBit::Char('\n'), _) => (0, true),
            (&TextBit::Char(_), _) => (*indent, false),
            (&TextBit::Tag(_), on_indent) => (*indent, on_indent)
        };

        Some((old_indent as u16, (index, bit)))
    })
}

/// Returns an [`Iterator`] over the sequences of [`WordChars`].
fn words<'a>(
    iter: impl Iterator<Item = (u16, (usize, TextBit))> + 'a, word_chars: &'a WordChars,
    width: usize
) -> impl Iterator<Item = (u16, SmallVec<[(usize, TextBit); 32]>)> + 'a {
    let mut iter = iter.peekable();
    let mut word_bits = SmallVec::new();
    let width = width as u16;
    let mut indent = 0;
    std::iter::from_fn(move || {
        if let Some(word) = word_bits.pop().map(|insides| smallvec![insides]) {
            Some((indent, word))
        } else {
            let mut len = 0;
            let mut word = SmallVec::new();
            while let Some((new_indent, (_, bit))) = iter.peek() {
                indent = *new_indent;
                if let &TextBit::Char(char) = bit {
                    len += UnicodeWidthChar::width(char).unwrap_or(0) as u16;
                    if char == '\n' || !word_chars.contains(char) {
                        if word.is_empty() {
                            word.push(iter.next().map(|(_, insides)| insides).unwrap());
                        }
                        break;
                    // Case where the word will not fit, no matter
                    // what.
                    } else if len > width - indent {
                        word_bits = std::mem::take(&mut word);
                        word_bits.reverse();
                        word = word_bits.pop().map(|insides| smallvec![insides]).unwrap();
                        break;
                    } else {
                        word.push(iter.next().map(|(_, insides)| insides).unwrap());
                    }
                } else if len >= width - indent {
                    break;
                } else {
                    word.push(iter.next().map(|(_, insides)| insides).unwrap());
                }
            }

            if word.is_empty() {
                None
            } else {
                Some((indent, word))
            }
        }
    })
}

fn width_iter<'a, T>(
    iter: impl Iterator<Item = (u16, T)> + 'a, coords: Coords, x_shift: usize,
    tab_stops: &'a TabStops, no_wraps: bool
) -> impl Iterator<Item = (Option<u16>, T)> + 'a
where
    T: TextIterable
{
    iter.scan((coords.tl.x, true), move |(x, new_line), (indent, ti)| {
        let is_new_line = ti.is_new_line();

        let len = ti.len_from(*x, x_shift, &tab_stops);
        *x += len;
        // A tag (with `len == 0`) at the end of a line should be wrapped.
        if !no_wraps && (*x > coords.br.x || (*x == coords.br.x && len == 0)) {
            *new_line = *x - len > coords.tl.x + indent;
            *x = coords.tl.x + indent + len;
        }

        let cr = if *new_line {
            *x = coords.tl.x + indent + len;
            *new_line = false;
            Some(indent)
        } else {
            None
        };

        if is_new_line {
            *x = coords.tl.x;
            *new_line = true;
        }

        Some((cr, ti))
    })
}

fn print<T>(
    mut iter: impl Iterator<Item = (Option<u16>, T)>, coords: Coords, is_active: bool,
    info: PrintInfo, cfg: &PrintCfg, mut form_former: FormFormer, stdout: &mut StdoutLock
) -> (Coord, bool)
where
    T: TextIterable
{
    let x_shift = info.x_shift();
    let mut last_char = 'a';
    let mut cursor = coords.tl;
    let mut show_cursor = false;
    let mut prev_style = None;

    while let Some((cr, text_iterable)) = iter.next() {
        if let Some(indent) = cr {
            clear_line(cursor, coords, info.x_shift(), stdout);

            if cursor.y + 1 > coords.br.y {
                break;
            }
            cursor.x = coords.tl.x + indent;
            indent_line(&form_former, cursor, coords, x_shift, stdout);
            cursor.y += 1;
        }

        for (_, bit) in text_iterable.bits() {
            if let &TextBit::Char(char) = &bit {
                last_char = real_char_from(char, &cfg.new_line, last_char);
                cursor.x += print_char(cursor, last_char, coords, x_shift, &cfg.tab_stops, stdout);
                if let Some(style) = prev_style.take() {
                    let _ = queue!(stdout, ResetColor, SetStyle(style));
                }
            } else if let TextBit::Tag(tag) = bit {
                (show_cursor, prev_style) =
                    trigger_tag(tag, show_cursor, is_active, &mut form_former, stdout);
            }
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
    let (x, y) = (coords.tl.x, cursor.y);
    let _ = queue!(stdout, MoveTo(x, y), Print(indent), SetStyle(prev_style));
}

fn print_char(
    cursor: Coord, char: char, coords: Coords, x_shift: usize, tab_stops: &TabStops,
    stdout: &mut StdoutLock
) -> u16 {
    let len = len_from(char, cursor.x, x_shift, tab_stops);

    if cursor.x < coords.tl.x + x_shift as u16 {
        let len = (cursor.x + len).saturating_sub(coords.tl.x + x_shift as u16) as usize;
        let _ = queue!(stdout, Print(" ".repeat(len)));
    } else if cursor.x + len <= coords.br.x + x_shift as u16 {
        let _ = match char {
            '\t' => queue!(stdout, Print(" ".repeat(len as usize))),
            char => queue!(stdout, Print(char))
        };
    } else if cursor.x < coords.br.x + x_shift as u16 {
        let width = coords.br.x - cursor.x;
        let _ = queue!(stdout, Print(" ".repeat(width as usize)));
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
            let _ = queue!(stdout, SetStyle(form_former.apply(id).style));
        }
        Tag::PopForm(id) => {
            let _ = queue!(stdout, SetStyle(form_former.remove(id).style));
        }
        Tag::MainCursor => {
            let cursor = form_former.main_cursor();
            if let (Some(caret), true) = (cursor.caret, is_active) {
                let _ = queue!(stdout, caret, SavePosition);
                cursor_shown = true || show_cursor;
            } else {
                prev_style = Some(form_former.make_form().style);
                let _ = queue!(stdout, SetStyle(cursor.form.style));
            }
        }
        Tag::ExtraCursor => {
            let _ = queue!(stdout, SetStyle(form_former.extra_cursor().form.style));
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
    queue!(stdout, ResetColor, Print(" ".repeat(len.min(coords.width())))).unwrap();
}
