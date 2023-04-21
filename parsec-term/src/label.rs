use std::io::{self, StdoutLock, Write};

use crossterm::{
    cursor::{self, MoveTo, SavePosition},
    queue,
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
}

impl ui::Label<Area> for Label {
    fn print(
        &mut self, iter: impl Iterator<Item = (usize, TextBit)>, info: PrintInfo, cfg: PrintCfg,
        palette: &FormPalette
    ) {
        let mut stdout = io::stdout().lock();
        let _ = queue!(stdout, MoveTo(self.area.tl().x, self.area.tl().y), cursor::Hide);

        let coords = self.area.coords();

        let form_former = palette.form_former();
        let iter =
            iter.filter(|(index, bit)| *index >= info.first_char() || bit.as_char().is_none());
        let indents = indents(iter, &cfg.tab_stops, self.area.width());
        let (mut cursor, show_cursor) = match cfg.wrap_method {
            WrapMethod::NoWrap | WrapMethod::Width => {
                print(indents, coords, self.is_active, info, &cfg, form_former, &mut stdout)
            }
            WrapMethod::Word => {
                let words = words(indents, &cfg.word_chars, self.area.width());
                print(words, coords, self.is_active, info, &cfg, form_former, &mut stdout)
            }
            WrapMethod::Capped(_) => todo!()
        };

        let mut stdout = io::stdout().lock();
        while cursor.y < coords.br.y {
            clear_line(cursor, coords, &mut stdout);
            cursor.y += 1;
        }

        if show_cursor {
            let _ = queue!(stdout, cursor::Show);
        }

        stdout.flush().unwrap();
    }

    fn set_as_active(&mut self) {
        self.is_active = true;
    }

    fn area(&self) -> &Area {
        &self.area
    }

    fn wrap_count(&self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg) -> usize {
        let coords = match cfg.wrap_method {
            WrapMethod::Capped(cap) => self.area.coords().from_tl(cap, self.area.height()),
            _ => self.area.coords()
        };
        let top_y = self.area.tl().y;

        let indents = indents(iter, &cfg.tab_stops, coords.width());
        let bottom_y = match cfg.wrap_method {
            WrapMethod::Width | WrapMethod::Capped(_) => {
                coords_iter(indents, coords, 0, &cfg.tab_stops, false)
                    .last()
                    .map(|(coord, ..)| coord.y)
            }
            WrapMethod::Word => {
                let words = words(indents, &cfg.word_chars, coords.width());
                coords_iter(words, coords, 0, &cfg.tab_stops, false)
                    .last()
                    .map(|(coord, ..)| coord.y)
            }
            WrapMethod::NoWrap => None
        };

        bottom_y.map(|bottom_y| (bottom_y - top_y) as usize).unwrap_or(0)
    }

    fn char_at_wrap(
        &self, mut iter: impl Iterator<Item = (usize, TextBit)>, wrap: usize, cfg: &PrintCfg
    ) -> Option<usize> {
        let coords = match cfg.wrap_method {
            WrapMethod::Capped(cap) => self.area.coords().from_tl(cap, self.area.height()),
            _ => self.area.coords()
        };
        let top_y = self.area.tl().y;
        match cfg.wrap_method {
            WrapMethod::Width | WrapMethod::Capped(_) => {
                let indents = indents(iter, &cfg.tab_stops, coords.width());
                coords_iter(indents, coords, 0, &cfg.tab_stops, false)
                    .flat_map(|(coord, once)| once.bits().map(move |inner| (coord, inner)))
                    .find(|(coord, ..)| coord.y == top_y + wrap as u16)
                    .map(|(_, (index, _))| index)
            }

            WrapMethod::Word => {
                let indents = indents(iter, &cfg.tab_stops, coords.width());
                let words = words(indents, &cfg.word_chars, coords.width());
                coords_iter(words, coords, 0, &cfg.tab_stops, false)
                    .flat_map(|(coord, word)| word.bits().map(move |inner| (coord, inner)))
                    .find(|(coord, ..)| coord.y == top_y + wrap as u16)
                    .map(|(_, (index, _))| index)
            }
            WrapMethod::NoWrap => iter.next().map(|(index, _)| index)
        }
    }

    fn get_width(&self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg) -> usize {
        let indents = indents(iter, &cfg.tab_stops, self.area.width());
        coords_iter(indents, self.area.coords(), 0, &cfg.tab_stops, true)
            .last()
            .map(|(coord, ..)| coord.x as usize)
            .unwrap_or(0)
    }

    fn col_at_dist(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, dist: usize, cfg: &PrintCfg
    ) -> usize {
        let indents = indents(iter, &cfg.tab_stops, self.area.width());
        coords_iter(indents, self.area.coords(), 0, &cfg.tab_stops, true)
            .enumerate()
            .find(|(_, (coord, ..))| coord.x as usize >= dist)
            .map(|(count, ..)| count)
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
            .map(|char| len_from(*char, start, x_shift, tab_stops))
            .unwrap_or(0)
    }

    fn is_new_line(&self) -> bool {
        self.1.as_char().is_some_and(|char| *char == '\n')
    }

    fn indent_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> Option<u16> {
        self.1.as_char().map(|char| indent_from(*char, start, x_shift, tab_stops))
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
                *x += len_from(*char, start + *x, x_shift, tab_stops);
                Some(*x)
            })
            .last()
            .unwrap_or(0)
    }

    fn is_new_line(&self) -> bool {
        self.first()
            .is_some_and(|(_, bit)| bit.as_char().is_some_and(|char| *char == '\n'))
    }

    fn indent_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> Option<u16> {
        self.iter()
            .filter_map(|(_, bit)| bit.as_char())
            .scan(0, |x, char| {
                *x = indent_from(*char, start + *x, x_shift, tab_stops);
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
                    if char == '\n' || !word_chars.contains(char) {
                        if word.is_empty() {
                            word.push(iter.next().map(|(_, insides)| insides).unwrap());
                        }
                        break;
                    // Case where the word will not fit, no matter
                    // what.
                    } else if len >= width - indent {
                        word_bits = std::mem::take(&mut word);
                        word = word_bits.pop().map(|insides| smallvec![insides]).unwrap();
                        break;
                    } else {
                        len += UnicodeWidthChar::width(char).unwrap_or(0) as u16;
                        word.push(iter.next().map(|(_, insides)| insides).unwrap());
                    }
                } else {
                    if len >= width - indent {
                        word_bits = word.drain(..).rev().collect();
                        word = word_bits.pop().map(|insides| smallvec![insides]).unwrap();
                        break;
                    }
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

fn coords_iter<'a, T>(
    iter: impl Iterator<Item = (u16, T)> + 'a, coords: Coords, x_shift: usize,
    tab_stops: &'a TabStops, no_wraps: bool
) -> impl Iterator<Item = (Coord, T)> + 'a
where
    T: TextIterable
{
    iter.scan(coords.tl, move |cursor, (indent, ti)| {
        let is_new_line = ti.is_new_line();

        let mut start = *cursor;
        let len = ti.len_from(cursor.x, x_shift, &tab_stops);
        cursor.x += len;
        // A tag (with `len == 0`) at the end of a line should be wrapped.
        if !no_wraps && (cursor.x > coords.br.x || (cursor.x == coords.br.x && len == 0)) {
            if start.x > indent {
                start = Coord::new(coords.tl.x + indent, cursor.y + 1);
            }

            *cursor = Coord::new(coords.tl.x + indent + len, cursor.y + 1);
        }
        if is_new_line {
            *cursor = Coord::new(coords.tl.x, cursor.y + 1);
        }

        Some((start, ti))
    })
}

fn print<T>(
    iter: impl Iterator<Item = (u16, T)>, coords: Coords, is_active: bool, info: PrintInfo,
    cfg: &PrintCfg, mut form_former: FormFormer, stdout: &mut StdoutLock
) -> (Coord, bool)
where
    T: TextIterable
{
    let no_wraps = cfg.wrap_method.is_no_wrap();
    let mut iter = coords_iter(iter, coords, info.x_shift(), &cfg.tab_stops, no_wraps)
        .take_while(move |(cursor, ..)| cursor.y < coords.br.y);

    let first_char = info.first_char();
    let mut final_cursor = coords.tl;
    let mut last_char = 'a';

    let mut show_cursor = false;
    let mut prev_style = None;

    while let Some((mut cursor, text_iterable)) = iter.next() {
        if final_cursor.y < cursor.y {
            clear_line(final_cursor, coords, stdout);
            let style = form_former.make_form().style;
            queue!(
                stdout,
                ResetColor,
                MoveTo(coords.tl.x, cursor.y),
                Print(" ".repeat((cursor.x - coords.tl.x) as usize)),
                SetStyle(style)
            )
            .unwrap();
        }

        for (index, bit) in text_iterable.bits() {
            if let (&TextBit::Char(char), true) = (&bit, index >= first_char) {
                let char = real_char_from(char, &cfg.new_line, last_char);
                let len = print_char(cursor, char, coords, info.x_shift(), &cfg.tab_stops, stdout);
                if let Some(style) = prev_style {
                    let _ = queue!(stdout, ResetColor, SetStyle(style));
                }
                cursor.x += len;
                last_char = char;
                prev_style = None;
            } else if let TextBit::Tag(tag) = bit {
                (show_cursor, prev_style) =
                    trigger_tag(tag, show_cursor, is_active, &mut form_former, stdout);
            }
        }
        final_cursor = cursor;
    }

    (final_cursor, show_cursor)
}

fn print_char(
    cursor: Coord, char: char, coords: Coords, x_shift: usize, tab_stops: &TabStops,
    stdout: &mut StdoutLock
) -> u16 {
    let len = len_from(char, cursor.x, x_shift, tab_stops);

    if cursor.x + len <= coords.br.x {
        let _ = match char {
            '\t' => queue!(stdout, Print(" ".repeat(len as usize))),
            char => queue!(stdout, Print(char))
        };
    } else if cursor.x < coords.br.x {
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

fn clear_line(cursor: Coord, coords: Coords, stdout: &mut StdoutLock) {
    queue!(
        stdout,
        ResetColor,
        Print(" ".repeat(coords.br.x.saturating_sub(cursor.x) as usize))
    )
    .unwrap();
}
