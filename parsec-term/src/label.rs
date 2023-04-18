use std::{
    io::{self, StdoutLock, Write},
    iter::repeat_with,
    ops::RangeInclusive
};

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
    text::{NewLine, PrintCfg, PrintInfo, TabStops, TextBit, WrapMethod},
    ui::{self, Area as UiArea}
};
use smallvec::SmallVec;
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

        let state = PrintState {
            coords: self.area.coords(),
            is_active: self.is_active,
            form_former: palette.form_former(),
            info,
            cfg,
            last_char: 'a',
            prev_style: None,
            show_cursor: false
        };

        let (mut cursor, show_cursor) = match state.cfg.wrap_method {
            WrapMethod::NoWrap | WrapMethod::Width => print(iter, state, &mut stdout),
            WrapMethod::Word => {
                let iter = words(iter, state.cfg.word_chars.clone());
                print(iter, state, &mut stdout)
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
        let tab_stops = cfg.tab_stops.clone();

        let bottom_y = match cfg.wrap_method {
            WrapMethod::Width | WrapMethod::Capped(_) => {
                coords_iter(iter, coords, 0, tab_stops, cfg.indent_wrap, false)
                    .last()
                    .map(|(coord, ..)| coord.y)
            }
            WrapMethod::Word => {
                let words = words(iter, cfg.word_chars.clone());
                coords_iter(words, coords, 0, tab_stops, cfg.indent_wrap, false)
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
        let tab_stops = cfg.tab_stops.clone();
        match cfg.wrap_method {
            WrapMethod::Width | WrapMethod::Capped(_) => {
                coords_iter(iter, coords, 0, tab_stops, cfg.indent_wrap, false)
                    .flat_map(|(coord, once)| once.bits().map(move |inner| (coord, inner)))
                    .find(|(coord, ..)| coord.y == top_y + wrap as u16)
                    .map(|(_, (index, _))| index)
            }

            WrapMethod::Word => {
                let words = words(iter, cfg.word_chars.clone());
                coords_iter(words, coords, 0, tab_stops, cfg.indent_wrap, false)
                    .flat_map(|(coord, word)| word.bits().map(move |inner| (coord, inner)))
                    .find(|(coord, ..)| coord.y == top_y + wrap as u16)
                    .map(|(_, (index, _))| index)
            }
            WrapMethod::NoWrap => iter.next().map(|(index, _)| index)
        }
    }

    fn get_width(&self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg) -> usize {
        coords_iter(iter, self.area.coords(), 0, cfg.tab_stops.clone(), false, true)
            .last()
            .map(|(coord, ..)| coord.x as usize)
            .unwrap_or(0)
    }

    fn col_at_dist(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, dist: usize, cfg: &PrintCfg
    ) -> usize {
        coords_iter(iter, self.area.coords(), 0, cfg.tab_stops.clone(), false, true)
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

struct PrintState<'a> {
    coords: Coords,
    is_active: bool,
    form_former: FormFormer<'a>,
    info: PrintInfo,
    cfg: PrintCfg,
    last_char: char,
    prev_style: Option<ContentStyle>,
    show_cursor: bool
}

fn end_from(char: char, start: u16, x_shift: usize, tab_stops: &TabStops) -> u16 {
    match char {
        '\t' => start + tab_stops.spaces_at(start as usize + x_shift) as u16,
        _ => start + UnicodeWidthChar::width(char).unwrap_or(0) as u16
    }
}

fn indent_from(char: char, start: u16, x_shift: usize, tab_stops: &TabStops) -> u16 {
    match char {
        '\t' | ' ' => end_from(char, start, x_shift, tab_stops),
        _ => start
    }
}

trait TextIterable {
    type Iterator: Iterator<Item = (usize, TextBit)>;
    fn end_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> u16;

    fn is_new_line(&self) -> bool;

    fn indent_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> Option<u16>;

    fn bits(self) -> Self::Iterator;
}

impl TextIterable for (usize, TextBit) {
    type Iterator = std::iter::Once<(usize, TextBit)>;

    fn end_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> u16 {
        self.1
            .as_char()
            .map(|char| end_from(*char, start, x_shift, tab_stops))
            .unwrap_or(start)
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

    fn end_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> u16 {
        self.iter()
            .filter_map(|(_, bit)| bit.as_char())
            .scan(start, |x, char| {
                *x = end_from(*char, *x, x_shift, tab_stops);
                Some(*x)
            })
            .last()
            .unwrap_or(start)
    }

    fn is_new_line(&self) -> bool {
        self.first()
            .is_some_and(|(_, bit)| bit.as_char().is_some_and(|char| *char == '\n'))
    }

    fn indent_from(&self, start: u16, x_shift: usize, tab_stops: &TabStops) -> Option<u16> {
        self.iter()
            .filter_map(|(_, bit)| bit.as_char())
            .scan(start, |x, char| {
                let end = indent_from(*char, *x, x_shift, tab_stops);
                if end > *x {
                    *x = end;
                    Some(*x)
                } else {
                    None
                }
            })
            .last()
    }

    fn bits(self) -> Self::Iterator {
        self.into_iter()
    }
}

fn words<'a>(
    iter: impl Iterator<Item = (usize, TextBit)> + 'a, word_chars: Vec<RangeInclusive<char>>
) -> impl Iterator<Item = SmallVec<[(usize, TextBit); 32]>> + 'a {
    let mut iter = iter.peekable();
    repeat_with(move || {
        let mut word = SmallVec::<[(usize, TextBit); 32]>::new();
        while let Some((_, bit)) = iter.peek() {
            if let &TextBit::Char(char) = bit {
                if char != '\n' || !word_chars.iter().any(|chars| chars.contains(&char)) {
                    if word.is_empty() {
                        word.push(iter.next().unwrap());
                    }
                    break;
                } else {
                    word.push(iter.next().unwrap());
                }
            }
        }
        if !word.is_empty() { Some(word) } else { None }
    })
    .flatten()
}

fn coords_iter<'a, T>(
    iter: impl Iterator<Item = T> + 'a, coords: Coords, x_shift: usize, tab_stops: TabStops,
    indent_wrap: bool, no_wraps: bool
) -> impl Iterator<Item = (Coord, T)>
where
    T: TextIterable
{
    let indent = coords.tl.x;
    iter.scan((coords.tl, indent, true), move |(cursor, indent, on_indent), measurable| {
        let is_new_line = measurable.is_new_line();
        if indent_wrap {
            if *on_indent {
                if let Some(new_indent) = measurable.indent_from(cursor.x, x_shift, &tab_stops) {
                    if new_indent > coords.br.x {
                        *indent = coords.tl.x;
                        *on_indent = false;
                    } else if new_indent > *indent {
                        *indent = new_indent;
                    } else {
                        *on_indent = false;
                    }
                }
            } else if is_new_line {
                *indent = coords.tl.x;
                *on_indent = true;
            }
        }

        let mut start = *cursor;
        cursor.x = measurable.end_from(cursor.x, x_shift, &tab_stops);
        if is_new_line {
            *cursor = Coord::new(*indent, cursor.y + 1);
        } else if cursor.x > coords.br.x && !no_wraps {
            let x = cursor.x - start.x;
            start = Coord::new(*indent, cursor.y + 1);
            *cursor = Coord::new(*indent + x, cursor.y + 1);
        }

        Some((start, measurable))
    })
}

fn print<T>(
    iter: impl Iterator<Item = T>, mut state: PrintState, stdout: &mut StdoutLock
) -> (Coord, bool)
where
    T: TextIterable
{
    let tab_stops = state.cfg.tab_stops.clone();
    let indent_wrap = state.cfg.indent_wrap;
    let no_wraps = state.cfg.wrap_method.is_no_wrap();
    let mut iter =
        coords_iter(iter, state.coords, state.info.x_shift(), tab_stops, indent_wrap, no_wraps)
            .take_while(move |(cursor, ..)| cursor.y < state.coords.br.y);

    let first_char = state.info.first_char();
    let mut final_cursor = state.coords.tl;
    while let Some((mut cursor, text_iterable)) = iter.next() {
        if final_cursor.y < cursor.y {
            clear_line(final_cursor, state.coords, stdout);
            let style = state.form_former.make_form().style;
            queue!(
                stdout,
                ResetColor,
                MoveTo(state.coords.tl.x, cursor.y),
                Print(" ".repeat((cursor.x - state.coords.tl.x) as usize)),
                SetStyle(style)
            )
            .unwrap();
        }

        for (index, bit) in text_iterable.bits() {
            final_cursor = cursor;

            if let TextBit::Char(char) = bit {
                if index < first_char {
                    continue;
                }

                print_char(cursor, char, &state, stdout);
                cursor.x = end_from(char, cursor.x, state.info.x_shift(), &state.cfg.tab_stops);
                state.last_char = char;
                state.prev_style = None;
            } else if let TextBit::Tag(tag) = bit {
                trigger_tag(tag, &mut state, stdout);
            }
        }
    }

    (final_cursor, state.show_cursor)
}

fn print_char(cursor: Coord, char: char, state: &PrintState, stdout: &mut StdoutLock) {
    let char = real_char_from(char, &state.cfg, state.last_char);
    let end = end_from(char, cursor.x, state.info.x_shift(), &state.cfg.tab_stops);

    if end <= state.coords.br.x {
        let _ = match char {
            '\t' => queue!(stdout, Print(" ".repeat((end - cursor.x) as usize))),
            char => queue!(stdout, Print(char))
        };
    } else if cursor.x < state.coords.br.x {
        let width = state.coords.br.x - cursor.x;
        let _ = queue!(stdout, Print(" ".repeat(width as usize)));
    }

    if let Some(style) = state.prev_style {
        let _ = queue!(stdout, ResetColor, SetStyle(style));
    }
}

fn trigger_tag(tag: Tag, state: &mut PrintState, stdout: &mut StdoutLock) {
    match tag {
        Tag::PushForm(id) => {
            let _ = queue!(stdout, SetStyle(state.form_former.apply(id).style));
        }
        Tag::PopForm(id) => {
            let _ = queue!(stdout, SetStyle(state.form_former.remove(id).style));
        }
        Tag::MainCursor => {
            let cursor = state.form_former.main_cursor();
            if let (Some(caret), true) = (cursor.caret, state.is_active) {
                let _ = queue!(stdout, caret, SavePosition);
                state.show_cursor = true;
            } else {
                state.prev_style = Some(state.form_former.make_form().style);
                let _ = queue!(stdout, SetStyle(cursor.form.style));
            }
        }
        Tag::ExtraCursor => {
            let _ = queue!(stdout, SetStyle(state.form_former.extra_cursor().form.style));
        }
        Tag::HoverBound => todo!(),
        Tag::PermanentConceal { .. } => todo!()
    }
}

fn real_char_from(char: char, print_cfg: &PrintCfg, last_char: char) -> char {
    match char {
        '\n' => match print_cfg.new_line {
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
        Print(" ".repeat(coords.br.x.saturating_sub(cursor.x + 1) as usize))
    )
    .unwrap();
}
