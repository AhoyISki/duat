use std::io::{self, StdoutLock};

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
            WrapMethod::NoWrap => print_no_wrap(iter, state, &mut stdout),
            WrapMethod::Width => {
                let iter = barrier_wrap_iter(iter, info.x_shift(), self.area.br().x, &state.cfg);
                print_wrap_width(iter, state, &mut stdout)
            }
            WrapMethod::Word => {
                let iter = barrier_wrap_iter(iter, info.x_shift(), self.area.br().x, &state.cfg);
                print_word_wrap(iter, state, &mut stdout)
            }
            WrapMethod::Capped(_) => todo!()
        };

        let mut stdout = io::stdout().lock();
        while cursor.y < coords.br.y {
            cursor = next_line(cursor, coords, &mut stdout, ContentStyle::default());
        }

        if show_cursor {
            let _ = queue!(stdout, cursor::Show);
        }
    }

    fn set_as_active(&mut self) {
        self.is_active = true;
    }

    fn area(&self) -> &Area {
        &self.area
    }

    fn wrap_count(&self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg) -> usize {
        match cfg.wrap_method {
            WrapMethod::Width => {
                width_sum_iter(iter, self.area.width() as u16, cfg)
                    .map(|(v_sum, ..)| v_sum as usize)
                    .last()
                    .unwrap_or(0)
                    / self.area.width()
            }
            WrapMethod::Capped(cap) => {
                width_sum_iter(iter, cap as u16, cfg)
                    .map(|(v_sum, ..)| v_sum as usize)
                    .last()
                    .unwrap_or(0)
                    / self.area.width()
            }
            WrapMethod::Word => self.get_width(iter, cfg) / self.area.width(),
            WrapMethod::NoWrap => 0
        }
    }

    fn col_at_wrap(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, wrap: usize, cfg: &PrintCfg
    ) -> usize {
        match cfg.wrap_method {
            WrapMethod::Width => {
                let total_width = (self.area.width() * wrap) as u16;
                width_sum_iter(iter, self.area.width() as u16, cfg)
                    .enumerate()
                    .find(|(_, (v_sum, ..))| *v_sum >= total_width)
                    .map(|(count, ..)| count)
                    .unwrap_or(0)
            }
            WrapMethod::Capped(cap) => {
                let total_width = (cap * wrap) as u16;
                width_sum_iter(iter, cap as u16, cfg)
                    .enumerate()
                    .find(|(_, (v_sum, ..))| *v_sum >= total_width)
                    .map(|(count, ..)| count)
                    .unwrap_or(0)
            }

            WrapMethod::Word => todo!(),
            WrapMethod::NoWrap => 0
        }
    }

    fn get_width(&self, iter: impl Iterator<Item = (usize, TextBit)>, cfg: &PrintCfg) -> usize {
        width_sum_iter(iter, self.area.width() as u16, cfg)
            .map(|(_, sum, ..)| sum as usize)
            .last()
            .unwrap_or(0)
    }

    fn col_at_dist(
        &self, iter: impl Iterator<Item = (usize, TextBit)>, dist: usize, cfg: &PrintCfg
    ) -> usize {
        width_sum_iter(iter, self.area.width() as u16, cfg)
            .enumerate()
            .find(|(_, (_, sum, ..))| *sum as usize >= dist)
            .map(|(count, ..)| count)
            .unwrap_or(0)
    }

    fn area_index(&self) -> usize {
        self.area.index
    }
}

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

/// Returns an [`Iterator`] that counts the width sum of [`char`]s.
///
/// This [`Iterator`]'s [`Item`][Iterator::Item] tuple is read,
/// respectively, as:
/// - The visual width sum, which includes indent wraps.
/// - The actual width sum, which doesn't.
/// - The index of the [`char`], relative to the beginning of the
///   file.
/// - The [`char`] in question.
///
/// The returned widths represent the *START* of a character, not
/// where it'll end.
fn width_sum_iter<'a>(
    iter: impl Iterator<Item = (usize, TextBit)> + 'a, barrier: u16, cfg: &PrintCfg
) -> impl Iterator<Item = (u16, u16, usize)> + 'a {
    let tab_stops = cfg.tab_stops.clone();
    barrier_wrap_iter(iter, 0, barrier, cfg)
        .filter_map(|(indent, x, index, bit)| bit.as_char().map(|char| (indent, x, index, *char)))
        .scan((barrier, 0, 0), move |(next_barrier, v_sum, sum), (indent, x, index, char)| {
            if *v_sum >= *next_barrier {
                *v_sum += indent;
                *next_barrier += barrier;
            }

            let len = len_of(char, &tab_stops, x as usize);
            *v_sum += len;
            *sum += len;
            Some((*v_sum - len, *sum - len, index))
        })
}

fn barrier_wrap_iter<'a>(
    iter: impl Iterator<Item = (usize, TextBit)> + 'a, x_shift: usize, barrier: u16, cfg: &PrintCfg
) -> impl Iterator<Item = (u16, u16, usize, TextBit)> + 'a {
    let tab_stops = cfg.tab_stops.clone();
    let indent_wrap = cfg.indent_wrap;
    iter.scan((0, true, x_shift as u16), move |(indent, add_to_indent, x), (index, bit)| {
        let old_x = *x;
        if let TextBit::Char(char) = bit {
            *x += len_of(char, &tab_stops, *x as usize) as u16;
            if indent_wrap {
                if *add_to_indent {
                    if char == ' ' || char == '\t' {
                        *indent = *x;
                    } else {
                        *add_to_indent = false;
                    }
                } else if char == '\n' {
                    *x = 0;
                    *indent = 0;
                    *add_to_indent = true;
                }
            }
        }

        if *indent >= barrier {
            Some((0, old_x, index, bit))
        } else {
            Some((*indent, old_x, index, bit))
        }
    })
}

fn print_no_wrap(
    mut iter: impl Iterator<Item = (usize, TextBit)>, mut state: PrintState,
    stdout: &mut StdoutLock
) -> (Coord, bool) {
    let mut cursor = state.coords.tl;
    let mut skip_til_nl = false;

    while let Some((index, text_bit)) = iter.next() {
        if let TextBit::Char(char) = text_bit {
            if index < state.info.first_char() || (skip_til_nl && state.last_char != '\n') {
                state.last_char = char;
                continue;
            } else {
                skip_til_nl = false
            }

            if cursor.y == state.coords.br.y {
                break;
            }

            (cursor, state.last_char) = print_char(cursor, char, &state, stdout);

            if char == '\n' || cursor.x == state.coords.br.x {
                cursor = clear_to_next_line(cursor, char, 0, &state, stdout);
                if char != '\n' {
                    skip_til_nl = true;
                }
            }
        } else if let TextBit::Tag(tag) = text_bit {
            trigger_tag(tag, &mut state, stdout);
        }
    }

    (cursor, state.show_cursor)
}

fn print_wrap_width(
    mut iter: impl Iterator<Item = (u16, u16, usize, TextBit)>, mut state: PrintState,
    stdout: &mut StdoutLock
) -> (Coord, bool) {
    let mut cursor = state.coords.tl;

    while let Some((indent, _, index, text_bit)) = iter.next() {
        if let TextBit::Char(char) = text_bit {
            if index < state.info.first_char() {
                continue;
            }

            (cursor, state.last_char) = print_char(cursor, char, &state, stdout);
            state.prev_style = None;

            if char == '\n' || cursor.x == state.coords.br.x {
                cursor = clear_to_next_line(cursor, char, indent, &state, stdout);
            }

            if cursor.y == state.coords.br.y {
                break;
            }
        } else if let TextBit::Tag(tag) = text_bit {
            trigger_tag(tag, &mut state, stdout);
        }
    }

    (cursor, state.show_cursor)
}

fn print_word_wrap(
    mut iter: impl Iterator<Item = (u16, u16, usize, TextBit)>, mut state: PrintState,
    stdout: &mut StdoutLock
) -> (Coord, bool) {
    let mut cursor = state.coords.tl;
    let mut end_cursor = cursor;
    let mut cur_word = Vec::new();

    while let Some((indent, _, index, bit)) = iter.next() {
        cur_word.push(bit);
        if let TextBit::Char(char) = *cur_word.last().unwrap() {
            if index < state.info.first_char() {
                continue;
            }

            if state.cfg.is_word_char(char) && char != '\n' {
                let x = cursor.x as usize + state.info.x_shift();
                end_cursor.x += len_of(char, &state.cfg.tab_stops, x);
            } else {
                let start_x = state.coords.tl.x + indent;
                if end_cursor.x > state.coords.br.x && cursor.x > start_x {
                    cursor = clear_to_next_line(cursor, char, indent, &state, stdout);
                }

                let drained_bits = cur_word.drain(..);
                cursor = print_bits(drained_bits, cursor, indent, &mut state, stdout);

                end_cursor = cursor;
            }

            state.last_char = char;

            if cursor.y == state.coords.br.y {
                break;
            }
        }
    }

    (cursor, state.show_cursor)
}

fn print_bits(
    bits: impl Iterator<Item = TextBit>, mut cursor: Coord, indent: u16, state: &mut PrintState,
    stdout: &mut StdoutLock
) -> Coord {
    for bit in bits {
        if let TextBit::Char(char) = bit {
            (cursor, state.last_char) = print_char(cursor, char, state, stdout);

            if char == '\n' || cursor.x == state.coords.br.x {
                let style = state.form_former.make_form().style;
                cursor = next_line(cursor, state.coords, stdout, style);
                if char != '\n' && indent > 0 {
                    let _ = queue!(stdout, Print(" ".repeat(indent as usize)));
                    cursor.x += indent;
                }
            }

            if cursor.y == state.coords.br.y {
                break;
            }
        } else if let TextBit::Tag(tag) = bit {
            trigger_tag(tag, state, stdout);
        }
    }

    cursor
}

fn print_char(
    mut cursor: Coord, char: char, state: &PrintState, stdout: &mut StdoutLock
) -> (Coord, char) {
    let char = real_char_from(char, &state.cfg, state.last_char);

    let len = len_of(char, &state.cfg.tab_stops, cursor.x as usize + state.info.x_shift());

    if cursor.x <= state.coords.br.x - len {
        cursor.x += len;
        let _ = match char {
            '\t' => queue!(stdout, Print(" ".repeat(len as usize))),
            char => queue!(stdout, Print(char))
        };
    } else if cursor.x < state.coords.br.x {
        let width = state.coords.br.x - cursor.x;
        cursor.x += width;
        let _ = queue!(stdout, Print(" ".repeat(width as usize)));
    }

    if let Some(style) = state.prev_style {
        let _ = queue!(stdout, ResetColor, SetStyle(style));
    }

    (cursor, char)
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
    let char = match char {
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
    };
    char
}

fn clear_to_next_line(
    mut cursor: Coord, char: char, indent: u16, state: &PrintState, stdout: &mut StdoutLock
) -> Coord {
    let style = state.form_former.make_form().style;
    cursor = next_line(cursor, state.coords, stdout, style);
    if char != '\n' && indent > 0 {
        let _ = queue!(stdout, Print(" ".repeat(indent as usize)));
        cursor.x += indent;
    }

    cursor
}

fn next_line(
    mut cursor: Coord, coords: Coords, stdout: &mut StdoutLock, style: ContentStyle
) -> Coord {
    let _ = queue!(stdout, ResetColor);

    if cursor.x < coords.br.x {
        // The rest of the line is featureless.
        let padding_count = (coords.br.x - cursor.x) as usize;
        queue!(stdout, Print(" ".repeat(padding_count))).unwrap();
    }

    cursor.y += 1;
    cursor.x = coords.tl.x;

    let _ = queue!(stdout, ResetColor, MoveTo(cursor.x, cursor.y), SetStyle(style));

    cursor
}

fn len_of(char: char, tab_stops: &TabStops, x: usize) -> u16 {
    match char {
        ' ' => 1,
        '\t' => tab_stops.spaces_at(x) as u16,
        _ => UnicodeWidthChar::width(char).unwrap_or(0) as u16
    }
}

unsafe impl Send for Label {}
unsafe impl Sync for Label {}
