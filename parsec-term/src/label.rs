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
    text::{NewLine, PrintCfg, PrintInfo, TabStops, TextBit, TextIter, WrapMethod},
    ui::{self, Area as UiArea}
};
use ropey::RopeSlice;
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
    fn print<CI, TI>(
        &mut self, iter: TextIter<CI, TI>, info: PrintInfo, cfg: PrintCfg, palette: &FormPalette
    ) where
        CI: Iterator<Item = char>,
        TI: Iterator<Item = (usize, Tag)>
    {
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
                let iter = width_wrap_iter(iter, info.x_shift(), &state.cfg, self.area.br().x);
                print_wrap_width(iter, state, &mut stdout)
            }
            WrapMethod::Word => {
                let iter = width_wrap_iter(iter, info.x_shift(), &state.cfg, self.area.br().x);
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

    fn wrap_count(&self, slice: RopeSlice, print_cfg: &PrintCfg) -> usize {
        match print_cfg.wrap_method {
            WrapMethod::Width => self.get_width(slice, print_cfg) / self.area.width(),
            WrapMethod::Capped(_) => todo!(),
            WrapMethod::Word => self.get_width(slice, print_cfg) / self.area.width(),
            WrapMethod::NoWrap => 0
        }
    }

    fn col_at_wrap(&self, slice: RopeSlice, wrap: usize, print_cfg: &PrintCfg) -> usize {
        match print_cfg.wrap_method {
            WrapMethod::Width => {
                let dist = wrap * self.area.width();
                slice
                    .chars()
                    .enumerate()
                    .scan((0, false), |(width, end_reached), (index, char)| {
                        *width += len_of(char, &print_cfg.tab_stops, *width) as usize;
                        if *end_reached {
                            return None;
                        }
                        if *width >= dist {
                            *end_reached = true
                        }
                        Some(index)
                    })
                    .last()
                    .unwrap()
            }
            WrapMethod::Capped(_) => todo!(),
            WrapMethod::Word => todo!(),
            WrapMethod::NoWrap => 0
        }
    }

    fn get_width(&self, slice: RopeSlice, print_cfg: &PrintCfg) -> usize {
        slice
            .chars()
            .scan(0, |width, char| Some(len_of(char, &print_cfg.tab_stops, *width) as usize))
            .sum()
    }

    fn col_at_dist(&self, slice: RopeSlice, dist: usize, print_cfg: &PrintCfg) -> usize {
        slice
            .chars()
            .enumerate()
            .scan((0, false), |(width, end_reached), (index, char)| {
                *width += len_of(char, &print_cfg.tab_stops, *width) as usize;
                if *end_reached {
                    return None;
                }
                if *width >= dist {
                    *end_reached = true
                }
                Some(index)
            })
            .last()
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

fn width_wrap_iter<'a, 'b>(
    iter: impl Iterator<Item = (usize, TextBit)> + 'a, x_shift: usize, cfg: &'b PrintCfg,
    barrier: u16
) -> impl Iterator<Item = (u16, usize, TextBit)> + 'a {
    let tab_stops = cfg.tab_stops.clone();
    let indent_wrap = cfg.indent_wrap;
    iter.scan((0, true), move |(indent, add_to_indent), (index, text_bit)| {
        if indent_wrap {
            if *add_to_indent {
                if let TextBit::Char(char) = text_bit {
                    if char == ' ' || char == '\t' {
                        *indent += len_of(char, &tab_stops, x_shift);
                    } else {
                        *add_to_indent = false;
                    }
                }
            } else {
                if let TextBit::Char('\n') = text_bit {
                    *indent = 0;
                    *add_to_indent = true;
                }
            }
        }

        if *indent >= barrier {
            Some((0, index, text_bit))
        } else {
            Some((*indent, index, text_bit))
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
    mut iter: impl Iterator<Item = (u16, usize, TextBit)>, mut state: PrintState,
    stdout: &mut StdoutLock
) -> (Coord, bool) {
    let mut cursor = state.coords.tl;

    while let Some((indent, index, text_bit)) = iter.next() {
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

fn print_word_wrap(
    mut iter: impl Iterator<Item = (u16, usize, TextBit)>, mut state: PrintState,
    stdout: &mut StdoutLock
) -> (Coord, bool) {
    let mut cursor = state.coords.tl;
    let mut end_cursor = cursor;
    let mut cur_word = Vec::new();

    while let Some((indent, index, bit)) = iter.next() {
        if let TextBit::Char(char) = bit {
            if index < state.info.first_char() {
                continue;
            }

            if state.cfg.is_word_char(char) && char != '\n' {
                let x = cursor.x as usize + state.info.x_shift();
                end_cursor.x += len_of(char, &state.cfg.tab_stops, x);
                cur_word.push(bit);
            } else {
                if end_cursor.x > state.coords.br.x {
                    cursor = clear_to_next_line(cursor, char, indent, &state, stdout);
                }

                let drained_bits = cur_word.drain(..).chain(std::iter::once(bit));
                cursor = print_bits(drained_bits, cursor, indent, &mut state, stdout);

                end_cursor = cursor;
            }

            state.last_char = char;

            if cursor.y == state.coords.br.y {
                break;
            }
        } else {
            cur_word.push(bit);
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
    let char = mod_char(char, &state.cfg, state.last_char);

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

fn mod_char(char: char, print_cfg: &PrintCfg, last_char: char) -> char {
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
