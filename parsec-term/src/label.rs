use std::io::{stdout, StdoutLock};

use crossterm::{
    cursor::{self, MoveTo, SavePosition, Show},
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
        let mut stdout = stdout().lock();
        let _ = queue!(stdout, MoveTo(self.area.tl().x, self.area.tl().y), cursor::Hide);

        let coords = self.area.coords();

        let print_opts = PrintOpts {
            is_active: self.is_active,
            form_former: palette.form_former(),
            info,
            cfg
        };

        let (mut cursor, show_cursor) = match print_opts.cfg.wrap_method {
            WrapMethod::NoWrap => print_no_wrap(iter, self.area.coords(), print_opts),
            WrapMethod::Width => {
                let iter = width_wrap_iter(iter, info.x_shift(), &print_opts.cfg, self.area.br().x);
                print_wrap_width(iter, self.area.coords(), print_opts)
            }
            WrapMethod::Word => {
                let iter = width_wrap_iter(iter, info.x_shift(), &print_opts.cfg, self.area.br().x);
                print_word_wrap(iter, self.area.coords(), print_opts)
            }
            WrapMethod::Capped(_) => todo!()
        };

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

struct PrintOpts<'a> {
    is_active: bool,
    form_former: FormFormer<'a>,
    info: PrintInfo,
    cfg: PrintCfg
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
    mut iter: impl Iterator<Item = (usize, TextBit)>, coords: Coords, mut opts: PrintOpts
) -> (Coord, bool) {
    let mut stdout = stdout().lock();
    let mut cursor = coords.tl;
    let mut last_char = 'a';
    let mut style_bef_cursor = None;
    let mut show_cursor = false;
    let mut skip_til_nl = false;

    while let Some((index, text_bit)) = iter.next() {
        if let TextBit::Char(char) = text_bit {
            if index < opts.info.first_char() || (skip_til_nl && last_char != '\n') {
                last_char = char;
                continue;
            } else {
                skip_til_nl = false
            }

            if cursor.y == coords.br.y {
                break;
            }

            cursor = print_char(char, last_char, coords.br, cursor, &mut stdout, &opts);
            last_char = char;

            if let Some(style) = style_bef_cursor.take() {
                let _ = queue!(stdout, ResetColor, SetStyle(style));
            }

            if char == '\n' || cursor.x == coords.br.x {
                let style = opts.form_former.make_form().style;
                cursor = next_line(cursor, coords, &mut stdout, style);
                if char != '\n' {
                    skip_til_nl = true;
                }
            }
        } else if let TextBit::Tag(tag) = text_bit {
            let (cursor_printed, style) = trigger_tag(tag, &mut stdout, &mut opts);
            show_cursor |= cursor_printed;
            style_bef_cursor = style;
        }
    }

    (cursor, show_cursor)
}

fn print_wrap_width(
    mut iter: impl Iterator<Item = (u16, usize, TextBit)>, coords: Coords, mut opts: PrintOpts
) -> (Coord, bool) {
    let mut stdout = stdout().lock();
    let mut cursor = coords.tl;
    let mut last_char = 'a';
    let mut style_bef_cursor = None;
    let mut show_cursor = false;

    while let Some((indent, index, text_bit)) = iter.next() {
        if let TextBit::Char(char) = text_bit {
            if index < opts.info.first_char() {
                continue;
            }

            cursor = print_char(char, last_char, coords.br, cursor, &mut stdout, &opts);
            last_char = char;

            if let Some(style) = style_bef_cursor.take() {
                let _ = queue!(stdout, ResetColor, SetStyle(style));
            }

            if char == '\n' || cursor.x == coords.br.x {
                let style = opts.form_former.make_form().style;
                cursor = next_line(cursor, coords, &mut stdout, style);
                if char != '\n' && indent > 0 {
                    let _ = queue!(stdout, Print(" ".repeat(indent as usize)));
                    cursor.x += indent;
                }
            }

            if cursor.y == coords.br.y {
                break;
            }
        } else if let TextBit::Tag(tag) = text_bit {
            let (cursor_printed, style) = trigger_tag(tag, &mut stdout, &mut opts);
            show_cursor |= cursor_printed;
            style_bef_cursor = style;
        }
    }

    (cursor, show_cursor)
}

fn print_word_wrap(
    mut iter: impl Iterator<Item = (u16, usize, TextBit)>, coords: Coords, mut opts: PrintOpts
) -> (Coord, bool) {
    let mut stdout = stdout().lock();
    let mut cursor = coords.tl;
    let mut end_cursor = coords.tl;
    let mut last_char = 'a';
    let mut style_bef_cursor = None;
    let mut show_cursor = false;

    let mut cur_word = Vec::new();

    while let Some((indent, index, bit)) = iter.next() {
        if let TextBit::Char(char) = bit {
            if index < opts.info.first_char() {
                continue;
            }

            if opts.cfg.is_word_char(last_char) && last_char != '\n' {
                let x = cursor.x as usize + opts.info.x_shift();
                let len = len_of(last_char, &opts.cfg.tab_stops, x);
                // If '\n' is a word char, this whole thing goes outta wack.
                if end_cursor.x + len <= coords.br.x {
                    end_cursor.x += len;
                } else {
                    let style = opts.form_former.make_form().style;
                    cursor = next_line(cursor, coords, &mut stdout, style);
                    if indent > 0 {
                        let _ = queue!(stdout, Print(" ".repeat(indent as usize)));
                        cursor.x += indent;
                    }
                    end_cursor = cursor;
                }
            } else {
                let drain = cur_word.drain(..);
                let (new_cursor, cursor_printed, style) = print_bits(
                    drain,
                    cursor,
                    coords,
                    last_char,
                    style_bef_cursor,
                    indent,
                    &mut stdout,
                    &mut opts
                );

                cursor = new_cursor;
                end_cursor = cursor;

                show_cursor |= cursor_printed;
                style_bef_cursor = style;
            }

            last_char = char;

            if cursor.y == coords.br.y {
                break;
            }
        }

        cur_word.push(bit);
    }

    (cursor, show_cursor)
}

fn print_bits(
    bits: impl Iterator<Item = TextBit>, mut cursor: Coord, coords: Coords, mut last_char: char,
    mut style_bef_cursor: Option<ContentStyle>, indent: u16, stdout: &mut StdoutLock,
    opts: &mut PrintOpts
) -> (Coord, bool, Option<ContentStyle>) {
    let mut show_cursor = false;

    for bit in bits {
        if let TextBit::Char(char) = bit {
            cursor = print_char(char, last_char, coords.br, cursor, stdout, opts);
            last_char = char;

            if let Some(style) = style_bef_cursor.take() {
                let _ = queue!(stdout, ResetColor, SetStyle(style));
            }

            if char == '\n' || cursor.x == coords.br.x {
                let style = opts.form_former.make_form().style;
                cursor = next_line(cursor, coords, stdout, style);
                if char != '\n' && indent > 0 {
                    let _ = queue!(stdout, Print(" ".repeat(indent as usize)));
                    cursor.x += indent;
                }
            }

            if cursor.y == coords.br.y {
                break;
            }
        } else if let TextBit::Tag(tag) = bit {
            let (cursor_printed, style) = trigger_tag(tag, stdout, opts);
            show_cursor |= cursor_printed;
            style_bef_cursor = style;
        }
    }

    (cursor, show_cursor, style_bef_cursor)
}

fn trigger_tag(
    tag: Tag, stdout: &mut StdoutLock, printer: &mut PrintOpts
) -> (bool, Option<ContentStyle>) {
    let mut style_bef_cursor = None;
    match tag {
        Tag::PushForm(id) => {
            let _ = queue!(stdout, SetStyle(printer.form_former.apply(id).style));
        }
        Tag::PopForm(id) => {
            let _ = queue!(stdout, SetStyle(printer.form_former.remove(id).style));
        }
        Tag::MainCursor => {
            let cursor = printer.form_former.main_cursor();
            if let (Some(caret), true) = (cursor.caret, printer.is_active) {
                let _ = queue!(stdout, caret, SavePosition);
                return (true, None);
            } else {
                style_bef_cursor = Some(printer.form_former.make_form().style);
                let _ = queue!(stdout, SetStyle(cursor.form.style));
            }
        }
        Tag::ExtraCursor => {
            let _ = queue!(stdout, SetStyle(printer.form_former.extra_cursor().form.style));
        }
        Tag::HoverBound => todo!(),
        Tag::PermanentConceal { .. } => todo!()
    }

    (false, style_bef_cursor)
}

fn print_char(
    char: char, last_char: char, br: Coord, mut cursor: Coord, stdout: &mut StdoutLock,
    printer: &PrintOpts
) -> Coord {
    let char = mod_char(char, &printer.cfg, last_char);

    let len = len_of(char, &printer.cfg.tab_stops, cursor.x as usize + printer.info.x_shift());

    if cursor.x <= br.x - len {
        cursor.x += len;
        let _ = match char {
            '\t' => queue!(stdout, Print(" ".repeat(len as usize))),
            char => queue!(stdout, Print(char))
        };
    } else if cursor.x < br.x {
        let width = br.x - cursor.x;
        cursor.x += width;
        let _ = queue!(stdout, Print(" ".repeat(width as usize)));
    }

    cursor
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
