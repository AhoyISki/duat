use std::io::{stdout, StdoutLock};

use crossterm::{
    cursor::{self, MoveTo, SavePosition},
    queue,
    style::{ContentStyle, Print, ResetColor, SetStyle},
};
use parsec_core::{
    tags::{
        form::{FormFormer, FormPalette},
        Tag,
    },
    text::{NewLine, PrintCfg, PrintInfo, TextBit, TextIter, WrapMethod},
    ui::{self, Area as UiArea},
};
use ropey::RopeSlice;
use unicode_width::UnicodeWidthChar;

use crate::{Area, Coord, Coords};

pub struct Label {
    area: Area,
    is_active: bool,
}

impl Label {
    pub fn new(area: Area) -> Self {
        Label {
            area,
            is_active: false,
        }
    }
}

impl ui::Label<Area> for Label {
    fn print<CI, TI>(
        &mut self,
        mut text: TextIter<CI, TI>,
        print_info: PrintInfo,
        cfg: PrintCfg,
        palette: &FormPalette,
    ) where
        CI: Iterator<Item = char>,
        TI: Iterator<Item = (usize, Tag)>,
    {
        let mut stdout = stdout().lock();
        let _ = queue!(stdout, MoveTo(self.area.tl().x, self.area.tl().y), cursor::Hide);

        let mut cursor = self.area.tl();
        let coords = self.area.coords();
        let first_char = print_info.first_char();
        let x_shift = print_info.x_shift();

        let mut form_former = palette.form_former();
        let mut style_bef_cursor = None;
        let mut til_nl = false;
        let mut show_cursor = false;
        let mut indent = 0;
        let mut add_to_indent = true;
        let mut last_char = 'a';

        while let Some((index, text_bit)) = text.next() {
            if let TextBit::Char(char) = text_bit {
                if add_to_indent {
                    indent += match char {
                        ' ' => 1,
                        '\t' => tab_len(&cfg, cursor.x + x_shift as u16),
                        _ => {
                            add_to_indent = false;
                            0
                        }
                    }
                }

                if index < first_char || (til_nl && last_char != '\n') {
                    last_char = char;
                    continue;
                }

                print_char(char, last_char, x_shift, coords.br, &mut cursor, &mut stdout, &cfg);
                last_char = char;

                if let Some(style) = style_bef_cursor.take() {
                    let _ = queue!(stdout, ResetColor, SetStyle(style));
                }

                til_nl = if char == '\n' {
                    let style = form_former.make_form().style;
                    next_line(&mut cursor, coords, &mut stdout, style);
                    false
                } else {
                    maybe_wrap(coords, &form_former, indent, &mut cursor, &mut stdout, &cfg)
                };

                if cursor.y == coords.br.y {
                    break;
                }
            } else if let TextBit::Tag(tag) = text_bit {
                show_cursor |= trigger_tag(
                    tag,
                    self.is_active,
                    &mut style_bef_cursor,
                    &mut form_former,
                    &mut stdout,
                );
            }
        }

        while cursor.y < coords.br.y {
            next_line(&mut cursor, coords, &mut stdout, ContentStyle::default());
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

    fn wrap_count(
        &self,
        slice: RopeSlice,
        print_cfg: &PrintCfg
    ) -> usize {
        match print_cfg.wrap_method {
            WrapMethod::Width => self.get_width(slice, print_cfg) / self.area.width(),
            WrapMethod::Capped(_) => todo!(),
            WrapMethod::Word => todo!(),
            WrapMethod::NoWrap => 0,
        }
    }

    fn col_at_wrap(
        &self,
        slice: RopeSlice,
        wrap: usize,
        print_cfg: &PrintCfg
    ) -> usize {
        match print_cfg.wrap_method {
            WrapMethod::Width => {
                let dist = wrap * self.area.width();
                slice
                    .chars()
                    .enumerate()
                    .scan((0, false), |(width, end_reached), (index, ch)| {
                        *width += match ch {
                            '\t' => tab_len(print_cfg, *width as u16) as usize,
                            ch => UnicodeWidthChar::width(ch).unwrap_or(0),
                        };
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
            WrapMethod::NoWrap => 0,
        }
    }

    fn get_width(&self, slice: RopeSlice, print_cfg: &PrintCfg) -> usize {
        let mut width = 0;
        for ch in slice.chars() {
            width += match ch {
                '\t' => tab_len(print_cfg, width as u16) as usize,
                ch => UnicodeWidthChar::width(ch).unwrap_or(0),
            }
        }

        width
    }

    fn col_at_dist(&self, slice: RopeSlice, dist: usize, print_cfg: &PrintCfg) -> usize {
        slice
            .chars()
            .enumerate()
            .scan((0, false), |(width, end_reached), (index, ch)| {
                *width += match ch {
                    '\t' => tab_len(print_cfg, *width as u16) as usize,
                    ch => UnicodeWidthChar::width(ch).unwrap_or(0),
                };
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

fn trigger_tag(
    tag: Tag,
    is_active: bool,
    style_bef_cursor: &mut Option<ContentStyle>,
    form_former: &mut FormFormer,
    stdout: &mut StdoutLock,
) -> bool {
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
                return true;
            } else {
                *style_bef_cursor = Some(form_former.make_form().style);
                let _ = queue!(stdout, SetStyle(cursor.form.style));
            }
        }
        Tag::ExtraCursor => {
            let _ = queue!(stdout, SetStyle(form_former.extra_cursor().form.style));
        }
        Tag::HoverBound => todo!(),
        Tag::PermanentConceal { .. } => todo!(),
    }

    false
}

fn print_char(
    char: char,
    last_char: char,
    x_shift: usize,
    br: Coord,
    cursor: &mut Coord,
    stdout: &mut StdoutLock,
    print_cfg: &PrintCfg,
) {
    let char = mod_char(char, print_cfg, last_char);

    let len = match char {
        '\t' => tab_len(print_cfg, cursor.x + x_shift as u16),
        _ => UnicodeWidthChar::width(char).unwrap_or(0) as u16,
    };

    if cursor.x <= br.x - len {
        cursor.x += len;
        let _ = match char {
            '\t' => queue!(stdout, Print(" ".repeat(len as usize))),
            char => queue!(stdout, Print(char)),
        };
    } else if cursor.x < br.x {
        let width = br.x - cursor.x;
        cursor.x += width;
        let _ = queue!(stdout, Print(" ".repeat(width as usize)));
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
        char => char,
    };
    char
}

fn maybe_wrap(
    coords: Coords,
    form_former: &FormFormer,
    indent: u16,
    cursor: &mut Coord,
    stdout: &mut StdoutLock,
    print_cfg: &PrintCfg,
) -> bool {
    if cursor.x == coords.br.x {
        let style = form_former.make_form().style;
        next_line(cursor, coords, stdout, style);
        match print_cfg.wrap_method {
            WrapMethod::Width => {
                if print_cfg.wrap_indent {
                    cursor.x += indent;
                }
            }
            WrapMethod::NoWrap => return true,
            WrapMethod::Capped(_) => todo!(),
            WrapMethod::Word => todo!(),
        }
    }

    false
}

fn next_line(cursor: &mut Coord, coords: Coords, stdout: &mut StdoutLock, style: ContentStyle) {
    let _ = queue!(stdout, ResetColor);

    if cursor.x < coords.br.x {
        // The rest of the line is featureless.
        let padding_count = (coords.br.x - cursor.x) as usize;
        queue!(stdout, Print(" ".repeat(padding_count))).unwrap();
    }

    cursor.y += 1;
    cursor.x = coords.tl.x;

    let _ = queue!(stdout, ResetColor, MoveTo(cursor.x, cursor.y), SetStyle(style));
}

fn tab_len(print_cfg: &PrintCfg, x: u16) -> u16 {
    print_cfg.tab_stop as u16 - x % print_cfg.tab_stop as u16
}

unsafe impl Send for Label {}
unsafe impl Sync for Label {}
