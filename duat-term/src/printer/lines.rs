use std::fmt::Alignment;

use crossterm::style::{Attribute, ContentStyle};
use duat_core::{cfg::PrintCfg, form::Painter};

use super::Lines;
use crate::{CStyle, Coords, area::Coord, print_style};

pub struct LinesBuilder {
    lines: Lines,

    // Temporary things
    cur_line: usize,
    line: Vec<u8>,
    len: u32,
    positions: Vec<(usize, u32)>,
    gaps: Gaps,
    default_gaps: Gaps,

    // Outside information
    shift: u32,
    cap: u32,
    rightmost: bool,
}

impl LinesBuilder {
    pub fn new(coords: Coords, max: Coord, shift: u32, cfg: PrintCfg) -> Self {
        let cap = cfg.wrap_width(coords.width());

        LinesBuilder {
            lines: Lines {
                list: (0..coords.height() + 1)
                    .map(|_| Vec::with_capacity(coords.width() as usize * 4))
                    .collect(),
                coords,
                real_cursor: None,
            },

            cur_line: 0,
            line: Vec::with_capacity(coords.width() as usize * 4),
            len: 0,
            positions: Vec::new(),
            gaps: Gaps::OnRight,
            default_gaps: Gaps::OnRight,
            rightmost: coords.br.x == max.x,

            shift,
            cap,
        }
    }

    pub(super) fn build(self) -> Lines {
        self.lines
    }

    pub fn push_char(&mut self, char: char, len: u32) {
        self.len += len;
        let mut bytes = [0; 4];
        char.encode_utf8(&mut bytes);
        if self.lines.coords.width() < self.cap {
            self.positions.push((self.line.len(), len));
        }
        self.line.extend(&bytes[..char.len_utf8()]);
    }

    pub fn realign(&mut self, alignment: Alignment) {
        self.default_gaps = match alignment {
            Alignment::Left => Gaps::OnRight,
            Alignment::Right => Gaps::OnLeft,
            Alignment::Center => Gaps::OnSides,
        };
        if let Gaps::OnRight | Gaps::OnLeft | Gaps::OnSides = self.gaps {
            self.gaps = self.default_gaps.clone();
        }
    }

    pub fn add_spacer(&mut self) {
        self.gaps.add_spacer(self.line.len());
    }

    pub fn show_real_cursor(&mut self) {
        self.lines.real_cursor = Some(true);
    }

    pub fn hide_real_cursor(&mut self) {
        self.lines.real_cursor = Some(false);
    }

    pub fn end_line(
        &mut self,
        ansi_codes: &mut micromap::Map<CStyle, String, 16>,
        painter: &Painter,
    ) {
        fn starting_spaces(bytes: &mut Vec<u8>, len: usize) {
            if len > 0 {
                bytes.extend_from_slice(&BLANK[..len]);
            }
        }

        fn ending_spaces(
            bytes: &mut Vec<u8>,
            len: usize,
            style: ContentStyle,
            ansi_codes: &mut micromap::Map<CStyle, String, 16>,
            is_rightmost: bool,
        ) {
            print_style(bytes, style, ansi_codes);
            if is_rightmost {
                bytes.extend_from_slice(b"\x1b[0K");
            } else {
                bytes.extend_from_slice(&BLANK[..len])
            }
        }

        const BLANK: [u8; 1000] = [b' '; 1000];
        let default = {
            let mut default_form = painter.get_default();
            default_form.style.attributes.set(Attribute::Reset);
            default_form.style
        };

        let line = &mut self.lines.list[self.cur_line];

        let spaces = self.gaps.get_spaces(self.cap - self.len);
        // Shortcut
        if self.lines.coords.width() >= self.cap {
            print_style(line, default, ansi_codes);

            let start_d = match &self.gaps {
                Gaps::OnRight => 0,
                Gaps::OnLeft => self.cap - self.len,
                Gaps::OnSides => (self.cap - self.len) / 2,
                Gaps::Spacers(bytes) => {
                    let spacers = bytes.iter().zip(spaces);
                    let mut start = 0;

                    for (&end, len) in spacers {
                        line.extend_from_slice(&self.line[start..end]);
                        line.extend(&BLANK[..len as usize]);
                        start = end
                    }
                    line.extend(&self.line[start..self.line.len()]);

                    self.go_to_next_line();
                    return;
                }
            };

            starting_spaces(line, start_d as usize);
            line.extend_from_slice(&self.line);
            let end_d = start_d + self.len;
            if self.lines.coords.width() > end_d {
                let len = (self.lines.coords.width() - end_d) as usize;
                ending_spaces(line, len, default, ansi_codes, self.rightmost);
            }
            self.go_to_next_line();
            return;
        }

        let (start_i, start_d) = {
            let mut dist = match &self.gaps {
                Gaps::OnRight | Gaps::Spacers(_) => 0,
                Gaps::OnLeft => self.cap - self.len,
                Gaps::OnSides => (self.cap - self.len) / 2,
            };

            // Using a different loop when there are no spacers in order to
            // improve efficiency.
            let found_start = if let Gaps::Spacers(bytes) = &self.gaps {
                let mut sb = spaces.iter().zip(bytes).peekable();
                self.positions.iter().find(|(b, len)| {
                    dist += len;
                    if let Some((space, _)) = sb.next_if(|(_, lhs)| *lhs <= b) {
                        dist += space;
                    }
                    dist > self.shift
                })
            } else {
                self.positions.iter().find(|(_, len)| {
                    dist += len;
                    dist > self.shift
                })
            };

            // Situation where the line is empty, from having no characters or
            // from being shifted out of sight.
            let Some(&(start, len)) = found_start else {
                let len = self.lines.coords.width() as usize;
                ending_spaces(line, len, default, ansi_codes, self.rightmost);

                self.go_to_next_line();
                return;
            };

            // If the character is cut by the start, don't print it.
            if dist - len < self.shift && len <= 2 {
                let str = unsafe { std::str::from_utf8_unchecked(&self.line[start..]) };
                let char = str.chars().next().unwrap();
                (start + char.len_utf8(), dist - self.shift)
            } else {
                (start, dist - len - self.shift)
            }
        };

        let (end_i, end_d) = {
            let mut dist = match &self.gaps {
                Gaps::OnRight => self.len,
                Gaps::OnLeft | Gaps::Spacers(_) => self.cap,
                Gaps::OnSides => self.len + (self.cap - self.len) / 2,
            };
            let found_end = if let Gaps::Spacers(bytes) = &self.gaps {
                let mut sb = spaces.iter().zip(bytes).rev().peekable();
                self.positions.iter().rev().find(|(b, len)| {
                    dist -= len;
                    if let Some((space, _)) = sb.next_if(|(_, lhs)| *lhs > b) {
                        dist -= space;
                    }
                    dist < self.shift + self.lines.coords.width()
                })
            } else {
                self.positions.iter().rev().find(|(_, len)| {
                    dist -= len;
                    dist < self.shift + self.lines.coords.width()
                })
            };

            // Due to spacers in the middle of the line, end_i might actually be
            // smaller than start_i.
            let Some(&(end, len)) = found_end.filter(|(end_i, _)| *end_i >= start_i) else {
                print_style(line, default, ansi_codes);
                line.extend_from_slice(&BLANK[..self.lines.coords.width() as usize]);

                self.go_to_next_line();
                return;
            };
            // If the character is cut by the end, don't print it.
            if dist + len > self.shift + self.lines.coords.width() {
                (end, dist - self.shift)
            } else {
                let str = unsafe { std::str::from_utf8_unchecked(&self.line[end..]) };
                let char = str.chars().next().unwrap();
                (end + char.len_utf8(), dist + len - self.shift)
            }
        };

        print_style(line, default, ansi_codes);
        starting_spaces(line, start_d as usize);

        self.add_ansi(start_i);

        let line = &mut self.lines.list[self.cur_line];

        if let Gaps::Spacers(bytes) = &mut self.gaps {
            let spacers = bytes
                .iter()
                .zip(spaces)
                .skip_while(|(b, _)| **b <= start_i)
                .take_while(|(b, _)| **b < end_i);

            let mut start = start_i;

            for (&end, len) in spacers {
                line.extend_from_slice(&self.line[start..end]);
                line.extend_from_slice(&BLANK[..len as usize]);
                start = end;
            }

            line.extend_from_slice(&self.line[start..end_i]);
        } else {
            line.extend_from_slice(&self.line[start_i..end_i]);
        }

        if self.lines.coords.width() > end_d {
            let len = (self.lines.coords.width() - end_d) as usize;
            ending_spaces(line, len, default, ansi_codes, self.rightmost);
        }

        self.go_to_next_line();
    }

    pub fn coords(&self) -> Coords {
        self.lines.coords
    }

    pub fn cap(&self) -> u32 {
        self.cap
    }

    fn go_to_next_line(&mut self) {
        self.cur_line += 1;
        self.line.clear();
        self.positions.clear();
        self.gaps = self.default_gaps.clone();
        self.len = 0;
    }

    fn add_ansi(&mut self, start_i: usize) {
        let mut adding_ansi = false;
        let line = &mut self.lines.list[self.cur_line];

        for &b in &self.line[..start_i] {
            if b == 0x1b {
                adding_ansi = true;
                line.push(0x1b)
            } else if b == b'm' && adding_ansi {
                adding_ansi = false;
                line.push(b'm')
            } else if adding_ansi {
                line.push(b)
            }
        }
    }
}

impl std::io::Write for LinesBuilder {
    /// For writing *ONLY* crossterm commands
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.line.extend(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Gaps {
    OnRight,
    OnLeft,
    OnSides,
    Spacers(Vec<usize>),
}

impl Gaps {
    /// Adds a [`Spacer`] to this [`Gaps`]
    ///
    /// [`Spacer`]: duat_core::text::Tag::Spacer
    pub fn add_spacer(&mut self, byte: usize) {
        match self {
            Gaps::OnRight | Gaps::OnLeft | Gaps::OnSides => *self = Gaps::Spacers(vec![byte]),
            Gaps::Spacers(items) => items.push(byte),
        };
    }

    pub fn get_spaces(&self, space: u32) -> Vec<u32> {
        let Self::Spacers(bytes) = self else {
            return Vec::new();
        };
        let mut enough_space = space;
        while !enough_space.is_multiple_of(bytes.len() as u32) {
            enough_space += 1;
        }
        let diff = enough_space - space;
        (0..bytes.len() as u32)
            .map(|i| (enough_space / bytes.len() as u32) - (i < diff) as u32)
            .collect()
    }
}
