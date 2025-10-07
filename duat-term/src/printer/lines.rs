use std::fmt::Alignment;

use crossterm::style::{Attribute, Attributes};
use duat_core::{cfg::PrintCfg, form::Painter};

use super::Lines;
use crate::{CStyle, Coords, print_style, printer::InnerLineInfo};

pub struct LinesBuilder {
    lines: Lines,

    // Temporary things
    line: Vec<u8>,
    len: u32,
    positions: Vec<(usize, u32)>,
    gaps: Gaps,
    default_gaps: Gaps,

    // Outside information
    shift: u32,
    cap: u32,
}

impl LinesBuilder {
    pub fn new(coords: Coords, shift: u32, cfg: PrintCfg) -> Self {
        let cap = cfg.wrap_width(coords.width());

        LinesBuilder {
            lines: Lines {
                bytes: Vec::with_capacity(2 * (coords.width() * coords.height()) as usize),
                line_infos: Vec::with_capacity(coords.height() as usize),
                coords,
                real_cursor: None,
            },

            line: Vec::with_capacity(coords.width() as usize * 4),
            len: 0,
            positions: Vec::new(),
            gaps: Gaps::OnRight,
            default_gaps: Gaps::OnRight,

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
        painter: &mut Painter,
    ) {
        const SPACES: &[u8] = &[b' '; 1000];

        fn end_fmt(
            bytes: &mut Vec<u8>,
            painter: &mut Painter,
            ansi_codes: &mut micromap::Map<CStyle, String, 16>,
        ) -> usize {
            let mut default_style = painter.get_default();
            default_style.style.foreground_color = None;
            default_style.style.underline_color = None;
            default_style.style.attributes = Attributes::from(Attribute::Reset);

            let end_fmt_i = bytes.len();
            print_style(bytes, default_style.style, ansi_codes);
            end_fmt_i
        }

        let bytes = &mut self.lines.bytes;

        let spaces = self.gaps.get_spaces(self.cap - self.len);
        let offset = bytes.len();

        // Shortcut
        if self.lines.coords.width() >= self.cap {
            let start_d = match &self.gaps {
                Gaps::OnRight => 0,
                Gaps::OnLeft => self.cap - self.len,
                Gaps::OnSides => (self.cap - self.len) / 2,
                Gaps::Spacers(indices) => {
                    let spacers = indices.iter().zip(spaces);
                    let mut start = 0;

                    for (&end, len) in spacers {
                        bytes.extend_from_slice(&self.line[start..end]);
                        bytes.extend(&SPACES[..len as usize]);
                        start = end
                    }
                    bytes.extend(&self.line[start..self.line.len()]);

                    let end_fmt_i = end_fmt(bytes, painter, ansi_codes);
                    self.go_to_next_line(offset, end_fmt_i, 0);
                    return;
                }
            };

            bytes.extend_from_slice(&SPACES[..start_d as usize]);
            bytes.extend_from_slice(&self.line);
            let end_spaces = self.lines.coords.width().saturating_sub(start_d + self.len);
            let end_fmt_i = end_fmt(bytes, painter, ansi_codes);
            self.go_to_next_line(offset, end_fmt_i, end_spaces);
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
            let found_start = if let Gaps::Spacers(indices) = &self.gaps {
                let mut sb = spaces.iter().zip(indices).peekable();
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
                let end_spaces = self.lines.coords.width();
                let end_fmt_i = end_fmt(bytes, painter, ansi_codes);
                self.go_to_next_line(offset, end_fmt_i, end_spaces);
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
            let found_end = if let Gaps::Spacers(indices) = &self.gaps {
                let mut sb = spaces.iter().zip(indices).rev().peekable();
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
                let end_spaces = self.lines.coords.width();
                let end_fmt_i = end_fmt(bytes, painter, ansi_codes);
                self.go_to_next_line(offset, end_fmt_i, end_spaces);
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

        bytes.extend_from_slice(&SPACES[..start_d as usize]);
        self.add_ansi(start_i);

        let bytes = &mut self.lines.bytes;

        if let Gaps::Spacers(indices) = &mut self.gaps {
            let spacers = indices
                .iter()
                .zip(spaces)
                .skip_while(|(b, _)| **b <= start_i)
                .take_while(|(b, _)| **b < end_i);

            let mut start = start_i;

            for (&end, len) in spacers {
                bytes.extend_from_slice(&self.line[start..end]);
                bytes.extend_from_slice(&SPACES[..len as usize]);
                start = end;
            }

            bytes.extend_from_slice(&self.line[start..end_i]);
        } else {
            bytes.extend_from_slice(&self.line[start_i..end_i]);
        }

        let end_spaces = self.lines.coords.width().saturating_sub(end_d);
        let end_fmt_i = end_fmt(bytes, painter, ansi_codes);
        self.go_to_next_line(offset, end_fmt_i, end_spaces);
    }

    pub fn coords(&self) -> Coords {
        self.lines.coords
    }

    pub fn cap(&self) -> u32 {
        self.cap
    }

    fn go_to_next_line(&mut self, offset: usize, end_fmt_i: usize, end_spaces: u32) {
        self.lines.line_infos.push(InnerLineInfo {
            offset,
            end_fmt_i: end_fmt_i - offset,
            end_spaces: end_spaces as usize,
        });

        self.line.clear();
        self.positions.clear();
        self.gaps = self.default_gaps.clone();
        self.len = 0;
    }

    fn add_ansi(&mut self, start_i: usize) {
        let mut adding_ansi = false;
        let bytes = &mut self.lines.bytes;

        for &b in &self.line[..start_i] {
            if b == 0x1b {
                adding_ansi = true;
                bytes.push(0x1b)
            } else if b == b'm' && adding_ansi {
                adding_ansi = false;
                bytes.push(b'm')
            } else if adding_ansi {
                bytes.push(b)
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
