use std::ops::RangeBounds;

use ropey::{Rope, RopeSlice};

#[derive(Debug)]
pub enum InnerText {
    String(String),
    Rope(Rope),
}

impl InnerText {
    pub fn replace(&mut self, range: impl RangeBounds<usize> + Clone, edit: impl AsRef<str>) {
        match self {
            InnerText::String(string) => {
                let (start, end) = get_ends(range, string.chars().count());
                let start = string.chars().take(start).map(|ch| ch.len_utf8()).sum::<usize>();
                let end = string.chars().take(end).map(|ch| ch.len_utf8()).sum::<usize>();
                string.replace_range(start..end, edit.as_ref())
            }
            InnerText::Rope(rope) => {
                rope.remove(range.clone());
                let (start, _) = get_ends(range, rope.len_chars());
                rope.insert(start, edit.as_ref());
            }
        }
    }

    pub fn chars_at(&self, ch_index: usize) -> Box<dyn Iterator<Item = char> + '_> {
        match self {
            InnerText::String(string) => Box::new(string.chars().skip(ch_index)),
            InnerText::Rope(rope) => Box::new(rope.chars_at(ch_index)),
        }
    }

    pub fn char_from_line_start(&self, ch_index: usize) -> usize {
        match self {
            InnerText::String(string) => {
                let line_ch = string
                    .lines()
                    .scan(0, |accum, line| {
                        *accum = *accum + line.chars().count();
                        if *accum <= ch_index {
                            Some(*accum)
                        } else {
                            None
                        }
                    })
                    .last()
                    .unwrap();
                ch_index - line_ch
            }
            InnerText::Rope(rope) => {
                let line = rope.char_to_line(ch_index);
                ch_index - rope.line_to_char(line)
            }
        }
    }

    pub fn char_to_byte(&self, ch_index: usize) -> usize {
        let ch = match self {
            InnerText::String(string) => {
                string.char_indices().nth(ch_index).map(|(index, _)| index)
            }
            InnerText::Rope(rope) => rope.try_char_to_byte(ch_index).ok(),
        };
        assert!(ch.is_some(), "Char index {} not found", ch_index);
        ch.unwrap()
    }

    pub fn char_to_line(&self, ch_index: usize) -> usize {
        let line = match self {
            InnerText::String(string) => string
                .lines()
                .enumerate()
                .scan(0, |accum, (index, line)| {
                    if *accum <= ch_index {
                        *accum = *accum + line.chars().count();
                        Some(index)
                    } else {
                        None
                    }
                })
                .last(),
            InnerText::Rope(rope) => rope.try_char_to_line(ch_index).ok(),
        };
        assert!(line.is_some(), "Char index {} not found", ch_index);
        line.unwrap()
    }

    pub fn line(&self, line_index: usize) -> ropey::RopeSlice {
        let line = match self {
            InnerText::String(string) => {
                string.lines().nth(line_index).map(|line| RopeSlice::from(line))
            }
            InnerText::Rope(rope) => rope.get_line(line_index),
        };
        assert!(line.is_some(), "Line index {} not found", line_index);
        line.unwrap()
    }

    pub fn line_to_char(&self, line_index: usize) -> usize {
        match self {
            InnerText::String(string) => {
                string.lines().take(line_index).map(|line| line.chars().count()).sum()
            }
            InnerText::Rope(rope) => rope.line_to_char(line_index),
        }
    }

    pub fn slice(&self, range: impl RangeBounds<usize> + Clone) -> RopeSlice {
        let (start, end) = get_ends(range.clone(), self.len_chars());
        assert!(end <= self.len_chars(), "Range end at {} is out of bounds.", end);
        match self {
            InnerText::String(string) => {
                let mut chars = string.char_indices().map(|(index, _)| index);
                let start = chars.nth(start).unwrap();
                let mut chars = string.char_indices().map(|(index, _)| index);
                let end = chars.nth(start).unwrap();
                RopeSlice::from(&string[start..end])
            }
            InnerText::Rope(rope) => rope.slice(range),
        }
    }

    pub fn len_bytes(&self) -> usize {
        match self {
            InnerText::String(string) => string.len(),
            InnerText::Rope(rope) => rope.len_bytes(),
        }
    }

    pub fn len_chars(&self) -> usize {
        match self {
            InnerText::String(string) => string.chars().count(),
            InnerText::Rope(rope) => rope.len_chars(),
        }
    }

    pub fn len_lines(&self) -> usize {
        match self {
            InnerText::String(string) => string.lines().count(),
            InnerText::Rope(rope) => rope.len_lines(),
        }
    }

    pub fn clear(&mut self) {
        match self {
            InnerText::String(string) => string.clear(),
            InnerText::Rope(rope) => *rope = Rope::default(),
        }
    }

    pub fn string(&mut self) -> &mut String {
        match self {
            InnerText::String(string) => string,
            InnerText::Rope(_) => {
                panic!(
                    "Use of string() in a place where `InnerText` is not guaranteed to be \
                     `String`."
                )
            }
        }
    }
}

pub fn get_ends(range: impl std::ops::RangeBounds<usize>, max: usize) -> (usize, usize) {
    let start = match range.start_bound() {
        std::ops::Bound::Included(start) => *start,
        std::ops::Bound::Excluded(start) => *start + 1,
        std::ops::Bound::Unbounded => 0,
    };
    let end = match range.end_bound() {
        std::ops::Bound::Included(end) => *end + 1,
        std::ops::Bound::Excluded(end) => *end,
        std::ops::Bound::Unbounded => max,
    };

    (start, end)
}
