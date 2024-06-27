use std::ops::{
    Range, RangeBounds, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive,
};

use super::File;
use crate::{
    history::Change,
    text::{ExactPos, Iter, Part, RevIter},
};

pub trait Reader {
    fn read_change(&mut self, file: &mut File, change: Change);

    fn read_click(&mut self, file: &mut File, pos: ExactPos);
}

pub struct Searcher<'a> {
    pos: ExactPos,
    iter: Iter<'a>,
}

impl<'a> Searcher<'a> {
    pub fn new_at(pos: ExactPos, iter: Iter<'a>) -> Self {
        Self { pos, iter }
    }

    pub fn find(&mut self, pat: impl Pattern) -> Option<(ExactPos, ExactPos)> {
        let mut index = 0;
        let mut end_pos = self.pos;

        while let Some(item) = self.iter.next() {
            if index == pat.length() {
                return Some((self.pos, end_pos));
            }

            end_pos = item.pos;

            if pat.matches(item.part, index) {
                index += 1;
            } else {
                index = 0;
                self.pos = item.pos;
            }
        }

        match index == pat.length() {
            true => Some(if pat.length() == 0 {
                (self.pos, self.pos)
            } else {
                let end_pos = if self.iter.on_ghost() {
                    ExactPos::new(end_pos.real(), end_pos.ghost() + 1)
                } else {
                    ExactPos::new(end_pos.real() + 1, 0)
                };

                (self.pos, end_pos)
            }),
            false => None,
        }
    }
}

pub struct RevSearcher<'a> {
    pos: ExactPos,
    iter: RevIter<'a>,
}

impl<'a> RevSearcher<'a> {
    pub fn new_at(pos: ExactPos, iter: RevIter<'a>) -> Self {
        Self { pos, iter }
    }

    pub fn find(&mut self, pat: impl Pattern) -> Option<(ExactPos, ExactPos)> {
        let mut index = pat.length().saturating_sub(1);
        let mut start_pos = self.pos;

        while let Some(item) = self.iter.next() {
            if index == 0 {
                return Some((start_pos, self.pos));
            }

            start_pos = item.pos;

            if pat.matches(item.part, index) {
                index -= 1;
            } else {
                index = pat.length().saturating_sub(1);
                self.pos = item.pos;
            }
        }

        match index == 0 {
            true => Some(if pat.length() == 0 {
                (self.pos, self.pos)
            } else {
                let start_pos = if self.iter.on_ghost() {
                    ExactPos::new(start_pos.real(), start_pos.ghost() - 1)
                } else {
                    ExactPos::new(start_pos.real().saturating_sub(1), 0)
                };

                (start_pos, self.pos)
            }),
            false => None,
        }
    }
}

trait Pattern {
    fn length(&self) -> usize;

    fn matches(&self, part: Part, index: usize) -> bool;
}

impl Pattern for Part {
    fn length(&self) -> usize {
        1
    }

    fn matches(&self, part: Part, _index: usize) -> bool {
        *self == part
    }
}

impl<const N: usize> Pattern for [Part; N] {
    fn length(&self) -> usize {
        N
    }

    fn matches(&self, part: Part, index: usize) -> bool {
        self[index] == part
    }
}

impl Pattern for &[Part] {
    fn length(&self) -> usize {
        self.len()
    }

    fn matches(&self, part: Part, index: usize) -> bool {
        self[index] == part
    }
}

impl Pattern for &str {
    fn length(&self) -> usize {
        self.len()
    }

    fn matches(&self, part: Part, index: usize) -> bool {
        let cmp = part.as_byte();
        let byte = self.bytes().nth(index);

        byte.zip(cmp).is_some_and(|(byte, cmp)| byte == cmp)
    }
}

impl Pattern for char {
    fn length(&self) -> usize {
        1
    }

    fn matches(&self, part: Part, _index: usize) -> bool {
        part.as_byte().is_some_and(|cmp| *self == cmp as char)
    }
}

impl<const N: usize> Pattern for [char; N] {
    fn length(&self) -> usize {
        1
    }

    fn matches(&self, part: Part, _index: usize) -> bool {
        part.as_byte()
            .is_some_and(|cmp| self.contains(&(cmp as char)))
    }
}

impl Pattern for &[char] {
    fn length(&self) -> usize {
        1
    }

    fn matches(&self, part: Part, _index: usize) -> bool {
        part.as_byte()
            .is_some_and(|cmp| self.contains(&(cmp as char)))
    }
}

impl_ranges!(
    Range<char>,
    RangeTo<char>,
    RangeFrom<char>,
    RangeInclusive<char>,
    RangeToInclusive<char>,
    RangeFull
);

macro impl_ranges($($r:ty),+) {
    $(
        impl Pattern for $r {
            fn length(&self) -> usize {
                1
            }

            fn matches(&self, part: Part, _index: usize) -> bool {
                part.as_byte().is_some_and(|cmp| self.contains(&(cmp as char)))
            }
        }
    )+

    $(
        impl<const N: usize> Pattern for [$r; N] {
            fn length(&self) -> usize {
                1
            }

            fn matches(&self, part: Part, _index: usize) -> bool {
                part.as_byte()
                    .is_some_and(|cmp| self.iter().any(|range| range.contains(&(cmp as char))))
            }
        }
    )+

    $(
        impl Pattern for &[$r] {
            fn length(&self) -> usize {
                1
            }

            fn matches(&self, part: Part, _index: usize) -> bool {
                part.as_byte()
                    .is_some_and(|cmp| self.iter().any(|range| range.contains(&(cmp as char))))
            }
        }
    )+
}
