use std::ops::{
    Range, RangeBounds, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive,
};

use super::File;
use crate::{
    history::Change,
    text::{Iter, Part, Point, RevIter},
};

pub trait Reader {
    fn read_change(&mut self, file: &mut File, change: Change);

    fn read_click(&mut self, file: &mut File, point: Point);
}

pub struct Searcher<'a> {
    point: Point,
    iter: Iter<'a>,
}

impl<'a> Searcher<'a> {
    pub fn new_at(point: Point, iter: Iter<'a>) -> Self {
        Self { point, iter }
    }

    pub fn find(&mut self, pat: impl Pattern) -> Option<(Point, Point)> {
        let mut i = 0;
        let mut end = self.point;

        while let Some(item) = self.iter.next() {
            if i == pat.length() {
                return Some((self.point, end));
            }

            end = item.real;

            if pat.matches(item.part, i) {
                i += 1;
            } else {
                i = 0;
                self.point = item.real;
            }
        }

        (i == pat.length()).then(|| (self.point, end))
    }
}

pub struct RevSearcher<'a> {
    point: Point,
    iter: RevIter<'a>,
}

impl<'a> RevSearcher<'a> {
    pub fn new_at(point: Point, iter: RevIter<'a>) -> Self {
        Self { point, iter }
    }

    pub fn find(&mut self, pat: impl Pattern) -> Option<(Point, Point)> {
        let mut i = pat.length();
        let mut start = self.point;

        while let Some(item) = self.iter.next()
            && i > 0
        {
            start = item.real;

            if pat.matches(item.part, i - 1) {
                i -= 1;
            } else {
                i = pat.length();
                self.point = item.real;
            }
        }

        (i == 0).then(|| (self.point, start))
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
        let cmp = part.as_char();
        let byte = self.chars().nth(index);

        byte.zip(cmp).is_some_and(|(c, cmp)| c == cmp)
    }
}

impl Pattern for char {
    fn length(&self) -> usize {
        1
    }

    fn matches(&self, part: Part, _index: usize) -> bool {
        part.as_char().is_some_and(|cmp| *self == cmp as char)
    }
}

impl<const N: usize> Pattern for [char; N] {
    fn length(&self) -> usize {
        1
    }

    fn matches(&self, part: Part, _index: usize) -> bool {
        part.as_char()
            .is_some_and(|cmp| self.contains(&(cmp as char)))
    }
}

impl Pattern for &[char] {
    fn length(&self) -> usize {
        1
    }

    fn matches(&self, part: Part, _index: usize) -> bool {
        part.as_char()
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
                part.as_char().is_some_and(|cmp| self.contains(&(cmp as char)))
            }
        }
    )+

    $(
        impl<const N: usize> Pattern for [$r; N] {
            fn length(&self) -> usize {
                1
            }

            fn matches(&self, part: Part, _index: usize) -> bool {
                part.as_char()
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
                part.as_char()
                    .is_some_and(|cmp| self.iter().any(|range| range.contains(&(cmp as char))))
            }
        }
    )+
}
