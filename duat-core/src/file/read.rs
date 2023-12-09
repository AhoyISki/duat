use std::ops::{
    OneSidedRange, Range, RangeBounds, RangeFrom, RangeFull, RangeInclusive, RangeTo,
    RangeToInclusive,
};

use super::File;
use crate::{
    history::Change,
    text::{ExactPos, Iter, Part},
};

pub trait Reader {
    fn read_change(&mut self, file: &mut File, change: Change);

    fn read_click(&mut self, file: &mut File, pos: ExactPos);
}

pub struct Searcher<'a> {
    iter: Iter<'a>,
}

impl Searcher<'_> {
    pub fn find(&mut self, pat: impl Pattern) -> Option<ExactPos> {}
}

trait Pattern {
    fn matches(&self, slice: &[Part]) -> bool;
}

impl Pattern for Part {
    fn matches(&self, slice: &[Part]) -> bool {
        slice[0] == *self
    }
}

impl<const N: usize> Pattern for [Part; N] {
    fn matches(&self, slice: &[Part]) -> bool {
        self == slice
    }
}

impl Pattern for &[Part] {
    fn matches(&self, slice: &[Part]) -> bool {
        *self == slice
    }
}

impl Pattern for &str {
    fn matches(&self, slice: &[Part]) -> bool {
        let mut parts = slice.iter();

        self.chars()
            .zip(parts)
            .all(|(char, part)| part.as_char().is_some_and(|cmp| char == cmp))
    }
}

impl Pattern for char {
    fn matches(&self, slice: &[Part]) -> bool {
        slice[0].as_char().is_some_and(|cmp| *self == cmp)
    }
}

impl<const N: usize> Pattern for [char; N] {
    fn matches(&self, slice: &[Part]) -> bool {
        slice[0].as_char().is_some_and(|cmp| self.contains(&cmp))
    }
}

impl Pattern for &[char] {
    fn matches(&self, slice: &[Part]) -> bool {
        slice[0].as_char().is_some_and(|cmp| self.contains(&cmp))
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
            fn matches(&self, slice: &[Part]) -> bool {
                slice[0].as_char().is_some_and(|cmp| self.contains(&cmp))
            }
        }
    )+

    $(
        impl<const N: usize> Pattern for [$r; N] {
            fn matches(&self, slice: &[Part]) -> bool {
                slice[0].as_char().is_some_and(|cmp| {
                    self.iter().any(|range| range.contains(&cmp))
                })
            }
        }
    )+

    $(
        impl Pattern for &[$r] {
            fn matches(&self, slice: &[Part]) -> bool {
                slice[0].as_char().is_some_and(|cmp| {
                    self.iter().any(|range| range.contains(&cmp))
                })
            }
        }
    )+
}
