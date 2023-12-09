use std::ops::{
    Range, RangeBounds, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive,
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
    pub fn find(&mut self, pat: impl Pattern) -> Option<(ExactPos, ExactPos)> {
    }
}

trait Pattern {
    fn len(&self) -> usize;

    fn matches(&self, part: Part, index: usize) -> MatchState;
}

impl Pattern for Part {
    fn len(&self) -> usize {
        1
    }

    fn matches(&self, part: Part, _index: usize) -> MatchState {
        if *self == part {
            MatchState::Finished
        } else {
            MatchState::Break
        }
    }
}

impl<const N: usize> Pattern for [Part; N] {
    fn len(&self) -> usize {
        N
    }

    fn matches(&self, part: Part, index: usize) -> MatchState {
        if N == 0 {
            MatchState::Finished
        } else if self[index] == part {
            match N == index + 1 {
                true => MatchState::Finished,
                false => MatchState::Continue,
            }
        } else {
            MatchState::Break
        }
    }
}

impl Pattern for &[Part] {
    fn len(&self) -> usize {
        self.len()
    }

    fn matches(&self, part: Part, index: usize) -> MatchState {
        if self.is_empty() {
            MatchState::Finished
        } else if self[index] == part {
            match self.len() == index + 1 {
                true => MatchState::Finished,
                false => MatchState::Continue,
            }
        } else {
            MatchState::Break
        }
    }
}

impl Pattern for &str {
    fn len(&self) -> usize {
        self.len()
    }

    fn matches(&self, part: Part, index: usize) -> MatchState {
        let cmp = part.as_char();
        let char = self.chars().nth(index);

        if char.zip(cmp).is_some_and(|(char, cmp)| char == cmp) {
            match self.len() == index + 1 {
                true => MatchState::Finished,
                false => MatchState::Continue,
            }
        } else {
            MatchState::Break
        }
    }
}

impl Pattern for char {
    fn len(&self) -> usize {
        1
    }

    fn matches(&self, part: Part, index: usize) -> MatchState {
        if part.as_char().is_some_and(|cmp| *self == cmp) {
            MatchState::Finished
        } else {
            MatchState::Break
        }
    }
}

impl<const N: usize> Pattern for [char; N] {
    fn len(&self) -> usize {
        1
    }

    fn matches(&self, part: Part, index: usize) -> MatchState {
        if part.as_char().is_some_and(|cmp| self.contains(&cmp)) {
            MatchState::Finished
        } else {
            MatchState::Break
        }
    }
}

impl Pattern for &[char] {
    fn len(&self) -> usize {
        1
    }

    fn matches(&self, part: Part, index: usize) -> MatchState {
        if part.as_char().is_some_and(|cmp| self.contains(&cmp)) {
            MatchState::Finished
        } else {
            MatchState::Break
        }
    }
}

enum MatchState {
    Break,
    Continue,
    Finished,
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
            fn len(&self) -> usize {
                1
            }

            fn matches(&self, part: Part, index: usize) -> MatchState {
                if part.as_char().is_some_and(|cmp| self.contains(&cmp)) {
                    MatchState::Finished
                } else {
                    MatchState::Break
                }
            }
        }
    )+

    $(
        impl<const N: usize> Pattern for [$r; N] {
            fn len(&self) -> usize {
                1
            }

            fn matches(&self, part: Part, index: usize) -> MatchState {
                if part
                    .as_char()
                    .is_some_and(|cmp| self.iter().any(|range| range.contains(&cmp)))
                {
                    MatchState::Finished
                } else {
                    MatchState::Break
                }
            }
        }
    )+

    $(
        impl Pattern for &[$r] {
            fn len(&self) -> usize {
                1
            }

            fn matches(&self, part: Part, index: usize) -> MatchState {
                if part
                    .as_char()
                    .is_some_and(|cmp| self.iter().any(|range| range.contains(&cmp)))
                {
                    MatchState::Finished
                } else {
                    MatchState::Break
                }
            }
        }
    )+
}
