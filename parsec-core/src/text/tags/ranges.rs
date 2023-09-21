use std::{cmp::Ordering::*, ops::{Range, RangeFrom, RangeTo}};

use super::RawTag;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TagRange {
    Bounded(RawTag, Range<usize>),
    From(RawTag, RangeFrom<usize>),
    Until(RawTag, RangeTo<usize>),
}

impl TagRange {
    pub fn tag(&self) -> RawTag {
        match self {
            TagRange::Bounded(tag, _) => *tag,
            TagRange::From(tag, _) => *tag,
            TagRange::Until(tag, _) => *tag,
        }
    }

    pub fn get_start(&self) -> Option<usize> {
        match self {
            TagRange::Bounded(_, bounded) => Some(bounded.start),
            TagRange::From(_, from) => Some(from.start),
            TagRange::Until(..) => None,
        }
    }

    pub fn get_end(&self) -> Option<usize> {
        match self {
            TagRange::Bounded(_, bounded) => Some(bounded.end),
            TagRange::Until(_, until) => Some(until.end),
            TagRange::From(..) => None,
        }
    }

    pub fn starts_with(&self, other: &(usize, RawTag)) -> bool {
        match self {
            TagRange::Bounded(tag, bounded) => bounded.start == other.0 && *tag == other.1,
            TagRange::From(tag, from) => from.start == other.0 && *tag == other.1,
            TagRange::Until(..) => false,
        }
    }

    pub fn ends_with(&self, other: &(usize, RawTag)) -> bool {
        match self {
            TagRange::Bounded(tag, bounded) => bounded.end == other.0 && tag.ends_with(&other.1),
            TagRange::Until(tag, until) => until.end == other.0 && *tag == other.1,
            TagRange::From(..) => false,
        }
    }

    pub fn can_start_with(&self, other: &(usize, RawTag)) -> bool {
        match self {
            TagRange::Until(tag, until) => other.0 <= until.end && other.1.ends_with(tag),
            TagRange::Bounded(..) | TagRange::From(..) => false,
        }
    }

    pub fn can_end_with(&self, other: &(usize, RawTag)) -> bool {
        match self {
            TagRange::From(tag, from) => from.start <= other.0 && tag.ends_with(&other.1),
            TagRange::Bounded(..) | TagRange::Until(..) => false,
        }
    }

    pub fn count_ge(&self, other: usize) -> bool {
        match self {
            TagRange::Bounded(_, bounded) => bounded.clone().count() >= other,
            TagRange::From(..) | TagRange::Until(..) => true,
        }
    }
}

impl Ord for TagRange {
    /// Entries will be ordered in the following order:
    ///
    /// - First, a mix of `TagRange::Bounded` and `TagRange::From`, sorted by:
    ///   - Their starts;
    ///   - Their ends (if `TagRange::From`, always `Greater`);
    ///   - Their `Tag`s;
    ///   - Their `Handle`s.
    ///
    /// - After this, all of the `TagRange::Until` are placed, sorted by:
    ///   - Their ends;
    ///   - Their `Tag`s;
    ///   - Their `Handle`s.
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (TagRange::Bounded(..), TagRange::Until(..))
            | (TagRange::From(..), TagRange::Until(..)) => Less,
            (TagRange::Until(..), TagRange::Bounded(..))
            | (TagRange::Until(..), TagRange::From(..)) => Greater,

            (TagRange::Bounded(lhs_tag, lhs_range), TagRange::Bounded(rhs_tag, rhs_range)) => {
                let lhs = (lhs_range.start, lhs_range.end, lhs_tag);
                lhs.cmp(&(rhs_range.start, rhs_range.end, rhs_tag))
            }
            (TagRange::Bounded(_, lhs), TagRange::From(_, rhs)) => {
                let ordering = lhs.start.cmp(&rhs.start);
                if ordering == Equal { Less } else { ordering }
            }
            (TagRange::Until(lhs_tag, lhs_range), TagRange::Until(rhs_tag, rhs_range)) => {
                (lhs_range.end, lhs_tag).cmp(&(rhs_range.end, rhs_tag))
            }
            (TagRange::From(_, lhs), TagRange::Bounded(_, rhs)) => {
                let ordering = lhs.start.cmp(&rhs.start);
                if ordering == Equal { Greater } else { ordering }
            }
            (TagRange::From(lhs_tag, lhs_range), TagRange::From(rhs_tag, rhs_range)) => {
                (lhs_range.start, lhs_tag).cmp(&(rhs_range.start, rhs_tag))
            }
        }
    }
}

impl PartialOrd for TagRange {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
