use std::{cmp::Ordering::*, ops::Range};

use super::RawTag;

#[derive(Clone, PartialEq, Eq)]
pub enum TagRange {
    Bounded(RawTag, Range<usize>),
    From(RawTag, usize),
    Until(RawTag, usize),
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
            TagRange::From(_, from) => Some(*from),
            TagRange::Until(..) => None,
        }
    }

    pub fn get_end(&self) -> Option<usize> {
        match self {
            TagRange::Bounded(_, bounded) => Some(bounded.end),
            TagRange::Until(_, until) => Some(*until),
            TagRange::From(..) => None,
        }
    }

    pub fn start(&self) -> usize {
        self.get_start().unwrap_or(0)
    }

    pub fn end(&self) -> usize {
        self.get_end().unwrap_or(usize::MAX)
    }

    pub fn starts_with(&self, (b, s_tag): &(usize, RawTag)) -> bool {
        match self {
            TagRange::Bounded(tag, bounded) => bounded.start == *b && *tag == *s_tag,
            TagRange::From(tag, from) => from == b && *tag == *s_tag,
            TagRange::Until(..) => false,
        }
    }

    pub fn ends_with(&self, (b, e_tag): &(usize, RawTag)) -> bool {
        match self {
            TagRange::Bounded(s_tag, bounded) => bounded.end == *b && s_tag.ends_with(e_tag),
            TagRange::Until(s_tag, until) => until == b && *s_tag == *e_tag,
            TagRange::From(..) => false,
        }
    }

    pub fn can_or_does_start_with(&self, (b, s_tag): &(usize, RawTag)) -> bool {
        match self {
            TagRange::Until(e_tag, until) => b <= until && s_tag.ends_with(e_tag),
            TagRange::Bounded(tag, bounded) => tag == s_tag && bounded.start == *b,
            TagRange::From(..) => false,
        }
    }

    pub fn can_or_does_end_with(&self, (b, e_tag): &(usize, RawTag)) -> bool {
        match self {
            TagRange::From(s_tag, from) => from <= b && s_tag.ends_with(e_tag),
            TagRange::Bounded(tag, bounded) => {
                tag.inverse().unwrap() == *e_tag && bounded.end == *b
            }
            TagRange::Until(..) => false,
        }
    }

    pub fn count_ge(&self, other: usize) -> bool {
        match self {
            TagRange::Bounded(_, bounded) => bounded.clone().count() >= other,
            TagRange::From(..) | TagRange::Until(..) => true,
        }
    }

    pub fn entries(&self) -> [Option<(usize, RawTag)>; 2] {
        match self {
            TagRange::Bounded(tag, bounded) => [
                Some((bounded.start, *tag)),
                Some((bounded.end, tag.inverse().unwrap())),
            ],
            TagRange::From(tag, from) => [Some((*from, *tag)), None],
            TagRange::Until(tag, until) => [None, Some((*until, *tag))],
        }
    }

    /// Returns `true` if the tag range is [`Bounded`].
    ///
    /// [`Bounded`]: TagRange::Bounded
    #[must_use]
    pub fn is_bounded(&self) -> bool {
        matches!(self, Self::Bounded(..))
    }
}

impl std::fmt::Debug for TagRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TagRange::Bounded(tag, bounded) => write!(f, "Bounded({tag:?}, {bounded:?})"),
            TagRange::From(tag, from) => write!(f, "From({tag:?}, {from:?})"),
            TagRange::Until(tag, until) => write!(f, "Until({tag:?}, {until:?})"),
        }
    }
}

impl Ord for TagRange {
    /// Entries will be ordered in the following order:
    ///
    /// - First, a mix of `TagRange::Bounded` and `TagRange::From`,
    ///   sorted by:
    ///   - Their starts;
    ///   - Their ends (if `TagRange::From`, always `Greater`);
    ///   - Their `Tag`s;
    ///   - Their `Handle`s.
    ///
    /// - After this, all of the `TagRange::Until` are placed, sorted
    ///   by:
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
                let ordering = lhs.start.cmp(rhs);
                if ordering == Equal { Less } else { ordering }
            }
            (TagRange::Until(lhs_tag, lhs_range), TagRange::Until(rhs_tag, rhs_range)) => {
                (lhs_range, lhs_tag).cmp(&(rhs_range, rhs_tag))
            }
            (TagRange::From(_, lhs), TagRange::Bounded(_, rhs)) => {
                let ordering = lhs.cmp(&rhs.start);
                if ordering == Equal { Greater } else { ordering }
            }
            (TagRange::From(lhs_tag, lhs_range), TagRange::From(rhs_tag, rhs_range)) => {
                (lhs_range, lhs_tag).cmp(&(rhs_range, rhs_tag))
            }
        }
    }
}

impl PartialOrd for TagRange {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
