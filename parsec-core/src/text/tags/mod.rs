use std::{
    cmp::Ordering::*,
    ops::{Range, RangeFrom, RangeTo}
};

use any_rope::{Measurable, Rope};
use container::Container;
pub use types::{InsertionTag, RawTag};

use super::Text;
use crate::{position::Point, text::chars::Chars};

mod container;
mod types;

const MIN_CHARS_TO_KEEP: usize = 50;

const BUMP_AMOUNT: usize = 50;
const LIMIT_TO_BUMP: usize = 500;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Handle(u16);

impl Handle {
    pub fn new() -> Handle {
        use std::sync::atomic::{AtomicU16, Ordering};
        static LOCK_COUNT: AtomicU16 = AtomicU16::new(0);

        Handle(LOCK_COUNT.fetch_add(1, Ordering::Acquire))
    }
}

impl Default for Handle {
    fn default() -> Self {
        Handle::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ToggleId(pub(super) usize);

#[derive(Clone, Copy)]
pub enum TagOrSkip {
    Tag(RawTag),
    Skip(usize)
}

impl std::fmt::Debug for TagOrSkip {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TagOrSkip::Tag(tag) => write!(f, "Tag({:?})", tag),
            TagOrSkip::Skip(amount) => write!(f, "Skip({amount})")
        }
    }
}

impl TagOrSkip {
    pub fn as_skip(&self) -> Option<&usize> {
        match self {
            Self::Skip(v) => Some(v),
            TagOrSkip::Tag(..) => None
        }
    }

    fn as_tag(&self) -> Option<RawTag> {
        match self {
            TagOrSkip::Tag(tag) => Some(*tag),
            TagOrSkip::Skip(_) => None
        }
    }
}

impl Measurable for TagOrSkip {
    #[inline]
    fn width(&self) -> usize {
        match self {
            TagOrSkip::Tag(..) => 0,
            TagOrSkip::Skip(skip) => *skip
        }
    }
}

// TODO: Generic container.
pub struct Tags {
    container: Container,
    ranges: Vec<TagRange>,
    texts: Vec<Text>,
    toggles: Vec<(Box<dyn Fn(Point) + Send + Sync>, Box<dyn Fn(Point) + Send + Sync>)>,
    min_to_keep: usize
}

impl Tags {
    pub fn default_vec() -> Self {
        Tags {
            container: Container::Vec(Vec::new()),
            ranges: Vec::new(),
            texts: Vec::new(),
            toggles: Vec::new(),
            min_to_keep: MIN_CHARS_TO_KEEP
        }
    }

    pub fn default_rope() -> Self {
        Tags {
            container: Container::Rope(Rope::new()),
            ranges: Vec::new(),
            texts: Vec::new(),
            toggles: Vec::new(),
            min_to_keep: MIN_CHARS_TO_KEEP
        }
    }

    pub fn clear(&mut self) {
        match &mut self.container {
            Container::Vec(vec) => vec.clear(),
            Container::Rope(rope) => *rope = Rope::new()
        }
    }

    pub fn as_mut_vec(&mut self) -> Option<&mut Vec<TagOrSkip>> {
        match &mut self.container {
            Container::Vec(vec) => Some(vec),
            Container::Rope(_) => None
        }
    }

    pub fn insert(&mut self, pos: usize, insertion_tag: InsertionTag, handle: Handle) {
        let raw_tag = insertion_tag.to_raw(handle, &mut self.texts, &mut self.toggles);

        assert!(pos <= self.width(), "Char index {} too large", pos);

        let Some((start, TagOrSkip::Skip(skip))) = self.get_from_char(pos) else {
            self.container.insert(pos, TagOrSkip::Tag(raw_tag));
            return;
        };

        // If inserting at any of the ends, no splitting is necessary.
        if pos == start || pos == (start + skip) {
            self.container.insert(pos, TagOrSkip::Tag(raw_tag))
        } else {
            let insertion = [
                TagOrSkip::Skip(pos - start),
                TagOrSkip::Tag(raw_tag),
                TagOrSkip::Skip(start + skip - pos)
            ];
            self.container.insert_slice(start, &insertion);

            let skip_range = (start + skip)..(start + 2 * skip);
            self.container.remove_exclusive(skip_range);
        }

        try_insert((pos, raw_tag), &mut self.ranges, self.min_to_keep, true);
        rearrange_ranges(&mut self.ranges, self.min_to_keep);
        self.cull_small_ranges()
    }

    /// Removes all [Tag]s associated with a given [Lock] in the
    /// `ch_index`.
    pub fn remove_on(&mut self, pos: usize, handle: Handle) {
        let removed = self.container.remove_inclusive_on(pos, handle);

        self.merge_surrounding_skips(pos);
        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges);
        }

        rearrange_ranges(&mut self.ranges, self.min_to_keep);
    }

    pub fn transform_range(&mut self, old: Range<usize>, new_end: usize) {
        let (start, t_or_s) = self.get_from_char(old.start).unwrap();
        let new = old.start..new_end;

        let removal_start = start.min(old.start);
        let removal_end = {
            let (start, t_or_s) = self
                .get_from_char(old.end)
                .filter(|(end_start, _)| *end_start > start)
                .unwrap_or((start, t_or_s));

            old.end.max(start + t_or_s.width())
        };

        let range_diff = new_end as isize - old.end as isize;
        let skip = (removal_end - removal_start).saturating_add_signed(range_diff);

        let removed: Vec<(usize, RawTag)> = self
            .container
            .iter_at(old.start + 1)
            .filter_map(|(pos, t_or_s)| t_or_s.as_tag().map(|tag| (pos, tag)))
            .take_while(|(pos, ..)| *pos < old.end)
            .collect();

        self.container.insert(start, TagOrSkip::Skip(skip));
        self.container.remove_exclusive((removal_start + skip)..(removal_end + skip));

        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges);
        }

        shift_ranges_after(new.end, &mut self.ranges, range_diff);
        self.process_ranges_containing(new, old.count());
        rearrange_ranges(&mut self.ranges, self.min_to_keep);
        self.cull_small_ranges()
    }

    fn process_ranges_containing(&mut self, new: Range<usize>, old_count: usize) {
        let new_count = new.clone().count();
        if old_count == new_count {
            return;
        }

        if new_count < old_count {
            self.ranges
                .extract_if(|range| {
                    if let TagRange::Bounded(_, range, ..) = range {
                        if range.start <= new.start && range.end >= new.end {
                            return range.clone().count() <= self.min_to_keep;
                        }
                    }
                    false
                })
                .take_while(|range| range.get_start().is_some_and(|start| start <= new.start))
                .last();
        } else {
            let start = new.start.saturating_sub(self.min_to_keep - old_count);
            let end = new.end + self.min_to_keep - old_count;
            let mut entry_counts = Vec::new();

            for entry in self
                .container
                .iter_at(start)
                .take_while(|(pos, _)| *pos < end)
                .filter_map(|(pos, t_or_s)| t_or_s.as_tag().map(|tag| (pos, tag)))
            {
                let count = if let Some((count, _)) =
                    entry_counts.iter_mut().find(|(_, other)| *other == entry)
                {
                    count
                } else {
                    let count = count_entry(entry, &self.ranges);
                    entry_counts.push((count, entry));
                    &mut entry_counts.last_mut().unwrap().0
                };

                if *count == 0 {
                    try_insert(entry, &mut self.ranges, self.min_to_keep, false);
                } else {
                    *count -= 1;
                }
            }
        }
    }

    /// Transforms any surrounding clusters of multiple skips into a
    /// single one.
    ///
    /// This is crucial to prevent the gradual deterioration of the
    /// [`InnerTags`]'s structure.
    fn merge_surrounding_skips(&mut self, from: usize) {
        let tags_in_middle = self
            .container
            .iter_at(from)
            .next()
            .is_some_and(|(_, t_or_s)| matches!(t_or_s, TagOrSkip::Tag(..)));

        let (next_skip, last_width) = {
            let mut total_skip = 0;
            let mut last_width = from;
            let mut next_tags = self
                .container
                .iter_at(from)
                .skip_while(|(_, t_or_s)| matches!(t_or_s, TagOrSkip::Tag(..)));

            while let Some((width, TagOrSkip::Skip(skip))) = next_tags.next() {
                total_skip += skip;
                last_width = width;
            }

            (total_skip, last_width)
        };

        let (prev_skip, first_width) = {
            let mut total_skip = 0;
            let mut first_width = from;

            let mut prev_tags = self
                .container
                .rev_iter_at(from)
                .skip_while(|(_, t_or_s)| matches!(t_or_s, TagOrSkip::Tag(..)));

            while let Some((width, TagOrSkip::Skip(skip))) = prev_tags.next() {
                total_skip += skip;
                first_width = width;
            }

            (total_skip, first_width)
        };

        // Merge groups of ranges around `from` into 2 groups.
        if tags_in_middle {
            if last_width > from {
                // The removal happens after the insertion in order to prevent `Tag`s
                // which are meant to be separate from coming together.
                self.container.insert(from, TagOrSkip::Skip(next_skip));
                let skip_range = (from + next_skip)..(from + 2 * next_skip);
                self.container.remove_exclusive(skip_range);
            }
            if first_width < from {
                self.container.insert(first_width, TagOrSkip::Skip(prev_skip));

                let range = (first_width + prev_skip)..(from + prev_skip);
                self.container.remove_exclusive(range);
            }
        // Merge groups of ranges around `from` into 1 group.
        } else if first_width < last_width {
            let total_skip = prev_skip + next_skip;

            self.container.insert(first_width, TagOrSkip::Skip(total_skip));
            let range = (first_width + total_skip)..(first_width + 2 * total_skip);
            self.container.remove_exclusive(range);
        }
    }

    fn cull_small_ranges(&mut self) {
        let mut cullable =
            self.ranges.iter().filter(|range| matches!(range, TagRange::Bounded(..))).count();

        while cullable > LIMIT_TO_BUMP {
            self.min_to_keep += BUMP_AMOUNT;
            cullable -= self
                .ranges
                .extract_if(|range| {
                    if let TagRange::Bounded(_, bounded) = range {
                        bounded.clone().count() < self.min_to_keep
                    } else {
                        false
                    }
                })
                .count()
        }
    }

    pub fn new(chars: &Chars) -> Self {
        let skip = TagOrSkip::Skip(chars.len_chars());
        let container = match chars {
            Chars::String(_) => Container::Vec(vec![skip]),
            Chars::Rope(_) => Container::Rope(Rope::from_slice(&[skip]))
        };
        Tags {
            container,
            ranges: Vec::new(),
            texts: Vec::new(),
            toggles: Vec::new(),
            min_to_keep: MIN_CHARS_TO_KEEP
        }
    }

    pub fn back_check_amount(&self) -> usize {
        self.min_to_keep
    }

    /// Returns the is empty of this [`Tags`].
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn as_vec(&self) -> Option<&[TagOrSkip]> {
        match &self.container {
            Container::Vec(vec) => Some(vec),
            Container::Rope(_) => None
        }
    }

    pub fn width(&self) -> usize {
        match &self.container {
            Container::Vec(vec) => vec.iter().map(|tag_or_skip| tag_or_skip.width()).sum(),
            Container::Rope(rope) => rope.width()
        }
    }

    pub fn len(&self) -> usize {
        match &self.container {
            Container::Vec(vec) => vec.len(),
            Container::Rope(rope) => rope.len()
        }
    }

    pub fn get_from_char(&self, char: usize) -> Option<(usize, TagOrSkip)> {
        self.container.get_from_char(char)
    }

    pub fn iter_at(&self, pos: usize) -> Iter {
        let pos = pos.min(self.width());
        let ranges = Ranges::new(pos, self.ranges.iter());
        let raw_tags = RawTags::new(&self.ranges, self.container.iter_at(pos));
        Iter::new(self, ranges, raw_tags)
    }

    pub fn rev_iter_at(&self, pos: usize) -> RevIter {
        let possible_ranges: Vec<TagRange> = self
            .ranges
            .iter()
            .filter(|range| {
                range.get_start().is_some_and(|start| start < pos)
                    && range.get_end().map_or(true, |end| end >= pos)
            })
            .cloned()
            .collect();

        let ranges = RevRanges::new(possible_ranges);
        let tags = RevRawTags::new(&self.ranges, self.container.rev_iter_at(pos));
        RevIter::new(self, ranges, tags)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TagRange {
    Bounded(RawTag, Range<usize>),
    From(RawTag, RangeFrom<usize>),
    Until(RawTag, RangeTo<usize>)
}

impl TagRange {
    fn tag(&self) -> RawTag {
        match self {
            TagRange::Bounded(tag, _) => *tag,
            TagRange::From(tag, _) => *tag,
            TagRange::Until(tag, _) => *tag
        }
    }

    fn get_start(&self) -> Option<usize> {
        match self {
            TagRange::Bounded(_, bounded) => Some(bounded.start),
            TagRange::From(_, from) => Some(from.start),
            TagRange::Until(..) => None
        }
    }

    fn get_end(&self) -> Option<usize> {
        match self {
            TagRange::Bounded(_, bounded) => Some(bounded.end),
            TagRange::Until(_, until) => Some(until.end),
            TagRange::From(..) => None
        }
    }

    fn starts_with(&self, other: (usize, RawTag)) -> bool {
        match self {
            TagRange::Bounded(tag, bounded) => bounded.start == other.0 && *tag == other.1,
            TagRange::From(tag, from) => from.start == other.0 && *tag == other.1,
            TagRange::Until(..) => false
        }
    }

    fn ends_with(&self, other: (usize, RawTag)) -> bool {
        match self {
            TagRange::Bounded(tag, bounded) => bounded.end == other.0 && tag.ends_with(&other.1),
            TagRange::Until(tag, until) => until.end == other.0 && *tag == other.1,
            TagRange::From(..) => false
        }
    }

    fn can_start_with(&self, other: (usize, RawTag)) -> bool {
        match self {
            TagRange::Until(tag, until) => other.0 <= until.end && other.1.ends_with(tag),
            TagRange::Bounded(..) | TagRange::From(..) => false
        }
    }

    fn can_end_with(&self, other: (usize, RawTag)) -> bool {
        match self {
            TagRange::From(tag, from) => from.start <= other.0 && tag.ends_with(&other.1),
            TagRange::Bounded(..) | TagRange::Until(..) => false
        }
    }

    fn count_ge(&self, other: usize) -> bool {
        match self {
            TagRange::Bounded(_, bounded) => bounded.clone().count() >= other,
            TagRange::From(..) | TagRange::Until(..) => true
        }
    }
}

impl Ord for TagRange {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        unsafe { self.partial_cmp(other).unwrap_unchecked() }
    }
}

impl PartialOrd for TagRange {
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
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let ordering = match (self, other) {
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
        };

        Some(ordering)
    }
}

#[derive(Clone)]
struct Ranges<'a> {
    max_start: usize,
    iter: std::slice::Iter<'a, TagRange>
}

impl<'a> Ranges<'a> {
    fn new(max_start: usize, iter: std::slice::Iter<'a, TagRange>) -> Self {
        Self { max_start, iter }
    }
}

impl<'a> Iterator for Ranges<'a> {
    type Item = (usize, RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .find(|range| {
                range.get_start().is_some_and(|start| start < self.max_start)
                    && range.get_end().map_or(true, |end| end > self.max_start)
            })
            .and_then(|range| range.get_start().map(|start| (start, range.tag())))
    }
}

#[derive(Clone)]
struct RevRanges {
    ranges: Vec<TagRange>
}

impl RevRanges {
    fn new(ranges: Vec<TagRange>) -> Self {
        Self { ranges }
    }
}

impl Iterator for RevRanges {
    type Item = (usize, RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((index, _)) = self
            .ranges
            .iter()
            .enumerate()
            .max_by_key(|(_, range)| range.get_end().unwrap_or(usize::MAX))
        {
            let range = self.ranges.remove(index);
            let end = range.get_end().unwrap_or(usize::MAX);
            Some((end, range.tag().inverse().unwrap()))
        } else {
            None
        }
    }
}

#[derive(Clone)]
struct RawTags<'a> {
    ranges: &'a [TagRange],
    iter: container::Iter<'a, container::ForwardTags<'a>>
}
impl<'a> RawTags<'a> {
    fn new(ranges: &'a [TagRange], iter: container::Iter<'a, container::ForwardTags<'a>>) -> Self {
        Self { ranges, iter }
    }
}

impl<'a> Iterator for RawTags<'a> {
    type Item = (usize, RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.find_map(|(pos, t_or_s)| match t_or_s {
            TagOrSkip::Tag(RawTag::ConcealStart(handle)) => {
                if let Some(range) = self
                    .ranges
                    .iter()
                    .find(|range| range.starts_with((pos, RawTag::ConcealStart(handle))))
                {
                    let skip = range.get_end().map_or(usize::MAX, |end| end - pos);
                    Some((pos, RawTag::Concealed(skip)))
                } else {
                    Some((pos, RawTag::ConcealStart(handle)))
                }
            }
            TagOrSkip::Tag(tag) => Some((pos, tag)),
            TagOrSkip::Skip(_) => None
        })
    }
}

#[derive(Clone)]
struct RevRawTags<'a> {
    ranges: &'a [TagRange],
    iter: container::Iter<'a, container::ReverseTags<'a>>
}

impl<'a> RevRawTags<'a> {
    fn new(ranges: &'a [TagRange], iter: container::Iter<'a, container::ReverseTags<'a>>) -> Self {
        Self { ranges, iter }
    }
}

impl<'a> Iterator for RevRawTags<'a> {
    type Item = (usize, RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.find_map(|(pos, t_or_s)| match t_or_s {
            TagOrSkip::Tag(RawTag::ConcealEnd(handle)) => {
                if let Some(range) = self
                    .ranges
                    .iter()
                    .find(|range| range.ends_with((pos, RawTag::ConcealEnd(handle))))
                {
                    let skip = range.get_start().map_or(0, |start| pos - start);
                    Some((pos, RawTag::Concealed(skip)))
                } else {
                    Some((pos, RawTag::ConcealEnd(handle)))
                }
            }
            TagOrSkip::Tag(tag) => Some((pos, tag)),
            TagOrSkip::Skip(_) => None
        })
    }
}

#[derive(Clone)]
pub struct Iter<'a> {
    tags: &'a Tags,
    iter: std::iter::Chain<Ranges<'a>, RawTags<'a>>,
    peeked: Option<Option<(usize, RawTag)>>
}

impl<'a> Iter<'a> {
    fn new(tags: &'a Tags, ranges: Ranges<'a>, raw_tags: RawTags<'a>) -> Self {
        let iter = ranges.chain(raw_tags);
        Self { tags, iter, peeked: None }
    }

    pub fn move_to(&mut self, pos: usize) {
        *self = self.tags.iter_at(pos.saturating_sub(self.tags.back_check_amount()));
    }

    pub fn peek(&mut self) -> Option<&(usize, RawTag)> {
        let iter = &mut self.iter;
        self.peeked.get_or_insert_with(|| iter.next()).as_ref()
    }
}

impl Iterator for Iter<'_> {
    type Item = (usize, RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        self.peeked.take().flatten().or_else(|| self.iter.next())
    }
}

#[derive(Clone)]
pub struct RevIter<'a> {
    tags: &'a Tags,
    iter: std::iter::Chain<RevRanges, RevRawTags<'a>>,
    peeked: Option<Option<(usize, RawTag)>>
}

impl<'a> RevIter<'a> {
    fn new(tags: &'a Tags, ranges: RevRanges, raw_tags: RevRawTags<'a>) -> Self {
        let iter = ranges.chain(raw_tags);
        Self { tags, iter, peeked: None }
    }

    pub fn move_to(&mut self, pos: usize) {
        *self = self.tags.rev_iter_at(pos + self.tags.back_check_amount());
    }

    pub fn peek(&mut self) -> Option<&(usize, RawTag)> {
        let iter = &mut self.iter;
        self.peeked.get_or_insert_with(|| iter.next()).as_ref()
    }
}

impl Iterator for RevIter<'_> {
    type Item = (usize, RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        self.peeked.take().flatten().or_else(|| self.iter.next())
    }
}

/// Removes the given `(usize, Tag, Handle)` triples from any
/// range in `self.ranges`.
///
/// This will either lead to a partially unbounded range, or
/// completely remove it.
///
/// Will return the range as it was, before the removal.
fn remove_from_ranges(entry: (usize, RawTag), ranges: &mut Vec<TagRange>) {
    if entry.1.is_start() {
        let range = ranges.extract_if(|range| range.starts_with(entry)).next();
        if let Some(TagRange::Bounded(tag, bounded)) = range {
            let range = TagRange::Until(tag.inverse().unwrap(), ..bounded.end);
            let (Ok(index) | Err(index)) = ranges.binary_search(&range);
            ranges.insert(index, range);
        }
    } else if entry.1.is_end() {
        let range = ranges.extract_if(|range| range.ends_with(entry)).next();
        if let Some(TagRange::Bounded(tag, bounded)) = range {
            let range = TagRange::From(tag, bounded.start..);
            let (Ok(index) | Err(index)) = ranges.binary_search(&range);
            ranges.insert(index, range);
        }
    }
}

fn count_entry(entry: (usize, RawTag), ranges: &[TagRange]) -> usize {
    if entry.1.is_start() {
        let (start, tag) = entry;
        let range = TagRange::From(tag, start..);
        if let Ok(index) = ranges.binary_search(&range) {
            let start_matches = |range: &&TagRange| range.starts_with(entry);
            let next = ranges.iter().skip(index).take_while(start_matches);
            let prev = ranges.iter().take(index).rev().take_while(start_matches);
            next.chain(prev).count()
        } else {
            0
        }
    } else if entry.1.is_end() {
        ranges.iter().filter(|range| range.ends_with(entry)).count()
    } else {
        0
    }
}

fn try_insert(
    entry: (usize, RawTag), ranges: &mut Vec<TagRange>, min_to_keep: usize, allow_unbounded: bool
) {
    let range = if entry.1.is_start() {
        let (start, tag) = entry;
        if let Some(range) = ranges.extract_if(|range| range.can_start_with(entry)).next() {
            let end = range.get_end().unwrap();
            Some(TagRange::Bounded(tag, start..end))
        } else {
            allow_unbounded.then_some(TagRange::From(tag, start..))
        }
    } else if entry.1.is_end() {
        let (end, tag) = entry;
        if let Some(index) = ranges.iter().rev().position(|range| range.can_end_with(entry)) {
            let range = ranges.remove(ranges.len() - 1 - index);
            let start = range.get_start().unwrap();
            Some(TagRange::Bounded(range.tag(), start..end))
        } else {
            allow_unbounded.then_some(TagRange::Until(tag, ..end))
        }
    } else {
        return;
    };

    if let Some(range) = range.filter(|range| range.count_ge(min_to_keep)) {
        let (Ok(index) | Err(index)) = ranges.binary_search(&range);
        ranges.insert(index, range);
    }
}

fn shift_ranges_after(after: usize, ranges: &mut [TagRange], amount: isize) {
    for range in ranges.iter_mut() {
        match range {
            TagRange::Bounded(_, bounded) => {
                if bounded.start >= after {
                    let start = bounded.start.saturating_add_signed(amount);
                    let end = bounded.end.saturating_add_signed(amount);
                    *bounded = start..end
                } else if bounded.end >= after {
                    bounded.end = bounded.end.saturating_add_signed(amount)
                }
            }
            TagRange::From(_, from) => {
                if from.start >= after {
                    *from = from.start.saturating_add_signed(amount)..
                }
            }
            TagRange::Until(_, until) => {
                if until.end >= after {
                    *until = ..until.end.saturating_add_signed(amount)
                }
            }
        }
    }
}

fn rearrange_ranges(ranges: &mut Vec<TagRange>, min_to_keep: usize) {
    let mut mergers = Vec::new();
    for (index, range) in ranges.iter().enumerate() {
        let (start, tag) = match range {
            TagRange::Bounded(tag, bounded) => (bounded.start, *tag),
            TagRange::From(tag, from) => (from.start, *tag),
            TagRange::Until(..) => break
        };

        let other = ranges
            .iter()
            .enumerate()
            .rev()
            .filter(|(index, _)| mergers.iter().all(|(_, other)| other != index))
            .take_while(|(_, range)| matches!(range, TagRange::Until(..)))
            .filter(|(_, until)| until.can_start_with((start, tag)))
            .last();

        if let Some((other, _)) = other {
            mergers.push((index, other));
        }
    }

    for (range, until) in mergers {
        let until = ranges.remove(until);
        let range = ranges.remove(range);

        let (tag, start) = match range {
            TagRange::Bounded(tag, bounded) => {
                let new_until = TagRange::Until(tag.inverse().unwrap(), ..bounded.end);
                let (Ok(index) | Err(index)) = ranges.binary_search(&new_until);
                ranges.insert(index, new_until);

                (tag, bounded.start)
            }
            TagRange::From(tag, from) => (tag, from.start),
            TagRange::Until(..) => unreachable!("We filtered out this type of range.")
        };

        let TagRange::Until(_, until) = until else {
            unreachable!("We filtered out all other types of range.");
        };

        if (start..until.end).count() >= min_to_keep {
            let range = TagRange::Bounded(tag, start..until.end);
            let (Ok(index) | Err(index)) = ranges.binary_search(&range);
            ranges.insert(index, range);
        }
    }
}
