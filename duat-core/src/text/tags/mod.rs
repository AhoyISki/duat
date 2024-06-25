use std::{collections::HashMap, ops::Range};

use any_rope::{Measurable, Rope};

pub use self::{
    ids::{Marker, Markers, TextId, ToggleId},
    types::{RawTag, Tag},
};
use self::{ranges::TagRange, types::Toggle};
use super::Text;

mod ranges;
mod types;

const MIN_CHARS_TO_KEEP: usize = 50;
const BUMP_AMOUNT: usize = 50;
const LIMIT_TO_BUMP: usize = 500;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TagOrSkip {
    Tag(RawTag),
    Skip(usize),
}

impl TagOrSkip {
    pub fn as_skip(&self) -> Option<usize> {
        match self {
            Self::Skip(v) => Some(*v),
            TagOrSkip::Tag(..) => None,
        }
    }

    fn as_tag(&self) -> Option<RawTag> {
        match self {
            TagOrSkip::Tag(tag) => Some(*tag),
            TagOrSkip::Skip(_) => None,
        }
    }
}

impl std::fmt::Debug for TagOrSkip {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TagOrSkip::Tag(tag) => write!(f, "Tag({:?})", tag),
            TagOrSkip::Skip(amount) => write!(f, "Skip({amount})"),
        }
    }
}

impl Measurable for TagOrSkip {
    type Measure = usize;

    #[inline]
    fn measure(&self) -> usize {
        match self {
            TagOrSkip::Tag(_) => 0,
            TagOrSkip::Skip(skip) => *skip,
        }
    }
}

#[derive(Clone)]
pub struct Tags {
    rope: Rope<TagOrSkip>,
    pub ranges: Vec<TagRange>,
    pub texts: HashMap<TextId, Text>,
    pub toggles: HashMap<ToggleId, Toggle>,
    range_min: usize,
}

impl Tags {
    pub fn new() -> Self {
        Self {
            rope: Rope::new(),
            ranges: Vec::new(),
            texts: HashMap::new(),
            toggles: HashMap::new(),
            range_min: MIN_CHARS_TO_KEEP,
        }
    }

    pub fn with_len(len: usize) -> Self {
        Self {
            rope: Rope::from_slice(&[TagOrSkip::Skip(len)]),
            ranges: Vec::new(),
            texts: HashMap::new(),
            toggles: HashMap::new(),
            range_min: MIN_CHARS_TO_KEEP,
        }
    }

    pub fn clear(&mut self) {
        self.rope = Rope::new();
        self.texts.clear();
        self.toggles.clear();
    }

    pub fn insert(&mut self, pos: usize, tag: Tag, marker: Marker) -> Option<ToggleId> {
        let (raw_tag, toggle_id) = tag.to_raw(marker, &mut self.texts, &mut self.toggles);

        self.insert_raw(pos, raw_tag);

        toggle_id
    }

    pub fn insert_raw(&mut self, pos: usize, raw_tag: RawTag) {
        assert!(pos <= self.measure(), "Char index {} too large", pos);

        if let Some((start, TagOrSkip::Skip(skip))) = self.get_from_pos(pos) {
            // If inserting at any of the ends, no splitting is necessary.
            if pos == start || pos == (start + skip) {
                self.rope.insert(pos, TagOrSkip::Tag(raw_tag), usize::cmp)
            } else {
                let insertion = [
                    TagOrSkip::Skip(pos - start),
                    TagOrSkip::Tag(raw_tag),
                    TagOrSkip::Skip(start + skip - pos),
                ];
                self.rope.insert_slice(start, &insertion, usize::cmp);

                let skip_range = (start + skip)..(start + 2 * skip);
                self.rope.remove_exclusive(skip_range, usize::cmp);
            }
        } else {
            self.rope.insert(pos, TagOrSkip::Tag(raw_tag), usize::cmp);
        }

        add_to_ranges((pos, raw_tag), &mut self.ranges, self.range_min, true, true);
        rearrange_ranges(&mut self.ranges, self.range_min);
        self.cull_small_ranges();
    }

    pub fn append(&mut self, other: Tags) {
        let end = self.measure();
        self.rope.append(other.rope);
        self.texts.extend(other.texts);
        self.toggles.extend(other.toggles);

        for range in other.ranges {
            if let Some(entry) = range.get_start().zip(Some(range.tag())) {
                add_to_ranges(entry, &mut self.ranges, self.range_min, true, true);
            }

            if let Some(entry) = range.get_end().zip(range.tag().inverse()) {
                add_to_ranges(entry, &mut self.ranges, self.range_min, true, true);
            }
        }

        self.merge_surrounding_skips(end);
        rearrange_ranges(&mut self.ranges, self.range_min);
        self.cull_small_ranges();
    }

    /// Removes all [`Tag`]s associated with a given [`Handle`] in the
    /// `pos`.
    pub fn remove_on(&mut self, pos: usize, markers: impl Markers) {
        if self
            .rope
            .iter_at_measure(pos, usize::cmp)
            .next()
            .map_or(true, |(_, t_or_s)| matches!(t_or_s, TagOrSkip::Skip(_)))
        {
            return;
        }

        let removed = remove_inclusive_on(&mut self.rope, pos, markers);

        self.merge_surrounding_skips(pos);
        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges);
        }

        rearrange_ranges(&mut self.ranges, self.range_min);
    }

    pub fn transform_range(&mut self, old: Range<usize>, new_end: usize) {
        // In case we're appending to the rope, a shortcut can be made.
        let Some((start, t_or_s)) = self.get_from_pos(old.start) else {
            let skip = TagOrSkip::Skip(new_end - old.end);
            self.rope.insert(self.rope.measure(), skip, usize::cmp);
            self.merge_surrounding_skips(old.start);
            return;
        };

        let new = old.start..new_end;

        let removal_start = start.min(old.start);
        let removal_end = {
            let (start, t_or_s) = self
                .get_from_pos(old.end)
                .filter(|(end_start, _)| *end_start > start)
                .unwrap_or((start, t_or_s));

            old.end.max(start + t_or_s.measure())
        };

        let range_diff = new_end as isize - old.end as isize;
        let skip = (removal_end - removal_start).saturating_add_signed(range_diff);

        let removed: Vec<_> = self
            .rope
            .iter_at_measure((old.start + 1).min(self.rope.measure()), usize::cmp)
            .filter_map(|(pos, t_or_s)| t_or_s.as_tag().map(|tag| (pos, tag)))
            .take_while(|(pos, ..)| *pos < old.end)
            .collect();

        self.rope.insert(start, TagOrSkip::Skip(skip), usize::cmp);

        let removal_range = (removal_start + skip)..(removal_end + skip).min(self.rope.measure());
        self.rope.remove_exclusive(removal_range, usize::cmp);

        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges);
        }

        shift_ranges_after(new.end, &mut self.ranges, range_diff);
        self.process_ranges_containing(new);
        rearrange_ranges(&mut self.ranges, self.range_min);
        self.cull_small_ranges();
    }

    pub fn back_check_amount(&self) -> usize {
        self.range_min
    }

    /// Returns the is empty of this [`Tags`].
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn measure(&self) -> usize {
        self.rope.measure()
    }

    pub fn len(&self) -> usize {
        self.rope.len()
    }

    pub fn get_from_pos(&self, pos: usize) -> Option<(usize, TagOrSkip)> {
        self.rope.get_from_measure(pos, usize::cmp)
    }

    pub fn iter_at(&self, pos: usize) -> ForwardIter {
        let measure = pos.min(self.measure());

        let ranges = self
            .ranges
            .iter()
            .take_while(move |range| range.get_start().is_some_and(|start| start < measure))
            .filter(move |range| range.get_end().map_or(true, |end| end > measure))
            .flat_map(|range| range.get_start().map(|start| (start, range.tag())));

        let raw_tags =
            self.rope
                .iter_at_measure(measure, usize::cmp)
                .filter_map(move |(pos, t_or_s)| match t_or_s {
                    TagOrSkip::Tag(RawTag::ConcealStart(marker)) => {
                        if let Some(range) = self
                            .ranges
                            .iter()
                            .find(|range| range.starts_with(&(pos, RawTag::ConcealStart(marker))))
                        {
                            let skip = range.get_end().unwrap_or(usize::MAX) - pos;
                            Some((pos, RawTag::Concealed(skip)))
                        } else {
                            Some((pos, RawTag::ConcealStart(marker)))
                        }
                    }
                    TagOrSkip::Tag(tag) => Some((pos, tag)),
                    TagOrSkip::Skip(_) => None,
                });

        ranges.chain(raw_tags).peekable()
    }

    pub fn rev_iter_at(&self, pos: usize) -> ReverseTags {
        let measure = pos.min(self.measure());

        let possible_ranges = {
            let mut ranges: Vec<_> = self
                .ranges
                .iter()
                .filter(|&range| range.get_start().map_or(true, |start| start < measure))
                .map(|range| {
                    let end = range.get_end().unwrap_or(usize::MAX);
                    (end, range.tag().inverse().unwrap())
                })
                .filter(|(end, _)| *end > measure)
                .collect();

            ranges.sort();

            ranges
        };

        let raw_tags = self
            .rope
            .iter_at_measure(measure, usize::cmp)
            .reversed()
            .filter_map(move |(pos, t_or_s)| match t_or_s {
                TagOrSkip::Tag(RawTag::ConcealStart(marker)) => {
                    if let Some(range) = self
                        .ranges
                        .iter()
                        .find(|range| range.ends_with(&(pos, RawTag::ConcealStart(marker))))
                    {
                        let skip = pos - range.get_start().unwrap_or(0);
                        Some((pos, RawTag::Concealed(skip)))
                    } else {
                        Some((pos, RawTag::ConcealStart(marker)))
                    }
                }
                TagOrSkip::Tag(tag) => Some((pos, tag)),
                TagOrSkip::Skip(_) => None,
            });

        possible_ranges.into_iter().rev().chain(raw_tags).peekable()
    }

    pub fn on(&self, measure: usize) -> impl Iterator<Item = RawTag> + '_ {
        self.rope
            .iter_at_measure(measure, usize::cmp)
            .take_while(move |(pos, _)| *pos == measure)
            .filter_map(|(_, t_or_s)| t_or_s.as_tag())
    }

    fn process_ranges_containing(&mut self, new: Range<usize>) {
        let before = {
            let search_start = new.start.saturating_sub(self.range_min);

            self.rope
                .iter_at_measure(search_start, usize::cmp)
                .take_while(|(pos, _)| *pos <= new.start)
                .filter_map(|(pos, t_or_s)| t_or_s.as_tag().map(|tag| (pos, tag)))
        };

        let after = {
            let intersection_shift = (new.end == new.start) as usize;
            let search_end = (new.end + self.range_min + intersection_shift).min(self.measure());

            self.rope
                .iter_at_measure(search_end, usize::cmp)
                .take_while(move |(pos, _)| *pos <= search_end)
                .filter_map(|(pos, t_or_s)| t_or_s.as_tag().map(|tag| (pos, tag)))
        };

        for entry in before.clone().chain(after.clone()) {
            let entry_on_both_sides = entry.0 == new.start && new.start == new.end;
            let start_entry_on_left = entry.0 <= new.start && entry.1.is_start();
            let end_entry_on_right = entry.0 >= new.end && entry.1.is_end();

            if entry_on_both_sides || start_entry_on_left || end_entry_on_right {
                remove_from_ranges(entry, &mut self.ranges);
            }
        }

        for entry in before {
            add_to_ranges(entry, &mut self.ranges, self.range_min, true, false);
        }
        for entry in after {
            add_to_ranges(entry, &mut self.ranges, self.range_min, false, true);
        }
    }

    /// Transforms any surrounding clusters of multiple skips into a
    /// single one.
    ///
    /// This is crucial to prevent the gradual deterioration of the
    /// [`Container`]'s structure.
    fn merge_surrounding_skips(&mut self, from: usize) {
        let tags_in_middle = self
            .rope
            .iter_at_measure(from, usize::cmp)
            .next()
            .is_some_and(|(_, t_or_s)| matches!(t_or_s, TagOrSkip::Tag(..)));

        let (next_skip, last_measure) = {
            let mut total_skip = 0;
            let mut last_measure = from;
            let mut next_tags = self
                .rope
                .iter_at_measure(from, usize::cmp)
                .skip_while(|(_, t_or_s)| matches!(t_or_s, TagOrSkip::Tag(..)));

            while let Some((width, TagOrSkip::Skip(skip))) = next_tags.next() {
                total_skip += skip;
                last_measure = width;
            }

            (total_skip, last_measure)
        };

        let (prev_skip, first_measure) = {
            let mut total_skip = 0;
            let mut first_measure = from;

            let mut prev_tags = self
                .rope
                .iter_at_measure(from, usize::cmp)
                .reversed()
                .skip_while(|(_, t_or_s)| matches!(t_or_s, TagOrSkip::Tag(..)));

            while let Some((measure, TagOrSkip::Skip(skip))) = prev_tags.next() {
                total_skip += skip;
                first_measure = measure;
            }

            (total_skip, first_measure)
        };

        // Merge groups of ranges around `from` into 2 groups.
        if tags_in_middle {
            if last_measure > from {
                // The removal happens after the insertion in order to prevent `Tag`s
                // which are meant to be separate from coming together.
                self.rope
                    .insert(from, TagOrSkip::Skip(next_skip), usize::cmp);
                let skip_range = (from + next_skip)..(from + 2 * next_skip);
                self.rope.remove_exclusive(skip_range, usize::cmp);
            }
            if first_measure < from {
                self.rope
                    .insert(first_measure, TagOrSkip::Skip(prev_skip), usize::cmp);

                let range = (first_measure + prev_skip)..(from + prev_skip);
                self.rope.remove_exclusive(range, usize::cmp);
            }
        // Merge groups of ranges around `from` into 1 group.
        } else if first_measure < last_measure {
            let total_skip = prev_skip + next_skip;

            self.rope
                .insert(first_measure, TagOrSkip::Skip(total_skip), usize::cmp);
            let range = (first_measure + total_skip)..(first_measure + 2 * total_skip);
            self.rope.remove_exclusive(range, usize::cmp);
        }
    }

    fn cull_small_ranges(&mut self) {
        let mut cullable = self
            .ranges
            .iter()
            .filter(|range| matches!(range, TagRange::Bounded(..)))
            .count();

        while cullable > LIMIT_TO_BUMP {
            self.range_min += BUMP_AMOUNT;
            cullable -= self
                .ranges
                .extract_if(|range| {
                    if let TagRange::Bounded(_, bounded) = range {
                        bounded.clone().count() < self.range_min
                    } else {
                        false
                    }
                })
                .count()
        }
    }
}

impl Default for Tags {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for Tags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tags")
            .field("container", &self.rope)
            .field("ranges", &self.ranges)
            .field("texts", &self.texts)
            .field("range_min", &self.range_min)
            .finish_non_exhaustive()
    }
}

impl Eq for Tags {}

impl PartialEq for Tags {
    fn eq(&self, other: &Self) -> bool {
        self.rope.iter().eq_by(other.rope.iter(), |lhs, rhs| {
            if lhs.0 != rhs.0 {
                return false;
            }

            match (lhs, rhs) {
                ((_, TagOrSkip::Skip(lhs)), (_, TagOrSkip::Skip(rhs))) => lhs == rhs,
                ((_, TagOrSkip::Tag(lhs)), (_, TagOrSkip::Tag(rhs))) => match (lhs, rhs) {
                    (RawTag::Concealed(_), _) | (_, RawTag::Concealed(_)) => unreachable!(),
                    (RawTag::PushForm(_, lhs), RawTag::PushForm(_, rhs)) => lhs == rhs,
                    (RawTag::PopForm(_, lhs), RawTag::PopForm(_, rhs)) => lhs == rhs,
                    (RawTag::GhostText(_, lhs), RawTag::GhostText(_, rhs)) => lhs == rhs,
                    (RawTag::ToggleStart(_, lhs), RawTag::ToggleStart(_, rhs)) => lhs == rhs,
                    (RawTag::ToggleEnd(_, lhs), RawTag::ToggleEnd(_, rhs)) => lhs == rhs,
                    (RawTag::MainCursor(_), RawTag::MainCursor(_))
                    | (RawTag::ExtraCursor(_), RawTag::ExtraCursor(_))
                    | (RawTag::StartAlignLeft(_), RawTag::StartAlignLeft(_))
                    | (RawTag::EndAlignLeft(_), RawTag::EndAlignLeft(_))
                    | (RawTag::StartAlignCenter(_), RawTag::StartAlignCenter(_))
                    | (RawTag::EndAlignCenter(_), RawTag::EndAlignCenter(_))
                    | (RawTag::StartAlignRight(_), RawTag::StartAlignRight(_))
                    | (RawTag::EndAlignRight(_), RawTag::EndAlignRight(_))
                    | (RawTag::ConcealStart(_), RawTag::ConcealStart(_))
                    | (RawTag::ConcealEnd(_), RawTag::ConcealEnd(_)) => true,
                    _ => false,
                },
                _ => false,
            }
        }) && self.ranges == other.ranges
            && self.texts == other.texts
            && self.range_min == other.range_min
    }
}

unsafe impl Send for Tags {}
unsafe impl Sync for Tags {}

pub type ForwardIter<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;
pub type ReverseTags<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;

fn remove_inclusive_on(
    rope: &mut Rope<TagOrSkip>,
    pos: usize,
    markers: impl Markers,
) -> Vec<(usize, RawTag)> {
    let range = markers.range();

    let slice = rope.measure_slice(pos..pos, usize::cmp);

    let mut kept = Vec::new();
    let removed = slice
        .iter()
        .filter_map(|(_, t_or_s)| t_or_s.as_tag().map(|tag| (pos, tag)))
        .filter(|(_, tag)| {
            if range.contains(&tag.marker()) {
                true
            } else {
                kept.push(TagOrSkip::Tag(*tag));
                false
            }
        })
        .collect();

    rope.remove_inclusive(pos..pos, usize::cmp);
    rope.insert_slice(pos, &kept, usize::cmp);

    removed
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
        let range = ranges.extract_if(|range| range.starts_with(&entry)).next();
        if let Some(TagRange::Bounded(tag, bounded)) = range {
            let range = TagRange::Until(tag.inverse().unwrap(), ..bounded.end);
            let (Ok(index) | Err(index)) = ranges.binary_search(&range);
            ranges.insert(index, range);
        }
    } else if entry.1.is_end() {
        let range = ranges.extract_if(|range| range.ends_with(&entry)).next();
        if let Some(TagRange::Bounded(tag, bounded)) = range {
            let range = TagRange::From(tag, bounded.start..);
            let (Ok(index) | Err(index)) = ranges.binary_search(&range);
            ranges.insert(index, range);
        }
    }
}

fn add_to_ranges(
    entry: (usize, RawTag),
    ranges: &mut Vec<TagRange>,
    range_min: usize,
    allow_from: bool,
    allow_until: bool,
) {
    let range = if entry.1.is_start() {
        let (start, tag) = entry;
        if let Some(range) = ranges
            .extract_if(|range| range.can_start_with(&entry))
            .next()
        {
            let end = range.get_end().unwrap();
            Some(TagRange::Bounded(tag, start..end))
        } else {
            allow_from.then_some(TagRange::From(tag, start..))
        }
    } else if entry.1.is_end() {
        let (end, tag) = entry;
        if let Some(index) = ranges
            .iter()
            .rev()
            .position(|range| range.can_end_with(&entry))
        {
            let range = ranges.remove(ranges.len() - 1 - index);
            let start = range.get_start().unwrap();
            Some(TagRange::Bounded(range.tag(), start..end))
        } else {
            allow_until.then_some(TagRange::Until(tag, ..end))
        }
    } else {
        return;
    };

    if let Some(range) = range.filter(|range| range.count_ge(range_min)) {
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
            TagRange::Until(..) => break,
        };

        let other = ranges
            .iter()
            .enumerate()
            .rev()
            .filter(|(index, _)| mergers.iter().all(|(_, other)| other != index))
            .take_while(|(_, range)| matches!(range, TagRange::Until(..)))
            .filter(|(_, until)| until.can_start_with(&(start, tag)))
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
            TagRange::Until(..) => unreachable!("We filtered out this type of range."),
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

mod ids {
    use std::{
        ops::Range,
        sync::atomic::{AtomicU16, Ordering},
    };

    static TEXT_COUNT: AtomicU16 = AtomicU16::new(0);
    static TOGGLE_COUNT: AtomicU16 = AtomicU16::new(0);
    static MARKER_COUNT: AtomicU16 = AtomicU16::new(0);

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TextId(u16);

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ToggleId(u16);

    impl TextId {
        pub fn new() -> Self {
            Self(TEXT_COUNT.fetch_add(1, Ordering::Relaxed))
        }
    }

    impl Default for TextId {
        fn default() -> Self {
            Self::new()
        }
    }

    impl ToggleId {
        pub fn new() -> Self {
            Self(TOGGLE_COUNT.fetch_add(1, Ordering::Relaxed))
        }
    }

    impl Default for ToggleId {
        fn default() -> Self {
            Self::new()
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Marker(u16);

    impl Marker {
        pub fn new() -> Self {
            Self(MARKER_COUNT.fetch_add(1, Ordering::Relaxed))
        }

        pub fn new_many(amount: usize) -> Range<Self> {
            let start = Self(MARKER_COUNT.fetch_add(1, Ordering::Relaxed));
            let end = Self(MARKER_COUNT.fetch_add(amount as u16, Ordering::Relaxed));

            start..end
        }
    }

    impl Default for Marker {
        fn default() -> Self {
            Self::new()
        }
    }

    impl std::iter::Step for Marker {
        fn steps_between(start: &Self, end: &Self) -> Option<usize> {
            (end.0 as usize).checked_sub(start.0 as usize)
        }

        fn forward_checked(start: Self, count: usize) -> Option<Self> {
            Some(Self(start.0 + count as u16))
        }

        fn backward_checked(start: Self, count: usize) -> Option<Self> {
            start.0.checked_sub(count as u16).map(Self)
        }
    }

    pub trait Markers {
        fn range(self) -> Range<Marker>;
    }

    impl Markers for Marker {
        fn range(self) -> Range<Marker> {
            Marker(self.0)..Marker(self.0 + 1)
        }
    }

    impl Markers for Range<Marker> {
        fn range(self) -> Range<Marker> {
            self
        }
    }
}
