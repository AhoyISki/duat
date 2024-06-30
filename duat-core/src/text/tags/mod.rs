use std::{cell::RefCell, collections::HashMap, fmt::Write, ops::Range};

use gapbuf::{gap_buffer, GapBuffer};

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

    /// Returns `true` if the tag or skip is [`Skip`].
    ///
    /// [`Skip`]: TagOrSkip::Skip
    #[must_use]
    pub fn is_skip(&self) -> bool {
        matches!(self, Self::Skip(..))
    }

    /// Returns `true` if the tag or skip is [`Tag`].
    ///
    /// [`Tag`]: TagOrSkip::Tag
    #[must_use]
    pub fn is_tag(&self) -> bool {
        matches!(self, Self::Tag(..))
    }

    #[inline]
    fn len(&self) -> usize {
        match self {
            TagOrSkip::Tag(_) => 0,
            TagOrSkip::Skip(skip) => *skip,
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

#[derive(Clone)]
pub struct Tags {
    buf: GapBuffer<TagOrSkip>,
    pub ranges: Vec<TagRange>,
    pub texts: HashMap<TextId, Text>,
    pub toggles: HashMap<ToggleId, Toggle>,
    range_min: usize,
    len: usize,
    cursor: usize,
    last_known: RefCell<Option<(usize, usize)>>,
}

impl Tags {
    pub fn new() -> Self {
        Self {
            buf: GapBuffer::new(),
            ranges: Vec::new(),
            texts: HashMap::new(),
            toggles: HashMap::new(),
            range_min: MIN_CHARS_TO_KEEP,
            len: 0,
            cursor: 0,
            last_known: RefCell::new(None),
        }
    }

    pub fn with_len(len: usize) -> Self {
        Self {
            buf: gap_buffer![TagOrSkip::Skip(len)],
            ranges: Vec::new(),
            texts: HashMap::new(),
            toggles: HashMap::new(),
            range_min: MIN_CHARS_TO_KEEP,
            len,
            cursor: 0,
            last_known: RefCell::new(None),
        }
    }

    pub fn clear(&mut self) {
        self.buf = GapBuffer::new();
        self.texts.clear();
        self.toggles.clear();
    }

    pub fn insert(&mut self, at: usize, tag: Tag, marker: Marker) -> Option<ToggleId> {
        let (raw_tag, toggle_id) = tag.to_raw(marker, &mut self.texts, &mut self.toggles);

        self.insert_raw(at, raw_tag);

        toggle_id
    }

    pub fn insert_raw(&mut self, at: usize, tag: RawTag) {
        self.last_known.take();
        let Some((b, n, skip)) = self.get_skip_at(at) else {
            assert_len!(at, self.len);
            self.buf.push_back(TagOrSkip::Tag(tag));
            return;
        };

        // If inserting at any of the ends, no splitting is necessary.
        if at == b || at == (b + skip) {
            self.buf.insert(n, TagOrSkip::Tag(tag))
        } else {
            self.buf.splice(n..=n, [
                TagOrSkip::Skip(
                    at.checked_sub(b)
                        .unwrap_or_else(|| panic!("{at}, {b}, {:?}", self.buf)),
                ),
                TagOrSkip::Tag(tag),
                TagOrSkip::Skip(b + skip - at),
            ]);
        }

        add_to_ranges((at, tag), &mut self.ranges, self.range_min, true, true);
        rearrange_ranges(&mut self.ranges, self.range_min);
        self.cull_small_ranges();
    }

    pub fn append(&mut self, other: Tags) {
        self.buf.extend(other.buf);
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

        self.merge_surrounding_skips(self.len);
        rearrange_ranges(&mut self.ranges, self.range_min);
        self.cull_small_ranges();

        self.len += other.len;
    }

    /// Removes all [`Tag`]s associated with a given [`Handle`] in the
    /// `pos`.
    pub fn remove_at(&mut self, at: usize, markers: impl Markers) {
        // If we are removing in the middle of a skip, there is
        // nothing to do.
        let Some((_, n, _)) = self.get_skip_at(at).filter(|&(b, ..)| b == at) else {
            return;
        };
        self.last_known.take();

        let removed: Vec<_> = self
            .buf
            .iter()
            .rev()
            .skip(self.buf.len() - n)
            .enumerate()
            .map_while(|(i, ts)| Some(n - (i + 1)).zip(ts.as_tag()))
            .filter(|(_, tag)| markers.clone().contains(tag.marker()))
            .collect();

        for (i, tag) in removed {
            self.buf.remove(i);
            remove_from_ranges((i, tag), &mut self.ranges);
        }

        self.merge_surrounding_skips(at);
        rearrange_ranges(&mut self.ranges, self.range_min);
    }

    pub fn transform_range(&mut self, old: Range<usize>, new_end: usize) {
        let new = old.start..new_end;

        // In case we're appending to the rope, a shortcut can be made.
        let Some((start_p, start_n, _)) = self.get_skip_at(old.start) else {
            assert_len!(old.start, self.len);
            let last = self.buf.len() - 1;
            if let Some(TagOrSkip::Skip(skip)) = self.buf.get_mut(last) {
                *skip += new.end - old.end;
            } else {
                self.buf.push_back(TagOrSkip::Skip(new.end - old.end));
            }

            self.len += new.end;
            self.len -= old.end;

            return;
        };

        let (end_p, end_n) = self
            .get_skip_at(old.end)
            .map(|(end_p, end_n, skip)| (old.end.max(end_p + skip), end_n))
            .unwrap_or((self.len, self.buf.len()));

        let range_diff = new.end as isize - old.end as isize;
        let skip = (end_p - start_p).saturating_add_signed(range_diff);

        let removed = self
            .buf
            .splice(start_n..=end_n, [TagOrSkip::Skip(skip)])
            .scan(start_p, |p, ts| {
                *p += ts.len();
                Some((*p - ts.len(), ts))
            })
            .filter_map(|(p, ts)| ts.as_tag().map(|t| (p, t)));

        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges);
        }

        self.len += new.end;
        self.len -= old.end;
        self.last_known.take();

        shift_ranges_after(new.end, &mut self.ranges, range_diff);
        self.process_ranges_containing(new.clone());
        rearrange_ranges(&mut self.ranges, self.range_min);
        self.cull_small_ranges();
    }

    pub fn back_check_amount(&self) -> usize {
        self.range_min
    }

    /// Returns the is empty of this [`Tags`].
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.buf.len() == 0
    }

    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns information about the skip in the byte `at`
    ///
    /// * The byte where it starts
    /// * The index in the [`GapBuffer`] where it starts
    /// * Its length
    pub fn get_skip_at(&self, at: usize) -> Option<(usize, usize, usize)> {
        let last_known = *self.last_known.borrow();
        let (mut b, n) = [
            Some((0, 0)),
            Some((self.len, self.buf.len())),
            // Some((self.cursor, self.buf.gap())),
            // self.last_known.take(),
        ]
        .into_iter()
        .flatten()
        .min_by(|&(b0, _), &(b1, _)| b0.abs_diff(at).cmp(&b1.abs_diff(at)))
        .unwrap();

        let (first_b, first_n) = (b, n);

        let skips = |(n, s): (usize, &TagOrSkip)| Some(n).zip(s.as_skip());
        let matching_skip = if at >= b {
            let iter = self.buf.iter().enumerate().skip(n);
            let skips = iter.filter_map(skips).take_while(|(_, skip)| {
                if at >= b {
                    b += skip;
                    true
                } else {
                    false
                }
            });

            skips.last()
        } else {
            let (s0, s1) = self.buf.as_slices();
            // Damn you Chain!!!!
            let iter_0 = s0.iter().enumerate().rev();
            let s1_len = |(i, ts)| (i + s0.len(), ts);
            let iter_1 = s1.iter().enumerate().rev().map(s1_len);

            let iter = iter_1.chain(iter_0).skip(self.buf.len() - n);
            let skips = iter.filter_map(skips).take_while(|(_, skip)| {
                if b - skip >= at {
                    b -= skip;
                    true
                } else {
                    false
                }
            });

            skips.last()
        };

        if matching_skip.is_none() && self.len > 100000 {
            // write!(
            //    PANIC_LOG.lock(),
            //    "at {at}, fb {first_b}, fn {first_n}, lk
            // {last_known:?}"
            //);
        }

        matching_skip.map(|(i, skip)| {
            // write!(PANIC_LOG.lock(), "wrote {:?} to lk\n", (b - skip, i));
            self.last_known.replace(Some((b - skip, i)));
            (b - skip, i, skip)
        })
    }

    pub fn iter_at(&self, at: usize) -> ForwardIter {
        let at = at.min(self.len);
        let (mut b, n, _) = self.get_skip_at(at).unwrap();

        let ranges = self
            .ranges
            .iter()
            .take_while(move |range| range.get_start().is_some_and(|start| start < at))
            .filter(move |range| range.get_end().map_or(true, |end| end > at))
            .flat_map(|range| range.get_start().map(|start| (start, range.tag())));

        let tags = self.buf.iter().skip(n).filter_map(move |&ts| match ts {
            TagOrSkip::Tag(RawTag::ConcealStart(marker)) => {
                if let Some(range) = self
                    .ranges
                    .iter()
                    .find(|range| range.starts_with(&(b, RawTag::ConcealStart(marker))))
                {
                    let skip = range.get_end().unwrap_or(usize::MAX) - b;
                    Some((b, RawTag::Concealed(skip as u32)))
                } else {
                    Some((b, RawTag::ConcealStart(marker)))
                }
            }
            TagOrSkip::Tag(tag) => Some((b, tag)),
            TagOrSkip::Skip(skip) => {
                b += skip;
                None
            }
        });

        ranges.chain(tags).peekable()
    }

    pub fn rev_iter_at(&self, at: usize) -> ReverseTags {
        let at = at.min(self.len);
        let (mut b, n, _) = self.get_skip_at(at).unwrap_or_else(|| panic!("{at}"));

        let ranges = {
            let mut ranges: Vec<_> = self
                .ranges
                .iter()
                .filter(|&range| range.get_start().map_or(true, |start| start < at))
                .map(|range| {
                    let end = range.get_end().unwrap_or(self.len);
                    (end, range.tag().inverse().unwrap())
                })
                .filter(|(end, _)| *end > at)
                .collect();

            ranges.sort();

            ranges
        };

        let raw_tags =
            self.buf
                .iter()
                .rev()
                .skip(self.buf.len() - n)
                .filter_map(move |&ts| match ts {
                    TagOrSkip::Tag(RawTag::ConcealStart(marker)) => {
                        if let Some(range) = self
                            .ranges
                            .iter()
                            .find(|range| range.ends_with(&(b, RawTag::ConcealStart(marker))))
                        {
                            let skip = b - range.get_start().unwrap_or(0);
                            Some((b, RawTag::Concealed(skip as u32)))
                        } else {
                            Some((b, RawTag::ConcealStart(marker)))
                        }
                    }
                    TagOrSkip::Tag(tag) => Some((b, tag)),
                    TagOrSkip::Skip(skip) => {
                        b += skip;
                        None
                    }
                });

        ranges.into_iter().rev().chain(raw_tags).peekable()
    }

    pub fn at(&self, at: usize) -> impl Iterator<Item = RawTag> + '_ {
        let b = self.get_skip_at(at).map(|(_, b, _)| b);

        b.into_iter()
            .flat_map(|b| self.buf.iter().skip(b).map_while(TagOrSkip::as_tag))
    }

    fn process_ranges_containing(&mut self, range: Range<usize>) {
        let search = {
            let start = range.start.saturating_sub(self.range_min);
            // in case `range.start == range.end`, we would be double
            // checking that position, so we shift once to the right.
            let intersection_shift = (range.end == range.start) as usize;
            let end = (range.end + self.range_min + intersection_shift).min(self.len);

            start..end
        };

        let before = {
            let (mut b, n, _) = self.get_skip_at(search.start).unwrap();

            self.buf
                .iter()
                .skip(n)
                .filter_map(move |ts| {
                    b += ts.len();
                    ts.as_tag().map(|t| (b - ts.len(), t))
                })
                .take_while(|&(b, _)| b <= range.start)
        };

        let after = {
            let after = self.get_skip_at(range.end);

            after.into_iter().flat_map(|(mut b, n, _)| {
                self.buf
                    .iter()
                    .skip(n)
                    .filter_map(move |ts| {
                        b += ts.len();
                        ts.as_tag().map(|t| (b - ts.len(), t))
                    })
                    .take_while(|&(b, _)| b <= search.end)
            })
        };

        // Removing all ranges that contain the range in question.
        // The reason why this is done is so that the `add_to_ranges`s
        // below can judge wether or not those entries should be added
        // or not, based on factors such as the new distance between
        // the start and end of the ranges. Ranges that end on the left
        // or start on the right cannot be affected by the searched range.
        for entry in before.clone().chain(after.clone()) {
            let entry_on_both_sides = entry.0 == range.start && range.start == range.end;
            let start_entry_on_left = entry.0 <= range.start && entry.1.is_start();
            let end_entry_on_right = entry.0 >= range.end && entry.1.is_end();

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
    fn merge_surrounding_skips(&mut self, at: usize) {
        let (b, n) = match self.get_skip_at(at) {
            Some((b, n, _)) => (b, n),
            None => {
                assert_len!(at, self.len);
                (self.len, self.buf.len())
            }
        };

        let has_tag = b == at && n > 0 && self.buf.get(n - 1).is_some_and(|ts| ts.is_tag());

        let (r_skip, r_end) = {
            let mut total_skip = 0;
            let mut i = 0;
            let mut iter = self.buf.iter().skip(n).skip_while(|ts| ts.is_tag());

            while let Some(TagOrSkip::Skip(skip)) = iter.next() {
                total_skip += skip;
                i += 1;
            }

            (total_skip, n + i)
        };

        let (l_skip, l_range) = {
            let mut total_skip = 0;
            let mut i = 0;

            let rev_n = self.buf.len() - n;
            let tags = self
                .buf
                .iter()
                .rev()
                .skip(rev_n)
                .take_while(|ts| ts.is_tag())
                .count();

            let mut iter = self.buf.iter().rev().skip(rev_n + tags);

            while let Some(TagOrSkip::Skip(skip)) = iter.next() {
                total_skip += skip;
                i += 1;
            }

            (total_skip, (n - (tags + i))..(n - tags))
        };

        // Merge groups of ranges around `at` into 2 groups.
        if has_tag {
            self.buf.splice(n..r_end, [TagOrSkip::Skip(r_skip)]);
            self.buf.splice(l_range, [TagOrSkip::Skip(l_skip)]);
        // Merge groups of ranges around `from` into 1 group.
        } else {
            let total_skip = l_skip + r_skip;
            self.buf
                .splice(l_range.start..r_end, [TagOrSkip::Skip(total_skip)]);
        }
    }

    fn cull_small_ranges(&mut self) {
        let mut cullable = self
            .ranges
            .iter()
            .filter(|range| range.is_bounded())
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

impl PartialEq for Tags {
    fn eq(&self, other: &Self) -> bool {
        self.buf == other.buf
    }
}
impl Eq for Tags {}

impl Default for Tags {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for Tags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tags")
            .field("container", &self.buf)
            .field("ranges", &self.ranges)
            .field("texts", &self.texts)
            .field("range_min", &self.range_min)
            .finish_non_exhaustive()
    }
}

unsafe impl Send for Tags {}
unsafe impl Sync for Tags {}

pub type ForwardIter<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;
pub type ReverseTags<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;

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
        let (Ok(b) | Err(b)) = ranges.binary_search(&range);
        ranges.insert(b, range);
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
                let (Ok(b) | Err(b)) = ranges.binary_search(&new_until);
                ranges.insert(b, new_until);

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

    pub trait Markers: Clone + PartialEq + Eq {
        fn range(self) -> Range<Marker>;

        fn contains(self, marker: Marker) -> bool {
            let range = self.range();
            marker >= range.start && range.end > marker
        }
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

macro assert_len($at:expr, $len:expr) {
    let (at, len) = ($at, $len);
    assert!(at == len, "byte {at} has to be length {len}, but isn't")
}
