use std::{
    self,
    collections::HashMap,
    ops::{Range, RangeBounds},
};

use gapbuf::{gap_buffer, GapBuffer};

pub use self::{
    ids::{Marker, Markers, TextId, ToggleId},
    types::{
        RawTag::{self, *},
        Tag,
    },
};
use self::{ranges::TagRange, types::Toggle};
use super::{get_ends, records::Records, Point, Text};

mod ids;
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
    pub buf: GapBuffer<TagOrSkip>,
    pub texts: HashMap<TextId, Text>,
    toggles: HashMap<ToggleId, Toggle>,
    range_min: usize,
    ranges: Vec<TagRange>,
    pub records: Records<(usize, usize)>,
}

impl Tags {
    pub fn new() -> Self {
        Self {
            buf: GapBuffer::new(),
            ranges: Vec::new(),
            texts: HashMap::new(),
            toggles: HashMap::new(),
            range_min: MIN_CHARS_TO_KEEP,
            records: Records::new(),
        }
    }

    pub fn with_len(len: usize) -> Self {
        Self {
            buf: gap_buffer![TagOrSkip::Skip(len)],
            ranges: Vec::new(),
            texts: HashMap::new(),
            toggles: HashMap::new(),
            range_min: MIN_CHARS_TO_KEEP,
            records: Records::with_max((1, len)),
        }
    }

    pub fn clear(&mut self) {
        self.buf = GapBuffer::new();
        self.texts.clear();
        self.toggles.clear();
        self.records.clear();
    }

    pub fn insert(&mut self, at: usize, tag: Tag, marker: Marker) -> Option<ToggleId> {
        let (tag, toggle_id) = tag.to_raw(marker, &mut self.texts, &mut self.toggles);

        self.insert_raw(at, tag);

        toggle_id
    }

    pub fn insert_raw(&mut self, at: usize, tag: RawTag) {
        let Some((n, b, skip)) = self.get_skip_at(at) else {
            self.buf.push_back(TagOrSkip::Tag(tag));
            self.records.append((1, 0));
            return;
        };

        if at == b {
            self.buf.insert(n, TagOrSkip::Tag(tag));
            self.records.transform((n, at), (0, 0), (1, 0));
            self.records.insert((n + 1, at));
        } else {
            self.buf.splice(n..=n, [
                TagOrSkip::Skip(at - b),
                TagOrSkip::Tag(tag),
                TagOrSkip::Skip(b + skip - at),
            ]);
            self.records.transform((n, at), (0, 0), (2, 0));
            self.records.insert((n + 2, at));
        }

        add_to_ranges((at, tag), &mut self.ranges, self.range_min, true, true);
        rearrange_ranges(&mut self.ranges, self.range_min);
        self.cull_small_ranges();
    }

    pub fn extend(&mut self, other: Tags) {
        self.buf.extend(other.buf);
        self.texts.extend(other.texts);
        self.toggles.extend(other.toggles);
        self.records.extend(other.records);

        for range in other.ranges {
            if let Some(entry) = range.get_start().zip(Some(range.tag())) {
                add_to_ranges(entry, &mut self.ranges, self.range_min, true, true);
            }

            if let Some(entry) = range.get_end().zip(range.tag().inverse()) {
                add_to_ranges(entry, &mut self.ranges, self.range_min, true, true);
            }
        }

        self.fuse_skips_at(self.len_bytes());
        rearrange_ranges(&mut self.ranges, self.range_min);
        self.cull_small_ranges();
    }

    /// Removes all [`Tag`]s associated with a given [`Handle`] in the
    /// `pos`.
    pub fn remove_at(&mut self, at: usize, markers: impl Markers) {
        let (n, b) = match self.get_skip_at(at) {
            Some((n, b, _)) if b == at => (n, b),
            None => (self.buf.len(), self.len_bytes()),
            // If `b != n`, we're in the middle of a skip, and nothing
            // is done.
            _ => return,
        };

        let removed: Vec<_> = iter_range_rev(&self.buf, ..n)
            .enumerate()
            .map_while(|(i, ts)| Some(n - (i + 1)).zip(ts.as_tag()))
            .filter(|(_, tag)| markers.clone().contains(tag.marker()))
            .collect();

        self.records.transform((n, b), (removed.len(), 0), (0, 0));

        for (i, tag) in removed {
            self.buf.remove(i);
            remove_from_ranges((b, tag), &mut self.ranges);
        }

        self.fuse_skips_at(at);
        rearrange_ranges(&mut self.ranges, self.range_min);
    }

    pub fn transform(&mut self, old: Range<usize>, new_end: usize) {
        let new = old.start..new_end;

        // In case we're appending to the rope, a shortcut can be made.
        let Some((start_n, start_b, _)) = self.get_skip_at(old.start) else {
            let last = self.buf.len().saturating_sub(1);
            if let Some(TagOrSkip::Skip(skip)) = self.buf.get_mut(last) {
                *skip += new.end - old.start;
                self.records.append((0, new.end - old.start));
            } else {
                self.buf.push_back(TagOrSkip::Skip(new.end - old.start));
                self.records.append((1, new.end - old.start));
            }

            return;
        };

        let (end_n, end_b) = self
            .get_skip_at(old.end)
            .map(|(end_n, end_b, skip)| (end_n, old.end.max(end_b + skip)))
            .unwrap_or((self.buf.len() - 1, self.len_bytes()));

        let range_diff = new.end as isize - old.end as isize;
        let skip = (end_b - start_b).saturating_add_signed(range_diff);

        let removed = self
            .buf
            .splice(start_n..=end_n, [TagOrSkip::Skip(skip)])
            .scan(start_b, |p, ts| {
                *p += ts.len();
                Some((*p - ts.len(), ts))
            })
            .filter_map(|(p, ts)| ts.as_tag().map(|t| (p, t)));

        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges);
        }

        self.records.transform(
            (start_n, old.start),
            (1 + end_n - start_n, old.clone().count()),
            (1, new.clone().count()),
        );

        shift_ranges_after(new.end, &mut self.ranges, range_diff);
        self.process_ranges_containing(new.clone());
        rearrange_ranges(&mut self.ranges, self.range_min);
        self.cull_small_ranges();
    }

    /// Returns the is empty of this [`Tags`].
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.buf.len() == 0
    }

    pub fn len_bytes(&self) -> usize {
        self.records.max().1
    }

    pub fn iter_at(&self, at: usize) -> FwdTags {
        let at = at.min(self.len_bytes()).saturating_sub(self.range_min);

        let (n, b) = {
            let (n, b, _) = self.get_skip_at(at).unwrap_or_default();
            let iter = iter_range_rev(&self.buf, ..n);

            (n - iter.take_while(|ts| ts.is_tag()).count(), b)
        };

        let ranges = self
            .ranges
            .iter()
            .take_while(move |range| range.get_start().is_some_and(|start| start < at))
            .filter(move |range| range.get_end().map_or(true, |end| end > at))
            .flat_map(|range| range.get_start().map(|start| (start, range.tag())));

        let tags = {
            let iter = iter_range(&self.buf, n..).filter_map(raw_from(b));
            iter.map(|(b, tag)| match tag {
                ConcealStart(marker) => {
                    match self
                        .ranges
                        .iter()
                        .find(|range| range.starts_with(&(b, ConcealStart(marker))))
                    {
                        Some(range) => (b, Concealed(range.end() as u32)),
                        None => (b, ConcealStart(marker)),
                    }
                }
                tag => (b, tag),
            })
        };

        ranges.chain(tags).peekable()
    }

    pub fn rev_iter_at(&self, at: usize) -> RevTags {
        let (n, b) = self
            .get_skip_at(at)
            .map(|(n, b, _)| (n, b))
            .unwrap_or((self.buf.len(), self.len_bytes()));

        let ranges = {
            let mut ranges: Vec<_> = self
                .ranges
                .iter()
                .filter(|&range| range.get_start().map_or(true, |start| start < at))
                .map(|range| (range.end(), range.tag().inverse().unwrap()))
                .filter(|(end, _)| *end > at)
                .collect();

            ranges.sort();
            ranges
        };

        let raw_tags = {
            let iter = iter_range_rev(&self.buf, ..n).filter_map(raw_from_rev(b));
            iter.map(|(b, tag)| match tag {
                ConcealEnd(marker) => {
                    if let Some(range) = self
                        .ranges
                        .iter()
                        .find(|range| range.ends_with(&(b, ConcealEnd(marker))))
                    {
                        (b, Concealed((b - range.get_start().unwrap_or(0)) as u32))
                    } else {
                        (b, ConcealEnd(marker))
                    }
                }
                tag => (b, tag),
            })
        };

        ranges.into_iter().rev().chain(raw_tags).peekable()
    }

    pub fn iter_only_at(&self, at: usize) -> impl Iterator<Item = RawTag> + '_ {
        let (n, b) = self
            .get_skip_at(at)
            .map(|(n, b, _)| (n, b))
            .unwrap_or((self.buf.len(), self.len_bytes()));

        (b == at)
            .then(|| iter_range_rev(&self.buf, ..n).map_while(TagOrSkip::as_tag))
            .into_iter()
            .flatten()
    }

    pub fn ghosts_total_at(&self, at: usize) -> Option<Point> {
        self.iter_only_at(at).fold(None, |p, tag| match tag {
            RawTag::GhostText(_, id) => {
                let max_point = self.texts.get(&id).unwrap().max_point();
                Some(p.map_or(max_point, |p| p + max_point))
            }
            _ => p,
        })
    }

    /// Returns information about the skip in the byte `at`
    ///
    /// * The byte where it starts
    /// * The index in the [`GapBuffer`] where it starts
    /// * Its length
    fn get_skip_at(&self, at: usize) -> Option<(usize, usize, usize)> {
        assert!(
            at <= self.len_bytes(),
            "byte out of bounds: the len is {}, but the byte is {at}",
            self.len_bytes()
        );
        let (n, b) = self.records.closest_to(at);

        let skips = {
            let mut b_len = 0;
            move |(n, ts): (usize, &TagOrSkip)| {
                b_len += ts.len();
                ts.as_skip().map(|s| (n, b_len, s))
            }
        };

        if at >= b {
            let iter = iter_range(&self.buf, n..).enumerate().filter_map(skips);
            iter.map(|(i, this_b, skip)| (n + i, b + (this_b - skip), skip))
                .take_while(|(_, b, _)| at >= *b)
                .last()
        } else {
            let iter = iter_range_rev(&self.buf, ..n).enumerate().filter_map(skips);
            iter.map(|(i, this_b, skip)| (n - (i + 1), b - this_b, skip))
                .take_while(|(_, b, skip)| *b + *skip > at)
                .last()
        }
    }

    fn process_ranges_containing(&mut self, range: Range<usize>) {
        let (before, after) = {
            let before_start = range.start.saturating_sub(self.range_min);
            // in case `range.start == range.end`, we would be double
            // checking that position, so we shift once to the right.
            let cross_shift = (range.end == range.start) as usize;
            let after_start = (range.end + cross_shift).min(self.len_bytes());
            let after_end = (range.end + self.range_min).min(self.len_bytes());

            (before_start..=range.start, after_start..=after_end)
        };

        let before = self
            .get_skip_at(*before.start())
            .into_iter()
            .flat_map(|(n, b, _)| iter_range(&self.buf, n..).filter_map(raw_from(b)))
            .take_while(|&(b, _)| b <= *before.end());

        let after = self
            .get_skip_at(*after.start())
            .into_iter()
            .flat_map(|(n, b, _)| iter_range(&self.buf, n..).filter_map(raw_from(b)))
            .take_while(|&(b, _)| b <= *after.end());

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

    /// If there is no [`Tag`] in `at`, fuses two skips surrounding
    /// it.
    ///
    /// This method is used to get rid of sequences of 2 skips, in
    /// order to keep the [`GapBuffer`] nice and tidy.
    fn fuse_skips_at(&mut self, at: usize) {
        let (n, skip) = match self.get_skip_at(at) {
            Some((n, b, skip)) if b == at && n > 0 => (n, skip),
            _ => return,
        };

        if let Some(p_skip) = self.buf[n - 1].as_skip() {
            self.buf
                .splice((n - 1)..=n, [TagOrSkip::Skip(p_skip + skip)]);
            self.records.transform((n, at), (1, 0), (0, 0));
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

impl Default for Tags {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for Tags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tags")
            .field("buf", &self.buf)
            .field("ranges", &self.ranges)
            .field("range_min", &self.range_min)
            .finish_non_exhaustive()
    }
}

impl PartialEq for Tags {
    fn eq(&self, other: &Self) -> bool {
        self.buf == other.buf
    }
}
impl Eq for Tags {}

unsafe impl Send for Tags {}
unsafe impl Sync for Tags {}

pub fn iter_range(
    buf: &GapBuffer<TagOrSkip>,
    range: impl RangeBounds<usize>,
) -> impl Iterator<Item = &TagOrSkip> + Clone + '_ {
    let (s0, s1) = buf.as_slices();
    let (start, end) = get_ends(range, buf.len());

    let r0 = start.min(s0.len())..end.min(s0.len());
    let r1 = start.saturating_sub(s0.len())..end.saturating_sub(s0.len());

    s0[r0].iter().chain(s1[r1].iter())
}

pub fn iter_range_rev(
    buf: &GapBuffer<TagOrSkip>,
    range: impl RangeBounds<usize> + std::fmt::Debug,
) -> impl Iterator<Item = &TagOrSkip> + Clone + '_ {
    let (s0, s1) = buf.as_slices();
    let (start, end) = get_ends(range, buf.len());

    let r0 = start.min(s0.len())..end.min(s0.len());
    let r1 = start.saturating_sub(s0.len())..end.saturating_sub(s0.len());

    s1[r1].iter().rev().chain(s0[r0].iter().rev())
}

fn raw_from(mut b: usize) -> impl FnMut(&TagOrSkip) -> Option<(usize, RawTag)> + Clone {
    move |ts| {
        b += ts.len();
        Some(b).zip(ts.as_tag())
    }
}

fn raw_from_rev(mut b: usize) -> impl FnMut(&TagOrSkip) -> Option<(usize, RawTag)> + Clone {
    move |ts| {
        b -= ts.len();
        Some(b).zip(ts.as_tag())
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

pub type FwdTags<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;
pub type RevTags<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;
