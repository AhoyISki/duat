use std::{self, collections::HashMap, ops::Range};

use gapbuf::{gap_buffer, GapBuffer};

pub use self::{
    ids::{Marker, Markers, TextId, ToggleId},
    types::{RawTag, Tag},
};
use self::{ranges::TagRange, types::Toggle};
use super::{records::Records, Point, Text};
use crate::log_info;

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
    buf: GapBuffer<TagOrSkip>,
    pub texts: HashMap<TextId, Text>,
    toggles: HashMap<ToggleId, Toggle>,
    range_min: usize,
    ranges: Vec<TagRange>,
    records: Records<(usize, usize)>,
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
            assert_len!(at, self.len_bytes());
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

        self.merge_surrounding_skips(self.len_bytes());
        rearrange_ranges(&mut self.ranges, self.range_min);
        self.cull_small_ranges();
    }

    /// Removes all [`Tag`]s associated with a given [`Handle`] in the
    /// `pos`.
    pub fn remove_at(&mut self, at: usize, markers: impl Markers) {
        // If we are removing in the middle of a skip, there is
        // nothing to do.
        let Some((n, b, _)) = self.get_skip_at(at).filter(|&(_, b, _)| b == at) else {
            return;
        };

        let removed: Vec<_> = self
            .buf
            .iter()
            .rev()
            .skip(self.buf.len() - n)
            .enumerate()
            .map_while(|(i, ts)| Some(n - (i + 1)).zip(ts.as_tag()))
            .filter(|(_, tag)| markers.clone().contains(tag.marker()))
            .collect();

        self.records.transform((n, b), (removed.len(), 0), (0, 0));

        for (i, tag) in removed {
            self.buf.remove(i);
            remove_from_ranges((i, tag), &mut self.ranges);
        }

        self.merge_surrounding_skips(at);
        rearrange_ranges(&mut self.ranges, self.range_min);
    }

    pub fn transform(&mut self, old: Range<usize>, new_end: usize) {
        let new = old.start..new_end;

        // In case we're appending to the rope, a shortcut can be made.
        let Some((start_n, start_b, _)) = self.get_skip_at(old.start) else {
            assert_len!(old.start, self.len_bytes());

            let last = self.buf.len() - 1;
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
            .unwrap_or((self.buf.len(), self.len_bytes()));

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

        log_info!("transforming ({start_n}, {end_n}, {old:?}) to {new:?}");
        self.records.transform(
            (start_n, old.start),
            (end_n, old.clone().count()),
            (start_n + 1, new.clone().count()),
        );
        self.records.insert((start_n, old.start));

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

        let (n, mut b) = {
            let (n, b, _) = self.get_skip_at(at).unwrap();
            let iter = self.buf.iter().rev().skip(self.buf.len() - n);

            (n - iter.take_while(|ts| ts.is_tag()).count(), b)
        };

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

    pub fn rev_iter_at(&self, at: usize) -> RevTags {
        let at = (at + self.range_min).min(self.len_bytes());
        let (n, mut b) = self
            .get_skip_at(at)
            .map(|(n, b, _)| (n, b))
            .unwrap_or((self.buf.len(), self.len_bytes()));

        let ranges = {
            let mut ranges: Vec<_> = self
                .ranges
                .iter()
                .filter(|&range| range.get_start().map_or(true, |start| start < at))
                .map(|range| {
                    let end = range.get_end().unwrap_or(self.len_bytes());
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

    pub fn iter_only_at(&self, at: usize) -> impl Iterator<Item = RawTag> + '_ {
        let b = self.get_skip_at(at).map(|(_, b, _)| b);

        b.into_iter()
            .flat_map(|b| self.buf.iter().skip(b).map_while(TagOrSkip::as_tag))
    }

    pub fn ghosts_total_at(&self, at: usize) -> Option<Point> {
        self.iter_only_at(at).fold(None, |p, tag| match tag {
            RawTag::GhostText(_, id) => Some(p.map_or(Point::default(), |p| {
                p + self.texts.get(&id).unwrap().max_point()
            })),
            _ => p,
        })
    }

    /// Returns information about the skip in the byte `at`
    ///
    /// * The byte where it starts
    /// * The index in the [`GapBuffer`] where it starts
    /// * Its length
    fn get_skip_at(&self, at: usize) -> Option<(usize, usize, usize)> {
        let (n, mut b) = self.records.closest_to(at);
        log_info!("closest {n}, {b}");

        let skips = |(n, s): (usize, &TagOrSkip)| Some(n).zip(s.as_skip());

        if at >= b {
            let iter = self.buf.iter().enumerate().skip(n);
            let skips = iter.filter_map(skips).take_while(|(_, skip)| {
                if at >= b {
                    b += skip;
                    true
                } else {
                    false
                }
            });

            skips.last().map(|(i, skip)| (i, b - skip, skip))
        } else {
            let (s0, s1) = self.buf.as_slices();
            // Damn you Chain!!!!
            let iter_0 = s0.iter().enumerate().rev();
            let plus_s0_len = |(i, ts)| (i + s0.len(), ts);
            let iter_1 = s1.iter().enumerate().rev().map(plus_s0_len);

            let iter = iter_1.chain(iter_0).skip(self.buf.len() - n);
            let skips = iter.filter_map(skips).take_while(|(_, skip)| {
                if b >= at {
                    b -= skip;
                    true
                } else {
                    false
                }
            });

            skips.last().map(|(i, skip)| (i, b, skip))
        }
    }

    fn process_ranges_containing(&mut self, range: Range<usize>) {
        let search = {
            let start = range.start.saturating_sub(self.range_min);
            // in case `range.start == range.end`, we would be double
            // checking that position, so we shift once to the right.
            let intersection_shift = (range.end == range.start) as usize;
            let end = (range.end + self.range_min + intersection_shift).min(self.len_bytes());

            start..end
        };

        let before = {
            let (n, mut b, _) = self.get_skip_at(search.start).unwrap();

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

            after.into_iter().flat_map(|(n, mut b, _)| {
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
        let (n, b) = match self.get_skip_at(at) {
            Some((n, b, _)) => (n, b),
            None => {
                assert_len!(at, self.len_bytes());
                (self.buf.len(), self.len_bytes())
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
            .field("buf", &self.buf)
            .field("ranges", &self.ranges)
            .field("range_min", &self.range_min)
            .finish_non_exhaustive()
    }
}

unsafe impl Send for Tags {}
unsafe impl Sync for Tags {}

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

macro assert_len($at:expr, $len:expr) {
    let (at, len) = ($at, $len);
    assert!(
        at == len,
        "byte out of bounds: the len is {len}, but the byte is {at}"
    );
}
