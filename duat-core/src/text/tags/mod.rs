use std::{
    self,
    collections::HashMap,
    ops::{Range, RangeBounds},
};

use gapbuf::{GapBuffer, gap_buffer};

pub use self::{
    ids::{Key, Keys, TextId, ToggleId},
    types::{
        RawTag::{self, *},
        Tag,
    },
};
use self::{
    ranges::TagRange::{self, *},
    types::Toggle,
};
use super::{Point, Text, get_ends, records::Records};

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
    texts: HashMap<TextId, Text>,
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

    pub fn insert(&mut self, at: usize, tag: Tag, key: Key) -> Option<ToggleId> {
        let (tag, toggle_id) = tag.to_raw(key, &mut self.texts, &mut self.toggles);

        let (n, b, skip) = self
            .get_skip_at(at)
            .unwrap_or((self.buf.len(), self.len_bytes(), 0));

        // Don't add the tag, if it already exists in that position.
        if b == at
            && iter_range_rev(&self.buf, ..n)
                .map_while(|ts| ts.as_tag())
                .any(|t| t == tag)
        {
            return None;
        }

        let n = if at == b {
            self.buf.insert(n, TagOrSkip::Tag(tag));
            self.records.transform((n, at), (0, 0), (1, 0));
            self.records.insert((n + 1, at));
            n
        } else {
            self.buf.splice(n..=n, [
                TagOrSkip::Skip(at - b),
                TagOrSkip::Tag(tag),
                TagOrSkip::Skip(b + skip - at),
            ]);
            self.records.transform((n, at), (0, 0), (2, 0));
            self.records.insert((n + 2, at));
            n + 1
        };

        if let Some(entry) = find_match_too_close(&self.buf, (n, at, tag), self.range_min) {
            remove_from_ranges(entry, &mut self.ranges);
        } else if tag.is_start() || tag.is_end() {
            add_to_ranges((at, tag), &mut self.ranges, self.range_min);
            deintersect(&mut self.ranges, self.range_min);
            self.cull_small_ranges();
        }

        toggle_id
    }

    pub fn extend(&mut self, mut other: Tags) {
        let last = self.buf.len() - 1;
        if let Some(TagOrSkip::Skip(first)) = other.buf.get(0)
            && let Some(TagOrSkip::Skip(last)) = self.buf.get_mut(last)
        {
            *last += first;
            other.buf.remove(0);
            other.records.transform((0, 0), (1, 0), (0, 0));
        }

        self.buf.extend(other.buf);
        self.texts.extend(other.texts);
        self.toggles.extend(other.toggles);
        self.records.extend(other.records);

        for range in other.ranges {
            if let Some(entry) = range.get_start().zip(Some(range.tag())) {
                add_to_ranges(entry, &mut self.ranges, self.range_min);
            }

            if let Some(entry) = range.get_end().zip(range.tag().inverse()) {
                add_to_ranges(entry, &mut self.ranges, self.range_min);
            }
        }

        self.cull_small_ranges();
    }

    /// Removes all [`Tag`]s associated with a given [`Key`] in the
    /// `pos`.
    pub fn remove_at(&mut self, at: usize, keys: impl Keys) {
        self.remove_at_if(at, |t| keys.clone().contains(t.key()));
    }

    pub fn remove_of(&mut self, keys: impl Keys) {
        let keys = keys.range();
        let b_to_remove: Vec<usize> = iter_range(&self.buf, ..)
            .filter_map(raw_from(0))
            .filter_map(|(i, t)| keys.clone().contains(t.key()).then_some(i))
            .collect();

        for b in b_to_remove {
            self.remove_at(b, keys.clone());
        }
    }

    pub fn remove_at_if(&mut self, at: usize, f: impl Fn(&RawTag) -> bool) {
        let (n, b, skip) = match self.get_skip_at(at) {
            Some((n, b, skip)) if b == at => (n, b, skip),
            None => (self.buf.len(), self.len_bytes(), 0),
            // If `b != n`, we're in the middle of a skip, and nothing
            // is done.
            _ => return,
        };

        let (removed, total): (Vec<(usize, RawTag)>, usize) = {
            let mut total = 0;
            let removed = iter_range_rev(&self.buf, ..n)
                .enumerate()
                .map_while(|(i, ts)| Some(n - (i + 1)).zip(ts.as_tag()))
                .inspect(|_| total += 1)
                .filter(|(_, t)| f(t))
                .collect();

            (removed, total)
        };

        for &(i, tag) in removed.iter() {
            self.buf.remove(i);
            remove_from_ranges((b, tag), &mut self.ranges);
        }

        self.records
            .transform((n, b), (total, 0), (total - removed.len(), 0));

        if let Some(i) = n.checked_sub(removed.len() + 1)
            && skip > 0
            && let Some(TagOrSkip::Skip(prev)) = self.buf.get(i)
        {
            self.buf.splice(i..=(i + 1), [TagOrSkip::Skip(prev + skip)]);
            self.records
                .transform((n - removed.len(), b), (1, 0), (0, 0));
        }

        deintersect(&mut self.ranges, self.range_min);
    }

    pub fn transform(&mut self, old: Range<usize>, new_end: usize) {
        let new = old.start..new_end;

        // In case we're appending to the rope, a shortcut can be made.
        let Some((start_n, start_b, _)) = self.get_skip_at(old.start) else {
            let last = self.buf.len().saturating_sub(1);
            if let Some(TagOrSkip::Skip(skip)) = self.buf.get_mut(last) {
                *skip += new.end - old.start;
                self.records.append((0, new.end - old.start));
            } else if new.end > old.start {
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

        let (removed, added) = {
            let skip = (end_b - start_b).saturating_add_signed(range_diff);
            let replacement = (skip > 0).then_some(TagOrSkip::Skip(skip));

            let removed = self
                .buf
                .splice(start_n..=end_n, replacement)
                .scan(start_b, |p, ts| {
                    *p += ts.len();
                    Some((*p - ts.len(), ts))
                })
                .filter_map(|(p, ts)| ts.as_tag().map(|t| (p, t)));

            (removed, replacement.is_some() as usize)
        };

        for entry in removed {
            remove_from_ranges(entry, &mut self.ranges);
        }

        self.records.transform(
            (start_n, old.start),
            (1 + end_n - start_n, old.clone().count()),
            (added, new.clone().count()),
        );

        shift_ranges_after(&mut self.ranges, old.end, range_diff, self.range_min);
        self.process_ranges_around(new.clone());
        self.cull_small_ranges();
        deintersect(&mut self.ranges, self.range_min);
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

            // If b == at, include the tags before the skip.
            if b == at {
                (n - iter.take_while(|ts| ts.is_tag()).count(), b)
            } else {
                (n, b)
            }
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
                StartConceal(key) => {
                    match self
                        .ranges
                        .iter()
                        .find(|range| range.starts_with(&(b, StartConceal(key))))
                    {
                        Some(t_range) => (b, ConcealUntil(t_range.end() as u32)),
                        None => (b, StartConceal(key)),
                    }
                }
                tag => (b, tag),
            })
        };

        ranges.chain(tags).peekable()
    }

    pub fn rev_iter_at(&self, at: usize) -> RevTags {
        let at = (at + self.range_min).min(self.len_bytes());

        let (n, b) = self
            .get_skip_at(at)
            .map(|(n, b, _)| (n, b))
            .unwrap_or((self.buf.len(), self.len_bytes()));

        let ranges = {
            let mut ranges: Vec<_> = self
                .ranges
                .iter()
                .filter(|&range| range.get_start().map_or(true, |start| start < b))
                .map(|range| (range.end(), range.tag().inverse().unwrap()))
                .filter(|(end, _)| *end > b)
                .collect();

            ranges.sort();
            ranges
        };

        let raw_tags = {
            let iter = iter_range_rev(&self.buf, ..n).filter_map(raw_from_rev(b));
            iter.map(|(b, tag)| match tag {
                EndConceal(key) => {
                    if let Some(range) = self
                        .ranges
                        .iter()
                        .find(|range| range.ends_with(&(b, EndConceal(key))))
                    {
                        (b, ConcealUntil(range.start() as u32))
                    } else {
                        (b, EndConceal(key))
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
                let max_point = self.texts.get(&id).unwrap().len_point();
                Some(p.map_or(max_point, |p| p + max_point))
            }
            _ => p,
        })
    }

    fn process_ranges_around(&mut self, range: Range<usize>) {
        let (b_range, a_range) = {
            let before_start = range.start.saturating_sub(self.range_min);
            // in case `range.start == range.end`, we would be double
            // checking that position, so we shift once to the right.
            let cross_shift = (range.end == range.start) as usize;
            let after_start = (range.end + cross_shift).min(self.len_bytes());
            let after_end = (range.end + self.range_min).min(self.len_bytes());

            (before_start..=range.start, after_start..=after_end)
        };

        let b_tags = {
            let (n, b) = self
                .get_skip_at(*b_range.end())
                .map(|(n, b, _)| (n, b))
                .unwrap_or((self.buf.len(), self.len_bytes()));

            iter_range_rev(&self.buf, ..n)
                .filter_map(raw_from_rev(b))
                .take_while(|&(b, _)| b >= *b_range.start())
        };

        let a_tags = {
            let (mut n, b) = self
                .get_skip_at(*a_range.start())
                .map(|(n, b, _)| (n, b))
                .unwrap_or((self.buf.len(), self.len_bytes()));

            if b == *a_range.start() {
                n -= iter_range_rev(&self.buf, ..n)
                    .take_while(|ts| ts.is_tag())
                    .count();
            }

            iter_range(&self.buf, n..)
                .filter_map(raw_from(b))
                .take_while(|&(b, _)| b <= *a_range.end())
        };

        let mut ranges = Vec::new();
        for entry in b_tags {
            add_to_ranges(entry, &mut ranges, 0);
        }
        ranges.extract_if(|r| matches!(r, Until(..))).for_each(drop);

        for entry in a_tags {
            add_to_ranges(entry, &mut ranges, 0);
        }
        ranges
            .extract_if(|r| matches!(r, From(_, b) if *b >= *a_range.start()))
            .for_each(drop);

        for entry in ranges.iter().flat_map(TagRange::entries).flatten() {
            add_to_ranges(entry, &mut self.ranges, self.range_min)
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
                    if let Bounded(_, bounded) = range {
                        bounded.clone().count() < self.range_min
                    } else {
                        false
                    }
                })
                .count()
        }
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

    pub fn get_text(&self, k: &TextId) -> Option<&Text> {
        self.texts.get(k)
    }
}

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
    range: impl RangeBounds<usize> + std::fmt::Debug + Clone,
) -> impl Iterator<Item = &TagOrSkip> + Clone + '_ {
    let (s0, s1) = buf.as_slices();
    let (start, end) = get_ends(range.clone(), buf.len());

    let r0 = start.min(s0.len())..end.min(s0.len());
    let r1 = start.saturating_sub(s0.len())..end.saturating_sub(s0.len());

    if let (Some(s0), Some(s1)) = (s0.get(r0), s1.get(r1)) {
        s1.iter().rev().chain(s0.iter().rev())
    } else {
        panic!("{buf:#?}, {range:?}");
    }
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

fn add_to_ranges(entry: (usize, RawTag), ranges: &mut Vec<TagRange>, range_min: usize) {
    let t_range = if entry.1.is_start() {
        let (start, s_tag) = entry;
        let n = ranges.iter().position(|r| r.can_or_does_start_with(&entry));
        match n.and_then(|n| ranges.get(n)) {
            Some(&Until(_, until)) => {
                ranges.remove(n.unwrap());
                Bounded(s_tag, start..until)
            }
            Some(Bounded(..)) => return,
            None => From(s_tag, start),
            _ => unreachable!(),
        }
    } else if entry.1.is_end() {
        let (end, e_tag) = entry;
        let n = ranges.iter().rposition(|r| r.can_or_does_end_with(&entry));
        match n.and_then(|n| ranges.get(n)) {
            Some(&From(s_tag, from)) => {
                ranges.remove(n.unwrap());
                Bounded(s_tag, from..end)
            }
            Some(Bounded(..)) => return,
            None => Until(e_tag, end),
            _ => unreachable!(),
        }
    } else {
        return;
    };

    if t_range.count_ge(range_min)
        && let Err(n) = ranges.binary_search(&t_range)
    {
        ranges.insert(n, t_range.clone());
    }
}

/// Removes the given `(usize, RawTag)` entry from any
/// range in `self.ranges`.
///
/// This will either lead to a partially unbounded range, or
/// completely remove it.
///
/// Will return the range as it was, before the removal.
fn remove_from_ranges(entry: (usize, RawTag), ranges: &mut Vec<TagRange>) {
    if entry.1.is_start() {
        let t_range = ranges.extract_if(|range| range.starts_with(&entry)).next();
        if let Some(Bounded(tag, bounded)) = t_range {
            let range = Until(tag.inverse().unwrap(), bounded.end);
            let (Ok(n) | Err(n)) = ranges.binary_search(&range);
            ranges.insert(n, range);
        }
    } else if entry.1.is_end() {
        let t_range = ranges.extract_if(|range| range.ends_with(&entry)).next();
        if let Some(Bounded(tag, bounded)) = t_range {
            let range = From(tag, bounded.start);
            let (Ok(n) | Err(n)) = ranges.binary_search(&range);
            ranges.insert(n, range);
        }
    }
}

fn shift_ranges_after(ranges: &mut Vec<TagRange>, after: usize, amount: isize, range_min: usize) {
    ranges
        .extract_if(|t_range| {
            match t_range {
                Bounded(_, bounded) => {
                    if bounded.start > after || (amount < 0 && bounded.start >= after) {
                        let start = bounded.start.saturating_add_signed(amount);
                        let end = bounded.end.saturating_add_signed(amount);
                        *bounded = start..end
                    } else if bounded.end > after || (amount < 0 && bounded.end >= after) {
                        bounded.end = bounded.end.saturating_add_signed(amount)
                    }
                }
                From(_, from) => {
                    if *from > after || (amount < 0 && *from >= after) {
                        *from = from.saturating_add_signed(amount)
                    }
                }
                Until(_, until) => {
                    if *until > after || (amount < 0 && *until >= after) {
                        *until = until.saturating_add_signed(amount)
                    }
                }
            }

            !t_range.count_ge(range_min)
        })
        .for_each(drop);
}

fn deintersect(ranges: &mut Vec<TagRange>, range_min: usize) {
    let mut entries: Vec<(usize, RawTag)> = Vec::new();

    for t_range in ranges.drain(..) {
        for (b, tag) in t_range.entries().into_iter().flatten() {
            let (Ok(n) | Err(n)) =
                entries.binary_search_by_key(&(b, tag.is_end()), |&(b, tag)| (b, tag.is_end()));
            entries.insert(n, (b, tag));
        }
    }

    for entry in entries {
        add_to_ranges(entry, ranges, range_min);
    }
}

fn find_match_too_close(
    buf: &GapBuffer<TagOrSkip>,
    (n, b, tag): (usize, usize, RawTag),
    range_min: usize,
) -> Option<(usize, RawTag)> {
    if tag.is_start() {
        let n = n - iter_range_rev(buf, ..n).take_while(|t| t.is_tag()).count();
        iter_range(buf, n..)
            .filter_map(raw_from(b))
            .take_while(|(cmp, _)| *cmp < b + range_min)
            .find(|(_, t)| *t == tag.inverse().unwrap())
    } else if tag.is_end() {
        iter_range_rev(buf, ..n)
            .filter_map(raw_from_rev(b))
            .take_while(|(cmp, _)| *cmp > b.saturating_sub(range_min))
            .find(|(_, t)| *t == tag.inverse().unwrap())
    } else {
        None
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
            .field("records", &self.records)
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

pub type FwdTags<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;
pub type RevTags<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;
