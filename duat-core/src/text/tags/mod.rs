//! Internal struct for holding [`Tag`]s
//!
//! [`Tag`]s are held internally as [`RawTag`]s, which occupy much
//! less space and can be very cheaply copied around. The [`Tags`]
//! struct also makes use of [`Records`] to keep track of positions,
//! as well as [`TagRange`]s to keep track of tags occupying very long
//! ranges of [`Text`].
mod ids;
mod types;

use std::{
    self,
    collections::HashMap,
    ops::{Range, RangeBounds},
};

use gapbuf::{GapBuffer, gap_buffer};

use self::types::Toggle;
pub use self::{
    ids::{Key, Keys, TextId, ToggleId},
    types::{
        RawTag::{self, *},
        Tag,
    },
};
use super::{Point, Text, get_ends, records::Records};

/// How many characters to keep a [`TagRange`]
const MIN_CHARS_TO_KEEP: u32 = 75;
/// How many [`TagRange`]s until their minimum size is increased
const LIMIT_TO_BUMP: usize = 500;
/// How much to increase that amount when enough [`TagRange`]s exist
const BUMP_AMOUNT: u32 = 75;

/// The struct that holds the [`RawTag`]s of the [`Text`]
///
/// It also holds the [`Text`]s of any [`GhostText`]s, and the
/// functions of [`ToggleStart`]s
#[derive(Clone)]
pub struct Tags {
    buf: GapBuffer<TagOrSkip>,
    texts: HashMap<TextId, Text>,
    toggles: HashMap<ToggleId, Toggle>,
    range_min: u32,
    ranges: Vec<(u32, RawTag)>,
    records: Records<(u32, u32)>,
}

impl Tags {
    /// Creates a new [`Tags`]
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

    /// Creates a new [`Tags`] with a given len
    pub fn with_len(len: u32) -> Self {
        Self {
            buf: gap_buffer![TagOrSkip::Skip(len)],
            ranges: Vec::new(),
            texts: HashMap::new(),
            toggles: HashMap::new(),
            range_min: MIN_CHARS_TO_KEEP,
            records: Records::with_max((1, len)),
        }
    }

    /// Removes all [`RawTag`]s and sets the len to 0
    pub fn clear(&mut self) {
        self.buf = GapBuffer::new();
        self.texts.clear();
        self.toggles.clear();
        self.records.clear();
    }

    /// Insert a new [`Tag`] at a given byte
    pub fn insert(&mut self, at: u32, tag: Tag, key: Key) -> Option<ToggleId> {
        let (tag, toggle) = tag.to_raw(key, &mut self.texts, &mut self.toggles);

        let (n, b, skip) =
            self.get_skip_at(at)
                .unwrap_or((self.buf.len() as u32, self.len_bytes(), 0));
        self.insert_inner(at, tag, (n, b, skip));

        toggle
    }

    // pub fn insert_many(&mut self, tags: impl Iterator<Item = (u32, Tag,
    // Key)>) {     let mut tags = tags;
    //     let Some((mut old_at, tag, key)) = tags.next() else {
    //         return;
    //     };
    //     let (mut n, mut b, mut skip) =
    //         self.get_skip_at(old_at)
    //             .unwrap_or((self.buf.len() as u32, self.len_bytes(),
    // 0));

    //     let (tag, _) = tag.to_raw(key, &mut self.texts, &mut
    // self.toggles);     self.insert_inner(old_at, tag, (n, b,
    // skip));

    //     for (at, tag, key) in tags {
    //         if at == old_at {}
    //     }
    // }

    fn insert_inner(&mut self, at: u32, tag: RawTag, (n, b, skip): (u32, u32, u32)) {
        // Don't add the tag, if it already exists in that position.
        if b == at
            && rev_range(&self.buf, ..n)
                .map_while(|(_, ts)| ts.as_tag())
                .any(|t| t == tag)
        {
            return;
        }

        let n = if at == b {
            self.buf.insert(n as usize, TagOrSkip::Tag(tag));
            self.records.transform((n, at), (0, 0), (1, 0));
            self.records.insert((n, at));
            n
        } else {
            self.buf.splice(n as usize..=n as usize, [
                TagOrSkip::Skip(at - b),
                TagOrSkip::Tag(tag),
                TagOrSkip::Skip(b + skip - at),
            ]);
            self.records.transform((n, at), (0, 0), (2, 0));
            self.records.insert((n + 1, at));
            n + 1
        };

        if tag.is_start() || tag.is_end() {
            self.add_to_ranges((n, at, tag));
            self.cull_small_ranges();
        }
    }

    /// Extends this [`Tags`] with another one
    pub fn extend(&mut self, mut other: Tags) {
        let len = self.buf.len();
        let init_skip = if let Some(TagOrSkip::Skip(first)) = other.buf.get(0)
            && let Some(TagOrSkip::Skip(last)) = self.buf.get_mut(len - 1)
        {
            let first = *first;
            *last += first;
            other.buf.remove(0);
            other.records.transform((0, 0), (1, 0), (0, 0));
            first
        } else {
            0
        };

        self.buf.extend(&other.buf);
        self.texts.extend(other.texts);
        self.toggles.extend(other.toggles);

        let mut b = self.len_bytes() + init_skip;
        for (n, &ts) in fwd_range(&other.buf, ..) {
            let n = (n + len) as u32;
            match ts {
                TagOrSkip::Tag(tag) => {
                    if let Some((_, b, tag)) =
                        find_match_too_close(&self.buf, (n, b, tag), self.range_min)
                        && let Ok(i) = self.ranges.binary_search(&(b, tag))
                    {
                        self.ranges.remove(i);
                    } else if tag.is_start() || tag.is_end() {
                        let (Ok(i) | Err(i)) = self.ranges.binary_search(&(b, tag));
                        self.ranges.insert(i, (b, tag));
                    }
                }
                TagOrSkip::Skip(skip) => b += skip,
            }
        }

        self.records.extend(other.records);
    }

    /// Removes all [`RawTag`]s of a given [`Keys`] in a given byte
    pub fn remove_at(&mut self, at: u32, keys: impl Keys) {
        self.remove_at_if(at, |t| keys.clone().contains(t.key()));
    }

    /// Removes all [`RawTag`]s of a give [`Keys`]
    pub fn remove_from(&mut self, range: impl RangeBounds<u32>, keys: impl Keys) {
        let (start, end) = get_ends(range, self.len_bytes());
        let Some((n, b, _)) = self.get_skip_behind(start) else {
            return;
        };

        let entries: Vec<_> = fwd_range(&self.buf, n..)
            .filter_map(entries_fwd(b))
            .take_while(|(_, b, _)| *b < end)
            .inspect(|(_, b, t)| crate::log_file!("{b}, {t:?}"))
            .collect();

        for (_, b, _) in entries {
            self.remove_at(b, keys.clone());
        }
    }

    /// Removes [`RawTag`]s given a predicate
    pub fn remove_at_if(&mut self, at: u32, f: impl Fn(&RawTag) -> bool) {
        let (n, b, skip) = match self.get_skip_at(at) {
            Some((n, b, skip)) if b == at => (n, b, skip),
            None => (self.buf.len() as u32, self.len_bytes(), 0),
            // If `b != n`, we're in the middle of a skip, and nothing
            // is done.
            _ => return,
        };

        let (removed, total): (Vec<(u32, RawTag)>, u32) = {
            let mut total = 0;
            let removed = rev_range(&self.buf, ..n)
                .map_while(|(n, ts)| Some(n as u32).zip(ts.as_tag()))
                .inspect(|_| total += 1)
                .filter(|(_, t)| f(t))
                .collect();

            (removed, total as u32)
        };

        if removed.is_empty() {
            return;
        }

        for &(i, tag) in removed.iter() {
            self.buf.remove(i as usize);
            if let Ok(i) = self.ranges.binary_search(&(b, tag)) {
                self.ranges.remove(i);
            }
        }

        self.records
            .transform((n, b), (total, 0), (total - removed.len() as u32, 0));

        // Try to merge this skip with the previous one.
        if let Some(i) = n.checked_sub(removed.len() as u32 + 1)
            && skip > 0
            && let Some(TagOrSkip::Skip(prev)) = self.buf.get(i as usize)
        {
            let range = i as usize..=i as usize + 1;
            self.buf.splice(range, [TagOrSkip::Skip(prev + skip)]);
            self.records
                .transform((n - removed.len() as u32, b), (1, 0), (0, 0));
        }
    }

    /// Transforms a byte range into another byte range
    ///
    /// This will destroy any [`RawTag`]s contained in the original
    /// range.
    pub fn transform(&mut self, old: Range<u32>, new_end: u32) {
        let new = old.start..new_end;
        // In case we're appending to the GapBuffer, a shortcut can be made.
        let Some((n, b, skip)) = self.get_skip_at(old.start) else {
            // Unlike inserting in the middle, appending should not move the tags
            // ahead.
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

        if old.end > old.start {
            self.remove_len(old.start, old.end - old.start, (n, b, skip));
        }
        if new_end > old.start {
            let only_insert = old.start == old.end;
            self.insert_len(old.start, new_end - old.start, (n, b, skip), only_insert);
        }

        ////////// Range management
        let range_diff = new.end as i32 - old.end as i32;
        for (b, _) in self.ranges.iter_mut() {
            // MAYBE CHANGE TO *B >= OLD.END
            if *b > old.end || (range_diff < 0 && *b >= old.end) {
                *b = b.saturating_add_signed(range_diff)
            }
        }
        self.process_ranges_around(new.clone(), range_diff);
        self.cull_small_ranges();
    }

    fn insert_len(&mut self, at: u32, len: u32, (n, b, skip): (u32, u32, u32), only_insert: bool) {
        // If a == b, we change the length before the tags
        // If I am inserting and removing, then only the skip of the modified
        // range should be altered.
        if only_insert && at == b && n != 0 {
            let prev_entry =
                rev_range(&self.buf, ..n).find_map(|(n, ts)| Some(n).zip(ts.as_skip()));
            if let Some((prev_n, prev_skip)) = prev_entry {
                self.buf[prev_n] = TagOrSkip::Skip(prev_skip + len);
                let start = (prev_n as u32, b - prev_skip);
                self.records.transform(start, (0, 0), (0, len))
            } else {
                self.buf.push_front(TagOrSkip::Skip(len));
                self.records.transform((0, 0), (0, 0), (1, len))
            }
        // Otherwise, just resize
        } else {
            self.buf[n as usize] = TagOrSkip::Skip(skip + len);
            self.records.transform((n, b), (0, 0), (0, len));
        }
    }

    fn remove_len(&mut self, at: u32, len: u32, (s_n, s_b, skip): (u32, u32, u32)) {
        let (e_n, e_b) = if at + len <= s_b + skip {
            (s_n, s_b + skip)
        } else {
            self.get_skip_behind(at + len)
                .map(|(n, b, skip)| (n, b + skip))
                .unwrap()
        };

        let (removed, added) = {
            let skip = e_b - s_b - len;
            let replacement = (skip > 0).then_some(TagOrSkip::Skip(skip));

            let range = s_n as usize..=e_n as usize;
            let removed = self
                .buf
                .splice(range, replacement)
                .scan(s_b, |p, ts| {
                    *p += ts.len();
                    Some((*p - ts.len(), ts))
                })
                .filter_map(|(p, ts)| ts.as_tag().map(|t| (p, t)));

            (removed, replacement.is_some() as u32)
        };

        self.records
            .transform((s_n, s_b), (1 + e_n - s_n, len), (added, 0));

        for entry in removed {
            if let Ok(i) = self.ranges.binary_search(&entry) {
                self.ranges.remove(i);
            }
        }
    }

    /// Returns true if there are no [`RawTag`]s
    pub fn is_empty(&self) -> bool {
        self.buf.len() == 0
    }

    /// Returns the len of the [`Tags`] in bytes
    pub fn len_bytes(&self) -> u32 {
        self.records.max().1
    }

    /// Returns a forward iterator at a given byte
    pub fn fwd_at(&self, at: u32) -> FwdTags {
        let at = at.min(self.len_bytes()).saturating_sub(self.range_min);

        let (n, b) = {
            let (n, b, _) = self.get_skip_at(at).unwrap_or_default();
            let iter = rev_range(&self.buf, ..n);

            // If b == at, include the tags before the skip.
            if b == at {
                (n - iter.take_while(|(_, ts)| ts.is_tag()).count() as u32, b)
            } else {
                (n, b)
            }
        };

        let prev_fwd = {
            let mut prev_fwd = Vec::new();
            for (b, tag) in self.ranges.iter().take_while(|(b, _)| *b < at) {
                if tag.is_start() {
                    prev_fwd.push((*b, *tag))
                } else if tag.is_end()
                    && let Some(i) = prev_fwd.iter().rposition(|(_, t)| t.ends_with(tag))
                {
                    prev_fwd.remove(i);
                }
            }
            prev_fwd
        };

        let tags = {
            let iter = fwd_range(&self.buf, n..).filter_map(entries_fwd(b));
            iter.map(|(_, b, tag)| match tag {
                StartConceal(key) => {
                    if let Ok(i) = self.ranges.binary_search(&(b, StartConceal(key)))
                        && let Ok(i) =
                            self.ranges[i..].binary_search_by_key(&EndConceal(key), |(_, t)| *t)
                    {
                        let (b, _) = self.ranges[i];
                        (b, ConcealUntil(b))
                    } else {
                        (b, StartConceal(key))
                    }
                }
                tag => (b, tag),
            })
        };

        prev_fwd.into_iter().chain(tags).peekable()
    }

    /// Returns a reverse iterator at a given byte
    pub fn rev_at(&self, at: u32) -> RevTags {
        let at = (at + self.range_min).min(self.len_bytes());

        let (n, b) = self
            .get_skip_at(at)
            .map(|(n, b, _)| (n, b))
            .unwrap_or((self.buf.len() as u32, self.len_bytes()));

        let post_rev = {
            let mut post_rev = Vec::new();
            for (b, tag) in self.ranges.iter().rev().take_while(|(b, _)| *b > at) {
                if tag.is_end() {
                    post_rev.push((*b, *tag))
                } else if tag.is_start()
                    && let Some(i) = post_rev.iter().rposition(|(_, t)| t.ends_with(tag))
                {
                    post_rev.remove(i);
                }
            }
            post_rev
        };

        let tags = {
            let iter = rev_range(&self.buf, ..n).filter_map(entries_rev(b));
            iter.map(|(_, b, tag)| match tag {
                EndConceal(key) => {
                    if let Ok(i) = self.ranges.binary_search(&(b, EndConceal(key)))
                        && let Ok(i) =
                            self.ranges[..i].binary_search_by_key(&StartConceal(key), |(_, t)| *t)
                    {
                        let (b, _) = self.ranges[i];
                        (b, ConcealUntil(b))
                    } else {
                        (b, EndConceal(key))
                    }
                }
                tag => (b, tag),
            })
        };

        post_rev.into_iter().rev().chain(tags).peekable()
    }

    /// Returns an iterator over a single byte
    pub fn iter_only_at(&self, at: u32) -> impl Iterator<Item = RawTag> + '_ {
        let (n, b) = self
            .get_skip_at(at)
            .map(|(n, b, _)| (n, b))
            .unwrap_or((self.buf.len() as u32, self.len_bytes()));

        (b == at)
            .then(|| rev_range(&self.buf, ..n).map_while(|(_, ts)| ts.as_tag()))
            .into_iter()
            .flatten()
    }

    /// Returns the length of all [`GhostText`]s in a byte
    pub fn ghosts_total_at(&self, at: u32) -> Option<Point> {
        self.iter_only_at(at).fold(None, |p, tag| match tag {
            RawTag::GhostText(_, id) => {
                let max_point = self.texts.get(&id).unwrap().len();
                Some(p.map_or(max_point, |p| p + max_point))
            }
            _ => p,
        })
    }

    /// Given a range, add or remove [`TagRange`]s around it
    fn process_ranges_around(&mut self, range: Range<u32>, range_diff: i32) {
        let (b_range, a_range) = {
            let before_start = range.start.saturating_sub(self.range_min);
            // in case `range.start == range.end`, we would be double
            // checking that position, so we shift once to the right.
            let cross_shift = (range.end == range.start) as u32;
            let after_start = (range.end + cross_shift).min(self.len_bytes());
            let after_end = (range.end + self.range_min).min(self.len_bytes());

            (before_start..=range.start, after_start..=after_end)
        };

        let b_tags = {
            let (n, b) = self
                .get_skip_at(*b_range.start())
                .map(|(n, b, _)| (n, b))
                .unwrap_or((self.buf.len() as u32, self.len_bytes()));

            fwd_range(&self.buf, n..)
                .filter_map(entries_fwd(b))
                .take_while(|(_, b, _)| *b <= *b_range.end())
        };

        let a_tags = {
            let (mut n, b) = self
                .get_skip_at(*a_range.start())
                .map(|(n, b, _)| (n, b))
                .unwrap_or((self.buf.len() as u32, self.len_bytes()));

            if b == *a_range.start() {
                n -= rev_range(&self.buf, ..n)
                    .take_while(|(_, ts)| ts.is_tag())
                    .count() as u32;
            }

            fwd_range(&self.buf, n..)
                .filter_map(entries_fwd(b))
                .take_while(|&(_, b, _)| b <= *a_range.end())
        };

        // This creates a list of entries whose range >= self.range_min, by
        // having only one bound or being bounded before and after the range,
        // sufficiently apart.
        let mut entries = Vec::new();
        for (n, b, tag) in b_tags.chain(a_tags) {
            if let Some((_, b1, t1)) = find_match_too_close(&self.buf, (n, b, tag), self.range_min)
                && let Ok(i) = entries.binary_search(&(b1, t1))
            {
                entries.remove(i);
            } else if tag.is_start() || tag.is_end() {
                let (Ok(i) | Err(i)) = entries.binary_search(&(b, tag));
                entries.insert(i, (b, tag));
            }
        }

        // Then, I remove all entries that are not bounded, as well as entries
        // whose ranges were already long enough before the transformation.
        let mut to_remove = Vec::new();
        for (i, (b0, tag)) in entries.iter().enumerate() {
            let matched_bound = if tag.is_start() {
                entries[(i + 1)..].iter().find_map(find_bound_fn(*tag))
            } else {
                entries[..i].iter().rev().find_map(find_bound_fn(*tag))
            };
            if let Some((b1, _)) = matched_bound {
                // In this case, the range in question was already large before the
                // transformation, so we need not add its entries.
                if b0.abs_diff(b1) as i32 > self.range_min as i32 + range_diff {
                    // The other bound will eventually be removed in the iteration.
                    to_remove.push(i);
                }
            } else {
                // In this case, no bound was added, so this range was already there.
                to_remove.push(i)
            }
        }
        for i in to_remove.into_iter().rev() {
            entries.remove(i);
        }

        for (b, tag) in entries {
            let (Ok(i) | Err(i)) = self.ranges.binary_search(&(b, tag));
            self.ranges.insert(i, (b, tag));
        }
    }

    /// Remove all tag ranges that are too small
    fn cull_small_ranges(&mut self) {
        let mut to_cull = Vec::new();
        loop {
            let iter = self.ranges.iter().enumerate();
            for (n, (b, tag)) in iter.filter(|(_, (_, t))| t.is_start()) {
                let mut find_fn = find_bound_fn(*tag);
                if let Some(shift) = self.ranges[(n + 1)..]
                    .iter()
                    .position(|entry| find_fn(entry).is_some())
                    && self.ranges[n + 1 + shift].0 - b <= self.range_min
                {
                    for i in [n, n + 1 + shift] {
                        let (Ok(i) | Err(i)) = to_cull.binary_search(&i);
                        to_cull.insert(i, i);
                    }
                }
            }
            for n in to_cull.drain(..).rev() {
                self.ranges.remove(n);
            }

            if self.ranges.len() <= LIMIT_TO_BUMP {
                break;
            }
            self.range_min += BUMP_AMOUNT;
        }
    }

    fn add_to_ranges(&mut self, (n, at, tag): (u32, u32, RawTag)) {
        // If the "too close entry" was not in the ranges, that must mean that
        // we have added a redundant tag, but it is easiest to add it
        // to the ranges anyway.
        if let Some((_, b, tag)) = find_match_too_close(&self.buf, (n, at, tag), self.range_min) {
            if let Ok(i) = self.ranges.binary_search(&(b, tag)) {
                self.ranges.remove(i);
            }
        } else if tag.is_start() || tag.is_end() {
            let (Ok(i) | Err(i)) = self.ranges.binary_search(&(at, tag));
            self.ranges.insert(i, (at, tag));
        }
    }

    /// Returns information about the skip in the byte `at`
    ///
    /// * The byte where it starts
    /// * The index in the [`GapBuffer`] where it starts
    /// * Its length
    fn get_skip_at(&self, at: u32) -> Option<(u32, u32, u32)> {
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
            let iter = fwd_range(&self.buf, n..).filter_map(skips);
            iter.map(|(n, this_b, skip)| (n as u32, b + (this_b - skip), skip))
                .take_while(|(_, b, _)| at >= *b)
                .last()
        } else {
            let iter = rev_range(&self.buf, ..n).filter_map(skips);
            iter.map(|(n, this_b, skip)| (n as u32, b - this_b, skip))
                .take_while(|(_, b, skip)| *b + *skip > at)
                .last()
        }
    }

    /// Same as [`get_skip_at`], but takes the previous skip
    ///
    /// This will return the same skip if `b != at`, and the previous
    /// skip otherwise.
    ///
    /// [`get_skip_at`]: Tags::get_skip_at
    fn get_skip_behind(&mut self, at: u32) -> Option<(u32, u32, u32)> {
        let (n, b, skip) = self.get_skip_at(at)?;

        Some(if b == at {
            rev_range(&self.buf, ..n)
                .find_map(|(n, ts)| Some(n as u32).zip(ts.as_skip()))
                .map(|(n, skip)| (n, b - skip, skip))
                .unwrap_or((0, 0, 0))
        } else {
            (n, b, skip)
        })
    }

    /// Return the [`Text`] of a given [`TextId`]
    pub fn get_text(&self, k: &TextId) -> Option<&Text> {
        self.texts.get(k)
    }
}

/// Forward iterator over a range in the [`GapBuffer`]
fn fwd_range(
    buf: &GapBuffer<TagOrSkip>,
    range: impl RangeBounds<u32>,
) -> impl Iterator<Item = (usize, &TagOrSkip)> + Clone + '_ {
    let (s0, s1) = buf.as_slices();
    let (start, end) = get_ends(range, buf.len() as u32);
    let (start, end) = (start as usize, end as usize);

    let r0 = start.min(s0.len())..end.min(s0.len());
    let r1 = start.saturating_sub(s0.len())..end.saturating_sub(s0.len());

    s0[r0]
        .iter()
        .chain(s1[r1].iter())
        .enumerate()
        .map(move |(n, ts)| (n + start, ts))
}

/// Reverse iterator over a range in the [`GapBuffer`]
fn rev_range(
    buf: &GapBuffer<TagOrSkip>,
    range: impl RangeBounds<u32> + std::fmt::Debug + Clone,
) -> impl Iterator<Item = (usize, &TagOrSkip)> + Clone + '_ {
    let (s0, s1) = buf.as_slices();
    let (start, end) = get_ends(range.clone(), buf.len() as u32);
    let (start, end) = (start as usize, end as usize);

    let r0 = start.min(s0.len())..end.min(s0.len());
    let r1 = start.saturating_sub(s0.len())..end.saturating_sub(s0.len());

    if let (Some(s0), Some(s1)) = (s0.get(r0), s1.get(r1)) {
        s1.iter()
            .rev()
            .chain(s0.iter().rev())
            .enumerate()
            .map(move |(n, ts)| (end - (n + 1), ts))
    } else {
        panic!("{buf:#?}, {range:?}");
    }
}

/// Forward enumerating function for a [`TagOrSkip::Tag`] from a byte
fn entries_fwd(
    mut b: u32,
) -> impl FnMut((usize, &TagOrSkip)) -> Option<(u32, u32, RawTag)> + Clone {
    move |(n, ts)| {
        b += ts.len();
        ts.as_tag().map(|t| (n as u32, b, t))
    }
}

/// Reverse enumerating function for a [`TagOrSkip::Tag`] from a byte
fn entries_rev(
    mut b: u32,
) -> impl FnMut((usize, &TagOrSkip)) -> Option<(u32, u32, RawTag)> + Clone {
    move |(n, ts)| {
        b -= ts.len();
        ts.as_tag().map(|t| (n as u32, b, t))
    }
}

/// Look for a match for a [`RawTag`] too close to form a [`TagRange`]
fn find_match_too_close(
    buf: &GapBuffer<TagOrSkip>,
    (n, b, tag): (u32, u32, RawTag),
    range_min: u32,
) -> Option<(u32, u32, RawTag)> {
    let mut bound_fn = find_bound_fn(tag);
    if tag.is_start() {
        let n = n - rev_range(buf, ..n)
            .take_while(|(_, ts)| ts.is_tag())
            .count() as u32;
        fwd_range(buf, n..)
            .filter_map(entries_fwd(b))
            .take_while(|(_, end_b, _)| *end_b <= b + range_min)
            .find_map(|(n, b, t)| bound_fn(&(b, t)).map(|(b, t)| (n, b, t)))
    } else if tag.is_end() {
        rev_range(buf, ..n)
            .filter_map(entries_rev(b))
            .take_while(|(_, start_b, _)| *start_b >= b.saturating_sub(range_min))
            .find_map(|(n, b, t)| bound_fn(&(b, t)).map(|(b, t)| (n, b, t)))
    } else {
        None
    }
}

/// Nests equal [`RawTag`]s until a match is found
fn find_bound_fn(tag: RawTag) -> impl FnMut(&(u32, RawTag)) -> Option<(u32, RawTag)> {
    let mut bounds = 1;
    move |&(b, t): &(u32, RawTag)| {
        bounds -= (t == tag.inverse().unwrap()) as usize;
        bounds += (t == tag) as usize;
        (bounds == 0).then_some((b, t))
    }
}

/// Either a [`RawTag`] or an empty range of bytes
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TagOrSkip {
    Tag(RawTag),
    Skip(u32),
}

impl TagOrSkip {
    /// Returns how many bytes this skips, if any
    pub fn as_skip(&self) -> Option<u32> {
        match self {
            Self::Skip(v) => Some(*v),
            TagOrSkip::Tag(..) => None,
        }
    }

    /// Returns the [`RawTag`] within, if any
    fn as_tag(&self) -> Option<RawTag> {
        match self {
            TagOrSkip::Tag(tag) => Some(*tag),
            TagOrSkip::Skip(_) => None,
        }
    }

    /// Returns `true` if the tag or skip is [`Skip`]
    ///
    /// [`Skip`]: TagOrSkip::Skip
    #[must_use]
    pub fn is_skip(&self) -> bool {
        matches!(self, Self::Skip(..))
    }

    /// Returns `true` if the tag or skip is [`Tag`]
    ///
    /// [`Tag`]: TagOrSkip::Tag
    #[must_use]
    pub fn is_tag(&self) -> bool {
        matches!(self, Self::Tag(..))
    }

    /// Returns the amount skipped, 0 if it is a [`RawTag`]
    #[inline]
    fn len(&self) -> u32 {
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

impl Default for Tags {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Debug for Tags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct TagsDbg<'a>(&'a GapBuffer<TagOrSkip>);
        impl std::fmt::Debug for TagsDbg<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if f.alternate() {
                    let mut b = 0;
                    f.write_str("[\n")?;
                    for (i, ts) in self.0.iter().enumerate() {
                        writeln!(f, "    (n: {i}, b: {b}): {ts:?}")?;
                        b += ts.len();
                    }
                    f.write_str("]")
                } else {
                    write!(f, "{:?}", self.0)
                }
            }
        }

        struct RangesDbg<'a>(&'a [(u32, RawTag)]);
        impl std::fmt::Debug for RangesDbg<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if f.alternate() {
                    f.write_str("[\n")?;
                    for entry in self.0 {
                        writeln!(f, "    {entry:?}")?;
                    }
                    f.write_str("]")
                } else {
                    write!(f, "{:?}", self.0)
                }
            }
        }

        f.debug_struct("Tags")
            .field("buf", &TagsDbg(&self.buf))
            .field("ranges", &RangesDbg(&self.ranges))
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

/// A forward [`Iterator`] of [`RawTag`]s
///
/// This iterator automatically takes into account [`TagRange`]s and
/// iterates their bounds as if they were regular [`RawTag`]s
pub type FwdTags<'a> = std::iter::Peekable<impl Iterator<Item = (u32, RawTag)> + Clone + 'a>;
/// A reverse [`Iterator`] of [`RawTag`]s
///
/// This iterator automatically takes into account [`TagRange`]s and
/// iterates their bounds as if they were regular [`RawTag`]s
pub type RevTags<'a> = std::iter::Peekable<impl Iterator<Item = (u32, RawTag)> + Clone + 'a>;
