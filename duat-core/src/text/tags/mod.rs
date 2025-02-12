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
use super::{Point, Text, records::Records};

/// How many characters to keep a [`TagRange`]
const MIN_CHARS_TO_KEEP: usize = 75;
/// How many [`TagRange`]s until their minimum size is increased
const LIMIT_TO_BUMP: usize = 500;
/// How much to increase that amount when enough [`TagRange`]s exist
const BUMP_AMOUNT: usize = 75;

/// The struct that holds the [`RawTag`]s of the [`Text`]
///
/// It also holds the [`Text`]s of any [`GhostText`]s, and the
/// functions of [`ToggleStart`]s
#[derive(Clone)]
pub struct Tags {
    buf: GapBuffer<TagOrSkip>,
    texts: HashMap<TextId, Text>,
    toggles: HashMap<ToggleId, Toggle>,
    range_min: usize,
    ranges: Vec<(usize, RawTag)>,
    records: Records<[usize; 2]>,
    ranges_to_update: Vec<Range<usize>>,
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
            ranges_to_update: Vec::new(),
        }
    }

    /// Creates a new [`Tags`] with a given len
    pub fn with_len(len: usize) -> Self {
        Self {
            buf: gap_buffer![TagOrSkip::Skip(len as u32)],
            ranges: Vec::new(),
            texts: HashMap::new(),
            toggles: HashMap::new(),
            range_min: MIN_CHARS_TO_KEEP,
            records: Records::with_max([1, len]),
            ranges_to_update: Vec::new(),
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
    pub fn insert(&mut self, at: usize, tag: Tag, key: Key) -> Option<ToggleId> {
        let (tag, toggle) = tag.to_raw(key, &mut self.texts, &mut self.toggles);

        let [n, b, skip] = self
            .get_skip_at(at)
            .unwrap_or([self.buf.len(), self.len_bytes(), 0]);
        self.insert_inner(at, tag, [n, b, skip]);

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

    fn insert_inner(&mut self, at: usize, tag: RawTag, [n, b, skip]: [usize; 3]) {
        // Don't add the tag, if it already exists in that position.
        if b == at
            && rev_range(&self.buf, ..n)
                .map_while(|(_, ts)| ts.as_tag())
                .any(|t| t == tag)
        {
            return;
        }

        let n = if at == b {
            // let n = if !tag.is_end() {
            //    n - rev_range(&self.buf, ..n)
            //        .take_while(|(_, ts)| ts.is_tag())
            //        .count()
            //} else {
            //    n
            //};
            self.buf.insert(n, TagOrSkip::Tag(tag));
            self.records.transform([n, at], [0, 0], [1, 0]);
            self.records.insert([n, at]);
            n
        } else {
            self.buf.splice(n..=n, [
                TagOrSkip::Skip((at - b) as u32),
                TagOrSkip::Tag(tag),
                TagOrSkip::Skip((b + skip - at) as u32),
            ]);
            self.records.transform([n, at], [0, 0], [2, 0]);
            self.records.insert([n + 1, at]);
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
            other.records.transform([0, 0], [1, 0], [0, 0]);
            first as usize
        } else {
            0
        };

        self.buf.extend(&other.buf);
        self.texts.extend(other.texts);
        self.toggles.extend(other.toggles);

        let mut b = self.len_bytes() + init_skip;
        for (n, &ts) in fwd_range(&other.buf, ..) {
            let n = n + len;
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
                TagOrSkip::Skip(skip) => b += skip as usize,
            }
        }

        self.records.extend(other.records);
    }

    /// Removes all [`RawTag`]s of a given [`Keys`] in a given byte
    pub fn remove_at(&mut self, at: usize, keys: impl Keys) {
        self.remove_at_if(at, |t| keys.clone().contains(t.key()));
    }

    /// Removes all [`RawTag`]s of a give [`Keys`]
    pub fn remove_from(&mut self, range: impl RangeBounds<usize>, keys: impl Keys) {
        let (start, end) = crate::get_ends(range, self.len_bytes());
        let Some([n, b, _]) = self.get_skip_behind(start) else {
            return;
        };

        let entries: Vec<_> = fwd_range(&self.buf, n..)
            .filter_map(entries_fwd(b))
            .take_while(|(_, b, _)| *b < end)
            .collect();

        for (_, b, _) in entries {
            self.remove_at(b, keys.clone());
        }
    }

    /// Removes [`RawTag`]s given a predicate
    pub fn remove_at_if(&mut self, at: usize, f: impl Fn(&RawTag) -> bool) {
        let [n, b, skip] = match self.get_skip_at(at) {
            Some([n, b, skip]) if b == at => [n, b, skip],
            None => [self.buf.len(), self.len_bytes(), 0],
            // If `b != n`, we're in the middle of a skip, and nothing
            // is done.
            _ => return,
        };

        let removed: Vec<(usize, RawTag)> = rev_range(&self.buf, ..n)
            .map_while(|(n, ts)| Some(n).zip(ts.as_tag()))
            .filter(|(_, t)| f(t))
            .collect();

        if removed.is_empty() {
            return;
        }

        for &(i, tag) in removed.iter() {
            self.buf.remove(i);
            if tag.is_start() || tag.is_end() {
                if let Ok(i) = self.ranges.binary_search(&(b, tag)) {
                    self.ranges.remove(i);
                }
            }
        }

        self.records.transform([n, b], [removed.len(), 0], [0, 0]);

        // I wish this was not necessary, but I need to add potential matches
        // to the ranges, because this removal may take place outside the
        // screen, where it won't get updated, but I still want to show very
        // long ranges accurately in this situation.
        // This will not _necessarily_ be correct, since it won't look for
        // matches of matches, but it will have the intended effect of adding
        // at least one open range, which would look the same from any point
        // in the text after it.
        // This will later be remedied by updating the ranges.
        let removed: Vec<(usize, RawTag)> = removed.into_iter().map(|(_, t)| (at, t)).collect();
        let n = n - removed.len();
        for entry in pairs_of_removed(&self.buf, &removed, (n, b), at..at, self.range_min) {
            if let Err(i) = self.ranges.binary_search(&entry) {
                self.ranges.insert(i, entry);
            }
        }

        // Try to merge this skip with the previous one.
        if let Some(i) = n.checked_sub(1)
            && skip > 0
            && let Some(TagOrSkip::Skip(prev)) = self.buf.get(i)
        {
            let range = i..=i + 1;
            self.buf
                .splice(range, [TagOrSkip::Skip(prev + skip as u32)]);
            self.records.transform([n, b], [1, 0], [0, 0]);
        }
        let s_update = at.saturating_sub(self.range_min);
        let e_update = at + self.range_min;
        super::merge_range_in(&mut self.ranges_to_update, s_update..e_update);
    }

    /// Transforms a byte range into another byte range
    ///
    /// This will destroy any [`RawTag`]s contained in the original
    /// range.
    pub fn transform(&mut self, old: Range<usize>, new_end: usize) {
        let new = old.start..new_end;
        // In case we're appending to the GapBuffer, a shortcut can be made.
        let Some([s_n, s_b, s_skip]) = self.get_skip_at(old.start) else {
            let last = self.buf.len().saturating_sub(1);
            let new_len = (new.end - old.start) as u32;
            if let Some(TagOrSkip::Skip(skip)) = self.buf.get_mut(last) {
                *skip += new_len;
                self.records.append([0, new.end - old.start]);
            } else if new.end > old.start {
                self.buf.push_back(TagOrSkip::Skip(new_len));
                self.records.append([1, new.end - old.start]);
            }

            return;
        };

        // Old length removal.
        if old.end > old.start {
            let rm_b = old.end - old.start;
            // If the range to be removed is contained within one skip,
            // there is no need to check for where it ends.
            let (e_n, e_b) = if old.start + rm_b <= s_b + s_skip {
                (s_n, s_b + s_skip)
            } else {
                // The check for the final skip is a `get_skip_behind` because we
                // don't want to remove one skip ahead of the end in the cases of
                // `old.start + len == some_skip`, since that would remove the tags at
                // the end of   the range.
                self.get_skip_behind(old.start + rm_b)
                    .map(|[n, b, skip]| (n, b + skip))
                    .unwrap()
            };

            let (rm, added_n, rm_before_n, rm_after_n) = {
                let skip = (e_b - s_b - rm_b) as u32;
                let replacement = (skip > 0).then_some(TagOrSkip::Skip(skip));

                let mut rm_before = Vec::new();
                let mut rm_after = Vec::new();
                let mut rm_pairs_before_n = 0;
                let mut rm_pairs_after_n = 0;

                // If the range collapses into a single byte, get rid of duplicates
                // and pairs.
                if replacement.is_none() && new_end == old.start {
                    let mut dupes_and_pairs: Vec<(usize, usize, RawTag)> = Vec::new();
                    let mut to_remove = Vec::new();
                    for (i, b, t) in rev_range(&self.buf, ..s_n)
                        .map_while(|(i, ts)| ts.as_tag().map(|t| (i, s_b, t)))
                        .chain(
                            fwd_range(&self.buf, (e_n + 1)..)
                                .map_while(|(i, ts)| ts.as_tag().map(|t| (i, e_b, t))),
                        )
                    {
                        if let Some(j) = dupes_and_pairs
                            .iter()
                            .position(|(.., p)| p.ends_with(&t) || t.ends_with(p))
                        {
                            if b == s_b {
                                rm_pairs_before_n += 1;
                                to_remove.insert(0, i);
                            } else {
                                rm_pairs_after_n += 1;
                                to_remove.push(i);
                            }
                            let (i, b, _) = dupes_and_pairs.remove(j);
                            let (Ok(k) | Err(k)) = to_remove.binary_search_by_key(&i, |i| *i);
                            to_remove.insert(k, i);
                            if b == s_b {
                                rm_pairs_before_n += 1;
                            } else {
                                rm_pairs_after_n += 1;
                            }
                        } else if dupes_and_pairs.iter().any(|(.., d)| *d == t) {
                            if b == s_b {
                                rm_before.push((b, t));
                                to_remove.insert(0, i);
                            } else {
                                rm_after.push((b, t));
                                to_remove.push(i);
                            }
                        } else {
                            dupes_and_pairs.push((i, b, t));
                        };
                    }

                    for i in to_remove.into_iter().rev() {
                        self.buf.remove(i);
                    }
                }

                let rm_before_n = rm_before.len() + rm_pairs_before_n;
                let rm_after_n = rm_after.len() + rm_pairs_after_n;
                let range = s_n - rm_before_n..=e_n - rm_before_n;
                let mut rm = rm_before;
                rm.extend(
                    self.buf
                        .splice(range, replacement)
                        .scan(s_b, |b, ts| {
                            *b += ts.len();
                            Some((*b - ts.len(), ts))
                        })
                        .filter_map(|(b, ts)| ts.as_tag().map(|t| (b, t))),
                );
                rm.extend(rm_after);

                (rm, replacement.is_some() as usize, rm_before_n, rm_after_n)
            };

            let rm_n = rm_before_n + rm_after_n + e_n + 1 - s_n;
            let s_n = s_n - rm_before_n;
            self.records
                .transform([s_n, s_b], [rm_n, rm_b], [added_n, 0]);

            for entry in &rm {
                if let Ok(i) = self.ranges.binary_search(entry) {
                    self.ranges.remove(i);
                }
            }
            for entry in pairs_of_removed(&self.buf, &rm, (s_n, s_b), new.clone(), self.range_min) {
                if let Err(i) = self.ranges.binary_search(&entry) {
                    self.ranges.insert(i, entry);
                }
            }
        }

        // New length insertion is straightforward, just add the len, dummy.
        if new_end > old.start {
            let len = new_end - old.start;
            let new_skip = self.buf[s_n].len();
            self.buf[s_n] = TagOrSkip::Skip((new_skip + len) as u32);
            self.records.transform([s_n, s_b], [0, 0], [0, len]);
            // If there is no new len, I'll remove pairs and
            // duplicates from the starting position.
        }

        ////////// Range management
        let range_diff = new.end as isize - old.end as isize;
        for (b, _) in self.ranges.iter_mut() {
            // MAYBE CHANGE TO *B >= OLD.END
            if *b > old.end || (range_diff < 0 && *b >= old.end) {
                *b = b.saturating_add_signed(range_diff)
            }
        }
        let start = new.start.saturating_sub(self.range_min);
        let end = new.end + self.range_min;
        super::merge_range_in(&mut self.ranges_to_update, start..end);
    }

    pub fn update_range(&mut self, within: Range<usize>) {
        let mut new_ranges = Vec::new();
        let ranges = std::mem::take(&mut self.ranges_to_update);

        for range in ranges.into_iter() {
            let (to_check, split_off) = super::split_range_within(range.clone(), within.clone());
            if let Some(range) = to_check {
                let (Ok(s_i) | Err(s_i)) =
                    self.ranges.binary_search_by_key(&range.start, |(b, _)| *b);
                let (Ok(e_i) | Err(e_i)) =
                    self.ranges.binary_search_by_key(&range.end, |(b, _)| *b);
                self.ranges.drain(s_i..e_i);

                let [n, b, _] = self.get_skip_behind(range.start).unwrap_or([0, 0, 0]);

                for entry in ranges_in(&self.buf, (n, b), range, self.range_min) {
                    if let Err(i) = self.ranges.binary_search(&entry) {
                        self.ranges.insert(i, entry);
                    }
                }
            }
            new_ranges.extend(split_off.into_iter().flatten());
        }

        self.ranges_to_update = new_ranges;

        self.cull_small_ranges();
    }

    /// Returns true if there are no [`RawTag`]s
    pub fn is_empty(&self) -> bool {
        self.buf.len() == 0
    }

    /// Returns the len of the [`Tags`] in bytes
    pub fn len_bytes(&self) -> usize {
        self.records.max()[1]
    }

    /// Returns a forward iterator at a given byte
    pub fn fwd_at(&self, at: usize) -> FwdTags {
        let at = at.min(self.len_bytes()).saturating_sub(self.range_min);

        let (n, b) = {
            let [n, b, _] = self.get_skip_at(at).unwrap_or_default();
            let iter = rev_range(&self.buf, ..n);

            // If b == at, include the tags before the skip.
            if b == at {
                (n - iter.take_while(|(_, ts)| ts.is_tag()).count(), b)
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
                        (b, ConcealUntil(b as u32))
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
    pub fn rev_at(&self, at: usize) -> RevTags {
        let at = (at + self.range_min).min(self.len_bytes());

        let (n, b) = self
            .get_skip_at(at)
            .map(|[n, b, _]| (n, b))
            .unwrap_or((self.buf.len(), self.len_bytes()));

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
                        (b, ConcealUntil(b as u32))
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
    pub fn iter_only_at(&self, at: usize) -> impl Iterator<Item = RawTag> + '_ {
        let (n, b) = self
            .get_skip_at(at)
            .map(|[n, b, _]| (n, b))
            .unwrap_or((self.buf.len(), self.len_bytes()));

        (b == at)
            .then(|| rev_range(&self.buf, ..n).map_while(|(_, ts)| ts.as_tag()))
            .into_iter()
            .flatten()
    }

    /// Returns the length of all [`GhostText`]s in a byte
    pub fn ghosts_total_at(&self, at: usize) -> Option<Point> {
        self.iter_only_at(at).fold(None, |p, tag| match tag {
            RawTag::GhostText(_, id) => {
                let max_point = self.texts.get(&id).unwrap().len();
                Some(p.map_or(max_point, |p| p + max_point))
            }
            _ => p,
        })
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
                    && self.ranges[n + 1 + shift].0 - b < self.range_min
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

            if self.ranges.len() < LIMIT_TO_BUMP {
                break;
            }
            self.range_min += BUMP_AMOUNT;
        }
    }

    fn add_to_ranges(&mut self, (n, at, tag): (usize, usize, RawTag)) {
        // If the "too close entry" was not in the ranges, that must mean that
        // we have added a redundant tag, but it is easiest to add it
        // to the ranges anyway.
        if let Some((_, b, tag)) = find_match_too_close(&self.buf, (n, at, tag), self.range_min) {
            if let Ok(i) = self.ranges.binary_search(&(b, tag)) {
                self.ranges.remove(i);
            }
        } else if (tag.is_start() || tag.is_end())
            && let Err(i) = self.ranges.binary_search(&(at, tag))
        {
            self.ranges.insert(i, (at, tag));
        }
    }

    /// Returns information about the skip in the byte `at`
    ///
    /// * The byte where it starts
    /// * The index in the [`GapBuffer`] where it starts
    /// * Its length
    fn get_skip_at(&self, at: usize) -> Option<[usize; 3]> {
        assert!(
            at <= self.len_bytes(),
            "byte out of bounds: the len is {}, but the byte is {at}",
            self.len_bytes()
        );
        let [n, b] = self.records.closest_to(at);
        let skips = {
            let mut b_len = 0;
            move |(n, ts): (usize, &TagOrSkip)| {
                b_len += ts.len();
                ts.as_skip().map(|s| [n, b_len, s])
            }
        };

        if at >= b {
            let iter = fwd_range(&self.buf, n..).filter_map(skips);
            iter.map(|[n, this_b, skip]| [n, b + (this_b - skip), skip])
                .take_while(|[_, b, _]| at >= *b)
                .last()
        } else {
            let iter = rev_range(&self.buf, ..n).filter_map(skips);
            iter.map(|[n, this_b, skip]| [n, b - this_b, skip])
                .take_while(|[_, b, skip]| *b + *skip > at)
                .last()
        }
    }

    /// Same as [`get_skip_at`], but takes the previous skip
    ///
    /// This will return the same skip if `b != at`, and the previous
    /// skip otherwise.
    ///
    /// [`get_skip_at`]: Tags::get_skip_at
    fn get_skip_behind(&mut self, at: usize) -> Option<[usize; 3]> {
        let [n, b, skip] = self.get_skip_at(at).unwrap();
        Some(if b == at {
            rev_range(&self.buf, ..n)
                .find_map(|(n, ts)| Some(n).zip(ts.as_skip()))
                .map(|(n, skip)| [n, b - skip, skip])
                .unwrap_or([0, 0, 0])
        } else {
            [n, b, skip]
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
    range: impl RangeBounds<usize> + std::fmt::Debug,
) -> impl Iterator<Item = (usize, &TagOrSkip)> + Clone + '_ {
    let (s0, s1) = buf.as_slices();
    let (start, end) = crate::get_ends(range, buf.len());

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
    range: impl RangeBounds<usize> + std::fmt::Debug + Clone,
) -> impl Iterator<Item = (usize, &TagOrSkip)> + Clone + '_ {
    let (s0, s1) = buf.as_slices();
    let (start, end) = crate::get_ends(range.clone(), buf.len());

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
///
/// This function will iterate over the positional index, byte index
/// and [`RawTag`], respectively, assuming an initial byte.
fn entries_fwd(
    mut b: usize,
) -> impl FnMut((usize, &TagOrSkip)) -> Option<(usize, usize, RawTag)> + Clone {
    move |(n, ts)| {
        b += ts.len();
        ts.as_tag().map(|t| (n, b, t))
    }
}

/// Reverse enumerating function for a [`TagOrSkip::Tag`] from a byte
fn entries_rev(
    mut b: usize,
) -> impl FnMut((usize, &TagOrSkip)) -> Option<(usize, usize, RawTag)> + Clone {
    move |(n, ts)| {
        b -= ts.len();
        ts.as_tag().map(|t| (n, b, t))
    }
}

/// Look for a match for a [`RawTag`] too close to form a [`TagRange`]
fn find_match_too_close(
    buf: &GapBuffer<TagOrSkip>,
    (n, b, tag): (usize, usize, RawTag),
    range_min: usize,
) -> Option<(usize, usize, RawTag)> {
    let mut bound_fn = find_bound_fn(tag);
    if tag.is_start() {
        let n = n - rev_range(buf, ..n)
            .take_while(|(_, ts)| ts.is_tag())
            .count();
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
fn find_bound_fn(tag: RawTag) -> impl FnMut(&(usize, RawTag)) -> Option<(usize, RawTag)> {
    let mut bounds = 1;
    move |&(b, t): &(usize, RawTag)| {
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
    pub fn as_skip(&self) -> Option<usize> {
        match self {
            Self::Skip(v) => Some(*v as usize),
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
    fn len(&self) -> usize {
        match self {
            TagOrSkip::Tag(_) => 0,
            TagOrSkip::Skip(skip) => *skip as usize,
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

        struct RangesDbg<'a>(&'a [(usize, RawTag)]);
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
            //.field("buf", &TagsDbg(&self.buf))
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
pub type FwdTags<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;
/// A reverse [`Iterator`] of [`RawTag`]s
///
/// This iterator automatically takes into account [`TagRange`]s and
/// iterates their bounds as if they were regular [`RawTag`]s
pub type RevTags<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;

fn ranges_in(
    buf: &GapBuffer<TagOrSkip>,
    (skip_n, skip_b): (usize, usize),
    range: Range<usize>,
    min: usize,
) -> Vec<(usize, RawTag)> {
    let mut entries = Vec::new();

    // Add all tags in the range to the list, keeping only those whose
    // ranges are too long.
    for (_, b, tag) in fwd_range(buf, skip_n..)
        .filter_map(entries_fwd(skip_b))
        .skip_while(|(_, b, _)| *b < range.start)
        .take_while(|(_, b, _)| *b < range.end)
    {
        if tag.is_start() {
            entries.push((b, tag));
        } else if !tag.is_end() {
            continue;
        } else if let Some(rev_i) = entries
            .iter()
            .rev()
            .take_while(|(s_b, _)| b < s_b + min)
            .position(|(_, s_t)| s_t.ends_with(&tag))
        {
            let match_i = entries.len() - rev_i - 1;
            entries.remove(match_i);
        } else {
            entries.push((b, tag));
        }
    }

    // From the previous loop, if an entry is still in the list, it will
    // be for one of two reasons:
    // - Its match is within the range, but still too far away.
    // - Its match is either outside of the range, or nonexistant.
    // We only need to care about the second scenario, and we will look
    // behind or ahead of the range for any tags that could match it.
    // Since the surrounding ranges will never be greater than `min`, I
    // don't need to check if a range is big enough to be included in
    // `entries_before`
    if entries
        .iter()
        .take_while(|(b, _)| *b < range.start + min)
        .any(|(_, t)| t.is_end())
    {
        let mut end_entries = Vec::new();
        for (_, b, tag) in rev_range(buf, ..skip_n)
            .filter_map(entries_rev(skip_b))
            .skip_while(|(_, b, _)| *b >= range.start)
            .take_while(|(_, b, _)| *b + min >= range.start)
        {
            if tag.is_end() {
                end_entries.push((b, tag))
            } else if !tag.is_start() {
                continue;
            } else if let Some(match_i) =
                end_entries.iter().rposition(|(_, e_t)| tag.ends_with(e_t))
            {
                end_entries.remove(match_i);
            } else if let Some(match_i) = entries
                .iter()
                .take_while(|(e_b, _)| *e_b <= b + min)
                .position(|(_, e_t)| tag.ends_with(e_t))
            {
                entries.remove(match_i);
            }
        }
    }
    if entries
        .iter()
        .rev()
        .take_while(|(b, _)| *b + min > range.end)
        .any(|(_, t)| t.is_start())
    {
        let mut start_entries = Vec::new();
        for (_, b, tag) in fwd_range(buf, skip_n..)
            .filter_map(entries_fwd(skip_b))
            .skip_while(|(_, b, _)| *b < range.end)
            .take_while(|(_, b, _)| *b < range.end + min)
        {
            if tag.is_start() {
                start_entries.push((b, tag))
            } else if !tag.is_end() {
                continue;
            } else if let Some(match_i) = start_entries
                .iter()
                .rposition(|(_, s_t)| s_t.ends_with(&tag))
            {
                start_entries.remove(match_i);
            } else if let Some(rev_i) = entries
                .iter()
                .rev()
                .take_while(|(s_b, _)| b <= s_b + min)
                .position(|(_, s_t)| s_t.ends_with(&tag))
            {
                let match_i = entries.len() - rev_i - 1;
                entries.remove(match_i);
            }
        }
    }

    entries
}

fn pairs_of_removed(
    buf: &GapBuffer<TagOrSkip>,
    removed: &[(usize, RawTag)],
    (skip_n, skip_b): (usize, usize),
    range: Range<usize>,
    min: usize,
) -> Vec<(usize, RawTag)> {
    let mut entries = Vec::new();
    if removed
        .iter()
        .take_while(|(b, _)| *b < range.start + min)
        .any(|(_, t)| t.is_end())
    {
        let mut end_entries = Vec::new();
        for (_, b, tag) in rev_range(buf, ..skip_n)
            .filter_map(entries_rev(skip_b))
            .take_while(|(_, b, _)| *b + min >= range.start)
        {
            if tag.is_end() {
                end_entries.push((b, tag))
            } else if !tag.is_start() {
                continue;
            } else if let Some(match_i) =
                end_entries.iter().rposition(|(_, e_t)| tag.ends_with(e_t))
            {
                end_entries.remove(match_i);
            } else if removed
                .iter()
                .take_while(|(e_b, _)| *e_b <= b + min)
                .any(|(_, e_t)| tag.ends_with(e_t))
            {
                entries.push((b, tag))
            }
        }
    }
    if removed
        .iter()
        .rev()
        .take_while(|(b, _)| *b + min > range.end)
        .any(|(_, t)| t.is_start())
    {
        let mut start_entries = Vec::new();
        for (_, b, tag) in fwd_range(buf, skip_n..)
            .filter_map(entries_fwd(skip_b))
            .take_while(|(_, b, _)| *b < range.end + min)
        {
            if tag.is_start() {
                start_entries.push((b, tag))
            } else if !tag.is_end() {
                continue;
            } else if let Some(match_i) = start_entries
                .iter()
                .rposition(|(_, s_t)| s_t.ends_with(&tag))
            {
                start_entries.remove(match_i);
            } else if removed
                .iter()
                .rev()
                .take_while(|(s_b, _)| b <= s_b + min)
                .any(|(_, s_t)| s_t.ends_with(&tag))
            {
                entries.push((b, tag));
            }
        }
    }

    entries
}
