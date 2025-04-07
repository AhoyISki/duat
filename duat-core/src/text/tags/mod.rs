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
use ids::RangeId;

use self::types::Toggle;
pub use self::{
    ids::{GhostId, Key, Keys, ToggleId},
    types::{
        RawTag::{self, *},
        Tag,
    },
};
use super::{Point, Text, records::Records};
use crate::get_ends;

/// How many [`TagOrSkip`]s to keep a [`RawTag`] range
///
/// The limit is low because most of the time, people don't nest
/// [`Tag`]s, and a low limit means that iteration will go back very
/// few [`TagOrSkip`]s
const MIN_FOR_RANGE: usize = 16;
/// How many [`RawTag`] ranges until their minimum size is increased
const LIMIT_TO_BUMP: usize = 128;
/// How much to increase the minimum when enough [`RawTag`] ranges
/// exist
const BUMP_AMOUNT: usize = 16;

/// The struct that holds the [`RawTag`]s of the [`Text`]
///
/// It also holds the [`Text`]s of any [`GhostText`]s, and the
/// functions of [`ToggleStart`]s
#[derive(Clone)]
pub struct Tags {
    buf: GapBuffer<TagOrSkip>,
    texts: HashMap<GhostId, Text>,
    toggles: HashMap<ToggleId, Toggle>,
    range_min: usize,
    pub bounds: Vec<([usize; 2], RangeId, RawTag)>,
    records: Records<[usize; 2]>,
    ranges_to_update: Vec<Range<usize>>,
}

impl Tags {
    /// Creates a new [`Tags`] with a given len
    pub fn new(len: usize) -> Self {
        Self {
            buf: if len == 0 {
                GapBuffer::new()
            } else {
                gap_buffer![TagOrSkip::Skip(len as u32)]
            },
            texts: HashMap::new(),
            toggles: HashMap::new(),
            range_min: MIN_FOR_RANGE,
            bounds: Vec::new(),
            records: Records::with_max([(len != 0) as usize, len]),
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
    pub fn insert(&mut self, key: Key, tag: Tag) -> Option<ToggleId> {
        let ((s_at, s_tag), end, toggle) = tag.into_raw(key, &mut self.texts, &mut self.toggles);
        get_ends(s_at..end.map(|(e, _)| e).unwrap_or(s_at), self.len_bytes());

        let mut sh_bounds = shift_bounds();
        let [s_n, s_b, s_skip] = self.get_skip_at(s_at);

        if let Some((e_at, e_tag)) = end {
            if s_at == e_at {
                return None;
            }

            let [ins_s_n, added_on_s_n] = self.insert_inner(s_at, s_tag, [s_n, s_b, s_skip]);
            sh_bounds(&mut self.bounds, (ins_s_n, [added_on_s_n as i32, 0]), false);

            // This is very likely the hot path.
            let [e_n, e_b, e_skip] = if e_at < s_b + s_skip {
                // These changes account for a possibly split skip.
                [s_n + added_on_s_n, s_at, s_skip - (s_at - s_b)]
            // Almost all insertions will fall here or above.
            } else if e_at < s_at + self.range_min {
                let [max_n, max_b] = self.records.max();
                let mut b = s_b;
                fwd_range(&self.buf, s_n..)
                    .filter_map(|(n, ts)| Some(n).zip(ts.as_skip()))
                    .find_map(|(n, skip)| {
                        b += skip;
                        (b > e_at).then_some([n, b - skip, skip])
                    })
                    .unwrap_or([max_n, max_b, 0])
            } else {
                self.get_skip_at(e_at)
            };

            let [ins_e_n, added_on_e_n] = self.insert_inner(e_at, e_tag, [e_n, e_b, e_skip]);
            sh_bounds(&mut self.bounds, (ins_e_n, [added_on_e_n as i32, 0]), true);

            if e_n + added_on_e_n - s_n + added_on_s_n >= self.range_min {
                let id = RangeId::new();
                let s_bound = ([ins_s_n + added_on_s_n - 1, s_at], id, s_tag);
                let e_bound = ([ins_e_n + added_on_e_n - 1, e_at], id, e_tag);
                for bound in [s_bound, e_bound] {
                    let (Ok(i) | Err(i)) = self.bounds.binary_search(&bound);
                    self.bounds.insert(i, bound);
                }
            }
            for [s, e] in [[ins_s_n, ins_s_n + added_on_s_n], [
                ins_e_n,
                ins_e_n + added_on_e_n,
            ]] {
                super::merge_range_in(
                    &mut self.ranges_to_update,
                    s.saturating_sub(self.range_min)..(e + self.range_min).min(self.buf.len()),
                );
            }
        } else {
            let [ins_s_n, added_on_s_n] = self.insert_inner(s_at, s_tag, [s_n, s_b, s_skip]);
            sh_bounds(&mut self.bounds, (ins_s_n, [added_on_s_n as i32, 0]), true);

            // Since we are adding more TagOrSkips, ranges surrounding this
            // insertion could need their bounds saved, as enough entries fit
            // inside to justify it.
            // However, this is going to be lazyly evaluated when Tags is updated.
            let [s, e] = [ins_s_n, ins_s_n + added_on_s_n];
            super::merge_range_in(
                &mut self.ranges_to_update,
                s.saturating_sub(ins_s_n)..(e + self.range_min).min(self.buf.len()),
            );
        };

        toggle
    }

    /// Inserts a [`RawTag`] in `at`, returns how many [`TagOrSkip`]s
    /// were added
    fn insert_inner(&mut self, at: usize, tag: RawTag, [n, b, skip]: [usize; 3]) -> [usize; 2] {
        if at == b {
            let n = n - rev_range(&self.buf, ..n)
                .take_while(|(_, ts)| ts.as_tag().is_some_and(|t| t.priority() > tag.priority()))
                .count();

            self.buf.insert(n, TagOrSkip::Tag(tag));
            self.records.transform([n, at], [0, 0], [1, 0]);
            self.records.insert([n, at]);
            [n, 1]
        } else {
            self.buf.splice(n..=n, [
                TagOrSkip::Skip((at - b) as u32),
                TagOrSkip::Tag(tag),
                TagOrSkip::Skip((b + skip - at) as u32),
            ]);
            self.records.transform([n, at], [0, 0], [2, 0]);
            self.records.insert([n + 1, at]);
            [n, 2]
        }
    }

    /// Extends this [`Tags`] with another one
    pub fn extend(&mut self, mut other: Tags) {
        let len = self.buf.len();
        let taken_from_start = if let Some(TagOrSkip::Skip(first)) = other.buf.get(0)
            && let Some(TagOrSkip::Skip(last)) = self.buf.get_mut(len - 1)
        {
            let first = *first;
            *last += first;
            other.buf.remove(0);
            other.records.transform([0, 0], [1, 0], [0, 0]);
            1
        } else {
            0
        };

        for ([n, b], ..) in other.bounds.iter_mut() {
            *n += self.buf.len() - taken_from_start;
            *b += self.len_bytes();
        }

        self.buf.extend(&other.buf);
        self.texts.extend(other.texts);
        self.toggles.extend(other.toggles);
        self.records.extend(other.records);
        self.bounds.extend(other.bounds);
    }

    /// Removes all [`RawTag`]s of a give [`Keys`]
    pub fn remove_from(&mut self, range: impl RangeBounds<usize>, keys: impl Keys) {
        let (start, end) = crate::get_ends(range, self.len_bytes());

        // It is best to do this first, so getting skips returns correct
        // entries.
        let mut sh_bounds =
            self.remove_intersecting_ranges(start..end, |tag| keys.clone().contains(tag.key()));

        let [mut n, mut b, _] = self.get_skip_behind(start);
        let (mut initial_n, initial_b) = (n, b);

        let mut removed = 0;
        let mut starts = Vec::new();

        while b < end {
            let ts = self.buf.get(n).copied();
            if let Some(TagOrSkip::Skip(skip)) = ts {
                if removed > 0 {
                    self.records.transform([n, b], [removed, 0], [0, 0]);
                }

                // Try to merge this skip with the previous one.
                if let Some(i) = n.checked_sub(1)
                    && let Some(TagOrSkip::Skip(prev_skip)) = self.buf.get(i)
                {
                    self.buf
                        .splice(i..=i + 1, [TagOrSkip::Skip(prev_skip + skip)]);
                    self.records.transform([n, b], [1, 0], [0, 0]);
                    sh_bounds(&mut self.bounds, (i, [-1, 0]), false);
                } else {
                    n += 1;
                }

                b += skip as usize;
                removed = 0;
            } else if let Some(TagOrSkip::Tag(tag)) = ts {
                if !keys.clone().contains(tag.key()) {
                    n += 1;
                    continue;
                }
                self.buf.remove(n);
                sh_bounds(&mut self.bounds, (n, [-1, 0]), false);

                let (rm_s_n, diff) =
                    self.handle_removed_tag((b, tag), initial_n, initial_b, &mut starts);
                if diff != 0 {
                    n = (n as i32 + diff) as usize;
                    initial_n = (initial_n as i32 + diff) as usize;
                    sh_bounds(&mut self.bounds, (rm_s_n, [diff, 0]), false);
                }

                removed += 1;
            } else {
                if removed > 0 {
                    self.records.transform([n, b], [removed, 0], [0, 0]);
                }
                break;
            }
        }

        // This should be uncommon enought that I don't really care about
        // optimizing it tbh.
        for (_, tag) in starts {
            let (n, b, _) = find_match(&self.buf, (n, b, tag)).unwrap();
            let (rm_s_n, diff) = self.remove(n, b);
            sh_bounds(&mut self.bounds, (rm_s_n, [diff, 0]), false);
        }

        sh_bounds(&mut self.bounds, (self.buf.len(), [0; 2]), true);
        // Since we are only removing TagOrSkips, no bounds need to be
        // added to self.bounds.
    }

    /// Removes a specific [`RawTag`] in the [`GapBuffer`]
    ///
    /// The `n` index MUST point to a [`TagOrSkip::Tag`], you should
    /// NOT remove skips with this function.
    ///
    /// Will NOT handle matching [`Tag`]s, WILL handle shifting
    /// bounds, however, will return info useful for doing that.
    #[must_use]
    fn remove(&mut self, n: usize, b: usize) -> (usize, i32) {
        let TagOrSkip::Tag(_) = self.buf.remove(n) else {
            unreachable!("You are only supposed to remove Tags like this, not skips")
        };

        // Try to merge this skip with the previous one.
        if let Some(&TagOrSkip::Skip(r_skip)) = self.buf.get(n)
            // We may be at the very start
            && let Some(prev_n) = n.checked_sub(1)
            && let Some(&TagOrSkip::Skip(l_skip)) = self.buf.get(prev_n)
        {
            self.buf
                .splice(prev_n..=n, [TagOrSkip::Skip(l_skip + r_skip)]);
            self.records.transform([prev_n, b], [2, 0], [0, 0]);
            (prev_n, -2)
        } else {
            self.records.transform([n, b], [1, 0], [0, 0]);
            (n, -1)
        }
    }

    /// Transforms a byte range into another byte range
    ///
    /// This will destroy any [`RawTag`]s contained in the original
    /// range.
    pub fn transform(&mut self, old: Range<usize>, new_end: usize) {
        let new = old.start..new_end;

        // First, get rid of all ranges that start and/or end in the old
        // range.
        let mut sh_bounds = self.remove_intersecting_ranges(old.clone(), |_| true);

        let [s_n, s_b, s_skip] = self.get_skip_at(old.start);
        // In case we're appending to the GapBuffer, a shortcut can be made.
        if [s_n, s_b] == self.records.max() {
            let last = self.buf.len().saturating_sub(1);
            let new_len = (new.end - old.start) as u32;
            if let Some(TagOrSkip::Skip(skip)) = self.buf.get_mut(last) {
                *skip += new_len;
                self.records.append([0, new_len as usize]);
            } else if new.end > old.start {
                self.buf.push_back(TagOrSkip::Skip(new_len));
                self.records.append([1, new_len as usize]);
                // For now, since ending Tags are not shifted
                // forwards, there is no need to use sh_bounds here.
            }

            return;
        };
        // You might wonder: Why not return if old == new?
        // It is assumed that a removal must get rid of all tags within, so
        // returning would prevent that.

        // Old length removal.
        let [s_n, s_b] = if old.end > old.start {
            let rm_b = old.end - old.start;
            // If the range to be removed is contained within one skip,
            // there is no need to check for where it ends.
            let [e_n, e_b] = if old.end <= s_b + s_skip {
                [s_n, s_b + s_skip]
            } else {
                // The check for the final skip is a `get_skip_behind` because we
                // don't want to remove one skip ahead of the end in the cases of
                // `old.start + len == some_skip`, since that would remove the tags at
                // the end of the range.
                let [n, b, skip] = self.get_skip_behind(old.end);
                [n, b + skip]
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
                if replacement.is_none() && new.end == old.start {
                    let mut dupes_and_pairs: Vec<Entry> = Vec::new();
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
                            let (Ok(k) | Err(k)) = to_remove.binary_search(&i);
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
            let mut s_n = s_n - rm_before_n;
            self.records
                .transform([s_n, s_b], [rm_n, rm_b], [added_n, 0]);
            let diff = [added_n as i32 - rm_n as i32, -(rm_b as i32)];
            sh_bounds(&mut self.bounds, (s_n, diff), false);

            // We can do the same thing that is done in remove_from, since we are
            // also "removing from".
            let mut starts = Vec::new();
            for (b, tag) in rm {
                let (rm_s_n, diff) = self.handle_removed_tag((b, tag), s_n, s_b, &mut starts);
                if diff != 0 {
                    s_n = (s_n as i32 + diff) as usize;
                    sh_bounds(&mut self.bounds, (rm_s_n, [diff, 0]), false);
                }
            }

            for (_, tag) in starts {
                let (n, b, _) = find_match(&self.buf, (s_n + added_n, e_b, tag)).unwrap();
                let (rm_s_n, diff) = self.remove(n, b);
                sh_bounds(&mut self.bounds, (rm_s_n, [diff, 0]), false);
            }

            [s_n, s_b]
        } else {
            [s_n, s_b]
        };

        if new.end > old.start {
            let added_b = new.len();
            if let Some(TagOrSkip::Skip(skip)) = self.buf.get_mut(s_n) {
                *skip += added_b as u32;
                self.records.transform([s_n, s_b], [0, 0], [0, added_b]);
                sh_bounds(&mut self.bounds, (s_n, [0, added_b as i32]), false);
            // The skip may have deleted by now, so we add it back.
            } else {
                self.buf.insert(s_n, TagOrSkip::Skip(added_b as u32));
                self.records.transform([s_n, s_b], [0, 0], [1, added_b]);
                sh_bounds(&mut self.bounds, (s_n, [1, added_b as i32]), false);
            }
        }

        sh_bounds(&mut self.bounds, (self.buf.len(), [0, 0]), true);
        // Just like remove_from, this function can only take
        // TagOrSkips away, so no new bounds need to be considered.
    }

    fn handle_removed_tag(
        &mut self,
        (b, tag): (usize, RawTag),
        initial_n: usize,
        initial_b: usize,
        starts: &mut Vec<(usize, RawTag)>,
    ) -> (usize, i32) {
        if tag.is_start() {
            starts.push((b, tag));
        } else if tag.is_end() {
            if let Some(i) = starts.iter().rposition(|(_, lhs)| lhs.ends_with(&tag)) {
                starts.remove(i);
            // If the starting Tag wasn't here, it was
            // before start, so we need to look for it.
            } else {
                // Search from initial positions, to skip iterating through the range.
                let (n, b, _) = find_match(&self.buf, (initial_n, initial_b, tag)).unwrap();
                // Here, we'll shift all ranges ahead, since this happens so
                // infrequently that I don't care about the performance impact.
                return self.remove(n, b);
            }
        }

        (0, 0)
    }

    /// Removes all ranges that start or end in the range
    ///
    /// WILL handle matching [`Tag`]s, WILL handle shifting of ranges,
    /// will NOT handle fully contained ranges.
    fn remove_intersecting_ranges(
        &mut self,
        range: Range<usize>,
        f: impl Fn(&RawTag) -> bool,
    ) -> impl FnMut(&mut [([usize; 2], RangeId, RawTag)], (usize, [i32; 2]), bool) + 'static {
        let mut sh_bounds = shift_bounds();
        if range.is_empty() {
            return sh_bounds;
        }

        let removed: Vec<([usize; 2], [usize; 2])> = {
            let removed: Vec<_> = self
                .bounds
                .extract_if(.., |([_, b], _, tag)| range.clone().contains(b) && f(tag))
                .collect();
            removed
                .into_iter()
                .filter_map(|([n0, b0], id, _)| {
                    // If there is a matching bound still in self.bounds, it means that
                    // one of the bounds was outside of the range.
                    // When this happens, we are assuming that the caller is not removing
                    // said bound, so we have to do it in this function.
                    // Also, I have to collect beforehand, in order to not remove bounds
                    // while using the sh_bounds closure.
                    self.bounds
                        .extract_if(.., |(_, lhs, _)| *lhs == id)
                        .next()
                        .map(|([n1, b1], ..)| ([n0, b0], [n1, b1]))
                })
                .collect()
        };

        for ([n0, b0], [n1, b1]) in removed {
            let [s_n, s_b] = [n0.min(n1), b0.min(b1)];
            let [e_n, e_b] = [n0.max(n1), b0.max(b1)];
            let (e_rm_s_n, e_diff) = self.remove(e_n, e_b);
            // Do the end first, to not incur shifts on the start.
            sh_bounds(&mut self.bounds, (e_rm_s_n, [e_diff, 0]), false);
            // We remove both bounds, in order to prevent a state of dangling
            // bounds, which would cause a lookback or lookahead over the whole
            // Text.
            let (s_rm_s_n, s_diff) = self.remove(s_n, s_b);
            sh_bounds(&mut self.bounds, (s_rm_s_n, [s_diff, 0]), false);
        }

        sh_bounds
    }

    pub fn update_bounds(&mut self) {
        let ranges = std::mem::take(&mut self.ranges_to_update);

        let mut start_tags = Vec::new();
        for range in ranges.into_iter() {
            // let (to_check, split_off) =
            // super::split_range_within(range.clone(), within.clone());
            if range.len() < self.range_min {
                continue;
            }

            let [n, b, _] = self.get_skip_behind(range.start);
            // Add all tags in the range to the list, keeping only those whose
            // ranges are too long.
            for (n, b, tag) in fwd_range(&self.buf, n..)
                .filter_map(entries_fwd(b))
                .skip_while(|(_, b, _)| *b < range.start)
                .take_while(|(_, b, _)| *b < range.end)
            {
                if tag.is_start() {
                    start_tags.push((n, b, tag));
                } else if tag.is_end()
                    && let Some(i) = start_tags.iter().rposition(|(.., lhs)| lhs.ends_with(&tag))
                {
                    let (s_n, s_b, s_tag) = start_tags.remove(i);
                    if s_n + self.range_min <= n {
                        let key = |&(place, _, tag)| (place, tag);
                        let s_i = self.bounds.binary_search_by_key(&([s_n, s_b], s_tag), key);
                        let e_i = self.bounds.binary_search_by_key(&([n, b], tag), key);

                        match (s_i, e_i) {
                            (Ok(s_i), Err(e_i)) | (Err(s_i), Ok(e_i)) | (Err(s_i), Err(e_i)) => {
                                let id = RangeId::new();
                                self.bounds.insert(e_i, ([n, b], id, tag));
                                self.bounds.insert(s_i, ([s_n, s_b], id, s_tag));
                            }
                            // In this case, we assume that said range already exists, and don't
                            // add it ¯\_(ツ)_/¯.
                            (Ok(_), Ok(_)) => {}
                        }
                    }
                }
            }
        }

        // Cull ranges that are too short.
        while self.bounds.len() >= LIMIT_TO_BUMP * 2 {
            let mut bounds_to_remove = Vec::new();
            self.range_min += BUMP_AMOUNT;
            let iter = self.bounds.iter().filter(|(.., tag)| tag.is_start());
            for (i, ([n0, _], id, _)) in iter.enumerate() {
                let (j, ([n1, _], ..)) = self.bounds[(i + 1)..]
                    .iter()
                    .enumerate()
                    .find(|(_, (_, lhs, _))| lhs == id)
                    .unwrap();

                if n1 - n0 < self.range_min {
                    for k in [i, j] {
                        let (Ok(l) | Err(l)) = bounds_to_remove.binary_search(&k);
                        bounds_to_remove.insert(l, k);
                    }
                }
            }

            for i in bounds_to_remove.into_iter().rev() {
                self.bounds.remove(i);
            }
        }
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
    #[define_opaque(FwdTags)]
    pub fn fwd_at(&self, at: usize) -> FwdTags {
        let [s_n, s_b] = {
            let [n, b, _] = self.get_skip_behind(at.min(self.len_bytes()));
            rev_range(&self.buf, ..n)
                .take(self.range_min)
                .fold([n, b], |[_, b], (n, ts)| [n, b - ts.len()])
        };

        let bounds = self
            .bounds
            .iter()
            .map_while(move |([n, b], _, tag)| (*n < s_n).then_some((*b, *tag)));

        let tags = {
            let iter = fwd_range(&self.buf, s_n..).filter_map(entries_fwd(s_b));
            iter.map(|(n, b, tag)| match tag {
                StartConceal(key) => {
                    if let Ok(i) = self.bounds.binary_search_by_key(&n, |([n, _], ..)| *n) {
                        let (_, id, _) = self.bounds[i];
                        let e_bound = self.bounds[(i + 1)..].iter().find(|(_, lhs, _)| *lhs == id);
                        let ([_, e_b], ..) = e_bound.unwrap();
                        (b, ConcealUntil(*e_b as u32))
                    } else {
                        (b, StartConceal(key))
                    }
                }
                tag => (b, tag),
            })
        };

        bounds.into_iter().chain(tags).peekable()
    }

    /// Returns a reverse iterator at a given byte
    #[define_opaque(RevTags)]
    pub fn rev_at(&self, at: usize) -> RevTags {
        let [e_n, e_b, _] = {
            let [n, b, _] = self.get_skip_at(at.min(self.len_bytes()));
            fwd_range(&self.buf, n..)
                .take(self.range_min)
                .fold([n, b, 0], |[_, b, len], (n, ts)| [n, b + len, ts.len()])
        };

        let bounds = self
            .bounds
            .iter()
            .rev()
            .map_while(move |([n, b], _, tag)| (*n > e_n).then_some((*b, *tag)));

        let tags = {
            let iter = rev_range(&self.buf, ..e_n).filter_map(entries_rev(e_b));
            iter.map(|(n, b, tag)| match tag {
                EndConceal(key) => {
                    if let Ok(i) = self.bounds.binary_search_by_key(&n, |([n, _], ..)| *n) {
                        let (_, id, _) = self.bounds[i];
                        let s_bound = self.bounds[..i].iter().rev().find(|(_, lhs, _)| *lhs == id);
                        let ([_, s_b], ..) = s_bound.unwrap();
                        (b, ConcealUntil(*s_b as u32))
                    } else {
                        (b, EndConceal(key))
                    }
                }
                tag => (b, tag),
            })
        };

        bounds.chain(tags).peekable()
    }

    pub fn raw_fwd_at(&self, b: usize) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        let [n, b, _] = self.get_skip_behind(b);
        fwd_range(&self.buf, n..)
            .filter_map(entries_fwd(b))
            .map(|(_, b, t)| (b, t))
    }

    pub fn raw_rev_at(&self, b: usize) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        let [n, b, _] = self.get_skip_at(b);
        rev_range(&self.buf, ..n)
            .filter_map(entries_rev(b))
            .map(|(_, b, t)| (b, t))
    }

    /// Returns an iterator over a single byte
    pub fn iter_only_at(&self, at: usize) -> impl Iterator<Item = RawTag> + '_ {
        let [n, b, _] = self.get_skip_at(at);
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

    /// Returns information about the skip in the byte `at`
    ///
    /// * The index in the [`GapBuffer`] where it starts
    /// * The byte where it starts
    /// * Its length
    fn get_skip_at(&self, at: usize) -> [usize; 3] {
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
                .find(|[_, b, skip]| *b + *skip > at)
        } else {
            let iter = rev_range(&self.buf, ..n).filter_map(skips);
            iter.map(|[n, this_b, skip]| [n, b - this_b, skip])
                .find(|[_, b, skip]| *b + *skip > at && *b <= at)
        }
        .unwrap_or({
            let [n, b] = self.records.max();
            [n, b, 0]
        })
    }

    /// Return the [`Text`] of a given [`TextId`]
    pub fn get_ghost(&self, k: GhostId) -> Option<&Text> {
        self.texts.get(&k)
    }

    /// Same as [`get_skip_at`], but takes the previous skip
    ///
    /// This will return the same skip if `b != at`, and the previous
    /// skip otherwise.
    ///
    /// [`get_skip_at`]: Tags::get_skip_at
    fn get_skip_behind(&self, at: usize) -> [usize; 3] {
        let [n, b, skip] = self.get_skip_at(at);
        if b == at {
            rev_range(&self.buf, ..n)
                .find_map(|(n, ts)| Some(n).zip(ts.as_skip()))
                .map(|(n, skip)| [n, b - skip, skip])
                .unwrap_or([0, 0, 0])
        } else {
            [n, b, skip]
        }
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
fn entries_fwd(mut b: usize) -> impl FnMut((usize, &TagOrSkip)) -> Option<Entry> + Clone {
    move |(n, ts)| {
        b += ts.len();
        ts.as_tag().map(|t| (n, b, t))
    }
}

/// Reverse enumerating function for a [`TagOrSkip::Tag`] from a byte
fn entries_rev(mut b: usize) -> impl FnMut((usize, &TagOrSkip)) -> Option<Entry> + Clone {
    move |(n, ts)| {
        b -= ts.len();
        ts.as_tag().map(|t| (n, b, t))
    }
}

/// Look for a match for a [`RawTag`]
fn find_match(buf: &GapBuffer<TagOrSkip>, (n, b, tag): Entry) -> Option<(usize, usize, RawTag)> {
    fn is_tag((_, ts): &(usize, &TagOrSkip)) -> bool {
        ts.is_tag()
    }
    let mut bound_fn = find_bound_fn(tag);
    if tag.is_start() {
        let n = n - rev_range(buf, ..n).take_while(is_tag).count();
        fwd_range(buf, n..)
            .filter_map(entries_fwd(b))
            .find_map(|(n, b, t)| bound_fn(t).then_some((n, b, t)))
    } else if tag.is_end() {
        let n = n + fwd_range(buf, n..).take_while(is_tag).count();
        rev_range(buf, ..n)
            .filter_map(entries_rev(b))
            .find_map(|(n, b, t)| bound_fn(t).then_some((n, b, t)))
    } else {
        None
    }
}

/// Nests equal [`RawTag`]s until a match is found
fn find_bound_fn(rhs: RawTag) -> impl FnMut(RawTag) -> bool {
    let mut bounds = 1;
    move |lhs| {
        bounds -= (lhs == rhs.inverse().unwrap()) as usize;
        bounds += (lhs == rhs) as usize;
        bounds == 0
    }
}

/// A stateful tracker of [`TagOrSkip`] additions and deletions
///
/// This is really useful if you want to incrementally shift the
/// ranges without incurring O(n^2) complexity (for example, in
/// [`Tags::remove_from`]).
///
/// All that you have to do is pass an entry composed of the index of
/// the removal and how many things were removed there.
///
/// Just make sure that the ranges [`Vec`] doesn't change in size.
fn shift_bounds() -> impl FnMut(&mut [([usize; 2], RangeId, RawTag)], (usize, [i32; 2]), bool) {
    let mut total_diff = [0; 2];
    let mut cur_n = 0;

    move |bounds, (insertion_n, [n_diff, b_diff]), shift_rest| {
        if let Some(([cur_n_n, _], ..)) = bounds.get(cur_n) {
            if insertion_n < *cur_n_n {
                let mut iter = bounds[..cur_n].iter_mut().rev();
                while let Some(([n, b], ..)) = iter.next()
                    && *n >= insertion_n
                {
                    *n = (*n as i32 + n_diff) as usize;
                    *b = (*b as i32 + b_diff) as usize;
                }
            } else {
                let mut iter = bounds[cur_n..].iter_mut();
                while let Some(([n, b], ..)) = iter.next()
                    && *n < insertion_n
                {
                    *n = (*n as i32 + total_diff[0]) as usize;
                    *b = (*b as i32 + total_diff[1]) as usize;
                    cur_n += 1;
                }
            }
            total_diff[0] += n_diff;
            total_diff[1] += b_diff;
        }

        if shift_rest {
            for ([n, b], ..) in bounds.get_mut(cur_n..).into_iter().flatten() {
                *n = (*n as i32 + n_diff) as usize;
                *b = (*b as i32 + b_diff) as usize;
            }
        }
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
            TagOrSkip::Tag(tag) => write!(f, "{:?}", tag),
            TagOrSkip::Skip(amount) => write!(f, "Skip({amount})"),
        }
    }
}

impl std::fmt::Debug for Tags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct TagsDbg<'a>(&'a GapBuffer<TagOrSkip>, usize);
        impl std::fmt::Debug for TagsDbg<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                if f.alternate() {
                    let n_spc = self.0.len().checked_ilog10().unwrap_or(0) as usize + 4;
                    let b_spc = self.1.checked_ilog10().unwrap_or(0) as usize + 4;

                    let mut b = 0;
                    let mut nesting: usize = 0;
                    f.write_str("[\n")?;
                    for (n, ts) in self.0.iter().enumerate() {
                        nesting = nesting
                            .saturating_sub(ts.as_tag().is_some_and(|t| t.is_end()) as usize);
                        let space = " ".repeat(nesting);

                        let n_fmt = format!("n: {n}");
                        let b_fmt = format!("b: {b}");
                        writeln!(f, "    ({n_fmt:<n_spc$}, {b_fmt:<b_spc$}): {space}{ts:?}")?;
                        nesting += ts.as_tag().is_some_and(|t| t.is_start()) as usize;
                        b += ts.len();
                    }
                    f.write_str("]")
                } else {
                    write!(f, "{:?}", self.0)
                }
            }
        }

        struct BoundsDbg<'a>(&'a [([usize; 2], RangeId, RawTag)]);
        impl std::fmt::Debug for BoundsDbg<'_> {
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
            .field("buf", &TagsDbg(&self.buf, self.len_bytes()))
            .field("bounds", &BoundsDbg(&self.bounds))
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

type Entry = (usize, usize, RawTag);
