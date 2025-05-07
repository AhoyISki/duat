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
    cell::Cell,
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
        RawTagsFn, Tag,
    },
};
use super::{Point, Text, records::Records};
use crate::{get_ends, merging_range_by_guess_and_lazy_shift};

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
    ghosts: HashMap<GhostId, Text>,
    toggles: HashMap<ToggleId, Toggle>,
    range_min: usize,
    records: Records<[usize; 2]>,
    bounds: Vec<(Cell<[usize; 2]>, RawTag, RangeId)>,
    bounds_shift_state: Cell<(usize, [i32; 2])>,
    ranges_to_update: Vec<Range<usize>>,
    ranges_shift_state: (usize, i32),
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
            ghosts: HashMap::new(),
            toggles: HashMap::new(),
            range_min: MIN_FOR_RANGE,
            bounds: Vec::new(),
            records: Records::with_max([(len != 0) as usize, len]),
            ranges_to_update: Vec::new(),
            bounds_shift_state: Cell::new((0, [0; 2])),
            ranges_shift_state: (0, 0),
        }
    }

    /// Removes all [`RawTag`]s and sets the len to 0
    pub fn clear(&mut self) {
        self.buf = GapBuffer::new();
        self.ghosts.clear();
        self.toggles.clear();
        self.records.clear();
    }

    /// Insert a new [`Tag`] at a given byte
    pub fn insert(
        &mut self,
        key: Key,
        tag: Tag<impl RangeBounds<usize>, impl RawTagsFn>,
    ) -> Option<ToggleId> {
        fn exists_at(tags: &GapBuffer<TagOrSkip>, n: usize, tag: RawTag) -> bool {
            rev_range(tags, ..n)
                .map_while(|(_, ts)| ts.as_tag())
                .any(|lhs| lhs == tag)
        }

        let (s_at, e_at) = get_ends(tag.range, self.len_bytes());
        let (s_tag, e_tag, toggle) = (tag.tags)(key, &mut self.ghosts, &mut self.toggles);

        let [s_n, s_b, s_skip] = self.skip_at(s_at);

        if let Some(e_tag) = e_tag
            && s_at != e_at
        {
            let [e_n, e_b, e_skip] = {
                // This is very likely the hot path.
                let [n, b, skip] = if e_at < s_b + s_skip {
                    // These changes account for a possibly split skip.
                    [s_n, s_at, s_skip - (s_at - s_b)]
                } else {
                    self.skip_at(e_at)
                };

                if exists_at(&self.buf, s_n, s_tag) && exists_at(&self.buf, n, e_tag) {
                    return None;
                }
                let s_n_diff = if s_at == s_b { 1 } else { 2 };

                [n + s_n_diff, b, skip]
            };

            let [s_ins, s_n_diff] = self.insert_inner(s_at, s_tag, [s_n, s_b, s_skip]);
            let bounds_s_ins = self.shift_bounds_and_ranges(s_ins, [s_n_diff as i32, 0]);
            self.merge_range_to_update({
                let [s, e] = [s_ins, s_ins + s_n_diff];
                s.saturating_sub(self.range_min)..(e + self.range_min).min(self.buf.len())
            });

            let [e_ins, e_n_diff] = self.insert_inner(e_at, e_tag, [e_n, e_b, e_skip]);

            if e_ins + e_n_diff - s_ins + s_n_diff >= self.range_min {
                let id = RangeId::new();

                self.bounds.insert(
                    bounds_s_ins,
                    (Cell::new([s_ins + s_n_diff - 1, s_at]), s_tag, id),
                );
                self.declare_shifted(bounds_s_ins);

                let bounds_e_ins = self.shift_bounds_and_ranges(e_ins, [e_n_diff as i32, 0]);
                self.bounds.insert(
                    bounds_e_ins,
                    (Cell::new([e_ins + e_n_diff - 1, e_at]), e_tag, id),
                );
                self.declare_shifted(bounds_e_ins);
            } else {
                self.shift_bounds_and_ranges(e_ins, [e_n_diff as i32, 0]);
            }
            self.merge_range_to_update({
                let [s, e] = [e_ins, e_ins + e_n_diff];
                s.saturating_sub(self.range_min)..(e + self.range_min).min(self.buf.len())
            });

            toggle
        } else if e_tag.is_none() {
            let [ins, n_diff] = self.insert_inner(s_at, s_tag, [s_n, s_b, s_skip]);
            self.shift_bounds_and_ranges(ins, [n_diff as i32, 0]);

            // Since we are adding more TagOrSkips, ranges surrounding this
            // insertion could need their bounds saved, as enough entries fit
            // inside to justify it.
            // However, this is going to be lazyly evaluated when Tags is updated.
            let [s, e] = [ins, ins + n_diff];
            self.merge_range_to_update(
                s.saturating_sub(self.range_min)..(e + self.range_min).min(self.buf.len()),
            );

            None
        } else {
            None
        }
    }

    /// Inserts a [`RawTag`] in `at`, returns how many [`TagOrSkip`]s
    /// were added
    ///
    /// Will NOT handle bounds creation, WILL transform [`Records`].
    fn insert_inner(&mut self, at: usize, tag: RawTag, [n, b, skip]: [usize; 3]) -> [usize; 2] {
        if at == b {
            let n = n - rev_range(&self.buf, ..n)
                .take_while(|(_, ts)| ts.as_tag().is_some_and(|t| t.priority() > tag.priority()))
                .count();

            self.buf.insert(n, TagOrSkip::Tag(tag));
            self.records.transform([n, b], [0, 0], [1, 0]);
            self.records.insert([n, b]);
            [n, 1]
        } else {
            self.buf.splice(n..=n, [
                TagOrSkip::Skip((at - b) as u32),
                TagOrSkip::Tag(tag),
                TagOrSkip::Skip((b + skip - at) as u32),
            ]);
            self.records.transform([n + 1, at], [0, 0], [2, 0]);
            self.records.insert([n + 1, at]);
            [n, 2]
        }
    }

    /// Extends this [`Tags`] with another one
    pub fn extend(&mut self, mut other: Tags) {
        self.finish_shifting_bounds();

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

        for (bound, ..) in other.bounds.iter() {
            let [n, b] = bound.get();
            bound.set([n + self.buf.len() - taken_from_start, b + self.len_bytes()]);
        }

        self.buf.extend(&other.buf);
        self.ghosts.extend(other.ghosts);
        self.toggles.extend(other.toggles);
        self.records.extend(other.records);
        self.bounds.extend(other.bounds);
    }

    /// Removes all [`RawTag`]s of a give [`Keys`]
    pub fn remove_from(&mut self, range: impl RangeBounds<usize>, keys: impl Keys) {
        let (start, end) = crate::get_ends(range, self.len_bytes());

        // It is best to do this first, so getting skips returns correct
        // entries.
        self.remove_intersecting_bounds(start..end, |tag| keys.clone().contains(tag.key()));

        let [mut n, mut b, _] = self.skip_behind(start);
        let (mut initial_n, mut initial_b) = (n, b);

        let mut removed = 0;
        let mut starts = Vec::new();

        while b < end {
            let ts = self.buf.get(n).copied();
            if let Some(TagOrSkip::Skip(skip)) = ts {
                if removed > 0 {
                    self.records.transform([n, b], [removed, 0], [0, 0]);
                }

                // Try to merge this skip with the previous one.
                if let Some(prev_n) = n.checked_sub(1)
                    && let Some(TagOrSkip::Skip(prev_skip)) = self.buf.get(prev_n)
                {
                    self.buf
                        .splice(prev_n..=n, [TagOrSkip::Skip(prev_skip + skip)]);
                    self.records.transform([n, b], [1, 0], [0, 0]);
                    self.shift_bounds_and_ranges(n, [-1, 0]);
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
                self.shift_bounds_and_ranges(n, [-1, 0]);

                // The handled removed RawTag is always before the range.
                let ([rm_n, rm_b], n_diff) =
                    self.handle_removed_tag((b, tag), initial_n, initial_b, &mut starts);
                if n_diff != 0 {
                    n = sh(n, n_diff);
                    initial_n = sh(initial_n, n_diff);
                    if initial_n == rm_n {
                        initial_b = rm_b
                    }
                    self.shift_bounds_and_ranges(rm_n, [n_diff, 0]);
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
            let ([rm_n, _], n_diff) = self.remove(n, b);
            self.shift_bounds_and_ranges(rm_n, [n_diff, 0]);
        }

        // Since we are only removing TagOrSkips, no bounds need to be
        // added to self.bounds.
    }

    /// Removes a specific [`RawTag`] in the [`GapBuffer`]
    ///
    /// The `n` index MUST point to a [`TagOrSkip::Tag`], you should
    /// NOT remove skips with this function.
    ///
    /// Will NOT handle matching [`Tag`]s, WILL transform [`Records`],
    /// will NOT handle shifting bounds, however, will return info
    /// useful for doing that.
    #[must_use]
    fn remove(&mut self, n: usize, b: usize) -> ([usize; 2], i32) {
        let TagOrSkip::Tag(_) = self.buf.remove(n) else {
            unreachable!("You are only supposed to remove Tags like this, not skips")
        };

        // Try to merge this skip with the previous one.
        if let Some(&TagOrSkip::Skip(r_skip)) = self.buf.get(n)
            // We may be at the very start
            && let Some(l_n) = n.checked_sub(1)
            && let Some(&TagOrSkip::Skip(l_skip)) = self.buf.get(l_n)
        {
            self.buf.splice(l_n..=n, [TagOrSkip::Skip(l_skip + r_skip)]);
            self.records.transform([n, b], [2, 0], [0, 0]);
            ([l_n, b - l_skip as usize], -2)
        } else {
            self.records.transform([n, b], [1, 0], [0, 0]);
            ([n, b], -1)
        }
    }

    /// Transforms a byte range into another byte range
    ///
    /// This will destroy any [`RawTag`]s contained in the original
    /// range.
    pub fn transform(&mut self, old: Range<usize>, new_end: usize) {
        let new = old.start..new_end;

        // In case we're appending to the GapBuffer, a shortcut can be made.
        if old.start == self.len_bytes() {
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
            // First, get rid of all ranges that start and/or end in the old
            // range.
            // old.start + 1 because we don't want to get rid of bounds that
            // merely coincide with the edges.
            self.remove_intersecting_bounds(old.start + 1..old.end, |_| true);

            let [s_n, s_b, s_skip] = self.skip_at(old.start);

            let b_diff = old.end - old.start;
            // If the range to be removed is contained within one skip,
            // there is no need to check for where it ends.
            let [e_n, e_b] = if old.end <= s_b + s_skip {
                [s_n, s_b + s_skip]
            } else {
                // The check for the final skip is a `skip_behind` because we don't
                // want to remove one skip ahead of the end in the cases of
                // `old.start + len == some_skip`, since that would remove the tags at
                // the end of the range.
                let [n, b, skip] = self.skip_behind(old.end);
                [n, b + skip]
            };

            let skip = (e_b - s_b - b_diff) as u32;
            let added_n = (skip > 0) as usize;
            let rm: Vec<(usize, TagOrSkip)> = self
                .buf
                .splice(s_n..=e_n, (skip > 0).then_some(TagOrSkip::Skip(skip)))
                .scan(s_b, |b, ts| {
                    *b += ts.len();
                    Some((*b - ts.len(), ts))
                })
                .collect();

            let (mut s_n, mut s_b) = (s_n, s_b);
            self.records
                .transform([s_n, s_b], [rm.len(), b_diff], [added_n, 0]);
            self.shift_bounds_and_ranges(s_n, [added_n as i32 - rm.len() as i32, -(b_diff as i32)]);

            // We can do the same thing that is done in remove_from, since we are
            // also "removing from".
            let mut starts = Vec::new();
            for (b, tag) in rm
                .into_iter()
                .filter_map(|(b, ts)| ts.as_tag().map(|t| (b, t)))
            {
                let ([rm_n, rm_b], diff) = self.handle_removed_tag((b, tag), s_n, s_b, &mut starts);
                if diff != 0 {
                    s_n = (s_n as i32 + diff) as usize;
                    if s_n == rm_n {
                        s_b = rm_b;
                    }
                    self.shift_bounds_and_ranges(rm_n, [diff, 0]);
                }
            }

            for (_, tag) in starts {
                let (n, b, _) =
                    find_match(&self.buf, (s_n + added_n, s_b + skip as usize, tag)).unwrap();
                let ([rm_n, _], diff) = self.remove(n, b);
                self.shift_bounds_and_ranges(rm_n, [diff, 0]);
            }

            // If the range becomes empty, we should remove the remainig pairs
            if skip == 0 && new.end == old.start {
                let is_bound = |i| {
                    self.bounds
                        .binary_search_by(|(bound, ..)| bound.get()[0].cmp(&i))
                        .map(|i| self.bounds[i].2)
                };

                let mut to_remove: Vec<usize> = Vec::new();
                let mut starts = Vec::new();

                let s_n = s_n
                    - rev_range(&self.buf, ..s_n)
                        .take_while(|(_, ts)| ts.is_tag())
                        .count();

                for (n, tag) in fwd_range(&self.buf, s_n..)
                    .map_while(|(i, ts)| Some(i).zip(ts.as_tag()))
                    .filter(|(_, tag)| tag.is_end())
                {
                    if tag.is_start() {
                        starts.push((n, tag));
                    } else if tag.is_end()
                        && let Some(i) = starts.iter().position(|(_, lhs)| lhs.ends_with(&tag))
                        && let (s_n, _) = starts.remove(i)
                        && !is_bound(s_n).is_ok_and(|id| Ok(id) != is_bound(n))
                    {
                        for n in [n, s_n] {
                            let ins = to_remove.iter().take_while(|j| **j < n).count();
                            to_remove.insert(ins, n);
                        }
                    }
                }

                self.records
                    .transform([s_n, s_b], [to_remove.len(), 0], [0, 0]);

                for n in to_remove.into_iter().rev() {
                    self.buf.remove(n);
                    self.shift_bounds_and_ranges(n, [-1, 0]);
                }
            }

            [s_n, s_b]
        } else {
            let [s_n, s_b, _] = self.skip_at(old.start);
            [s_n, s_b]
        };

        if new.end > old.start {
            let added_b = new.len();
            if let Some(TagOrSkip::Skip(skip)) = self.buf.get_mut(s_n) {
                *skip += added_b as u32;
                self.records.transform([s_n, s_b], [0, 0], [0, added_b]);
                self.shift_bounds_and_ranges(s_n, [0, added_b as i32]);
            // The skip may have deleted by now, so we add it back.
            } else {
                self.buf.insert(s_n, TagOrSkip::Skip(added_b as u32));
                self.records.transform([s_n, s_b], [0, 0], [1, added_b]);
                self.shift_bounds_and_ranges(s_n, [1, added_b as i32]);
            }
        }

        // Just like remove_from, this function can only take
        // TagOrSkips away, so no new bounds need to be considered.
    }

    fn handle_removed_tag(
        &mut self,
        (b, tag): (usize, RawTag),
        initial_n: usize,
        initial_b: usize,
        starts: &mut Vec<(usize, RawTag)>,
    ) -> ([usize; 2], i32) {
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

        ([0; 2], 0)
    }

    pub fn update_bounds(&mut self) {
        self.finish_shifting_bounds();

        // Finish shifting ranges to update
        let (sh_from, total_n_diff) = std::mem::take(&mut self.ranges_shift_state);
        for range in self.ranges_to_update[sh_from..].iter_mut() {
            range.start = sh(range.start, total_n_diff);
            range.end = sh(range.end, total_n_diff);
        }

        let ranges = std::mem::take(&mut self.ranges_to_update);

        for range in ranges.into_iter() {
            let b = self.byte_at(range.start);
            // Add all tags in the range to the list, keeping only those whose
            // ranges are too long.
            let mut start_tags = Vec::new();
            for (n, b, tag) in fwd_range(&self.buf, range).filter_map(entries_fwd(b)) {
                if tag.is_start() {
                    start_tags.push((n, b, tag));
                } else if tag.is_end()
                    && let Some(i) = start_tags.iter().rposition(|(.., lhs)| lhs.ends_with(&tag))
                {
                    let (s_n, s_b, s_tag) = start_tags.remove(i);
                    if s_n + self.range_min > n {
                        continue;
                    }

                    type Bound = (Cell<[usize; 2]>, RawTag, RangeId);
                    let key = |(place, tag, _): &Bound| (place.get(), *tag);
                    let s_i = self.bounds.binary_search_by_key(&([s_n, s_b], s_tag), key);
                    let e_i = self.bounds.binary_search_by_key(&([n, b], tag), key);

                    match (s_i, e_i) {
                        (Err(s_i), Err(e_i)) => {
                            let id = RangeId::new();
                            self.bounds.insert(e_i, (Cell::new([n, b]), tag, id));
                            self.bounds.insert(s_i, (Cell::new([s_n, s_b]), s_tag, id));
                        }
                        // The bounds were already set in these cases
                        (Ok(_), Ok(_) | Err(_)) | (Err(_), Ok(_)) => {}
                    }
                }
            }
        }

        // Cull ranges that are too short.
        while self.bounds.len() >= LIMIT_TO_BUMP * 2 {
            let mut bounds_to_remove = Vec::new();
            self.range_min += BUMP_AMOUNT;
            let iter = self.bounds.iter().filter(|(_, tag, _)| tag.is_start());
            for (i, (bound0, _, id)) in iter.enumerate() {
                let [n0, _] = bound0.get();
                let (j, (bound1, ..)) = self.bounds[(i + 1)..]
                    .iter()
                    .enumerate()
                    .find(|(_, (.., lhs))| lhs == id)
                    .unwrap();

                let [n1, _] = bound1.get();
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
        self.finish_shifting_bounds();

        let [s_n, s_b] = {
            let [n, b, _] = self.skip_behind(at.min(self.len_bytes()));
            rev_range(&self.buf, ..n)
                .take(self.range_min)
                .fold([n, b], |[_, b], (n, ts)| [n, b - ts.len()])
        };

        let bounds = self.bounds.iter().map_while(move |(bound, tag, _)| {
            let [n, b] = bound.get();
            (n < s_n).then_some((b, *tag))
        });

        let tags = {
            let iter = fwd_range(&self.buf, s_n..).filter_map(entries_fwd(s_b));
            iter.map(|(n, b, tag)| match tag {
                StartConceal(key) => {
                    if let Ok(i) = self
                        .bounds
                        .binary_search_by_key(&n, |(bound, ..)| bound.get()[0])
                    {
                        let (_, _, id) = self.bounds[i];
                        let e_bound = self.bounds[(i + 1)..].iter().find(|(_, _, lhs)| *lhs == id);
                        (b, ConcealUntil(e_bound.unwrap().0.get()[1] as u32))
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
        self.finish_shifting_bounds();

        let [e_n, e_b, _] = {
            let [n, b, _] = self.skip_at(at.min(self.len_bytes()));
            fwd_range(&self.buf, n..)
                .take(self.range_min)
                .fold([n, b, 0], |[_, b, len], (n, ts)| [n, b + len, ts.len()])
        };

        let bounds = self.bounds.iter().rev().map_while(move |(bound, tag, _)| {
            let [n, b] = bound.get();
            (n > e_n).then_some((b, *tag))
        });

        let tags = {
            let iter = rev_range(&self.buf, ..e_n).filter_map(entries_rev(e_b));
            iter.map(|(n, b, tag)| match tag {
                EndConceal(key) => {
                    if let Ok(i) = self
                        .bounds
                        .binary_search_by_key(&n, |(bound, ..)| bound.get()[0])
                    {
                        let (_, _, id) = self.bounds[i];
                        let s_bound = self.bounds[..i].iter().rev().find(|(_, _, lhs)| *lhs == id);
                        (b, ConcealUntil(s_bound.unwrap().0.get()[1] as u32))
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
        let [n, b, _] = self.skip_behind(b);
        fwd_range(&self.buf, n..)
            .filter_map(entries_fwd(b))
            .map(|(_, b, t)| (b, t))
    }

    pub fn raw_rev_at(&self, b: usize) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        let [n, b, _] = self.skip_at(b);
        rev_range(&self.buf, ..n)
            .filter_map(entries_rev(b))
            .map(|(_, b, t)| (b, t))
    }

    /// Returns an iterator over a single byte
    pub fn iter_only_at(&self, at: usize) -> impl Iterator<Item = RawTag> + '_ {
        self.finish_shifting_bounds();

        let [n, b, _] = self.skip_at(at);
        (b == at)
            .then(|| rev_range(&self.buf, ..n).map_while(|(_, ts)| ts.as_tag()))
            .into_iter()
            .flatten()
    }

    /// Returns the length of all [`GhostText`]s in a byte
    pub fn ghosts_total_at(&self, at: usize) -> Option<Point> {
        self.iter_only_at(at).fold(None, |p, tag| match tag {
            RawTag::Ghost(_, id) => {
                let max_point = self.ghosts.get(&id).unwrap().len();
                Some(p.map_or(max_point, |p| p + max_point))
            }
            _ => p,
        })
    }

    /// Return the [`Text`] of a given [`TextId`]
    pub fn get_ghost(&self, k: GhostId) -> Option<&Text> {
        self.ghosts.get(&k)
    }

    /// Returns information about the skip in the byte `at`
    ///
    /// * The index in the [`GapBuffer`] where it starts
    /// * The byte where it starts
    /// * Its length
    fn skip_at(&self, at: usize) -> [usize; 3] {
        let [n, b] = self.records.closest_to_by_key(at, |[_, b]| *b);

        if at >= b {
            let iter = {
                let (s0, s1) = self.buf.as_slices();
                let (start, end) = crate::get_ends(n.., self.buf.len());

                let r0 = start.min(s0.len())..end.min(s0.len());
                let r1 = start.saturating_sub(s0.len())..end.saturating_sub(s0.len());

                s0[r0].iter().chain(s1[r1].iter()).enumerate()
            };

            let mut ret = None;
            let mut b = b;
            for (i, skip) in iter.filter_map(|(i, ts)| Some(i).zip(ts.as_skip())) {
                if b + skip > at {
                    ret = Some([n + i, b, skip]);
                    break;
                }
                b += skip;
            }
            ret
        } else {
            let iter = {
                let (s0, s1) = self.buf.as_slices();
                let (start, end) = crate::get_ends(..n, self.buf.len());

                let r0 = start.min(s0.len())..end.min(s0.len());
                let r1 = start.saturating_sub(s0.len())..end.saturating_sub(s0.len());

                s1[r1].iter().rev().chain(s0[r0].iter().rev()).enumerate()
            };

            let mut ret = None;
            let mut b = b;
            for (i, skip) in iter.filter_map(|(i, ts)| Some(i).zip(ts.as_skip())) {
                b -= skip;
                if b <= at {
                    ret = Some([n - (i + 1), b, skip]);
                    break;
                }
            }
            ret
        }
        .unwrap_or({
            let [n, b] = self.records.max();
            [n, b, 0]
        })
    }

    /// The byte in the `at`th position
    fn byte_at(&self, at: usize) -> usize {
        let [n, b] = self.records.closest_to(at);
        if n < at {
            fwd_range(&self.buf, n..at).fold(b, |b, (_, ts)| b + ts.len())
        } else {
            rev_range(&self.buf, at..n).fold(b, |b, (_, ts)| b - ts.len())
        }
    }

    /// Same as [`skip_at`], but takes the previous skip
    ///
    /// This will return the same skip if `b != at`, and the previous
    /// skip otherwise.
    ///
    /// [`skip_at`]: Tags::skip_at
    fn skip_behind(&self, at: usize) -> [usize; 3] {
        let [n, b, skip] = self.skip_at(at);
        if b == at {
            rev_range(&self.buf, ..n)
                .find_map(|(n, ts)| Some(n).zip(ts.as_skip()))
                .map(|(n, skip)| [n, b - skip, skip])
                .unwrap_or([0, 0, 0])
        } else {
            [n, b, skip]
        }
    }

    /// Removes all ranges that start or end in the range
    ///
    /// WILL handle matching [`Tag`]s, WILL handle shifting of ranges,
    /// will NOT handle fully contained ranges.
    fn remove_intersecting_bounds(&mut self, range: Range<usize>, f: impl Fn(&RawTag) -> bool) {
        fn extract(
            sh_from: &mut usize,
            [n_diff, b_diff]: [i32; 2],
            f: impl Fn(usize, usize, &Cell<[usize; 2]>, &RawTag, &RangeId) -> bool,
        ) -> impl FnMut(&mut (Cell<[usize; 2]>, RawTag, RangeId)) -> bool {
            let mut i = 0;
            move |(bound, tag, id)| {
                let [n, b] = bound.get();
                if f(i, *sh_from, bound, tag, id) {
                    if i < *sh_from {
                        *sh_from -= 1;
                    } else {
                        *bound.get_mut() = [sh(n, n_diff), sh(b, b_diff)];
                    }
                    true
                } else {
                    i += 1;
                    false
                }
            }
        }
        let (mut sh_from, diff) = self.bounds_shift_state.get();
        if range.is_empty() {
            return;
        }

        let removed: Vec<[usize; 2]> = {
            let is_contained = extract(&mut sh_from, diff, |i, sh_from, bound, tag, _| {
                let diff = if i < sh_from { 0 } else { diff[1] };
                range.clone().contains(&sh(bound.get()[1], diff)) && f(tag)
            });
            let removed: Vec<_> = self.bounds.extract_if(.., is_contained).collect();

            let mut removed: Vec<[usize; 2]> = removed
                .into_iter()
                .filter_map(|(bound, _, id)| {
                    let is_match = extract(&mut sh_from, diff, |_, _, _, _, lhs| *lhs == id);
                    let [n0, b0] = bound.get();
                    // If there is a matching bound still in self.bounds, it means that
                    // one of the bounds was outside of the range.
                    // When this happens, we are assuming that the caller is not removing
                    // said bound, so we have to do it in this function.
                    // Also, I have to collect beforehand, in order to not remove bounds
                    // while using the sh_bounds closure.
                    self.bounds
                        .extract_if(.., is_match)
                        .next()
                        .map(|(bound, ..)| {
                            let [n1, b1] = bound.get();
                            [[n0, b0], [n1, b1]]
                        })
                })
                .flatten()
                .collect();

            removed.sort_unstable();

            removed
        };
        self.bounds_shift_state.set((sh_from, diff));

        for [n, b] in removed.into_iter().rev() {
            // We remove both bounds, in order to prevent a state of dangling
            // bounds, which would cause a lookback or lookahead over the whole
            // Text.
            let ([rm_n, _], n_diff) = self.remove(n, b);
            self.shift_bounds_and_ranges(rm_n, [n_diff, 0]);
        }
    }

    #[track_caller]
    fn shift_bounds_and_ranges(&mut self, ins_n: usize, [n_diff, b_diff]: [i32; 2]) -> usize {
        ////////// Shifting of bounds
        let (mut sh_from, [mut total_n_diff, mut total_b_diff]) = self.bounds_shift_state.take();
        let bounds_ins = if let Some((first_unshifted, ..)) = self.bounds.get(sh_from) {
            let mut bounds_ins = sh_from;
            let [unshifted_n, _] = first_unshifted.get();
            // cur_n is the first bound that has not been shifted, so we need to
            // take that into consideration.
            if ins_n < sh(unshifted_n, total_n_diff) {
                let mut iter = self.bounds[..sh_from].iter().rev();
                while let Some((bound, ..)) = iter.next()
                    && let [n, b] = bound.get()
                    && n >= ins_n
                {
                    bound.set([sh(n, n_diff), sh(b, b_diff)]);
                    bounds_ins -= 1;
                }
            } else {
                let mut iter = self.bounds[sh_from..].iter();
                // All bounds from cur_n have not been shifted, so we account for
                // that.
                while let Some((bound, ..)) = iter.next()
                    && let [n, b] = bound.get()
                    && sh(n, total_n_diff) < ins_n
                {
                    bound.set([sh(n, total_n_diff), sh(b, total_b_diff)]);
                    bounds_ins += 1;
                    sh_from += 1;
                }
            }
            total_n_diff += n_diff;
            total_b_diff += b_diff;

            bounds_ins
        } else {
            0
        };

        if sh_from < self.bounds.len() {
            self.bounds_shift_state
                .set((sh_from, [total_n_diff, total_b_diff]));
        }

        ////////// Shifting of ranges
        let (mut sh_from, mut total_n_diff) = std::mem::take(&mut self.ranges_shift_state);

        // In this case, since n_diff < 0, it acts much like a taken_range
        // from a Change, so we must merge the ranges that were caught up in
        // ins_n..abs(n_diff)

        if n_diff < 0 {
            let m_range = merging_range_by_guess_and_lazy_shift(
                (&self.ranges_to_update, self.ranges_to_update.len()),
                (0, [ins_n, ins_n + n_diff.unsigned_abs() as usize]),
                (sh_from, total_n_diff, 0, sh),
                (|r| r.start, |r| r.end),
            );

            if sh_from <= m_range.end {
                for range in self.ranges_to_update[sh_from..m_range.end].iter_mut() {
                    range.start = sh(range.start, total_n_diff);
                    range.end = sh(range.end, total_n_diff);
                }
                sh_from = m_range.end;
            }

            let mut iter = self.ranges_to_update[m_range.clone()].iter();
            let first = iter.next().cloned();
            if let Some(first) = first {
                let last = iter.next_back().cloned().unwrap_or(first.clone());
                self.ranges_to_update.splice(m_range.clone(), [
                    // Either last.end <= ins_n, in which case take ins_n, or last.end > ins_n,
                    // needing to be shifted.
                    first.start..sh(last.end, n_diff).max(ins_n),
                ]);
            };

            // If any ranges were taken, at least one was added.
            sh_from -= m_range.clone().count().saturating_sub(1);
        } else if n_diff == 0 {
            return bounds_ins;
        }

        if let Some(unshifted_range) = self.ranges_to_update.get_mut(sh_from) {
            if ins_n < sh(unshifted_range.start, total_n_diff) {
                let mut iter = self.ranges_to_update[..sh_from]
                    .iter_mut()
                    .rev()
                    .flat_map(|range| [&mut range.end, &mut range.start]);
                while let Some(bound) = iter.next()
                    && *bound >= ins_n
                {
                    *bound = sh(*bound, n_diff);
                }
            // The change could happen in the middle of this range.
            } else if ins_n < sh(unshifted_range.end, total_n_diff) {
                unshifted_range.end = sh(unshifted_range.end, n_diff);
                sh_from += 1;
            } else {
                let mut iter = self.ranges_to_update[sh_from..].iter_mut();
                while let Some(range) = iter.next()
                    && sh(range.start, total_n_diff) < ins_n
                {
                    range.start = sh(range.start, total_n_diff);
                    range.end = sh(range.end, total_n_diff);
                    sh_from += 1;
                }
            }

            total_n_diff += n_diff;
        }

        if sh_from < self.ranges_to_update.len() {
            self.ranges_shift_state = (sh_from, total_n_diff);
        }

        bounds_ins
    }

    fn finish_shifting_bounds(&self) {
        let (sh_from, [total_n_diff, total_b_diff]) = self.bounds_shift_state.take();

        for (bound, ..) in self.bounds[sh_from..].iter() {
            let [n, b] = bound.get();
            bound.set([sh(n, total_n_diff), sh(b, total_b_diff)]);
        }
    }

    /// Inserts a callibrated range
    fn merge_range_to_update(&mut self, range: Range<usize>) {
        let (sh_from, total_n_diff) = std::mem::take(&mut self.ranges_shift_state);
        let m_range = merging_range_by_guess_and_lazy_shift(
            (&self.ranges_to_update, self.ranges_to_update.len()),
            (0, [range.start, range.end]),
            (sh_from, total_n_diff, 0, |i, sh| (i as i32 + sh) as usize),
            (|r| r.start, |r| r.end),
        );

        // The ranges in this region have not been callibrated yet.
        if sh_from <= m_range.end {
            for range in self.ranges_to_update[sh_from..m_range.end].iter_mut() {
                range.start = sh(range.start, total_n_diff);
                range.end = sh(range.end, total_n_diff);
            }
        }

        let range = {
            let mut iter = self.ranges_to_update[m_range.clone()].iter();
            let first = iter.next().cloned().unwrap_or(range.clone());
            let last = iter.next_back().cloned().unwrap_or(first.clone());
            first.start.min(range.start)..last.end.max(range.end)
        };

        self.ranges_to_update.splice(m_range.clone(), [range]);

        let ranges_taken = m_range.clone().count();
        let new_sh_from = sh_from.saturating_sub(ranges_taken).max(m_range.start) + 1;
        if new_sh_from < self.ranges_to_update.len() {
            self.ranges_shift_state = (new_sh_from, total_n_diff);
        }
    }

    fn declare_shifted(&mut self, bounds_e_ins: usize) {
        self.bounds_shift_state.set({
            // Either I inserted before sh_from, in which case it moved once
            // ahead, or after sh_from, which would set it to bounds_s_ins, which,
            // after inserting, would be a shifted position, so move once ahead.
            if bounds_e_ins + 1 < self.bounds.len() {
                let (sh_from, diff) = self.bounds_shift_state.get();
                (sh_from + 1, diff)
            } else {
                (0, [0; 2])
            }
        });
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
    #[inline(always)]
    pub fn as_skip(&self) -> Option<usize> {
        match self {
            Self::Skip(v) => Some(*v as usize),
            TagOrSkip::Tag(..) => None,
        }
    }

    /// Returns the [`RawTag`] within, if any
    #[inline(always)]
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

    s1[r1]
        .iter()
        .rev()
        .chain(s0[r0].iter().rev())
        .enumerate()
        .map(move |(n, ts)| (end - (n + 1), ts))
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
fn find_match(buf: &GapBuffer<TagOrSkip>, (n, b, tag): Entry) -> Option<Entry> {
    let mut bound_fn = {
        let mut bounds = 1;
        move |lhs| {
            bounds -= (lhs == tag.inverse().unwrap()) as usize;
            bounds += (lhs == tag) as usize;
            bounds == 0
        }
    };

    if tag.is_start() {
        fwd_range(buf, n..)
            .filter_map(entries_fwd(b))
            .find_map(|(n, b, t)| bound_fn(t).then_some((n, b, t)))
    } else if tag.is_end() {
        rev_range(buf, ..n)
            .filter_map(entries_rev(b))
            .find_map(|(n, b, t)| bound_fn(t).then_some((n, b, t)))
    } else {
        None
    }
}

impl std::fmt::Debug for TagOrSkip {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TagOrSkip::Tag(tag) => write!(f, "{tag:?}"),
            TagOrSkip::Skip(amount) => write!(f, "Skip({amount})"),
        }
    }
}

struct DebugBuf<'a, R: RangeBounds<usize>>(&'a Tags, R);
impl<R: RangeBounds<usize> + Clone> std::fmt::Debug for DebugBuf<'_, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (start, end) = get_ends(self.1.clone(), self.0.len_bytes());
        let [n, mut b, _] = self.0.skip_behind(start);

        if f.alternate() {
            let n_spc = self.0.buf.len().checked_ilog10().unwrap_or(0) as usize + 4;
            let b_spc = self.0.len_bytes().checked_ilog10().unwrap_or(0) as usize + 4;

            let (mut nesting, _): (usize, isize) =
                self.0.buf.iter().take(n).fold((0, 0), |(n, l), ts| {
                    (
                        n.saturating_add_signed(
                            l - ts.as_tag().is_some_and(|tag| tag.is_end()) as isize,
                        ),
                        ts.as_tag().is_some_and(|tag| tag.is_start()) as isize,
                    )
                });

            f.write_str("[\n")?;
            for (n, ts) in self.0.buf.iter().enumerate().skip(n) {
                nesting = nesting.saturating_sub(ts.as_tag().is_some_and(|t| t.is_end()) as usize);
                let space = " ".repeat(nesting);

                let n_fmt = format!("n: {n}");
                let b_fmt = format!("b: {b}");
                writeln!(f, "    ({n_fmt:<n_spc$}, {b_fmt:<b_spc$}): {space}{ts:?}")?;
                nesting += ts.as_tag().is_some_and(|t| t.is_start()) as usize;
                b += ts.len();
                if b >= end {
                    break;
                }
            }
            f.write_str("]")
        } else {
            write!(f, "{:?}", self.0.buf)
        }
    }
}

struct DebugBounds<'a>(&'a Tags);
impl std::fmt::Debug for DebugBounds<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() && !self.0.is_empty() {
            f.write_str("[\n")?;
            let (sh_from, [total_n_diff, total_b_diff]) = self.0.bounds_shift_state.get();
            for (i, (bound, tag, id)) in self.0.bounds.iter().enumerate() {
                let bound = if i < sh_from {
                    bound.get()
                } else {
                    let [n, b] = bound.get();
                    [sh(n, total_n_diff), sh(b, total_b_diff)]
                };
                writeln!(f, "    {:?}", (bound, tag, id))?;
                writeln!(f, "        {:?}", self.0.buf.get(bound[0]))?;
            }
            f.write_str("]")
        } else {
            write!(f, "{:?}", self.0.bounds)
        }
    }
}

impl std::fmt::Debug for Tags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Tags")
            .field("buf", &DebugBuf(self, ..))
            .field("bounds", &DebugBounds(self))
            .field("range_min", &self.range_min)
            .field("records", &self.records)
            .finish_non_exhaustive()
    }
}

impl PartialEq for Tags {
    fn eq(&self, other: &Self) -> bool {
        use RawTag::*;
        use TagOrSkip::*;

        self.buf
            .iter()
            .zip(&other.buf)
            .all(|(lhs, rhs)| match (lhs, rhs) {
                (Tag(PushForm(_, l_id, l_prio)), Tag(PushForm(_, r_id, r_prio))) => {
                    l_id == r_id && l_prio == r_prio
                }
                (Tag(PopForm(_, lhs)), Tag(PopForm(_, rhs))) => lhs == rhs,
                (Tag(Ghost(_, lhs)), Tag(Ghost(_, rhs))) => self.ghosts[lhs] == other.ghosts[rhs],
                (Tag(MainCursor(_)), Tag(MainCursor(_)))
                | (Tag(ExtraCursor(_)), Tag(ExtraCursor(_)))
                | (Tag(StartAlignCenter(_)), Tag(StartAlignCenter(_)))
                | (Tag(EndAlignCenter(_)), Tag(EndAlignCenter(_)))
                | (Tag(StartAlignRight(_)), Tag(StartAlignRight(_)))
                | (Tag(EndAlignRight(_)), Tag(EndAlignRight(_)))
                | (Tag(Spacer(_)), Tag(Spacer(_)))
                | (Tag(StartConceal(_)), Tag(StartConceal(_)))
                | (Tag(EndConceal(_)), Tag(EndConceal(_)))
                | (Tag(ToggleStart(..)), Tag(ToggleStart(..)))
                | (Tag(ToggleEnd(..)), Tag(ToggleEnd(..))) => true,
                (Skip(lhs), Skip(rhs)) => lhs == rhs,
                _ => false,
            })
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

fn sh(n: usize, diff: i32) -> usize {
    (n as i32 + diff) as usize
}
