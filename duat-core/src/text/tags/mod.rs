//! Internal struct for holding [`Tag`]s
//!
//! [`Tag`]s are held internally as [`RawTag`]s, which occupy much
//! less space and can be very cheaply copied around. The
//! [`InnerTags`] struct also makes use of [`Records`] to keep track
//! of positions, as well as [`TagRange`]s to keep track of tags
//! occupying very long ranges of [`Text`].
mod bounds;
mod taggers;
mod types;

use std::{
    self,
    ops::{Range, RangeBounds},
};

use gapbuf::{GapBuffer, gap_buffer};

use self::{
    bounds::Bounds,
    taggers::TaggerExtents,
    types::{TagId, Toggle},
};
pub use self::{
    ids::*,
    taggers::{Tagger, Taggers},
    types::{
        AlignCenter, AlignLeft, AlignRight, Conceal, ExtraCaret, FormTag, Ghost, MainCaret,
        RawTag::{self, *},
        Spacer, Tag,
    },
};
use super::{Point, Text, TextRangeOrPoint, records::Records};
use crate::get_ends;

/// A public interface for mutating the [`Tag`]s of a [`Text`]
///
/// It lets you modify the tags in expected ways, such as adding and
/// removing [`Tag`]s, without letting you do things like swapping it
/// for another instance.
///
/// This is very useful if you want to be able to add [`Tag`]s while,
/// for example, holding a reference to the [`Bytes`] of the [`Text`]
///
/// [`Bytes`]: super::Bytes
pub struct Tags<'a>(pub(super) &'a mut InnerTags);

impl Tags<'_> {
    /// Inserts a [`Tag`] at the given position
    pub fn insert<R>(&mut self, tagger: Tagger, r: R, tag: impl Tag<R>) {
        self.0.insert(tagger, r, tag);
    }

    /// Removes the [`Tag`]s of a [tagger] from a region
    ///
    /// The input can either be a byte index, a [`Point`], or a
    /// [range] of byte indices/[`Point`]s. If you are
    /// implementing a [`Parser`] that applies [`Tag`]s to the
    /// [`Text`] [when changes happen]/[on updates], you can "refresh"
    /// those [`Tag`]s in a very efficient way -- even in very large
    /// files -- just by doing this:
    ///
    /// ```rust
    /// use std::ops::Range;
    ///
    /// use duat_core::{prelude::*, text::Point};
    ///
    /// struct MyParser {
    ///     tagger: Tagger,
    /// }
    ///
    /// impl<U: Ui> Parser<U> for MyParser {
    ///     fn update_range(&mut self, mut parts: FileParts<U>, within: Option<Range<Point>>) {
    ///         // Removing on the whole File
    ///         parts.tags.remove(self.tagger, ..);
    ///         // Logic to add InnerTags...
    ///     }
    /// }
    /// ```
    ///
    /// [tagger]: Taggers
    /// [range]: RangeBounds
    /// [`Parser`]: crate::file::Parser
    /// [when changes happen]: crate::file::Parser::parse
    /// [on updates]: crate::file::Parser::update_range
    pub fn remove(&mut self, taggers: impl Taggers, range: impl TextRangeOrPoint) {
        let range = range.to_range(self.0.len_bytes());
        self.0.remove_from(taggers, range)
    }

    /// Removes all [`Tag`]s
    ///
    /// Refrain from using this function on [`File`]s, as there may be
    /// other [`Tag`] providers, and you should avoid measing with
    /// their tags.
    ///
    /// [`File`]: crate::file::File
    pub fn clear(&mut self) {
        *self.0 = InnerTags::new(self.0.len_bytes());
    }
}

impl std::ops::Deref for Tags<'_> {
    type Target = InnerTags;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl std::fmt::Debug for Tags<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// The struct that holds the [`RawTag`]s of the [`Text`]
///
/// It also holds the [`Text`]s of any [`Ghost`]s, and the
/// functions of [`ToggleStart`]s
#[derive(Clone)]
pub struct InnerTags {
    buf: GapBuffer<TagOrSkip>,
    ghosts: Vec<(GhostId, Text)>,
    toggles: Vec<(ToggleId, Toggle)>,
    records: Records<[usize; 2]>,
    bounds: Bounds,
    extents: TaggerExtents,
}

impl InnerTags {
    /// Creates a new [`InnerTags`] with a given len
    pub fn new(len: usize) -> Self {
        Self {
            buf: if len == 0 {
                GapBuffer::new()
            } else {
                gap_buffer![TagOrSkip::Skip(len as u32)]
            },
            ghosts: Vec::new(),
            toggles: Vec::new(),
            records: Records::with_max([(len != 0) as usize, len]),
            bounds: Bounds::new(),
            extents: TaggerExtents::default(),
        }
    }

    /// Removes all [`RawTag`]s and sets the len to 0
    pub fn clear(&mut self) {
        self.buf = GapBuffer::new();
        self.ghosts.clear();
        self.toggles.clear();
        self.records.clear();
        self.bounds = Bounds::new();
        self.extents = TaggerExtents::default();
    }

    /// Insert a new [`Tag`] at a given byte
    pub fn insert<R>(&mut self, tagger: Tagger, r: R, tag: impl Tag<R>) -> Option<ToggleId> {
        fn exists_at(tags: &GapBuffer<TagOrSkip>, n: usize, tag: RawTag) -> bool {
            rev_range(tags, ..n)
                .map_while(|(_, ts)| ts.as_tag())
                .any(|lhs| lhs == tag)
        }

        fn insert_id(tags: &mut InnerTags, id: Option<TagId>) -> Option<ToggleId> {
            match id {
                Some(TagId::Ghost(id, ghost)) => {
                    tags.ghosts.push((id, ghost));
                    None
                }
                Some(TagId::Toggle(id, toggle)) => {
                    tags.toggles.push((id, toggle));
                    Some(id)
                }
                None => None,
            }
        }

        fn insert_raw_tags(
            tags: &mut InnerTags,
            (s_at, s_tag): (usize, RawTag),
            end: Option<(usize, RawTag)>,
        ) -> bool {
            let [s_n, s_c, s_skip] = tags.skip_at(s_at);

            if let Some((e_at, e_tag)) = end
                && s_at != e_at
            {
                let [e_n, e_c, e_skip] = {
                    // This is very likely the hot path.
                    let [e_n, e_c, skip] = if e_at < s_c + s_skip {
                        // These changes account for a possibly split skip.
                        [s_n, s_at, s_skip - (s_at - s_c)]
                    } else {
                        tags.skip_at(e_at)
                    };

                    if exists_at(&tags.buf, s_n, s_tag) && exists_at(&tags.buf, e_n, e_tag) {
                        return false;
                    }
                    let s_n_diff = if s_at == s_c { 1 } else { 2 };

                    [e_n + s_n_diff, e_c, skip]
                };

                let [s_ins, s_n_diff] = tags.insert_inner(s_at, s_tag, [s_n, s_c, s_skip]);
                let [e_ins, e_n_diff] = tags.insert_inner(e_at, e_tag, [e_n, e_c, e_skip]);

                tags.bounds.insert(tags.buf.len(), [
                    (s_ins, [s_ins + s_n_diff - 1, s_at], s_tag),
                    (e_ins, [e_ins + e_n_diff - 1, e_at], e_tag),
                ]);
            } else if end.is_none() {
                let [ins, n_diff] = tags.insert_inner(s_at, s_tag, [s_n, s_c, s_skip]);
                tags.bounds
                    .shift_by(tags.buf.len(), ins, [n_diff as i32, 0]);
            }

            tags.extents.insert(s_tag.tagger(), s_at);
            if let Some((s_at, _)) = end {
                tags.extents.insert(s_tag.tagger(), s_at);
            }

            true
        }

        let (start, end, tag_id) = tag.decompose(r, self.len_bytes(), tagger);

        if insert_raw_tags(self, start, end) {
            insert_id(self, tag_id)
        } else {
            None
        }
    }

    /// Inserts a [`RawTag`] in `at`, returns how many [`TagOrSkip`]s
    /// were added
    ///
    /// Will NOT handle bounds creation, WILL transform [`Records`].
    fn insert_inner(&mut self, at: usize, tag: RawTag, [n, c, skip]: [usize; 3]) -> [usize; 2] {
        if at == c {
            let n = n - rev_range(&self.buf, ..n)
                .take_while(|(_, ts)| ts.as_tag().is_some_and(|t| t.priority() > tag.priority()))
                .count();

            self.buf.insert(n, TagOrSkip::Tag(tag));
            self.records.transform([n, c], [0, 0], [1, 0]);
            self.records.insert([n, c]);
            [n, 1]
        } else {
            self.buf.splice(n..=n, [
                TagOrSkip::Skip((at - c) as u32),
                TagOrSkip::Tag(tag),
                TagOrSkip::Skip((c + skip - at) as u32),
            ]);
            self.records.transform([n + 1, at], [0, 0], [2, 0]);
            self.records.insert([n + 1, at]);
            [n, 2]
        }
    }

    /// Insert another [`InnerTags`] into this one
    pub fn insert_tags(&mut self, p: Point, mut other: InnerTags) {
        let mut starts = Vec::new();

        for (_, c, tag) in fwd_range(&other.buf, ..).filter_map(entries_fwd(p.byte())) {
            match tag {
                PushForm(..) => starts.push((c, tag)),
                PopForm(tagger, id) => {
                    let i = starts.iter().rposition(|(_, t)| t.ends_with(&tag)).unwrap();
                    let (sb, _) = starts.remove(i);
                    self.insert(tagger, sb..c, id.to_tag(tag.priority()));
                }
                RawTag::MainCaret(tagger) => {
                    self.insert(tagger, c, MainCaret);
                }
                RawTag::ExtraCaret(tagger) => {
                    self.insert(tagger, c, ExtraCaret);
                }
                StartAlignCenter(_) => starts.push((c, tag)),
                EndAlignCenter(tagger) => {
                    let i = starts.iter().rposition(|(_, t)| t.ends_with(&tag)).unwrap();
                    let (sb, _) = starts.remove(i);
                    self.insert(tagger, sb..c, AlignCenter);
                }
                StartAlignRight(_) => starts.push((c, tag)),
                EndAlignRight(tagger) => {
                    let i = starts.iter().rposition(|(_, t)| t.ends_with(&tag)).unwrap();
                    let (sb, _) = starts.remove(i);
                    self.insert(tagger, sb..c, AlignRight);
                }
                RawTag::Spacer(tagger) => {
                    self.insert(tagger, c, Spacer);
                }
                StartConceal(_) => starts.push((c, tag)),
                EndConceal(tagger) => {
                    let i = starts.iter().rposition(|(_, t)| t.ends_with(&tag)).unwrap();
                    let (sb, _) = starts.remove(i);
                    self.insert(tagger, sb..c, Conceal);
                }
                ConcealUntil(_) => unreachable!(),
                RawTag::Ghost(tagger, id) => {
                    let entry = other.ghosts.extract_if(.., |(l, _)| l == &id).next();
                    self.insert(tagger, c, Ghost(entry.unwrap().1));
                }
                StartToggle(..) => todo!(),
                EndToggle(..) => todo!(),
            };
        }
    }

    /// Extends this [`InnerTags`] with another one
    pub fn extend(&mut self, mut other: InnerTags) {
        self.bounds.finish_shifting();

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
            let [n, c] = bound.get();
            bound.set([n + self.buf.len() - taken_from_start, c + self.len_bytes()]);
        }

        self.buf.extend(&other.buf);
        self.ghosts.extend(other.ghosts);
        self.toggles.extend(other.toggles);
        self.records.extend(other.records);
        self.bounds.extend(other.bounds);
    }

    /// Removes all [`RawTag`]s of a give [`Taggers`]
    pub fn remove_from(&mut self, taggers: impl Taggers, within: impl RangeBounds<usize>) {
        let (start, end) = crate::get_ends(within, self.len_bytes());

        for range in self
            .extents
            .remove_range(start..end, self.buf.len(), |tagger| {
                taggers.contains_tagger(tagger)
            })
        {
            self.remove_from_range(&taggers, range);
        }
    }

    fn remove_from_range(&mut self, taggers: &impl Taggers, range: Range<usize>) {
        let (start, end) = (range.start, range.end);

        // It is best to do this first, so getting skips returns correct
        // entries.
        self.remove_intersections(start..end, |tag| {
            taggers.clone().contains_tagger(tag.tagger())
        });

        let [mut n, mut c, _] = self.skip_behind(start);
        let (mut initial_n, mut initial_c) = (n, c);

        let mut removed = 0;
        let mut starts = Vec::new();

        while c < end {
            let ts = self.buf.get(n).copied();
            if let Some(TagOrSkip::Skip(skip)) = ts {
                if removed > 0 {
                    self.records.transform([n, c], [removed, 0], [0, 0]);

                    // Try to merge this skip with the previous one.
                    if let Some(prev_n) = n.checked_sub(1)
                        && let Some(TagOrSkip::Skip(prev_skip)) = self.buf.get(prev_n)
                    {
                        self.buf
                            .splice(prev_n..=n, [TagOrSkip::Skip(prev_skip + skip)]);
                        self.records.transform([n, c], [1, 0], [0, 0]);
                        self.bounds.shift_by(self.buf.len(), n, [-1, 0]);
                    } else {
                        n += 1;
                    }

                    removed = 0;
                } else {
                    n += 1;
                }

                c += skip as usize;
            } else if let Some(TagOrSkip::Tag(tag)) = ts {
                if !taggers.clone().contains_tagger(tag.tagger()) {
                    n += 1;
                    continue;
                }

                self.buf.remove(n);
                self.bounds.shift_by(self.buf.len(), n, [-1, 0]);

                // The handled removed RawTag is always before the range.
                let ([rm_n, rm_c], n_diff) =
                    self.handle_removed_tag((c, tag), initial_n, initial_c, &mut starts);
                if n_diff != 0 {
                    n = sh(n, n_diff);
                    initial_n = sh(initial_n, n_diff);
                    if initial_n == rm_n {
                        initial_c = rm_c
                    }
                    self.bounds.shift_by(self.buf.len(), rm_n, [n_diff, 0]);
                }

                removed += 1;
            } else {
                if removed > 0 {
                    self.records.transform([n, c], [removed, 0], [0, 0]);
                }
                break;
            }
        }

        // At this point in time, n should point to the index of the first
        // thing that didn't match the byte constraint, so too should c point
        // to where this element is.
        // that I don't really care about optimizing it tbh.
        for (_, tag) in starts {
            let (n1, b1, _) = find_match(&self.buf, (n, c, tag)).unwrap();
            let ([rm_n, rm_c], n_diff) = self.remove(n1, b1);

            if n >= rm_n {
                c = rm_c;
                n = rm_n;
            }

            self.bounds.shift_by(self.buf.len(), rm_n, [n_diff, 0]);
        }
    }

    /// Removes bounds from a given [`Range`], given a predicate
    #[track_caller]
    fn remove_intersections(&mut self, range: Range<usize>, filter: impl Fn(&RawTag) -> bool) {
        let removed = self.bounds.remove_intersecting(range, filter);

        for [n, c] in removed.into_iter().rev() {
            // We remove both bounds in order to prevent a state of dangling
            // bounds, which would cause a lookback or lookahead over the whole
            // Text.
            let ([rm_n, _], n_diff) = self.remove(n, c);
            self.bounds.shift_by(self.buf.len(), rm_n, [n_diff, 0]);
        }
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
    fn remove(&mut self, n: usize, c: usize) -> ([usize; 2], i32) {
        let TagOrSkip::Tag(_) = self.buf.remove(n) else {
            unreachable!("You are only supposed to remove InnerTags like this, not skips")
        };

        // Try to merge this skip with the previous one.
        if let Some(&TagOrSkip::Skip(r_skip)) = self.buf.get(n)
            // We may be at the very start
            && let Some(l_n) = n.checked_sub(1)
            && let Some(&TagOrSkip::Skip(l_skip)) = self.buf.get(l_n)
        {
            self.buf.splice(l_n..=n, [TagOrSkip::Skip(l_skip + r_skip)]);
            self.records.transform([n, c], [2, 0], [0, 0]);
            ([l_n, c - l_skip as usize], -2)
        } else {
            self.records.transform([n, c], [1, 0], [0, 0]);
            ([n, c], -1)
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
                // For now, since ending InnerTags are not shifted
                // forwards, there is no need to use sh_bounds here.
            }

            return;
        };
        // You might wonder: Why not return if old == new?
        // It is assumed that a removal must get rid of all tags within, so
        // returning would prevent that.

        // Old length removal.
        let [s_n, s_c] = if old.end > old.start {
            // First, get rid of all ranges that start and/or end in the old
            // range.
            // old.start + 1 because we don't want to get rid of bounds that
            // merely coincide with the edges.
            self.remove_intersections(old.start + 1..old.end, |_| true);
            self.extents
                .remove_range(old.start + 1..old.end, self.buf.len(), |_| true);

            let [s_n, s_c, s_skip] = self.skip_at(old.start);

            let b_diff = old.end - old.start;
            // If the range to be removed is contained within one skip,
            // there is no need to check for where it ends.
            let [e_n, e_c] = if old.end <= s_c + s_skip {
                [s_n, s_c + s_skip]
            } else {
                // The check for the final skip is a `skip_behind` because we don't
                // want to remove one skip ahead of the end in the cases of
                // `old.start + len == some_skip`, since that would remove the tags at
                // the end of the range.
                let [n, c, skip] = self.skip_behind(old.end);
                [n, c + skip]
            };

            let skip = (e_c - s_c - b_diff) as u32;
            let added_n = (skip > 0) as usize;
            let rm: Vec<(usize, TagOrSkip)> = self
                .buf
                .splice(s_n..=e_n, (skip > 0).then_some(TagOrSkip::Skip(skip)))
                .scan(s_c, |c, ts| {
                    *c += ts.len();
                    Some((*c - ts.len(), ts))
                })
                .collect();

            let (mut s_n, mut s_c) = (s_n, s_c);
            self.records
                .transform([s_n, s_c], [rm.len(), b_diff], [added_n, 0]);
            self.bounds.shift_by(self.buf.len(), s_n, [
                added_n as i32 - rm.len() as i32,
                -(b_diff as i32),
            ]);

            // We can do the same thing that is done in remove_from, since we are
            // also "removing from".
            let mut starts = Vec::new();
            for (c, tag) in rm
                .into_iter()
                .filter_map(|(c, ts)| ts.as_tag().map(|t| (c, t)))
            {
                let ([rm_n, rm_c], diff) = self.handle_removed_tag((c, tag), s_n, s_c, &mut starts);
                if diff != 0 {
                    s_n = (s_n as i32 + diff) as usize;
                    if s_n == rm_n {
                        s_c = rm_c;
                    }
                    self.bounds.shift_by(self.buf.len(), rm_n, [diff, 0]);
                }
            }

            for (_, tag) in starts {
                let (n, c, _) =
                    find_match(&self.buf, (s_n + added_n, s_c + skip as usize, tag)).unwrap();
                let ([rm_n, _], diff) = self.remove(n, c);
                self.bounds.shift_by(self.buf.len(), rm_n, [diff, 0]);
            }

            // If the range becomes empty, we should remove the remainig pairs
            if skip == 0 && new.end == old.start {
                let mut to_remove: Vec<usize> = Vec::new();
                let mut starts = Vec::new();

                let s_n = s_n
                    - rev_range(&self.buf, ..s_n)
                        .take_while(|(_, ts)| ts.is_tag())
                        .count();

                for (n, tag) in fwd_range(&self.buf, s_n..)
                    .map_while(|(i, ts)| Some(i).zip(ts.as_tag()))
                    .filter(|(_, tag)| tag.is_start() || tag.is_end())
                {
                    if tag.is_start() {
                        starts.push((n, tag));
                    } else if tag.is_end()
                        && let Some(i) = starts.iter().position(|(_, lhs)| lhs.ends_with(&tag))
                        && let (s_n, _) = starts.remove(i)
                        && let None | Some(true) = self.bounds.remove_range(s_n..n)
                    {
                        for n in [n, s_n] {
                            let ins = to_remove.iter().take_while(|j| **j < n).count();
                            to_remove.insert(ins, n);
                        }
                    }
                }

                self.records
                    .transform([s_n, s_c], [to_remove.len(), 0], [0, 0]);

                for n in to_remove.into_iter().rev() {
                    self.buf.remove(n);
                    self.bounds.shift_by(self.buf.len(), n, [-1, 0]);
                }
            }

            [s_n, s_c]
        } else {
            let [s_n, s_c, _] = self.skip_at(old.start);
            [s_n, s_c]
        };

        if new.end > old.start {
            let added_c = new.len();
            if let Some(TagOrSkip::Skip(skip)) = self.buf.get_mut(s_n) {
                *skip += added_c as u32;
                self.records.transform([s_n, s_c], [0, 0], [0, added_c]);
                self.bounds
                    .shift_by(self.buf.len(), s_n, [0, added_c as i32]);
            // The skip may have been deleted by now, so we add it
            // back.
            } else {
                self.buf.insert(s_n, TagOrSkip::Skip(added_c as u32));
                self.records.transform([s_n, s_c], [0, 0], [1, added_c]);
                self.bounds
                    .shift_by(self.buf.len(), s_n, [1, added_c as i32]);
            }
        }

        self.extents
            .shift_by(old.start, new.len() as i32 - old.len() as i32);
    }

    fn handle_removed_tag(
        &mut self,
        (c, tag): (usize, RawTag),
        initial_n: usize,
        initial_c: usize,
        starts: &mut Vec<(usize, RawTag)>,
    ) -> ([usize; 2], i32) {
        if tag.is_start() {
            starts.push((c, tag));
        } else if tag.is_end() {
            if let Some(i) = starts.iter().rposition(|(_, lhs)| lhs.ends_with(&tag)) {
                starts.remove(i);
            // If the starting Tag wasn't here, it was
            // before start, so we need to look for it.
            } else {
                // Search from initial positions, to skip iterating through the range.
                let (n, c, _) = find_match(&self.buf, (initial_n, initial_c, tag)).unwrap();
                // Here, we'll shift all ranges ahead, since this happens so
                // infrequently that I don't care about the performance impact.
                return self.remove(n, c);
            }
        }

        ([0; 2], 0)
    }

    pub(crate) fn update_bounds(&mut self) {
        self.bounds.finish_shifting();

        for range in self.bounds.take_ranges() {
            let c = self.byte_at(range.start);
            // Add all tags in the range to the list, keeping only those whose
            // ranges are too long.
            let mut start_tags = Vec::new();
            for (n, c, tag) in fwd_range(&self.buf, range).filter_map(entries_fwd(c)) {
                if tag.is_start() {
                    start_tags.push((n, c, tag));
                } else if tag.is_end()
                    && let Some(i) = start_tags.iter().rposition(|(.., lhs)| lhs.ends_with(&tag))
                {
                    let (s_n, s_c, s_tag) = start_tags.remove(i);
                    self.bounds.represent([([s_n, s_c], s_tag), ([n, c], tag)]);
                }
            }
        }

        self.bounds.cull_small_ranges();
    }

    /// Returns true if there are no [`RawTag`]s
    pub fn is_empty(&self) -> bool {
        self.buf.len() == 0 || self.buf.len() == 1 && self.buf[0].is_skip()
    }

    /// Returns the len of the [`InnerTags`] in bytes
    pub fn len_bytes(&self) -> usize {
        self.records.max()[1]
    }

    /// Returns a forward iterator at a given byte
    #[define_opaque(FwdTags)]
    pub fn fwd_at(&self, at: usize) -> FwdTags<'_> {
        self.bounds.finish_shifting();

        let [s_n, s_c] = {
            let [n, c, _] = self.skip_behind(at.min(self.len_bytes()));
            rev_range(&self.buf, ..n)
                .take(self.bounds.min_len())
                .fold([n, c], |[_, c], (n, ts)| [n, c - ts.len()])
        };

        let bounds = self.bounds.iter().map_while(move |(bound, tag)| {
            let [n, c] = bound.get();
            (n < s_n).then_some((c, tag))
        });

        let tags = {
            let iter = fwd_range(&self.buf, s_n..).filter_map(entries_fwd(s_c));
            iter.map(|(n, c, tag)| match tag {
                StartConceal(tagger) => match self.bounds.match_of(n) {
                    Some(([_, e_c], _)) => (c, ConcealUntil(e_c as u32)),
                    _ => (c, StartConceal(tagger)),
                },
                tag => (c, tag),
            })
        };

        bounds.into_iter().chain(tags).peekable()
    }

    /// Returns a reverse iterator at a given byte
    #[define_opaque(RevTags)]
    pub fn rev_at(&self, at: usize) -> RevTags<'_> {
        self.bounds.finish_shifting();

        let [e_n, e_c, _] = {
            let [n, c, _] = self.skip_at(at.min(self.len_bytes()));
            fwd_range(&self.buf, n..)
                .take(self.bounds.min_len())
                .fold([n, c, 0], |[_, c, len], (n, ts)| [n, c + len, ts.len()])
        };

        let bounds = self.bounds.iter().rev().map_while(move |(bound, tag)| {
            let [n, c] = bound.get();
            (n > e_n).then_some((c, tag))
        });

        let tags = {
            let iter = rev_range(&self.buf, ..e_n).filter_map(entries_rev(e_c));
            iter.map(|(n, c, tag)| match tag {
                EndConceal(tagger) => match self.bounds.match_of(n) {
                    Some(([_, s_c], _)) => (c, ConcealUntil(s_c as u32)),
                    _ => (c, EndConceal(tagger)),
                },
                tag => (c, tag),
            })
        };

        bounds.chain(tags).peekable()
    }

    pub fn raw_fwd_at(&self, c: usize) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        let [n, c, _] = self.skip_behind(c);
        fwd_range(&self.buf, n..)
            .filter_map(entries_fwd(c))
            .map(|(_, c, t)| (c, t))
    }

    pub fn raw_rev_at(&self, c: usize) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        let [n, c, _] = self.skip_at(c);
        rev_range(&self.buf, ..n)
            .filter_map(entries_rev(c))
            .map(|(_, c, t)| (c, t))
    }

    /// Returns an iterator over a single byte
    pub fn iter_only_at(&self, at: usize) -> impl Iterator<Item = RawTag> + '_ {
        self.bounds.finish_shifting();

        let [n, c, _] = self.skip_at(at);
        (c == at)
            .then(|| rev_range(&self.buf, ..n).map_while(|(_, ts)| ts.as_tag()))
            .into_iter()
            .flatten()
    }

    /// Returns the length of all [`Ghost`]s in a byte
    pub fn ghosts_total_at(&self, at: usize) -> Option<Point> {
        self.iter_only_at(at).fold(None, |p, tag| match tag {
            RawTag::Ghost(_, id) => {
                let (_, text) = self.ghosts.iter().find(|(lhs, _)| *lhs == id)?;
                Some(p.map_or(text.len(), |p| p + text.len()))
            }
            _ => p,
        })
    }

    /// Return the [`Text`] of a given [`GhostId`]
    pub fn get_ghost(&self, id: GhostId) -> Option<&Text> {
        self.ghosts
            .iter()
            .find_map(|(lhs, text)| (*lhs == id).then_some(text))
    }

    /// Returns information about the skip in the byte `at`
    ///
    /// * The index in the [`GapBuffer`] where it starts
    /// * The byte where it starts
    /// * Its length
    fn skip_at(&self, at: usize) -> [usize; 3] {
        let [n, c] = self.records.closest_to_by_key(at, |[_, c]| *c);

        if at >= c {
            let iter = {
                let (s0, s1) = self.buf.as_slices();
                let (start, end) = crate::get_ends(n.., self.buf.len());

                let r0 = start.min(s0.len())..end.min(s0.len());
                let r1 = start.saturating_sub(s0.len())..end.saturating_sub(s0.len());

                s0[r0].iter().chain(s1[r1].iter()).enumerate()
            };

            let mut ret = None;
            let mut c = c;
            for (i, skip) in iter.filter_map(|(i, ts)| Some(i).zip(ts.as_skip())) {
                if c + skip > at {
                    ret = Some([n + i, c, skip]);
                    break;
                }
                c += skip;
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
            let mut c = c;
            for (i, skip) in iter.filter_map(|(i, ts)| Some(i).zip(ts.as_skip())) {
                c -= skip;
                if c <= at {
                    ret = Some([n - (i + 1), c, skip]);
                    break;
                }
            }
            ret
        }
        .unwrap_or({
            let [n, c] = self.records.max();
            [n, c, 0]
        })
    }

    /// The byte in the `at`th position
    #[track_caller]
    fn byte_at(&self, at: usize) -> usize {
        let [n, c] = self.records.closest_to(at);
        if n < at {
            fwd_range(&self.buf, n..at).fold(c, |c, (_, ts)| c + ts.len())
        } else {
            rev_range(&self.buf, at..n).fold(c, |c, (_, ts)| c - ts.len())
        }
    }

    /// Same as [`skip_at`], but takes the previous skip
    ///
    /// This will return the same skip if `c != at`, and the previous
    /// skip otherwise.
    ///
    /// [`skip_at`]: InnerTags::skip_at
    fn skip_behind(&self, at: usize) -> [usize; 3] {
        let [n, c, skip] = self.skip_at(at);
        if c == at {
            rev_range(&self.buf, ..n)
                .find_map(|(n, ts)| Some(n).zip(ts.as_skip()))
                .map(|(n, skip)| [n, c - skip, skip])
                .unwrap_or([0, 0, 0])
        } else {
            [n, c, skip]
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
#[track_caller]
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
fn entries_fwd(mut c: usize) -> impl FnMut((usize, &TagOrSkip)) -> Option<Entry> + Clone {
    move |(n, ts)| {
        c += ts.len();
        ts.as_tag().map(|t| (n, c, t))
    }
}

/// Reverse enumerating function for a [`TagOrSkip::Tag`] from a byte
fn entries_rev(mut c: usize) -> impl FnMut((usize, &TagOrSkip)) -> Option<Entry> + Clone {
    move |(n, ts)| {
        c -= ts.len();
        ts.as_tag().map(|t| (n, c, t))
    }
}

/// Look for a match for a [`RawTag`]
fn find_match(buf: &GapBuffer<TagOrSkip>, (n, c, tag): Entry) -> Option<Entry> {
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
            .filter_map(entries_fwd(c))
            .find_map(|(n, c, t)| bound_fn(t).then_some((n, c, t)))
    } else if tag.is_end() {
        rev_range(buf, ..n)
            .filter_map(entries_rev(c))
            .find_map(|(n, c, t)| bound_fn(t).then_some((n, c, t)))
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

struct DebugBuf<'a, R: RangeBounds<usize>>(&'a InnerTags, R);
impl<R: RangeBounds<usize> + Clone> std::fmt::Debug for DebugBuf<'_, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (start, end) = get_ends(self.1.clone(), self.0.len_bytes());
        let [n, mut c, _] = self.0.skip_behind(start);

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
                let b_fmt = format!("c: {c}");
                writeln!(f, "    ({n_fmt:<n_spc$}, {b_fmt:<b_spc$}): {space}{ts:?}")?;
                nesting += ts.as_tag().is_some_and(|t| t.is_start()) as usize;
                c += ts.len();
                if c >= end {
                    break;
                }
            }
            f.write_str("]")
        } else {
            write!(f, "{:?}", self.0.buf)
        }
    }
}

impl std::fmt::Debug for InnerTags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InnerTags")
            .field("buf", &DebugBuf(self, ..))
            .field("bounds", &bounds::DebugBounds(self))
            .field("records", &self.records)
            .finish_non_exhaustive()
    }
}

impl PartialEq for InnerTags {
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
                (Tag(Ghost(_, lhs)), Tag(Ghost(_, rhs))) => {
                    let self_ghost = self
                        .ghosts
                        .iter()
                        .find_map(|(id, text)| (lhs == id).then_some(text));
                    let other_ghost = other
                        .ghosts
                        .iter()
                        .find_map(|(id, text)| (rhs == id).then_some(text));
                    self_ghost == other_ghost
                }
                (Tag(MainCaret(_)), Tag(MainCaret(_)))
                | (Tag(ExtraCaret(_)), Tag(ExtraCaret(_)))
                | (Tag(StartAlignCenter(_)), Tag(StartAlignCenter(_)))
                | (Tag(EndAlignCenter(_)), Tag(EndAlignCenter(_)))
                | (Tag(StartAlignRight(_)), Tag(StartAlignRight(_)))
                | (Tag(EndAlignRight(_)), Tag(EndAlignRight(_)))
                | (Tag(Spacer(_)), Tag(Spacer(_)))
                | (Tag(StartConceal(_)), Tag(StartConceal(_)))
                | (Tag(EndConceal(_)), Tag(EndConceal(_)))
                | (Tag(StartToggle(..)), Tag(StartToggle(..)))
                | (Tag(EndToggle(..)), Tag(EndToggle(..))) => true,
                (Skip(lhs), Skip(rhs)) => lhs == rhs,
                _ => false,
            })
    }
}
impl Eq for InnerTags {}

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

/// Shorthand to shift a [`usize`] by an [`i32`]
fn sh(n: usize, diff: i32) -> usize {
    (n as i32 + diff) as usize
}

mod ids {
    use std::sync::atomic::{AtomicU16, Ordering};

    /// The id of a [ghost text]
    ///
    /// [ghost text]: super::Ghost
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct GhostId(u16);

    impl GhostId {
        /// Creates a new [`GhostId`]
        #[allow(clippy::new_without_default)]
        pub fn new() -> Self {
            static TEXT_COUNT: AtomicU16 = AtomicU16::new(0);
            Self(TEXT_COUNT.fetch_add(1, Ordering::Relaxed))
        }
    }

    impl std::fmt::Debug for GhostId {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "GhostId({})", self.0)
        }
    }

    /// The id of a toggleable
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ToggleId(u16);

    impl ToggleId {
        /// Creates a new [`ToggleId`]
        #[allow(clippy::new_without_default)]
        pub fn new() -> Self {
            static TOGGLE_COUNT: AtomicU16 = AtomicU16::new(0);
            Self(TOGGLE_COUNT.fetch_add(1, Ordering::Relaxed))
        }
    }

    impl std::fmt::Debug for ToggleId {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "ToggleId({})", self.0)
        }
    }
}
