//! Internal struct for holding [`Tag`]s
//!
//! [`Tag`]s are held internally as [`RawTag`]s, which occupy much
//! less space and can be very cheaply copied around. The
//! [`InnerTags`] struct also makes use of [`Records`] to keep track
//! of positions, as well as [`TagRange`]s to keep track of tags
//! occupying very long ranges of [`Text`].
use std::{
    self,
    iter::Chain,
    ops::{Range, RangeBounds},
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

use RawTag::*;

pub(super) use crate::text::tags::types::RawTag;
pub use crate::text::tags::types::{
    Conceal, FormTag, Inlay, Mask, Overlay, Spacer, Spawn, Tag, TagPart, Toggle, ToggleFn,
};
use crate::{
    Ns,
    context::Handle,
    data::Pass,
    text::{
        Point, Text, TextRangeOrIndex,
        shift_list::{Shift, ShiftList, Shiftable},
        tags::{bounds::Bounds, extents::NsExtents},
    },
    ui::{SpawnId, Widget},
    utils::get_range,
};

mod bounds;
mod extents;
mod types;

/// A public interface for mutating the [`Tag`]s of a [`Text`]
///
/// It lets you modify the tags in expected ways, such as adding and
/// removing [`Tag`]s, without letting you do things like swapping it
/// for another instance.
///
/// This is very useful if you want to be able to add [`Tag`]s while,
/// for example, holding a reference to the [`Strs`] of the [`Text`]
///
/// [`Strs`]: super::Strs
pub struct Tags<'t>(&'t mut InnerTags);

impl Tags<'_> {
    /// Inserts a [`Tag`] at the given position
    ///
    /// Insertion may fail if you try to push a `Tag` to a position or
    /// range which already has the exact same `Tag` with the exact
    /// same `Ns`.
    ///
    /// For some `Tag`s (like [`Inlay`]) can return an id (like
    /// [`InlayId`]). This id can then be used in order to insert the
    /// same `Tag`. In the [`Inlay`] example, that would print the
    /// same ghost [`Text`] multiple times without needing to
    /// pointlessly copy the [`Text`] for every time you want to
    /// insert the same ghost.
    ///
    /// When the `Tag` doesn't return an id, it will return `Some(())`
    /// if the `Tag` was successfully added, and `None` otherwise.
    #[track_caller]
    pub fn insert<Idx>(&mut self, ns: Ns, idx: Idx, tag: impl Tag<Idx>) {
        self.0.insert_inner(ns, idx, tag, false)
    }

    /// Same as [`insert`], but does it after other [`Tags`] of the
    /// same priority
    ///
    /// [`insert`]: Self::insert
    #[track_caller]
    pub fn insert_after<Idx>(&mut self, ns: Ns, idx: Idx, tag: impl Tag<Idx>) {
        self.0.insert_inner(ns, idx, tag, true)
    }

    /// Removes the [`Tag`]s of a [ns] from a region
    ///
    /// The input can either be a byte index, a [`Point`], or a
    /// [range] of byte indices/[`Point`]s. If you are implementing a
    /// [`Buffer`] updating hook through [`BufferUpdated`], it can be
    /// very useful to just "undo" all of the [`Tag`] additions done
    /// by previous updates, you can do that efficiently with this
    /// function:
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// use duat::prelude::*;
    /// setup_duat!(setup);
    ///
    /// fn setup() {
    ///     let ns = Ns::new();
    ///
    ///     hook::add::<BufferUpdated>(move |pa, handle| {
    ///         let buf = handle.write(pa);
    ///         // Removing on the whole Buffer
    ///         buf.text_mut().remove_tags(ns, ..);
    ///         // Logic to add Tags with ns...
    ///     });
    /// }
    /// ```
    ///
    /// [range]: RangeBounds
    /// [`Buffer`]: crate::buffer::Buffer
    /// [`BufferUpdated`]: crate::hook::BufferUpdated
    /// [ns]: Ns
    #[track_caller]
    pub fn remove(&mut self, ns: Ns, from: impl TextRangeOrIndex) {
        let range = from.to_range(self.0.len_bytes() + 1);
        self.0.remove_from(ns, range)
    }

    /// Just like [`Tags::remove`] but excludes ends on the start and
    /// starts on the end
    ///
    /// In the regular [`remove`] function, if you remove from a range
    /// `x..y`, tag ranges that end in `x` or start in `y - 1`
    /// (exclusive range) will also be removed.
    ///
    /// If you don't want that to happen, you can use this function
    /// instead.
    ///
    /// [`remove`]: Self::remove
    #[track_caller]
    pub fn remove_excl(&mut self, ns: Ns, from: impl TextRangeOrIndex) {
        let range = from.to_range(self.0.len_bytes() + 1);
        self.0.remove_from_excl(ns, range);
    }

    /// Like [`Tags::remove`], but removes base on a predicate
    ///
    /// If the function returns `true`, then the tag is removed. Note
    /// that every [`RawTag`] in here is guaranteed to have the same
    /// [`Ns`] as the one passed to the function, so you don't
    /// need to chack for that.
    #[track_caller]
    pub fn remove_if(
        &mut self,
        ns: Ns,
        from: impl TextRangeOrIndex,
        filter: impl FnMut(usize, TagPart) -> bool,
    ) {
        let range = from.to_range(self.0.len_bytes() + 1);
        self.0.remove_if(ns, range, filter)
    }

    /// Removes all [`Tag`]s
    ///
    /// Refrain from using this function on [`Buffer`]s, as there may
    /// be other [`Tag`] providers, and you should avoid measing
    /// with their tags.
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    pub fn clear(&mut self) {
        *self.0 = InnerTags::new(self.0.list.max() as usize);
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
/// It also holds the [`Text`]s of any [`Inlay`]s, and the
/// functions of [`StartToggle`]s
pub struct InnerTags {
    list: ShiftList<(i32, RawTag)>,
    ghosts: Vec<Option<(Ghost, usize)>>,
    toggles: Vec<Option<(Toggle, usize)>>,
    spawns: Vec<SpawnCell>,
    pub(super) spawn_fns: SpawnFns,
    bounds: Bounds,
    extents: NsExtents,
    tags_version: u64,
    meta_tags_version: u64,
}

impl InnerTags {
    /// Returns a [`Tags`] of `self`
    pub(super) fn tags(&mut self) -> Tags<'_> {
        Tags(self)
    }

    /// Creates a new [`InnerTags`] with a given len
    pub fn new(max: usize) -> Self {
        Self {
            list: ShiftList::new(max as i32),
            ghosts: Vec::new(),
            toggles: Vec::new(),
            spawns: Vec::new(),
            spawn_fns: SpawnFns(Vec::new()),
            bounds: Bounds::new(max),
            extents: NsExtents::new(max),
            tags_version: 0,
            meta_tags_version: 0,
        }
    }

    /// Insert a new [`Tag`] at a given [`TextIndex`] or [`TextRange`]
    ///
    /// If the `Tag` is ranged (like [`FormTag`] or
    ///
    /// [`TextIndex`]: super::TextIndex
    /// [`TextRange`]: super::TextRange
    #[track_caller]
    pub(crate) fn insert_inner<T, Idx>(&mut self, ns: Ns, idx: Idx, mut tag: T, after: bool)
    where
        T: Tag<Idx>,
    {
        let (start, end) = tag.get_raw(self, idx, self.len_bytes(), ns);
        let inserted = self.insert_raw(start, end, after);

        if inserted {
            tag.on_insertion(self);
            self.meta_tags_version += T::IS_META as u64;
            self.tags_version += 1;
        }
    }

    fn insert_raw(
        &mut self,
        (s_b, s_tag): (usize, RawTag),
        end: Option<(usize, RawTag)>,
        after: bool,
    ) -> bool {
        let same_prio = |byte: usize| {
            move |(_, (b, tag)): &(_, (i32, RawTag))| {
                byte as i32 == *b && tag.priority() == s_tag.priority()
            }
        };

        if let Some((e_b, e_tag)) = end {
            if s_b >= e_b {
                return false;
            }

            let (s_i, e_i) = {
                let (mut s_i, mut e_i) = match (
                    self.list.find_by_key((s_b as i32, s_tag), |t| t),
                    self.list.find_by_key((e_b as i32, e_tag), |t| t),
                ) {
                    (Ok(_), Ok(_)) => {
                        return false;
                    }
                    (Ok(s_i), Err(e_i)) | (Err(s_i), Ok(e_i)) | (Err(s_i), Err(e_i)) => {
                        (s_i, e_i + 1)
                    }
                };

                if after {
                    s_i += self.list.iter_fwd(s_i..).take_while(same_prio(s_b)).count();
                    e_i += self.list.iter_fwd(e_i..).take_while(same_prio(e_b)).count();
                }

                (s_i, e_i)
            };

            self.list.insert(s_i, (s_b as i32, s_tag));
            self.list.insert(e_i, (e_b as i32, e_tag));

            self.bounds
                .insert([([s_i, s_b], s_tag), ([e_i, e_b], e_tag)]);

            self.extents.insert(s_tag.ns(), s_b);
            self.extents.insert(s_tag.ns(), e_b);

            true
        } else if end.is_none() {
            let i = {
                let (Ok(mut i) | Err(mut i)) = self.list.find_by_key((s_b as i32, s_tag), |s| s);

                if after {
                    i += self.list.iter_fwd(i..).take_while(same_prio(s_b)).count();
                }

                i
            };

            self.list.insert(i, (s_b as i32, s_tag));

            self.bounds.shift_by(i, [1, 0]);

            self.extents.insert(s_tag.ns(), s_b);
            true
        } else {
            false
        }
    }

    /// Insert another [`InnerTags`] into this one
    pub fn insert_tags(&mut self, p: Point, cap: usize, other: &InnerTags) {
        let mut starts = Vec::new();

        for (_, (b, tag)) in other.list.iter_fwd(..) {
            let b = (b as usize).min(cap) + p.byte();
            match tag {
                PushForm(..) | StartConceal(_) | PushMask(..) => starts.push((b, tag)),
                PopForm(..) | EndConceal(_) | PopMask(..) => {
                    let i = starts.iter().rposition(|(_, t)| t.ends_with(&tag)).unwrap();
                    let (sb, stag) = starts.remove(i);
                    if b > sb {
                        self.insert_raw((sb, stag), Some((b, tag)), false);
                    }
                }
                ConcealUntil(_) => unreachable!(),
                RawTag::Inlay(_, mut idx) | RawTag::Overlay(_, mut idx) => {
                    let (ghost, _) = other.ghosts[idx as usize].as_ref().unwrap();
                    idx = reflist_pos(&self.ghosts, ghost) as u32;
                    reflist_insert(&mut self.ghosts, ghost.clone(), idx as usize);

                    self.insert_raw((b, tag), None, false);
                }
                RawTag::StartToggle(_, mut idx) => {
                    let (toggle, _) = other.toggles[idx as usize].as_ref().unwrap();
                    idx = reflist_pos(&self.toggles, toggle) as u32;
                    reflist_insert(&mut self.toggles, toggle.clone(), idx as usize);

                    self.insert_raw((b, tag), None, false);
                }
                RawTag::Spacer(_) | SpawnedWidget(..) | RawTag::EndToggle(..) => {
                    _ = self.insert_raw((b, tag), None, false)
                }
            };
        }
    }

    /// Extends this [`InnerTags`] with another one
    pub fn extend(&mut self, other: InnerTags) {
        self.list.extend(other.list);
        self.ghosts.extend(other.ghosts);
        self.bounds.extend(other.bounds);
        self.extents.extend(other.extents);
    }

    /// Removes all [`RawTag`]s of a given [`Ns`]
    #[track_caller]
    pub(super) fn remove_from(&mut self, ns: Ns, within: Range<usize>) {
        for extent in self.extents.remove(within.clone(), |other| other == ns) {
            self.remove_inner(extent.clone(), |(_, tag), _, _| tag.ns() == ns);
        }
    }

    #[track_caller]
    pub(super) fn remove_from_excl(&mut self, ns: Ns, within: Range<usize>) {
        let mut remained_on = [false; 2];

        for extent in self.extents.remove(within.clone(), |other| other == ns) {
            self.remove_inner(extent.clone(), |(b, tag), _, _| {
                if ns != tag.ns() {
                    return false;
                };

                let removed = (b > within.start as i32 || !tag.is_end())
                    && (b < within.end as i32 || !tag.is_start());

                if !removed {
                    if b == within.start as i32 {
                        remained_on[0] = true;
                    }
                    if b == within.end as i32 {
                        remained_on[1] = true;
                    }
                }

                removed
            });
        }

        if remained_on[0] {
            self.extents.insert(ns, within.start);
        }
        if remained_on[1] {
            self.extents.insert(ns, within.end);
        }
    }

    /// Removes every [`RawTag`] from a range that matches a given
    /// predicate
    pub(super) fn remove_if(
        &mut self,
        ns: Ns,
        within: Range<usize>,
        mut filter: impl FnMut(usize, TagPart) -> bool,
    ) {
        for extent in self.extents.iter_over(within.clone(), ns) {
            self.remove_inner(extent.clone(), |(byte, tag), ghosts, toggles| {
                filter(byte as usize, TagPart::from_raw(tag, ghosts, toggles))
            });
        }
    }

    /// Removes every [`RawTag`] from a range, as well as their
    /// matches
    ///
    /// WILL remove every required [`RawTag`], WILL shift the indices
    /// of the [`Bounds`], WILL NOT shift [`NsExtents`] since
    /// there is no byte shifting, WILL NOT shift the bytes of the
    /// [`Bounds`]
    #[inline(always)]
    fn remove_inner(
        &mut self,
        range: Range<usize>,
        mut filter: impl FnMut(
            (i32, RawTag),
            &[Option<(Ghost, usize)>],
            &[Option<(Toggle, usize)>],
        ) -> bool,
    ) {
        let removed = self
            .bounds
            .remove_intersecting(range.clone(), |entry| {
                filter(entry, &self.ghosts, &self.toggles)
            })
            .into_iter();

        let mut tags_changed = removed.len() > 0;
        let mut meta_tags_changed = false;

        for i in removed.rev() {
            let (_, tag) = self.list.get(i).unwrap();
            meta_tags_changed |= tag.is_meta();
            match tag {
                RawTag::Overlay(_, idx) | RawTag::Inlay(_, idx) => {
                    reflist_remove(&mut self.ghosts, idx as usize)
                }
                StartToggle(_, idx) | EndToggle(_, idx) => {
                    reflist_remove(&mut self.toggles, idx as usize)
                }
                _ => {}
            }

            // We remove both bounds in order to prevent a state of dangling
            // bounds, which would cause a lookback or lookahead over the whole
            // Text.
            self.list.remove(i);
            self.bounds.shift_by(i, [-1, 0]);
        }

        let mut starts = Vec::new();
        let mut ends = Vec::new();
        // For aiding exclusive removal.
        let mut first_and_last_removed_indices = None;

        let (Ok(start) | Err(start)) = self.list.find_by_key(range.start as i32, |(b, _)| b);
        let (Ok(end) | Err(end)) = self.list.find_by_key(range.end as i32, |(b, _)| b);

        self.list
            .extract_if_while(start..end, |_, entry| {
                let remove = filter(entry, &self.ghosts, &self.toggles);
                if remove {
                    match entry.1 {
                        RawTag::Overlay(_, idx) | RawTag::Inlay(_, idx) => {
                            reflist_remove(&mut self.ghosts, idx as usize)
                        }
                        StartToggle(_, idx) | EndToggle(_, idx) => {
                            reflist_remove(&mut self.toggles, idx as usize)
                        }
                        _ => {}
                    }
                }

                Some(remove)
            })
            .for_each(|(i, (_, tag))| {
                self.bounds.shift_by(i, [-1, 0]);

                if let Some((_, last)) = first_and_last_removed_indices.as_mut() {
                    *last = i;
                } else {
                    first_and_last_removed_indices = Some((i, i));
                }

                // Matches for meta tags are already accounted for here, so no need to
                // check ahead.
                meta_tags_changed |= tag.is_meta();
                tags_changed = true;

                if tag.is_start() {
                    starts.push(tag);
                } else if tag.is_end() {
                    if let Some(i) = starts.iter().rposition(|s| s.ends_with(&tag)) {
                        starts.remove(i);
                    } else {
                        ends.push(tag);
                    }
                } else if let RawTag::SpawnedWidget(_, spawn_id) = tag {
                    self.spawns.retain(|spawn_cell| spawn_cell.0 != spawn_id);
                    self.spawn_fns.0.retain(|(id, _)| *id != spawn_id);
                }
            });

        let Some((first, last)) = first_and_last_removed_indices else {
            return;
        };

        self.meta_tags_version += meta_tags_changed as u64;
        self.tags_version += tags_changed as u64;

        self.list
            .extract_if_while(last.., |i, (_, tag)| {
                if let Some(s_i) = starts.iter().rposition(|s| s.ends_with(&tag))
                    && self.bounds.match_of(i).is_none()
                {
                    self.bounds.shift_by(i, [-1, 0]);
                    starts.remove(s_i);
                    Some(true)
                } else if starts.is_empty() {
                    None
                } else {
                    Some(false)
                }
            })
            .for_each(|_| {});

        self.list
            .rextract_if_while(..first, |i, (_, tag)| {
                if let Some(e_i) = ends.iter().rposition(|e| tag.ends_with(e))
                    && self.bounds.match_of(i).is_none()
                {
                    self.bounds.shift_by(i, [-1, 0]);
                    ends.remove(e_i);
                    Some(true)
                } else if ends.is_empty() {
                    None
                } else {
                    Some(false)
                }
            })
            .for_each(|_| {});
    }

    /// Transforms a byte range into another byte range
    ///
    /// This will destroy any [`RawTag`]s contained in the original
    /// range.
    pub fn transform(&mut self, old: Range<usize>, new_end: usize, before: bool) {
        let new = old.start..new_end;

        // Old length removal.
        if old.end > old.start {
            // First, get rid of all ranges that start and/or end in the old
            // range.
            // old.start + 1 because we don't want to ge rid of bounds that merely
            // coincide with the edges.
            self.remove_inner(old.start + 1..old.end, |_, _, _| true);
            self.extents.remove(old.start + 1..old.end, |_| true);

            // If the range becomes empty, we should remove the remaining pairs
            if new.end == old.start
                && let Ok(s_i) = self.list.find_by_key(old.start as i32, |(b, _)| b)
            {
                let mut to_remove: Vec<usize> = Vec::new();
                let mut starts = Vec::new();
                let mut iter = self.list.iter_fwd(s_i..);

                while let Some((i, (b, tag))) = iter.next()
                    && b <= old.end as i32
                {
                    if tag.is_start() {
                        starts.push((i, tag));
                    } else if tag.is_end()
                        && let Some(s_i) = starts.iter().rposition(|(_, s)| s.ends_with(&tag))
                    {
                        let (s_i, _) = starts.remove(s_i);
                        let rm_i = to_remove.iter().take_while(|j| **j < s_i).count();
                        to_remove.insert(rm_i, s_i);
                        let rm_i = to_remove.iter().take_while(|j| **j < i).count();
                        to_remove.insert(rm_i, i)
                    }
                }

                for i in to_remove.into_iter().rev() {
                    self.list.remove(i);
                    self.bounds.remove_if_represented(i);
                    self.bounds.shift_by(i, [-1, 0]);
                }
            }
        }

        let shift = new.len() as i32 - old.len() as i32;
        let from = old.start as i32 + if before { 0 } else { 1 };
        let (Ok(i) | Err(i)) = self.list.find_by_key(from, |(b, _)| b);

        self.list.shift_by(i, shift);
        self.bounds.shift_by(i, [0, shift]);
        self.extents.shift_by(old.start + 1, shift);
    }

    pub fn update_bounds(&mut self) {
        for range in self.bounds.take_ranges() {
            let mut starts = Vec::new();
            for (i, (b, tag)) in self.list.iter_fwd(range.clone()) {
                if tag.is_start() {
                    starts.push((i as i32, b, tag));
                } else if tag.is_end()
                    && let Some(j) = starts.iter().rposition(|(.., lhs)| lhs.ends_with(&tag))
                {
                    let (s_n, s_b, s_tag) = starts.remove(j);
                    self.bounds
                        .represent([([s_n, s_b], s_tag), ([i as i32, b], tag)]);
                }
            }
        }

        self.bounds.cull_small_ranges();
    }

    ////////// Iterator functions

    /// Returns a forward iterator at a given byte
    pub(super) fn fwd_at(&self, b: usize, lookaround: Option<usize>) -> FwdTags<'_> {
        let start = {
            let (Ok(s_i) | Err(s_i)) = self.list.find_by_key(b as i32, |(b, _)| b);
            s_i.saturating_sub(lookaround.unwrap_or(self.bounds.min_len()))
        };

        let bounds = FwdBoundsBefore { iter: self.bounds.iter_fwd(), start };
        let tags = FwdTagsMapper {
            iter: self.list.iter_fwd(start..),
            bounds: &self.bounds,
        };
        bounds.into_iter().chain(tags).peekable()
    }

    /// Returns a reverse iterator at a given byte
    pub(super) fn rev_at(&self, b: usize, lookaround: Option<usize>) -> RevTags<'_> {
        let end = {
            let (Ok(e_i) | Err(e_i)) = self.list.find_by_key(b as i32, |(b, _)| b);
            (e_i + lookaround.unwrap_or(self.bounds.min_len())).min(self.list.len())
        };

        let bounds = RevBoundsAfter { iter: self.bounds.iter_rev(), end };
        let tags = RevTagsMapper {
            iter: self.list.iter_rev(..end),
            bounds: &self.bounds,
        };
        bounds.chain(tags).peekable()
    }

    pub fn tag_parts_fwd(&self, b: usize) -> impl Iterator<Item = (usize, TagPart<'_>)> + '_ {
        let (Ok(s_i) | Err(s_i)) = self.list.find_by_key(b as i32, |(b, _)| b);
        self.list.iter_fwd(s_i..).map(|(_, (b, tag))| {
            (
                b as usize,
                TagPart::from_raw(tag, &self.ghosts, &self.toggles),
            )
        })
    }

    pub fn tag_parts_rev(&self, b: usize) -> impl Iterator<Item = (usize, TagPart<'_>)> + '_ {
        let (Ok(e_i) | Err(e_i)) = self.list.find_by_key(b as i32, |(b, _)| b);
        self.list.iter_rev(..e_i).map(|(_, (b, tag))| {
            (
                b as usize,
                TagPart::from_raw(tag, &self.ghosts, &self.toggles),
            )
        })
    }

    /// Returns an iterator over a single byte
    pub fn iter_only_at(&self, b: usize) -> impl Iterator<Item = TagPart<'_>> + '_ {
        let (Ok(s_i) | Err(s_i)) = self.list.find_by_key(b as i32, |(b, _)| b);
        self.list
            .iter_fwd(s_i..)
            .take_while(move |(_, (cur_b, _))| *cur_b as usize == b)
            .map(|(_, (_, tag))| TagPart::from_raw(tag, &self.ghosts, &self.toggles))
    }

    ////////// Querying functions

    /// The version of the `InnerTags`
    ///
    /// First element is the `tags_version`, second is the
    /// `meta_tags_version`
    pub(crate) fn versions(&self) -> (u64, u64) {
        (self.tags_version, self.meta_tags_version)
    }

    /// Returns true if there are no [`RawTag`]s
    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    /// Returns the len of the [`InnerTags`] in bytes
    pub fn len_bytes(&self) -> usize {
        self.list.max() as usize
    }

    /// Returns the length of all [`Inlay`]s in a byte
    pub fn ghosts_total_at(&self, at: usize) -> Option<Point> {
        self.iter_only_at(at).fold(None, |p, tag| match tag {
            TagPart::Inlay(inlay) => {
                Some(p.map_or(inlay.text().last_point(), |p| p + inlay.text().last_point()))
            }
            _ => p,
        })
    }

    /// A list of all [`SpawnId`]s that belong to this `Tags`
    pub(crate) fn get_spawned_ids(&self) -> impl Iterator<Item = SpawnId> {
        self.spawns.iter().map(|spawn_cell| spawn_cell.0)
    }

    /// a list of all [`ToggleFn`]s surrouding a byte index.
    pub(crate) fn toggles_surrounding(
        &self,
        byte: usize,
    ) -> impl Iterator<Item = (Range<usize>, ToggleFn)> {
        let byte = byte as i32;
        let on_bounds = self.bounds.toggles_surrounding(byte);
        let (Ok(i) | Err(i)) = self.list.find_by_key(byte, |(byte, _)| byte);
        let not_on_bounds = self
            .list
            .iter_fwd(i.saturating_sub(self.bounds.min_len())..self.list.len().min(i + 1))
            .take_while(move |(_, (b, _))| *b <= byte)
            .filter_map(move |(i, (sb, tag))| {
                let RawTag::StartToggle(_, sid) = tag else {
                    return None;
                };
                let eb = self
                    .list
                    .iter_fwd(i + 1..i + self.bounds.min_len())
                    .find_map(|(_, (eb, tag))| {
                        if let RawTag::EndToggle(_, eid) = tag
                            && (eid == sid && eb > byte)
                        {
                            Some(eb)
                        } else {
                            None
                        }
                    })?;

                Some((sb as usize..eb as usize, sid))
            });

        on_bounds.chain(not_on_bounds).map(|(range, idx)| {
            let (toggle, _) = self.toggles[idx as usize].as_ref().unwrap();
            (range, toggle.func.clone())
        })
    }

    /// Get the [`Inlay`] or [`Overlay`] [`Text`] from an index
    pub(super) fn get_ghost(&self, idx: u32) -> &Text {
        self.ghosts[idx as usize].as_ref().unwrap().0.text()
    }
}

impl Clone for InnerTags {
    fn clone(&self) -> Self {
        Self {
            list: self.list.clone(),
            ghosts: self.ghosts.clone(),
            toggles: self.toggles.clone(),
            spawns: Vec::new(),
            spawn_fns: SpawnFns(Vec::new()),
            bounds: self.bounds.clone(),
            extents: self.extents.clone(),
            tags_version: self.tags_version,
            meta_tags_version: self.tags_version,
        }
    }
}

struct DebugBuf<'a, R: RangeBounds<usize>>(&'a InnerTags, R);
impl<R: RangeBounds<usize> + Clone> std::fmt::Debug for DebugBuf<'_, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let range = get_range(self.1.clone(), self.0.len_bytes());

        if f.alternate() {
            let n_spc = self.0.list.len().checked_ilog10().unwrap_or(0) as usize + 4;
            let b_spc = self.0.len_bytes().checked_ilog10().unwrap_or(0) as usize + 4;

            let mut nesting: usize = 0;

            f.write_str("[\n")?;

            for (i, (b, tag)) in self.0.list.iter_fwd(..) {
                nesting = nesting.saturating_sub(tag.is_end() as usize);
                if (range.start as u32..=range.end as u32).contains(&(b as u32)) {
                    let space = " ".repeat(nesting);
                    let n_txt = format!("n: {i}");
                    let b_txt = format!("b: {b}");
                    writeln!(f, "    ({n_txt:<n_spc$}, {b_txt:<b_spc$}): {space}{tag:?}")?;
                }
                nesting += tag.is_start() as usize;
            }

            f.write_str("]")
        } else {
            write!(f, "{:?}", self.0.list)
        }
    }
}

impl std::fmt::Debug for InnerTags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InnerTags")
            .field("buf", &DebugBuf(self, ..))
            .field("bounds", &bounds::DebugBounds(self))
            .finish_non_exhaustive()
    }
}

impl PartialEq for InnerTags {
    fn eq(&self, other: &Self) -> bool {
        use RawTag::*;

        self.list.iter_fwd(..).zip(other.list.iter_fwd(..)).all(
            |((_, (l_b, l_tag)), (_, (r_b, r_tag)))| match (l_tag, r_tag) {
                (PushForm(_, l_id, l_prio), PushForm(_, r_id, r_prio)) => {
                    l_id == r_id && l_prio == r_prio && l_b == r_b
                }
                (PopForm(_, lhs), PopForm(_, rhs)) => lhs == rhs && l_b == r_b,
                (Inlay(_, lhs), Inlay(_, rhs)) | (Overlay(_, lhs), Overlay(_, rhs)) => {
                    let self_ghost = &self.ghosts[lhs as usize];
                    let other_ghost = &other.ghosts[rhs as usize];
                    self_ghost == other_ghost && l_b == r_b
                }
                (Spacer(_), Spacer(_))
                | (StartConceal(_), StartConceal(_))
                | (EndConceal(_), EndConceal(_)) => l_b == r_b,
                (PushMask(_, l_id), PushMask(_, r_id)) => l_id == r_id && l_b == r_b,
                (PopMask(_, lhs), PopMask(_, rhs)) => lhs == rhs && l_b == r_b,
                _ => false,
            },
        )
    }
}
impl Eq for InnerTags {}

/// A forward [`Iterator`] of [`RawTag`]s
///
/// This iterator automatically takes into account [`TagRange`]s and
/// iterates their bounds as if they were regular [`RawTag`]s
pub(super) type FwdTags<'a> = std::iter::Peekable<Chain<FwdBoundsBefore<'a>, FwdTagsMapper<'a>>>;

#[derive(Debug, Clone)]
pub(super) struct FwdTagsMapper<'a> {
    iter: crate::text::shift_list::IterFwd<'a, (i32, RawTag)>,
    bounds: &'a Bounds,
}

impl Iterator for FwdTagsMapper<'_> {
    type Item = (usize, RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(n, (b, tag))| match tag {
            StartConceal(ns) => match self.bounds.match_of(n) {
                Some(([_, e_b], _)) => (b as usize, ConcealUntil(e_b as u32)),
                _ => (b as usize, StartConceal(ns)),
            },
            tag => (b as usize, tag),
        })
    }
}

#[derive(Debug, Clone)]
pub(super) struct FwdBoundsBefore<'a> {
    iter: self::bounds::IterFwd<'a>,
    start: usize,
}

impl Iterator for FwdBoundsBefore<'_> {
    type Item = (usize, RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        let start = self.start;
        let is_before = move |([n, b], tag): ([usize; 2], _)| (n < start).then_some((b, tag));
        self.iter.next().and_then(is_before)
    }
}

/// A reverse [`Iterator`] of [`RawTag`]s
///
/// This iterator automatically takes into account [`TagRange`]s and
/// iterates their bounds as if they were regular [`RawTag`]s
pub type RevTags<'a> = std::iter::Peekable<Chain<RevBoundsAfter<'a>, RevTagsMapper<'a>>>;

#[derive(Debug, Clone)]
pub(super) struct RevTagsMapper<'a> {
    iter: crate::text::shift_list::IterRev<'a, (i32, RawTag)>,
    bounds: &'a Bounds,
}

impl Iterator for RevTagsMapper<'_> {
    type Item = (usize, RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(n, (b, tag))| match tag {
            EndConceal(ns) => match self.bounds.match_of(n) {
                Some(([_, s_b], _)) => (b as usize, ConcealUntil(s_b as u32)),
                _ => (b as usize, EndConceal(ns)),
            },
            tag => (b as usize, tag),
        })
    }
}

#[derive(Debug, Clone)]
pub(super) struct RevBoundsAfter<'a> {
    iter: self::bounds::IterRev<'a>,
    end: usize,
}

impl Iterator for RevBoundsAfter<'_> {
    type Item = (usize, RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        let end = self.end;
        let is_after = move |([n, b], tag): ([usize; 2], _)| (n > end).then_some((b, tag));
        self.iter.next().and_then(is_after)
    }
}

impl Shiftable for (i32, RawTag) {
    type Shift = i32;

    fn shift(self, by: Self::Shift) -> Self {
        (self.0 + by, self.1)
    }
}

impl Shift for i32 {
    fn neg(self) -> Self {
        -self
    }

    fn add(self, other: Self) -> Self {
        self + other
    }
}

/// A destructor for spawned `Widget`s
struct SpawnCell(SpawnId, Arc<AtomicBool>);

impl Drop for SpawnCell {
    fn drop(&mut self) {
        self.1.store(true, Ordering::Relaxed);
        crate::context::windows().queue_close_spawned(self.0);
    }
}

pub(super) struct SpawnFns(
    pub(super)  Vec<(
        SpawnId,
        Box<dyn FnOnce(&mut Pass, usize, Handle<dyn Widget>) + Send>,
    )>,
);

/// SAFETY: The function are only ever used when there's access to a
/// Pass Besides, does Sync even apply to FnOnce? isn't it impossible
/// to do anything with an &FnOnce value?
unsafe impl Sync for SpawnFns {}

/// Either an [`Inlay`] or an [`Overlay`].
#[derive(Clone, Debug, PartialEq, Eq)]
enum Ghost {
    Inlay(Inlay),
    Overlay(Overlay),
}

impl Ghost {
    fn text(&self) -> &Text {
        match self {
            Ghost::Inlay(inlay) => inlay.text(),
            Ghost::Overlay(overlay) => overlay.text(),
        }
    }
}

impl PartialEq<Inlay> for Ghost {
    fn eq(&self, other: &Inlay) -> bool {
        match self {
            Ghost::Inlay(inlay) => inlay == other,
            Ghost::Overlay(_) => false,
        }
    }
}

impl PartialEq<Overlay> for Ghost {
    fn eq(&self, other: &Overlay) -> bool {
        match self {
            Ghost::Overlay(overlay) => overlay == other,
            Ghost::Inlay(_) => false,
        }
    }
}

/// Returns a position that's either equal and occupied, not occupied,
/// or not yet pushed.
fn reflist_pos<T, U>(list: &[Option<(T, usize)>], value: &U) -> usize
where
    T: PartialEq<U>,
{
    if let Some(idx) = list
        .iter()
        .position(|entry| entry.as_ref().is_some_and(|(t, _)| t == value))
    {
        idx
    } else {
        list.iter()
            .position(|entry| entry.is_none())
            .unwrap_or(list.len())
    }
}

/// Add a value to the reflist or increment a refcount.
fn reflist_insert<T>(list: &mut Vec<Option<(T, usize)>>, value: T, idx: usize) {
    match list.get_mut(idx) {
        Some(Some((_, refcount))) => *refcount += 1,
        Some(ghost @ None) => *ghost = Some((value, 1)),
        None => list.push(Some((value, 1))),
    }
}

/// Remove a value from the reflist or decrement a refcount.
fn reflist_remove<T>(list: &mut [Option<(T, usize)>], idx: usize) {
    match list.get_mut(idx) {
        Some(entry @ Some((_, 1))) => *entry = None,
        Some(Some((_, refcount))) => *refcount -= 1,
        Some(None) | None => {}
    }
}
