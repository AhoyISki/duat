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

use self::{bounds::Bounds, taggers::TaggerExtents, types::Toggle};
pub use self::{
    ids::*,
    taggers::{Tagger, Taggers},
    types::{
        AlignCenter, AlignLeft, AlignRight, Conceal, ExtraCaret, FormTag, Ghost, MainCaret,
        RawTag::{self, *},
        Spacer, SpawnTag, Tag,
    },
};
use super::{
    Point, Text, TextRangeOrPoint,
    shift_list::{Shift, ShiftList, Shiftable},
};
use crate::{data::Pass, utils::get_ends};

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
    ///
    /// Insertion may fail if you try to push a `Tag` to a position or
    /// range which already has the exact same `Tag` with the exact
    /// same `Tagger`.
    ///
    /// For some `Tag`s (like [`Ghost`]) can return an id (like
    /// [`GhostId`]). This id can then be used in order to insert the
    /// same `Tag`. In the [`Ghost`] example, that would print the
    /// same ghost [`Text`] multiple times without needing to
    /// pointlessly copy the [`Text`] for every time you want to
    /// insert the same ghost.
    ///
    /// When the `Tag` doesn't return an id, it will return `Some(())`
    /// if the `Tag` was successfully added, and `None` otherwise.
    pub fn insert<I, R>(&mut self, tagger: Tagger, r: I, tag: impl Tag<I, R>) -> Option<R>
    where
        R: Copy,
    {
        self.0.insert(tagger, r, tag)
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
    ///     fn update(&mut self, pa: &mut Pass, file: &Handle<File<U>, U>, on: Vec<Range<Point>>) {
    ///         let file = file.write(pa);
    ///         // Removing on the whole File
    ///         file.text_mut().remove_tags(self.tagger, ..);
    ///         // Logic to add Tags with self.tagger...
    ///     }
    /// }
    /// ```
    ///
    /// [tagger]: Taggers
    /// [range]: RangeBounds
    /// [`Parser`]: crate::file::Parser
    /// [when changes happen]: crate::file::Parser::parse
    pub fn remove(&mut self, taggers: impl Taggers, range: impl TextRangeOrPoint) {
        let range = range.to_range(self.0.len_bytes());
        self.0.remove_from(taggers, range)
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
    pub fn remove_excl(&mut self, taggers: impl Taggers, range: impl TextRangeOrPoint) {
        let range = range.to_range(self.0.len_bytes());
        self.0.remove_from_excl(taggers, range);
    }

    /// Removes all [`Tag`]s
    ///
    /// Refrain from using this function on [`File`]s, as there may be
    /// other [`Tag`] providers, and you should avoid measing with
    /// their tags.
    ///
    /// [`File`]: crate::file::File
    pub fn clear(&mut self) {
        *self.0 = InnerTags::new(self.0.list.max() as usize);
    }

    /////////// Querying functions

    /// The lenght of this `Tags` struct
    ///
    /// This number is always identical to calling
    /// [`bytes.len().byte()`] from the [`Bytes`] of the same
    /// [`Text`], or calling `text.len().bytes()` from the `Text`
    /// itself.
    ///
    /// [`bytes.len().byte()`]: super::Bytes::len
    /// [`Bytes`]: super::Bytes
    pub fn len_bytes(&self) -> usize {
        self.0.len_bytes()
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
pub struct InnerTags {
    list: ShiftList<(i32, RawTag)>,
    ghosts: Vec<(GhostId, Text)>,
    toggles: Vec<(ToggleId, Toggle)>,
    spawns: Vec<SpawnCell>,
    pub(super) spawn_fns: Vec<Box<dyn FnOnce(&mut Pass, usize) + Send>>,
    bounds: Bounds,
    extents: TaggerExtents,
}

impl InnerTags {
    /// Creates a new [`InnerTags`] with a given len
    pub fn new(max: usize) -> Self {
        Self {
            list: ShiftList::new(max as i32),
            ghosts: Vec::new(),
            toggles: Vec::new(),
            spawns: Vec::new(),
            spawn_fns: Vec::new(),
            bounds: Bounds::new(max),
            extents: TaggerExtents::new(max),
        }
    }

    /// Insert a new [`Tag`] at a given byte
    pub fn insert<I, R>(&mut self, tagger: Tagger, i: I, tag: impl Tag<I, R>) -> Option<R>
    where
        R: Copy,
    {
        let (start, end, ret) = tag.get_raw(i, self.len_bytes(), tagger);
        let inserted = self.insert_raw(start, end);

        if inserted {
            tag.on_insertion(ret, self);
            Some(ret)
        } else {
            None
        }
    }

    fn insert_raw(&mut self, (s_b, s_tag): (usize, RawTag), end: Option<(usize, RawTag)>) -> bool {
        if let Some((e_b, e_tag)) = end
            && s_b < e_b
        {
            let (s_i, e_i) = match (
                self.list.find_by_key((s_b as i32, s_tag), |t| t),
                self.list.find_by_key((e_b as i32, e_tag), |t| t),
            ) {
                (Ok(_), Ok(_)) => return false,
                (Ok(s_i), Err(e_i)) | (Err(s_i), Ok(e_i)) | (Err(s_i), Err(e_i)) => (s_i, e_i + 1),
            };

            self.list.insert(s_i, (s_b as i32, s_tag));
            self.list.insert(e_i, (e_b as i32, e_tag));

            self.bounds
                .insert([([s_i, s_b], s_tag), ([e_i, e_b], e_tag)]);

            self.extents.insert(s_tag.tagger(), s_b);
            self.extents.insert(s_tag.tagger(), e_b);

            true
        } else if end.is_none() {
            let (Ok(i) | Err(i)) = self.list.find_by_key((s_b as i32, s_tag), |s| s);
            self.list.insert(i, (s_b as i32, s_tag));

            self.bounds.shift_by(i, [1, 0]);

            self.extents.insert(s_tag.tagger(), s_b);
            true
        } else {
            false
        }
    }

    /// Insert another [`InnerTags`] into this one
    pub fn insert_tags(&mut self, p: Point, mut other: InnerTags) {
        let mut starts = Vec::new();

        for (_, (b, tag)) in other.list.iter_fwd(..) {
            let b = b as usize + p.byte();
            match tag {
                PushForm(..) | StartAlignCenter(_) | StartAlignRight(_) | StartConceal(_) => {
                    starts.push((b, tag))
                }
                PopForm(..) | EndAlignCenter(_) | EndAlignRight(_) | EndConceal(_) => {
                    let i = starts.iter().rposition(|(_, t)| t.ends_with(&tag)).unwrap();
                    let (sb, stag) = starts.remove(i);
                    self.insert_raw((sb, stag), Some((b, tag)));
                }
                ConcealUntil(_) => unreachable!(),
                RawTag::Ghost(_, id) => {
                    self.ghosts
                        .extend(other.ghosts.extract_if(.., |(l, _)| l == &id).next());
                    self.insert_raw((b, tag), None);
                }
                StartToggle(..) | EndToggle(..) => todo!(),
                RawTag::MainCaret(_)
                | RawTag::ExtraCaret(_)
                | RawTag::Spacer(_)
                | SpawnedWidget(..) => {
                    self.insert_raw((b, tag), None);
                }
            };
        }
    }

    /// Extends this [`InnerTags`] with another one
    pub fn extend(&mut self, other: InnerTags) {
        self.list.extend(other.list);
        self.ghosts.extend(other.ghosts);
        self.toggles.extend(other.toggles);
        self.bounds.extend(other.bounds);
        self.extents.extend(other.extents);
    }

    /// Removes all [`RawTag`]s of a give [`Taggers`]
    pub fn remove_from(&mut self, taggers: impl Taggers, within: impl RangeBounds<usize>) {
        let (start, end) = crate::utils::get_ends(within, self.len_bytes());

        for range in self
            .extents
            .remove_range(start..end, |tagger| taggers.contains_tagger(tagger))
        {
            self.remove_from_if(range, |(_, tag)| taggers.contains_tagger(tag.tagger()));
        }
    }

    fn remove_from_excl(&mut self, taggers: impl Taggers, within: impl RangeBounds<usize>) {
        let (start, end) = crate::utils::get_ends(within, self.len_bytes());
        // In this case, this would be an empty range, and returning is
        // appropriate.
        let Some(excl_end) = end.checked_sub(1).filter(|e| *e >= start) else {
            return;
        };

        for range in self
            .extents
            .remove_range(start..end, |tagger| taggers.contains_tagger(tagger))
        {
            self.remove_from_if(range, |(b, tag)| {
                taggers.contains_tagger(tag.tagger())
                    && ((b > start as i32 || !tag.is_end())
                        && (b < excl_end as i32 && !tag.is_start()))
            });
        }
    }

    /// Removes every [`RawTag`] from a range, as well as their
    /// matches
    ///
    /// WILL remove every required [`RawTag`], WILL shift the indices
    /// of the [`Bounds`], WILL NOT shift [`TaggerExtents`], since
    /// there is no byte shifting, WILL NOT shift the bytes of the
    /// [`Bounds`]
    fn remove_from_if(
        &mut self,
        range: Range<usize>,
        filter: impl Fn((i32, RawTag)) -> bool + Copy,
    ) {
        for i in self
            .bounds
            .remove_intersecting(range.clone(), filter)
            .into_iter()
            .rev()
        {
            // We remove both bounds in order to prevent a state of dangling
            // bounds, which would cause a lookback or lookahead over the whole
            // Text.
            self.list.remove(i);
            self.bounds.shift_by(i, [-1, 0]);
        }

        let mut removed = 0;
        let mut starts = Vec::new();
        let mut ends = Vec::new();

        let (Ok(start) | Err(start)) = self.list.find_by_key(range.start as i32, |(b, _)| b);
        let (Ok(end) | Err(end)) = self.list.find_by_key(range.end as i32, |(b, _)| b);

        self.list
            .extract_if_while(start..end, |_, entry| Some(filter(entry)))
            .for_each(|(i, (_, tag))| {
                removed += 1;
                self.bounds.shift_by(i, [-1, 0]);
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
                }
            });

        self.list
            .extract_if_while(end - removed.., |i, (_, tag)| {
                if let Some(s_i) = starts.iter().rposition(|s| s.ends_with(&tag)) {
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
            .rextract_if_while(..start, |i, (_, tag)| {
                if let Some(e_i) = ends.iter().rposition(|e| tag.ends_with(e)) {
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
    pub fn transform(&mut self, old: Range<usize>, new_end: usize) {
        let new = old.start..new_end;

        // Old length removal.
        if old.end > old.start {
            // First, get rid of all ranges that start and/or end in the old
            // range.
            // old.start + 1 because we don't want to get rid of bounds that
            // merely coincide with the edges.
            self.remove_from_if(old.start + 1..old.end, |_| true);
            self.extents.remove_range(old.start + 1..old.end, |_| true);

            // If the range becomes empty, we should remove the remainig pairs
            if new.end == old.start
                && let Ok(s_i) = self.list.find_by_key(old.start as i32, |(b, _)| b)
            {
                let mut to_remove: Vec<usize> = Vec::new();
                let mut starts = Vec::new();
                let mut iter = self.list.iter_fwd(s_i..);

                while let Some((i, (b, tag))) = iter.next()
                    && b == old.start as i32
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
                drop(iter);

                for i in to_remove.into_iter().rev() {
                    self.list.remove(i);
                    self.bounds.shift_by(i, [-1, 0]);
                }
            }
        }

        let shift = new.len() as i32 - old.len() as i32;
        let (Ok(i) | Err(i)) = self.list.find_by_key(old.start as i32 + 1, |(b, _)| b);

        self.list.shift_by(i, shift);
        self.bounds.shift_by(i, [0, shift]);
        self.extents.shift_by(old.start + 1, shift);
    }

    pub fn update_bounds(&mut self) {
        for range in self.bounds.take_ranges() {
            let mut starts = Vec::new();
            for (i, (b, tag)) in self.list.iter_fwd(range) {
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
    #[define_opaque(FwdTags)]
    pub fn fwd_at(&self, b: usize) -> FwdTags<'_> {
        let s_i = {
            let (Ok(s_i) | Err(s_i)) = self.list.find_by_key(b as i32, |(b, _)| b);
            s_i.saturating_sub(self.bounds.min_len())
        };

        let is_before = move |([n, b], tag): ([usize; 2], _)| (n < s_i).then_some((b, tag));
        let bounds = self.bounds.iter_fwd().map_while(is_before);

        let tags = self.list.iter_fwd(s_i..).map(|(n, (b, tag))| match tag {
            StartConceal(tagger) => match self.bounds.match_of(n) {
                Some(([_, e_b], _)) => (b as usize, ConcealUntil(e_b as u32)),
                _ => (b as usize, StartConceal(tagger)),
            },
            tag => (b as usize, tag),
        });

        bounds.into_iter().chain(tags).peekable()
    }

    /// Returns a reverse iterator at a given byte
    #[define_opaque(RevTags)]
    pub fn rev_at(&self, b: usize) -> RevTags<'_> {
        let e_i = {
            let (Ok(e_i) | Err(e_i)) = self.list.find_by_key(b as i32, |(b, _)| b);
            (e_i + self.bounds.min_len()).min(self.list.len())
        };

        let is_after = move |([n, b], tag): ([usize; 2], _)| (n > e_i).then_some((b, tag));
        let bounds = self.bounds.iter_rev().map_while(is_after);

        let tags = self.list.iter_rev(..e_i).map(|(n, (b, tag))| match tag {
            EndConceal(tagger) => match self.bounds.match_of(n) {
                Some(([_, s_b], _)) => (b as usize, ConcealUntil(s_b as u32)),
                _ => (b as usize, EndConceal(tagger)),
            },
            tag => (b as usize, tag),
        });

        bounds.chain(tags).peekable()
    }

    pub fn raw_fwd_at(&self, b: usize) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        let (Ok(s_i) | Err(s_i)) = self.list.find_by_key(b as i32, |(b, _)| b);
        self.list
            .iter_fwd(s_i..)
            .map(|(_, (b, tag))| (b as usize, tag))
    }

    pub fn raw_rev_at(&self, b: usize) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        let (Ok(e_i) | Err(e_i)) = self.list.find_by_key(b as i32, |(b, _)| b);
        self.list
            .iter_rev(..e_i)
            .map(|(_, (b, tag))| (b as usize, tag))
    }

    /// Returns an iterator over a single byte
    pub fn iter_only_at(&self, b: usize) -> impl Iterator<Item = RawTag> + '_ {
        let (Ok(s_i) | Err(s_i)) = self.list.find_by_key(b as i32, |(b, _)| b);
        self.list
            .iter_fwd(s_i..)
            .take_while(move |(_, (cur_b, _))| *cur_b as usize == b)
            .map(|(_, (_, tag))| tag)
    }

    ////////// Querying functions

    /// Returns true if there are no [`RawTag`]s
    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    /// Returns the len of the [`InnerTags`] in bytes
    pub fn len_bytes(&self) -> usize {
        self.list.max() as usize
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

    /// A list of all [`SpawnId`]s that belong to this `Tags`
    pub(crate) fn get_spawned_ids(&self) -> impl Iterator<Item = SpawnId> {
        self.spawns.iter().map(|spawn_cell| spawn_cell.0)
    }
}

impl Clone for InnerTags {
    fn clone(&self) -> Self {
        Self {
            list: self.list.clone(),
            ghosts: self.ghosts.clone(),
            toggles: self.toggles.clone(),
            spawns: Vec::new(),
            spawn_fns: Vec::new(),
            bounds: self.bounds.clone(),
            extents: self.extents.clone(),
        }
    }
}

struct DebugBuf<'a, R: RangeBounds<usize>>(&'a InnerTags, R);
impl<R: RangeBounds<usize> + Clone> std::fmt::Debug for DebugBuf<'_, R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (start, end) = get_ends(self.1.clone(), self.0.len_bytes());

        if f.alternate() {
            let n_spc = self.0.list.len().checked_ilog10().unwrap_or(0) as usize + 4;
            let b_spc = self.0.len_bytes().checked_ilog10().unwrap_or(0) as usize + 4;

            let mut nesting: usize = 0;

            f.write_str("[\n")?;

            for (i, (b, tag)) in self.0.list.iter_fwd(..) {
                nesting = nesting.saturating_sub(tag.is_end() as usize);
                if (start as u32..=end as u32).contains(&(b as u32)) {
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
                (Ghost(_, lhs), Ghost(_, rhs)) => {
                    let self_ghost = self
                        .ghosts
                        .iter()
                        .find_map(|(id, text)| (lhs == *id).then_some(text));
                    let other_ghost = other
                        .ghosts
                        .iter()
                        .find_map(|(id, text)| (rhs == *id).then_some(text));
                    self_ghost == other_ghost && l_b == r_b
                }
                (MainCaret(_), MainCaret(_))
                | (ExtraCaret(_), ExtraCaret(_))
                | (StartAlignCenter(_), StartAlignCenter(_))
                | (EndAlignCenter(_), EndAlignCenter(_))
                | (StartAlignRight(_), StartAlignRight(_))
                | (EndAlignRight(_), EndAlignRight(_))
                | (Spacer(_), Spacer(_))
                | (StartConceal(_), StartConceal(_))
                | (EndConceal(_), EndConceal(_))
                | (StartToggle(..), StartToggle(..))
                | (EndToggle(..), EndToggle(..)) => l_b == r_b,
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
pub type FwdTags<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;
/// A reverse [`Iterator`] of [`RawTag`]s
///
/// This iterator automatically takes into account [`TagRange`]s and
/// iterates their bounds as if they were regular [`RawTag`]s
pub type RevTags<'a> = std::iter::Peekable<impl Iterator<Item = (usize, RawTag)> + Clone + 'a>;

mod ids {
    use std::sync::atomic::{AtomicU16, Ordering};

    /// The id of a [ghost text]
    ///
    /// [ghost text]: super::Ghost
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct GhostId(u16);

    impl GhostId {
        /// Creates a new [`GhostId`]
        pub(super) fn new() -> Self {
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
        pub(super) fn _new() -> Self {
            static TOGGLE_COUNT: AtomicU16 = AtomicU16::new(0);
            Self(TOGGLE_COUNT.fetch_add(1, Ordering::Relaxed))
        }
    }

    impl std::fmt::Debug for ToggleId {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "ToggleId({})", self.0)
        }
    }

    /// The id of a spawned [`Widget`]
    ///
    /// [`Widget`]: crate::ui::Widget
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct SpawnId(u16);

    impl SpawnId {
        /// Creates a new [`SpawnId`]
        #[allow(clippy::new_without_default)]
        pub(crate) fn new() -> Self {
            static SPAWN_COUNT: AtomicU16 = AtomicU16::new(0);
            Self(SPAWN_COUNT.fetch_add(1, Ordering::Relaxed))
        }
    }

    impl std::fmt::Debug for SpawnId {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "SpawnId({})", self.0)
        }
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
struct SpawnCell(SpawnId);

impl Drop for SpawnCell {
    fn drop(&mut self) {
        crate::context::windows().queue_close_spawned(self.0);
    }
}
