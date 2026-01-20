use std::ops::Range;

use self::id::RangeId;
use crate::{
    ranges::Ranges,
    text::{
        RawTag,
        shift_list::{Shift, ShiftList, Shiftable},
    },
};

/// How many [`Tag`]s to keep a [`RawTag`] range
///
/// The limit is low because most of the time, people don't nest
/// `Tag`s, and a low limit means that iteration will go back very
/// few `Tag`s
///
/// [`Tag`]: super::Tag
const MIN_FOR_RANGE: usize = 16;

/// A struct to keep better track of very long [`RawTag`] ranges
#[derive(Debug, Clone)]
pub struct Bounds {
    list: ShiftList<([i32; 2], RawTag, RangeId)>,
    ranges_to_update: Ranges,
    min_len: usize,
}

impl Bounds {
    /// Returns a new instance of [`Bounds`]
    pub fn new(max: usize) -> Self {
        let mut ranges_to_update = Ranges::empty();
        ranges_to_update.set_min_len(MIN_FOR_RANGE);

        Self {
            list: ShiftList::new([0, max as i32]),
            ranges_to_update,
            min_len: MIN_FOR_RANGE,
        }
    }

    /// Inserts a new bound to the list
    pub fn insert(&mut self, [s, e]: [([usize; 2], RawTag); 2]) {
        let [([s_n, s_b], s_tag), ([e_n, e_b], e_tag)] = [s, e];

        let s_i = self.shift_by(s_n, [1, 0]);

        if e_n - s_n >= self.min_len {
            let id = RangeId::new();

            self.list.insert(s_i, ([s_n as i32, s_b as i32], s_tag, id));

            let e_i = self.shift_by(e_n, [1, 0]);
            self.list.insert(e_i, ([e_n as i32, e_b as i32], e_tag, id));
        } else {
            self.shift_by(e_n, [1, 0]);
        }
    }

    /// Represents the given range in the list, if it wasn't there
    /// already
    pub fn represent(&mut self, [s, e]: [([i32; 2], RawTag); 2]) {
        let [([s_n, s_b], s_tag), ([e_n, e_b], e_tag)] = [s, e];

        if self.min_len > (e_n - s_n) as usize {
            return;
        }

        let before_id = |(bound, tag, _): ([i32; 2], RawTag, _)| (bound, tag);

        let (Err(s_i), Err(e_i)) = (
            self.list.find_by_key(s, before_id),
            self.list.find_by_key(e, before_id),
        ) else {
            return;
        };

        let id = RangeId::new();
        self.list.insert(e_i, ([e_n, e_b], e_tag, id));
        self.list.insert(s_i, ([s_n, s_b], s_tag, id));
    }

    /// Shifts the bounds within by a difference in a position,
    /// returns the insertion point of that shift
    pub fn shift_by(&mut self, n: usize, [n_diff, b_diff]: [i32; 2]) -> usize {
        let (Ok(i) | Err(i)) = self.list.find_by_key(n as i32, |([n, _], ..)| n);
        self.list.shift_by(i, [n_diff, b_diff]);

        self.ranges_to_update.shift_by(n, n_diff);
        // This means we are adding Tags, so new ranges might need to be
        // accounted for.
        if n_diff > 0 {
            self.ranges_to_update.add({
                let [s, e] = [n, n + n_diff.unsigned_abs() as usize];
                let end = (e + self.min_len).min(self.list.max()[0] as usize);
                s.saturating_sub(self.min_len)..end
            });
        }

        i
    }

    /// Removes the bounds for the `n`th entry
    ///
    /// Does nothing if no such bound existed
    pub fn remove_if_represented(&mut self, n: usize) {
        let Ok(i) = self.list.find_by_key(n as i32, |([n, _], ..)| n) else {
            return;
        };

        let (_, tag, id) = self.list.get(i).unwrap();
        self.list.remove(i);

        let (j, _) = if tag.is_start() {
            self.list
                .iter_fwd(i..)
                .find(|(_, (.., other))| *other == id)
                .unwrap()
        } else {
            self.list
                .iter_rev(..i)
                .find(|(_, (.., other))| *other == id)
                .unwrap()
        };

        self.list.remove(j);
    }

    /// Removes all ranges that start or end in the range
    ///
    /// WILL handle matching [`Tag`]s, WILL NOT handle shifting of
    /// ranges, will NOT handle fully contained ranges.
    ///
    /// [`Tag`]: super::Tag
    pub fn remove_intersecting(
        &mut self,
        range: Range<usize>,
        mut filter: impl FnMut((i32, RawTag)) -> bool,
    ) -> Vec<usize> {
        let (Ok(s) | Err(s)) = self.list.find_by_key(range.start as i32, |([_, c], ..)| c);
        let (Ok(e) | Err(e)) = self.list.find_by_key(range.end as i32, |([_, c], ..)| c);

        let mut removed = Vec::new();
        let mut starts = Vec::new();
        let mut ends = Vec::new();
        // For aiding exclusive removal.
        let mut first_and_last_removed_indices = None;

        self.list
            .extract_if_while(s..e, |_, ([_, b], tag, _)| Some(filter((b, tag))))
            .for_each(|(i, ([n, _], tag, id))| {
                removed.push(n as usize);

                if let Some((_, last)) = first_and_last_removed_indices.as_mut() {
                    *last = i;
                } else {
                    first_and_last_removed_indices = Some((i, i));
                }

                if tag.is_start() {
                    starts.push((n, id));
                } else {
                    ends.push((n, id));
                }
            });

        let Some((first, last)) = first_and_last_removed_indices else {
            removed.sort_unstable();
            return removed;
        };

        removed.extend(
            self.list
                .extract_if_while(first.., |_, (.., e_id)| {
                    if let Some(i) = starts.iter().rposition(|(_, s_id)| *s_id == e_id) {
                        starts.remove(i);
                        Some(true)
                    } else if starts.is_empty() {
                        None
                    } else {
                        Some(false)
                    }
                })
                .map(|(_, ([n, _], ..))| n as usize),
        );

        removed.extend(
            self.list
                .rextract_if_while(..last, |_, (.., s_id)| {
                    if let Some(i) = ends.iter().rposition(|(_, e_id)| *e_id == s_id) {
                        ends.remove(i);
                        Some(true)
                    } else if ends.is_empty() {
                        None
                    } else {
                        Some(false)
                    }
                })
                .map(|(_, ([n, _], ..))| n as usize),
        );

        // I could improve this, but idrc ¯\_(ツ)_/¯.
        removed.sort_unstable();

        removed
    }

    /// Removes ranges that are too small to be kept
    pub fn cull_small_ranges(&mut self) {
        /// How many [`RawTag`] ranges until their minimum size is
        /// increased
        const LIMIT_TO_BUMP: usize = 128;
        /// How much to increase the minimum when enough [`RawTag`]
        /// ranges exist
        const BUMP_AMOUNT: usize = 16;

        while self.list.len() >= LIMIT_TO_BUMP * 2 {
            let mut bounds_to_remove = Vec::new();
            self.min_len += BUMP_AMOUNT;

            let is_start = |(_, (_, tag, _)): &(_, (_, RawTag, _))| tag.is_start();
            for (i, ([n0, _], _, id)) in self.list.iter_fwd(..).filter(is_start) {
                let is_matching = |(_, (.., lhs)): &(_, (_, _, RangeId))| *lhs == id;
                let (j, ([n1, _], ..)) = self.list.iter_fwd(i + 1..).find(is_matching).unwrap();

                if n1 - n0 < self.min_len as i32 {
                    for k in [i, j] {
                        let (Ok(l) | Err(l)) = bounds_to_remove.binary_search(&k);
                        bounds_to_remove.insert(l, k);
                    }
                }
            }

            for i in bounds_to_remove.into_iter().rev() {
                self.list.remove(i);
            }
        }

        self.ranges_to_update.set_min_len(self.min_len)
    }

    /// Extends this [`BoundsList`] by another
    pub fn extend(&mut self, other: Self) {
        self.list.extend(other.list)
    }

    /// Iterates over the bounds
    pub fn iter_fwd(&self) -> IterFwd<'_> {
        IterFwd { iter: self.list.iter_fwd(..) }
    }

    /// Iterates over the bounds
    pub fn iter_rev(&self) -> IterRev<'_> {
        IterRev { iter: self.list.iter_rev(..) }
    }

    /// Takes the ranges of ranges_to_update
    pub fn take_ranges(&mut self) -> impl Iterator<Item = Range<usize>> + 'static {
        std::mem::take(&mut self.ranges_to_update).into_iter()
    }

    /// The minimum length to keep a range inside
    pub fn min_len(&self) -> usize {
        self.min_len
    }

    /// Try to find the match for a RawTag at a given index
    pub fn match_of(&self, n: usize) -> Option<([usize; 2], RawTag)> {
        let i = self.list.find_by_key(n as i32, |([n, _], ..)| n).ok()?;

        let is_matching = |(_, ([n, b], tag, id)): (_, ([i32; 2], _, RangeId))| {
            (id == self.list.get(i).unwrap().2).then_some(([n as usize, b as usize], tag))
        };

        Some(if self.list.get(i).unwrap().1.is_start() {
            self.list.iter_fwd(i + 1..).find_map(is_matching).unwrap()
        } else {
            self.list.iter_rev(..i).find_map(is_matching).unwrap()
        })
    }
}

/// A forward iterator for the [`Bounds`]
#[derive(Debug, Clone)]
pub struct IterFwd<'a> {
    iter: crate::text::shift_list::IterFwd<'a, ([i32; 2], RawTag, RangeId)>,
}

impl Iterator for IterFwd<'_> {
    type Item = ([usize; 2], RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(_, (bound, tag, _))| (bound.map(|x| x as usize), tag))
    }
}

/// A forward iterator for the [`Bounds`]
#[derive(Debug, Clone)]
pub struct IterRev<'a> {
    iter: crate::text::shift_list::IterRev<'a, ([i32; 2], RawTag, RangeId)>,
}

impl Iterator for IterRev<'_> {
    type Item = ([usize; 2], RawTag);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(_, (bound, tag, _))| (bound.map(|x| x as usize), tag))
    }
}

pub(super) struct DebugBounds<'a>(pub(super) &'a super::InnerTags);
impl std::fmt::Debug for DebugBounds<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.write_str("\nlist: [")?;
            for (_, ([n, b], tag, id)) in self.0.bounds.list.iter_fwd(..) {
                write!(f, "\n    {:?}", ([n, b], tag, id))?;
                write!(f, "\n        {:?}", self.0.list.get(n as usize))?;
            }
            if !self.0.bounds.list.is_empty() {
                f.write_str("\n")?;
            }
            f.write_str("],\n")?;

            writeln!(f, "min_len: {:#?},", self.0.bounds.min_len)?;
            writeln!(f, "ranges_to_update: {:#?}", self.0.bounds.ranges_to_update)
        } else {
            write!(f, "{:?}", self.0.bounds)
        }
    }
}

impl Shiftable for ([i32; 2], RawTag, RangeId) {
    type Shift = [i32; 2];

    fn shift(self, by: Self::Shift) -> Self {
        ([self.0[0] + by[0], self.0[1] + by[1]], self.1, self.2)
    }
}

impl Shift for [i32; 2] {
    fn neg(self) -> Self {
        [-self[0], -self[1]]
    }

    fn add(self, other: Self) -> Self {
        [self[0] + other[0], self[1] + other[1]]
    }
}

mod id {
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct RangeId(usize);

    impl RangeId {
        /// Creates a new [`RangeId`]
        #[allow(clippy::new_without_default)]
        pub fn new() -> Self {
            static RANGE_COUNT: AtomicUsize = AtomicUsize::new(0);
            Self(RANGE_COUNT.fetch_add(1, Ordering::Relaxed))
        }
    }
}
