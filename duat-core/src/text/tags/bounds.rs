use std::ops::Range;

use self::id::RangeId;
use crate::{
    ranges::Ranges,
    text::{
        RawTag,
        shift_list::{Shift, ShiftList, Shiftable},
    },
};

/// How many [`TagOrSkip`]s to keep a [`RawTag`] range
///
/// The limit is low because most of the time, people don't nest
/// [`Tag`]s, and a low limit means that iteration will go back very
/// few [`TagOrSkip`]s
const MIN_FOR_RANGE: usize = 16;

/// A struct to keep better track of very long [`RawTag`] ranges
#[derive(Debug, Clone)]
pub struct Bounds {
    list: ShiftList<([u32; 2], RawTag, RangeId)>,
    ranges_to_update: Ranges,
    min_len: usize,
}

impl Bounds {
    /// Returns a new instance of [`Bounds`]
    pub fn new(max: usize) -> Self {
        let mut ranges_to_update = Ranges::empty();
        ranges_to_update.set_min_len(MIN_FOR_RANGE);

        Self {
            list: ShiftList::new([max as i32, 0]),
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

            self.list.insert(s_i, ([s_n as u32, s_b as u32], s_tag, id));

            let e_i = self.shift_by(e_n, [1, 0]);
            self.list.insert(e_i, ([e_n as u32, e_b as u32], e_tag, id));
        } else {
            self.shift_by(e_n, [1, 0]);
        }
    }

    /// Represents the given range in the list, if it wasn't there
    /// already
    pub fn represent(&mut self, [s, e]: [([u32; 2], RawTag); 2]) {
        let before_id = |(bound, tag, _): ([u32; 2], RawTag, _)| (bound, tag);

        let (Err(s_i), Err(e_i)) = (
            self.list.find_by_key(s, before_id),
            self.list.find_by_key(e, before_id),
        ) else {
            return;
        };

        if e_i >= s_i + self.min_len {
            let [(s_bound, s_tag), (e_bound, e_tag)] = [s, e];

            let id = RangeId::new();
            self.list.insert(s_i, (s_bound, s_tag, id));
            self.list.insert(e_i, (e_bound, e_tag, id));
        }
    }

    /// Shifts the bounds within by a difference in a position,
    /// returns the insertion point of that shift
    pub fn shift_by(&mut self, n: usize, [n_diff, c_diff]: [i32; 2]) -> usize {
        let (Ok(i) | Err(i)) = self.list.find_by_key(n as u32, |([n, _], ..)| n);
        self.list.shift_by(i, [n_diff, c_diff]);

        self.ranges_to_update.shift_by(n, n_diff);
        // This means we are adding Tags, so new ranges might need to be
        // accounted for.
        if n_diff > 0 {
            self.ranges_to_update.add({
                let [s, e] = [n, n + n_diff.unsigned_abs() as usize];
                s.saturating_sub(self.min_len)..(e + self.min_len).min(self.list.max()[0] as usize)
            });
        }

        i
    }

    /// Removes all ranges that start or end in the range
    ///
    /// WILL handle matching [`Tag`]s, WILL handle shifting of ranges,
    /// will NOT handle fully contained ranges.
    pub fn remove_intersecting(
        &mut self,
        range: Range<usize>,
        filter: impl Fn(RawTag) -> bool,
    ) -> Vec<usize> {
        let mut removed = Vec::new();
        let mut starts = Vec::new();
        let mut ends = Vec::new();

        self.list
            .extract_if_while(range.clone(), |_, (_, tag, _)| Some(filter(tag)))
            .for_each(|(_, ([n, _], tag, id))| {
                removed.push(n as usize);
                if tag.is_start() {
                    starts.push((n, id));
                } else {
                    ends.push((n, id));
                }
            });

        removed.extend(
            self.list
                .extract_if_while(range.end - starts.len() - ends.len().., |_, (.., e_id)| {
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
                .rextract_if_while(..range.start, |_, (.., s_id)| {
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

        while self.list.buf().len() >= LIMIT_TO_BUMP * 2 {
            let mut bounds_to_remove = Vec::new();
            self.min_len += BUMP_AMOUNT;

            let is_start = |(_, (_, tag, _)): &(_, (_, RawTag, _))| tag.is_start();
            for (i, ([n0, _], _, id)) in self.list.iter_fwd(..).filter(is_start) {
                let is_matching = |(_, (.., lhs)): &(_, (_, _, RangeId))| *lhs == id;
                let (j, ([n1, _], ..)) = self.list.iter_fwd(i + 1..).find(is_matching).unwrap();

                if n1 - n0 < self.min_len as u32 {
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
    pub fn iter_fwd(&self) -> impl Iterator<Item = ([usize; 2], RawTag)> + Clone {
        self.list
            .iter_fwd(..)
            .map(|(_, (bound, tag, _))| (bound.map(|x| x as usize), tag))
    }

    /// Iterates over the bounds
    pub fn iter_rev(&self) -> impl Iterator<Item = ([usize; 2], RawTag)> + Clone {
        self.list
            .iter_rev(..)
            .map(|(_, (bound, tag, _))| (bound.map(|x| x as usize), tag))
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
        let i = self.list.find_by_key(n as u32, |([n, _], ..)| n).ok()?;

        let is_matching = |(_, ([n, b], tag, id)): (_, ([u32; 2], _, RangeId))| {
            (id == self.list.buf()[i].2).then_some(([n as usize, b as usize], tag))
        };

        Some(if self.list.buf()[i].1.is_start() {
            self.list.iter_fwd(i + 1..).find_map(is_matching).unwrap()
        } else {
            self.list.iter_rev(..i).find_map(is_matching).unwrap()
        })
    }
}

pub(super) struct DebugBounds<'a>(pub(super) &'a super::InnerTags);
impl std::fmt::Debug for DebugBounds<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() && !self.0.bounds.list.buf().is_empty() {
            f.write_str("[\n")?;
            for (_, ([n, b], tag, id)) in self.0.bounds.list.iter_fwd(..) {
                writeln!(f, "    {:?}", ([n, b], tag, id))?;
                writeln!(f, "        {:?}", self.0.list.buf().get(n as usize))?;
            }
            f.write_str("],\n")?;

            writeln!(f, "{:#?},", self.0.bounds.min_len)?;
            writeln!(f, "{:#?}", self.0.bounds.ranges_to_update)
        } else {
            write!(f, "{:?}", self.0.bounds)
        }
    }
}

impl Shiftable for ([u32; 2], RawTag, RangeId) {
    type Shift = [i32; 2];

    fn shift(self, by: Self::Shift) -> Self {
        let sh = |i: usize| (self.0[i] as i32 + by[i]) as u32;
        ([sh(0), sh(1)], self.1, self.2)
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
