use std::{cell::Cell, ops::Range};

use self::id::RangeId;
use super::sh;
use crate::{ranges::Ranges, text::RawTag};

/// How many [`TagOrSkip`]s to keep a [`RawTag`] range
///
/// The limit is low because most of the time, people don't nest
/// [`Tag`]s, and a low limit means that iteration will go back very
/// few [`TagOrSkip`]s
const MIN_FOR_RANGE: usize = 16;

/// A struct to keep better track of very long [`RawTag`] ranges
#[derive(Debug, Clone)]
pub struct Bounds {
    list: Vec<(Cell<[usize; 2]>, RawTag, RangeId)>,
    shift_state: Cell<(usize, [i32; 2])>,
    ranges_to_update: Ranges,
    min_len: usize,
}

impl Bounds {
    /// Returns a new instance of [`Bounds`]
    pub fn new() -> Self {
        let mut ranges_to_update = Ranges::empty();
        ranges_to_update.set_min_len(MIN_FOR_RANGE);

        Self {
            list: Vec::new(),
            shift_state: Cell::default(),
            ranges_to_update,
            min_len: MIN_FOR_RANGE,
        }
    }

    /// Inserts a new bound to the list
    pub fn insert(&mut self, final_buf_len: usize, [s, e]: [(usize, [usize; 2], RawTag); 2]) {
        /// Declares tha the given index is shifted
        fn declare_shifted(bounds: &mut Bounds, ins: usize) {
            bounds.shift_state.set({
                if ins + 1 < bounds.list.len() {
                    let (shift_from, diff) = bounds.shift_state.get();
                    (shift_from + 1, diff)
                } else {
                    (0, [0; 2])
                }
            });
        }

        let [(s_skip, [s_n, s_b], s_tag), (e_skip, [e_n, e_b], e_tag)] = [s, e];
        let s_n_diff = (s_n + 1 - s_skip) as i32;
        let e_n_diff = (e_n + 1 - e_skip) as i32;

        let buf_len_after_s = final_buf_len - e_n_diff as usize;
        let s_i = self.shift_by(buf_len_after_s, s_skip, [s_n_diff, 0]);

        if e_n - s_n >= self.min_len {
            let id = RangeId::new();

            self.list.insert(s_i, (Cell::new([s_n, s_b]), s_tag, id));
            declare_shifted(self, s_i);

            let e_i = self.shift_by(final_buf_len, e_skip, [e_n_diff, 0]);

            self.list.insert(e_i, (Cell::new([e_n, e_b]), e_tag, id));
            declare_shifted(self, s_i);
        } else {
            self.shift_by(final_buf_len, e_skip, [e_n_diff, 0]);
        }
    }

    /// Represents the given range in the list, if it wasn't there
    /// already
    pub fn represent(&mut self, [s, e]: [([usize; 2], RawTag); 2]) {
        let (Err(s_i), Err(e_i)) = (
            self.list.binary_search_by_key(&s, before_id),
            self.list.binary_search_by_key(&e, before_id),
        ) else {
            return;
        };

        if e_i >= s_i + self.min_len {
            let [(s_bound, s_tag), (e_bound, e_tag)] = [s, e];

            let id = RangeId::new();
            self.list.insert(s_i, (Cell::new(s_bound), s_tag, id));
            self.list.insert(e_i, (Cell::new(e_bound), e_tag, id));
        }
    }

    /// Shifts the bounds within by a difference in a position,
    /// returns the insertion point of that shift
    pub fn shift_by(&mut self, buf_len: usize, from: usize, [n_diff, b_diff]: [i32; 2]) -> usize {
        self.ranges_to_update.shift_by(from, n_diff);
        // This means we are adding TagOrSkips, so new ranges may be created
        if n_diff > 0 {
            self.ranges_to_update.add({
                let [s, e] = [from, from + n_diff.unsigned_abs() as usize];
                s.saturating_sub(self.min_len)..(e + self.min_len).min(buf_len)
            });
        }

        let (mut shift_from, [mut total_n_diff, mut total_b_diff]) = self.shift_state.take();

        let ins = if let Some((first_unshifted, ..)) = self.list.get(shift_from) {
            let mut ins = shift_from;
            let [unshifted_n, _] = first_unshifted.get();
            // cur_n is the first bound that has not been shifted, so we need to
            // take that into consideration.
            if from < sh(unshifted_n, total_n_diff) {
                let mut iter = self.list[..shift_from].iter().rev();
                while let Some((bound, ..)) = iter.next()
                    && let [n, b] = bound.get()
                    && n >= from
                {
                    bound.set([sh(n, n_diff), sh(b, b_diff)]);
                    ins -= 1;
                }
            } else {
                let mut iter = self.list[shift_from..].iter();
                // All bounds from cur_n have not been shifted, so we account for
                // that.
                while let Some((bound, ..)) = iter.next()
                    && let [n, b] = bound.get()
                    && sh(n, total_n_diff) < from
                {
                    bound.set([sh(n, total_n_diff), sh(b, total_b_diff)]);
                    ins += 1;
                    shift_from += 1;
                }
            }

            total_n_diff += n_diff;
            total_b_diff += b_diff;

            ins
        } else {
            0
        };

        if shift_from < self.list.len() {
            self.shift_state
                .set((shift_from, [total_n_diff, total_b_diff]));
        }

        ins
    }

    /// Finishes shifting
    pub fn finish_shifting(&self) {
        let (sh_from, [total_n_diff, total_b_diff]) = self.shift_state.take();

        for (bound, ..) in self.list[sh_from..].iter() {
            let [n, b] = bound.get();
            bound.set([sh(n, total_n_diff), sh(b, total_b_diff)]);
        }
    }

    /// Removes all ranges that start or end in the range
    ///
    /// WILL handle matching [`Tag`]s, WILL handle shifting of ranges,
    /// will NOT handle fully contained ranges.
    pub fn remove_intersecting(
        &mut self,
        range: Range<usize>,
        filter: impl Fn(&RawTag) -> bool,
    ) -> Vec<[usize; 2]> {
        fn extract(
            shift_from: &mut usize,
            [n_diff, b_diff]: [i32; 2],
            f: impl Fn(usize, usize, &Cell<[usize; 2]>, &RawTag, &RangeId) -> bool,
        ) -> impl FnMut(&mut (Cell<[usize; 2]>, RawTag, RangeId)) -> bool {
            let mut i = 0;
            move |(bound, tag, id)| {
                let [n, b] = bound.get();
                if f(i, *shift_from, bound, tag, id) {
                    if i < *shift_from {
                        *shift_from -= 1;
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
        let (mut shift_from, diff) = self.shift_state.get();

        if range.is_empty() {
            return Vec::new();
        }

        let is_contained = extract(&mut shift_from, diff, |i, shift_from, bound, tag, _| {
            let diff = if i < shift_from { 0 } else { diff[1] };
            range.clone().contains(&sh(bound.get()[1], diff)) && filter(tag)
        });
        let removed: Vec<_> = self.list.extract_if(.., is_contained).collect();

        let mut removed: Vec<[usize; 2]> = removed
            .into_iter()
            .filter_map(|(bound, _, id)| {
                let is_match = extract(&mut shift_from, diff, |_, _, _, _, lhs| *lhs == id);
                let [n0, b0] = bound.get();
                // If there is a matching bound still in self.bounds, it means that
                // one of the bounds was outside of the range.
                // When this happens, we are assuming that the caller is not removing
                // said bound, so we have to do it in this function.
                // Also, I have to collect beforehand, in order to not remove bounds
                // while using the sh_bounds closure.
                self.list
                    .extract_if(.., is_match)
                    .next()
                    .map(|(bound, ..)| {
                        let [n1, b1] = bound.get();
                        [[n0, b0], [n1, b1]]
                    })
            })
            .flatten()
            .collect();

        self.shift_state.set((shift_from, diff));

        removed.sort_unstable();

        removed
    }

    /// Attempts to remove a range in the list
    ///
    /// If none of the bounds matched, returns [`None`], if only one
    /// of them matched, returns [`Some(false)`], otherwise, if
    /// successful, returns [`Some(true)`]
    ///
    /// [`Some(false)`]: Some
    /// [`Some(true)`]: Some
    pub fn remove_range(&mut self, range: Range<usize>) -> Option<bool> {
        let (s_n, e_n) = (range.start, range.end);
        let s_i = self.list.binary_search_by_key(&s_n, |(b, ..)| b.get()[0]);
        let e_i = self.list.binary_search_by_key(&e_n, |(b, ..)| b.get()[0]);

        match (s_i, e_i) {
            (Ok(s_i), Ok(e_i)) if self.list[s_i].2 == self.list[e_i].2 => {
                let (mut shift_from, total_diff) = self.shift_state.take();

                shift_from -= (e_i < shift_from) as usize;
                self.list.remove(e_i);
                shift_from -= (s_i < shift_from) as usize;
                self.list.remove(s_i);

                if shift_from < self.list.len() {
                    self.shift_state.set((shift_from, total_diff));
                }

                Some(true)
            }
            (Ok(_), Ok(_)) | (Ok(_), Err(_)) | (Err(_), Ok(_)) => Some(false),
            (Err(_), Err(_)) => None,
        }
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
            let iter = self.list.iter().filter(|(_, tag, _)| tag.is_start());
            for (i, (bound0, _, id)) in iter.enumerate() {
                let [n0, _] = bound0.get();
                let (j, (bound1, ..)) = self.list[(i + 1)..]
                    .iter()
                    .enumerate()
                    .find(|(_, (.., lhs))| lhs == id)
                    .unwrap();

                let [n1, _] = bound1.get();
                if n1 - n0 < self.min_len {
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
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (&Cell<[usize; 2]>, RawTag)> + Clone {
        self.list.iter().map(|(b, tag, _)| (b, *tag))
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
    pub fn match_of(&self, index: usize) -> Option<([usize; 2], RawTag)> {
        let index_of = |(bound, ..): &(Cell<[usize; 2]>, _, _)| bound.get()[0];
        let i = self.list.binary_search_by_key(&index, index_of).ok()?;

        let (_, tag, lhs) = self.list[i];

        let matched = if tag.is_start() {
            self.list[(i + 1)..].iter().find(|(.., rhs)| lhs == *rhs)
        } else {
            self.list[..i].iter().rfind(|(.., rhs)| lhs == *rhs)
        };

        matched.map(|(bound, tag, _)| (bound.get(), *tag))
    }
}

fn before_id((bound, tag, _): &(Cell<[usize; 2]>, RawTag, RangeId)) -> ([usize; 2], RawTag) {
    (bound.get(), *tag)
}

pub(super) struct DebugBounds<'a>(pub(super) &'a super::Tags);
impl std::fmt::Debug for DebugBounds<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() && !self.0.bounds.list.is_empty() {
            f.write_str("[\n")?;
            let (sh_from, [total_n_diff, total_b_diff]) = self.0.bounds.shift_state.get();
            for (i, (bound, tag, id)) in self.0.bounds.list.iter().enumerate() {
                let bound = if i < sh_from {
                    bound.get()
                } else {
                    let [n, b] = bound.get();
                    [sh(n, total_n_diff), sh(b, total_b_diff)]
                };
                writeln!(f, "    {:?}", (bound, tag, id))?;
                writeln!(f, "        {:?}", self.0.buf.get(bound[0]))?;
            }
            f.write_str("]\n")?;
            writeln!(f, "{:#?}", self.0.bounds.min_len)?;
            writeln!(f, "{:#?}", self.0.bounds.shift_state.get())?;
            writeln!(f, "{:#?}", self.0.bounds.ranges_to_update)
        } else {
            write!(f, "{:?}", self.0.bounds)
        }
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
