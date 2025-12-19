//! The [`Ranges`] struct
//!
//! This struct uses the same principle as the `ShiftList` in the
//! [`crate::text`] module. That is, it has an internal `shift` value
//! to displace all [`Range`]s ahead of the `from` index within. This
//! enables efficient localized insertion, like we have with
//! [`GapBuffer`]s, while maintaining the ability to binary search
//! over the list, getting both efficient insertion as well as
//! traversal.
use std::ops::Range;

use gapbuf::{GapBuffer, gap_buffer};

use crate::utils::merging_range_by_guess_and_lazy_shift;

/// A list of non intersecting exclusive [`Range<usize>`]s
///
/// The primary purpose of this struct is to serve [`Parser`]s by
/// telling Duat which ranges need to be updated. This lets Duat
/// minimize as much as possible the amount of work done to update
/// the [`Text`] when it changes in a [`Buffer`].
///
/// [`Text`]: crate::text::Text
/// [`Buffer`]: crate::buffer::Buffer
/// [`Parser`]: crate::buffer::Parser
#[derive(Clone, Default, Debug)]
pub struct Ranges {
    list: GapBuffer<Range<i32>>,
    from: usize,
    shift: i32,
    min_len: usize,
}

impl Ranges {
    /// Return a new instance of [`Ranges`] with an initial
    /// [`Range`]
    pub fn new(range: Range<usize>) -> Self {
        Self {
            list: gap_buffer![range.start as i32..range.end as i32],
            ..Self::empty()
        }
    }

    /// Returns a new empty [`Ranges`]
    pub fn empty() -> Self {
        Self {
            list: GapBuffer::new(),
            from: 0,
            shift: 0,
            min_len: 1,
        }
    }

    /// Sets a minimum length to keep [`Range`]s
    pub fn set_min_len(&mut self, min: usize) {
        self.min_len = min.max(1);

        let mut i = 0;
        self.list.retain(|range| {
            i += 1;

            if range.len() < self.min_len {
                if i - 1 < self.from {
                    self.from -= 1;
                }
                false
            } else {
                true
            }
        });
    }

    /// Adds a range to the list of [`Range<usize>`]s
    ///
    /// This range will be merged in with the others on the list,
    /// so it may bridge gaps between ranges or for longer
    /// ranges within, without allowing for the existance
    /// of intersecting ranges.
    ///
    /// This function will return `true` if the `Ranges` were changed
    /// by this addition.
    #[track_caller]
    pub fn add(&mut self, new: Range<usize>) -> bool {
        assert_range(&new);
        if new.len() < self.min_len {
            return false;
        }

        let new = new.start as i32..new.end as i32;

        let m_range = merging_range_by_guess_and_lazy_shift(
            (&self.list, self.list.len()),
            (self.from, [new.start, new.end]),
            (self.from, self.shift, 0, std::ops::Add::add),
            (|r| r.start, |r| r.end),
        );

        // The ranges in this region have not been callibrated yet.
        if self.from <= m_range.end {
            for range in self.list.range_mut(self.from..m_range.end).iter_mut() {
                range.start += self.shift;
                range.end += self.shift;
            }
        }

        let changed_ranges = if m_range.len() == 1 {
            let range = self.list[m_range.start].clone();
            !(range.start <= new.start && range.end >= new.end)
        } else {
            true
        };

        let added_range = {
            let slice = self.list.range(m_range.clone());
            let mut iter = slice.iter();

            let first = iter.next().cloned().unwrap_or(new.clone());
            let last = iter.next_back().cloned().unwrap_or(first.clone());

            first.start.min(new.start)..last.end.max(new.end)
        };

        let new_from = self.from.saturating_sub(m_range.len()).max(m_range.start) + 1;
        if new_from < self.list.len() + 1 - m_range.len() {
            self.from = new_from;
        } else {
            self.from = 0;
            self.shift = 0;
        }

        self.list.splice(m_range.clone(), [added_range]);

        changed_ranges
    }

    /// Adds many [`Range`]s to be merged with the existing ones
    ///
    /// This function will return `true` if the `Ranges` were changed
    /// by this addition.
    pub fn extend(&mut self, ranges: impl IntoIterator<Item = Range<usize>>) -> bool {
        let mut has_changed = false;
        for range in ranges {
            has_changed |= self.add(range);
        }
        has_changed
    }

    /// Removes a [`Range`] from the list, returning an
    /// [`Iterator`] over the [`Range`]s that were taken
    #[track_caller]
    pub fn remove_on(&mut self, within: Range<usize>) -> impl Iterator<Item = Range<usize>> {
        assert_range(&within);
        let within = within.start as i32..within.end as i32;

        let m_range = merging_range_by_guess_and_lazy_shift(
            (&self.list, self.list.len()),
            (self.from, [within.start, within.end]),
            (self.from, self.shift, 0, std::ops::Add::add),
            (|r| r.start, |r| r.end),
        );

        // The ranges in this region have not been callibrated yet.
        if self.from <= m_range.end {
            for range in self.list.range_mut(self.from..m_range.end).iter_mut() {
                range.start += self.shift;
                range.end += self.shift;
            }
        }

        let split_off = {
            let slice = self.list.range(m_range.clone());

            let first = slice.iter().next().cloned();
            let last = slice.iter().next_back().cloned().or(first.clone());

            let split_start = first.filter(|f| f.start < within.start).map(|first| {
                self.list[m_range.start].start = within.start;
                first.start..within.start
            });
            let split_end = last.filter(|l| l.end > within.end).map(|last| {
                self.list[m_range.end - 1].end = within.end;
                within.end..last.end
            });

            let min_len = |range: &Range<i32>| range.len() >= self.min_len;

            [split_start.filter(min_len), split_end.filter(min_len)]
        };

        let added = split_off.iter().flatten().count();

        let new_from = self.from.saturating_sub(m_range.len()).max(m_range.start) + added;
        if new_from < self.list.len() + added - m_range.len() {
            self.from = new_from;
        } else {
            self.from = 0;
            self.shift = 0;
        }

        self.list
            .splice(m_range, split_off.into_iter().flatten())
            .filter_map(|r| (!r.is_empty()).then_some(r.start as usize..r.end as usize))
    }

    /// Removes all [`Range`]s that intersect with the given one,
    /// returning an [`Iterator`] over them
    #[track_caller]
    pub fn remove_intersecting(
        &mut self,
        within: Range<usize>,
    ) -> impl Iterator<Item = Range<usize>> {
        assert_range(&within);
        let within = within.start as i32..within.end as i32;

        let m_range = merging_range_by_guess_and_lazy_shift(
            (&self.list, self.list.len()),
            (self.from, [within.start, within.end]),
            (self.from, self.shift, 0, std::ops::Add::add),
            (|r| r.start, |r| r.end),
        );

        // The ranges in this region have not been callibrated yet.
        if self.from <= m_range.end {
            for range in self.list.range_mut(self.from..m_range.end).iter_mut() {
                range.start += self.shift;
                range.end += self.shift;
            }
        }

        let new_from = self.from.saturating_sub(m_range.len()).max(m_range.start);
        if new_from < self.list.len() - m_range.len() {
            self.from = new_from;
        } else {
            self.from = 0;
            self.shift = 0;
        }

        self.list
            .drain(m_range)
            .map(|range| range.start as usize..range.end as usize)
    }

    /// Applies the [`add`] function to another [`Ranges`]s
    ///
    /// [`add`]: Self::add
    #[track_caller]
    pub fn merge(&mut self, other: Self) -> bool {
        let mut has_changed = false;
        for range in other.list {
            has_changed |= self.add(range.start as usize..range.end as usize);
        }
        has_changed
    }

    /// Shifts the [`Range`]s within by a [`Change`]
    ///
    /// If the `diff` is negative (i.e. a part of the ranges were
    /// removed), then ranges will be removed ahead of `from`
    /// accordingly.
    ///
    /// [`Change`]: crate::text::Change
    pub fn shift_by(&mut self, from: usize, shift: i32) {
        let from = from as i32;

        // The range of changes that will be drained
        let m_range = merging_range_by_guess_and_lazy_shift(
            (&self.list, self.list.len()),
            (self.from, [from, from - shift.min(0)]),
            (self.from, self.shift, 0, std::ops::Add::add),
            (|r| r.start, |r| r.end),
        );

        // If self.from < m_range.end, I need to shift the changes between
        // the two, so that they match the shifting of the changes
        // before self.from.
        if self.from < m_range.end && self.shift != 0 {
            for range in self.list.range_mut(self.from..m_range.end).iter_mut() {
                range.start += self.shift;
                range.end += self.shift;
            }
        // If self.from > m_range.end, then I shift the ranges
        // inbetween by -self.shift, in order to keep the ranges
        // after m_range.start equally unshifted by self.shift +
        // shift.
        } else if self.from > m_range.end && self.shift != 0 {
            for range in &mut self.list.range_mut(m_range.end..self.from) {
                range.start -= self.shift;
                range.end -= self.shift;
            }
        }

        let range_left = {
            let slice = self.list.range(m_range.clone());
            let mut iter = slice.iter();

            iter.next().and_then(|first| {
                let last = iter.next_back().unwrap_or(first);
                let range = first.start.min(from)..(last.end + shift).max(from);

                (range.len() >= self.min_len).then_some(range)
            })
        };

        let new_from = m_range.start + range_left.is_some() as usize;

        self.list.splice(m_range.clone(), range_left);

        if new_from < self.list.len() {
            self.from = new_from;
            self.shift += shift;
        } else {
            self.from = 0;
            self.shift = 0;
        }
    }

    /// Returns the number of [`Range<usize>`]s
    pub fn len(&self) -> usize {
        self.list.len()
    }

    /// Returns `true` if there are no [`Range<usize>`]s
    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }

    /// An [`Iterator`] over the [`Range`]s in this list
    pub fn iter(&self) -> impl Iterator<Item = Range<usize>> {
        self.list.iter().enumerate().map(move |(i, range)| {
            if i >= self.from {
                (range.start + self.shift) as usize..(range.end + self.shift) as usize
            } else {
                range.start as usize..range.end as usize
            }
        })
    }

    /// The same as [`Ranges::remove`], but without removing, just
    /// iterating over the relevant ranges
    ///
    /// This method will trim the iterated [`Range`]s to the bounds of
    /// `within`. If you want a non trimmed version of this method,
    /// check out [`iter_intersecting`].
    ///
    /// [`iter_intersecting`]: Self::iter_over_incl
    #[track_caller]
    pub fn iter_over(&self, within: Range<usize>) -> impl Iterator<Item = Range<usize>> {
        self.iter_intersecting(within.clone())
            .map(move |range| range.start.max(within.start)..range.end.min(within.end))
    }

    /// Iterates over all [`Range`]s that intersect with `within`
    ///
    /// If you want to automatically trim those ranges to the bounds
    /// of `within`, check out [`iter_over`]
    ///
    /// [`iter_over`]: Self::iter_over
    #[track_caller]
    pub fn iter_intersecting(&self, within: Range<usize>) -> impl Iterator<Item = Range<usize>> {
        assert_range(&within);
        let m_range = merging_range_by_guess_and_lazy_shift(
            (&self.list, self.list.len()),
            (self.from, [within.start as i32, within.end as i32]),
            (self.from, self.shift, 0, std::ops::Add::add),
            (|r| r.start, |r| r.end),
        );

        let (s0, s1) = self.list.range(m_range.clone()).as_slices();
        s0.iter().chain(s1).enumerate().map(move |(i, range)| {
            let range = if i + m_range.start >= self.from {
                range.start + self.shift..range.end + self.shift
            } else {
                range.clone()
            };

            range.start as usize..range.end as usize
        })
    }

    /// Wether any [`Range`] in this `Ranges` intersects with the
    /// given `range`
    pub fn intersects_with(&self, range: Range<usize>) -> bool {
        assert_range(&range);

        let m_range = merging_range_by_guess_and_lazy_shift(
            (&self.list, self.list.len()),
            (self.from, [range.start as i32, range.end as i32]),
            (self.from, self.shift, 0, std::ops::Add::add),
            (|r| r.start, |r| r.end),
        );

        !m_range.is_empty()
    }
}

impl IntoIterator for Ranges {
    type IntoIter = IntoIter;
    type Item = Range<usize>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            iter: self.list.into_iter().enumerate(),
            from: self.from,
            shift: self.shift,
        }
    }
}

pub struct IntoIter {
    iter: std::iter::Enumerate<gapbuf::IntoIter<Range<i32>>>,
    from: usize,
    shift: i32,
}

impl Iterator for IntoIter {
    type Item = Range<usize>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(i, range)| {
            if i >= self.from {
                (range.start + self.shift) as usize..(range.end + self.shift) as usize
            } else {
                range.start as usize..range.end as usize
            }
        })
    }
}

impl ExactSizeIterator for IntoIter {}

impl PartialEq for Ranges {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().eq(other.iter())
    }
}

impl Eq for Ranges {}

impl PartialOrd for Ranges {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Ranges {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let cmp = |r: Range<usize>| [r.start, r.end];
        self.iter().flat_map(cmp).cmp(other.iter().flat_map(cmp))
    }
}

#[track_caller]
fn assert_range(range: &Range<usize>) {
    assert!(
        range.start <= range.end,
        "range starts at {} but ends at {}",
        range.start,
        range.end
    );
}
