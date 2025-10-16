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
/// [`Buffer`]: crate::prelude::Buffer
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
    pub fn add(&mut self, new: Range<usize>) {
        assert_range(&new);
        if new.len() < self.min_len {
            return;
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
    }

    /// Removes a [`Range`] from the list, returning an
    /// [`Iterator`] over the [`Range`]s that were taken
    pub fn remove(
        &mut self,
        within: Range<usize>,
    ) -> impl ExactSizeIterator<Item = Range<usize>> + '_ {
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
            .map(|r| r.start as usize..r.end as usize)
    }

    /// Applies the [`add`] function to another [`Ranges`]s
    ///
    /// [`add`]: Self::add
    pub fn merge(&mut self, other: Self) {
        for range in other.list {
            self.add(range.start as usize..range.end as usize)
        }
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
    pub fn iter_over(&self, within: Range<usize>) -> impl Iterator<Item = Range<usize>> {
        assert_range(&within);
        let within = within.start as i32..within.end as i32;

        let m_range = merging_range_by_guess_and_lazy_shift(
            (&self.list, self.list.len()),
            (self.from, [within.start, within.end]),
            (self.from, self.shift, 0, std::ops::Add::add),
            (|r| r.start, |r| r.end),
        );

        let (s0, s1) = self.list.range(m_range.clone()).as_slices();
        s0.iter().chain(s1).enumerate().map(move |(i, range)| {
            let mut range = if i + m_range.start > self.from {
                range.start + self.shift..range.end + self.shift
            } else {
                range.clone()
            };

            if i == 0 {
                range.start = range.start.max(within.start);
            }
            if i == m_range.len() - 1 {
                range.end = range.end.min(within.end);
            }

            range.start as usize..range.end as usize
        })
    }
}

impl IntoIterator for Ranges {
    type IntoIter = IntoIter;
    type Item = Range<usize>;

    #[define_opaque(IntoIter)]
    fn into_iter(self) -> Self::IntoIter {
        self.list.into_iter().enumerate().map(move |(i, range)| {
            if i >= self.from {
                (range.start + self.shift) as usize..(range.end + self.shift) as usize
            } else {
                range.start as usize..range.end as usize
            }
        })
    }
}

pub type IntoIter = impl ExactSizeIterator<Item = Range<usize>>;

fn assert_range(range: &Range<usize>) {
    assert!(
        range.start <= range.end,
        "range starts at {} but ends at {}",
        range.start,
        range.end
    );
}
