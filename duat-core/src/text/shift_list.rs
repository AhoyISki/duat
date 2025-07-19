use std::ops::RangeBounds;

use gapbuf::GapBuffer;

use crate::{binary_search_by_key_and_index, get_ends};

/// A sorted list of elements which keeps track of changes in a very
/// efficient manner
///
/// This list of elements can very efficiently shift its elements by
/// adhering to "shifting state". That is, it will keep track of two
/// ranges: the elements that have been shifted, and those yet to be.
///
/// By keeping track of this, it allows for binary search, without
/// having to update all elements to match changes in the underlying
/// structure, by simply returning the shifted elements, depending on
/// which group they belong to.
#[derive(Default, Clone, Debug)]
pub(super) struct ShiftList<S: Shiftable> {
    buf: GapBuffer<S>,
    from: usize,
    by: S::Shift,
    max: S::Shift,
}

impl<S: Shiftable> ShiftList<S> {
    /// Returns a new [`ShiftList`]
    pub(super) fn new(max: S::Shift) -> Self {
        Self {
            buf: GapBuffer::new(),
            from: 0,
            by: S::Shift::default(),
            max,
        }
    }

    /// Inserts a new element into the [`ShiftList`]
    ///
    /// The position where this element will be inserted needs to
    /// first be acquired by calling [`ShiftList::find_by_key`].
    pub(super) fn insert(&mut self, i: usize, new: S) {
        if self.by != S::Shift::default() && self.from < self.buf.len() {
            if i > self.from {
                for s in self.buf.range_mut(self.from..i).iter_mut() {
                    *s = s.shift(self.by);
                }
            } else {
                for s in self.buf.range_mut(i..self.from).iter_mut() {
                    *s = s.shift(self.by.neg())
                }
            }
        }

        self.buf.insert(i, new);

        if i + 1 < self.buf.len() && self.by != S::Shift::default() {
            self.from = i + 1;
        } else {
            self.from = 0;
            self.by = S::Shift::default();
        }
    }

    /// Removes the ith element from the [`ShiftList`]
    pub(super) fn remove(&mut self, i: usize) {
        self.buf.remove(i);
        if self.from > i {
            self.from -= 1;
        }
    }

    /// Extracts forward from a range of element indices
    ///
    /// This function takes a filter that continues trying to remove
    /// while it returns [`Some`]. It removes on [`Some(true)`], skips
    /// on [`Some(false)`], and halts at [`None`].
    ///
    /// [`Some(true)`]: Some
    /// [`Some(false)`]: Some
    #[inline]
    pub(super) fn extract_if_while<'a>(
        &'a mut self,
        range: impl RangeBounds<usize>,
        mut f: impl FnMut(usize, S) -> Option<bool> + 'a,
    ) -> impl Iterator<Item = (usize, S)> + 'a {
        let (mut i, mut end) = get_ends(range, self.buf.len());

        std::iter::from_fn(move || {
            while i < end {
                let shifted = if i >= self.from {
                    self.buf[i].shift(self.by)
                } else {
                    self.buf[i]
                };

                if f(i, shifted)? {
                    self.from -= (i < self.from) as usize;
                    end -= 1;
                    return Some((i, shifted));
                } else {
                    i += 1;
                }
            }

            None
        })
    }

    /// Extracts backwards from a range of element indices
    ///
    /// This function takes a filter that continues trying to remove
    /// while it returns [`Some`]. It removes on [`Some(true)`], skips
    /// on [`Some(false)`], and halts at [`None`].
    ///
    /// [`Some(true)`]: Some
    /// [`Some(false)`]: Some
    #[inline]
    pub(super) fn rextract_if_while<'a>(
        &'a mut self,
        range: impl RangeBounds<usize>,
        mut f: impl FnMut(usize, S) -> Option<bool> + 'a,
    ) -> impl Iterator<Item = (usize, S)> + 'a {
        let (start, mut i) = get_ends(range, self.buf.len());

        std::iter::from_fn(move || {
            while i > start {
                i -= 1;
                let shifted = if i >= self.from {
                    self.buf[i].shift(self.by)
                } else {
                    self.buf[i]
                };

                if f(i, shifted)? {
                    self.from -= (i < self.from) as usize;
                    return Some((i, shifted));
                }
            }

            None
        })
    }

    /// Shifts the items in the list after a certain point
    pub(super) fn shift_by(&mut self, from: usize, by: S::Shift) {
        if self.by != S::Shift::default() {
            if from > self.from {
                for s in self.buf.range_mut(self.from..from).iter_mut() {
                    *s = s.shift(self.by);
                }
            } else {
                for s in self.buf.range_mut(from..self.from).iter_mut() {
                    *s = s.shift(self.by.neg());
                }
            }
        }

        self.from = from;
        self.by = self.by.add(by);
    }

    /// Extends this [`ShiftList`] with another
    pub(super) fn extend(&mut self, mut other: Self) {
        for s in self.buf.range_mut(self.from..).iter_mut() {
            *s = s.shift(self.by);
        }

        self.from = self.buf.len() + other.from;
        self.by = other.by;

        self.buf
            .extend(other.buf.drain(..).map(|s| s.shift(self.max)));

        self.max = self.max.add(other.max);
    }

    /// Iterates forward on a range of indices
    #[inline]
    pub(super) fn iter_fwd(
        &self,
        range: impl RangeBounds<usize>,
    ) -> impl Iterator<Item = (usize, S)> + Clone + '_ {
        let (start, end) = get_ends(range, self.buf.len());
        let (s0, s1) = self.buf.range(start..end).as_slices();
        s0.iter().chain(s1).enumerate().map(move |(i, s)| {
            if i + start >= self.from {
                (i + start, s.shift(self.by))
            } else {
                (i + start, *s)
            }
        })
    }

    /// Iterates backwards on a range of indices
    #[inline]
    pub(super) fn iter_rev(
        &self,
        range: impl RangeBounds<usize>,
    ) -> impl Iterator<Item = (usize, S)> + Clone + '_ {
        let (start, end) = get_ends(range, self.buf.len());
        let (s0, s1) = self.buf.range(start..end).as_slices();
        let iter = s1.iter().rev().chain(s0.iter().rev());
        iter.enumerate().map(move |(i, s)| {
            if end - i >= self.from {
                (end - i, s.shift(self.by))
            } else {
                (end - i, *s)
            }
        })
    }

    /// Will find the _first_ element in the buffer that equals
    /// the key, if it exists
    pub(super) fn find_by_key<K: Copy + Eq + Ord>(
        &self,
        key: K,
        f: fn(S) -> K,
    ) -> Result<usize, usize> {
        let sh = |i: usize, s: &S| f(if i >= self.from { s.shift(self.by) } else { *s });

        match binary_search_by_key_and_index(&self.buf, self.buf.len(), key, sh) {
            Ok(mut i) => Ok(loop {
                if let Some(prev_i) = i.checked_sub(1)
                    && sh(prev_i, &self.buf[prev_i]) == key
                {
                    i = prev_i
                } else {
                    break i;
                }
            }),
            Err(i) => Err(i),
        }
    }

    /// The [`GapBuffer`] within
    pub(super) fn buf(&self) -> &GapBuffer<S> {
        &self.buf
    }

    /// The maximum value allowed in the list
    pub(super) fn max(&self) -> S::Shift {
        self.max
    }
}

pub(super) trait Shiftable: Copy + Ord {
    type Shift: Shift;

    fn shift(self, by: Self::Shift) -> Self;
}

pub(super) trait Shift: Default + Copy + Eq {
    fn neg(self) -> Self;

    fn add(self, other: Self) -> Self;
}
