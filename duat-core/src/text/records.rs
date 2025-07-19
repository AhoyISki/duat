//! Records are an internal struct used for O(log(n)) searching
//!
//! They work similarly to how a rope works, in that there are saved
//! positions which keep track of the amount of bytes, characters, and
//! lines (or bytes and tags) that show up between them and the next
//! record.
//!
//! This struct is used by the [`Text`] and the [`InnerTags`] structs.
//!
//! [`Text`]: super::Text
//! [`InnerTags`]: super::InnerTags

use gapbuf::{GapBuffer, gap_buffer};

use crate::{binary_search_by_key_and_index, text::sh};

/// A struct that keeps track of positions
pub trait Record: std::fmt::Debug + Default + Clone + Copy + Eq + Ord + 'static {
    type Signed: std::fmt::Debug + Default + Clone + Copy + Eq + Ord + 'static;

    /// The bytes at of this [`Record`]
    fn units(&self) -> usize;

    fn shift_units(shift: Self::Signed) -> i32;

    fn shift(&self, shift: Self::Signed) -> Self;

    fn len_per_record() -> usize;

    fn add(&self, other: Self) -> Self;

    fn sub(&self, other: Self) -> Self;

    fn neg(shift: Self::Signed) -> Self::Signed;

    fn add_shifts(l: Self::Signed, r: Self::Signed) -> Self::Signed;

    fn shift_diff(&self, other: Self) -> Self::Signed;
}

impl Record for [usize; 2] {
    type Signed = [i32; 2];

    fn units(&self) -> usize {
        self[0]
    }

    fn shift_units(shift: Self::Signed) -> i32 {
        shift[0]
    }

    fn shift(&self, shift: Self::Signed) -> Self {
        [
            (self[0] as i32 + shift[0]) as usize,
            (self[1] as i32 + shift[1]) as usize,
        ]
    }

    fn len_per_record() -> usize {
        64
    }

    fn add(&self, other: Self) -> Self {
        [self[0] + other[0], self[1] + other[1]]
    }

    fn sub(&self, other: Self) -> Self {
        [self[0] - other[0], self[1] - other[1]]
    }

    fn neg(shift: Self::Signed) -> Self::Signed {
        [-shift[0], -shift[1]]
    }

    fn add_shifts(l: Self::Signed, r: Self::Signed) -> Self::Signed {
        [l[0] + r[0], l[1] + r[1]]
    }

    fn shift_diff(&self, other: Self) -> Self::Signed {
        [
            self[0] as i32 - other[0] as i32,
            self[1] as i32 - other[1] as i32,
        ]
    }
}

impl Record for [usize; 3] {
    type Signed = [i32; 3];

    fn units(&self) -> usize {
        self[0]
    }

    fn shift_units(shift: Self::Signed) -> i32 {
        shift[0]
    }

    fn shift(&self, shift: Self::Signed) -> Self {
        [
            (self[0] as i32 + shift[0]) as usize,
            (self[1] as i32 + shift[1]) as usize,
            (self[2] as i32 + shift[2]) as usize,
        ]
    }

    fn len_per_record() -> usize {
        64
    }

    fn add(&self, other: Self) -> Self {
        [self[0] + other[0], self[1] + other[1], self[2] + other[2]]
    }

    fn sub(&self, other: Self) -> Self {
        [self[0] - other[0], self[1] - other[1], self[2] - other[2]]
    }

    fn neg(shift: Self::Signed) -> Self::Signed {
        [-shift[0], -shift[1], -shift[2]]
    }

    fn add_shifts(l: Self::Signed, r: Self::Signed) -> Self::Signed {
        [l[0] + r[0], l[1] + r[1], l[2] + r[2]]
    }

    fn shift_diff(&self, other: Self) -> Self::Signed {
        [
            self[0] as i32 - other[0] as i32,
            self[1] as i32 - other[1] as i32,
            self[2] as i32 - other[2] as i32,
        ]
    }
}

/// The records of a [`Text`] or [`InnerTags`]
///
/// [`Text`]: super::Text
/// [`InnerTags`]: super::InnerTags
#[derive(Clone, PartialEq, Eq)]
pub struct Records<R: Record> {
    max: R,
    list: GapBuffer<R>,
    shift_state: (usize, R::Signed),
}

impl<R: Record> Records<R> {
    /// Creates a new [`Records`]
    pub fn new() -> Self {
        Self {
            max: R::default(),
            list: gap_buffer![R::default()],
            shift_state: (0, R::Signed::default()),
        }
    }

    /// Creates a new [`Records`] with a given len
    pub fn with_max(max: R) -> Self {
        Self {
            max,
            list: gap_buffer![max],
            ..Self::new()
        }
    }

    /// Insert a new [`Record`], if it would fit
    pub fn insert(&mut self, new: R) {
        // Quick early return.
        if self.max.units() <= R::len_per_record() * 2 {
            return;
        }

        // For internal functions, I assume that I'm not going over self.max.
        let i = self.search(new.units(), R::units, R::shift_units);

        if (i > 0 && self.list[i - 1].units().abs_diff(new.units()) < R::len_per_record())
            || (i < self.list.len()
                && self.list[i].units().abs_diff(new.units()) < R::len_per_record())
        {
            return;
        }
        
        let (mut shift_from, shift) = std::mem::take(&mut self.shift_state);

        if shift_from <= i && shift != R::Signed::default() {
            for entry in self.list.range_mut(shift_from..i).iter_mut() {
                *entry = entry.shift(shift);
            }
        }
        shift_from = i + 1;

        self.list.insert(i, new);

        if shift_from < self.list.len() {
            self.shift_state = (shift_from, shift);
        }
    }

    /// Transforms a range in the [`Records`]
    pub fn transform(&mut self, start: R, old_len: R, new_len: R) {
        let (mut shift_from, mut shift) = std::mem::take(&mut self.shift_state);

        let s_i = self.search(start.units(), R::units, R::shift_units);
        let e_i = self.search(start.units() + old_len.units(), R::units, R::shift_units);

        if shift != R::Signed::default() {
            if s_i > shift_from {
                for rec in self.list.range_mut(shift_from..s_i).iter_mut() {
                    *rec = rec.shift(shift);
                }
            } else {
                for rec in self.list.range_mut(s_i..shift_from).iter_mut() {
                    *rec = rec.shift(R::neg(shift));
                }
            }
        }

        if s_i < e_i {
            let end = (e_i).min(self.list.len() - 1);
            self.list.splice((s_i + 1)..=end, []);
        }

        shift_from = s_i;
        shift = R::add_shifts(shift, new_len.shift_diff(old_len));
        self.max = self.max.add(new_len).sub(old_len);

        if shift_from < self.list.len() {
            self.shift_state = (shift_from, shift);
        }
    }

    /// Transforms the end of the [`Records`]
    pub fn append(&mut self, rec: R) {
        self.transform(self.max, R::default(), rec)
    }

    /// Extends this [`Records`] with another
    pub fn extend(&mut self, mut other: Records<R>) {
        for rec in self.list.range_mut(self.shift_state.0..).iter_mut() {
            *rec = rec.shift(self.shift_state.1);
        }

        for rec in other.list.iter_mut() {
            *rec = rec.add(self.max);
        }

        self.max = self.max.add(other.max);
        self.shift_state = (self.list.len() + other.shift_state.0, other.shift_state.1);
        self.list.extend(other.list);
    }

    /// The maximum [`Record`]
    pub fn max(&self) -> R {
        self.max
    }

    /// Returns the [`Record`] closest to the byte
    pub fn closest_to(&self, u: usize) -> R {
        let (shift_from, shift) = self.shift_state;
        let i = self.search(u, Record::units, R::shift_units);

        if let Some(rec) = self.list.get(i) {
            if i < shift_from {
                *rec
            } else {
                rec.shift(shift)
            }
        } else {
            R::default()
        }
    }

    /// The [`Record`] closest to `at` by a key extracting function
    pub fn closest_to_by_key(
        &self,
        at: usize,
        key_f: fn(&R) -> usize,
        s_units: fn(R::Signed) -> i32,
    ) -> R {
        let (shift_from, shift) = self.shift_state;
        let i = self.search(at, key_f, s_units);

        if let Some(rec) = self.list.get(i) {
            if i < shift_from {
                *rec
            } else {
                rec.shift(shift)
            }
        } else {
            R::default()
        }
    }

    /// Search for `at` with a key extracting function
    fn search(&self, at: usize, key_f: fn(&R) -> usize, s_units: fn(R::Signed) -> i32) -> usize {
        let (shift_from, shift) = self.shift_state;

        let key = |i: usize, entry: &R| {
            sh(
                key_f(entry),
                s_units(if i < shift_from {
                    R::Signed::default()
                } else {
                    shift
                }),
            )
        };

        let (Ok(i) | Err(i)) = binary_search_by_key_and_index(&self.list, self.list.len(), at, key);

        i
    }
}

impl<R: Record + Default> Default for Records<R> {
    fn default() -> Self {
        Self::new()
    }
}

impl<R: Record + std::fmt::Debug> std::fmt::Debug for Records<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Records")
            .field("max", &format!("{:?}", self.max))
            .field("stored", &format!("{:?}", self.list))
            .field("shift_state", &format!("{:?}", self.shift_state))
            .finish()
    }
}
