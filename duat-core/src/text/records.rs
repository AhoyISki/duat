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

const MAX_PER_RECORD: u32 = 256;

use super::shift_list::{Shift, ShiftList, Shiftable};

/// The records of a [`Text`] or [`InnerTags`]
///
/// [`Text`]: super::Text
/// [`InnerTags`]: super::InnerTags
#[derive(Default, Clone, Debug)]
pub struct Records(ShiftList<[u32; 3]>);

impl Records {
    /// Creates a new [`Records`]
    pub fn new(max: [usize; 3]) -> Self {
        Self(ShiftList::new(max.map(|x| x as i32)))
    }

    /// Insert a new [`Record`], if it would fit
    pub fn insert(&mut self, new: [usize; 3]) {
        // Quick early return.
        if MAX_PER_RECORD * 2 > self.0.max()[0] as u32 {
            return;
        }
        let new = new.map(|x| x as u32);

        // For internal functions, I assume that I'm not going over self.max.
        let i = match self.0.find_by_key(new[0], |[b, ..]| b) {
            Ok(_) => return,
            Err(i) => {
                let next = self.0.get(i);
                let prev = i.checked_sub(1).and_then(|prev_i| self.0.get(prev_i));
                if next.is_none_or(|[b, ..]| b >= new[0] + MAX_PER_RECORD)
                    && prev.is_none_or(|[b, ..]| b + MAX_PER_RECORD <= new[0])
                {
                    i
                } else {
                    return;
                }
            }
        };

        self.0.insert(i, new);
    }

    /// Transforms a range in the [`Records`]
    pub fn transform(&mut self, start: [usize; 3], old_len: [usize; 3], new_len: [usize; 3]) {
        let (Ok(s_i) | Err(s_i)) = self.0.find_by_key(start[0] as u32, |[b, ..]| b);
        let (Ok(e_i) | Err(e_i)) = self
            .0
            .find_by_key((start[0] + old_len[0]) as u32, |[b, ..]| b);

        self.0
            .extract_if_while(s_i..e_i, |_, _| Some(true))
            .for_each(|_| {});

        self.0.shift_by(s_i, [
            new_len[0] as i32 - old_len[0] as i32,
            new_len[1] as i32 - old_len[1] as i32,
            new_len[2] as i32 - old_len[2] as i32,
        ]);
    }

    /// The maximum [`Record`]
    pub fn max(&self) -> [usize; 3] {
        self.0.max().map(|x| x as usize)
    }

    /// The [`Record`] closest to `at` by a key extracting function
    pub fn closest_to_by_key(&self, key: usize, by: fn([u32; 3]) -> u32) -> [usize; 3] {
        let key = key as u32;
        match self.0.find_by_key(key, by) {
            Ok(i) => self.0.get(i).unwrap().map(|x| x as usize),
            Err(i) => {
                let prev = i.checked_sub(1).and_then(|prev_i| self.0.get(prev_i));
                let next = self.0.get(i);

                let (prev, next) = match (prev, next) {
                    (None, None) => ([0; 3], self.0.max().map(|x| x as u32)),
                    (None, Some(next)) => ([0; 3], next),
                    (Some(prev), None) => (prev, self.0.max().map(|x| x as u32)),
                    (Some(prev), Some(next)) => (prev, next),
                };

                if key - by(prev) > by(next) - key {
                    next.map(|x| x as usize)
                } else {
                    prev.map(|x| x as usize)
                }
            }
        }
    }
}

impl Shiftable for [u32; 3] {
    type Shift = [i32; 3];

    fn shift(self, by: Self::Shift) -> Self {
        let sh = |i: usize| (self[i] as i32 + by[i]) as u32;
        [sh(0), sh(1), sh(2)]
    }
}

impl Shift for [i32; 3] {
    fn neg(self) -> Self {
        [-self[0], -self[1], -self[2]]
    }

    fn add(self, other: Self) -> Self {
        [self[0] + other[0], self[1] + other[1], self[2] + other[2]]
    }
}
