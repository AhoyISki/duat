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

/// A struct that keeps track of positions
pub trait Record: std::fmt::Debug + Default + Clone + Copy + Eq + Ord + 'static {
    /// The bytes at of this [`Record`]
    fn units(&self) -> usize;

    /// Adds the values of two [`Record`]unwrap_or_default()
    fn add(self, other: Self) -> Self;

    /// Subtracts the values of two [`Record`]s
    fn sub(self, other: Self) -> Self;

    fn len_per_record() -> usize;
}

impl Record for [usize; 2] {
    fn units(&self) -> usize {
        self[0]
    }

    fn add(self, other: Self) -> Self {
        [self[0] + other[0], self[1] + other[1]]
    }

    fn sub(self, other: Self) -> Self {
        [self[0] - other[0], self[1] - other[1]]
    }

    fn len_per_record() -> usize {
        64
    }
}

impl Record for [usize; 3] {
    fn units(&self) -> usize {
        self[0]
    }

    fn add(self, other: Self) -> Self {
        [self[0] + other[0], self[1] + other[1], self[2] + other[2]]
    }

    fn sub(self, other: Self) -> Self {
        [self[0] - other[0], self[1] - other[1], self[2] - other[2]]
    }

    fn len_per_record() -> usize {
        64
    }
}

/// The records of a [`Text`] or [`InnerTags`]
///
/// [`Text`]: super::Text
/// [`InnerTags`]: super::InnerTags
#[derive(Clone, PartialEq, Eq)]
pub struct Records<R: Record> {
    last_used: (usize, R),
    max: R,
    pub stored: Vec<R>,
}

impl<R: Record> Records<R> {
    /// Creates a new [`Records`]
    pub fn new() -> Self {
        Self {
            last_used: (0, R::default()),
            max: R::default(),
            stored: vec![R::default()],
        }
    }

    /// Creates a new [`Records`] with a given len
    pub fn with_max(max: R) -> Self {
        Self {
            max,
            stored: vec![max],
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
        let (i, prev) = self.search(new.units(), Record::units);

        let next = self.stored.get(i).map(|len| len.add(prev));
        if prev.units().abs_diff(new.units()) < R::len_per_record()
            || (i + 1 < self.stored.len()
                && next.unwrap().add(prev).units().abs_diff(new.units()) < R::len_per_record())
        {
            return;
        }

        if let Some(rec) = self.stored.get_mut(i) {
            let len = *rec;
            *rec = new.sub(prev);
            self.stored.insert(i + 1, len.sub(new.sub(prev)));
            self.last_used = (i + 1, new);
        } else {
            self.stored.insert(i, self.max.sub(new));
            self.last_used = (i, new);
        }
    }

    /// Transforms a range in the [`Records`]
    pub fn transform(&mut self, start: R, old_len: R, new_len: R) {
        let (s_i, s_rec) = self.search(start.units(), Record::units);
        let (e_i, e_rec) = self.search(start.units() + old_len.units(), Record::units);
        let e_len = self.stored.get(e_i).cloned().unwrap_or_default();

        if s_i < e_i {
            let end = (e_i).min(self.stored.len() - 1);
            self.stored.splice((s_i + 1)..=end, []);
        }

        // Transformation of the beginning len.
        if let Some(len) = self.stored.get_mut(s_i) {
            *len = new_len.add(e_rec.add(e_len).sub(old_len)).sub(s_rec);
            let len = *len;

            // Removing if new_len has size 0 (no tags or skips).
            // If there are no tags or skips, no skip will start
            // exactly on this record, making it invalid.
            self.last_used = if let Some(prev_i) = s_i.checked_sub(1)
                && start.units() == s_rec.units()
            {
                let prev_len = self.stored.get_mut(prev_i).unwrap();
                *prev_len = prev_len.add(len);
                self.stored.remove(prev_i + 1);

                (s_i, s_rec.add(len))
            } else {
                (s_i + 1, s_rec.add(len))
            };
        } else if let Some(last) = self.stored.last_mut() {
            *last = last.add(new_len).sub(old_len);

            self.last_used = (s_i, s_rec.add(new_len).sub(old_len));
        };

        self.max = self.max.add(new_len).sub(old_len);
    }

    /// Transforms the end of the [`Records`]
    pub fn append(&mut self, r: R) {
        self.transform(self.max, R::default(), r)
    }

    /// Extends this [`Records`] with another
    pub fn extend(&mut self, mut other: Records<R>) {
        let self_e_len = self.stored.last().copied().unwrap_or_default();
        // If there was no first record, nothing needs to be done.
        let Some(other_s_len) = other.stored.first_mut() else {
            self.max = self.max.add(other.max);
            return;
        };

        if self_e_len.units() + other_s_len.units() < R::len_per_record() {
            self.last_used = (self.stored.len(), self.max.add(*other_s_len));
            *other_s_len = self_e_len.add(*other_s_len);
            self.stored.pop();
        }

        self.stored.extend(other.stored);
        self.max = self.max.add(other.max);
    }

    /// Clears all [`Record`]s and makes the len 0
    pub fn clear(&mut self) {
        self.last_used = (0, R::default());
        self.max = R::default();
        self.stored = Vec::new();
    }

    /// The maximum [`Record`]
    pub fn max(&self) -> R {
        self.max
    }

    /// Returns the [`Record`] closest to the byte
    pub fn closest_to(&self, u: usize) -> R {
        let (i, rec) = self.search(u, Record::units);
        let len = self.stored.get(i).cloned().unwrap_or_default();

        if rec.units().abs_diff(u) > len.add(rec).units().abs_diff(u) {
            len.add(rec)
        } else {
            rec
        }
    }

    /// The [`Record`] closest to `at` by a key extracting function
    pub fn closest_to_by_key(&self, at: usize, key_f: fn(&R) -> usize) -> R {
        let (i, rec) = self.search(at, key_f);
        let len = self.stored.get(i).cloned().unwrap_or_default();

        if key_f(&rec).abs_diff(at) > key_f(&len.add(rec)).abs_diff(at) {
            len.add(rec)
        } else {
            rec
        }
    }

    /// Search for `at` with a key extracting function
    fn search(&self, at: usize, key_f: fn(&R) -> usize) -> (usize, R) {
        let (n, mut rec) = self.last_used;

        if at == key_f(&self.max) {
            return (self.stored.len(), self.max);
        } else if at == key_f(&self.last_used.1) {
            return self.last_used;
        }

        if at >= key_f(&rec) {
            let mut i = n;
            for len in &self.stored[n..] {
                if key_f(&rec.add(*len)) > at {
                    return (i, rec);
                }
                rec = rec.add(*len);
                i += 1;
            }
        } else {
            let mut i = n;
            for len in self.stored[..n].iter().rev() {
                i -= 1;
                rec = rec.sub(*len);
                if key_f(&rec) <= at {
                    return (i, rec);
                }
            }
        }

        (self.stored.len(), self.max)
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
            .field("last", &format!("{:?}", self.last_used))
            .field("max", &format!("{:?}", self.max))
            .field("stored", &format!("{:?}", self.stored))
            .finish()
    }
}
