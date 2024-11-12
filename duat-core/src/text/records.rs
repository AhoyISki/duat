//! Records are an internal struct used for O(log(n)) searching
//!
//! They work similarly to how a rope works, in that there are saved
//! positions which keep track of the amount of bytes, characters, and
//! lines (or bytes and tags) that show up between them and the next
//! record.
//!
//! This struct is used by the [`Text`] and the [`Tags`] structs.
//!
//! [`Text`]: super::Text
//! [`Tags`]: super::Tags
use std::fmt::Debug;

/// Minimum length to insert a new record
const LEN_PER_RECORD: u32 = 150;

/// A struct that keeps track of positions
pub trait Record: Default + Debug + Clone + Copy + Eq + Ord + 'static {
    /// The bytes at of this [`Record`]
    fn bytes(&self) -> u32;

	/// Adds the values of two [`Record`]s
    fn add(self, other: Self) -> Self;

	/// Subtracts the values of two [`Record`]s
    fn sub(self, other: Self) -> Self;

	/// Whether or not this value has no bytes
    fn is_zero_len(&self) -> bool;
}

impl Record for (u32, u32) {
    fn bytes(&self) -> u32 {
        self.1
    }

    fn add(self, other: Self) -> Self {
        (self.0 + other.0, self.1 + other.1)
    }

    fn sub(self, other: Self) -> Self {
        (self.0 - other.0, self.1 - other.1)
    }

    fn is_zero_len(&self) -> bool {
        self.0 == 0
    }
}

impl Record for (u32, u32, u32) {
    fn bytes(&self) -> u32 {
        self.0
    }

    fn add(self, other: Self) -> Self {
        (self.0 + other.0, self.1 + other.1, self.2 + other.2)
    }

    fn sub(self, other: Self) -> Self {
        (self.0 - other.0, self.1 - other.1, self.2 - other.2)
    }

    fn is_zero_len(&self) -> bool {
        false
    }
}

/// The records of a [`Text`] or [`Tags`]
///
/// [`Text`]: super::Text
/// [`Tags`]: super::Tags
#[derive(Clone, PartialEq, Eq)]
pub struct Records<R: Record> {
    last: (u32, R),
    max: R,
    pub stored: Vec<R>,
}

impl<R: Record> Records<R> {
    /// Creates a new [`Records`]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new [`Records`] with a given len
    pub fn with_max(max: R) -> Self {
        Self {
            max,
            stored: vec![max],
            ..Self::default()
        }
    }

    /// Insert a new [`Record`], if it would fit
    pub fn insert(&mut self, new: R) {
        // For internal functions, I assume that I'm not
        // going over self.max.
        let (i, prev) = self.search(new.bytes(), Record::bytes);
        let i = i as usize;
        let len = *self.stored.get(i.min(self.stored.len() - 1)).unwrap();

        // If the recrds would be too close, don't add any
        if [prev, prev.add(len)]
            .iter()
            .any(|rec| rec.bytes().abs_diff(new.bytes()) < LEN_PER_RECORD)
        {
            return;
        }

        if let Some(rec) = self.stored.get_mut(i) {
            *rec = new.sub(prev);
            self.stored.insert(i + 1, len.sub(new.sub(prev)));
            self.last = (i as u32 + 1, new);
        }
    }

    /// Transforms a range in the [`Records`]
    pub fn transform(&mut self, start: R, old_len: R, new_len: R) {
        let (s_i, s_rec) = self.search(start.bytes(), Record::bytes);
        let (e_i, e_rec) = self.search(start.bytes() + old_len.bytes(), Record::bytes);
        let e_len = self.stored.get(e_i as usize).cloned().unwrap_or_default();

        if s_i < e_i {
            let start = s_i as usize + 1;
            let end = (e_i as usize).min(self.stored.len() - 1);
            self.stored.splice(start..=end, []);
        }

        // Transformation of the beginning len.
        self.last = if let Some(len) = self.stored.get_mut(s_i as usize) {
            *len = new_len.add(e_rec.add(e_len).sub(old_len)).sub(s_rec);
            let len = *len;

            // Removing if new_len has size 0 (no tags or skips).
            // If there are no tags or skips, no skip will start
            // exactly on this record, making it invalid.
            if let Some(prev_i) = s_i.checked_sub(1)
                && start.bytes() == s_rec.bytes()
                && (new_len.is_zero_len() || len.is_zero_len())
            {
                let prev_len = self.stored.get_mut(prev_i as usize).unwrap();
                *prev_len = prev_len.add(len);
                self.stored.remove(prev_i as usize + 1);

                (s_i, s_rec.add(len))
            } else {
                (s_i + 1, s_rec.add(len))
            }
        } else {
            let last = self.stored.last_mut().unwrap();
            *last = last.add(new_len).sub(old_len);

            (s_i, s_rec.add(new_len).sub(old_len))
        };

        self.max = self.max.add(new_len).sub(old_len);
    }

    /// Transforms the end of the [`Records`]
    pub fn append(&mut self, r: R) {
        self.transform(self.max, R::default(), r)
    }

    /// Extends this [`Records`] with another
    pub fn extend(&mut self, mut other: Records<R>) {
        let self_e_len = self.stored.last().unwrap();
        let other_s_len = other.stored.first_mut().unwrap();

        if self_e_len.bytes() + other_s_len.bytes() < LEN_PER_RECORD {
            self.last = (self.stored.len() as u32, self.max.add(*other_s_len));
            *other_s_len = self_e_len.add(*other_s_len);
            self.stored.pop();
        }

        self.stored.extend(other.stored);
        self.max = self.max.add(other.max);
    }

    /// Clears all [`Record`]s and makes the len 0
    pub fn clear(&mut self) {
        self.last = (0, R::default());
        self.max = R::default();
        self.stored = vec![R::default()];
    }

    /// The maximum [`Record`]
    pub fn max(&self) -> R {
        self.max
    }

    /// Returns the [`Record`] closest to the byte
    pub fn closest_to(&self, b: u32) -> R {
        let (i, rec) = self.search(b, Record::bytes);
        let len = self.stored.get(i as usize).cloned().unwrap_or(R::default());

        if rec.bytes().abs_diff(b) > len.add(rec).bytes().abs_diff(b) {
            len.add(rec)
        } else {
            rec
        }
    }

    /// The [`Record`] closest to `at` by a key extracting function
    pub fn closest_to_by_key(&self, at: u32, key_f: impl Fn(&R) -> u32 + Copy) -> R {
        let (i, rec) = self.search(at, key_f);
        let len = self.stored.get(i as usize).cloned().unwrap_or(R::default());

        if key_f(&rec).abs_diff(at) > key_f(&len.add(rec)).abs_diff(at) {
            len.add(rec)
        } else {
            rec
        }
    }

    /// Search for `at` with a key extracting function
    fn search(&self, at: u32, key_f: impl Fn(&R) -> u32 + Copy) -> (u32, R) {
        let (n, mut rec) = self.last;
        let n = n as usize;

        if at >= key_f(&rec) {
            self.stored[n..].iter().enumerate().find_map(|(i, len)| {
                rec = rec.add(*len);
                (key_f(&rec) > at).then_some(((n + i) as u32, rec.sub(*len)))
            })
        } else {
            self.stored[..n]
                .iter()
                .enumerate()
                .rev()
                .find_map(|(i, len)| {
                    rec = rec.sub(*len);
                    (key_f(&rec) <= at).then_some((i as u32, rec))
                })
        }
        .unwrap_or((self.stored.len() as u32, self.max))
    }
}

impl<R: Record> Default for Records<R> {
    fn default() -> Self {
        Self {
            last: (0, R::default()),
            max: R::default(),
            stored: vec![R::default()],
        }
    }
}

impl<R: Record> Debug for Records<R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Records")
            .field("last", &format!("{:?}", self.last))
            .field("max", &format!("{:?}", self.max))
            .field("stored", &format!("{:?}", self.stored))
            .finish()
    }
}
