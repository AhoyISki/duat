use std::{self, ops::Range};

use crate::{
    Ns,
    text::shift_list::{ShiftList, Shiftable},
};

/// The prevalence of a [`Ns`] in the [`Text`]
///
/// [`Text`]: crate::text::Text
#[derive(Clone, Debug)]
pub struct NsExtents {
    extents: Vec<(Ns, Extent)>,
    max: u32,
}

impl NsExtents {
    /// Returns a new instance of [`NsExtents`]
    pub fn new(max: usize) -> Self {
        Self { extents: Vec::new(), max: max as u32 }
    }

    /// Inserts a new [`RawTag`]'s byte
    ///
    /// [`RawTag`]: super::RawTag
    pub fn insert(&mut self, ns: Ns, byte: usize) {
        if ns == Ns::basic() {
            return;
        }

        if let Some((_, extent)) = self.extents.iter_mut().find(|(t, _)| *t == ns) {
            extent.insert(byte);
        } else {
            self.extents.push((
                ns,
                Extent::Sparse({
                    let mut list = ShiftList::new(self.max as i32);
                    list.insert(0, byte as i32);
                    list
                }),
            ));
        }
    }

    /// Extends this [`NsExtents`] with another
    pub fn extend(&mut self, other: NsExtents) {
        for (ns, extent) in other.extents {
            let is_ns = |(t, _): &&mut (Ns, _)| *t == ns;
            match extent {
                Extent::Sparse(mut new_list) => match self.extents.iter_mut().find(is_ns) {
                    Some((_, Extent::Sparse(list))) => {
                        list.extend(new_list);
                    }
                    Some((_, Extent::Rampant)) => {}
                    None => {
                        new_list.shift_by(0, self.max as i32);
                        self.extents.push((ns, Extent::Sparse(new_list)));
                    }
                },
                Extent::Rampant => match self.extents.iter_mut().find(is_ns) {
                    Some((_, extent)) => *extent = Extent::Rampant,
                    None => self.extents.push((ns, Extent::Rampant)),
                },
            }
        }

        self.max += other.max;
    }

    /// Which ranges should be checked, given a removal of this
    /// [`Range`]
    pub fn remove(
        &mut self,
        range: Range<usize>,
        filter: impl Fn(Ns) -> bool,
    ) -> Vec<Range<usize>> {
        const MAX_FOR_JOINING: usize = 32;

        if filter(Ns::basic()) {
            return vec![range];
        }

        let mut ranges = Vec::new();
        let mut rampant_matches = false;

        for (ns, extent) in self.extents.iter_mut() {
            if !filter(*ns) {
                continue;
            }

            let Some(bytes) = extent.remove_from_range(range.clone()) else {
                if !rampant_matches {
                    ranges = vec![range.clone()];
                }
                rampant_matches = true;
                continue;
            };

            if rampant_matches {
                bytes.for_each(|_| {});
                continue;
            }

            for byte in bytes {
                let i = ranges.iter_mut().take_while(|r| byte + 1 > r.end).count();

                if let Some(range) = ranges.get_mut(i)
                    && byte + MAX_FOR_JOINING >= range.start
                {
                    range.start = range.start.min(byte);
                } else if let Some(prev_i) = i.checked_sub(1)
                    && let Some(range) = ranges.get_mut(prev_i)
                    && byte <= range.end + MAX_FOR_JOINING
                {
                    range.end = range.end.max(byte + 1);
                } else {
                    ranges.insert(i, byte..byte + 1);
                }
            }
        }

        self.extents.retain(|(ns, extent)| match extent {
            Extent::Sparse(_) => true,
            Extent::Rampant => {
                !(filter(*ns) && range.start == 0 && range.end as u32 == self.max + 1)
            }
        });

        ranges
    }

    /// Which ranges should be checked, given a removal of this
    /// [`Range`]
    ///
    /// This function should be used when you don't know if the
    /// `RawTag`s in question will actually be removed, so you can't
    /// remove their respective extents.
    pub fn iter_over(&self, range: Range<usize>, ns: Ns) -> Vec<Range<usize>> {
        const MAX_FOR_JOINING: usize = 32;

        if ns == Ns::basic() {
            return vec![range];
        }

        let Some((_, extent)) = self.extents.iter().find(|(other, _)| *other == ns) else {
            return Vec::new();
        };

        let Some(bytes) = extent.iter_over(range.clone()) else {
            return vec![range.clone()];
        };

        let mut ranges: Vec<Range<usize>> = Vec::new();

        for byte in bytes {
            let i = ranges.iter_mut().take_while(|r| byte + 1 > r.end).count();

            if let Some(range) = ranges.get_mut(i)
                && byte + MAX_FOR_JOINING >= range.start
            {
                range.start = range.start.min(byte);
            } else if let Some(prev_i) = i.checked_sub(1)
                && let Some(range) = ranges.get_mut(prev_i)
                && byte <= range.end + MAX_FOR_JOINING
            {
                range.end = range.end.max(byte + 1);
            } else {
                ranges.insert(i, byte..byte + 1);
            }
        }

        ranges
    }

    /// Shifts the `NsExtents` by a given character difference
    pub fn shift_by(&mut self, s: usize, by: i32) {
        self.max = (self.max as i32 + by) as u32;
        for (_, extent) in self.extents.iter_mut() {
            extent.shift_by(s, by);
        }
    }
}

/// The level of prevalence of a [`Ns`] in the [`InnerTags`]
///
/// [`InnerTags`]: super::InnerTags
#[derive(Clone, Debug)]
enum Extent {
    Sparse(ShiftList<i32>),
    Rampant,
}

impl Extent {
    /// Shifts the indices within
    fn shift_by(&mut self, s: usize, by: i32) {
        if let Extent::Sparse(list) = self {
            let (Ok(i) | Err(i)) = list.find_by_key(s as i32, |s| s);
            list.shift_by(i, by);
        }
    }

    /// Inserts a [`RawTag`] byte
    ///
    /// [`RawTag`]: super::RawTag
    fn insert(&mut self, s: usize) {
        const MAX_FOR_SPARSE: usize = 1024;

        if let Self::Sparse(list) = self
            && let Err(i) = list.find_by_key(s as i32, |s| s)
        {
            if list.len() < MAX_FOR_SPARSE {
                list.insert(i, s as i32);
            } else {
                *self = Extent::Rampant;
            }
        }
    }

    /// Removes the [`RawTag`] indices from a [`Range`]
    ///
    /// [`RawTag`]: super::RawTag
    fn remove_from_range(&mut self, range: Range<usize>) -> Option<impl Iterator<Item = usize>> {
        let Extent::Sparse(list) = self else {
            return None;
        };

        let (Ok(s_i) | Err(s_i)) = list.find_by_key(range.start as i32, |s| s);
        let (Ok(e_i) | Err(e_i)) = list.find_by_key(range.end as i32, |s| s);

        Some(
            list.extract_if_while(s_i..e_i, |_, _| Some(true))
                .map(|(_, s)| s as usize),
        )
    }

    /// Iterates over the [`RawTag`] indices in a [`Range`]
    ///
    /// [`RawTag`]: super::RawTag
    fn iter_over(&self, range: Range<usize>) -> Option<impl Iterator<Item = usize>> {
        let Extent::Sparse(list) = self else {
            return None;
        };

        let (Ok(s_i) | Err(s_i)) = list.find_by_key(range.start as i32, |s| s);
        let (Ok(e_i) | Err(e_i)) = list.find_by_key(range.end as i32, |s| s);

        Some(list.iter_fwd(s_i..e_i).map(|(_, s)| s as usize))
    }
}

impl Shiftable for i32 {
    type Shift = i32;

    fn shift(self, by: Self::Shift) -> Self {
        self + by
    }
}
