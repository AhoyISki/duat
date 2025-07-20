use std::{
    ops::Range,
    sync::{
        LazyLock,
        atomic::{AtomicU32, Ordering},
    },
};

use crate::text::shift_list::{ShiftList, Shiftable};

/// The prevalence of a [`Tagger`] in a [`Text`]
///
/// [`Text`]: crate::text::Text
#[derive(Clone, Debug)]
pub struct TaggerExtents {
    extents: Vec<(Tagger, Extent)>,
    max: u32,
}

impl TaggerExtents {
    /// Returns a new instance of [`TaggerExtents`]
    pub fn new(max: usize) -> Self {
        Self { extents: Vec::new(), max: max as u32 }
    }

    /// Inserts a new [`RawTag`]'s byte
    ///
    /// [`RawTag`]: super::RawTag
    pub fn insert(&mut self, tagger: Tagger, byte: usize) {
        if tagger == Tagger::basic() {
            return;
        }

        if let Some((_, extent)) = self.extents.iter_mut().find(|(t, _)| *t == tagger) {
            extent.insert(byte);
        } else {
            self.extents.push((
                tagger,
                Extent::Sparse({
                    let mut list = ShiftList::new(self.max as i32);
                    list.insert(0, byte as u32);
                    list
                }),
            ));
        }
    }

    /// Extends this [`TaggerExtents`] with another
    pub fn extend(&mut self, other: TaggerExtents) {
        for (tagger, extent) in other.extents {
            let is_tagger = |(t, _): &&mut (Tagger, _)| *t == tagger;
            match extent {
                Extent::Sparse(mut new_list) => match self.extents.iter_mut().find(is_tagger) {
                    Some((_, Extent::Sparse(list))) => {
                        list.extend(new_list);
                    }
                    Some((_, Extent::Rampant)) => {}
                    None => {
                        new_list.shift_by(0, self.max as i32);
                        self.extents.push((tagger, Extent::Sparse(new_list)));
                    }
                },
                Extent::Rampant => match self.extents.iter_mut().find(is_tagger) {
                    Some((_, extent)) => *extent = Extent::Rampant,
                    None => self.extents.push((tagger, Extent::Rampant)),
                },
            }
        }

        self.max += other.max;
    }

    /// Which ranges should be checked, given a removal of this
    /// [`Range`]
    pub fn remove_range(
        &mut self,
        range: Range<usize>,
        filter: impl Fn(Tagger) -> bool,
    ) -> Vec<Range<usize>> {
        const MAX_FOR_JOINING: usize = 32;

        let mut ranges = Vec::new();
        let mut rampant_matches = false;

        for (tagger, extent) in self.extents.iter_mut() {
            if !filter(*tagger) {
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

            let mut bytes_list = Vec::new();
            for byte in bytes {
                bytes_list.push(byte);

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

        self.extents.retain(|(tagger, extent)| match extent {
            Extent::Sparse(_) => true,
            Extent::Rampant => {
                !(filter(*tagger) && range.start == 0 && range.end as u32 == self.max)
            }
        });

        ranges
    }

    /// Shifts the [`TaggerExtents`] by a given character difference
    pub fn shift_by(&mut self, c: usize, by: i32) {
        self.max = (self.max as i32 + by) as u32;
        for (_, extent) in self.extents.iter_mut() {
            extent.shift_by(c, by);
        }
    }
}

/// The level of prevalence of a [`Tagger`] in the [`InnerTags`]
///
/// [`InnerTags`]: super::InnerTags
#[derive(Clone, Debug)]
enum Extent {
    Sparse(ShiftList<u32>),
    Rampant,
}

impl Extent {
    /// Shifts the indices within
    fn shift_by(&mut self, c: usize, by: i32) {
        if let Extent::Sparse(list) = self {
            let (Ok(i) | Err(i)) = list.find_by_key(c as u32, |c| c);
            list.shift_by(i, by);
        }
    }

    /// Inserts a [`RawTag`] byte
    ///
    /// [`RawTag`]: super::RawTag
    fn insert(&mut self, c: usize) {
        const MAX_FOR_SPARSE: usize = 1024;

        if let Self::Sparse(list) = self
            && let Err(i) = list.find_by_key(c as u32, |c| c)
        {
            if list.len() < MAX_FOR_SPARSE {
                list.insert(i, c as u32);
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

        let (Ok(s_i) | Err(s_i)) = list.find_by_key(range.start as u32, |c| c);
        let (Ok(e_i) | Err(e_i)) = list.find_by_key(range.end as u32, |c| c);

        Some(
            list.extract_if_while(s_i..e_i, |_, _| Some(true))
                .map(|(_, c)| c as usize),
        )
    }
}

impl Shiftable for u32 {
    type Shift = i32;

    fn shift(self, by: Self::Shift) -> Self {
        (self as i32 + by) as u32
    }
}

static TAGGER_COUNT: AtomicU32 = AtomicU32::new(4);

/// A struct that lets one add and remove [`Tag`]s to a [`Text`]
///
/// With keys, you can use the methods [`Text::insert_tag`] and,
/// [`Text::remove_tags`] to add and remove [`Tag`]s on the [`Text`].
///
/// The reason why keys exist is mainly for the sake of [`File`]
/// widgets. In files, it is very expected that there will be many
/// different sources of modifiers, which can add and remove tags on
/// their own accord. Taggers exist so that these actors don't
/// interfere with eachother's work:
///
/// ```rust
/// # use duat_core::prelude::*;
/// let mut text = txt!("This is text with no tags in it").build();
/// // This key will be used to modify text.
/// let key1 = Tagger::new();
///
/// let id = form::id_of!("invisible");
///
/// // You can create an `impl Tag` directly from a `FormId`
/// text.insert_tag(key1, 18..20, id.to_tag(0));
///
/// assert_eq!(
///     text,
///     txt!("This is text with [invisible]no[] tags in it").build()
/// );
///
/// // key2 != key1, so it shouldn't be able to change what was done with key1.
/// let key2 = Tagger::new();
/// text.remove_tags(key2, 18);
///
/// assert_eq!(
///     text,
///     txt!("This is text with [invisible]no[] tags in it").build()
/// );
/// ```
///
/// [`Tag`]: super::Tag
/// [`Text`]: super::Text
/// [`File`]: crate::file::File
/// [`Text::insert_tag`]: super::Text::insert_tag
/// [`Text::remove_tags`]: super::Text::remove_tags
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Tagger(pub(super) u32);

impl Tagger {
    /// Returns a new, unique [`Tagger`]
    pub fn new() -> Self {
        Self(TAGGER_COUNT.fetch_add(1, Ordering::Relaxed))
    }

    /// Returns a new [`LazyLock<Tagger>`]
    pub const fn new_static() -> LazyLock<Self> {
        LazyLock::new(Self::new)
    }

    /// Returns a number of new, unique [`Tagger`]s
    ///
    /// You may want to do this if you expect to be placing and
    /// removing a lot of tags, and you want the finest possible
    /// control over what gets added and deleted from the [`Text`].
    ///
    /// [`Text`]: super::Text
    pub fn new_many(amount: u32) -> Range<Self> {
        let start = Self(TAGGER_COUNT.fetch_add(amount, Ordering::Relaxed));
        let end = Self(TAGGER_COUNT.fetch_add(1, Ordering::Relaxed));

        start..end
    }

    /// A simple key with no uniqueness guarantee
    ///
    /// You should use this if you're editing widgets that are not the
    /// [`File`] widget, since you're probably the only one that is
    /// going to be modifying said widget anyway.
    ///
    /// The advantage of this function is speed. Since it is a
    /// `const` function, it's value is just substituted in with the
    /// code, so there is no need to store it in structs or statics.
    ///
    /// [`File`]: crate::file::File
    pub const fn basic() -> Self {
        Self(0)
    }

    /// A [`Tagger`] specifically for selections
    pub(in crate::text) const fn for_selections() -> Self {
        Self(1)
    }

    /// A [`Tagger`] specifically for remaps
    pub(crate) const fn for_alias() -> Self {
        Self(2)
    }
}

impl std::fmt::Debug for Tagger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Tagger({})", self.0)
    }
}

impl Default for Tagger {
    fn default() -> Self {
        Self::new()
    }
}

impl std::iter::Step for Tagger {
    fn steps_between(start: &Self, end: &Self) -> (usize, Option<usize>) {
        if end.0 as usize >= start.0 as usize {
            ((end.0 - start.0) as usize, Some((end.0 - start.0) as usize))
        } else {
            (0, None)
        }
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        Some(Self(start.0 + count as u32))
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        start.0.checked_sub(count as u32).map(Self)
    }
}

/// Trait used to distinguish [`Tag`]s
///
/// This can be either one [`Tagger`] or multiple, in which case many
/// different [`Tagger`]s will be searched for and [removed]
///
/// [`Tag`]: super::Tag
/// [removed]: crate::text::Text::remove_tags
pub trait Taggers: std::fmt::Debug + Clone + PartialEq + Eq {
    /// Whether this range contains a given [`Tagger`]
    fn contains_tagger(&self, tagger: Tagger) -> bool;
}

impl Taggers for Tagger {
    fn contains_tagger(&self, tagger: Tagger) -> bool {
        *self == tagger
    }
}

impl Taggers for Range<Tagger> {
    /// Whether this range contains a given [`Tagger`]
    fn contains_tagger(&self, tagger: Tagger) -> bool {
        tagger >= self.start && self.end > tagger
    }
}

impl Taggers for &[Tagger] {
    fn contains_tagger(&self, tagger: Tagger) -> bool {
        self.contains(&tagger)
    }
}

impl Taggers for &[Range<Tagger>] {
    fn contains_tagger(&self, tagger: Tagger) -> bool {
        self.iter().any(|r| r.contains(&tagger))
    }
}

impl<const N: usize> Taggers for [Tagger; N] {
    fn contains_tagger(&self, tagger: Tagger) -> bool {
        self.contains(&tagger)
    }
}
