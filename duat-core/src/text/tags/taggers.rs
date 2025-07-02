use std::{
    ops::Range,
    sync::{
        LazyLock,
        atomic::{AtomicU16, Ordering},
    },
};

use super::sh;
use crate::binary_search_by_key_and_index;

/// The prevalence of a [`Tagger`] in a [`Text`]
///
/// [`Text`]: crate::text::Text
#[derive(Clone, Default, Debug)]
pub struct TaggerExtents(Vec<(Tagger, Extent)>);

impl TaggerExtents {
    /// Inserts a new [`RawTag`]'s byte
    ///
    /// [`RawTag`]: super::RawTag
    pub fn insert(&mut self, tagger: Tagger, byte: usize) {
        if tagger == Tagger::basic() {
            return;
        }

        if let Some((_, extent)) = self.0.iter_mut().find(|(t, _)| *t == tagger) {
            if tagger == Tagger(4)
                && let Extent::Sparse { shift_state, list } = extent
                && *shift_state != (0, 0)
                && byte >= 1342
            {
                crate::context::error!("before: {shift_state:?}, {}", list.len());
                crate::context::error!("before: {:?}", list.get(shift_state.0));
            }
            extent.insert(byte);
        } else {
            self.0.push((tagger, Extent::Sparse {
                list: vec![byte],
                shift_state: (0, 0),
            }));
        }
    }

    /// Which ranges should be checked, given a removal of this
    /// [`Range`]
    pub fn remove_range(
        &mut self,
        range: Range<usize>,
        buf_len: usize,
        filter: impl Fn(Tagger) -> bool,
    ) -> Vec<Range<usize>> {
        const MAX_FOR_JOINING: usize = 32;

        let mut ranges = Vec::new();
        let mut rampant_matches = false;

        for (tagger, extent) in self.0.iter_mut() {
            if !filter(*tagger) {
                continue;
            }

            if *tagger == Tagger(4)
                && let Extent::Sparse { shift_state, .. } = extent
                && range.start == 1342
            {
                crate::context::info!("from: {range:#?}");
                crate::context::warn!(
                    "shift_from: {}, total_diff: {}",
                    shift_state.0,
                    shift_state.1
                );
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

            if *tagger == Tagger(4) && range.start == 1342 {
                crate::context::error!("iterated over {bytes_list:?}");
                crate::context::debug!("ranges: {ranges:?}");
            }
        }

        self.0.retain(|(tagger, extent)| {
            !extent.is_empty() || (!filter(*tagger) && range.start == 0 && range.end == buf_len)
        });

        ranges
    }

    pub fn shift_by(&mut self, from: usize, diff: i32) {
        for (_, extent) in self.0.iter_mut() {
            extent.shift_by(from, diff);
        }
    }
}

/// The level of prevalence of a [`Tagger`] in the [`Tags`]
///
/// [`Tags`]: super::Tags
#[derive(Clone)]
enum Extent {
    Sparse {
        list: Vec<usize>,
        shift_state: (usize, i32),
    },
    Rampant,
}

impl std::fmt::Debug for Extent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sparse { list, shift_state } => f
                .debug_struct("Sparse")
                .field("list", list)
                .field("len", &list.len())
                .field("shift_state", shift_state)
                .finish(),
            Self::Rampant => write!(f, "Rampant"),
        }
    }
}

impl Extent {
    /// Shifts the indices within
    fn shift_by(&mut self, from: usize, diff: i32) {
        let Extent::Sparse { list, shift_state } = self else {
            return;
        };

        let (mut shift_from, mut total_diff) = std::mem::take(shift_state);

        if let Some(first_unshifted) = list.get(shift_from) {
            crate::context::debug!("shifting backwards");
            if from <= sh(*first_unshifted, total_diff) {
                let mut iter = list[..shift_from].iter_mut().rev();
                while let Some(b) = iter.next()
                    && *b > from
                {
                    crate::context::info!("shifted {} to {}", *b, sh(*b, diff));
                    *b = sh(*b, diff);
                }
            } else {
                crate::context::debug!("shifting forwards");
                let mut iter = list[shift_from..].iter_mut();
                // All bounds from cur_n have not been shifted, so we account for
                // that.
                while let Some(b) = iter.next()
                    && sh(*b, total_diff) <= from
                {
                    crate::context::info!("shifted {} to {}", *b, sh(*b, total_diff));
                    *b = sh(*b, total_diff);
                    shift_from += 1;
                }
            }

            total_diff += diff;
        }

        if shift_from < list.len() {
            *shift_state = (shift_from, total_diff);
        }
    }

    /// Inserts a [`RawTag`] byte
    ///
    /// [`RawTag`]: super::RawTag
    fn insert(&mut self, byte: usize) {
        let Extent::Sparse { list, shift_state } = self else {
            return;
        };
        let (mut shift_from, total_diff) = std::mem::take(shift_state);

        let key = |i: usize, byte: &usize| sh(*byte, if i < shift_from { 0 } else { total_diff });

        if let Err(i) = binary_search_by_key_and_index(list, list.len(), byte, key) {
            const TRACKED_BYTES_LIMIT: usize = 512;
            if list.len() == TRACKED_BYTES_LIMIT {
                *self = Extent::Rampant;
                return;
            }

            if i < shift_from {
                shift_from += 1;
            } else {
                if total_diff != 0 {
                    for byte in list[shift_from..i].iter_mut() {
                        *byte = sh(*byte, total_diff)
                    }
                }
                shift_from = i + 1;
            }

            list.insert(i, byte);
        };

        if shift_from < list.len() {
            *shift_state = (shift_from, total_diff);
        }
    }

    /// Removes the [`RawTag`] indices from a [`Range`]
    ///
    /// [`RawTag`]: super::RawTag
    fn remove_from_range(&mut self, range: Range<usize>) -> Option<impl Iterator<Item = usize>> {
        let Extent::Sparse { list, shift_state } = self else {
            return None;
        };

        let (shift_from, total_diff) = shift_state;

        let mut i = 0;
        Some(list.extract_if(.., move |byte| {
            if range.start == 1342 && *byte >= 1342 && *byte < 1500 {
                crate::context::warn!("for {}: i = {i}, shift_from = {}", *byte, *shift_from);
            }
            let diff = if i < *shift_from { 0 } else { *total_diff };
            if range.contains(&sh(*byte, diff)) {
                *shift_from -= (i < *shift_from) as usize;
                *byte = sh(*byte, diff);

                true
            } else {
                i += 1;
                false
            }
        }))
    }

    /// Wether this [`Tagger`]'s extent is presumed to be empty
    fn is_empty(&self) -> bool {
        match self {
            Extent::Sparse { list, .. } => list.is_empty(),
            Extent::Rampant => false,
        }
    }
}

static TAGGER_COUNT: AtomicU16 = AtomicU16::new(4);

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
pub struct Tagger(u16);

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
    pub fn new_many(amount: usize) -> Range<Self> {
        let start = Self(TAGGER_COUNT.fetch_add(amount as u16, Ordering::Relaxed));
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
        Some(Self(start.0 + count as u16))
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        start.0.checked_sub(count as u16).map(Self)
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
