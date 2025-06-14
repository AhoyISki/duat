//! Identification types for [`RawTag`]s
//!
//! There is an id for [`Text`]s in ghost text tags, an id for
//! buttons, and the main id of this module is the [`Tagger`], which
//! can be used to discern the origin of tags, even if the tags do the
//! same thing.
//!
//! [`RawTag`]: super::RawTag
//! [`Text`]: crate::text::Text
use std::{
    ops::Range,
    sync::atomic::{AtomicU16, AtomicUsize, Ordering},
};

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
    fn contains(self, key: Tagger) -> bool;
}

impl Taggers for Tagger {
    fn contains(self, key: Tagger) -> bool {
        self == key
    }
}

impl Taggers for Range<Tagger> {
    /// Whether this range contains a given [`Tagger`]
    fn contains(self, key: Tagger) -> bool {
        key >= self.start && self.end > key
    }
}

impl Taggers for &[Tagger] {
    fn contains(self, key: Tagger) -> bool {
        self.contains(&key)
    }
}

impl Taggers for &[Range<Tagger>] {
    fn contains(self, key: Tagger) -> bool {
        self.iter().any(|r| r.contains(&key))
    }
}

/// The id of a [ghost text]
///
/// [ghost text]: super::Ghost
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GhostId(u16);

impl std::fmt::Debug for GhostId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GhostId({})", self.0)
    }
}

/// The id of a toggleable
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ToggleId(u16);

impl std::fmt::Debug for ToggleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ToggleId({})", self.0)
    }
}

impl GhostId {
    /// Creates a new [`GhostId`]
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static TEXT_COUNT: AtomicU16 = AtomicU16::new(0);
        Self(TEXT_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

impl ToggleId {
    /// Creates a new [`ToggleId`]
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static TOGGLE_COUNT: AtomicU16 = AtomicU16::new(0);
        Self(TOGGLE_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct RangeId(usize);

impl RangeId {
    /// Creates a new [`RangeId`]
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static RANGE_COUNT: AtomicUsize = AtomicUsize::new(0);
        Self(RANGE_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}
