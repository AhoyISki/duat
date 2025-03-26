//! Identification types for [`RawTag`]s
//!
//! There is an id for [`Text`]s in ghost text tags, an id for
//! buttons, and the main id of this module is the [`Key`], which can
//! be used to discern the origin of tags, even if the tags do the
//! same thing.
//!
//! [`RawTag`]: super::RawTag
//! [`Text`]: crate::text::Text
use std::{
    ops::Range,
    sync::atomic::{AtomicU16, Ordering},
};

static KEY_COUNT: AtomicU16 = AtomicU16::new(4);

/// The id of a [ghost text]
///
/// [ghost text]: super::Tag::GhostText
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GhostId(u16);

impl std::fmt::Debug for GhostId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "GhostId({})", self.0)
    }
}

/// The id of a [toggleable]
///
/// [toggleable]: super::Tag::StartToggle
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ToggleId(u16);

impl std::fmt::Debug for ToggleId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ToggleId({})", self.0)
    }
}

impl GhostId {
    /// Creates a new [`TextId`]
    pub fn new() -> Self {
        static TEXT_COUNT: AtomicU16 = AtomicU16::new(0);

        Self(TEXT_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

impl Default for GhostId {
    fn default() -> Self {
        Self::new()
    }
}

impl ToggleId {
    /// Creates a new [`ToggleId`]
    pub fn new() -> Self {
        static TOGGLE_COUNT: AtomicU16 = AtomicU16::new(0);

        Self(TOGGLE_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

impl Default for ToggleId {
    fn default() -> Self {
        Self::new()
    }
}

/// A key letting one add and remove [`Tag`]s to a [`Text`]
///
/// With keys, you can use the methods [`Text::insert_tag`] and,
/// [`Text::remove_tags`] to add and remove [`Tag`]s on the [`Text`].
///
/// The reason why keys exist is mainly for the sake of [`File`]
/// widgets. In files, it is very expected that there will be many
/// different sources of modifiers, which can add and remove tags on
/// their own accord. Keys exist so that these actors don't interfere
/// with eachother's work:
///
/// ```rust
/// use duat_core::{form, text::{text, Key, Tag}};
/// let mut text = text!("This is text with no tags in it");
/// // This key will be used to modify text.
/// let key1 = Key::new();
///
/// let id = form::id_of!("Invisible");
///
/// text.insert_tag(18, Tag::PushForm(id), key1);
/// text.insert_tag(20, Tag::PopForm(id), key1);
///
/// assert_eq!(
///     text,
///     text!("This is text with " [Invisible] "no" [] " tags in it")
/// );
///
/// // key2 != key1, so it shouldn't be able to change what was done with key1.
/// let key2 = Key::new();
/// text.remove_tags(18, key2);
///
/// assert_eq!(
///     text,
///     text!("This is text with " [Invisible] "no" [] " tags in it")
/// );
/// ```
///
/// [`Tag`]: super::Tag
/// [`Text`]: super::Text
/// [`File`]: crate::widgets::File
/// [`Text::insert_tag`]: super::Text::insert_tag
/// [`Text::remove_tags`]: super::Text::remove_tags
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Key(u16);

impl Key {
    /// Returns a new, unique [`Key`]
    pub fn new() -> Self {
        Self(KEY_COUNT.fetch_add(1, Ordering::Relaxed))
    }

    /// Returns a number of new, unique [`Key`]s
    ///
    /// You may want to do this if you expect to be placing and
    /// removing a lot of tags, and you want the finest possible
    /// control over what gets added and deleted from the [`Text`].
    ///
    /// [`Text`]: super::Text
    pub fn new_many(amount: usize) -> Range<Self> {
        let start = Self(KEY_COUNT.fetch_add(amount as u16, Ordering::Relaxed));
        let end = Self(KEY_COUNT.fetch_add(1, Ordering::Relaxed));

        start..end
    }

    /// A simple key with no uniqueness guarantee
    ///
    /// You should use this if you're editing widgets that are not the
    /// [`File`] widget, since you're probably the only one that is
    /// going to be modifying said widget anyway.
    ///
    /// The advantage of this function is speed. Since it has a
    /// `const` function, it's value is just substituted in with the
    /// code, so there is no need to store it in structs or statics.
    ///
    /// [`File`]: crate::widgets::File
    pub const fn basic() -> Self {
        Self(0)
    }

    /// A [`Key`] specifically for cursors
    pub(in crate::text) const fn for_cursors() -> Self {
        Self(1)
    }

    /// A [`Key`] specifically for remaps
    pub(crate) const fn for_alias() -> Self {
        Self(2)
    }
}

impl std::fmt::Debug for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Key({})", self.0)
    }
}

impl Default for Key {
    fn default() -> Self {
        Self::new()
    }
}

impl std::iter::Step for Key {
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
/// This can be either one [`Key`] or multiple, in which case many
/// different [`Key`]s will be searched for and [removed]
///
/// [`Tag`]: super::Tag
/// [removed]: crate::text::Text::remove_tags
pub trait Keys: Clone + PartialEq + Eq {
    /// Whether this range contains a given [`Key`]
    fn contains(self, key: Key) -> bool;
}

impl Keys for Key {
    fn contains(self, key: Key) -> bool {
        self == key
    }
}

impl Keys for Range<Key> {
    /// Whether this range contains a given [`Key`]
    fn contains(self, key: Key) -> bool {
        key >= self.start && self.end > key
    }
}

impl Keys for &[Key] {
    fn contains(self, key: Key) -> bool {
        self.contains(&key)
    }
}

impl Keys for &[Range<Key>] {
    fn contains(self, key: Key) -> bool {
        self.iter().any(|r| r.contains(&key))
    }
}
