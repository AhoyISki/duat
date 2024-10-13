use std::{
    ops::Range,
    sync::atomic::{AtomicU16, Ordering},
};

static TEXT_COUNT: AtomicU16 = AtomicU16::new(0);
static TOGGLE_COUNT: AtomicU16 = AtomicU16::new(0);
static MARKER_COUNT: AtomicU16 = AtomicU16::new(2);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TextId(u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ToggleId(u16);

impl TextId {
    pub fn new() -> Self {
        Self(TEXT_COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

impl Default for TextId {
    fn default() -> Self {
        Self::new()
    }
}

impl ToggleId {
    pub fn new() -> Self {
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
/// With keys, you can use the methods [`Text::insert_tag`] and
/// [`Text::remove_on`] to add and remove tags to a text.
///
/// The reason why keys exist is mainly for the sake of [`File`]
/// widgets. In files, it is very expected that there will be many
/// different sources of modifiers, which can add and remove tags on
/// their own accord. In order to not have these different actors
/// interfere with eachother's work, keys were implemented:
///
/// ```rust
/// use duat_core::{forms, text::{text, Key, Tag};
/// let mut text = text!("This is text with no tags in it");
/// // This key will be used to modify text.
/// let key1 = Key::new();
///
/// let id = forms::to_id!("Invisible");
///
/// text.insert_tag(17, Tag::PushForm(id), key1);
/// text.insert_tag(20, Tag::PopForm(id), key1);
///
/// assert_eq!(
///     text,
///     text!("This is text with " [Invisible] "no " [] "tags in it")
/// );
///
/// // key2 != key, so it shouldn't be able to change what key1 has done.
/// let key2 = Key::new();
///
/// text.remove_tags_on(17, key2);
///
/// assert_eq!(
///     text,
///     text!("This is text with " [Invisible] "no " [] "tags in it")
/// );
/// ```
///
/// [`Tag`]: super::Tag
/// [`Text`]: super::Text
/// [`File`]: crate::widgets::File
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Key(u16);

impl Key {
    /// Returns a new, unique [`Key`]
    pub fn new() -> Self {
        Self(MARKER_COUNT.fetch_add(1, Ordering::Relaxed))
    }

    /// Returns a number of new, unique [`Key`]s
    ///
    /// You may want to do this if you expect to be placing and
    /// removing a lot of tags, and you want the finest possible
    /// control over what gets added and deleted from the [`Text`].
    ///
    /// [`Text`]: super::Text
    pub fn new_many(amount: usize) -> Range<Self> {
        let start = Self(MARKER_COUNT.fetch_add(1, Ordering::Relaxed));
        let end = Self(MARKER_COUNT.fetch_add(amount as u16, Ordering::Relaxed));

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
    fn steps_between(start: &Self, end: &Self) -> Option<usize> {
        (end.0 as usize).checked_sub(start.0 as usize)
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        Some(Self(start.0 + count as u16))
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        start.0.checked_sub(count as u16).map(Self)
    }
}

pub trait Keys: Clone + PartialEq + Eq {
    fn range(self) -> Range<Key>;

    fn contains(self, key: Key) -> bool {
        let range = self.range();
        key >= range.start && range.end > key
    }
}

impl Keys for Key {
    fn range(self) -> Range<Key> {
        Key(self.0)..Key(self.0 + 1)
    }
}

impl Keys for Range<Key> {
    fn range(self) -> Range<Key> {
        self
    }
}
