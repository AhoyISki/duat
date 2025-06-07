//! Types for convenience and efficiency
//!
//! There are two "types" of tag: [`Tag`]s and [`RawTag`]s. [`Tag`]s
//! are what is show to the end user, being convenient in the way they
//! include extra information, like a whole function in the case of
//! [`Tag::StartToggle`]. [`RawTag`]s, on the other hand, are meant to
//! be as small as possible in order not to waste memory, as they will
//! be stored in the [`Text`]. As such, they have as little
//! information as possible, occupying only 8 bytes.
use std::{ops::RangeBounds, sync::Arc};

use RawTag::*;
use crossterm::event::MouseEventKind;

use super::{
    Key,
    ids::{GhostId, ToggleId},
};
use crate::{
    form::FormId,
    text::{Point, Text},
};

pub trait Tag<R>: Sized {
    fn decompose(
        self,
        r: R,
        max: usize,
        key: Key,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>);
}

////////// Form-like Tags

/// [`Tag`]: Applies a [`Form`] to a given [range] in the [`Text`]
///
/// This struct can be created from the [`FormId::to_tag`] method,
/// granting it a priority that is used to properly order the
/// [`RawTag`]s within.
///
/// The [`Form`] is able to intersect with other [`Form`]s, which,
/// unlike when pushing [`Form`]s to a [`Builder`], interfere
/// constructively, with the latest attributes and colors winning out.
///
/// [`Form`]: crate::form::Form
/// [range]: RangeBounds
/// [`Builder`]: crate::text::Builder
#[derive(Clone, Copy)]
pub struct FormTag(pub FormId, pub u8);

impl<R: RangeBounds<usize>> Tag<R> for FormTag {
    fn decompose(
        self,
        r: R,
        max: usize,
        key: Key,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
        let FormTag(id, prio) = self;
        ranged(r, max, PushForm(key, id, prio), PopForm(key, id), None)
    }
}

/// [`Tag`]: Places the main Cursor on the [`Text`]
///
/// You shouldn't have to use this most of the time, since the
/// [`Text`] can come equipped with [`Cursors`], which manage that
/// automatically for you.
///
/// [`Cursors`]: crate::mode::Cursors
#[derive(Clone, Copy)]
pub struct MainCursor;
simple_impl_Tag!(MainCursor, RawTag::MainCursor);

/// [`Tag`]: Places an extra Cursor on the [`Text`]
///
/// How the extra cursor gets inserted is [`Ui`] dependant, for
/// example, a terminal can't show more than one cursor, so in
/// [`duat-term`], it defaults to showing the `"ExtraCursor"`
/// [`Form`], but in other platforms, it could show an actual cursor.
///
/// You shouldn't have to use this most of the time, since the
/// [`Text`] can come equipped with [`Cursors`], which manage that
/// automatically for you.
///
/// [`Cursors`]: crate::mode::Cursors
/// [`Ui`]: crate::ui::Ui
/// [`duat-term`]: https://crates.io/crates/duat-term
/// [`Form`]: crate::form::Form
#[derive(Clone, Copy)]
pub struct ExtraCursor;
simple_impl_Tag!(ExtraCursor, RawTag::ExtraCursor);

/////////// Alignment Tags

/// [`Builder`] part: Begins centered alignment
///
/// [`Tag`]: Aligns the [`Text`] on the center in a [range]
///
/// [`Builder`]: crate::text::Builder
/// [range]: RangeBounds
#[derive(Clone, Copy)]
pub struct AlignCenter;
ranged_impl_tag!(
    AlignCenter,
    RawTag::StartAlignCenter,
    RawTag::EndAlignCenter
);

/// [`Builder`] part: Begins alignment on the right
///
/// [`Tag`]: Aligns the [`Text`] on the right in a [range]
///
/// [`Builder`]: crate::text::Builder
/// [range]: RangeBounds
#[derive(Clone, Copy)]
pub struct AlignRight;
ranged_impl_tag!(AlignRight, RawTag::StartAlignRight, RawTag::EndAlignRight);

/// [`Builder`] part: Begins alignment on the left
///
/// Note that, unlike [`AlignCenter`] and [`AlignRight`], this struct
/// is not a [`Tag`], since the left alignment is the default
/// alignment, i.e., the lack of those other alignment tags. So this
/// just serves to cancel these other alignments inside of a
/// [`Builder`].
///
/// [`Builder`]: crate::text::Builder
/// [range]: RangeBounds
#[derive(Clone, Copy)]
pub struct AlignLeft;

/// [`Builder`] part and [`Tag`]: A spacer for more advanced alignment
///
/// When printing this screen line (one row on screen, i.e. until
/// it wraps), Instead of following the current alignment, will
/// put spacing between the next and previous characters. The
/// length of the space will be roughly equal to the available
/// space on this line divided by the number of [`Spacer`]s on it.
///
/// # Example
///
/// Let's say that this is the line being printed:
///
/// ```text
/// This is my line,please,pretend it has tags
/// ```
///
/// If we were to print it with `{Spacer}as like this:
///
/// ```text
/// This is my line,{Spacer}please,{Spacer}pretend it has tags
/// ```
///
/// In a screen with a width of 50, it would come out like:
///
/// ```text
/// This is my line,    please,    pretend it has tags
/// ```
///
/// [`Builder`]: crate::text::Builder
#[derive(Clone, Copy)]
pub struct Spacer;
simple_impl_Tag!(Spacer, RawTag::Spacer);

////////// Text modification Tags

/// [`Builder`] part and [`Tag`]: Places ghost text
///
/// This is useful when, for example, creating command line prompts,
/// since the text is non interactable.
///
/// [`Builder`]: crate::text::Builder
#[derive(Clone, Copy)]
pub struct Ghost<T: Into<Text>>(pub T);

impl<T: Into<Text>> Tag<usize> for Ghost<T> {
    fn decompose(
        self,
        r: usize,
        max: usize,
        key: Key,
    ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
        assert!(
            r <= max,
            "index out of bounds: the len is {max}, but the index is {r}",
        );
        let id = GhostId::new();
        let tag_id = TagId::Ghost(id, Into::<Text>::into(self.0).without_last_nl());
        ((r, RawTag::Ghost(key, id)), None, Some(tag_id))
    }
}

/// [`Tag`]: Conceals a [range] in the [`Text`]
///
/// This [range] is completely arbitrary, being able to partially
/// contain lines, as long as it is contained within the length of the
/// [`Text`].
///
/// [range]: RangeBounds
#[derive(Clone, Copy)]
pub struct Conceal;
ranged_impl_tag!(Conceal, RawTag::StartConceal, RawTag::EndConceal);

#[derive(Clone, Copy, Eq, PartialOrd, Ord)]
pub enum RawTag {
    // Implemented:
    /// Appends a form to the stack.
    PushForm(Key, FormId, u8),
    /// Removes a form from the stack. It won't always be the last
    /// one.
    PopForm(Key, FormId),

    /// Places the main cursor.
    MainCursor(Key),
    /// Places an extra cursor.
    ExtraCursor(Key),

    /// Starts aligning to the center, should happen to the whole
    /// line, even if it shows up in the middle of it.
    StartAlignCenter(Key),
    /// Ends aligning to the center, reverting to left alignment.
    /// Should happen to the whole line, even if it shows up in the
    /// middle of it.
    EndAlignCenter(Key),
    /// Starts aligning to the right, should happen to the whole
    /// line, even if it shows up in the middle of it.
    StartAlignRight(Key),
    /// Ends aligning to the right, reverting to left alignment.
    /// Should happen to the whole line, even if it shows up in the
    /// middle of it.
    EndAlignRight(Key),
    /// A spacer for the current screen line, replaces alignment.
    Spacer(Key),

    // In the process of implementing.
    /// Starts concealing the [`Text`], skipping all [`Tag`]s and
    /// [`char`]s until the [`EndConceal`] tag shows up.
    ///
    /// [`Text`]: super::Text
    /// [`EndConceal`]: RawTag::EndConceal
    StartConceal(Key),
    /// Stops concealing the [`Text`], returning the iteration process
    /// back to the regular [`Text`] iterator.
    ///
    /// [`Text`]: super::Text
    EndConceal(Key),

    // TODO: Deal with the consequences of changing this from a usize.
    /// More direct skipping method, allowing for full skips without
    /// the iteration, which could be slow.
    ///
    /// This variant is not actually stored in the buffer, but is
    /// created when iterating.
    ConcealUntil(u32),

    /// Text that shows up on screen, but is ignored otherwise.
    Ghost(Key, GhostId),

    // Not Implemented:
    /// Begins a toggleable section in the text.
    ToggleStart(Key, ToggleId),
    /// Ends a toggleable section in the text.
    ToggleEnd(Key, ToggleId),
}

impl PartialEq for RawTag {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::PushForm(l_key, l_id, _), Self::PushForm(r_key, r_id, _)) => {
                l_key == r_key && l_id == r_id
            }
            (Self::PopForm(l_key, l_id), Self::PopForm(r_key, r_id)) => {
                l_key == r_key && l_id == r_id
            }
            (Self::MainCursor(l_key), Self::MainCursor(r_key)) => l_key == r_key,
            (Self::ExtraCursor(l_key), Self::ExtraCursor(r_key)) => l_key == r_key,
            (Self::StartAlignCenter(l_key), Self::StartAlignCenter(r_key)) => l_key == r_key,
            (Self::EndAlignCenter(l_key), Self::EndAlignCenter(r_key)) => l_key == r_key,
            (Self::StartAlignRight(l_key), Self::StartAlignRight(r_key)) => l_key == r_key,
            (Self::EndAlignRight(l_key), Self::EndAlignRight(r_key)) => l_key == r_key,
            (Self::Spacer(l_key), Self::Spacer(r_key)) => l_key == r_key,
            (Self::StartConceal(l_key), Self::StartConceal(r_key)) => l_key == r_key,
            (Self::EndConceal(l_key), Self::EndConceal(r_key)) => l_key == r_key,
            (Self::ConcealUntil(l_key), Self::ConcealUntil(r_key)) => l_key == r_key,
            (Self::Ghost(l_key, l_id), Self::Ghost(r_key, r_id)) => l_key == r_key && l_id == r_id,
            (Self::ToggleStart(l_key, l_id), Self::ToggleStart(r_key, r_id)) => {
                l_key == r_key && l_id == r_id
            }
            (Self::ToggleEnd(l_key, l_id), Self::ToggleEnd(r_key, r_id)) => {
                l_key == r_key && l_id == r_id
            }
            _ => false,
        }
    }
}

impl RawTag {
    pub fn inverse(&self) -> Option<Self> {
        match self {
            Self::PushForm(key, id, _) => Some(Self::PopForm(*key, *id)),
            Self::PopForm(key, id) => Some(Self::PushForm(*key, *id, 0)),
            Self::ToggleStart(key, id) => Some(Self::ToggleEnd(*key, *id)),
            Self::ToggleEnd(key, id) => Some(Self::ToggleStart(*key, *id)),
            Self::StartConceal(key) => Some(Self::EndConceal(*key)),
            Self::EndConceal(key) => Some(Self::StartConceal(*key)),
            Self::StartAlignCenter(key) => Some(Self::EndAlignCenter(*key)),
            Self::EndAlignCenter(key) => Some(Self::StartAlignCenter(*key)),
            Self::StartAlignRight(key) => Some(Self::EndAlignRight(*key)),
            Self::EndAlignRight(key) => Some(Self::StartAlignRight(*key)),
            _ => None,
        }
    }

    pub fn ends_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::PushForm(l_key, l_id, _), Self::PopForm(r_key, r_id)) => {
                l_id == r_id && l_key == r_key
            }
            (Self::ToggleStart(l_key, l_id), Self::ToggleEnd(r_key, r_id)) => {
                l_id == r_id && l_key == r_key
            }
            (Self::StartAlignCenter(l_key), Self::EndAlignCenter(r_key))
            | (Self::StartAlignRight(l_key), Self::EndAlignRight(r_key))
            | (Self::StartConceal(l_key), Self::EndConceal(r_key)) => l_key == r_key,
            _ => false,
        }
    }

    pub fn is_start(&self) -> bool {
        matches!(
            self,
            Self::PushForm(..)
                | Self::StartAlignCenter(_)
                | Self::StartAlignRight(_)
                | Self::ToggleStart(..)
                | Self::StartConceal(_)
        )
    }

    pub fn is_end(&self) -> bool {
        matches!(
            self,
            Self::PopForm(..)
                | Self::EndAlignCenter(_)
                | Self::EndAlignRight(_)
                | Self::ToggleEnd(..)
                | Self::EndConceal(_)
        )
    }

    pub fn is_start_align(&self) -> bool {
        matches!(self, Self::StartAlignCenter(_) | Self::StartAlignRight(_))
    }

    pub fn is_end_align(&self) -> bool {
        matches!(self, Self::EndAlignCenter(_) | Self::EndAlignRight(_))
    }

    pub(in crate::text) fn key(&self) -> Key {
        match self.get_key() {
            Some(key) => key,
            None => unreachable!(
                "This method should only be used on stored tags, this not being one of them."
            ),
        }
    }

    fn get_key(&self) -> Option<Key> {
        match self {
            Self::PushForm(key, ..)
            | Self::PopForm(key, _)
            | Self::MainCursor(key)
            | Self::ExtraCursor(key)
            | Self::StartAlignCenter(key)
            | Self::EndAlignCenter(key)
            | Self::StartAlignRight(key)
            | Self::EndAlignRight(key)
            | Self::Spacer(key)
            | Self::StartConceal(key)
            | Self::EndConceal(key)
            | Self::Ghost(key, _)
            | Self::ToggleStart(key, _)
            | Self::ToggleEnd(key, _) => Some(*key),
            Self::ConcealUntil(_) => None,
        }
    }

    pub(super) fn priority(&self) -> u8 {
        match self {
            Self::PushForm(.., priority) => *priority + 5,
            Self::PopForm(..) => 0,
            Self::MainCursor(..) => 4,
            Self::ExtraCursor(..) => 4,
            Self::StartAlignCenter(..) => 1,
            Self::EndAlignCenter(..) => 2,
            Self::StartAlignRight(..) => 1,
            Self::EndAlignRight(..) => 2,
            Self::Spacer(..) => 2,
            Self::StartConceal(..) => 1,
            Self::EndConceal(..) => 2,
            Self::Ghost(..) => 3,
            Self::ToggleStart(..) => 1,
            Self::ToggleEnd(..) => 2,
            Self::ConcealUntil(_) => unreachable!("This shouldn't be queried"),
        }
    }
}

impl std::fmt::Debug for RawTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PushForm(key, id, prio) => {
                write!(f, "PushForm({key:?}, {}, {prio})", id.name())
            }
            Self::PopForm(key, id) => write!(f, "PopForm({key:?}, {})", id.name()),
            Self::MainCursor(key) => write!(f, "MainCursor({key:?})"),
            Self::ExtraCursor(key) => write!(f, "ExtraCursor({key:?})"),
            Self::StartAlignCenter(key) => write!(f, "StartAlignCenter({key:?})"),
            Self::EndAlignCenter(key) => write!(f, "EndAlignCenter({key:?})"),
            Self::StartAlignRight(key) => write!(f, "StartAlignRight({key:?})"),
            Self::EndAlignRight(key) => write!(f, "EndAlignRight({key:?})"),
            Self::Spacer(key) => write!(f, "Spacer({key:?})"),
            Self::StartConceal(key) => write!(f, "StartConceal({key:?})"),
            Self::EndConceal(key) => write!(f, "EndConceal({key:?})"),
            Self::ConcealUntil(key) => write!(f, "ConcealUntil({key:?})"),
            Self::Ghost(key, id) => write!(f, "GhostText({key:?}, {id:?})"),
            Self::ToggleStart(key, id) => write!(f, "ToggleStart({key:?}), {id:?})"),
            Self::ToggleEnd(key, id) => write!(f, "ToggleEnd({key:?}, {id:?})"),
        }
    }
}

pub type Toggle = Arc<dyn Fn(Point, MouseEventKind) + 'static + Send + Sync>;

#[derive(Clone)]
#[allow(dead_code)]
pub enum TagId {
    Ghost(GhostId, Text),
    Toggle(ToggleId, Toggle),
}

fn ranged(
    r: impl RangeBounds<usize>,
    max: usize,
    s_tag: RawTag,
    e_tag: RawTag,
    id: Option<TagId>,
) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
    let (s, e) = crate::get_ends(r, max);
    ((s, s_tag), Some((e, e_tag)), id)
}

macro simple_impl_Tag($tag:ty, $raw_tag:expr) {
    impl Tag<usize> for $tag {
        fn decompose(
            self,
            r: usize,
            max: usize,
            key: Key,
        ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
            assert!(
                r <= max,
                "index out of bounds: the len is {max}, but the index is {r}",
            );
            ((r, $raw_tag(key)), None, None)
        }
    }
}

macro ranged_impl_tag($tag:ty, $start:expr, $end:expr) {
    impl<R: RangeBounds<usize>> Tag<R> for $tag {
        fn decompose(
            self,
            r: R,
            max: usize,
            key: Key,
        ) -> ((usize, RawTag), Option<(usize, RawTag)>, Option<TagId>) {
            let (s, e) = $crate::get_ends(r, max);
            ((s, $start(key)), Some((e, $end(key))), None)
        }
    }
}
