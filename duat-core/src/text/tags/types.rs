//! Types for convenience and efficiency
//!
//! There are two "types" of tag: [`Tag`]s and [`RawTag`]s. [`Tag`]s
//! are what is show to the end user, being convenient in the way they
//! include extra information, like a whole function in the case of
//! [`Tag::StartToggle`]. [`RawTag`]s, on the other hand, are meant to
//! be as small as possible in order not to waste memory, as they will
//! be stored in the [`Text`]. As such, they have as little
//! information as possible, occupying only 8 bytes.
use std::{collections::HashMap, ops::Range, sync::Arc};

use crossterm::event::MouseEventKind;

use self::RawTag::*;
use super::{
    Key,
    ids::{GhostId, ToggleId},
};
use crate::{
    form::FormId,
    text::{Change, Point, Text},
};

/// A [`Text`] modifier
///
/// [`Tag`]s are one of the most powerful tools of Duat, they abstract
/// a ton of things that would usually be done through various APIs
/// and simplify them to just a single one.
///
/// They can do many things, such as:
///
/// * Change the style when printing;
/// * Add cursors on screen;
/// * Change alignment of text;
/// * Insert spacers for text formatting;
/// * Place "ghost [`Text`]" which can be easily ignored by readers of
///   the [`Text`];
/// * Conceal [`Text`] arbitrarily;
/// * Add toggles for mouse events (Not yet implemented);
/// * ...Possibly more to come!
///
/// # Internals
///
/// While externally, it may seem like some [`Tag`]s occupy more than
/// one space (like [`Form`], which needs a start and an end), this is
/// an illusion. Internally, those are separated into multiple
/// [`RawTag`]s, which are separated by skips, so for example, a
/// [`Form`] would be separated into a [`RawTag::PushForm`] and a
/// [`RawTag::PopForm`].
///
/// Previously, you could have unbounded ranges from [`Tag`]s (e.g. a
/// `Tag::PushForm`), but that was removed, because keeping things
/// internally correct would significantly hurt performance.
///
/// [`Form`]: Tag::Form
#[derive(Clone)]
pub struct Tag<R: std::ops::RangeBounds<usize>, F: RawTagsFn = RawTagsFnPtr> {
    pub(super) range: R,
    pub(super) tags: F,
}

impl Tag<Range<usize>> {
    /// Places the main cursor
    pub fn main_cursor(b: usize) -> Self {
        Self::new(b..b, |key, _, _| (MainCursor(key), None, None))
    }

    /// Places an extra cursor
    pub fn extra_cursor(b: usize) -> Self {
        Self::new(b..b, |key, _, _| (ExtraCursor(key), None, None))
    }

    /// A spacer for a single screen line
    ///
    /// When printing this screen line (one row on screen, i.e. until
    /// it wraps), Instead of following the current alignment, will
    /// put spacing between this character and the previous one. The
    /// length of the space will be roughly equal to the available
    /// space on this line divided by the number of spacers on it.
    ///
    /// # Example
    ///
    /// Let's say that this is the line being printed:
    ///
    /// ```text
    /// This is my line,please,pretend it has tags
    /// ```
    ///
    /// If we were to print it with `{Divider}`s like this:
    ///
    /// ```text
    /// This is my line,{Divider}please,{Divider}pretend it has tags
    /// ```
    ///
    /// In a screen with a width of 48, it would come out like:
    ///
    /// ```text
    /// This is my line,   please,   pretend it has tags
    /// ```
    pub fn spacer(b: usize) -> Self {
        Self::new(b..b, |key, _, _| (Spacer(key), None, None))
    }

    /// Text that shows up on screen, but "doesn't exist"
    pub fn ghost(b: usize, text: impl Into<Text>) -> Tag<Range<usize>, impl RawTagsFn> {
        let mut text: Text = text.into();
        if text.0.forced_new_line {
            text.apply_change_inner(0, Change::remove_nl(text.last_point().unwrap()));
        }

        Self::new(b..b, move |key, ghosts, _| {
            let id = GhostId::new();
            ghosts.insert(id, text);
            (Ghost(key, id), None, None)
        })
    }
}

impl<R: std::ops::RangeBounds<usize>> Tag<R> {
    /// Returns a new instance of [`Tag`]
    fn new<F: RawTagsFn>(range: R, tags: F) -> Tag<R, F> {
        Tag { range, tags }
    }

    /// Stylizes a [range] with a [`Form`]
    ///
    /// The last argument is a priority factor, a higher priority
    /// makes a [`Form`]'s application latent, so it takes precedence
    /// of earlier [`Form`]s. This is important if, for example,
    /// multiple [`Form`]s are inserted in the same byte.
    ///
    /// Priority values higher than `250` are clamped back down to
    /// `250`.
    ///
    /// [range]: Range
    /// [`Form`]: crate::form::Form
    pub fn form(range: R, form: FormId, priority: u8) -> Tag<R, impl RawTagsFn> {
        Self::new(range, move |key, _, _| {
            (
                PushForm(key, form, priority),
                Some(PopForm(key, form)),
                None,
            )
        })
    }

    /// Aligns text to the center of the screen within the lines
    /// containing the [range]. If said region intersects with an
    /// [`AlignRight`], the latest one prevails.
    ///
    /// [range]: Range
    /// [`AlignRight`]: Tag::AlignRight
    pub fn align_center(range: R) -> Tag<R> {
        Self::new(range, |key, _, _| {
            (StartAlignCenter(key), Some(EndAlignCenter(key)), None)
        })
    }

    /// Aligns text to the right of the screen within the lines
    /// containing the [range]. If said region intersects with an
    /// [`AlignCenter`], the latest one prevails.
    ///
    /// [range]: Range
    /// [`AlignCenter`]: Tag::AlignCenter
    pub fn align_right(range: R) -> Tag<R> {
        Self::new(range, |key, _, _| {
            (StartAlignRight(key), Some(EndAlignRight(key)), None)
        })
    }

    /// Conceal the given [range], preventing it from being printed
    ///
    /// This [range] is completely arbitrary, it can go over multiple
    /// lines, [`Ghost`]s, whatever.
    ///
    /// [range]: Range
    /// [`Ghost`]: Tag::Ghost
    pub fn conceal(range: R) -> Tag<R> {
        Self::new(range, |key, _, _| {
            (StartConceal(key), Some(EndConceal(key)), None)
        })
    }
}

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
pub trait RawTagsFn = FnOnce(
        Key,
        &mut HashMap<GhostId, Text>,
        &mut HashMap<ToggleId, Toggle>,
    ) -> (RawTag, Option<RawTag>, Option<ToggleId>)
    + Clone;

type RawTagsFnPtr = fn(
    Key,
    &mut HashMap<GhostId, Text>,
    &mut HashMap<ToggleId, Toggle>,
) -> (RawTag, Option<RawTag>, Option<ToggleId>);
