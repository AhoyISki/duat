//! Types for convenience and efficiency
//!
//! There are two "types" of tag: [`Tag`]s and [`RawTag`]s. [`Tag`]s
//! are what is show to the end user, being convenient in the way they
//! include extra information, like a whole function in the case of
//! [`Tag::StartToggle`]. [`RawTag`]s, on the other hand, are meant to
//! be as small as possible in order not to waste memory, as they will
//! be stored in the [`Text`]. As such, they have as little
//! information as possible, occupying only 8 bytes.
use std::{collections::HashMap, sync::Arc};

use crossterm::event::MouseEventKind;

use super::{
    Key,
    ids::{GhostId, ToggleId},
};
use crate::{
    form::{self, FormId},
    text::{Point, Text},
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
#[derive(Clone)]
pub enum Tag {
    // Implemented:
    /// Appends a form to the stack.
    PushForm(FormId),
    /// Removes a form from the stack. It won't always be the last
    /// one.
    PopForm(FormId),

    /// Places the main cursor.
    MainCursor,
    /// Places an extra cursor.
    ExtraCursor,

    /// Starts aligning to the center, should happen at the beginning
    /// of the next line, if in the middle of a line.
    StartAlignCenter,
    /// Ends alignment to the center, returning to the usual alignment
    /// (by default, left).
    EndAlignCenter,
    /// Starts aligning to the right, should happen at the beginning
    /// of the next line, if in the middle of a line.
    StartAlignRight,
    /// Ends alignment to the right, returning to the usual alignment
    /// (by default, left).
    EndAlignRight,
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
    Spacer,

	/// Text that shows up on screen, but "doesn't exist"
    GhostText(Text),
    /// Start concealing the [`Text`] from this point
    StartConceal,
    /// Finish [`Text`] concealment
    EndConceal,

    // Not yet implemented:
    // TODO: Make this take a Widget and Area arguments
    /// Begins a toggleable section in the file.
    StartToggle(Toggle),
    EndToggle(ToggleId),
}

impl Tag {
    pub fn ghost_text(to_text: impl Into<Text>) -> Self {
        Self::GhostText(to_text.into())
    }

    pub fn to_raw(
        self,
        key: Key,
        texts: &mut HashMap<GhostId, Text>,
        toggles: &mut HashMap<ToggleId, Toggle>,
    ) -> (RawTag, Option<ToggleId>) {
        match self {
            Self::PushForm(id) => (RawTag::PushForm(key, id), None),
            Self::PopForm(id) => (RawTag::PopForm(key, id), None),
            Self::MainCursor => (RawTag::MainCursor(key), None),
            Self::ExtraCursor => (RawTag::ExtraCursor(key), None),
            Self::StartAlignCenter => (RawTag::StartAlignCenter(key), None),
            Self::EndAlignCenter => (RawTag::EndAlignCenter(key), None),
            Self::StartAlignRight => (RawTag::StartAlignRight(key), None),
            Self::EndAlignRight => (RawTag::EndAlignRight(key), None),
            Self::Spacer => (RawTag::Spacer(key), None),
            Self::GhostText(mut text) => {
                if text.0.forced_new_line {
                    text.replace_range(text.len().byte() - 1.., "");
                    text.0.forced_new_line = false;
                }
                let id = GhostId::new();
                texts.insert(id, text);
                (RawTag::GhostText(key, id), None)
            }
            Self::StartConceal => (RawTag::StartConceal(key), None),
            Self::EndConceal => (RawTag::EndConceal(key), None),
            Self::StartToggle(toggle) => {
                let id = ToggleId::new();
                toggles.insert(id, toggle);
                (RawTag::ToggleStart(key, id), Some(id))
            }
            Self::EndToggle(id) => (RawTag::ToggleEnd(key, id), None),
        }
    }

    /// Works only on tags that are not toggles.
    pub fn ends_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::PushForm(lhs), Self::PopForm(rhs)) => lhs == rhs,
            (Self::StartAlignCenter, Self::EndAlignCenter)
            | (Self::StartAlignRight, Self::EndAlignRight)
            | (Self::StartConceal, Self::EndConceal) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, PartialOrd, Ord)]
pub enum RawTag {
    // Implemented:
    /// Appends a form to the stack.
    PushForm(Key, FormId),
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
    /// [`char`]s until the [`ConcealEnd`] tag shows up.
    ///
    /// [`Text`]: super::Text
    /// [`ConcealEnd`]: RawTag::ConcealEnd
    StartConceal(Key),
    /// Stops concealing the [`Text`], returning the iteration process
    /// back to the regular [`Text`] iterator.
    ///
    /// [`Text`]: super::Text
    /// [`ConcealEnd`]: RawTag::ConcealEnd
    EndConceal(Key),

    // TODO: Deal with the consequences of changing this from a usize.
    /// More direct skipping method, allowing for full skips without
    /// the iteration, which could be slow.
    ///
    /// This variant is not actually stored in the buffer, but is
    /// created when iterating.
    ConcealUntil(u32),

    /// Text that shows up on screen, but is ignored otherwise.
    GhostText(Key, GhostId),

    // Not Implemented:
    /// Begins a toggleable section in the text.
    ToggleStart(Key, ToggleId),
    /// Ends a toggleable section in the text.
    ToggleEnd(Key, ToggleId),
}

impl RawTag {
    pub fn inverse(&self) -> Option<Self> {
        match self {
            Self::PushForm(key, id) => Some(Self::PopForm(*key, *id)),
            Self::PopForm(key, id) => Some(Self::PushForm(*key, *id)),
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
            (Self::PushForm(_, lhs), Self::PopForm(_, rhs)) => lhs == rhs,
            (Self::ToggleStart(_, lhs), Self::ToggleEnd(_, rhs)) => lhs == rhs,
            (Self::StartAlignCenter(_), Self::EndAlignCenter(_))
            | (Self::StartAlignRight(_), Self::EndAlignRight(_))
            | (Self::StartConceal(_), Self::EndConceal(_)) => true,
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
        match self {
            Self::PushForm(key, _)
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
            | Self::GhostText(key, _)
            | Self::ToggleStart(key, _)
            | Self::ToggleEnd(key, _) => *key,
            Self::ConcealUntil(_) => unreachable!(
                "This method should only be used on stored tags, this not being one of them."
            ),
        }
    }
}

impl std::fmt::Debug for RawTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RawTag::PushForm(key, id) => write!(f, "PushForm({key:?}, {})", form::name_of(*id)),
            RawTag::PopForm(key, id) => write!(f, "PopForm({key:?}, {})", form::name_of(*id)),
            RawTag::MainCursor(key) => write!(f, "MainCursor({key:?})"),
            RawTag::ExtraCursor(key) => write!(f, "ExtraCursor({key:?})"),
            RawTag::StartAlignCenter(key) => write!(f, "StartAlignCenter({key:?})"),
            RawTag::EndAlignCenter(key) => write!(f, "EndAlignCenter({key:?})"),
            RawTag::StartAlignRight(key) => write!(f, "StartAlignRight({key:?})"),
            RawTag::EndAlignRight(key) => write!(f, "EndAlignRight({key:?})"),
            RawTag::Spacer(key) => write!(f, "Spacer({key:?})"),
            RawTag::StartConceal(key) => write!(f, "StartConceal({key:?})"),
            RawTag::EndConceal(key) => write!(f, "EndConceal({key:?})"),
            RawTag::ConcealUntil(key) => write!(f, "ConcealUntil({key:?})"),
            RawTag::GhostText(key, id) => write!(f, "GhostText({key:?}, {id:?})"),
            RawTag::ToggleStart(key, id) => write!(f, "ToggleStart({key:?}, {id:?})"),
            RawTag::ToggleEnd(key, id) => write!(f, "ToggleEnd({key:?}, {id:?})"),
        }
    }
}

impl Eq for RawTag {}

impl PartialEq for RawTag {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RawTag::PushForm(_, lhs), RawTag::PushForm(_, rhs)) => lhs == rhs,
            (RawTag::PopForm(_, lhs), RawTag::PopForm(_, rhs)) => lhs == rhs,
            (RawTag::GhostText(_, lhs), RawTag::GhostText(_, rhs)) => lhs == rhs,
            (RawTag::ToggleStart(_, lhs), RawTag::ToggleStart(_, rhs)) => lhs == rhs,
            (RawTag::ToggleEnd(_, lhs), RawTag::ToggleEnd(_, rhs)) => lhs == rhs,
            (lhs, rhs) => std::mem::discriminant(lhs) == std::mem::discriminant(rhs),
        }
    }
}

pub type Toggle = Arc<dyn Fn(Point, MouseEventKind) + 'static + Send + Sync>;
