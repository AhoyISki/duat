//! Types for convenience and efficiency
//!
//! There are two "types" of tag: [`Tag`]s and [`RawTag`]s. [`Tag`]s
//! are what is show to the end user, being convenient in the way they
//! include extra information, like a whole function in the case of
//! [`Tag::StartToggle`]. [`RawTag`]s, on the other hand, are meant to
//! be as small as possible in order not to waste memory, as they will
//! be stored in the [`Text`]. As such, they have as little
//! information as possible, occupying only 8 bytes.
use std::{collections::HashMap, rc::Rc};

use crossterm::event::MouseEventKind;

use super::{
    Key,
    ids::{TextId, ToggleId},
};
use crate::{
    form::{self, FormId},
    text::{Point, Text},
};

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

    GhostText(Text),
    StartConceal,
    EndConceal,

    // Not yet implemented:
    /// Begins a hoverable section in the file.
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
        texts: &mut HashMap<TextId, Text>,
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
            Self::GhostText(text) => {
                let id = TextId::new();
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

unsafe impl Send for Tag {}
unsafe impl Sync for Tag {}

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

    GhostText(Key, TextId),

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
            (RawTag::MainCursor(_), RawTag::MainCursor(_)) => true,
            (RawTag::ExtraCursor(_), RawTag::ExtraCursor(_)) => true,
            (RawTag::StartAlignCenter(_), RawTag::StartAlignCenter(_)) => true,
            (RawTag::EndAlignCenter(_), RawTag::EndAlignCenter(_)) => true,
            (RawTag::StartAlignRight(_), RawTag::StartAlignRight(_)) => true,
            (RawTag::EndAlignRight(_), RawTag::EndAlignRight(_)) => true,
            (RawTag::StartConceal(_), RawTag::StartConceal(_)) => true,
            (RawTag::EndConceal(_), RawTag::EndConceal(_)) => true,
            (RawTag::ConcealUntil(_), RawTag::ConcealUntil(_)) => true,
            (RawTag::GhostText(_, lhs), RawTag::GhostText(_, rhs)) => lhs == rhs,
            (RawTag::ToggleStart(_, lhs), RawTag::ToggleStart(_, rhs)) => lhs == rhs,
            (RawTag::ToggleEnd(_, lhs), RawTag::ToggleEnd(_, rhs)) => lhs == rhs,
            (..) => false,
        }
    }
}

pub type Toggle = Rc<dyn Fn(Point, MouseEventKind)>;
