use std::{collections::HashMap, rc::Rc};

use crossterm::event::MouseEventKind;

use super::{
    Key,
    ids::{TextId, ToggleId},
};
use crate::{
    forms::FormId,
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

    /// Starts aligning to the left, should happen at the beginning of
    /// the next line, if in the middle of a line.
    StartAlignLeft,
    /// Ends alignment to the left, returning to the usual alignment
    /// (by default, left).
    EndAlignLeft,
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
    ToggleStart(Toggle),
    ToggleEnd(ToggleId),
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
            Self::StartAlignLeft => (RawTag::StartAlignLeft(key), None),
            Self::EndAlignLeft => (RawTag::EndAlignLeft(key), None),
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
            Self::ToggleStart(toggle) => {
                let id = ToggleId::new();
                toggles.insert(id, toggle);
                (RawTag::ToggleStart(key, id), Some(id))
            }
            Self::ToggleEnd(id) => (RawTag::ToggleEnd(key, id), None),
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

    /// Starts aligning to the left, should happen at the beginning of
    /// the next line, if in the middle of a line.
    StartAlignLeft(Key),
    /// Ends alignment to the left, returning to the usual alignment
    /// (by default, left).
    EndAlignLeft(Key),
    /// Starts aligning to the center, should happen at the beginning
    /// of the next line, if in the middle of a line.
    StartAlignCenter(Key),
    /// Ends alignment to the center, returning to the usual alignment
    /// (by default, left).
    EndAlignCenter(Key),
    /// Starts aligning to the right, should happen at the beginning
    /// of the next line, if in the middle of a line.
    StartAlignRight(Key),
    /// Ends alignment to the right, returning to the usual alignment
    /// (by default, left).
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
            Self::StartAlignLeft(key) => Some(Self::EndAlignLeft(*key)),
            Self::EndAlignLeft(key) => Some(Self::StartAlignLeft(*key)),
            Self::StartAlignCenter(key) => Some(Self::EndAlignCenter(*key)),
            Self::EndAlignCenter(key) => Some(Self::StartAlignCenter(*key)),
            Self::StartAlignRight(key) => Some(Self::EndAlignRight(*key)),
            Self::EndAlignRight(key) => Some(Self::StartAlignRight(*key)),
            _ => None,
        }
    }

    pub fn ends_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::PushForm(lhs_m, lhs_id), Self::PopForm(rhs_m, rhs_id)) => {
                lhs_m == rhs_m && lhs_id == rhs_id
            }
            (Self::ToggleStart(lhs_m, lhs_id), Self::ToggleEnd(rhs_m, rhs_id)) => {
                lhs_m == rhs_m && lhs_id == rhs_id
            }
            (Self::StartAlignLeft(lhs), Self::EndAlignLeft(rhs))
            | (Self::StartAlignCenter(lhs), Self::EndAlignCenter(rhs))
            | (Self::StartAlignRight(lhs), Self::EndAlignRight(rhs))
            | (Self::StartConceal(rhs), Self::EndConceal(lhs)) => lhs == rhs,
            _ => false,
        }
    }

    pub fn is_start(&self) -> bool {
        matches!(
            self,
            Self::PushForm(..)
                | Self::StartAlignLeft(_)
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
                | Self::EndAlignLeft(_)
                | Self::EndAlignCenter(_)
                | Self::EndAlignRight(_)
                | Self::ToggleEnd(..)
                | Self::EndConceal(_)
        )
    }

    pub fn is_start_align(&self) -> bool {
        matches!(
            self,
            Self::StartAlignLeft(_) | Self::StartAlignCenter(_) | Self::StartAlignRight(_)
        )
    }

    pub fn is_end_align(&self) -> bool {
        matches!(
            self,
            Self::EndAlignLeft(_) | Self::EndAlignCenter(_) | Self::EndAlignRight(_)
        )
    }

    pub(in crate::text) fn key(&self) -> Key {
        match self {
            Self::PushForm(key, _)
            | Self::PopForm(key, _)
            | Self::MainCursor(key)
            | Self::ExtraCursor(key)
            | Self::StartAlignLeft(key)
            | Self::EndAlignLeft(key)
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
            RawTag::PushForm(id, key) => write!(f, "PushForm({id:?}, {key:?})"),
            RawTag::PopForm(id, key) => write!(f, "PopForm({id:?}, {key:?})"),
            RawTag::MainCursor(key) => write!(f, "MainCursor({key:?})"),
            RawTag::ExtraCursor(key) => write!(f, "ExtraCursor({key:?})"),
            RawTag::StartAlignLeft(key) => write!(f, "StartAlignLeft({key:?})"),
            RawTag::EndAlignLeft(key) => write!(f, "EndAlignLeft({key:?})"),
            RawTag::StartAlignCenter(key) => write!(f, "StartAlignCenter({key:?})"),
            RawTag::EndAlignCenter(key) => write!(f, "EndAlignCenter({key:?})"),
            RawTag::StartAlignRight(key) => write!(f, "StartAlignRight({key:?})"),
            RawTag::EndAlignRight(key) => write!(f, "EndAlignRight({key:?})"),
            RawTag::StartConceal(key) => write!(f, "StartConceal({key:?})"),
            RawTag::EndConceal(key) => write!(f, "EndConceal({key:?})"),
            RawTag::ConcealUntil(key) => write!(f, "ConcealUntil({key:?})"),
            RawTag::GhostText(id, key) => write!(f, "GhostText({id:?}, {key:?})"),
            RawTag::ToggleStart(id, key) => write!(f, "ToggleStart({id:?}, {key:?})"),
            RawTag::ToggleEnd(id, key) => write!(f, "ToggleEnd({id:?}, {key:?})"),
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
            (RawTag::StartAlignLeft(_), RawTag::StartAlignLeft(_)) => true,
            (RawTag::EndAlignLeft(_), RawTag::EndAlignLeft(_)) => true,
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
