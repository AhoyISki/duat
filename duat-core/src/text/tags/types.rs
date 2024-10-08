use std::{collections::HashMap, rc::Rc};

use crossterm::event::MouseEventKind;

use super::{
    ids::{TextId, ToggleId},
    Marker,
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
        marker: Marker,
        texts: &mut HashMap<TextId, Text>,
        toggles: &mut HashMap<ToggleId, Toggle>,
    ) -> (RawTag, Option<ToggleId>) {
        match self {
            Self::PushForm(id) => (RawTag::PushForm(marker, id), None),
            Self::PopForm(id) => (RawTag::PopForm(marker, id), None),
            Self::MainCursor => (RawTag::MainCursor(marker), None),
            Self::ExtraCursor => (RawTag::ExtraCursor(marker), None),
            Self::StartAlignLeft => (RawTag::StartAlignLeft(marker), None),
            Self::EndAlignLeft => (RawTag::EndAlignLeft(marker), None),
            Self::StartAlignCenter => (RawTag::StartAlignCenter(marker), None),
            Self::EndAlignCenter => (RawTag::EndAlignCenter(marker), None),
            Self::StartAlignRight => (RawTag::StartAlignRight(marker), None),
            Self::EndAlignRight => (RawTag::EndAlignRight(marker), None),
            Self::GhostText(text) => {
                let id = TextId::new();
                texts.insert(id, text);
                (RawTag::GhostText(marker, id), None)
            }
            Self::StartConceal => (RawTag::StartConceal(marker), None),
            Self::EndConceal => (RawTag::EndConceal(marker), None),
            Self::ToggleStart(toggle) => {
                let id = ToggleId::new();
                toggles.insert(id, toggle);
                (RawTag::ToggleStart(marker, id), Some(id))
            }
            Self::ToggleEnd(id) => (RawTag::ToggleEnd(marker, id), None),
        }
    }
}

unsafe impl Send for Tag {}
unsafe impl Sync for Tag {}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RawTag {
    // Implemented:
    /// Appends a form to the stack.
    PushForm(Marker, FormId),
    /// Removes a form from the stack. It won't always be the last
    /// one.
    PopForm(Marker, FormId),

    /// Places the main cursor.
    MainCursor(Marker),
    /// Places an extra cursor.
    ExtraCursor(Marker),

    /// Starts aligning to the left, should happen at the beginning of
    /// the next line, if in the middle of a line.
    StartAlignLeft(Marker),
    /// Ends alignment to the left, returning to the usual alignment
    /// (by default, left).
    EndAlignLeft(Marker),
    /// Starts aligning to the center, should happen at the beginning
    /// of the next line, if in the middle of a line.
    StartAlignCenter(Marker),
    /// Ends alignment to the center, returning to the usual alignment
    /// (by default, left).
    EndAlignCenter(Marker),
    /// Starts aligning to the right, should happen at the beginning
    /// of the next line, if in the middle of a line.
    StartAlignRight(Marker),
    /// Ends alignment to the right, returning to the usual alignment
    /// (by default, left).
    EndAlignRight(Marker),

    // In the process of implementing.
    /// Starts concealing the [`Text`], skipping all [`Tag`]s and
    /// [`char`]s until the [`ConcealEnd`] tag shows up.
    ///
    /// [`Text`]: super::Text
    /// [`ConcealEnd`]: RawTag::ConcealEnd
    StartConceal(Marker),
    /// Stops concealing the [`Text`], returning the iteration process
    /// back to the regular [`Text`] iterator.
    ///
    /// [`Text`]: super::Text
    /// [`ConcealEnd`]: RawTag::ConcealEnd
    EndConceal(Marker),

    // TODO: Deal with the concequences of changing this from a usize.
    /// More direct skipping method, allowing for full skips without
    /// the iteration, which could be slow.
    ConcealUntil(u32),

    GhostText(Marker, TextId),

    // Not Implemented:
    /// Begins a toggleable section in the text.
    ToggleStart(Marker, ToggleId),
    /// Ends a toggleable section in the text.
    ToggleEnd(Marker, ToggleId),
}

impl RawTag {
    pub fn inverse(&self) -> Option<Self> {
        match self {
            Self::PushForm(marker, id) => Some(Self::PopForm(*marker, *id)),
            Self::PopForm(marker, id) => Some(Self::PushForm(*marker, *id)),
            Self::ToggleStart(marker, id) => Some(Self::ToggleEnd(*marker, *id)),
            Self::ToggleEnd(marker, id) => Some(Self::ToggleStart(*marker, *id)),
            Self::StartConceal(marker) => Some(Self::EndConceal(*marker)),
            Self::EndConceal(marker) => Some(Self::StartConceal(*marker)),
            Self::StartAlignLeft(marker) => Some(Self::EndAlignLeft(*marker)),
            Self::EndAlignLeft(marker) => Some(Self::StartAlignLeft(*marker)),
            Self::StartAlignCenter(marker) => Some(Self::EndAlignCenter(*marker)),
            Self::EndAlignCenter(marker) => Some(Self::StartAlignCenter(*marker)),
            Self::StartAlignRight(marker) => Some(Self::EndAlignRight(*marker)),
            Self::EndAlignRight(marker) => Some(Self::StartAlignRight(*marker)),
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

    pub(super) fn marker(&self) -> Marker {
        match self {
            Self::PushForm(marker, _)
            | Self::PopForm(marker, _)
            | Self::MainCursor(marker)
            | Self::ExtraCursor(marker)
            | Self::StartAlignLeft(marker)
            | Self::EndAlignLeft(marker)
            | Self::StartAlignCenter(marker)
            | Self::EndAlignCenter(marker)
            | Self::StartAlignRight(marker)
            | Self::EndAlignRight(marker)
            | Self::StartConceal(marker)
            | Self::EndConceal(marker)
            | Self::GhostText(marker, _)
            | Self::ToggleStart(marker, _)
            | Self::ToggleEnd(marker, _) => *marker,
            Self::ConcealUntil(_) => unreachable!(
                "This method should only be used on stored tags, this not being one of them."
            ),
        }
    }
}

impl std::fmt::Debug for RawTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RawTag::PushForm(id, marker) => write!(f, "PushForm({id:?}, {marker:?})"),
            RawTag::PopForm(id, marker) => write!(f, "PopForm({id:?}, {marker:?})"),
            RawTag::MainCursor(marker) => write!(f, "MainCursor({marker:?})"),
            RawTag::ExtraCursor(marker) => write!(f, "ExtraCursor({marker:?})"),
            RawTag::StartAlignLeft(marker) => write!(f, "StartAlignLeft({marker:?})"),
            RawTag::EndAlignLeft(marker) => write!(f, "EndAlignLeft({marker:?})"),
            RawTag::StartAlignCenter(marker) => write!(f, "StartAlignCenter({marker:?})"),
            RawTag::EndAlignCenter(marker) => write!(f, "EndAlignCenter({marker:?})"),
            RawTag::StartAlignRight(marker) => write!(f, "StartAlignRight({marker:?})"),
            RawTag::EndAlignRight(marker) => write!(f, "EndAlignRight({marker:?})"),
            RawTag::StartConceal(marker) => write!(f, "StartConceal({marker:?})"),
            RawTag::EndConceal(marker) => write!(f, "EndConceal({marker:?})"),
            RawTag::ConcealUntil(marker) => write!(f, "ConcealUntil({marker:?})"),
            RawTag::GhostText(id, marker) => write!(f, "GhostText({id:?}, {marker:?})"),
            RawTag::ToggleStart(id, marker) => write!(f, "ToggleStart({id:?}, {marker:?})"),
            RawTag::ToggleEnd(id, marker) => write!(f, "ToggleEnd({id:?}, {marker:?})"),
        }
    }
}

pub type Toggle = Rc<dyn Fn(Point, MouseEventKind)>;
