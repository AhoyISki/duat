use std::{collections::HashMap, rc::Rc};

use crossterm::event::MouseEventKind;

use super::{
    ids::{TextId, ToggleId},
    Marker,
};
use crate::{palette::FormId, position::Point, text::Text};

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
    ConcealStart,
    ConcealEnd,

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
            Self::ConcealStart => (RawTag::ConcealStart(marker), None),
            Self::ConcealEnd => (RawTag::ConcealEnd(marker), None),
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

// NOTE: Unlike `TextPos`, character tags are line-byte indexed, not
// character indexed. The reason is that modules like `regex` and
// `tree-sitter` work on `u8`s, rather than `char`s.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    ConcealStart(Marker),
    /// Stops concealing the [`Text`], returning the iteration process
    /// back to the regular [`Text`] iterator.
    ///
    /// [`Text`]: super::Text
    /// [`ConcealEnd`]: RawTag::ConcealEnd
    ConcealEnd(Marker),

    /// More direct skipping method, allowing for full skips without
    /// the iteration, which could be slow.
    Concealed(usize),

    GhostText(Marker, TextId),

    // Not Implemented:
    /// Begins a hoverable section in the file.
    ToggleStart(Marker, ToggleId),
    /// Ends a hoverable section in the file.
    ToggleEnd(Marker, ToggleId),
}

impl RawTag {
    pub fn inverse(&self) -> Option<RawTag> {
        match self {
            RawTag::PushForm(marker, id) => Some(RawTag::PopForm(*marker, *id)),
            RawTag::PopForm(marker, id) => Some(RawTag::PushForm(*marker, *id)),
            RawTag::ToggleStart(marker, id) => Some(RawTag::ToggleEnd(*marker, *id)),
            RawTag::ToggleEnd(marker, id) => Some(RawTag::ToggleStart(*marker, *id)),
            RawTag::ConcealStart(marker) => Some(RawTag::ConcealEnd(*marker)),
            RawTag::ConcealEnd(marker) => Some(RawTag::ConcealStart(*marker)),
            RawTag::StartAlignLeft(marker) => Some(RawTag::EndAlignLeft(*marker)),
            RawTag::EndAlignLeft(marker) => Some(RawTag::StartAlignLeft(*marker)),
            RawTag::StartAlignCenter(marker) => Some(RawTag::EndAlignCenter(*marker)),
            RawTag::EndAlignCenter(marker) => Some(RawTag::StartAlignCenter(*marker)),
            RawTag::StartAlignRight(marker) => Some(RawTag::EndAlignRight(*marker)),
            RawTag::EndAlignRight(marker) => Some(RawTag::StartAlignRight(*marker)),
            _ => None,
        }
    }

    pub fn ends_with(&self, other: &RawTag) -> bool {
        match (self, other) {
            (RawTag::PushForm(lhs_m, lhs_id), RawTag::PopForm(rhs_m, rhs_id)) => {
                lhs_m == rhs_m && lhs_id == rhs_id
            }
            (RawTag::ToggleStart(lhs_m, lhs_id), RawTag::ToggleEnd(rhs_m, rhs_id)) => {
                lhs_m == rhs_m && lhs_id == rhs_id
            }
            (RawTag::StartAlignLeft(lhs), RawTag::EndAlignLeft(rhs))
            | (RawTag::StartAlignCenter(lhs), RawTag::EndAlignCenter(rhs))
            | (RawTag::StartAlignRight(lhs), RawTag::EndAlignRight(rhs))
            | (RawTag::ConcealStart(rhs), RawTag::ConcealEnd(lhs)) => lhs == rhs,
            _ => false,
        }
    }

    pub fn is_start(&self) -> bool {
        matches!(
            self,
            RawTag::PushForm(..)
                | RawTag::StartAlignLeft(_)
                | RawTag::StartAlignCenter(_)
                | RawTag::StartAlignRight(_)
                | RawTag::ToggleStart(..)
                | RawTag::ConcealStart(_)
        )
    }

    pub fn is_end(&self) -> bool {
        matches!(
            self,
            RawTag::PopForm(..)
                | RawTag::EndAlignLeft(_)
                | RawTag::EndAlignCenter(_)
                | RawTag::EndAlignRight(_)
                | RawTag::ToggleEnd(..)
                | RawTag::ConcealEnd(_)
        )
    }

    pub fn is_start_align(&self) -> bool {
        matches!(
            self,
            RawTag::StartAlignLeft(_) | RawTag::StartAlignCenter(_) | RawTag::StartAlignRight(_)
        )
    }

    pub fn is_end_align(&self) -> bool {
        matches!(
            self,
            RawTag::EndAlignLeft(_) | RawTag::EndAlignCenter(_) | RawTag::EndAlignRight(_)
        )
    }

    pub(super) fn marker(&self) -> Marker {
        match self {
            RawTag::PushForm(marker, _)
            | RawTag::PopForm(marker, _)
            | RawTag::MainCursor(marker)
            | RawTag::ExtraCursor(marker)
            | RawTag::StartAlignLeft(marker)
            | RawTag::EndAlignLeft(marker)
            | RawTag::StartAlignCenter(marker)
            | RawTag::EndAlignCenter(marker)
            | RawTag::StartAlignRight(marker)
            | RawTag::EndAlignRight(marker)
            | RawTag::ConcealStart(marker)
            | RawTag::ConcealEnd(marker)
            | RawTag::GhostText(marker, _)
            | RawTag::ToggleStart(marker, _)
            | RawTag::ToggleEnd(marker, _) => *marker,
            RawTag::Concealed(_) => unreachable!(
                "This method should only be used on stored tags, this not being one of them."
            ),
        }
    }
}

pub type Toggle = Rc<dyn Fn(Point, MouseEventKind)>;
