use std::collections::HashMap;

use super::ids::{TextId, ToggleId};
use crate::{
    forms::FormId,
    text::{types::Toggle, Text},
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

    /// Changes the alignment of the text to the left of the area.
    /// This only takes effect after this line terminates.
    AlignLeft,
    /// Changes the alignemet of the text to the center of the area.  
    /// This only takes effect after this line terminates.
    AlignCenter,
    /// Changes the alignment of the text to the right of the area.
    /// This only takes effect after this line terminates.
    AlignRight,

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
        texts: &mut HashMap<TextId, Text>,
        toggles: &mut HashMap<ToggleId, Toggle>,
    ) -> (RawTag, Option<ToggleId>) {
        match self {
            Self::PushForm(id) => (RawTag::PushForm(id), None),
            Self::PopForm(id) => (RawTag::PopForm(id), None),
            Self::MainCursor => (RawTag::MainCursor, None),
            Self::ExtraCursor => (RawTag::ExtraCursor, None),
            Self::AlignLeft => (RawTag::AlignLeft, None),
            Self::AlignCenter => (RawTag::AlignCenter, None),
            Self::AlignRight => (RawTag::AlignRight, None),
            Self::GhostText(text) => {
                #[cfg(feature = "wacky-colors")]
                let text = {
                    use crate::{forms, text::Tag};
                    let mut text = text;
                    let mut tagger = text.tag_with(Handle::new());
                    tagger.insert(0, Tag::PushForm(forms::COORDS));
                    tagger.insert(tagger.len_chars(), Tag::PopForm(forms::COORDS));

                    text
                };
                let id = TextId::new();
                texts.insert(id, text);
                (RawTag::GhostText(id), None)
            }
            Self::ConcealStart => (RawTag::ConcealStart, None),
            Self::ConcealEnd => (RawTag::ConcealEnd, None),
            Self::ToggleStart(toggle) => {
                let id = ToggleId::new();
                toggles.insert(id, toggle);
                (RawTag::ToggleStart(id), Some(id))
            }
            Self::ToggleEnd(id) => (RawTag::ToggleEnd(id), None),
        }
    }
}

// NOTE: Unlike `TextPos`, character tags are line-byte indexed, not
// character indexed. The reason is that modules like `regex` and
// `tree-sitter` work on `u8`s, rather than `char`s.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RawTag {
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

    /// Changes the alignment of the text to the left of the area.
    /// This only takes effect after this line terminates.
    AlignLeft,
    /// Changes the alignemet of the text to the center of the area.  
    /// This only takes effect after this line terminates.
    AlignCenter,
    /// Changes the alignment of the text to the right of the area.
    /// This only takes effect after this line terminates.
    AlignRight,

    // In the process of implementing.
    /// Starts concealing the [`Text`], skipping all [`Tag`]s and
    /// [`char`]s until the [`ConcealEnd`] tag shows up.
    ///
    /// [`Text`]: super::Text
    /// [`ConcealEnd`]: RawTag::ConcealEnd
    ConcealStart,
    /// Stops concealing the [`Text`], returning the iteration process
    /// back to the regular [`Text`] iterator.
    ///
    /// [`Text`]: super::Text
    /// [`ConcealEnd`]: RawTag::ConcealEnd
    ConcealEnd,

    /// More direct skipping method, allowing for full skips without
    /// the iteration, which could be slow.
    Concealed(usize),

    GhostText(TextId),

    // Not Implemented:
    /// Begins a hoverable section in the file.
    ToggleStart(ToggleId),
    /// Ends a hoverable section in the file.
    ToggleEnd(ToggleId),
}

impl RawTag {
    pub fn inverse(&self) -> Option<RawTag> {
        match self {
            RawTag::PushForm(id) => Some(RawTag::PopForm(*id)),
            RawTag::PopForm(id) => Some(RawTag::PushForm(*id)),
            RawTag::ToggleStart(id) => Some(RawTag::ToggleEnd(*id)),
            RawTag::ToggleEnd(id) => Some(RawTag::ToggleStart(*id)),
            RawTag::ConcealStart => Some(RawTag::ConcealEnd),
            RawTag::ConcealEnd => Some(RawTag::ConcealStart),
            _ => None,
        }
    }

    pub fn ends_with(&self, other: &RawTag) -> bool {
        match (self, other) {
            (RawTag::PushForm(lhs), RawTag::PopForm(rhs)) => lhs == rhs,
            (RawTag::ToggleStart(lhs), RawTag::ToggleEnd(rhs)) => lhs == rhs,
            (RawTag::AlignCenter | RawTag::AlignRight, RawTag::AlignLeft)
            | (RawTag::ConcealStart, RawTag::ConcealEnd) => true,
            _ => false,
        }
    }

    pub fn is_start(&self) -> bool {
        matches!(
            self,
            RawTag::PushForm(_)
                | RawTag::AlignCenter
                | RawTag::AlignRight
                | RawTag::ToggleStart(_)
                | RawTag::ConcealStart
        )
    }

    pub fn is_end(&self) -> bool {
        matches!(
            self,
            RawTag::PopForm(_) | RawTag::AlignLeft | RawTag::ToggleEnd(_) | RawTag::ConcealEnd
        )
    }
}
