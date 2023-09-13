use std::{cmp::Ordering, rc::Rc};

use super::Handle;
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

    GhostText(Rc<Text>),
    ConcealStart,
    ConcealEnd,

    // Not yet implemented:
    /// Begins a hoverable section in the file.
    ToggleStart(Rc<Toggle>),
    ToggleEnd(Rc<Toggle>),
}

impl Tag {
    pub fn ghost_text(to_text: impl Into<Text>) -> Self {
        Self::GhostText(Rc::new(to_text.into()))
    }

    pub fn to_raw(self, handle: Handle) -> RawTag {
        match self {
            Self::PushForm(id) => RawTag::PushForm(id),
            Self::PopForm(id) => RawTag::PopForm(id),
            Self::MainCursor => RawTag::MainCursor,
            Self::ExtraCursor => RawTag::ExtraCursor,
            Self::AlignLeft => RawTag::AlignLeft,
            Self::AlignCenter => RawTag::AlignCenter,
            Self::AlignRight => RawTag::AlignRight,
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

                RawTag::GhostText(text)
            }
            Self::ConcealStart => RawTag::ConcealStart,
            Self::ConcealEnd => RawTag::ConcealEnd,
            Self::ToggleStart(toggle) => RawTag::ToggleStart(toggle),
            Self::ToggleEnd(toggle) => RawTag::ToggleEnd(toggle),
        }
    }
}

// NOTE: Unlike `TextPos`, character tags are line-byte indexed, not
// character indexed. The reason is that modules like `regex` and
// `tree-sitter` work on `u8`s, rather than `char`s.
#[derive(Clone)]
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

    GhostText(Rc<Text>),

    // Not Implemented:
    /// Begins a hoverable section in the file.
    ToggleStart(Rc<Toggle>),
    /// Ends a hoverable section in the file.
    ToggleEnd(Rc<Toggle>),
}

impl RawTag {
    pub fn inverse(&self) -> Option<RawTag> {
        let a: Rc<dyn core::ops::Add<i32, Output = i32>> = Rc::new(123);
        match self {
            RawTag::PushForm(id) => Some(RawTag::PopForm(*id)),
            RawTag::PopForm(id) => Some(RawTag::PushForm(*id)),
            RawTag::ToggleStart(toggle) => Some(RawTag::ToggleEnd(toggle.clone())),
            RawTag::ToggleEnd(toggle) => Some(RawTag::ToggleStart(toggle.clone())),
            RawTag::ConcealStart => Some(RawTag::ConcealEnd),
            RawTag::ConcealEnd => Some(RawTag::ConcealStart),
            _ => None,
        }
    }

    pub fn ends_with(&self, other: &RawTag) -> bool {
        match (self, other) {
            (RawTag::PushForm(s_id), RawTag::PopForm(e_id)) => s_id == e_id,
            (RawTag::ToggleStart(s_toggle), RawTag::ToggleEnd(e_toggle)) => {
                Rc::ptr_eq(&s_toggle, &e_toggle)
            }
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

    fn discriminant(&self) -> u8 {
        match self {
            RawTag::PushForm(_) => 0,
            RawTag::PopForm(_) => 1,
            RawTag::MainCursor => 2,
            RawTag::ExtraCursor => 3,
            RawTag::AlignLeft => 4,
            RawTag::AlignCenter => 5,
            RawTag::AlignRight => 6,
            RawTag::ConcealStart => 7,
            RawTag::ConcealEnd => 8,
            RawTag::Concealed(_) => 9,
            RawTag::GhostText(_) => 10,
            RawTag::ToggleStart(_) => 11,
            RawTag::ToggleEnd(_) => 12,
        }
    }
}

impl PartialEq for RawTag {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RawTag::PushForm(lhs), RawTag::PushForm(rhs)) => lhs == rhs,
            (RawTag::PopForm(lhs), RawTag::PopForm(rhs)) => lhs == rhs,
            (RawTag::Concealed(lhs), RawTag::Concealed(rhs)) => lhs == rhs,
            (RawTag::GhostText(lhs), RawTag::GhostText(rhs)) => Rc::ptr_eq(lhs, rhs),
            (RawTag::ToggleStart(lhs), RawTag::ToggleStart(rhs)) => Rc::ptr_eq(lhs, rhs),
            (RawTag::ToggleEnd(lhs), RawTag::ToggleEnd(rhs)) => Rc::ptr_eq(lhs, rhs),
            (RawTag::MainCursor, RawTag::MainCursor)
            | (RawTag::ExtraCursor, RawTag::ExtraCursor)
            | (RawTag::AlignLeft, RawTag::AlignLeft)
            | (RawTag::AlignCenter, RawTag::AlignCenter)
            | (RawTag::AlignRight, RawTag::AlignRight)
            | (RawTag::ConcealStart, RawTag::ConcealStart)
            | (RawTag::ConcealEnd, RawTag::ConcealEnd) => true,
            _ => false,
        }
    }
}

impl Eq for RawTag {}

impl PartialOrd for RawTag {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RawTag {
    fn cmp(&self, other: &Self) -> Ordering {
        let ordering = self.discriminant().cmp(&other.discriminant());

        if let Ordering::Equal = ordering {
            match (self, other) {
                (RawTag::PushForm(lhs), RawTag::PushForm(rhs)) => lhs.cmp(rhs),
                (RawTag::PopForm(lhs), RawTag::PopForm(rhs)) => lhs.cmp(rhs),
                (RawTag::Concealed(lhs), RawTag::Concealed(rhs)) => lhs.cmp(rhs),
                (RawTag::GhostText(lhs), RawTag::GhostText(rhs)) => {
                    Rc::as_ptr(lhs).cmp(&Rc::as_ptr(rhs))
                }
                (RawTag::ToggleStart(lhs), RawTag::ToggleStart(rhs)) => {
                    Rc::as_ptr(lhs).cmp(&Rc::as_ptr(rhs))
                }
                (RawTag::ToggleEnd(lhs), RawTag::ToggleEnd(rhs)) => {
                    Rc::as_ptr(lhs).cmp(&Rc::as_ptr(rhs))
                }
                (RawTag::MainCursor, RawTag::MainCursor)
                | (RawTag::ExtraCursor, RawTag::ExtraCursor)
                | (RawTag::AlignLeft, RawTag::AlignLeft)
                | (RawTag::AlignCenter, RawTag::AlignCenter)
                | (RawTag::AlignRight, RawTag::AlignRight)
                | (RawTag::ConcealStart, RawTag::ConcealStart)
                | (RawTag::ConcealEnd, RawTag::ConcealEnd) => Ordering::Equal,
                _ => unreachable!(),
            }
        } else {
            ordering
        }
    }
}

impl std::fmt::Debug for RawTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RawTag::PushForm(id) => f.debug_tuple("PushForm").field(id).finish(),
            RawTag::PopForm(id) => f.debug_tuple("PopForm").field(id).finish(),
            RawTag::MainCursor => f.debug_tuple("MainCursor").finish(),
            RawTag::ExtraCursor => f.debug_tuple("ExtraCursor").finish(),
            RawTag::AlignLeft => f.debug_tuple("AlignLeft").finish(),
            RawTag::AlignCenter => f.debug_tuple("AlignCenter").finish(),
            RawTag::AlignRight => f.debug_tuple("AlignRight").finish(),
            RawTag::ConcealStart => f.debug_tuple("ConcealStart").finish(),
            RawTag::ConcealEnd => f.debug_tuple("ConcealEnd").finish(),
            RawTag::Concealed(count) => f.debug_tuple("Concealed").field(count).finish(),
            RawTag::GhostText(text) => f.debug_tuple("GhostText").field(text).finish(),
            RawTag::ToggleStart(_) => f.debug_tuple("ToggleStart").finish(),
            RawTag::ToggleEnd(_) => f.debug_tuple("ToggleEnd").finish(),
        }
    }
}
