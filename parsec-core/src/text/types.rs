use std::rc::Rc;

use crossterm::event::MouseEventKind;

use super::tags::{RawTag, ToggleId};
use crate::{forms::FormId, position::Point, PALETTE};

/// A part of the [`Text`], can be a [`char`] or a [`Tag`].
#[derive(Clone, Copy)]
pub enum Part {
    Char(char),
    PushForm(FormId),
    PopForm(FormId),
    MainCursor,
    ExtraCursor,
    AlignLeft,
    AlignCenter,
    AlignRight,
    ToggleStart(ToggleId),
    ToggleEnd(ToggleId),
    Termination,
}

impl Part {
    pub(super) fn from_raw(value: RawTag) -> Self {
        match value {
            RawTag::PushForm(id) => Part::PushForm(id),
            RawTag::PopForm(id) => Part::PopForm(id),
            RawTag::MainCursor => Part::MainCursor,
            RawTag::ExtraCursor => Part::ExtraCursor,
            RawTag::AlignLeft => Part::AlignLeft,
            RawTag::AlignCenter => Part::AlignCenter,
            RawTag::AlignRight => Part::AlignRight,
            RawTag::ToggleStart(id) => Part::ToggleStart(id),
            RawTag::ToggleEnd(id) => Part::ToggleEnd(id),
            RawTag::Concealed(_) => Part::Termination,
            RawTag::ConcealStart | RawTag::ConcealEnd | RawTag::GhostText(..) => {
                unreachable!("These tags are automatically processed elsewhere.")
            }
        }
    }
}

impl std::fmt::Debug for Part {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Part::Char(char) => f.debug_tuple("Char").field(char).finish(),
            Part::PushForm(id) => f
                .debug_tuple("PushForm")
                .field(&PALETTE.name_from_id(*id))
                .finish(),
            Part::PopForm(id) => f
                .debug_tuple("PopForm")
                .field(&PALETTE.name_from_id(*id))
                .finish(),
            Part::MainCursor => f.debug_tuple("MainCursor").finish(),
            Part::ExtraCursor => f.debug_tuple("ExtraCursor").finish(),
            Part::AlignLeft => f.debug_tuple("AlignLeft").finish(),
            Part::AlignCenter => f.debug_tuple("AlignCenter ").finish(),
            Part::AlignRight => f.debug_tuple("AlignRight ").finish(),
            Part::ToggleStart(_) => f.debug_tuple("ToggleStart").finish(),
            Part::ToggleEnd(_) => f.debug_tuple("ToggleEnd").finish(),
            Part::Termination => f.debug_tuple("Termination ").finish(),
        }
    }
}

impl Part {
    /// Returns `true` if the text bit is [`Char`].
    ///
    /// [`Char`]: TextBit::Char
    #[must_use]
    pub fn is_char(&self) -> bool {
        matches!(self, Part::Char(_))
    }

    pub fn as_char(&self) -> Option<char> {
        if let Self::Char(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn is_tag(&self) -> bool {
        !self.is_char()
    }
}

pub type Toggle = Rc<dyn Fn(Point, MouseEventKind) + Send + Sync>;
