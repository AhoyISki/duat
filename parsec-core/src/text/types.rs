use super::{
    tags::{RawTag, ToggleId},
    Handle,
};
use crate::{forms::FormId, position::Point};

/// A part of the [`Text`], can be a [`char`] or a [`Tag`].
#[derive(Debug, Clone, Copy)]
pub enum Part {
    Char(char),
    PushForm(FormId),
    PopForm(FormId),
    MainCursor,
    ExtraCursor,
    AlignLeft,
    AlignCenter,
    AlignRight,
    HoverStart(ToggleId),
    HoverEnd(ToggleId),
    LeftButtonStart(ToggleId),
    LeftButtonEnd(ToggleId),
    RightButtonStart(ToggleId),
    RightButtonEnd(ToggleId),
    MiddleButtonStart(ToggleId),
    MiddleButtonEnd(ToggleId),
    Termination,
}

impl From<RawTag> for Part {
    fn from(value: RawTag) -> Self {
        match value {
            RawTag::PushForm(id, _) => Part::PushForm(id),
            RawTag::PopForm(id, _) => Part::PopForm(id),
            RawTag::MainCursor(_) => Part::MainCursor,
            RawTag::ExtraCursor(_) => Part::ExtraCursor,
            RawTag::AlignLeft(_) => Part::AlignLeft,
            RawTag::AlignCenter(_) => Part::AlignCenter,
            RawTag::AlignRight(_) => Part::AlignRight,
            RawTag::HoverStart(id, _) => Part::HoverStart(id),
            RawTag::HoverEnd(id, _) => Part::HoverEnd(id),
            RawTag::LeftButtonStart(id, _) => Part::LeftButtonStart(id),
            RawTag::LeftButtonEnd(id, _) => Part::LeftButtonEnd(id),
            RawTag::RightButtonStart(id, _) => Part::RightButtonStart(id),
            RawTag::RightButtonEnd(id, _) => Part::RightButtonEnd(id),
            RawTag::MiddleButtonStart(id, _) => Part::MiddleButtonStart(id),
            RawTag::MiddleButtonEnd(id, _) => Part::MiddleButtonEnd(id),
            RawTag::Concealed(..) => Part::Termination,
            RawTag::ConcealStart(_) | RawTag::ConcealEnd(_) | RawTag::GhostText(..) => {
                unreachable!()
            }
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

pub type ButtonFn = Box<dyn Fn(Point) + Send + Sync>;

/// A subset of [`Tag`], that can be used by a [`TextBuilder`].
///
/// This subset excludes, for example, all cursor related tags, and all text
/// hiding/ghost text tags.
pub enum BuilderTag {
    PushForm(FormId),
    PopForm(FormId),
    AlignLeft,
    AlignCenter,
    AlignRight,

    HoverStartNew(ButtonFn, ButtonFn),
    HoverStart(ToggleId),
    HoverEnd(ToggleId),

    LeftButtonStartNew(ButtonFn, ButtonFn),
    LeftButtonStart(ToggleId),
    LeftButtonEnd(ToggleId),

    RightButtonStartNew(ButtonFn, ButtonFn),
    RightButtonStart(ToggleId),
    RightButtonEnd(ToggleId),

    MiddleButtonStartNew(ButtonFn, ButtonFn),
    MiddleButtonStart(ToggleId),
    MiddleButtonEnd(ToggleId),
}

impl BuilderTag {
    pub(super) fn into_raw(
        self,
        handle: Handle,
        toggles: &mut Vec<(ButtonFn, ButtonFn)>,
    ) -> RawTag {
        match self {
            BuilderTag::PushForm(id) => RawTag::PushForm(id, handle),
            BuilderTag::PopForm(id) => RawTag::PopForm(id, handle),
            BuilderTag::AlignLeft => RawTag::AlignLeft(handle),
            BuilderTag::AlignCenter => RawTag::AlignCenter(handle),
            BuilderTag::AlignRight => RawTag::AlignRight(handle),
            BuilderTag::HoverStartNew(on_fn, off_fn) => {
                toggles.push((on_fn, off_fn));
                RawTag::HoverStart(ToggleId::new(toggles.len() - 1), handle)
            }
            BuilderTag::HoverStart(id) => RawTag::HoverStart(id, handle),
            BuilderTag::HoverEnd(id) => RawTag::HoverEnd(id, handle),

            BuilderTag::LeftButtonStartNew(on_fn, off_fn) => {
                toggles.push((on_fn, off_fn));
                RawTag::LeftButtonStart(ToggleId::new(toggles.len() - 1), handle)
            }
            BuilderTag::LeftButtonStart(id) => RawTag::LeftButtonStart(id, handle),
            BuilderTag::LeftButtonEnd(id) => RawTag::LeftButtonEnd(id, handle),

            BuilderTag::RightButtonStartNew(on_fn, off_fn) => {
                toggles.push((on_fn, off_fn));
                RawTag::RightButtonStart(ToggleId::new(toggles.len() - 1), handle)
            }
            BuilderTag::RightButtonStart(id) => RawTag::RightButtonStart(id, handle),
            BuilderTag::RightButtonEnd(id) => RawTag::RightButtonEnd(id, handle),

            BuilderTag::MiddleButtonStartNew(on_fn, off_fn) => {
                toggles.push((on_fn, off_fn));
                RawTag::MiddleButtonStart(ToggleId::new(toggles.len() - 1), handle)
            }
            BuilderTag::MiddleButtonStart(id) => RawTag::MiddleButtonStart(id, handle),
            BuilderTag::MiddleButtonEnd(id) => RawTag::MiddleButtonEnd(id, handle),
        }
    }
}
