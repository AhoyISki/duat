//! The [`Part`] struct, used in [`Text`] iteration
//!
//! The [`Part`] is meant to combine the outputs of the `u8` and
//! [`RawTag`] buffers of the [`Text`]. It also gets rid of meta tags,
//! since those are not useful for the purposes of printing text.
//!
//! [`Text`]: super::Text
use super::tags::{RawTag, ToggleId};
use crate::forms::{self, FormId};

/// A part of the [`Text`], can be a [`char`] or a [`Tag`].
///
/// This type is used in iteration by [`Ui`]s in order to correctly
/// print Duat's content. Additionally, you may be able to tell that
/// there is no ghost text or concealment tags, and there is a
/// [`ResetState`].
///
/// That is because the [`Text`]'s iteration process automatically
/// gets rid of these tags, since, from the point of view of the ui,
/// ghost text is just regular text, while conceals are simply the
/// lack of text. And if the ui can handle printing regular text,
/// printing ghost text should be a breeze.
///
/// [`Text`]: super::Text
/// [`Tag`]: super::Tag
/// [`Ui`]: crate::ui::Ui
/// [`ResetState`]: Part::ResetState
#[derive(Clone, Copy, PartialEq, Eq)]
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
    ResetState,
}

impl Part {
    /// Returns a new [`Part`] from a [`RawTag`]
    #[inline]
    pub(super) fn from_raw(value: RawTag) -> Self {
        match value {
            RawTag::PushForm(_, id) => Part::PushForm(id),
            RawTag::PopForm(_, id) => Part::PopForm(id),
            RawTag::MainCursor(_) => Part::MainCursor,
            RawTag::ExtraCursor(_) => Part::ExtraCursor,
            RawTag::StartAlignLeft(_) => Part::AlignLeft,
            RawTag::EndAlignLeft(_) => Part::AlignLeft,
            RawTag::StartAlignCenter(_) => Part::AlignCenter,
            RawTag::EndAlignCenter(_) => Part::AlignLeft,
            RawTag::StartAlignRight(_) => Part::AlignRight,
            RawTag::EndAlignRight(_) => Part::AlignLeft,
            RawTag::ToggleStart(_, id) => Part::ToggleStart(id),
            RawTag::ToggleEnd(_, id) => Part::ToggleEnd(id),
            RawTag::ConcealUntil(_) => Part::ResetState,
            RawTag::StartConceal(_) | RawTag::EndConceal(_) | RawTag::GhostText(..) => {
                unreachable!("These tags are automatically processed elsewhere.")
            }
        }
    }

    /// Returns `true` if the part is [`Char`]
    ///
    /// [`Char`]: Part::Char
    #[must_use]
    #[inline]
    pub fn is_char(&self) -> bool {
        matches!(self, Part::Char(_))
    }

    /// Returns [`Some`] if the part is [`Char`]
    ///
    /// [`Char`]: Part::Char
    #[inline]
    pub fn as_char(&self) -> Option<char> {
        if let Self::Char(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    /// Returns `true` if the part is not [`Char`]
    ///
    /// [`Char`]: Part::Char
    #[inline]
    pub fn is_tag(&self) -> bool {
        !self.is_char()
    }
}

impl std::fmt::Debug for Part {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Part::Char(char) => f.debug_tuple("Char").field(char).finish(),
            Part::PushForm(id) => f
                .debug_tuple("PushForm")
                .field(&forms::name_from_id(*id))
                .finish(),
            Part::PopForm(id) => f
                .debug_tuple("PopForm")
                .field(&forms::name_from_id(*id))
                .finish(),
            Part::MainCursor => f.debug_tuple("MainCursor").finish(),
            Part::ExtraCursor => f.debug_tuple("ExtraCursor").finish(),
            Part::AlignLeft => f.debug_tuple("AlignLeft").finish(),
            Part::AlignCenter => f.debug_tuple("AlignCenter ").finish(),
            Part::AlignRight => f.debug_tuple("AlignRight ").finish(),
            Part::ToggleStart(_) => f.debug_tuple("ToggleStart").finish(),
            Part::ToggleEnd(_) => f.debug_tuple("ToggleEnd").finish(),
            Part::ResetState => f.debug_tuple("Termination ").finish(),
        }
    }
}
