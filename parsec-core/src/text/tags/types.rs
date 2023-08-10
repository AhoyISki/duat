use super::Handle;
use crate::{forms::FormId, position::Point, text::Text};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TextId(usize);

impl TextId {
	pub fn new(id: usize) -> Self {
    	Self(id)
	}
}

impl From<TextId> for usize {
    fn from(value: TextId) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ToggleId(usize);

impl ToggleId {
	pub fn new(id: usize) -> Self {
    	Self(id)
	}
}

impl From<ToggleId> for usize {
    fn from(value: ToggleId) -> Self {
        value.0
    }
}

pub enum InsertionTag {
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
    NewGhostText(Text),
    GhostText(TextId),
    ConcealStart,
    ConcealEnd,

    // Not Implemented:
    /// Begins a hoverable section in the file.
    NewHoverStart(Box<dyn Fn(Point)>, Box<dyn Fn(Point)>),
    HoverStart(ToggleId),
    /// Ends a hoverable section in the file.
    HoverEnd(ToggleId),

    NewLeftButtonStart(Box<dyn Fn(Point)>, Box<dyn Fn(Point)>),
    LeftButtonStart(ToggleId),
    LeftButtonEnd(ToggleId),

    NewRightButtonStart(Box<dyn Fn(Point)>, Box<dyn Fn(Point)>),
    RightButtonStart(ToggleId),
    RightButtonEnd(ToggleId),

    NewMiddleButtonStart(Box<dyn Fn(Point)>, Box<dyn Fn(Point)>),
    MiddleButtonStart(ToggleId),
    MiddleButtonEnd(ToggleId)
}

impl InsertionTag {
    pub fn ghost_from(to_string: impl ToString) -> Self {
        Self::NewGhostText(Text::new_string(to_string))
    }

    pub fn hover_start(on: impl Fn(Point) + 'static, off: impl Fn(Point) + 'static) -> Self {
        Self::NewHoverStart(Box::new(on), Box::new(off))
    }

    pub fn left_button_start(on: impl Fn(Point) + 'static, off: impl Fn(Point) + 'static) -> Self {
        Self::NewLeftButtonStart(Box::new(on), Box::new(off))
    }

    pub fn right_button_start(on: impl Fn(Point) + 'static, off: impl Fn(Point) + 'static) -> Self {
        Self::NewRightButtonStart(Box::new(on), Box::new(off))
    }

    pub fn middle_button_start(
        on: impl Fn(Point) + 'static, off: impl Fn(Point) + 'static
    ) -> Self {
        Self::NewMiddleButtonStart(Box::new(on), Box::new(off))
    }

    pub fn to_raw(
        self, handle: Handle, texts: &mut Vec<Text>,
        toggles: &mut Vec<(Box<dyn Fn(Point)>, Box<dyn Fn(Point)>)>
    ) -> (Option<TextId>, Option<ToggleId>, RawTag) {
        let (mut text_id, mut toggle_id) = (None, None);
        let tag = match self {
            Self::PushForm(id) => RawTag::PushForm(id, handle),
            Self::PopForm(id) => RawTag::PopForm(id, handle),
            Self::MainCursor => RawTag::MainCursor(handle),
            Self::ExtraCursor => RawTag::ExtraCursor(handle),
            Self::AlignLeft => RawTag::AlignLeft(handle),
            Self::AlignCenter => RawTag::AlignCenter(handle),
            Self::AlignRight => RawTag::AlignRight(handle),
            Self::NewGhostText(text) => {
                #[cfg(feature = "wacky-colors")]
                let text = {
                    use crate::{forms, text::Tag};
                    let mut text = text;
                    let mut tagger = text.tag_with(Handle::new());
                    tagger.insert(0, Tag::PushForm(forms::COORDS));
                    tagger.insert(tagger.len_chars(), Tag::PopForm(forms::COORDS));

                    text
                };

                texts.push(text);
                text_id = Some(TextId(texts.len() - 1));
                RawTag::GhostText(TextId(texts.len() - 1), handle)
            }
            Self::GhostText(id) => RawTag::GhostText(id, handle),
            Self::ConcealStart => RawTag::ConcealStart(handle),
            Self::ConcealEnd => RawTag::ConcealEnd(handle),
            Self::NewHoverStart(on_fn, off_fn) => {
                toggles.push((on_fn, off_fn));
                toggle_id = Some(ToggleId(toggles.len() - 1));
                RawTag::HoverStart(ToggleId(toggles.len() - 1), handle)
            }
            Self::HoverStart(id) => RawTag::HoverStart(id, handle),
            Self::HoverEnd(id) => RawTag::HoverEnd(id, handle),

            Self::NewLeftButtonStart(on_fn, off_fn) => {
                toggles.push((on_fn, off_fn));
                toggle_id = Some(ToggleId(toggles.len() - 1));
                RawTag::LeftButtonStart(ToggleId(texts.len() - 1), handle)
            }
            Self::LeftButtonStart(id) => RawTag::LeftButtonStart(id, handle),
            Self::LeftButtonEnd(id) => RawTag::LeftButtonEnd(id, handle),

            Self::NewRightButtonStart(on_fn, off_fn) => {
                toggles.push((on_fn, off_fn));
                toggle_id = Some(ToggleId(toggles.len() - 1));
                RawTag::RightButtonStart(ToggleId(texts.len() - 1), handle)
            }
            Self::RightButtonStart(id) => RawTag::RightButtonStart(id, handle),
            Self::RightButtonEnd(id) => RawTag::RightButtonEnd(id, handle),

            Self::NewMiddleButtonStart(on_fn, off_fn) => {
                toggles.push((on_fn, off_fn));
                toggle_id = Some(ToggleId(toggles.len() - 1));
                RawTag::MiddleButtonStart(ToggleId(texts.len() - 1), handle)
            }
            Self::MiddleButtonStart(id) => RawTag::MiddleButtonStart(id, handle),
            Self::MiddleButtonEnd(id) => RawTag::MiddleButtonEnd(id, handle)
        };

        (text_id, toggle_id, tag)
    }
}

// NOTE: Unlike `TextPos`, character tags are line-byte indexed, not
// character indexed. The reason is that modules like `regex` and
// `tree-sitter` work on `u8`s, rather than `char`s.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RawTag {
    // Implemented:
    /// Appends a form to the stack.
    PushForm(FormId, Handle),
    /// Removes a form from the stack. It won't always be the last
    /// one.
    PopForm(FormId, Handle),

    /// Places the main cursor.
    MainCursor(Handle),
    /// Places an extra cursor.
    ExtraCursor(Handle),

    /// Changes the alignment of the text to the left of the area.
    /// This only takes effect after this line terminates.
    AlignLeft(Handle),
    /// Changes the alignemet of the text to the center of the area.  
    /// This only takes effect after this line terminates.
    AlignCenter(Handle),
    /// Changes the alignment of the text to the right of the area.
    /// This only takes effect after this line terminates.
    AlignRight(Handle),

    // In the process of implementing.
    /// Starts concealing the [`Text`], skipping all [`Tag`]s and
    /// [`char`]s until the [`ConcealEnd`] tag shows up.
    ///
    /// [`Text`]: super::Text
    /// [`ConcealEnd`]: RawTag::ConcealEnd
    ConcealStart(Handle),
    /// Stops concealing the [`Text`], returning the iteration process
    /// back to the regular [`Text`] iterator.
    ///
    /// [`Text`]: super::Text
    /// [`ConcealEnd`]: RawTag::ConcealEnd
    ConcealEnd(Handle),

    /// More direct skipping method, allowing for full skips without
    /// the iteration, which could be slow.
    Concealed(usize),

    GhostText(TextId, Handle),

    // Not Implemented:
    /// Begins a hoverable section in the file.
    HoverStart(ToggleId, Handle),
    /// Ends a hoverable section in the file.
    HoverEnd(ToggleId, Handle),

    LeftButtonStart(ToggleId, Handle),
    LeftButtonEnd(ToggleId, Handle),

    RightButtonStart(ToggleId, Handle),
    RightButtonEnd(ToggleId, Handle),

    MiddleButtonStart(ToggleId, Handle),
    MiddleButtonEnd(ToggleId, Handle)
}

impl RawTag {
    pub fn inverse(&self) -> Option<RawTag> {
        match self {
            RawTag::PushForm(id, handle) => Some(RawTag::PopForm(*id, *handle)),
            RawTag::PopForm(id, handle) => Some(RawTag::PushForm(*id, *handle)),
            RawTag::HoverStart(id, handle) => Some(RawTag::HoverEnd(*id, *handle)),
            RawTag::HoverEnd(id, handle) => Some(RawTag::HoverStart(*id, *handle)),
            RawTag::ConcealStart(handle) => Some(RawTag::ConcealEnd(*handle)),
            RawTag::ConcealEnd(handle) => Some(RawTag::ConcealStart(*handle)),
            _ => None
        }
    }

    pub fn ends_with(&self, other: &RawTag) -> bool {
        match (self, other) {
            (RawTag::PushForm(s_id, s_handle), RawTag::PopForm(e_id, e_handle)) => {
                s_id == e_id && s_handle == e_handle
            }
            (
                RawTag::AlignCenter(s_handle) | RawTag::AlignRight(s_handle),
                RawTag::AlignLeft(e_handle)
            ) => s_handle == e_handle,
            (RawTag::HoverStart(s_id, s_handle), RawTag::HoverEnd(e_id, e_handle)) => {
                s_id == e_id && s_handle == e_handle
            }
            (RawTag::ConcealStart(s_handle), RawTag::ConcealEnd(e_handle)) => s_handle == e_handle,
            _ => false
        }
    }

    pub fn is_start(&self) -> bool {
        matches!(
            self,
            RawTag::PushForm(..)
                | RawTag::AlignCenter(_)
                | RawTag::AlignRight(_)
                | RawTag::HoverStart(..)
                | RawTag::ConcealStart(..)
        )
    }

    pub fn is_end(&self) -> bool {
        matches!(
            self,
            RawTag::PopForm(..)
                | RawTag::AlignLeft(_)
                | RawTag::HoverEnd(..)
                | RawTag::ConcealEnd(..)
        )
    }

    pub fn handle(&self) -> Handle {
        match self {
            RawTag::PushForm(_, handle)
            | RawTag::PopForm(_, handle)
            | RawTag::MainCursor(handle)
            | RawTag::ExtraCursor(handle)
            | RawTag::AlignLeft(handle)
            | RawTag::AlignCenter(handle)
            | RawTag::AlignRight(handle)
            | RawTag::ConcealStart(handle)
            | RawTag::ConcealEnd(handle)
            | RawTag::GhostText(_, handle)
            | RawTag::HoverStart(_, handle)
            | RawTag::HoverEnd(_, handle)
            | RawTag::LeftButtonStart(_, handle)
            | RawTag::LeftButtonEnd(_, handle)
            | RawTag::RightButtonStart(_, handle)
            | RawTag::RightButtonEnd(_, handle)
            | RawTag::MiddleButtonStart(_, handle)
            | RawTag::MiddleButtonEnd(_, handle) => *handle,
            RawTag::Concealed(..) => unreachable!()
        }
    }
}
