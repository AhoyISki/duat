use std::fmt::Write;

use super::{Key, Tag, Text, ToggleId};
use crate::data::{RoData, RwData};

/// Builds and modifies a [`Text`], based on replacements applied
/// to it.
///
/// The generation of text by the [`TextBuilder`] has a few
/// peculiarities that are convenient in the situations where it is
/// useful:
///
/// - The user cannot insert [`Tag`]s directly, only by appending and
///   modifying existing tags.
/// - All [`Tag`]s that are appended result in an inverse [`Tag`]
///   being placed before the next one, or at the end of the [`Tags`]
///   (e.g. [`Tag::PushForm`] would be followed a [`Tag::PopForm`]).
/// - You can insert swappable text with
///   [`push_swappable()`][Self::push_swappable].
///
/// These properties allow for quick and easy modification of the
/// [`Text`] within, which can then be accessed with
/// [`text`][Self::text].
pub struct Builder {
    text: Text,
    last_form: Option<Tag>,
    marker: Key,
    buffer: String,
}

impl Builder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn finish(mut self) -> Text {
        let len = self.text.len_bytes();

        if let Some(tag) = self.last_form {
            self.text.tags.insert(len, tag, self.marker);
        }

        self.text
    }

    pub fn push<D: std::fmt::Display>(&mut self, part: impl Into<BuilderPart<D>>) {
        match part.into() {
            BuilderPart::Text(text) => self.push_text(text),
            BuilderPart::Tag(tag) => {
                self.push_tag(tag);
            }
            BuilderPart::ToString(display) => self.push_str(display),
            BuilderPart::OptToString(Some(display)) => self.push_str(display),
            _ => {}
        }
    }

    pub(crate) fn push_str(&mut self, display: impl std::fmt::Display) {
        self.buffer.clear();
        write!(self.buffer, "{display}").unwrap();
        self.text.insert_str(self.text.len_bytes(), &self.buffer)
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well
    /// as its inverse at the end of the [`Text`].
    pub(crate) fn push_tag(&mut self, tag: Tag) -> Option<ToggleId> {
        let len = self.text.len_bytes();
        if let Tag::PushForm(id) = tag {
            let last_form = match id == crate::forms::DEFAULT_ID {
                true => self.last_form.take(),
                false => self.last_form.replace(Tag::PopForm(id)),
            };

            if let Some(tag) = last_form {
                self.text.tags.insert(len, tag, self.marker);
            }

            match id == crate::forms::DEFAULT_ID {
                true => None,
                false => self.text.tags.insert(len, tag, self.marker),
            }
        } else {
            self.text.tags.insert(len, tag, self.marker)
        }
    }

    pub(crate) fn push_text(&mut self, text: Text) {
        let end = self.text.len_bytes();

        if let Some(tag) = self.last_form.take() {
            self.text.tags.insert(end, tag, self.marker);
        }

        self.text.buf.splice(end..end, *text.buf);
        self.text.tags.extend(*text.tags);
    }
}

impl Default for Builder {
    fn default() -> Self {
        Builder {
            text: Text::default(),
            last_form: None,
            marker: Key::basic(),
            buffer: String::with_capacity(50),
        }
    }
}

pub struct AlignCenter;
pub struct AlignLeft;
pub struct AlignRight;
pub struct Ghost(pub Text);

pub enum BuilderPart<D>
where
    D: std::fmt::Display,
{
    Text(Text),
    Tag(Tag),
    ToString(D),
    OptToString(Option<D>),
}

impl From<AlignCenter> for BuilderPart<String> {
    fn from(_: AlignCenter) -> Self {
        BuilderPart::Tag(Tag::StartAlignCenter)
    }
}

impl From<AlignLeft> for BuilderPart<String> {
    fn from(_: AlignLeft) -> Self {
        BuilderPart::Tag(Tag::StartAlignLeft)
    }
}

impl From<AlignRight> for BuilderPart<String> {
    fn from(_: AlignRight) -> Self {
        BuilderPart::Tag(Tag::StartAlignRight)
    }
}

impl From<Ghost> for BuilderPart<String> {
    fn from(value: Ghost) -> Self {
        BuilderPart::Tag(Tag::GhostText(value.0))
    }
}

impl From<Tag> for BuilderPart<String> {
    fn from(value: Tag) -> Self {
        BuilderPart::Tag(value)
    }
}

impl From<Text> for BuilderPart<String> {
    fn from(value: Text) -> Self {
        BuilderPart::Text(value)
    }
}

impl<D> From<&RwData<D>> for BuilderPart<String>
where
    D: std::fmt::Display,
{
    fn from(value: &RwData<D>) -> Self {
        BuilderPart::ToString(value.read().to_string())
    }
}

impl<D> From<&RoData<D>> for BuilderPart<String>
where
    D: std::fmt::Display,
{
    fn from(value: &RoData<D>) -> Self {
        BuilderPart::ToString(value.read().to_string())
    }
}

impl<D> From<D> for BuilderPart<D>
where
    D: std::fmt::Display,
{
    fn from(value: D) -> Self {
        BuilderPart::ToString(value)
    }
}

impl<D> From<Option<D>> for BuilderPart<D>
where
    D: std::fmt::Display,
{
    fn from(value: Option<D>) -> Self {
        BuilderPart::OptToString(value)
    }
}

pub macro text {
    // Forms
    (@push $builder:expr, []) => {
        let id = crate::forms::to_id!("Default");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::forms::to_id!("Accent");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$form:ident]) => {
        let id = crate::forms::to_id!(stringify!($form));
        $builder.push(crate::text::Tag::PushForm(id))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        $builder.push($part)
    },

    (@parse $builder:expr, $part:tt $($parts:tt)*) => {{
        text!(@push $builder, $part);
        text!(@parse $builder, $($parts)*);
    }},
    (@parse $builder:expr,) => {},

    ($builder:expr, $($parts:tt)+) => {{
        let builder: &mut Builder = &mut $builder;
        text!(@parse builder, $($parts)+);
    }},
    ($($parts:tt)+) => {{
        let mut builder = Builder::new();
        text!(builder, $($parts)+);
        builder.finish()
    }},
}

pub macro ok {
    // Forms
    (@push $builder:expr, []) => {
        let id = crate::forms::to_id!("DefaultOk");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::forms::to_id!("AccentOk");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$form:ident]) => {
        let id = crate::forms::to_id!(stringify!($form));
        $builder.push(crate::text::Tag::PushForm(id))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        $builder.push($part)
    },

    (@parse $builder:expr, $part:tt $($parts:tt)*) => {{
        ok!(@push $builder, $part);
        ok!(@parse $builder, $($parts)*);
    }},
    (@parse $builder:expr,) => {},

    ($builder:expr, $($parts:tt)+) => {{
        let builder: &mut Builder = &mut $builder;
        ok!(@parse builder, $($parts)+);
    }},
    ($($parts:tt)+) => {{
        let mut builder = Builder::new();
        ok!(builder, [DefaultOk] $($parts)+);
        Ok(Some(builder.finish()))
    }},
}

pub macro err {
    // Forms
    (@push $builder:expr, []) => {
        let id = crate::forms::to_id!("DefaultErr");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::forms::to_id!("AccentErr");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$form:ident]) => {
        let id = crate::forms::to_id!(stringify!($form));
        $builder.push(crate::text::Tag::PushForm(id))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        $builder.push($part)
    },

    (@parse $builder:expr, $part:tt $($parts:tt)*) => {{
        err!(@push $builder, $part);
        err!(@parse $builder, $($parts)*);
    }},
    (@parse $builder:expr,) => {},

    ($builder:expr, $($parts:tt)+) => {{
        let builder: &mut Builder = &mut $builder;
        err!(@parse builder, $($parts)+);
    }},
    ($($parts:tt)+) => {{
        let mut builder = Builder::new();
        err!(builder, [DefaultErr] $($parts)+);
        builder.finish()
    }},
}

pub macro hint {
    // Forms
    (@push $builder:expr, []) => {
        let id = crate::forms::to_id!("DefaultHint");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::forms::to_id!("AccentHint");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$form:ident]) => {
        let id = crate::forms::to_id!(stringify!($form));
        $builder.push(crate::text::Tag::PushForm(id))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        $builder.push($part)
    },

    (@parse $builder:expr, $part:tt $($parts:tt)*) => {{
        hint!(@push $builder, $part);
        hint!(@parse $builder, $($parts)*);
    }},
    (@parse $builder:expr,) => {},

    ($builder:expr, $($parts:tt)+) => {{
        let builder: &mut Builder = &mut $builder;
        hint!(@parse builder, $($parts)+);
    }},
    ($($parts:tt)+) => {{
        let mut builder = Builder::new();
        hint!(builder, [DefaultHint] $($parts)+);
        builder.finish()
    }},
}
