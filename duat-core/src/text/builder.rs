use std::{fmt::Write, sync::LazyLock};

use super::{Marker, Tag, Text, ToggleId};
use crate::{
    data::{RoData, RwData},
    palette::FormId,
};

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
    last_align: Option<Tag>,
    marker: Marker,
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

        if let Some(tag) = self.last_align {
            self.text.tags.insert(len, tag, self.marker);
        }

        self.text
    }

    pub fn push_str(&mut self, display: impl std::fmt::Display) {
        self.buffer.clear();
        write!(self.buffer, "{display}").unwrap();
        self.text.insert_str(self.text.len_bytes(), &self.buffer)
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well
    /// as its inverse at the end of the [`Text`].
    pub fn push_tag(&mut self, tag: Tag) -> Option<ToggleId> {
        let len = self.text.len_bytes();
        let last_inverted = match tag {
            Tag::PushForm(id) => self.last_form.replace(Tag::PopForm(id)),
            Tag::StartAlignLeft => self.last_align.replace(Tag::EndAlignLeft),
            Tag::StartAlignCenter => self.last_align.replace(Tag::EndAlignCenter),
            Tag::StartAlignRight => self.last_align.replace(Tag::EndAlignRight),
            _ => None,
        };

        if let Some(tag) = last_inverted {
            self.text.tags.insert(len, tag, self.marker);
        }

        self.text.tags.insert(len, tag, self.marker)
    }

    pub fn push_text(&mut self, text: Text) {
        let end = self.text.len_bytes();

        if let Some(tag) = self.last_form.take() {
            self.text.tags.insert(end, tag, self.marker);
        }

        self.text.buf.splice(end..end, *text.buf);
        self.text.tags.extend(*text.tags);
    }

    pub fn push_part<D: std::fmt::Display>(&mut self, part: BuilderPart<D>) {
        match part {
            BuilderPart::Text(text) => self.push_text(text),
            BuilderPart::Tag(tag) => {
                self.push_tag(tag);
            }
            BuilderPart::ToString(display) => self.push_str(display),
        }
    }

    pub fn clear(&mut self) {
        self.text.clear();
        self.last_form = None;
    }
}

impl Default for Builder {
    fn default() -> Self {
        Builder {
            text: Text::default(),
            last_form: None,
            last_align: None,
            marker: Marker::base(),
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

// pub macro text {
//    ($builder:expr, $($parts:tt)+) => {
//        inner_text!("Default", "Accent", $builder, $($parts)+)
//    },
//    ($($parts:tt)+) => {{
//        inner_text!("Default", "Accent", $($parts)+)
//    }},
//}

pub macro text {
    // Forms
    (@push $builder:expr, []) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("Default")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },
    (@push $builder:expr, [*a]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("Accent")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    (@push $builder:expr, [$form:ident]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            let name = stringify!($form);
            crate::palette::__weakest_id_of_name(name)
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        let part = BuilderPart::from($part);
        $builder.push_part(part)
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
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("DefaultOk")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },
    (@push $builder:expr, [*a]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("AccentOk")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    (@push $builder:expr, [$form:ident]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            let name = stringify!($form);
            crate::palette::__weakest_id_of_name(name)
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        let part = BuilderPart::from($part);
        $builder.push_part(part)
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
        Ok(Some(builder.finish()))
    }},
}

pub macro err {
    // Forms
    (@push $builder:expr, []) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("DefaultErr")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },
    (@push $builder:expr, [*a]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("AccentErr")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    (@push $builder:expr, [$form:ident]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            let name = stringify!($form);
            crate::palette::__weakest_id_of_name(name)
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        let part = BuilderPart::from($part);
        $builder.push_part(part)
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

pub macro hint {
    // Forms
    (@push $builder:expr, []) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("DefaultHint")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },
    (@push $builder:expr, [*a]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("AccentHint")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    (@push $builder:expr, [$form:ident]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            let name = stringify!($form);
            crate::palette::__weakest_id_of_name(name)
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    // Plain text
    (@push $builder:expr, $part:expr) => {
        let part = BuilderPart::from($part);
        $builder.push_part(part)
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

#[doc(hidden)]
pub struct __FormIdLock(LazyLock<FormId>);

impl std::ops::Deref for __FormIdLock {
    type Target = FormId;

    fn deref(&self) -> &FormId {
        self.0.deref()
    }
}

impl __FormIdLock {
    #[doc(hidden)]
    pub const fn new(f: fn() -> FormId) -> Self {
        Self(LazyLock::new(f))
    }
}
