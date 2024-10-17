//! [`Text`] building macros
//!
//! This module contains 4 macros for text building: [`text!`],
//! [`err!`], [`ok!`] and [`hint!`]. These are supposed to be used in
//! various contexts, and they have differences on what the `Default`
//! and `Accent` forms.
use std::fmt::Write;

use super::{Key, Tag, Text, ToggleId};
use crate::data::{RoData, RwData};

/// Builds and modifies a [`Text`], based on replacements applied
/// to it.
///
/// This struct is meant to be used alongside the [`text!`] family of
/// macros. You pass it as the first argument, and the [`Text`] will
/// be extended by the macro. This lets you write a [`Text`] with
/// multiple macro invocations:
///
/// ```rust
/// # use duat_core::text::{Text, hint};
/// fn is_more_than_two(num: usize) -> Text {
///     let mut builder = Text::builder();
///     hint!(builder, "The number " [*a] num [] " is ");
///     if num > 2 {
///         hint!(builder, [*a] "more" [] " than 2.");
///     } else {
///         hint!(builder, [*a] "not more" [] " than 2.");
///     }
///     builder.finish()
/// }
/// ```
///
/// [`impl Display`]: std::fmt::Display
/// [tag]: AlignCenter
/// [`Form`]: crate::forms::Form
pub struct Builder {
    text: Text,
    last_form: Option<Tag>,
    marker: Key,
    buffer: String,
}

impl Builder {
    /// Returns a new instance of [`Builder`]
    ///
    /// Use [`Text::builder`] if you don't want to bring [`Builder`]
    /// into scope.
    pub fn new() -> Self {
        Self::default()
    }

    /// Finish construction and returns the [`Text`]
    ///
    /// Will also finish the last [`Form`] tag, pushing a [`PopForm`]
    /// at the very end.
    ///
    /// [`Form`]: crate::forms::Form
    /// [`PopForm`]: Tag::PopForm
    pub fn finish(mut self) -> Text {
        let len = self.text.len_bytes();

        if let Some(tag) = self.last_form {
            self.text.tags.insert(len, tag, self.marker);
        }

        self.text
    }

    /// Pushes a part of the text
    ///
    /// This can be an [`impl Display`] type, a [`Data`] type holding
    /// an [`impl Display`] or a [tag surrogate].
    ///
    /// [`impl Display`]: std::fmt::Display
    /// [`Data`]: crate::data::Data
    /// [tag surrogate]: Ghost
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

    /// Pushes an [`impl Display`] to the [`Text`]
    ///
    /// [`impl Display`]: std::fmt::Display
    pub(crate) fn push_str(&mut self, display: impl std::fmt::Display) {
        self.buffer.clear();
        write!(self.buffer, "{display}").unwrap();
        self.text.insert_str(self.text.len_bytes(), &self.buffer)
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well
    /// as its inverse at the end of the [`Text`]
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

    /// Pushes [`Text`] directly
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

/// Aligns the line centrally
pub struct AlignCenter;
/// Aligns the line on the left
pub struct AlignLeft;
/// Aligns the ine on the right, which is the default
pub struct AlignRight;
/// Places ghost text
///
/// This is useful for, for example, creating command line prompts,
/// since the text is non interactable.
pub struct Ghost(pub Text);

/// A part to be pushed to a [`Builder`] by a macro
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

/// The standard [`Text`] construction macro
///
/// TODO: Docs
pub macro text {
    // Forms
    (@push $builder:expr, []) => {
        let id = crate::forms::id_of!("Default");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::forms::id_of!("Accent");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$form:ident]) => {
        let id = crate::forms::id_of!(stringify!($form));
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

/// The standard [`Text`] construction macro
///
/// TODO: Docs
pub macro ok {
    // Forms
    (@push $builder:expr, []) => {
        let id = crate::forms::id_of!("DefaultOk");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::forms::id_of!("AccentOk");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$form:ident]) => {
        let id = crate::forms::id_of!(stringify!($form));
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

/// The standard [`Text`] construction macro
///
/// TODO: Docs
pub macro err {
    // Forms
    (@push $builder:expr, []) => {
        let id = crate::forms::id_of!("DefaultErr");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::forms::id_of!("AccentErr");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$form:ident]) => {
        let id = crate::forms::id_of!(stringify!($form));
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

/// The standard [`Text`] construction macro
///
/// TODO: Docs
pub macro hint {
    // Forms
    (@push $builder:expr, []) => {
        let id = crate::forms::id_of!("DefaultHint");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::forms::id_of!("AccentHint");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$form:ident]) => {
        let id = crate::forms::id_of!(stringify!($form));
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
