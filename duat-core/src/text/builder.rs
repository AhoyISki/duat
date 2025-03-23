//! [`Text`] building macros
//!
//! This module contains 4 macros for text building: [`text!`],
//! [`err!`], [`ok!`] and [`hint!`]. These are supposed to be used in
//! various contexts, and they have differences on what the `Default`
//! and `Accent` form.
use std::{fmt::Write, marker::PhantomData, path::PathBuf};

use super::{Change, Key, Tag, Text, ToggleId, tags::RawTag};
use crate::{data::RwData, form::FormId};

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
/// [`Form`]: crate::form::Form
pub struct Builder {
    text: Text,
    last_form: Option<FormId>,
    last_align: Option<Tag>,
    buffer: String,
    last_was_empty: bool,
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
    /// [`Form`]: crate::form::Form
    /// [`PopForm`]: Tag::PopForm
    pub fn finish(mut self) -> Text {
        let len = self.text.len().byte();

        if let Some(tag) = self.last_form {
            self.text
                .0
                .tags
                .insert(len, Tag::PopForm(tag), Key::basic());
        }
        if (self.text.buffers(..).next_back()).is_none_or(|b| b != b'\n') {
            self.push_str("\n");
            self.text.0.forced_new_line = true;
        }

        self.text
    }

    /// Pushes a part of the text
    ///
    /// This can be an [`impl Display`] type, a [`RwData`] holding
    /// an [`impl Display`] or a [tag surrogate].
    ///
    /// [`impl Display`]: std::fmt::Display
    /// [tag surrogate]: Ghost
    pub fn push<S: ToString, _T>(&mut self, part: impl Into<BuilderPart<S, _T>>) {
        let part = part.into();
        match part {
            BuilderPart::Text(text) => self.push_text(text),
            BuilderPart::Tag(tag) | BuilderPart::OptToTag(Some(tag)) => {
                self.push_tag(tag);
            }
            BuilderPart::ToString(display) => self.push_str(&display.to_string()),
            BuilderPart::OptToString(Some(display)) => self.push_str(&display.to_string()),
            BuilderPart::EndLastAlign(_) => match self.last_align {
                Some(Tag::StartAlignCenter) => {
                    self.push_tag(Tag::EndAlignCenter);
                }
                Some(Tag::StartAlignRight) => {
                    self.push_tag(Tag::EndAlignRight);
                }
                _ => {}
            },
            _ => {}
        }
    }

    /// Whether or not the last added piece was empty
    ///
    /// This happens when an empty [`String`] or an empty [`Text`] is
    /// pushed.
    pub fn last_was_empty(&self) -> bool {
        self.last_was_empty
    }

    /// Pushes an [`impl Display`] to the [`Text`]
    ///
    /// [`impl Display`]: std::fmt::Display
    pub(crate) fn push_str(&mut self, display: &str) {
        self.buffer.clear();
        write!(self.buffer, "{display}").unwrap();
        if self.buffer.is_empty() {
            self.last_was_empty = true;
        } else {
            self.last_was_empty = false;
            let end = self.text.len();
            self.text
                .apply_change_inner(Change::str_insert(&self.buffer, end))
        }
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well
    /// as its inverse at the end of the [`Text`]
    pub(crate) fn push_tag(&mut self, tag: Tag) -> Option<ToggleId> {
        let len = self.text.len().byte();
        if let Tag::PushForm(id) = tag {
            let last_form = match id == crate::form::DEFAULT_ID {
                true => self.last_form.take(),
                false => self.last_form.replace(id),
            };

            if let Some(id) = last_form {
                self.text.0.tags.insert(len, Tag::PopForm(id), Key::basic());
            }

            match id == crate::form::DEFAULT_ID {
                true => None,
                false => self.text.0.tags.insert(len, tag, Key::basic()),
            }
        } else {
            match tag {
                Tag::StartAlignCenter => {
                    self.last_align = Some(Tag::StartAlignCenter);
                    self.text.0.tags.insert(len, tag, Key::basic())
                }
                Tag::StartAlignRight => {
                    self.last_align = Some(Tag::StartAlignRight);
                    self.text.0.tags.insert(len, tag, Key::basic())
                }
                Tag::EndAlignCenter | Tag::EndAlignRight => {
                    if self.last_align.as_ref().is_some_and(|a| a.ends_with(&tag)) {
                        self.last_align = None;
                        self.text.0.tags.insert(len, tag, Key::basic())
                    } else {
                        None
                    }
                }
                tag => self.text.0.tags.insert(len, tag, Key::basic()),
            }
        }
    }

    /// Pushes [`Text`] directly
    pub(crate) fn push_text(&mut self, mut text: Text) {
        if text.0.forced_new_line {
            let change = Change::remove_nl(text.last_point().unwrap());
            text.apply_change_inner(change);
            text.0.forced_new_line = false;
        }
        self.last_was_empty = text.is_empty();
        let end = self.text.len();

        if let Some(last_id) = self.last_form.take() {
            let initial = text.tags_fwd(0).next();
            if let Some((0, RawTag::PushForm(_, id))) = initial
                && id == last_id
            {
                text.0.tags.remove_at_if(0, |t| t.key() == Key::basic());
            } else {
                self.text
                    .0
                    .tags
                    .insert(end.byte(), Tag::PopForm(last_id), Key::basic());
            }
        }

        self.text.0.bytes.extend(text.0.bytes);
        self.text.0.tags.extend(text.0.tags);
    }
}

impl Default for Builder {
    fn default() -> Self {
        Builder {
            text: Text::empty(),
            last_form: None,
            last_align: None,
            buffer: String::with_capacity(50),
            last_was_empty: false,
        }
    }
}

/// [`Builder`] part: Aligns the line centrally
pub struct AlignCenter;
/// [`Builder`] part: Aligns the line on the left
pub struct AlignLeft;
/// [`Builder`] part: Aligns the line on the right, which is the
/// default
pub struct AlignRight;
/// [`Builder`] part: A spacer for more advanced alignment
///
/// When printing this screen line (one row on screen, i.e. until
/// it wraps), Instead of following the current alignment, will
/// put spacing between the next and previous characters. The
/// length of the space will be roughly equal to the available
/// space on this line divided by the number of [`Spacer`]s on it.
///
/// # Example
///
/// Let's say that this is the line being printed:
///
/// ```text
/// This is my line,please,pretend it has tags
/// ```
///
/// If we were to print it with `{Spacer}`s like this:
///
/// ```text
/// This is my line,{Spacer}please,{Spacer}pretend it has tags
/// ```
///
/// In a screen with a width of 50, it would come out like:
///
/// ```text
/// This is my line,    please,    pretend it has tags
/// ```
pub struct Spacer;
/// [`Builder`] part: Places ghost text
///
/// This is useful when, for example, creating command line prompts,
/// since the text is non interactable.
pub struct Ghost(pub Text);

impl From<AlignCenter> for Tag {
    fn from(_: AlignCenter) -> Self {
        Tag::StartAlignCenter
    }
}

impl From<AlignRight> for Tag {
    fn from(_: AlignRight) -> Self {
        Tag::StartAlignRight
    }
}

impl From<Spacer> for Tag {
    fn from(_: Spacer) -> Self {
        Tag::Spacer
    }
}

impl From<Ghost> for Tag {
    fn from(value: Ghost) -> Self {
        Tag::GhostText(value.0)
    }
}

/// A part to be pushed to a [`Builder`] by a macro
pub enum BuilderPart<S: ToString, _T> {
    Text(Text),
    Tag(Tag),
    ToString(S),
    OptToString(Option<S>),
    OptToTag(Option<Tag>),
    EndLastAlign(PhantomData<_T>),
}

impl<T: Into<Tag>> From<T> for BuilderPart<String, Tag> {
    fn from(value: T) -> Self {
        BuilderPart::Tag(value.into())
    }
}

impl From<AlignLeft> for BuilderPart<String, AlignLeft> {
    fn from(_: AlignLeft) -> Self {
        BuilderPart::EndLastAlign(PhantomData)
    }
}

impl From<Text> for BuilderPart<String, Text> {
    fn from(value: Text) -> Self {
        BuilderPart::Text(value)
    }
}

impl<S: ToString> From<&RwData<S>> for BuilderPart<String, S> {
    fn from(value: &RwData<S>) -> Self {
        BuilderPart::ToString(value.read().to_string())
    }
}

impl<S: ToString> From<S> for BuilderPart<S, S> {
    fn from(value: S) -> Self {
        BuilderPart::ToString(value)
    }
}

impl From<PathBuf> for BuilderPart<String, PathBuf> {
    fn from(value: PathBuf) -> Self {
        BuilderPart::Text(Text::from(&value))
    }
}

impl From<&PathBuf> for BuilderPart<String, PathBuf> {
    fn from(value: &PathBuf) -> Self {
        BuilderPart::Text(Text::from(value))
    }
}

impl From<RwData<PathBuf>> for BuilderPart<String, PathBuf> {
    fn from(value: RwData<PathBuf>) -> Self {
        BuilderPart::Text(Text::from(&*value.read()))
    }
}

impl<T: Into<Tag>> From<Option<T>> for BuilderPart<String, Option<Tag>> {
    fn from(value: Option<T>) -> Self {
        BuilderPart::OptToTag(value.map(|t| t.into()))
    }
}

/// The standard [`Text`] construction macro
///
/// TODO: Docs
pub macro text {
    // Forms
    (@push $builder:expr, []) => {
        let id = crate::form::id_of!("Default");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::form::id_of!("Accent");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$($form:tt)+]) => {
        let id = crate::form::id_of!(stringify!($($form)+));
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
        let id = crate::form::id_of!("DefaultOk");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::form::id_of!("AccentOk");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$($form:tt)+]) => {
        let id = crate::form::id_of!(stringify!($($form)+));
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
        builder.finish()
    }},
}

/// The standard [`Text`] construction macro
///
/// TODO: Docs
pub macro err {
    // Forms
    (@push $builder:expr, []) => {
        let id = crate::form::id_of!("DefaultErr");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::form::id_of!("AccentErr");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$($form:tt)+]) => {
        let id = crate::form::id_of!(stringify!($($form)+));
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
        let id = crate::form::id_of!("DefaultHint");
        $builder.push(crate::text::Tag::PushForm(id))
    },
    (@push $builder:expr, [*a]) => {
        let id = crate::form::id_of!("AccentHint");
        $builder.push(crate::text::Tag::PushForm(id))
    },

    (@push $builder:expr, [$($form:tt)+]) => {
        let id = crate::form::id_of!(stringify!($($form)+));
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
