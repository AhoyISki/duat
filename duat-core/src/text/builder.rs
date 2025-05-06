//! [`Text`] building macros
//!
//! This module contains 4 macros for text building: [`text!`],
//! [`err!`], [`ok!`] and [`hint!`]. These are supposed to be used in
//! various contexts, and they have differences on what the `Default`
//! and `Accent` form.
use std::{
    fmt::{Alignment, Display, Write},
    marker::PhantomData,
    path::PathBuf,
};

pub use self::macros::*;
use super::{Change, Key, Tag, Text};
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
    last_form: Option<(usize, FormId)>,
    last_align: Option<(usize, Alignment)>,
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
    /// Will also finish the last [`Form`] and alignments pushed to
    /// the [builder].
    ///
    /// [`Form`]: crate::form::Form
    /// [builder]: Builder
    pub fn finish(mut self) -> Text {
        if (self.text.buffers(..).next_back()).is_none_or(|b| b != b'\n') {
            self.push_str("\n");
            self.text.0.forced_new_line = true;
        }

        if let Some((b, id)) = self.last_form
            && b < self.text.len().byte()
        {
            self.text.insert_tag(Key::basic(), Tag::form(b.., id, 0));
        }
        if let Some((b, align)) = self.last_align
            && b < self.text.len().byte()
        {
            match align {
                Alignment::Center => self.text.insert_tag(Key::basic(), Tag::align_center(b..)),
                Alignment::Right => self.text.insert_tag(Key::basic(), Tag::align_right(b..)),
                _ => {}
            }
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
    pub fn push<D: Display, _T>(&mut self, part: impl Into<BuilderPart<D, _T>>) {
        let part = part.into();
        let end = self.text.len().byte();
        match part {
            BuilderPart::Text(text) => self.push_text(text),
            BuilderPart::ToString(display) => self.push_str(display),
            BuilderPart::Form(id) => {
                let last_form = match id == crate::form::DEFAULT_ID {
                    true => self.last_form.take(),
                    false => self.last_form.replace((end, id)),
                };

                if let Some((b, id)) = last_form
                    && b < end
                {
                    self.text.insert_tag(Key::basic(), Tag::form(b.., id, 0));
                }
            }
            BuilderPart::AlignLeft => match self.last_align.take() {
                Some((b, Alignment::Center)) if b < end => {
                    self.text.insert_tag(Key::basic(), Tag::align_center(b..));
                }
                Some((b, Alignment::Right)) if b < end => {
                    self.text.insert_tag(Key::basic(), Tag::align_right(b..));
                }
                _ => {}
            },
            BuilderPart::AlignCenter => match self.last_align.take() {
                Some((b, Alignment::Center)) => self.last_align = Some((b, Alignment::Center)),
                Some((b, Alignment::Right)) if b < end => {
                    self.text.insert_tag(Key::basic(), Tag::align_right(b..));
                }
                None => self.last_align = Some((end, Alignment::Center)),
                Some(_) => {}
            },
            BuilderPart::AlignRight => match self.last_align.take() {
                Some((b, Alignment::Right)) => self.last_align = Some((b, Alignment::Right)),
                Some((b, Alignment::Center)) if b < end => {
                    self.text.insert_tag(Key::basic(), Tag::align_center(b..));
                }
                None => self.last_align = Some((end, Alignment::Right)),
                Some(_) => {}
            },
            BuilderPart::Spacer(_) => self.text.insert_tag(Key::basic(), Tag::spacer(end)),
            BuilderPart::Ghost(text) => self.text.insert_tag(Key::basic(), Tag::ghost(end, text)),
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
    pub fn push_str<D: Display>(&mut self, d: D) {
        self.buffer.clear();
        write!(self.buffer, "{d}").unwrap();
        if self.buffer.is_empty() {
            self.last_was_empty = true;
        } else {
            self.last_was_empty = false;
            let end = self.text.len();
            self.text
                .apply_change_inner(0, Change::str_insert(&self.buffer, end));
        }
    }

    /// Pushes [`Text`] directly
    pub(crate) fn push_text(&mut self, mut text: Text) {
        if text.0.forced_new_line {
            let change = Change::remove_nl(text.last_point().unwrap());
            text.apply_change_inner(0, change);
            text.0.forced_new_line = false;
        }
        self.last_was_empty = text.is_empty();

        if let Some((b, id)) = self.last_form.take() {
            self.text.insert_tag(Key::basic(), Tag::form(b.., id, 0));
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
#[derive(Clone)]
pub struct AlignCenter;
/// [`Builder`] part: Aligns the line on the left
#[derive(Clone)]
pub struct AlignLeft;
/// [`Builder`] part: Aligns the line on the right, which is the
/// default
#[derive(Clone)]
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
#[derive(Clone)]
pub struct Spacer;
/// [`Builder`] part: Places ghost text
///
/// This is useful when, for example, creating command line prompts,
/// since the text is non interactable.
#[derive(Clone)]
pub struct Ghost(pub Text);

/// A part to be pushed to a [`Builder`] by a macro
#[derive(Clone)]
pub enum BuilderPart<D: Display, _T = String> {
    Text(Text),
    ToString(D),
    Form(FormId),
    AlignLeft,
    AlignCenter,
    AlignRight,
    Spacer(PhantomData<_T>),
    Ghost(Text),
}

impl From<FormId> for BuilderPart<String, FormId> {
    fn from(value: FormId) -> Self {
        Self::Form(value)
    }
}

impl From<AlignLeft> for BuilderPart<String, AlignLeft> {
    fn from(_: AlignLeft) -> Self {
        BuilderPart::AlignLeft
    }
}

impl From<AlignCenter> for BuilderPart<String, AlignCenter> {
    fn from(_: AlignCenter) -> Self {
        BuilderPart::AlignCenter
    }
}

impl From<AlignRight> for BuilderPart<String, AlignRight> {
    fn from(_: AlignRight) -> Self {
        BuilderPart::AlignRight
    }
}

impl From<Spacer> for BuilderPart<String, Spacer> {
    fn from(_: Spacer) -> Self {
        BuilderPart::Spacer(PhantomData)
    }
}

impl From<Ghost> for BuilderPart<String, Ghost> {
    fn from(value: Ghost) -> Self {
        BuilderPart::Ghost(value.0)
    }
}

impl From<Text> for BuilderPart<String, Text> {
    fn from(value: Text) -> Self {
        BuilderPart::Text(value)
    }
}

impl<D: Display> From<&RwData<D>> for BuilderPart<String, D> {
    fn from(value: &RwData<D>) -> Self {
        BuilderPart::ToString(value.read().to_string())
    }
}

impl<D: Display> From<D> for BuilderPart<D, D> {
    fn from(value: D) -> Self {
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

mod macros {

    /// The standard [`Text`] construction macro
    ///
    /// TODO: Docs
    ///
    /// [`Text`]: super::Text
    pub macro text($($parts:tt)+) {{
        use $crate::{form::id_of, private_exports::inner_text};
        let (default, accent) = (id_of!("Default"), id_of!("Accent"));

        let mut builder = $crate::text::Builder::new();
        inner_text!(&mut builder, default, accent, $($parts)+);
        builder.finish()
    }}

    /// Allows you to add more [`Text`] to a [`Builder`]
    ///
    /// [`Text`]: super::Text
    /// [`Builder`]: super::Builder
    pub macro add_text($builder:expr, $($parts:tt)+) {{
        use $crate::{form::id_of, private_exports::inner_text};
        let (default, accent) = (id_of!("Default"), id_of!("Accent"));

        let builder: &mut $crate::text::Builder = &mut $builder;
        inner_text!(builder, default, accent, $($parts)+);
    }}

    /// Builds a [`Text`] for [`Ok`] results from commands
    ///
    /// TODO: Docs
    ///
    /// [`Text`]: super::Text
    pub macro ok($($parts:tt)+) {{
        use $crate::{form::id_of, private_exports::inner_text};
        let (default, accent) = (id_of!("DefaultOk"), id_of!("AccentOk"));

        let mut builder = $crate::text::Builder::new();
        builder.push(default);
        inner_text!(&mut builder, default, accent, $($parts)+);
        builder.finish()
    }}

    /// Builds a [`Text`] for [`Err`] results from commands
    ///
    /// TODO: Docs
    ///
    /// [`Text`]: super::Text
    pub macro err($($parts:tt)+) {{
        use $crate::{form::id_of, private_exports::inner_text};
        let (default, accent) = (id_of!("DefaultErr"), id_of!("AccentErr"));

        let mut builder = $crate::text::Builder::new();
        builder.push(default);
        inner_text!(&mut builder, default, accent, $($parts)+);
        builder.finish()
    }}

    /// Builds a [`Text`] for hints
    ///
    /// TODO: Docs
    ///
    /// [`Text`]: super::Text
    pub macro hint($($parts:tt)+) {{
        use $crate::{form::id_of, private_exports::inner_text};
        let (default, accent) = (id_of!("DefaultHint"), id_of!("AccentHint"));

        let mut builder = $crate::text::Builder::new();
        builder.push(default);
        inner_text!(&mut builder, default, accent, $($parts)+);
        builder.finish()
    }}
}
