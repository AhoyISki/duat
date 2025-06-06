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
use super::{Change, Key, Text};
use crate::{
    form::FormId,
    text::{AlignCenter, AlignLeft, AlignRight, Ghost, Spacer},
};

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
#[derive(Clone)]
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
    /// the [builder], as well as pushing a `'\n'` at the end if there
    /// is none, much like with regular [`Text`] construction.
    ///
    /// In general, you should only ever call this method (or
    /// [`Builder::into::<Text>`]) if you actually _need_ the finished
    /// [`Text`], and that will only really happen when finishing a
    /// [`Text`] that should show up in a [`Widget`].
    ///
    /// The main reason for this is that, while [`Text`] doesn't
    /// implement [`Send`] or [`Sync`], [`Builder`] _does_, so you
    /// can, for example, [notify] messages to Duat using them.
    ///
    /// One misconception might be that formatting functions (such as
    /// those used in the [`StatusLine`]) seem like they should return
    /// [`Text`], but no, they should _actually_ return [`Builder`],
    /// since it is more composable into a finalized [`Text`].
    ///
    /// [`Form`]: crate::form::Form
    /// [builder]: Builder
    /// [`Builder::into::<Text>`]: Builder::into
    /// [`Widget`]: crate::widgets::Widget
    /// [notify]: crate::context::notify
    pub fn build(mut self) -> Text {
        self.push_str("\n");
        self.build_no_nl()
    }

    /// Builds the [`Text`] without adding a `'\n'` at the end
    ///
    /// This should only be used when adding [`Text`] to another
    /// [`Builder`], as a `'\n'` should only be added at the end of
    /// [`Text`]s, or when creating a [ghosts], which don't end in a
    /// `'\n'`, since they are placed in the middle of another
    /// [`Text`], much like [`Text`]s added to a [`Builder`]
    ///
    /// [ghosts]: super::Tag::ghost
    pub(super) fn build_no_nl(mut self) -> Text {
        if let Some((b, id)) = self.last_form
            && b < self.text.len().byte()
        {
            self.text.insert_tag(Key::basic(), b.., id.to_tag(0));
        }
        if let Some((b, align)) = self.last_align
            && b < self.text.len().byte()
        {
            match align {
                Alignment::Center => self.text.insert_tag(Key::basic(), b.., AlignCenter),
                Alignment::Right => self.text.insert_tag(Key::basic(), b.., AlignRight),
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
        fn push_basic(builder: &mut Builder, part: BuilderPart) {
            use Alignment::*;
            use BuilderPart as BP;

            let end = builder.text.len().byte();
            match part {
                BP::Text(text) => builder.push_text(text),
                BP::Form(id) => {
                    let last_form = match id == crate::form::DEFAULT_ID {
                        true => builder.last_form.take(),
                        false => builder.last_form.replace((end, id)),
                    };

                    if let Some((b, id)) = last_form
                        && b < end
                    {
                        builder.text.insert_tag(Key::basic(), b.., id.to_tag(0));
                    }
                }
                BP::AlignLeft => match builder.last_align.take() {
                    Some((b, Center)) if b < end => {
                        builder.text.insert_tag(Key::basic(), b.., AlignCenter);
                    }
                    Some((b, Right)) if b < end => {
                        builder.text.insert_tag(Key::basic(), b.., AlignRight);
                    }
                    _ => {}
                },
                BP::AlignCenter => match builder.last_align.take() {
                    Some((b, Center)) => builder.last_align = Some((b, Center)),
                    Some((b, Right)) if b < end => {
                        builder.text.insert_tag(Key::basic(), b.., AlignRight);
                    }
                    None => builder.last_align = Some((end, Center)),
                    Some(_) => {}
                },
                BP::AlignRight => match builder.last_align.take() {
                    Some((b, Right)) => builder.last_align = Some((b, Right)),
                    Some((b, Center)) if b < end => {
                        builder.text.insert_tag(Key::basic(), b.., AlignCenter);
                    }
                    None => builder.last_align = Some((end, Right)),
                    Some(_) => {}
                },
                BP::Spacer(_) => builder.text.insert_tag(Key::basic(), end, Spacer),
                BP::Ghost(text) => builder.text.insert_tag(Key::basic(), end, Ghost(text)),
                BP::ToString(_) => unsafe { std::hint::unreachable_unchecked() },
            }
        }

        match Into::<BuilderPart<D, _T>>::into(part).try_to_basic() {
            Ok(basic_part) => push_basic(self, basic_part),
            Err(BuilderPart::ToString(display)) => self.push_str(display),
            Err(_) => unsafe { std::hint::unreachable_unchecked() },
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
    fn push_text(&mut self, text: Text) {
        self.last_was_empty = text.is_empty();

        if let Some((b, id)) = self.last_form.take() {
            self.text.insert_tag(Key::basic(), b.., id.to_tag(0));
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

impl std::fmt::Debug for Builder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Builder")
            .field("text", &self.text)
            .finish_non_exhaustive()
    }
}

impl From<Builder> for Text {
    fn from(value: Builder) -> Self {
        value.build()
    }
}

/// A part to be pushed to a [`Builder`] by a macro
#[derive(Clone)]
pub enum BuilderPart<D: Display = String, _T = ()> {
    Text(Text),
    ToString(D),
    Form(FormId),
    AlignLeft,
    AlignCenter,
    AlignRight,
    Spacer(PhantomData<_T>),
    Ghost(Text),
}

impl<D: Display, _T> BuilderPart<D, _T> {
    fn try_to_basic(self) -> Result<BuilderPart, Self> {
        match self {
            BuilderPart::Text(text) => Ok(BuilderPart::Text(text)),
            BuilderPart::ToString(d) => Err(BuilderPart::ToString(d)),
            BuilderPart::Form(form_id) => Ok(BuilderPart::Form(form_id)),
            BuilderPart::AlignLeft => Ok(BuilderPart::AlignLeft),
            BuilderPart::AlignCenter => Ok(BuilderPart::AlignCenter),
            BuilderPart::AlignRight => Ok(BuilderPart::AlignRight),
            BuilderPart::Spacer(_) => Ok(BuilderPart::Spacer(PhantomData)),
            BuilderPart::Ghost(text) => Ok(BuilderPart::Ghost(text)),
        }
    }
}

impl From<Builder> for BuilderPart {
    fn from(value: Builder) -> Self {
        Self::Text(value.build_no_nl())
    }
}

impl From<FormId> for BuilderPart {
    fn from(value: FormId) -> Self {
        Self::Form(value)
    }
}

impl From<AlignLeft> for BuilderPart {
    fn from(_: AlignLeft) -> Self {
        BuilderPart::AlignLeft
    }
}

impl From<AlignCenter> for BuilderPart {
    fn from(_: AlignCenter) -> Self {
        BuilderPart::AlignCenter
    }
}

impl From<AlignRight> for BuilderPart {
    fn from(_: AlignRight) -> Self {
        BuilderPart::AlignRight
    }
}

impl From<Spacer> for BuilderPart {
    fn from(_: Spacer) -> Self {
        BuilderPart::Spacer(PhantomData)
    }
}

impl<T: Into<Text>> From<Ghost<T>> for BuilderPart {
    fn from(value: Ghost<T>) -> Self {
        BuilderPart::Ghost(value.0.into())
    }
}

impl From<Text> for BuilderPart {
    fn from(mut value: Text) -> Self {
        value.replace_range(value.len().byte() - 1.., "");
        BuilderPart::Text(value)
    }
}

impl<D: Display> From<D> for BuilderPart<D, D> {
    fn from(value: D) -> Self {
        BuilderPart::ToString(value)
    }
}

impl From<PathBuf> for BuilderPart {
    fn from(value: PathBuf) -> Self {
        BuilderPart::ToString(value.to_string_lossy().to_string())
    }
}

impl From<&PathBuf> for BuilderPart {
    fn from(value: &PathBuf) -> Self {
        BuilderPart::ToString(value.to_string_lossy().to_string())
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
        builder
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
        builder
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
        builder
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
        builder
    }}
}
