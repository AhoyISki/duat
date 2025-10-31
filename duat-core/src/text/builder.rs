//! [`Text`] building macros
//!
//! This module defines the [`txt!`] macro, alongside its [`Builder`]
//! companion struct. This macro is very convenient for the creation
//! of [`Text`]s, since its syntax is just a superset of the natural
//! syntax of [`format_args!`], also allowing for the addition of
//! [`Form`]s through specialized arguments.
//!
//! [`Form`]: crate::form::Form
use std::{
    fmt::{Alignment, Display, Write},
    marker::PhantomData,
    path::PathBuf,
};

use super::{Change, Tagger, Text};
use crate::{
    form::FormId,
    text::{AlignCenter, AlignLeft, AlignRight, FormTag, Ghost, Spacer},
};

/// Builds and modifies a [`Text`], based on replacements applied
/// to it.
///
/// This struct is meant to be used alongside the [`txt!`] macro, as
/// you can just push more [`Text`] to the [`Builder`] py pushing
/// another [`Builder`], which can be returned by the [`txt!`] macro:
///
/// ```rust
/// # use duat_core::text::{Text, txt};
/// fn is_more_than_two(num: usize) -> Text {
///     let mut builder = Text::builder();
///     builder.push(txt!("The number [a]{num}[] is"));
///     if num > 2 {
///         builder.push(txt!("[a]more[] than 2."));
///     } else {
///         builder.push(txt!("[a]not more[] than 2."));
///     }
///     builder.build()
/// }
/// ```
///
/// In the above call, you can see that `num` was interpolated, just
/// like with [`println!`], there are also [`Form`]s being applied to
/// the [`Text`]. Each `[]` pair denotes a [`Form`]. These pairs
/// follow the following rule:
///
/// - `[]`: Will push the `"Default"` [`Form`], which is actually just
///   removing prior [`Form`]s.
/// - `[a]`: Will push the `"Accent"` [`Form`].
/// - `[{form}]`: Will push the `"{form}"`, where `{form}` can be any
///   sequence of valid identifiers, separated by `"."`s.
///
/// [`impl Display`]: std::fmt::Display
/// [tag]: AlignCenter
/// [`Form`]: crate::form::Form
#[derive(Clone)]
pub struct Builder {
    text: Text,
    last_form: Option<(usize, FormTag)>,
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
    /// the [builder], as well as pushing a `'\n'` at the end, much
    /// like with regular [`Text`] construction.
    ///
    /// [`Form`]: crate::form::Form
    /// [builder]: Builder
    /// [`Builder::into::<Text>`]: Into::into
    /// [`Widget`]: crate::ui::Widget
    /// [`StatusLine`]: https://docs.rs/duat/latest/duat/widgets/struct.StatusLine.html
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
    /// [ghosts]: super::Ghost
    fn build_no_nl(mut self) -> Text {
        if let Some((b, id)) = self.last_form
            && b < self.text.len().byte()
        {
            self.text.insert_tag(Tagger::basic(), b.., id);
        }
        if let Some((b, align)) = self.last_align
            && b < self.text.len().byte()
        {
            match align {
                Alignment::Center => {
                    self.text.insert_tag(Tagger::basic(), b.., AlignCenter);
                }
                Alignment::Right => {
                    self.text.insert_tag(Tagger::basic(), b.., AlignRight);
                }
                _ => {}
            }
        }

        self.text
    }

    /// Pushes a part of the text
    ///
    /// This can be an [`impl Display`] type, an [`impl Tag`], a
    /// [`FormId`], a [`PathBuf`], or even [`std::process::Output`].
    ///
    /// [`impl Display`]: std::fmt::Display
    /// [`impl Tag`]: super::Tag
    pub fn push<D: Display, _T>(&mut self, part: impl Into<BuilderPart<D, _T>>) {
        self.push_builder_part(part.into());
    }

    #[doc(hidden)]
    pub fn push_builder_part<_T>(&mut self, part: BuilderPart<impl Display, _T>) {
        fn push_basic(builder: &mut Builder, part: BuilderPart) {
            use Alignment::*;
            use BuilderPart as BP;

            let end = builder.text.len().byte();
            match part {
                BP::Text(text) => builder.push_text(text.without_last_nl()),
                BP::Builder(new) => builder.push_text(new.build_no_nl()),
                BP::Form(tag) => {
                    let last_form = if tag == crate::form::DEFAULT_ID.to_tag(0) {
                        builder.last_form.take()
                    } else {
                        builder.last_form.replace((end, tag))
                    };

                    if let Some((b, tag)) = last_form
                        && b < end
                    {
                        builder.text.insert_tag(Tagger::basic(), b.., tag);
                    }
                }
                BP::AlignLeft => match builder.last_align.take() {
                    Some((b, Center)) if b < end => {
                        builder.text.insert_tag(Tagger::basic(), b.., AlignCenter);
                    }
                    Some((b, Right)) if b < end => {
                        builder.text.insert_tag(Tagger::basic(), b.., AlignRight);
                    }
                    _ => {}
                },
                BP::AlignCenter => match builder.last_align.take() {
                    Some((b, Center)) => builder.last_align = Some((b, Center)),
                    Some((b, Right)) if b < end => {
                        builder.text.insert_tag(Tagger::basic(), b.., AlignRight);
                        builder.last_align = Some((end, Center));
                    }
                    None => builder.last_align = Some((end, Center)),
                    Some(_) => {}
                },
                BP::AlignRight => match builder.last_align.take() {
                    Some((b, Right)) => builder.last_align = Some((b, Right)),
                    Some((b, Center)) if b < end => {
                        builder.text.insert_tag(Tagger::basic(), b.., AlignCenter);
                        builder.last_align = Some((end, Right));
                    }
                    None => builder.last_align = Some((end, Right)),
                    Some(_) => {}
                },
                BP::Spacer(_) => {
                    builder.text.insert_tag(Tagger::basic(), end, Spacer);
                }
                BP::Ghost(text) => {
                    builder.text.insert_tag(Tagger::basic(), end, Ghost(text));
                }
                BP::ToString(_) => unsafe { std::hint::unreachable_unchecked() },
            }
        }

        match part.try_to_basic() {
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
        self.text.insert_text(self.text.len(), text);
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
    /// Text to be pushed
    ///
    /// > [!NOTE]
    /// >
    /// > Every [`Text`] struct has a `\n` attached at the end,
    /// > but when pushing it to a [`Builder`], said `\n` is
    /// > automatically removed. If you want to keep a `\n` at the
    /// > end, push an additional one.
    Text(Text),
    /// A Text Builder
    ///
    /// Much like the [`Text`], normally, the [`Builder`] finishes
    /// with a `\n`, but when pushed to another [`Builder`], that `\n`
    /// is removed as well.
    Builder(Builder),
    /// An [`impl Display`](std::fmt::Display) type
    ToString(D),
    /// A [`FormId`]
    Form(FormTag),
    /// Sets the alignment to the left, i.e. resets it
    AlignLeft,
    /// Sets the alignment to the center
    AlignCenter,
    /// Sets the alignment to the right
    AlignRight,
    /// A spacer for more advanced alignment
    Spacer(PhantomData<_T>),
    /// Ghost [`Text`] that is separate from the real thing
    Ghost(Text),
}

impl<D: Display, _T> BuilderPart<D, _T> {
    fn try_to_basic(self) -> Result<BuilderPart, Self> {
        match self {
            BuilderPart::Text(text) => Ok(BuilderPart::Text(text)),
            BuilderPart::Builder(builder) => Ok(BuilderPart::Builder(builder)),
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
        Self::Builder(value)
    }
}

impl From<FormId> for BuilderPart {
    fn from(value: FormId) -> Self {
        Self::Form(value.to_tag(0))
    }
}

impl From<FormTag> for BuilderPart {
    fn from(value: FormTag) -> Self {
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
    fn from(value: Text) -> Self {
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

impl<'a> From<&'a PathBuf> for BuilderPart<std::borrow::Cow<'a, str>> {
    fn from(value: &'a PathBuf) -> Self {
        BuilderPart::ToString(value.to_string_lossy())
    }
}

impl<'a> From<&'a std::path::Path> for BuilderPart<std::borrow::Cow<'a, str>> {
    fn from(value: &'a std::path::Path) -> Self {
        BuilderPart::ToString(value.to_string_lossy())
    }
}

/// The standard [`Text`] construction macro
///
/// TODO: Docs
///
/// [`Text`]: super::Text
pub macro txt($($parts:tt)+) {{
    #[allow(unused_imports)]
    use $crate::private_exports::{format_like, parse_arg, parse_form, parse_str};

    let mut builder = $crate::text::Builder::new();

    format_like!(
        parse_str,
        [('{', parse_arg, false), ('[', parse_form, true)],
        &mut builder,
        $($parts)*
    );

    builder
}}
