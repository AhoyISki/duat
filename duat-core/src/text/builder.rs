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
    path::Path,
};

use super::{Change, Tagger, Text};
use crate::{
    buffer::PathKind,
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
/// - `[]`: Will push the `"default"` [`Form`], which is actually just
///   removing prior [`Form`]s.
/// - `[a]`: Will push the `"accent"` [`Form`].
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
    /// Wether to collapse `" "`s after an empty element is pushed
    pub no_space_after_empty: bool,
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
        if let Some((b, id)) = self.last_form
            && b < self.text.last_point().byte()
        {
            self.text.insert_tag(Tagger::basic(), b.., id);
        }
        if let Some((b, align)) = self.last_align
            && b < self.text.last_point().byte()
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

    /// Builds the [`Text`], removing any double `\n`s at the end
    ///
    /// When building multi-line `Text`, it is common to just insert
    /// every piece of `Text` with a `\n` at the end. This ends up
    /// resulting in a `Text` that has two `\n`s at the end, since
    /// there is always at least one final `\n`.
    ///
    /// This function lets you construct the [`Text`] taking that
    /// effect into account.
    pub fn build_no_double_nl(self) -> Text {
        let mut text = self.build();
        if let Some(last_last_byte) = text.len().byte().checked_sub(2)
            && let Some(strs) = text.strs(last_last_byte..)
            && strs == "\n\n"
        {
            text.replace_range(last_last_byte..last_last_byte + 1, "");
        }

        text
    }

    /// Pushes a part of the text
    ///
    /// This can be an [`impl Display`] type, an [`impl Tag`], a
    /// [`FormId`], a [`PathBuf`], or even [`std::process::Output`].
    ///
    /// [`impl Display`]: std::fmt::Display
    /// [`impl Tag`]: super::Tag
    pub fn push<D: Display, _T>(&mut self, part: impl AsBuilderPart<D, _T>) {
        self.push_builder_part(part.as_builder_part());
    }

    #[doc(hidden)]
    pub fn push_builder_part<_T>(&mut self, part: BuilderPart<impl Display, _T>) {
        fn push_simple(builder: &mut Builder, part: BuilderPart) {
            use Alignment::*;
            use BuilderPart as BP;

            let end = builder.text.last_point().byte();
            let tagger = Tagger::basic();

            match part {
                BP::Text(text) => builder.push_text(text),
                BP::Builder(other) => builder.push_builder(other),
                BP::Path(path) => builder.push_str(path.to_string_lossy()),
                BP::PathKind(text) => builder.push_text(&text),
                BP::Form(tag) => {
                    let last_form = if tag == crate::form::DEFAULT_ID.to_tag(0) {
                        builder.last_form.take()
                    } else {
                        builder.last_form.replace((end, tag))
                    };

                    if let Some((b, tag)) = last_form
                        && b < end
                    {
                        builder.text.insert_tag(tagger, b..end, tag);
                    }
                }
                BP::AlignLeft => match builder.last_align.take() {
                    Some((b, Center)) if b < end => {
                        builder.text.insert_tag(tagger, b..end, AlignCenter);
                    }
                    Some((b, Right)) if b < end => {
                        builder.text.insert_tag(tagger, b..end, AlignRight);
                    }
                    _ => {}
                },
                BP::AlignCenter => match builder.last_align.take() {
                    Some((b, Center)) => builder.last_align = Some((b, Center)),
                    Some((b, Right)) if b < end => {
                        builder.text.insert_tag(tagger, b..end, AlignRight);
                        builder.last_align = Some((end, Center));
                    }
                    None => builder.last_align = Some((end, Center)),
                    Some(_) => {}
                },
                BP::AlignRight => match builder.last_align.take() {
                    Some((b, Right)) => builder.last_align = Some((b, Right)),
                    Some((b, Center)) if b < end => {
                        builder.text.insert_tag(tagger, b..end, AlignCenter);
                        builder.last_align = Some((end, Right));
                    }
                    None => builder.last_align = Some((end, Right)),
                    Some(_) => {}
                },
                BP::Spacer(_) => _ = builder.text.insert_tag(tagger, end, Spacer),
                BP::Ghost(ghost) => _ = builder.text.insert_tag(tagger, end, ghost.clone()),
                BP::ToString(_) => unsafe { std::hint::unreachable_unchecked() },
            }
        }

        match part.try_to_basic() {
            Ok(part_ref) => push_simple(self, part_ref),
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
    /// If `builder.no_space_after_empty` is set to `true` and
    /// the argument is equal to `" "`, then it won't be added if
    /// the previous argument was empty. This is useful especially
    /// in situations where you expect to be constructing a `Text`
    /// with spaces in between the elements (like in a status line),
    /// but you don't want an empty element to just leave to space
    /// wide gap in between two non empty elements.
    ///
    /// [`impl Display`]: std::fmt::Display
    pub fn push_str<D: Display>(&mut self, d: D) {
        self.buffer.clear();
        write!(self.buffer, "{d}").unwrap();
        if self.buffer.is_empty()
            || (self.no_space_after_empty && self.buffer == " " && self.last_was_empty)
        {
            self.last_was_empty = true;
        } else {
            self.last_was_empty = false;
            let end = self.text.last_point();
            self.text
                .apply_change(0, Change::str_insert(&self.buffer, end));
        }
    }

    /// Resets the [`Form`]
    ///
    /// This is equivalent to pushing the `default` `Form`.
    ///
    /// [`Form`]: crate::form::Form
    pub fn reset_form(&mut self) {
        let end = self.text.last_point().byte();
        if let Some((b, last_form)) = self.last_form.take() {
            self.text.insert_tag(Tagger::basic(), b..end, last_form);
        }
    }

    /// Resets the [`Form`]
    ///
    /// This is equivalent to pushing the `default` `Form`.
    ///
    /// [`Form`]: crate::form::Form
    pub fn reset_alignment(&mut self) {
        let end = self.text.last_point().byte();
        match self.last_align.take() {
            Some((b, Alignment::Center)) if b < end => {
                self.text.insert_tag(Tagger::basic(), b..end, AlignCenter);
            }
            Some((b, Alignment::Right)) if b < end => {
                self.text.insert_tag(Tagger::basic(), b..end, AlignRight);
            }
            _ => {}
        }
    }

    /// Pushes [`Text`] directly
    fn push_text(&mut self, text: &Text) {
        self.last_was_empty = text.is_empty();
        self.text.insert_text(self.text.last_point(), text);
    }

    /// Pushes [`Text`] directly
    fn push_builder(&mut self, other: &Builder) {
        self.last_was_empty = other.text.is_empty();

        let offset = self.text.last_point().byte();
        self.text.insert_text(offset, &other.text);
        let end = self.text.last_point().byte();

        if let Some((b, id)) = other.last_form
            && b < other.text.last_point().byte()
        {
            self.text.insert_tag(Tagger::basic(), offset + b..end, id);
        }
        if let Some((b, align)) = other.last_align
            && b < other.text.last_point().byte()
        {
            let tagger = Tagger::basic();
            if let Alignment::Center = align {
                self.text.insert_tag(tagger, offset + b..end, AlignCenter);
            } else {
                self.text.insert_tag(tagger, offset + b..end, AlignRight);
            }
        }
    }
}

impl Default for Builder {
    fn default() -> Self {
        Builder {
            text: Text::new(),
            last_form: None,
            last_align: None,
            buffer: String::with_capacity(50),
            last_was_empty: false,
            no_space_after_empty: false,
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
pub enum BuilderPart<'a, D: Display = String, _T = ()> {
    /// Text to be pushed
    ///
    /// # Note
    ///
    /// Every [`Text`] struct has a `\n` attached at the end,
    /// but when pushing it to a [`Builder`], said `\n` is
    /// automatically removed. If you want to keep a `\n` at the
    /// end, push an additional one.
    Text(&'a Text),
    /// A Text Builder
    ///
    /// Much like the [`Text`], normally, the [`Builder`] finishes
    /// with a `\n`, but when pushed to another [`Builder`], that `\n`
    /// is removed as well.
    Builder(&'a Builder),
    /// An [`impl Display`](std::fmt::Display) type
    ToString(&'a D),
    /// An [`Path`] reference
    Path(&'a std::path::Path),
    /// A [`PathKind`]
    PathKind(Text),
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
    Ghost(&'a Ghost),
}

impl<'a, D: Display, _T> BuilderPart<'a, D, _T> {
    fn try_to_basic(self) -> Result<BuilderPart<'a>, Self> {
        match self {
            BuilderPart::Text(text) => Ok(BuilderPart::Text(text)),
            BuilderPart::Builder(builder) => Ok(BuilderPart::Builder(builder)),
            BuilderPart::ToString(_) => Err(self),
            BuilderPart::Path(path) => Ok(BuilderPart::Path(path)),
            BuilderPart::PathKind(text) => Ok(BuilderPart::PathKind(text)),
            BuilderPart::Form(form_id) => Ok(BuilderPart::Form(form_id)),
            BuilderPart::AlignLeft => Ok(BuilderPart::AlignLeft),
            BuilderPart::AlignCenter => Ok(BuilderPart::AlignCenter),
            BuilderPart::AlignRight => Ok(BuilderPart::AlignRight),
            BuilderPart::Spacer(_) => Ok(BuilderPart::Spacer(PhantomData)),
            BuilderPart::Ghost(ghost) => Ok(BuilderPart::Ghost(ghost)),
        }
    }
}

/// Defines which types can be pushed onto a [`Builder`]
///
/// Every [`Tag`], as well as any [`Display`] and [`AsRef<Path>`]
/// types can be pushed to a `Builder`.
///
/// [`Tag`]: super::Tag
pub trait AsBuilderPart<D: Display = String, _T = ()> {
    /// Gets a [`BuilderPart`] fro this value
    fn as_builder_part(&self) -> BuilderPart<'_, D, _T>;
}

macro_rules! implAsBuilderPart {
    ($type:ident, $value:ident, $result:expr) => {
        impl AsBuilderPart for $type {
            fn as_builder_part(&self) -> BuilderPart<'_> {
                let $value = self;
                $result
            }
        }
    };
}

implAsBuilderPart!(Builder, builder, BuilderPart::Builder(builder));
implAsBuilderPart!(FormId, form_id, BuilderPart::Form(form_id.to_tag(0)));
implAsBuilderPart!(FormTag, form_tag, BuilderPart::Form(*form_tag));
implAsBuilderPart!(AlignLeft, _align, BuilderPart::AlignLeft);
implAsBuilderPart!(AlignCenter, _align, BuilderPart::AlignCenter);
implAsBuilderPart!(AlignRight, _align, BuilderPart::AlignRight);
implAsBuilderPart!(Spacer, _spacer, BuilderPart::Spacer(PhantomData));
implAsBuilderPart!(Ghost, ghost, BuilderPart::Ghost(ghost));
implAsBuilderPart!(Text, text, BuilderPart::Text(text));
implAsBuilderPart!(Path, path, BuilderPart::Path(path));
implAsBuilderPart!(PathKind, path, BuilderPart::PathKind(path.name_txt()));

impl<D: Display> AsBuilderPart<D, D> for D {
    fn as_builder_part(&self) -> BuilderPart<'_, D, D> {
        BuilderPart::ToString(self)
    }
}

/// The standard [`Text`] construction macro
///
/// TODO: Docs
///
/// [`Text`]: super::Text
#[macro_export]
#[doc(hidden)]
macro_rules! __txt__ {
    ($($parts:tt)+) => {{
        #[allow(unused_imports)]
        use $crate::{
            __parse_arg__, __parse_form__, __parse_str__, private_exports::format_like
        };

        let mut builder = $crate::text::Builder::new();
        let _ = format_like!(
            __parse_str__,
            [('{', __parse_arg__, false), ('[', __parse_form__, true)],
            &mut builder,
            $($parts)*
        );

        builder.build()
    }};
}

#[macro_export]
#[doc(hidden)]
macro_rules! __log__ {
    ($lvl:expr, $($arg:tt)*) => {{
        #[allow(unused_must_use)]
        let text = $crate::text::txt!($($arg)*);

        $crate::context::logs().push_record($crate::context::Record::new(
            text,
            $lvl,
        ));
    }}
}

#[macro_export]
#[doc(hidden)]
macro_rules! __parse_str__ {
    ($builder:expr, $str:literal) => {{
        let builder = $builder;
        builder.push_str($str);
        builder
    }};
}

#[macro_export]
#[doc(hidden)]
macro_rules! __parse_arg__ {
    ($builder:expr,"", $arg:expr) => {{
        use $crate::text::AsBuilderPart;
        let builder = $builder;
        builder.push_builder_part($arg.as_builder_part());
        builder
    }};
    ($builder:expr, $modif:literal, $arg:expr) => {{
        let builder = $builder;
        builder.push_str(format!(concat!("{:", $modif, "}"), &$arg));
        builder
    }};
}

#[macro_export]
#[doc(hidden)]
macro_rules! __parse_form__ {
    ($builder:expr, $priority:literal,) => {{
        use $crate::text::AsBuilderPart;
        const PRIORITY: u8 = $crate::priority($priority);
        let builder = $builder;
        let id = $crate::form::DEFAULT_ID;
        builder.push_builder_part(id.to_tag(PRIORITY).as_builder_part());
        builder
    }};
    ($builder:expr, $priority:literal, a) => {{
        use $crate::text::AsBuilderPart;
        const PRIORITY: u8 = $crate::priority($priority);
        let builder = $builder;
        let id = $crate::form::ACCENT_ID;
        builder.push_builder_part(id.to_tag(PRIORITY).as_builder_part());
        builder
    }};
    ($builder:expr, $priority:literal, $($form:tt)*) => {{
        use $crate::text::AsBuilderPart;
        const PRIORITY: u8 = $crate::priority($priority);
        let builder = $builder;
        let id = $crate::form::id_of!(concat!($(stringify!($form)),*));
        builder.push_builder_part(id.to_tag(PRIORITY).as_builder_part());
        builder
    }};
}
