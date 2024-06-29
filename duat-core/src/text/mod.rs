mod cfg;
mod iter;
pub mod reader;
mod tags;
mod types;

use std::{
    fmt::{Display, Write},
    io::Read,
    ops::Range,
    path::Path,
    str::from_utf8_unchecked,
    sync::LazyLock,
};

use gapbuf::GapBuffer;
use parking_lot::Mutex;

pub(crate) use self::iter::Positional;
use self::tags::{Markers, RawTag, TagOrSkip, Tags};
pub use self::{
    cfg::*,
    iter::{ExactPos, Item, Iter, RevIter},
    tags::{Marker, Tag, ToggleId},
    types::Part,
};
use crate::{
    data::{RoData, RwData},
    history::Change,
    input::Cursors,
    palette::{self, FormId},
};

static PANIC_LOG: LazyLock<Mutex<String>> = LazyLock::new(|| Mutex::new(String::new()));

/// The text in a given area.
#[derive(Debug, Default, Clone, Eq)]
pub struct Text {
    buf: Box<GapBuffer<u8>>,
    pub tags: Box<Tags>,
    /// This [`Marker`] is used for the addition and removal of cursor
    /// [`Tag`]s.
    marker: Marker,
}

impl PartialEq for Text {
    fn eq(&self, other: &Self) -> bool {
        self.buf == other.buf && self.tags == other.tags
    }
}

// TODO: Properly implement _replacements.
impl Text {
    pub fn new() -> Self {
        Self {
            buf: Box::new(GapBuffer::new()),
            tags: Box::new(Tags::new()),
            marker: Marker::new(),
        }
    }

    pub fn from_file<'a>(path: impl AsRef<Path>) -> Self {
        let file = std::fs::File::open(path.as_ref()).expect("File failed to open");
        let buf = Box::new(GapBuffer::from_iter(file.bytes().map_while(Result::ok)));
        let tags = Box::new(Tags::with_len(buf.len()));

        Self {
            buf,
            tags,
            marker: Marker::new(),
        }
    }

    pub fn builder() -> Builder {
        Builder::new()
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    pub fn get_char(&self, c: usize) -> Option<char> {
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.chars().chain(s1.chars()).nth(c)
    }

    pub fn len_chars(&self) -> usize {
        // TODO: make this value stateful.
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.chars().count() + s1.chars().count()
    }

    pub fn len_lines(&self) -> usize {
        // TODO: make this value stateful.
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.bytes().chain(s1.bytes()).filter(|b| *b == b'\n').count() + 1
    }

    pub fn len_bytes(&self) -> usize {
        self.buf.len()
    }

    pub fn char_to_line(&self, c: usize) -> usize {
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.chars()
            .chain(s1.chars())
            .take(c)
            .filter(|c| *c == '\n')
            .count()
    }

    pub fn line_to_char(&self, l: usize) -> usize {
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.lines()
            .chain(s1.lines())
            .map(|l| l.chars().count())
            .take(l)
            .sum()
    }

    pub fn char_to_byte(&self, c: usize) -> usize {
        self.get_char_to_byte(c).unwrap_or(self.buf.len())
    }

    pub fn get_char_to_line(&self, c: usize) -> Option<usize> {
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.chars()
            .chain(s1.chars())
            .scan(0, |l, c| Some(if c == '\n' { *l + 1 } else { *l }))
            .nth(c)
    }

    pub fn get_line_to_char(&self, l: usize) -> Option<usize> {
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.lines()
            .chain(s1.lines())
            .scan(0, |c, l| Some(*c + l.chars().count()))
            .nth(l)
    }

    pub fn get_char_to_byte(&self, c: usize) -> Option<usize> {
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.char_indices()
            .map(|(b, _)| b)
            .chain(s1.char_indices().map(|(b, _)| b + s0.len()))
            .nth(c)
    }

    /// The visual start of the line
    ///
    /// This point is defined not by where the line actually begins,
    /// but by where the last '\n' was located. For example, if
    /// [`Tag`]s create ghost text or ommit text from multiple
    /// different lines, this point may differ from where in the
    /// [`Text`] the physical line actually begins.
    pub fn visual_line_start(&self, pos: impl Positional) -> ExactPos {
        let pos = pos.to_exact();
        if pos == ExactPos::default() {
            return ExactPos::default();
        }

        // NOTE: 20000 is a magic number, being a guess for what a reasonable
        // limit would be.
        let mut iter = self.rev_iter_at(pos).peekable();
        let mut cur_pos = pos;
        while let Some(peek) = iter.peek() {
            match peek.part {
                Part::Char('\n') => return cur_pos,
                Part::Char(_) => cur_pos = iter.next().unwrap().pos,
                _ => drop(iter.next()),
            }
        }

        ExactPos::default()
    }

    pub(crate) fn insert_str(&mut self, at: usize, str: &str) {
        self.replace_range(at..at, str);
    }

    pub(crate) fn apply_change(&mut self, change: &Change) {
        self.replace_range(change.taken_range(), &change.added_text);
    }

    pub(crate) fn undo_change(&mut self, change: &Change, chars: isize) {
        let start = change.start.saturating_add_signed(chars);
        let end = change.added_end().saturating_add_signed(chars);
        self.replace_range(start..end, &change.taken_text);
    }

    /// Removes the tags for all the cursors, used before they are
    /// expected to move.
    pub(crate) fn add_cursor_tags(&mut self, cursors: &Cursors) {
        for (cursor, is_main) in cursors.iter() {
            let Range { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = cursor_tags(is_main);

            let pos_list = [
                (start, start_tag),
                (end, end_tag),
                (cursor.caret().char(), caret_tag),
            ];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.into_iter().skip(no_selection) {
                self.tags.insert(pos, tag, self.marker);
            }
        }
    }

    /// Adds the tags for all the cursors, used after they are
    /// expected to have moved.
    pub(crate) fn remove_cursor_tags(&mut self, cursors: &Cursors) {
        for (cursor, _) in cursors.iter() {
            let Range { start, end } = cursor.range();
            let skip = if start == end { 1 } else { 0 };
            for ch_index in [start, end].into_iter().skip(skip) {
                self.tags.remove_at(ch_index, self.marker);
            }
        }
    }

    pub(crate) fn write_to(&self, mut writer: impl std::io::Write) -> std::io::Result<usize> {
        let (s0, s1) = self.buf.as_slices();
        Ok(writer.write(s0)? + writer.write(s1)?)
    }

    fn clear(&mut self) {
        self.buf = Box::new(GapBuffer::new());
        self.tags.clear();
    }

    /// Merges `String`s with the body of text, given a range to
    /// replace.
    fn replace_range(&mut self, old: Range<usize>, edit: impl AsRef<str>) {
        let edit = edit.as_ref();

        self.buf
            .splice(old.clone(), edit.as_bytes().iter().cloned());

        if edit.len() != old.clone().count() {
            let new_end = old.start + edit.len();
            self.tags.transform_range(old, new_end);
        }
    }

    pub fn tags(&self) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        self.tags.iter_at(0)
    }

    pub fn tags_at(&self, at: usize) -> impl Iterator<Item = (usize, RawTag)> + Clone + '_ {
        self.tags.iter_at(at)
    }

    pub fn chars(&self) -> impl Iterator<Item = char> + Clone + '_ {
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.chars().chain(s1.chars())
    }

    pub fn insert_tag(&mut self, at: usize, tag: Tag, marker: Marker) {
        self.tags.insert(at, tag, marker);
    }

    pub fn remove_tags_on(&mut self, b: usize, markers: impl Markers) {
        self.tags.remove_at(b, markers)
    }
}

// Iterator methods.
impl Text {
    pub fn iter(&self) -> Iter<'_> {
        Iter::new_at(self, 0)
    }

    pub fn iter_at(&self, pos: impl Positional) -> Iter<'_> {
        Iter::new_at(self, pos)
    }

    pub fn rev_iter(&self) -> RevIter {
        RevIter::new_at(self, self.len_chars())
    }

    pub fn rev_iter_at(&self, pos: impl Positional) -> RevIter<'_> {
        RevIter::new_at(self, pos)
    }

    pub fn rev_iter_following(&self, pos: impl Positional) -> RevIter<'_> {
        RevIter::new_following(self, pos)
    }

    pub fn iter_chars_at(&self, c: usize) -> impl Iterator<Item = char> + '_ {
        let (s0, s1) = self.buf.as_slices();
        let (s0, s1) = unsafe { (from_utf8_unchecked(s0), from_utf8_unchecked(s1)) };

        s0.chars().chain(s1.chars()).skip(c)
    }

    /// TO BE DEPRECATED.
    pub fn iter_line(&self, line: usize) -> impl Iterator<Item = Item> + Clone + '_ {
        let start = self.line_to_char(line);
        let end = self.get_line_to_char(line + 1).unwrap_or(start);

        Iter::new_at(self, start).take_while(move |item| item.real() < end)
    }

    pub fn iter_line_chars(&self, line: usize) -> impl Iterator<Item = char> + '_ {
        self.iter_line(line).filter_map(|item| item.part.as_char())
    }
}

impl<S> From<S> for Text
where
    S: ToString,
{
    fn from(value: S) -> Self {
        let buf = Box::new(GapBuffer::from_iter(value.to_string().bytes()));
        let tags = Box::new(Tags::with_len(buf.len()));

        Self {
            buf,
            tags,
            marker: Marker::new(),
        }
    }
}

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

    pub fn push_str(&mut self, display: impl Display) {
        self.buffer.clear();
        write!(self.buffer, "{display}").unwrap();
        write!(
            PANIC_LOG.lock(),
            "\npush {}",
            self.buffer.to_string().lines().collect::<String>()
        );
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

    pub fn push_text(&mut self, mut text: Text) {
        let end = self.text.len_bytes();
        self.text.buf.splice(end..end, *text.buf);
        self.text.tags.append(*text.tags);
    }

    pub fn push_part<D: Display>(&mut self, part: BuilderPart<D>) {
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
            marker: Marker::new(),
            buffer: String::with_capacity(50),
        }
    }
}

fn cursor_tags(is_main: bool) -> (Tag, Tag, Tag) {
    if is_main {
        (
            Tag::MainCursor,
            Tag::PushForm(palette::MAIN_SEL),
            Tag::PopForm(palette::MAIN_SEL),
        )
    } else {
        (
            Tag::MainCursor,
            Tag::PushForm(palette::EXTRA_SEL),
            Tag::PopForm(palette::EXTRA_SEL),
        )
    }
}

pub fn get_ends(range: impl std::ops::RangeBounds<usize>, max: usize) -> (usize, usize) {
    let start = match range.start_bound() {
        std::ops::Bound::Included(start) => *start,
        std::ops::Bound::Excluded(start) => *start + 1,
        std::ops::Bound::Unbounded => 0,
    };
    let end = match range.end_bound() {
        std::ops::Bound::Included(end) => *end + 1,
        std::ops::Bound::Excluded(end) => *end,
        std::ops::Bound::Unbounded => max,
    };

    (start, end)
}

pub struct AlignCenter;
pub struct AlignLeft;
pub struct AlignRight;
pub struct Ghost(pub Text);

pub enum BuilderPart<D>
where
    D: Display,
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
    D: Display,
{
    fn from(value: &RwData<D>) -> Self {
        BuilderPart::ToString(value.read().to_string())
    }
}

impl<D> From<&RoData<D>> for BuilderPart<String>
where
    D: Display,
{
    fn from(value: &RoData<D>) -> Self {
        BuilderPart::ToString(value.read().to_string())
    }
}

impl<D> From<D> for BuilderPart<D>
where
    D: Display,
{
    fn from(value: D) -> Self {
        BuilderPart::ToString(value)
    }
}

pub macro text {
    // Forms
    (@push $builder:expr, []) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            crate::palette::__weakest_id_of_name("Default")
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
