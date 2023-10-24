mod cfg;
mod iter;
pub mod reader;
mod tags;
mod types;
mod gap_buffer;

use std::{
    fmt::{Display, Write},
    ops::Range,
    sync::LazyLock,
};

use ropey::Rope;

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

trait InnerTags: std::fmt::Debug + Default + Sized + Clone {
    fn with_len(len: usize) -> Self;
}

/// The text in a given area.
#[derive(Debug, Default, Clone, Eq)]
pub struct Text {
    rope: Rope,
    pub tags: Tags,
    cursor_marker: Marker,
}

impl PartialEq for Text {
    fn eq(&self, other: &Self) -> bool {
        self.rope == other.rope && self.tags == other.tags
    }
}

// TODO: Properly implement _replacements.
impl Text {
    pub fn new(string: impl ToString) -> Self {
        let rope = Rope::from(string.to_string());
        let tags = Tags::with_len(rope.len_chars());
        Text {
            rope,
            tags,
            cursor_marker: Marker::new(),
        }
    }

    pub fn builder() -> Builder {
        Builder::new()
    }

    pub fn is_empty(&self) -> bool {
        self.rope.len_chars() == 0
    }

    pub fn get_char(&self, char_index: usize) -> Option<char> {
        self.rope.get_char(char_index)
    }

    pub fn len_chars(&self) -> usize {
        self.rope.len_chars()
    }

    pub fn len_lines(&self) -> usize {
        self.rope.len_lines()
    }

    pub fn len_bytes(&self) -> usize {
        self.rope.len_bytes()
    }

    pub fn char_to_line(&self, char: usize) -> usize {
        self.rope.char_to_line(char)
    }

    pub fn line_to_char(&self, line: usize) -> usize {
        self.rope.line_to_char(line)
    }

    pub fn char_to_byte(&self, char: usize) -> usize {
        self.rope.char_to_byte(char)
    }

    pub fn get_char_to_line(&self, char: usize) -> Option<usize> {
        self.rope.try_char_to_line(char).ok()
    }

    pub fn get_line_to_char(&self, line: usize) -> Option<usize> {
        self.rope.try_line_to_char(line).ok()
    }

    pub fn get_char_to_byte(&self, char: usize) -> Option<usize> {
        self.rope.try_char_to_byte(char).ok()
    }

    pub fn visual_line_start(&self, exact_pos: ExactPos) -> ExactPos {
        if exact_pos == ExactPos::default() {
            return ExactPos::default();
        }

        // NOTE: 20000 is a magic number, being a guess for what a reasonable
        // limit would be.
        let mut iter = self.rev_iter_exactly_at(exact_pos).peekable();
        let mut cur_pos = exact_pos;
        while let Some(peek) = iter.peek() {
            match peek.part {
                Part::Char('\n') => return cur_pos,
                Part::Char(_) => cur_pos = iter.next().unwrap().pos,
                _ => drop(iter.next()),
            }
        }

        ExactPos::default()
    }

    pub(crate) fn insert_str(&mut self, pos: usize, str: &str) {
        self.replace_range(pos..pos, str);
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
                self.tags.insert(pos, tag, self.cursor_marker);
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
                self.tags.remove_on(ch_index, self.cursor_marker);
            }
        }
    }

    pub(crate) fn write_to(&self, writer: impl std::io::Write) -> std::io::Result<usize> {
        self.rope.write_to(writer).map(|_| self.rope.len_bytes())
    }

    fn clear(&mut self) {
        self.rope = Rope::new();
        self.tags.clear();
    }

    /// Merges `String`s with the body of text, given a range to
    /// replace.
    fn replace_range(&mut self, old: Range<usize>, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let edit_len = edit.chars().count();

        self.rope.remove(old.clone());
        let (start, _) = get_ends(old.clone(), self.len_chars());
        self.rope.insert(start, edit.as_ref());

        if edit_len != old.clone().count() {
            let new_end = old.start + edit_len;
            self.tags.transform_range(old, new_end);
        }
    }

    pub fn tags(&self) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        self.tags.iter_at(0)
    }

    pub fn get_from_char(&self, pos: usize) -> Option<(usize, TagOrSkip)> {
        self.tags.get_from_pos(pos)
    }

    pub fn tags_at(&self, ch_index: usize) -> impl Iterator<Item = (usize, RawTag)> + Clone + '_ {
        self.tags.iter_at(ch_index)
    }

    pub fn chars(&self) -> impl Iterator<Item = char> + Clone + '_ {
        self.rope.chars_at(0)
    }

    pub fn chars_at(&self, pos: usize) -> impl Iterator<Item = char> + Clone + '_ {
        self.rope.chars_at(pos)
    }

    pub fn insert_tag(&mut self, pos: usize, tag: Tag, marker: Marker) {
        self.tags.insert(pos, tag, marker);
    }

    pub fn remove_on(&mut self, pos: usize, markers: impl Markers) {
        self.tags.remove_on(pos, markers)
    }
}

// Iterator methods.
impl Text {
    pub fn iter(&self) -> Iter<'_> {
        Iter::new_at(self, 0)
    }

    /// TO BE DEPRECATED.
    pub fn iter_line(&self, line: usize) -> impl Iterator<Item = Item> + Clone + '_ {
        let start = self.line_to_char(line);
        let end = self.get_line_to_char(line + 1).unwrap_or(start);

        Iter::new_at(self, start).take_while(move |item| item.real() < end)
    }

    pub fn iter_at(&self, pos: usize) -> Iter<'_> {
        Iter::new_at(self, pos)
    }

    pub fn iter_exactly_at(&self, exact_pos: ExactPos) -> Iter<'_> {
        Iter::new_exactly_at(self, exact_pos)
    }

    pub fn rev_iter(&self) -> RevIter {
        RevIter::new_at(self, self.len_chars())
    }

    pub fn rev_iter_at(&self, pos: usize) -> RevIter<'_> {
        RevIter::new_at(self, pos)
    }

    pub fn rev_iter_exactly_at(&self, exact_pos: ExactPos) -> RevIter<'_> {
        RevIter::new_exactly_at(self, exact_pos)
    }

    pub fn rev_iter_following(&self, exact_pos: ExactPos) -> RevIter<'_> {
        RevIter::new_following(self, exact_pos)
    }

    pub fn iter_chars_at(&self, pos: usize) -> impl Iterator<Item = char> + '_ {
        self.rope.chars_at(pos)
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
        Self::new(value.to_string())
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
///   modifying
/// existing tags.
/// - All [`Tag`]s that are appended result in an inverse [`Tag`]
///   being placed
/// before the next one, or at the end of the [`Tags`] (e.g.
/// [`Tag::PushForm`] would be followed a [`Tag::PopForm`]).
/// - You can insert swappable text with
///   [`push_swappable()`][Self::push_swappable].
///
/// These properties allow for quick and easy modification of the
/// [`Text`] within, which can then be accessed with
/// [`text()`][Self::text()].
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
        let len_chars = self.text.len_chars();

        if let Some(tag) = self.last_form {
            self.text.tags.insert(len_chars, tag, self.marker);
        }

        if let Some(tag) = self.last_align {
            self.text.tags.insert(len_chars, tag, self.marker);
        }

        self.text
    }

    pub fn push_str(&mut self, display: impl Display) {
        self.buffer.clear();
        write!(self.buffer, "{}", display).unwrap();
        self.text.insert_str(self.text.len_chars(), &self.buffer)
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well
    /// as its inverse at the end of the [`Text`].
    pub fn push_tag(&mut self, tag: Tag) -> Option<ToggleId> {
        let len_chars = self.text.len_chars();

        let last_inverted = match tag {
            Tag::PushForm(id) => self.last_form.replace(Tag::PopForm(id)),
            Tag::StartAlignLeft => self.last_align.replace(Tag::EndAlignLeft),
            Tag::StartAlignCenter => self.last_align.replace(Tag::EndAlignCenter),
            Tag::StartAlignRight => self.last_align.replace(Tag::EndAlignRight),
            _ => None,
        };

        if let Some(tag) = last_inverted {
            self.text.tags.insert(len_chars, tag, self.marker);
        }

        self.text.tags.insert(len_chars, tag, self.marker)
    }

    pub fn push_text(&mut self, mut text: Text) {
        let end = self.text.len_chars();
        self.text
            .tags
            .transform_range(end..end, end + text.len_chars());
        self.text.rope.append(text.rope);
        self.text.tags.toggles.extend(text.tags.toggles.drain());
        self.text.tags.texts.extend(text.tags.texts.drain());

        for entry in text.tags.iter_at(0) {
            let (pos, tag): (usize, RawTag) = entry;

            let last_inverted = match (tag, &self.last_form, &self.last_align) {
                (RawTag::PushForm(_, id), ..) => self.last_form.replace(Tag::PopForm(id)),
                (RawTag::StartAlignLeft(_), ..) => self.last_align.replace(Tag::EndAlignLeft),
                (RawTag::StartAlignCenter(_), ..) => self.last_align.replace(Tag::EndAlignCenter),
                (RawTag::StartAlignRight(_), ..) => self.last_align.replace(Tag::EndAlignRight),

                // If the text already had the ending tags, don't duplicate them.
                (RawTag::PopForm(_, lhs), Some(Tag::PopForm(rhs)), _) if lhs == *rhs => {
                    self.last_form = None;
                    None
                }
                (RawTag::EndAlignLeft(_), _, Some(Tag::EndAlignLeft))
                | (RawTag::EndAlignCenter(_), _, Some(Tag::EndAlignCenter))
                | (RawTag::EndAlignRight(_), _, Some(Tag::EndAlignRight)) => {
                    self.last_align = None;
                    None
                }
                _ => None,
            };

            if let Some(tag) = last_inverted {
                self.text.tags.insert(end + pos, tag, self.marker);
            }

            self.text.tags.insert_raw(end + pos, tag);
        }
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
            crate::palette::weakest_id_of_name("Default")
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },

    (@push $builder:expr, [$form:ident]) => {
        static FORM_ID: __FormIdLock = __FormIdLock::new(|| {
            let name = stringify!($form);
            crate::palette::weakest_id_of_name(name)
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
