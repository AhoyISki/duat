mod cfg;
mod chars;
mod iter;
pub mod reader;
mod tags;
mod types;

use std::{io::Write, ops::Range};

use ropey::Rope;

pub use self::{
    cfg::*,
    iter::{Item, Iter, RevIter},
    tags::Tag,
    types::Part,
};
use self::{
    chars::Chars,
    tags::{RawTag, TagOrSkip, Tags, ToggleId},
};
use crate::{
    forms::{self, FormId},
    history::Change,
    input::Cursors,
};

/// The text in a given area.
#[derive(Debug)]
pub struct Text {
    chars: Chars,
    pub tags: Tags,
}

// TODO: Properly implement _replacements.
impl Text {
    pub fn default_string() -> Self {
        Text {
            chars: Chars::String(String::with_capacity(500)),
            tags: Tags::new_vec(),
        }
    }

    pub fn default_rope() -> Self {
        Text {
            chars: Chars::Rope(Rope::default()),
            tags: Tags::new_rope(),
        }
    }

    pub fn new_string(string: impl ToString) -> Self {
        let chars = Chars::String(string.to_string());
        let tags = Tags::from_chars(&chars);
        Text { chars, tags }
    }

    pub fn new_rope(string: impl ToString) -> Self {
        let chars = Chars::Rope(Rope::from(string.to_string()));
        let tags = Tags::from_chars(&chars);
        Text { chars, tags }
    }

    pub fn is_empty(&self) -> bool {
        self.chars.len_chars() == 0
    }

    pub fn get_char(&self, char_index: usize) -> Option<char> {
        self.chars.get_char(char_index)
    }

    pub fn len_chars(&self) -> usize {
        self.chars.len_chars()
    }

    pub fn len_lines(&self) -> usize {
        self.chars.len_lines()
    }

    pub fn len_bytes(&self) -> usize {
        self.chars.len_bytes()
    }

    pub fn char_to_line(&self, char: usize) -> usize {
        self.chars
            .char_to_line(char)
            .unwrap_or_else(|| panic!("Char index {char} out of bounds."))
    }

    pub fn line_to_char(&self, line: usize) -> usize {
        self.chars
            .line_to_char(line)
            .unwrap_or_else(|| panic!("Line index {line} out of bounds."))
    }

    pub fn char_to_byte(&self, char: usize) -> usize {
        self.chars
            .char_to_byte(char)
            .unwrap_or_else(|| panic!("Char index {char} out of bounds."))
    }

    pub fn get_char_to_line(&self, char: usize) -> Option<usize> {
        self.chars.char_to_line(char)
    }

    pub fn get_line_to_char(&self, line: usize) -> Option<usize> {
        self.chars.line_to_char(line)
    }

    pub fn get_char_to_byte(&self, char: usize) -> Option<usize> {
        self.chars.char_to_byte(char)
    }

    pub fn close_visual_line_start(&self, pos: usize) -> Option<usize> {
        if pos == 0 {
            return Some(0);
        }
        // NOTE: 20000 is a magic number, being a guess for what a reasonable
        // limit would be.
        self.rev_iter_at(pos)
            .take(20000)
            .find_map(|Item { pos, part, .. }| {
                if part.as_char().is_some_and(|char| char == '\n') {
                    Some(pos + 1)
                } else {
                    (pos == 0).then_some(0)
                }
            })
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
                (cursor.caret().true_char(), caret_tag),
            ];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.into_iter().skip(no_selection) {
                self.tags.insert(pos, tag);
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
                self.tags.remove_on(ch_index);
            }
        }
    }

    pub(crate) fn write_to(
        &self,
        mut writer: std::io::BufWriter<std::fs::File>,
    ) -> Result<usize, String> {
        match &self.chars {
            Chars::String(string) => writer
                .write(string.as_bytes())
                .map_err(|err| err.to_string()),
            Chars::Rope(rope) => rope
                .write_to(writer)
                .map(|_| rope.len_bytes())
                .map_err(|err| err.to_string()),
        }
    }

    fn clear(&mut self) {
        self.chars.clear();
        self.tags.clear();
    }

    /// Merges `String`s with the body of text, given a range to
    /// replace.
    fn replace_range(&mut self, old: Range<usize>, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let edit_len = edit.chars().count();
        let new = old.start..(old.start + edit_len);

        self.chars.replace(old.clone(), edit);

        if old != new {
            self.tags.transform_range(old, new.end);
        }
    }

    pub fn tags(&self) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        self.tags.iter_at(0)
    }

    pub fn get_from_char(&self, char: usize) -> Option<(usize, TagOrSkip)> {
        self.tags.get_from_char(char)
    }

    pub fn tags_at(&self, ch_index: usize) -> impl Iterator<Item = (usize, RawTag)> + Clone + '_ {
        self.tags.iter_at(ch_index)
    }

    pub fn chars(&self) -> impl Iterator<Item = char> + Clone + '_ {
        self.chars.iter_at(0)
    }

    pub fn chars_at(&self, ch_index: usize) -> impl Iterator<Item = char> + Clone + '_ {
        self.chars.iter_at(ch_index)
    }

    pub fn insert(&mut self, char: usize, tag: Tag) {
        self.tags.insert(char, tag);
    }

    pub fn remove_on(&mut self, ch_index: usize) {
        self.tags.remove_on(ch_index)
    }
}

// Iterator methods.
impl Text {
    pub fn iter(&self) -> Iter<'_> {
        let chars = self.chars.iter_at(0);
        let tags = self.tags.iter_at(0);

        Iter::new(chars, tags, &self.tags.texts, 0, 0)
    }

    /// TO BE DEPRECATED.
    pub fn iter_line(&self, line: usize) -> impl Iterator<Item = Item> + Clone + '_ {
        let start = self.line_to_char(line);
        let end = self.get_line_to_char(line + 1).unwrap_or(start);
        let chars = self.chars.iter_at(start);

        let tags_start = start.saturating_sub(self.tags.back_check_amount());
        let tags = self.tags.iter_at(tags_start);

        Iter::new(chars, tags, &self.tags.texts, start, line).take_while(move |item| item.pos < end)
    }

    pub fn iter_at(&self, pos: usize) -> Iter<'_> {
        let chars = self.chars.iter_at(pos);

        let line = self.char_to_line(pos);
        let tags_start = pos.saturating_sub(self.tags.back_check_amount());
        let tags = self.tags.iter_at(tags_start);

        Iter::new(chars, tags, &self.tags.texts, pos, line)
    }

    pub fn rev_iter(&self) -> RevIter {
        let start = self.len_chars();
        let chars = self.chars.rev_iter_at(start);
        let tags = self.tags.rev_iter_at(start);

        RevIter::new(chars, tags, &self.tags.texts, start, start)
    }

    pub fn rev_iter_at(&self, pos: usize) -> RevIter<'_> {
        let chars = self.chars.rev_iter_at(pos);

        let line = self.char_to_line(pos);
        let tags = self.tags.rev_iter_at(pos);

        RevIter::new(chars, tags, &self.tags.texts, pos, line)
    }

    pub fn iter_chars_at(&self, pos: usize) -> impl Iterator<Item = char> + '_ {
        self.chars.iter_at(pos)
    }

    pub fn iter_line_chars(&self, line: usize) -> impl Iterator<Item = char> + '_ {
        self.iter_line(line).filter_map(|item| item.part.as_char())
    }
}

impl<S: ToString> From<S> for Text {
    fn from(value: S) -> Self {
        Text::new_string(value.to_string())
    }
}

/// Builds and modifies a [`Text<U>`], based on replacements applied
/// to it.
///
/// The generation of text by the `TextBuilder<U>` has a few
/// peculiarities that are convenient in the situations where it is
/// useful:
///
/// - The user cannot insert [`Tag`]s directly, only by appending and modifying
/// existing tags.
/// - All [`Tag`]s that are appended result in an inverse [`Tag`] being placed
/// before the next one, or at the end of the [`Tags`] (e.g.
/// [`Tag::PushForm`] would be followed a [`Tag::PopForm`]).
/// - You can insert swappable text with
///   [`push_swappable()`][Self::push_swappable].
///
/// These properties allow for quick and easy modification of the
/// [`Text<U>`] within, which can then be accessed with
/// [`text()`][Self::text()].
pub struct TextBuilder {
    text: Text,
    last_form: Option<FormId>,
}

impl TextBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn finish(self) -> Text {
        self.text
    }

    pub fn push_str(&mut self, str: impl AsRef<str>) {
        let range = self.text.len_chars()..self.text.len_chars();
        let change = Change::new(str.as_ref(), range, &self.text);
        self.text.apply_change(&change);
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well
    /// as its inverse at the end of the [`Text<U>`].
    pub fn push_tag(&mut self, tag: Tag) -> Option<ToggleId> {
        if let Some(id) = self.last_form {
            self.text
                .tags
                .insert(self.text.len_chars(), Tag::PopForm(id));
        }

        if let Tag::PushForm(id) = tag {
            self.last_form = Some(id);
        }

        self.text.tags.insert(self.text.len_chars(), tag)
    }

    pub fn push_text(&mut self, mut text: Text) {
        let Some((chars, tags)) = text.chars.as_mut_string().zip(text.tags.as_mut_vec()) else {
            panic!(
                "Don't push Rope Texts into a Text whilst building it, it'll be significantly \
                 slower with no real benefit"
            );
        };

        self.text.chars.as_mut_string().unwrap().push_str(chars);
        self.text.tags.as_mut_vec().unwrap().append(tags);
    }

    pub fn clear(&mut self) {
        self.text.clear();
        self.last_form = None;
    }

    pub fn text(&self) -> &Text {
        &self.text
    }
}

impl Default for TextBuilder {
    fn default() -> Self {
        TextBuilder {
            text: Text::default_string(),
            last_form: None,
        }
    }
}

fn cursor_tags(is_main: bool) -> (Tag, Tag, Tag) {
    if is_main {
        (
            Tag::MainCursor,
            Tag::PushForm(forms::MAIN_SEL),
            Tag::PopForm(forms::MAIN_SEL),
        )
    } else {
        (
            Tag::MainCursor,
            Tag::PushForm(forms::EXTRA_SEL),
            Tag::PopForm(forms::EXTRA_SEL),
        )
    }
}

pub macro build_text {
    // Forms
    (@push $builder:expr, [$form:ident]) => {
        use std::sync::LazyLock;
        static FORM_ID: LazyLock<crate::forms::FormId> = LazyLock::new(|| {
            crate::log_info!("{}", stringify!($form));
            let name = stringify!($form);
            crate::PALETTE.from_name(name).1
        });
        $builder.push_tag(crate::text::Tag::PushForm(*FORM_ID))
    },
    (@push $builder:expr, [[$form_id:expr]]) => {
        $builder.push_tag(crate::text::Tag::PushForm($form_id))
    },

    // Alignments
    (@push $builder:expr, (AlignCenter)) => {
        $builder.push_tag(crate::text::Tag::AlignCenter)
    },
    (@push $builder:expr, (AlignLeft)) => {
        $builder.push_tag(crate::text::Tag::AlignLeft)
    },
    (@push $builder:expr, (AlignRight)) => {
        $builder.push_tag(crate::text::Tag::AlignRight)
    },

    // Other tags
    (@push $builder:expr, (($tag:expr))) => {
        $builder.push_tag($tag)
    },

    // Failure
    (@push $builder:expr, ($not_allowed:expr)) => {
        compile_error!("Expressions are not allowed in place of tag identifiers.");
    },

    // Plain text
    (@push $builder:expr, $str:expr) => {
        $builder.push_str($str)
    },

    (@parse $builder:expr, $part:tt $($parts:tt)*) => {{
        build_text!(@push $builder, $part);
        build_text!(@parse $builder, $($parts)*);
    }},
    (@parse $builder:expr,) => {},

    ($builder:expr, $($parts:tt)*) => {{
        let builder: &mut TextBuilder = &mut $builder;
        build_text!(@parse builder, $($parts)*);
    }},
    ($($parts:tt)*) => {{
        let mut builder = TextBuilder::new();
        build_text!(builder, $($parts)*);
        builder
    }}
}
