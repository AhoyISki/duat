pub mod inner;
pub mod reader;

use std::{
    iter::Peekable,
    ops::{Range, RangeBounds, RangeInclusive}
};

use ropey::Rope;

use self::inner::InnerText;
use crate::{
    history::Change,
    position::Cursor,
    tags::{
        form::{EXTRA_SEL, MAIN_SEL},
        Lock, Tag, TagOrSkip, Tags
    }
};

/// Builds and modifies a [`Text<U>`], based on replacements applied
/// to it.
///
/// The generation of text by the `TextBuilder<U>` has a few
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
/// [`Text<U>`] within, which can then be accessed with
/// [`text()`][Self::text()].
#[derive(Debug)]
pub struct TextBuilder {
    text: Text,
    swappables: Vec<usize>
}

impl TextBuilder {
    pub fn push_text(&mut self, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let edit_len = edit.chars().count() as u32;
        self.text.inner.string().push_str(edit);

        self.add_to_last_skip(edit_len);
    }

    pub fn push_swappable(&mut self, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let edit_len = edit.chars().count() as u32;

        let last_skip = self
            .text
            .tags
            .as_mut_vec()
            .unwrap()
            .iter()
            .filter(|t_or_s| matches!(t_or_s, TagOrSkip::Skip(_)))
            .count();

        self.swappables.push(last_skip);
        self.text.inner.string().push_str(edit);

        self.add_to_last_skip(edit_len);
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well
    /// as its inverse at the end of the [`Text<U>`].
    pub fn push_tag(&mut self, tag: Tag) -> Lock {
        let lock = self.text.tags.new_lock();
        let tags = self.text.tags.as_mut_vec().unwrap();
        tags.push(TagOrSkip::Tag(tag, lock));
        if let Some(inv_tag) = tag.inverse() {
            tags.push(TagOrSkip::Tag(inv_tag, lock));
        }
        lock
    }

    /// Replaces a range with a new piece of text.
    pub fn swap_range(&mut self, index: usize, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let swap_index = self.swappables[index];

        let Some((start, skip)) = self.get_mut_skip(swap_index) else {
            return;
        };
        let old_skip = *skip as usize;
        *skip = edit.chars().count() as u32;

        self.text.inner.replace(start..(start + old_skip), edit);
    }

    pub fn swap_tag(&mut self, tag_index: usize, new_tag: Tag) {
        let tags_vec = self.text.tags.as_mut_vec().unwrap();
        let mut tags = tags_vec.iter_mut().enumerate().filter_map(|(index, t_or_s)| match t_or_s {
            TagOrSkip::Tag(Tag::PopForm(_), _) => None,
            TagOrSkip::Tag(tag, lock) => Some((index, tag, *lock)),
            TagOrSkip::Skip(_) => None
        });

        if let Some((index, tag, lock)) = tags.nth(tag_index) {
            let inv_tag = tag.inverse();

            *tag = new_tag;
            let forward = match &tags_vec[index + 1] {
                TagOrSkip::Tag(_, _) => 1,
                TagOrSkip::Skip(_) => 2
            };

            if let Some(new_inv_tag) = new_tag.inverse() {
                if let Some(_) = inv_tag {
                    tags_vec[index + forward] = TagOrSkip::Tag(new_inv_tag, lock);
                } else {
                    tags_vec.insert(index + forward, TagOrSkip::Tag(new_inv_tag, lock));
                }
            } else if let Some(_) = inv_tag {
                tags_vec.remove(index + forward);
            }
        }
    }

    pub fn get_mut_skip(&mut self, index: usize) -> Option<(usize, &mut u32)> {
        self.text
            .tags
            .as_mut_vec()
            .unwrap()
            .iter_mut()
            .filter_map(|tag_or_skip| match tag_or_skip {
                TagOrSkip::Skip(skip) => Some(skip),
                TagOrSkip::Tag(..) => None
            })
            .scan(0, |accum, skip| {
                let prev_accum = *accum;
                *accum += *skip as usize;
                Some((prev_accum, skip))
            })
            .nth(index)
    }

    fn add_to_last_skip(&mut self, edit_len: u32) {
        let tags_vec = self.text.tags.as_mut_vec().unwrap();

        let mut tags = tags_vec.iter().enumerate().rev();
        while let Some((index, TagOrSkip::Tag(tag, _))) = tags.next() {
            if let Tag::PopForm(_) = tag {
                if let Some(TagOrSkip::Skip(skip)) = tags_vec.get_mut(index) {
                    *skip += edit_len;
                } else {
                    tags_vec.insert(index, TagOrSkip::Skip(edit_len));
                }
                return;
            }
        }

        tags_vec.push(TagOrSkip::Skip(edit_len));
    }

    pub fn clear(&mut self) {
        self.text.clear();
        self.swappables.clear();
    }

    pub fn truncate(&mut self, range_index: usize) {
        let tags_vec = self.text.tags.as_mut_vec().unwrap();

        let Some(&swap_index) = self.swappables.get(range_index) else {
            return;
        };
        let (mut index, cutoff) = tags_vec
            .iter_mut()
            .enumerate()
            .filter_map(|(index, t_or_s)| t_or_s.as_skip().map(|skip| (index, skip)))
            .scan(0, |accum, (index, skip)| {
                let prev_accum = *accum;
                *accum += *skip as usize;
                Some((index, prev_accum))
            })
            .nth(swap_index)
            .unwrap();

        self.swappables.truncate(range_index);
        self.text.inner.string().truncate(cutoff);

        let mut tags = tags_vec.iter().take(index).rev();
        while let Some(TagOrSkip::Tag(tag, ..)) = tags.next() {
            if let Tag::PopForm(_) = tag {
                break;
            }

            index -= 1;
        }
        tags_vec.truncate(index);
    }

    pub fn ranges_len(&self) -> usize {
        self.swappables.len()
    }

    pub fn text(&self) -> &Text {
        &self.text
    }
}

impl Default for TextBuilder {
    fn default() -> Self {
        TextBuilder {
            text: Text::default_string(),
            swappables: Vec::default()
        }
    }
}

/// The text in a given area.
#[derive(Debug)]
pub struct Text {
    inner: InnerText,
    pub tags: Tags,
    lock: Lock,
    _replacements: Vec<(Vec<Text>, RangeInclusive<usize>, bool)>
}

// TODO: Properly implement _replacements.
impl Text {
    pub fn default_string() -> Self {
        let mut tags = Tags::default_vec();
        let lock = tags.new_lock();
        Text {
            inner: InnerText::String(String::default()),
            tags,
            lock,
            _replacements: Vec::new()
        }
    }

    pub fn default_rope() -> Self {
        let mut tags = Tags::default_rope();
        let lock = tags.new_lock();
        Text {
            inner: InnerText::Rope(Rope::default()),
            tags,
            lock,
            _replacements: Vec::new()
        }
    }

    pub fn new_string(string: impl ToString) -> Self {
        let inner = InnerText::String(string.to_string());
        let mut tags = Tags::new(&inner);
        let lock = tags.new_lock();
        Text {
            inner,
            tags,
            lock,
            _replacements: Vec::new()
        }
    }

    pub fn new_rope(string: impl ToString) -> Self {
        let inner = InnerText::Rope(Rope::from(string.to_string()));
        let mut tags = Tags::new(&inner);
        let lock = tags.new_lock();
        Text {
            inner,
            tags,
            lock,
            _replacements: Vec::new()
        }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len_chars() == 0
    }

    pub fn get_char(&self, char_index: usize) -> Option<char> {
        self.inner.get_char(char_index)
    }

    /// Merges `String`s with the body of text, given a range to
    /// replace.
    fn replace_range(&mut self, old_range: Range<usize>, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let edit_len = edit.chars().count();
        let new_range = old_range.start..(old_range.start + edit_len);

        self.inner.replace(old_range.start..old_range.end, edit);

        if old_range != new_range {
            self.tags.transform_range(old_range, new_range);
        }
    }

    pub(crate) fn apply_change(&mut self, change: &Change) {
        self.replace_range(change.taken_range(), &change.added_text);
    }

    pub(crate) fn undo_change(&mut self, change: &Change, chars: isize) {
        let start = change.start.saturating_add_signed(chars);
        let end = change.added_end().saturating_add_signed(chars);
        self.replace_range(start..end, &change.taken_text);
    }

    pub(crate) fn inner(&self) -> &InnerText {
        &self.inner
    }

    /// Removes the tags for all the cursors, used before they are
    /// expected to move.
    pub(crate) fn add_cursor_tags(&mut self, cursors: &[Cursor], main_index: usize) {
        for (index, cursor) in cursors.iter().enumerate() {
            let Range { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = cursor_tags(index == main_index);

            let pos_list =
                [(start, start_tag), (end, end_tag), (cursor.caret().true_char(), caret_tag)];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.into_iter().skip(no_selection) {
                self.tags.insert(pos, tag, self.lock);
            }
        }
    }

    /// Adds the tags for all the cursors, used after they are
    /// expected to have moved.
    pub(crate) fn remove_cursor_tags(&mut self, cursors: &[Cursor]) {
        for cursor in cursors.iter() {
            let Range { start, end } = cursor.range();
            let skip = if start == end { 1 } else { 0 };
            for ch_index in [start, end].into_iter().skip(skip) {
                self.tags.remove_on(ch_index, self.lock);
            }
        }
    }

    pub fn len_chars(&self) -> usize {
        self.inner.len_chars()
    }

    pub fn len_lines(&self) -> usize {
        self.inner.len_lines()
    }

    pub fn len_bytes(&self) -> usize {
        self.inner.len_bytes()
    }

    fn clear(&mut self) {
        self.inner.clear();
        self.tags.clear();
    }

    pub fn char_to_line(&self, char: usize) -> usize {
        self.inner
            .char_to_line(char)
            .unwrap_or_else(|| panic!("Char index {char} out of bounds."))
    }

    pub fn line_to_char(&self, line: usize) -> usize {
        self.inner
            .line_to_char(line)
            .unwrap_or_else(|| panic!("Line index {line} out of bounds."))
    }

    pub fn char_to_byte(&self, char: usize) -> usize {
        self.inner
            .char_to_byte(char)
            .unwrap_or_else(|| panic!("Char index {char} out of bounds."))
    }

    pub fn get_char_to_line(&self, char: usize) -> Option<usize> {
        self.inner.char_to_line(char)
    }

    pub fn get_line_to_char(&self, line: usize) -> Option<usize> {
        self.inner.line_to_char(line)
    }

    pub fn get_char_to_byte(&self, char: usize) -> Option<usize> {
        self.inner.char_to_byte(char)
    }

    pub fn iter_chars_at(&self, char: usize) -> impl Iterator<Item = char> + '_ {
        self.inner.chars_at(char)
    }

    pub fn iter_line_chars(&self, line: usize) -> impl Iterator<Item = char> + '_ {
        self.iter_line(line).filter_map(|(_, bit)| bit.as_char())
    }
}

// Iterator methods.
impl Text {
    pub fn iter(&self) -> impl Iterator<Item = (usize, TextBit)> + Clone + '_ {
        let chars = self.inner.chars_at(0);
        let tags = self.tags.iter_at(0).peekable();

        Iter::new(chars, tags, 0)
    }

    pub fn iter_line(&self, line: usize) -> impl Iterator<Item = (usize, TextBit)> + Clone + '_ {
        let start = self.line_to_char(line);
        let end = self.get_line_to_char(line + 1).unwrap_or(start);
        let chars = self.inner.chars_at(start).take(end - start);
        let tags = self.tags.iter_at(start).take_while(move |(index, _)| *index < end).peekable();

        Iter::new(chars, tags, start)
    }

    pub fn iter_range(
        &self, range: impl RangeBounds<usize>
    ) -> impl Iterator<Item = (usize, TextBit)> + '_ {
        let start = match range.start_bound() {
            std::ops::Bound::Included(start) => *start,
            std::ops::Bound::Excluded(start) => *start + 1,
            std::ops::Bound::Unbounded => 0
        };

        let end = match range.end_bound() {
            std::ops::Bound::Included(end) => end + 1,
            std::ops::Bound::Excluded(end) => *end,
            std::ops::Bound::Unbounded => self.inner.len_chars() + 1
        };

        let chars = self.inner.chars_at(start).take(end - start);
        let tags = self.tags.iter_at(start).take_while(move |(index, _)| *index < end).peekable();

        Iter::new(chars, tags, start)
    }
}

/// A part of the [`Text`], can be a [`char`] or a [`Tag`].
pub enum TextBit {
    Tag(Tag),
    Char(char)
}

impl std::fmt::Debug for TextBit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TextBit::Char(char) => f.write_fmt(format_args!("Char({})", char)),
            TextBit::Tag(tag) => f.write_fmt(format_args!("Tag({:?})", tag))
        }
    }
}

impl TextBit {
    /// Returns `true` if the text bit is [`Char`].
    ///
    /// [`Char`]: TextBit::Char
    #[must_use]
    pub fn is_char(&self) -> bool {
        match self {
            TextBit::Char(_) => true,
            _ => false
        }
    }

    /// Returns `true` if the text bit is [`Tag`].
    ///
    /// [`Tag`]: TextBit::Tag
    #[must_use]
    pub fn is_tag(&self) -> bool {
        match self {
            TextBit::Tag(_) => true,
            _ => false
        }
    }

    pub fn as_char(&self) -> Option<char> {
        if let Self::Char(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn as_tag(&self) -> Option<&Tag> {
        if let Self::Tag(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn is_new_line(&self) -> bool {
        match self {
            TextBit::Char('\n') => true,
            _ => false
        }
    }
}

/// An [`Iterator`] over the [`TextBit`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
#[derive(Clone)]
pub struct Iter<Chars, Tags>
where
    Chars: Iterator<Item = char> + Clone,
    Tags: Iterator<Item = (usize, Tag)> + Clone
{
    chars: Chars,
    tags: Peekable<Tags>,
    cur_char: usize
}

impl<Chars, Tags> Iter<Chars, Tags>
where
    Chars: Iterator<Item = char> + Clone,
    Tags: Iterator<Item = (usize, Tag)> + Clone
{
    pub fn new(chars: Chars, tags: Peekable<Tags>, cur_char: usize) -> Self {
        Self {
            chars,
            tags,
            cur_char
        }
    }
}

impl<Chars, Tags> Iterator for Iter<Chars, Tags>
where
    Chars: Iterator<Item = char> + Clone,
    Tags: Iterator<Item = (usize, Tag)> + Clone
{
    type Item = (usize, TextBit);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        // `<=` because some `Tag`s may be triggered before printing.
        if let Some(&(index, tag)) = self.tags.peek().filter(|(index, _)| *index <= self.cur_char) {
            self.tags.next();
            Some((index, TextBit::Tag(tag)))
        } else if let Some(char) = self.chars.next() {
            self.cur_char += 1;
            Some((self.cur_char - 1, TextBit::Char(char)))
        } else {
            None
        }
    }
}

fn cursor_tags(is_main: bool) -> (Tag, Tag, Tag) {
    if is_main {
        (Tag::MainCursor, Tag::PushForm(MAIN_SEL), Tag::PopForm(MAIN_SEL))
    } else {
        (Tag::MainCursor, Tag::PushForm(EXTRA_SEL), Tag::PopForm(EXTRA_SEL))
    }
}

/// If and how to wrap lines at the end of the screen.
#[derive(Default, Debug, Copy, Clone)]
pub enum WrapMethod {
    Width,
    Capped(usize),
    Word,
    #[default]
    NoWrap
}

impl WrapMethod {
    /// Returns `true` if the wrap method is [`NoWrap`].
    ///
    /// [`NoWrap`]: WrapMethod::NoWrap
    #[must_use]
    pub fn is_no_wrap(&self) -> bool {
        matches!(self, Self::NoWrap)
    }

    pub fn wrapping_cap(&self, width: usize) -> usize {
        match self {
            WrapMethod::Capped(cap) => *cap,
            _ => width
        }
    }
}

/// Where the tabs are placed on screen, can be regular or varied.
#[derive(Debug, Clone)]
pub struct TabStops(pub usize);

impl TabStops {
    pub fn spaces_at(&self, x: usize) -> usize {
        self.0 - (x % self.0)
    }
}

impl Default for TabStops {
    fn default() -> Self {
        TabStops(4)
    }
}

/// Wheter to show the new line or not.
#[derive(Default, Debug, Clone, Copy)]
pub enum NewLine {
    #[default]
    /// Never show new lines.
    Blank,
    /// Show the given character on every new line.
    AlwaysAs(char),
    /// Show the given character only when there is whitespace at end
    /// of the line.
    AfterSpaceAs(char)
}

impl NewLine {
    pub fn new_line_char(&self, last_ch: char) -> char {
        match self {
            NewLine::Blank => ' ',
            NewLine::AlwaysAs(char) => *char,
            NewLine::AfterSpaceAs(char) => {
                if last_ch.is_whitespace() {
                    *char
                } else {
                    ' '
                }
            }
        }
    }
}

// Pretty much only exists because i wanted one of these with
// usize as its builtin type.
#[derive(Debug, Copy, Clone)]
pub struct ScrollOff {
    pub y_gap: usize,
    pub x_gap: usize
}

impl Default for ScrollOff {
    fn default() -> Self {
        ScrollOff { y_gap: 3, x_gap: 3 }
    }
}

#[derive(Debug, Clone)]
pub struct WordChars(Vec<RangeInclusive<char>>);

impl WordChars {
    pub fn new(ranges: Vec<RangeInclusive<char>>) -> Self {
        let word_chars = WordChars(ranges);

        assert!(
            ![' ', '\t', '\n'].into_iter().any(|char| word_chars.contains(char)),
            "WordChars cannot contain ' ', '\\n' or '\\t'."
        );

        word_chars
    }

    pub fn contains(&self, char: char) -> bool {
        self.0.iter().any(|chars| chars.contains(&char))
    }
}

/// Configuration options for printing.
#[derive(Debug, Clone)]
pub struct PrintCfg {
    /// How to wrap the file.
    pub wrap_method: WrapMethod,
    /// Wether to indent wrapped lines or not.
    pub indent_wrap: bool,
    /// Which places are considered a "tab stop".
    pub tab_stops: TabStops,
    /// Wether (and how) to show new lines.
    pub new_line: NewLine,
    /// The horizontal and vertical gaps between the main
    /// cursor and the edges of a [`Label`][crate::ui::Label].
    pub scrolloff: ScrollOff,
    // NOTE: This is relevant for printing with `WrapMethod::Word`.
    /// Characters that are considered to be part of a word.
    pub word_chars: WordChars
}

impl Default for PrintCfg {
    fn default() -> Self {
        Self {
            wrap_method: WrapMethod::default(),
            indent_wrap: true,
            tab_stops: TabStops(4),
            new_line: NewLine::default(),
            scrolloff: ScrollOff::default(),
            word_chars: WordChars::new(vec!['A'..='Z', 'a'..='z', '_'..='_'])
        }
    }
}
