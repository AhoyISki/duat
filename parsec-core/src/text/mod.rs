mod cfg;
mod chars;
pub mod reader;
mod tags;

use std::{
    io::Write,
    iter::Peekable,
    ops::{Range, RangeBounds, RangeInclusive}
};

pub use cfg::*;
use chars::Chars;
use ropey::Rope;
pub use tags::{Handle, RawTag, Tag};
use tags::{TagOrSkip, Tags, ToggleId};

use crate::{
    forms::{FormId, EXTRA_SEL, MAIN_SEL},
    history::Change,
    position::Cursor
};

/// The text in a given area.
pub struct Text {
    chars: Chars,
    tags: Tags,
    handle: Handle,
    _replacements: Vec<(Vec<Text>, RangeInclusive<usize>, bool)>
}

// TODO: Properly implement _replacements.
impl Text {
    pub fn default_string() -> Self {
        Text {
            chars: Chars::String(String::default()),
            tags: Tags::default_vec(),
            handle: Handle::default(),
            _replacements: Vec::new()
        }
    }

    pub fn default_rope() -> Self {
        Text {
            chars: Chars::Rope(Rope::default()),
            tags: Tags::default_rope(),
            handle: Handle::default(),
            _replacements: Vec::new()
        }
    }

    pub fn new_string(string: impl ToString) -> Self {
        let chars = Chars::String(string.to_string());
        let tags = Tags::new(&chars);
        Text { chars, tags, handle: Handle::default(), _replacements: Vec::new() }
    }

    pub fn new_rope(string: impl ToString) -> Self {
        let chars = Chars::Rope(Rope::from(string.to_string()));
        let tags = Tags::new(&chars);
        Text { chars, tags, handle: Handle::default(), _replacements: Vec::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.chars.len_chars() == 0
    }

    pub fn get_char(&self, char_index: usize) -> Option<char> {
        self.chars.get_char(char_index)
    }

    pub fn tag_with(&mut self, handle: Handle) -> Tagger {
        Tagger { chars: &self.chars, tags: &mut self.tags, handle }
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

    fn clear(&mut self) {
        self.chars.clear();
        self.tags.clear();
    }

    pub fn char_to_line(&self, char: usize) -> usize {
        self.chars.char_to_line(char).unwrap_or_else(|| panic!("Char index {char} out of bounds."))
    }

    pub fn line_to_char(&self, line: usize) -> usize {
        self.chars.line_to_char(line).unwrap_or_else(|| panic!("Line index {line} out of bounds."))
    }

    pub fn char_to_byte(&self, char: usize) -> usize {
        self.chars.char_to_byte(char).unwrap_or_else(|| panic!("Char index {char} out of bounds."))
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
    pub(crate) fn add_cursor_tags(&mut self, cursors: &[Cursor], main_index: usize) {
        for (index, cursor) in cursors.iter().enumerate() {
            let Range { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = cursor_tags(index == main_index);

            let pos_list =
                [(start, start_tag), (end, end_tag), (cursor.caret().true_char(), caret_tag)];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.into_iter().skip(no_selection) {
                self.tags.insert(pos, tag, self.handle);
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
                self.tags.remove_on(ch_index, self.handle);
            }
        }
    }

    pub(crate) fn write_to(
        &self, mut writer: std::io::BufWriter<std::fs::File>
    ) -> Result<usize, String> {
        match &self.chars {
            Chars::String(string) => writer.write(string.as_bytes()).map_err(|err| err.to_string()),
            Chars::Rope(rope) => {
                rope.write_to(writer).map(|_| rope.len_bytes()).map_err(|err| err.to_string())
            }
        }
    }
}

// Iterator methods.
impl Text {
    pub fn iter(&self) -> impl Iterator<Item = (usize, usize, Part)> + Clone + '_ {
        let chars = self.chars.iter_at(0);
        let tags = self.tags.iter_at(0);

        Iter::new(chars, tags, 0, 0)
    }

    /// TO BE DEPRECATED.
    pub fn iter_line(
        &self, line: usize
    ) -> impl Iterator<Item = (usize, usize, Part)> + Clone + '_ {
        let start = self.line_to_char(line);
        let end = self.get_line_to_char(line + 1).unwrap_or(start);
        let chars = self.chars.iter_at(start);

        let tags_start = start.saturating_sub(self.tags.back_check_amount());
        let tags = self.tags.iter_at(tags_start);

        Iter::new(chars, tags, start, line).take_while(move |(pos, ..)| *pos < end)
    }

    pub fn iter_range(
        &self, range: impl RangeBounds<usize>
    ) -> impl Iterator<Item = (usize, usize, Part)> + '_ {
        let start = match range.start_bound() {
            std::ops::Bound::Included(start) => *start,
            std::ops::Bound::Excluded(start) => *start + 1,
            std::ops::Bound::Unbounded => 0
        };

        let end = match range.end_bound() {
            std::ops::Bound::Included(end) => end + 1,
            std::ops::Bound::Excluded(end) => *end,
            std::ops::Bound::Unbounded => self.chars.len_chars() + 1
        };

        let chars = self.chars.iter_at(start);

        let line = self.char_to_line(start);
        let tags_start = start.saturating_sub(self.tags.back_check_amount());
        let tags = self.tags.iter_at(tags_start);

        Iter::new(chars, tags, start, line).take_while(move |(pos, ..)| *pos < end)
    }

    pub fn iter_chars_at(&self, char: usize) -> impl Iterator<Item = char> + '_ {
        self.chars.iter_at(char)
    }

    pub fn iter_line_chars(&self, line: usize) -> impl Iterator<Item = char> + '_ {
        self.iter_line(line).filter_map(|(.., bit)| bit.as_char())
    }
}
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
pub struct TextBuilder {
    text: Text,
    swappables: Vec<usize>,
    handle: Handle
}

impl TextBuilder {
    pub fn push_text(&mut self, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let edit_len = edit.chars().count();
        self.text.chars.string().push_str(edit);

        self.add_to_last_skip(edit_len);
    }

    pub fn push_swappable(&mut self, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let edit_len = edit.chars().count();

        let last_skip = self
            .text
            .tags
            .as_vec()
            .unwrap()
            .iter()
            .filter(|t_or_s| matches!(t_or_s, TagOrSkip::Skip(_)))
            .count();

        self.swappables.push(last_skip);
        self.text.chars.string().push_str(edit);

        self.add_to_last_skip(edit_len);
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well
    /// as its inverse at the end of the [`Text<U>`].
    pub fn push_tag(&mut self, tag: RawTag) {
        let tags = self.text.tags.as_mut_vec().unwrap();
        tags.push(TagOrSkip::Tag(tag, self.handle));
        if let Some(inv_tag) = tag.inverse() {
            tags.push(TagOrSkip::Tag(inv_tag, self.handle));
        }
    }

    /// Replaces a range with a new piece of text.
    pub fn swap_range(&mut self, index: usize, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let swap_index = self.swappables[index];

        let Some((start, skip)) = self.get_mut_skip(swap_index) else {
            return;
        };
        let old_skip = *skip as usize;
        *skip = edit.chars().count();

        self.text.chars.replace(start..(start + old_skip), edit);
    }

    pub fn swap_tag(&mut self, tag_index: usize, new_tag: RawTag) {
        let tags_vec = self.text.tags.as_mut_vec().unwrap();
        let mut tags = tags_vec.iter_mut().enumerate().filter_map(|(index, t_or_s)| match t_or_s {
            TagOrSkip::Tag(RawTag::PopForm(_), _) => None,
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
                if inv_tag.is_some() {
                    tags_vec[index + forward] = TagOrSkip::Tag(new_inv_tag, lock);
                } else {
                    tags_vec.insert(index + forward, TagOrSkip::Tag(new_inv_tag, lock));
                }
            } else if inv_tag.is_some() {
                tags_vec.remove(index + forward);
            }
        }
    }

    pub fn get_mut_skip(&mut self, index: usize) -> Option<(usize, &mut usize)> {
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
                *accum += *skip;
                Some((prev_accum, skip))
            })
            .nth(index)
    }

    fn add_to_last_skip(&mut self, edit_len: usize) {
        let tags_vec = self.text.tags.as_mut_vec().unwrap();

        let mut tags = tags_vec.iter().enumerate().rev();
        while let Some((index, TagOrSkip::Tag(tag, _))) = tags.next() {
            if let RawTag::PopForm(_) = tag {
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
        self.text.chars.string().truncate(cutoff);

        let mut tags = tags_vec.iter().take(index).rev();
        while let Some(TagOrSkip::Tag(tag, ..)) = tags.next() {
            if let RawTag::PopForm(_) = tag {
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
            swappables: Vec::default(),
            handle: Handle::default()
        }
    }
}

pub struct Tagger<'a> {
    chars: &'a Chars,
    tags: &'a mut Tags,
    handle: Handle
}

impl<'a> Tagger<'a> {
    pub fn tags(&self) -> impl Iterator<Item = (usize, RawTag)> + '_ {
        self.tags.iter_at(0)
    }

    pub fn is_empty(&self) -> bool {
        self.tags.is_empty()
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
        self.tags.insert(char, tag, self.handle)
    }

    pub fn remove_on(&mut self, ch_index: usize) {
        self.tags.remove_on(ch_index, self.handle)
    }

    pub fn len_bytes(&self) -> usize {
        self.chars.len_bytes()
    }

    pub fn len_chars(&self) -> usize {
        self.chars.len_chars()
    }

    pub fn len_lines(&self) -> usize {
        self.chars.len_lines()
    }
}

/// A part of the [`Text`], can be a [`char`] or a [`Tag`].
pub enum Part {
    Char(char),
    PushForm(FormId),
    PopForm(FormId),
    MainCursor,
    ExtraCursor,
    AlignLeft,
    AlignCenter,
    AlignRight,
    HoverStart(ToggleId),
    HoverEnd(ToggleId),
    LeftButtonStart(ToggleId),
    LeftButtonEnd(ToggleId),
    RightButtonStart(ToggleId),
    RightButtonEnd(ToggleId),
    MiddleButtonStart(ToggleId),
    MiddleButtonEnd(ToggleId)
}

impl From<RawTag> for Part {
    fn from(value: RawTag) -> Self {
        match value {
            RawTag::PushForm(id) => Part::PushForm(id),
            RawTag::PopForm(id) => Part::PopForm(id),
            RawTag::MainCursor => Part::MainCursor,
            RawTag::ExtraCursor => Part::ExtraCursor,
            RawTag::AlignLeft => Part::AlignLeft,
            RawTag::AlignCenter => Part::AlignCenter,
            RawTag::AlignRight => Part::AlignRight,
            RawTag::HoverStart(id) => Part::HoverStart(id),
            RawTag::HoverEnd(id) => Part::HoverEnd(id),
            RawTag::LeftButtonStart(id) => Part::LeftButtonStart(id),
            RawTag::LeftButtonEnd(id) => Part::LeftButtonEnd(id),
            RawTag::RightButtonStart(id) => Part::RightButtonStart(id),
            RawTag::RightButtonEnd(id) => Part::RightButtonEnd(id),
            RawTag::MiddleButtonStart(id) => Part::MiddleButtonStart(id),
            RawTag::MiddleButtonEnd(id) => Part::MiddleButtonEnd(id),
            RawTag::ConcealStart | RawTag::ConcealEnd | RawTag::Skip(_) | RawTag::GhostText(_) => unsafe {
                std::hint::unreachable_unchecked()
            }
        }
    }
}

impl Part {
    /// Returns `true` if the text bit is [`Char`].
    ///
    /// [`Char`]: TextBit::Char
    #[must_use]
    pub fn is_char(&self) -> bool {
        matches!(self, Part::Char(_))
    }

    pub fn as_char(&self) -> Option<char> {
        if let Self::Char(v) = self { Some(*v) } else { None }
    }
}

/// An [`Iterator`] over the [`TextBit`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
#[derive(Clone)]
pub struct Iter<'a> {
    chars: chars::Iter<'a>,
    tags: tags::Iter<'a>,
    char: usize,
    line: usize
}

impl Iter<'_> {
    pub fn new(chars: chars::Iter<'_>, tags: tags::Iter<'_>, char: usize, line: usize) -> Self {
        Self { chars, tags, char, line }
    }
}

impl Iterator for Iter<'_> {
    type Item = (usize, usize, Part);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        // `<=` because some `Tag`s may be triggered before printing.
        let mut tag = self.tags.peek().filter(|(pos, _)| *pos <= self.char).cloned();
        let mut conceal_count = 0;
        while let Some((_, RawTag::ConcealStart | RawTag::ConcealEnd | RawTag::Skip(_))) = tag {
            self.tags.next();
            if let Some((pos, RawTag::Skip(skip))) = tag {
                self.tags.move_forwards_by(skip);
                self.line = self.chars.move_forwards_by(skip);
                conceal_count = 0;
                break;
            } else if let Some((pos, RawTag::ConcealStart)) = tag {
                conceal_count += 1;
                let mut skip = 0;
                let mut tags = self.tags.clone();
                while conceal_count > 0 && let Some((new_pos, tag)) = tags.next() {
                    skip = new_pos - pos;
                    match tag {
                        RawTag::ConcealStart => conceal_count += 1,
                        RawTag::ConcealEnd => conceal_count -= 1,
                        RawTag::MainCursor | RawTag::ExtraCursor => {
                            skip = 0;
                        }
                        _ => {}
                    }
                }

                self.tags.move_forwards_by(skip);
                self.line = self.chars.move_forwards_by(skip);
            }
            let mut tags = self.tags.clone().enumerate();
            self.tags.next();
            let mut conceal_ended = false;
            for (skip, (pos, tag)) in self.tags.clone().enumerate() {
                if let RawTag::ConcealEnd = tag {
                    break;
                }
            }
        }

        if let Some((pos, tag)) = tag {
            self.tags.next();
            Some((pos, self.line, Part::from(tag)))
        } else if let Some(char) = self.chars.next() {
            self.char += 1;
            let prev_line = self.line;
            self.line += (char == '\n') as usize;
            Some((self.char - 1, prev_line, Part::Char(char)))
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
