mod cfg;
mod chars;
pub mod reader;
mod tags;
mod types;

use std::{
    io::Write,
    ops::{Range, RangeInclusive}
};

pub use cfg::*;
use chars::Chars;
use ropey::Rope;
pub use tags::{Handle, InsertionTag as Tag};
use tags::{RawTag, TagOrSkip, Tags};
pub use types::{BuilderTag, Part};

use crate::{
    forms::{EXTRA_SEL, MAIN_SEL},
    history::Change,
    position::{Cursor, Point}
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

    pub fn tag_with(&mut self, handle: Handle) -> Tagger {
        Tagger { chars: &self.chars, tags: &mut self.tags, handle }
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

    pub fn close_visual_line_start(&self, pos: usize) -> Option<usize> {
        if pos == 0 {
            return Some(0);
        }
        // NOTE: 20000 is a magic number, being a guess for what a reasonable
        // limit would be.
        self.rev_iter_at(pos).take(20000).find_map(|(pos, _, part)| {
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
}

// Iterator methods.
impl Text {
    pub fn iter(&self) -> Iter<'_> {
        let chars = self.chars.iter_at(0);
        let tags = self.tags.iter_at(0);

        Iter::new(chars, tags, &self.tags.texts, 0, 0)
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

        Iter::new(chars, tags, &self.tags.texts, start, line)
            .take_while(move |(pos, ..)| *pos < end)
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
    handle: Handle,
    toggles: Vec<(Box<dyn Fn(Point) + Send + Sync>, Box<dyn Fn(Point) + Send + Sync>)>
}

impl TextBuilder {
    pub fn push_text(&mut self, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let edit_len = edit.chars().count();
        self.text.chars.as_mut_string().unwrap().push_str(edit);

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
        self.text.chars.as_mut_string().unwrap().push_str(edit);

        self.add_to_last_skip(edit_len);
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well
    /// as its inverse at the end of the [`Text<U>`].
    pub fn push_tag(&mut self, builder_tag: BuilderTag) {
        let raw_tag = builder_tag.into_raw(self.handle, &mut self.toggles);
        let tags = self.text.tags.as_mut_vec().unwrap();
        tags.push(TagOrSkip::Tag(raw_tag));
        if let Some(inv_tag) = raw_tag.inverse() {
            tags.push(TagOrSkip::Tag(inv_tag));
        }
    }

    /// Replaces a range with a new piece of text.
    pub fn swap_range(&mut self, index: usize, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let swap_index = self.swappables[index];

        let Some((start, skip)) = self.get_mut_skip(swap_index) else {
            return;
        };
        let old_skip = *skip;
        *skip = edit.chars().count();

        self.text.chars.replace(start..(start + old_skip), edit);
    }

    pub fn swap_tag(&mut self, tag_index: usize, builder_tag: BuilderTag) {
        let raw_tag = builder_tag.into_raw(self.handle, &mut self.toggles);
        let tags = self.text.tags.as_mut_vec().unwrap();
        let mut iter = tags.iter_mut().enumerate().filter_map(|(index, t_or_s)| match t_or_s {
            TagOrSkip::Tag(RawTag::PopForm(..)) => None,
            TagOrSkip::Tag(tag) => Some((index, tag)),
            TagOrSkip::Skip(_) => None
        });

        if let Some((index, tag)) = iter.nth(tag_index) {
            let inv_tag = tag.inverse();

            *tag = raw_tag;
            let forward = match &tags[index + 1] {
                TagOrSkip::Tag(_) => 1,
                TagOrSkip::Skip(_) => 2
            };

            if let Some(new_inv_tag) = raw_tag.inverse() {
                if inv_tag.is_some() {
                    tags[index + forward] = TagOrSkip::Tag(new_inv_tag);
                } else {
                    tags.insert(index + forward, TagOrSkip::Tag(new_inv_tag));
                }
            } else if inv_tag.is_some() {
                tags.remove(index + forward);
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
        let tags = self.text.tags.as_mut_vec().unwrap();

        let mut iter = tags.iter().enumerate().rev();
        while let Some((index, TagOrSkip::Tag(tag))) = iter.next() {
            if let RawTag::PopForm(..) = tag {
                if let Some(TagOrSkip::Skip(skip)) = tags.get_mut(index) {
                    *skip += edit_len;
                } else {
                    tags.insert(index, TagOrSkip::Skip(edit_len));
                }
                return;
            }
        }

        tags.push(TagOrSkip::Skip(edit_len));
    }

    pub fn clear(&mut self) {
        self.text.clear();
        self.swappables.clear();
    }

    pub fn truncate(&mut self, range_index: usize) {
        let tags = self.text.tags.as_mut_vec().unwrap();

        let Some(&swap_index) = self.swappables.get(range_index) else {
            return;
        };
        let (mut index, cutoff) = tags
            .iter_mut()
            .enumerate()
            .filter_map(|(index, t_or_s)| t_or_s.as_skip().map(|skip| (index, skip)))
            .scan(0, |accum, (index, skip)| {
                let prev_accum = *accum;
                *accum += *skip;
                Some((index, prev_accum))
            })
            .nth(swap_index)
            .unwrap();

        self.swappables.truncate(range_index);
        self.text.chars.as_mut_string().unwrap().truncate(cutoff);

        let mut iter = tags.iter().take(index).rev();
        while let Some(TagOrSkip::Tag(tag, ..)) = iter.next() {
            if let RawTag::PopForm(..) = tag {
                break;
            }

            index -= 1;
        }
        tags.truncate(index);
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
            handle: Handle::default(),
            toggles: Vec::default()
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

    pub fn is_tag(&self) -> bool {
        !self.is_char()
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
    pos: usize,
    line: usize,
    conceal_count: usize,
    texts: &'a [Text],
    backup_iters: Vec<(usize, chars::Iter<'a>, tags::Iter<'a>)>,

    ghosts: bool,
    conceals: Conceal<'a>
}

impl<'a> Iter<'a> {
    fn new(
        chars: chars::Iter<'a>, tags: tags::Iter<'a>, texts: &'a [Text], pos: usize, line: usize
    ) -> Self {
        Self {
            chars,
            tags,
            pos,
            line,
            conceal_count: 0,
            texts,
            backup_iters: Vec::new(),
            ghosts: true,
            conceals: Conceal::All
        }
    }

    pub fn no_conceals(self) -> Self {
        Self { conceals: Conceal::None, ..self }
    }

    pub fn dont_conceal_containing(self, list: &'a [Cursor]) -> Self {
        Self { conceals: Conceal::Excluding(list), ..self }
    }

    pub fn no_ghosts(self) -> Self {
        Self { ghosts: false, ..self }
    }

    fn peek_valid_tag(&mut self) -> Option<(usize, RawTag)> {
        self.tags.peek().filter(|(pos, _)| *pos <= self.pos || self.conceal_count > 0).cloned()
    }
}

impl Iterator for Iter<'_> {
    type Item = (usize, usize, Part);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        let mut tag = self.peek_valid_tag();
        while let Some((
            pos,
            RawTag::ConcealStart(_) | RawTag::ConcealEnd(_) | RawTag::GhostText(..)
        )) = tag
        {
            self.tags.next();

            match tag.unwrap() {
                (_, RawTag::ConcealStart(_)) => self.conceal_count += 1,
                (_, RawTag::ConcealEnd(_)) => {
                    self.conceal_count = self.conceal_count.saturating_sub(1);
                    if self.conceal_count == 0 {
                        self.pos = self.pos.max(pos);
                        self.line = self.chars.move_to(self.pos);
                    }
                }
                (_, RawTag::GhostText(id, _)) if self.ghosts => {
                    if let Some(text) = self.texts.get(id) {
                        let iter = if pos >= self.pos {
                            text.iter()
                        } else {
                            text.iter_at(text.len_chars())
                        };

                        let pos = std::mem::replace(&mut self.pos, iter.pos);
                        let chars = std::mem::replace(&mut self.chars, iter.chars);
                        let tags = std::mem::replace(&mut self.tags, iter.tags);

                        self.backup_iters.push((pos, chars, tags));
                    }
                }
                _ => {}
            }

            tag = self.peek_valid_tag()
        }

        if let Some((pos, tag)) = tag {
            if let RawTag::Concealed(skip) = tag {
                self.pos = pos.saturating_add(skip);
                self.tags.move_to(self.pos);
                self.line = self.chars.move_to(self.pos);
                self.conceal_count = 0;
            }
            self.tags.next();
            Some((pos, self.line, Part::from(tag)))
        } else if let Some(char) = self.chars.next().or_else(|| {
            self.backup_iters.pop().and_then(|(pos, chars, tags)| {
                (self.pos, self.chars, self.tags) = (pos, chars, tags);
                self.chars.next()
            })
        }) {
            let prev_line = self.line;
            self.pos += 1;
            let pos = if let Some(pos) = self.backup_iters.first().map(|(pos, ..)| *pos) {
                pos
            } else {
                self.line += (char == '\n') as usize;
                self.pos - 1
            };
            Some((pos, prev_line, Part::Char(char)))
        } else {
            None
        }
    }
}

/// An [`Iterator`] over the [`Part`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
#[derive(Clone)]
pub struct RevIter<'a> {
    chars: chars::Iter<'a>,
    tags: tags::RevIter<'a>,
    pos: usize,
    line: usize,
    conceal_count: usize,
    texts: &'a [Text],
    backup_iters: Vec<(usize, chars::Iter<'a>, tags::RevIter<'a>)>,

    // Iteration options:
    ghosts: bool,
    conceals: Conceal<'a>
}

impl<'a> RevIter<'a> {
    pub fn new(
        chars: chars::Iter<'a>, tags: tags::RevIter<'a>, texts: &'a [Text], pos: usize, line: usize
    ) -> Self {
        Self {
            chars,
            tags,
            pos,
            line,
            conceal_count: 0,
            texts,
            backup_iters: Vec::new(),
            ghosts: true,
            conceals: Conceal::All
        }
    }

    pub fn no_conceals(self) -> Self {
        Self { conceals: Conceal::None, ..self }
    }

    pub fn dont_conceal_containing(self, list: &'a [Cursor]) -> Self {
        Self { conceals: Conceal::Excluding(list), ..self }
    }

    pub fn dont_conceal_on_lines(self, list: &'a [Cursor]) -> Self {
        Self { conceals: Conceal::NotOnLineOf(list), ..self }
    }

    pub fn no_ghosts(self) -> Self {
        Self { ghosts: false, ..self }
    }
}

impl Iterator for RevIter<'_> {
    type Item = (usize, usize, Part);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        let mut tag =
            self.tags.peek().filter(|(pos, _)| *pos >= self.pos || self.conceal_count > 0).cloned();
        while let Some((
            pos,
            RawTag::ConcealStart(_) | RawTag::ConcealEnd(_) | RawTag::GhostText(..)
        )) = tag
        {
            let prev_conceal_count = self.conceal_count;

            self.tags.next();

            match tag.unwrap() {
                (_, RawTag::ConcealStart(_)) => {
                    self.conceal_count = self.conceal_count.saturating_sub(1)
                }
                (_, RawTag::ConcealEnd(_)) => self.conceal_count += 1,
                (_, RawTag::GhostText(id, _)) if self.ghosts => {
                    if let Some(text) = self.texts.get(id) {
                        let iter =
                            if pos <= self.pos { text.rev_iter() } else { text.rev_iter_at(0) };

                        let pos = std::mem::replace(&mut self.pos, iter.pos);
                        let chars = std::mem::replace(&mut self.chars, iter.chars);
                        let tags = std::mem::replace(&mut self.tags, iter.tags);

                        self.backup_iters.push((pos, chars, tags));
                    }
                }
                _ => {}
            }

            if self.conceal_count == 0 && prev_conceal_count == 1 {
                self.pos = self.pos.min(pos);
                self.line = self.chars.move_to(self.pos);
            }

            tag = self
                .tags
                .peek()
                .filter(|(pos, _)| *pos >= self.pos || self.conceal_count > 0)
                .cloned();
        }

        if let Some((pos, tag)) = tag {
            if let RawTag::Concealed(skip) = tag {
                self.pos = pos.saturating_sub(skip);
                self.tags.move_to(self.pos);
                self.line = self.chars.move_to(self.pos);
                self.conceal_count = 0;
            }
            self.tags.next();
            Some((pos, self.line, Part::from(tag)))
        } else if let Some(char) = self.chars.next().or_else(|| {
            self.backup_iters.pop().and_then(|(pos, chars, tags)| {
                (self.pos, self.chars, self.tags) = (pos, chars, tags);
                self.chars.next()
            })
        }) {
            self.pos -= 1;
            let pos = if let Some(pos) = self.backup_iters.first().map(|(pos, ..)| *pos) {
                pos
            } else {
                self.line -= (char == '\n') as usize;
                self.pos
            };
            Some((pos, self.line, Part::Char(char)))
        } else {
            None
        }
    }
}

#[derive(Clone, Default)]
enum Conceal<'a> {
    #[default]
    All,
    None,
    Excluding(&'a [Cursor]),
    NotOnLineOf(&'a [Cursor])
}

fn cursor_tags(is_main: bool) -> (Tag, Tag, Tag) {
    if is_main {
        (Tag::MainCursor, Tag::PushForm(MAIN_SEL), Tag::PopForm(MAIN_SEL))
    } else {
        (Tag::MainCursor, Tag::PushForm(EXTRA_SEL), Tag::PopForm(EXTRA_SEL))
    }
}
