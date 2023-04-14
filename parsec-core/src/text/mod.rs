pub mod inner;
pub mod reader;

use std::{
    iter::Peekable,
    ops::{Range, RangeInclusive},
};

use ropey::Rope;

use self::{inner::InnerText, reader::MutTextReader};
use crate::{
    config::{Config, WrapMethod},
    history::Change,
    position::{Cursor, Pos},
    tags::{
        form::{EXTRA_SEL, MAIN_SEL},
        Lock, Tag, TagOrSkip, Tags,
    },
    ui::{Area, Label, Ui},
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
pub struct TextBuilder<U>
where
    U: Ui,
{
    text: Text<U>,
    swappables: Vec<usize>,
}

impl<U> Default for TextBuilder<U>
where
    U: Ui,
{
    fn default() -> Self {
        TextBuilder {
            text: Text::default_string(),
            swappables: Vec::default(),
        }
    }
}

impl<U> TextBuilder<U>
where
    U: Ui,
{
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
            .vec()
            .iter()
            .filter(|tag_or_skip| matches!(tag_or_skip, TagOrSkip::Skip(_)))
            .count();

        self.swappables.push(last_skip);
        self.text.inner.string().push_str(edit);

        self.add_to_last_skip(edit_len);
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well
    /// as its inverse at the end of the [`Text<U>`].
    pub fn push_tag(&mut self, tag: Tag) -> Lock {
        let lock = self.text.tags.get_lock();
        self.text.tags.vec().push(TagOrSkip::Tag(tag, lock));
        if let Some(inv_tag) = tag.inverse() {
            self.text.tags.vec().push(TagOrSkip::Tag(inv_tag, lock));
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
        let tags_vec = self.text.tags.vec();
        let mut tags =
            tags_vec
                .iter_mut()
                .enumerate()
                .filter_map(|(index, tag_or_skip)| match tag_or_skip {
                    TagOrSkip::Tag(Tag::PopForm(_), _) => None,
                    TagOrSkip::Tag(tag, lock) => Some((index, tag, *lock)),
                    TagOrSkip::Skip(_) => None,
                });

        if let Some((index, tag, lock)) = tags.nth(tag_index) {
            let inv_tag = tag.inverse();

            *tag = new_tag;

            if let Some(new_inv_tag) = new_tag.inverse() {
                let forward = match &tags_vec[index + 1] {
                    TagOrSkip::Tag(_, _) => 1,
                    TagOrSkip::Skip(_) => 2,
                };

                if let Some(_) = inv_tag {
                    tags_vec[index + forward] = TagOrSkip::Tag(new_inv_tag, lock);
                } else {
                    tags_vec.insert(index + 1, TagOrSkip::Tag(new_inv_tag, lock));
                }
            } else {
                tags_vec.remove(index + 1);
            }
        }
    }

    pub fn get_mut_skip(&mut self, index: usize) -> Option<(usize, &mut u32)> {
        self.text
            .tags
            .vec()
            .iter_mut()
            .filter_map(|tag_or_skip| match tag_or_skip {
                TagOrSkip::Skip(skip) => Some(skip),
                TagOrSkip::Tag(..) => None,
            })
            .scan(0, |accum, skip| {
                let prev_accum = *accum;
                *accum += *skip as usize;
                Some((prev_accum, skip))
            })
            .nth(index)
    }

    fn add_to_last_skip(&mut self, edit_len: u32) {
        let tags_vec = self.text.tags.vec();
        let Some(tag_or_skip) = tags_vec.last_mut() else {
            return;
        };

        match tag_or_skip {
            TagOrSkip::Tag(Tag::PopForm(_), _) => {
                let prev = tags_vec.len() - 1;
                if let Some(TagOrSkip::Skip(skip)) = tags_vec.get_mut(prev) {
                    *skip += edit_len;
                } else {
                    tags_vec.insert(prev, TagOrSkip::Skip(edit_len));
                }
            }
            TagOrSkip::Tag(..) => tags_vec.push(TagOrSkip::Skip(edit_len)),
            TagOrSkip::Skip(skip) => *skip += edit_len,
        }
    }

    pub fn clear(&mut self) {
        self.text.clear();
        self.swappables.clear();
    }

    pub fn truncate(&mut self, range_index: usize) {
        let tags_vec = self.text.tags.vec();

        let Some(swap_index) = self.swappables.get(range_index) else {
            return;
        };
        let (cutoff, index) = tags_vec
            .iter_mut()
            .enumerate()
            .filter_map(|(index, tag_or_skip)| match tag_or_skip {
                TagOrSkip::Skip(skip) => Some((index, skip)),
                TagOrSkip::Tag(..) => None,
            })
            .scan(0, |accum, (index, skip)| {
                let prev_accum = *accum;
                *accum += *skip as usize;
                Some((prev_accum, index))
            })
            .nth(*swap_index)
            .unwrap();

        self.swappables.truncate(range_index);
        self.text.inner.string().truncate(cutoff);
        tags_vec.truncate(index);
    }

    pub fn ranges_len(&self) -> usize {
        self.swappables.len()
    }

    pub fn text(&self) -> &Text<U> {
        &self.text
    }
}

/// The text in a given area.
pub struct Text<U>
where
    U: Ui + ?Sized,
{
    pub inner: InnerText,
    pub tags: Tags,
    lock: Lock,
    _replacements: Vec<(Vec<Text<U>>, RangeInclusive<usize>, bool)>,
    _readers: Vec<Box<dyn MutTextReader<U>>>,
}

// TODO: Properly implement _replacements.
impl<U> Text<U>
where
    U: Ui,
{
    pub fn default_string() -> Self {
        let mut tags = Tags::default_vec();
        let lock = tags.get_lock();
        Text {
            inner: InnerText::String(String::default()),
            tags,
            lock,
            _replacements: Vec::new(),
            _readers: Vec::new(),
        }
    }

    pub fn default_rope() -> Self {
        let mut tags = Tags::default_rope();
        let lock = tags.get_lock();
        Text {
            inner: InnerText::Rope(Rope::default()),
            tags,
            lock,
            _replacements: Vec::new(),
            _readers: Vec::new(),
        }
    }

    pub fn new_string(string: impl ToString) -> Self {
        let inner = InnerText::String(string.to_string());
        let mut tags = Tags::new(&inner);
        let lock = tags.get_lock();
        Text {
            inner,
            tags,
            lock,
            _replacements: Vec::new(),
            _readers: Vec::new(),
        }
    }

    pub fn new_rope(string: impl ToString) -> Self {
        let inner = InnerText::Rope(Rope::from(string.to_string()));
        let mut tags = Tags::new(&inner);
        let lock = tags.get_lock();
        Text {
            inner,
            tags,
            lock,
            _replacements: Vec::new(),
            _readers: Vec::new(),
        }
    }

    /// Prints the contents of a given area in a given `EndNode`.
    pub(crate) fn print(&self, label: &mut U::Label, config: &Config, print_info: PrintInfo) {
        let cur_char = {
            let first_line = self.inner.char_to_line(print_info.first_char);
            self.inner.line_to_char(first_line)
        };

        let chars = self.inner.chars_at(cur_char);
        let tags = self.tags.iter_at(cur_char).peekable();

        let text_iter = TextIter {
            chars,
            tags,
            cur_char,
        };

        label.print(text_iter, print_info, config);
    }

    /// Merges `String`s with the body of text, given a range to
    /// replace.
    fn replace_range(&mut self, old: Range<usize>, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let edit_len = edit.chars().count();
        let new = old.start..(old.start + edit_len);

        self.inner.replace(old.start..old.end, edit);

        if old != new {
            self.tags.transform_range(old, new);
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

    pub fn inner(&self) -> &InnerText {
        &self.inner
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len_chars() == 0
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
}

/// A part of the [`Text`], can be a [`char`] or a [`Tag`].
pub enum TextBit {
    Tag(Tag),
    Char(char),
}

/// An [`Iterator`] over the [`TextBit`]s of the [`Text`].
///
/// This is useful for both printing and measurement of [`Text`], and
/// can incorporate string replacements as part of its design.
pub struct TextIter<CI, TI>
where
    CI: Iterator<Item = char>,
    TI: Iterator<Item = (usize, Tag)>,
{
    chars: CI,
    tags: Peekable<TI>,
    cur_char: usize,
}

impl<CI, TI> Iterator for TextIter<CI, TI>
where
    CI: Iterator<Item = char>,
    TI: Iterator<Item = (usize, Tag)>,
{
    type Item = (usize, TextBit);

    fn next(&mut self) -> Option<Self::Item> {
        // `<=` because some `Tag`s may be triggered before printing.
        if let Some(&(index, tag)) = self.tags.peek().filter(|(index, _)| *index <= self.cur_char) {
            self.tags.next();
            Some((index, TextBit::Tag(tag)))
        } else if let Some(char) = self.chars.next() {
            self.cur_char += 1;
            Some((self.cur_char, TextBit::Char(char)))
        } else {
            None
        }
    }
}

// NOTE: The defaultness in here, when it comes to `last_main`, may
// cause issues in the future.
/// Information about how to print the file on the `Label`.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct PrintInfo {
    /// The index of the first [char] that should be printed on the
    /// screen.
    pub first_char: usize,
    /// How shifted the text is to the left.
    pub x_shift: usize,
    /// The last position of the main cursor.
    pub last_main: Pos,
}

impl PrintInfo {
    /// Scrolls up until the gap between the main cursor and the top
    /// of the widget is equal to `config.scrolloff.y_gap`.
    fn scroll_up_to_gap<U>(
        &mut self,
        target: Pos,
        inner: &InnerText,
        label: &U::Label,
        config: &Config,
    ) where
        U: Ui,
    {
        let max_dist = config.scrolloff.y_gap;
        let (wrap_method, tab_places) = (config.wrap_method, &config.tab_places);

        let slice = inner.slice(..target.true_char());
        let mut lines =
            slice
                .lines_at(target.true_row())
                .reversed()
                .scan(target.true_char(), |ch, line| {
                    *ch -= line.len_chars();
                    Some((*ch, line))
                });

        let mut accum = 0;
        while let Some((line_ch, line)) = lines.next() {
            accum += 1 + label.wrap_count(line, wrap_method, tab_places);
            if accum >= max_dist && line_ch >= self.first_char {
                return;
            } else if accum >= max_dist {
                // `max_dist - accum` is the amount of wraps that should be offscreen.
                self.first_char =
                    line_ch + label.col_at_wrap(line, accum - max_dist, wrap_method, tab_places);
                return;
            }
        }

        // This means we can't scroll up anymore.
        if accum < max_dist {
            self.first_char = 0;
        }
    }

    /// Scrolls down until the gap between the main cursor and the
    /// bottom of the widget is equal to `config.scrolloff.y_gap`.
    fn scroll_down_to_gap<U>(
        &mut self,
        target: Pos,
        inner: &InnerText,
        label: &U::Label,
        config: &Config,
    ) where
        U: Ui,
    {
        let max_dist = label.area().height() - config.scrolloff.y_gap;
        let (wrap_method, tab_places) = (config.wrap_method, &config.tab_places);

        let slice = inner.slice(..target.true_char());
        let mut lines =
            slice
                .lines_at(slice.len_lines())
                .reversed()
                .scan(target.true_char(), |ch, line| {
                    *ch -= line.len_chars();
                    Some((*ch, line))
                });
        let mut lines_to_top = target.true_row() - inner.char_to_line(self.first_char);

        let mut accum = 0;
        while let Some((line_ch, line)) = lines.next() {
            lines_to_top = lines_to_top.saturating_sub(1);
            accum += 1 + label.wrap_count(line, wrap_method, tab_places);
            if accum >= max_dist {
                // `accum - gap` is the amount of wraps that should be offscreen.
                self.first_char =
                    line_ch + label.col_at_wrap(line, accum - max_dist, wrap_method, tab_places);
                break;
            // We have reached the top of the screen before the accum
            // equaled gap. This means that no scrolling
            // actually needs to take place.
            } else if lines_to_top == 0 {
                break;
            }
        }
    }

    /// Scrolls the file horizontally, usually when no wrapping is
    /// being used.
    fn scroll_hor_to_gap<U>(
        &mut self,
        target: Pos,
        inner: &InnerText,
        label: &U::Label,
        config: &Config,
    ) where
        U: Ui,
    {
        let max_dist = label.area().width() - config.scrolloff.x_gap;
        let (_wrap_method, tab_places) = (config.wrap_method, &config.tab_places);

        let _slice = inner.slice(..target.true_char());
        let line = inner.line(target.true_row());

        let target_dist = label.get_width(line, tab_places);
        self.x_shift = target_dist.saturating_sub(max_dist);
    }

    /// Updates the print info, according to a [`Config`]'s
    /// specifications.
    pub fn update<U>(&mut self, target: Pos, inner: &InnerText, label: &U::Label, config: &Config)
    where
        U: Ui,
    {
        if let WrapMethod::NoWrap = config.wrap_method {
            self.scroll_hor_to_gap::<U>(target, inner, label, config);
        }

        if target < self.last_main {
            self.scroll_up_to_gap::<U>(target, inner, label, config);
        } else if target > self.last_main {
            self.scroll_down_to_gap::<U>(target, inner, label, config);
        }

        self.last_main = target;
    }
}

fn cursor_tags(is_main: bool) -> (Tag, Tag, Tag) {
    if is_main {
        (Tag::MainCursor, Tag::PushForm(MAIN_SEL), Tag::PopForm(MAIN_SEL))
    } else {
        (Tag::MainCursor, Tag::PushForm(EXTRA_SEL), Tag::PopForm(EXTRA_SEL))
    }
}
