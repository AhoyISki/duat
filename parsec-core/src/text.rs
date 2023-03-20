pub mod inner_text;
pub mod reader;

use std::{
    iter::Peekable,
    ops::{Range, RangeInclusive},
};

use ropey::{iter::Chars, Rope};

use self::{inner_text::InnerText, reader::MutTextReader};
use crate::{
    config::{Config, WrapMethod},
    history::Change,
    position::{Cursor, Pos},
    tags::{
        form::{FormFormer, EXTRA_SEL, MAIN_SEL},
        Lock, Tag, Tags,
    },
    ui::{Area, EndNode, Label, Ui},
};

/// Builds and modifies a [`Text<U>`], based on replacements applied to it.
///
/// The generation of text by the `TextBuilder<U>` has a few peculiarities
/// that are convenient in the situations where it is useful:
///
/// - The user cannot insert [`Tag`]s directly, only by appending and modifying
/// existing tags.
/// - All [`Tag`]s that are appended result in an inverse [`Tag`] being placed
/// before the next one (e.g. [`Tag::PushForm`] would be followed a [`Tag::PopForm`]),
/// or at the end of the [`Tags`].
/// - You can insert swappable text with [`push_swappable()`][Self::push_swappable].
///
/// These properties allow for quick and easy modification of the [`Text<U>`] within,
/// which can then be accessed with [`text()`][Self::text()].
pub struct TextBuilder<U>
where
    U: Ui,
{
    text: Text<U>,
    ranges: Vec<Range<usize>>,
    tags: Vec<(Tag, usize, usize, Lock)>,
}

impl<U> TextBuilder<U>
where
    U: Ui,
{
    pub fn default_string() -> Self {
        TextBuilder {
            text: Text::default_string(),
            ranges: Vec::new(),
            tags: Vec::new(),
        }
    }

    pub fn default_rope() -> Self {
        TextBuilder {
            text: Text::default_string(),
            ranges: Vec::new(),
            tags: Vec::new(),
        }
    }

    pub fn push_text(&mut self, edit: impl AsRef<str>) {
        let len = self.text.inner.len_chars();

        let edit = edit.as_ref().to_string();
        self.move_last_tag(edit.chars().count());

        let change = Change::new(edit, len..len, self.text.inner());
        self.text.apply_change(&change);
    }

    pub fn push_swappable(&mut self, edit: impl AsRef<str>) {
        let len = self.text.len_chars();
        self.ranges.push(len..(len + edit.as_ref().len()));

        let edit = edit.as_ref().to_string();
        self.move_last_tag(edit.chars().count());

        let change = Change::new(edit, len..len, self.text.inner());
        self.text.apply_change(&change);
    }

    /// Replaces a range with a new piece of text.
    pub fn swap_range(&mut self, index: usize, edit: impl AsRef<str>) {
        let edit = edit.as_ref();
        let range = self.ranges[index].clone();

        let change = Change::new(edit.to_string(), range.clone(), self.text.inner());
        self.text.apply_change(&change);

        let range_diff = edit.chars().count() as isize - range.clone().count() as isize;

        self.ranges[index].end = range.end.saturating_add_signed(range_diff);

        for range in self.ranges.iter_mut().skip(index + 1) {
            range.start = range.start.saturating_add_signed(range_diff);
            range.end = range.end.saturating_add_signed(range_diff);
        }

        for (_, start, end, _) in self.tags.iter_mut() {
            if *start > range.start {
                *start = start.saturating_add_signed(range_diff);
                *end = end.saturating_add_signed(range_diff)
            }
        }
    }

    /// Pushes a [`Tag`] to the end of the list of [`Tag`]s, as well as its
    /// inverse at the end of the [`Text<U>`].
    pub fn push_tag(&mut self, tag: Tag) -> Lock {
        let width = self.text.tags.width();
        let lock = self.text.tags.get_lock();

        self.text.tags.insert(tag, lock, width);

        if let Some(inv_tag) = tag.inverse() {
            self.text.tags.insert(inv_tag, lock, width);
        }
        self.tags.push((tag, width, width, lock));

        lock
    }

    fn move_last_tag(&mut self, edit_len: usize) {
        if let Some((tag, start, end, lock)) = self.tags.last_mut() {
            let new_end = *end + edit_len;
            if let (Some(inv_tag), true) = (tag.inverse(), start == end) {
                self.text.tags.remove(*start, *lock);
                self.text.tags.insert(*tag, *lock, *start);
                self.text
                    .tags
                    .insert(tag.inverse().unwrap(), *lock, new_end)
            }

            *end = new_end;
        }
    }

    pub fn swap_tag(&mut self, tag_index: usize, new_tag: Tag) {
        let (tag, start, end, lock) = self.tags.get_mut(tag_index).unwrap();
        self.text.tags.remove(*start, *lock);
        if let Some(_) = tag.inverse() {
            self.text.tags.remove(*end, *lock);
        }

        *tag = new_tag;

        self.text.tags.insert(*tag, *lock, *start);
        if let Some(inv_tag) = tag.inverse() {
            self.text.tags.insert(*tag, *lock, *end);
        }
    }

    pub fn clear(&mut self) {
        self.text.inner.clear();
        self.tags.clear();
        self.ranges.clear();
    }

    pub fn truncate(&mut self, range_index: usize) {
        let trunc_start = self.ranges.get(range_index).unwrap().start;
        self.ranges.truncate(range_index);

        let change = Change::new("", trunc_start.., self.text.inner());
        self.text.apply_change(&change);

        for (_, start, end, lock) in &self.tags {
            if *start >= trunc_start {
                self.text.tags.remove(*start, *lock);
                self.text.tags.remove(*end, *lock);
            }
        }
    }

    pub fn is_default(&self) -> bool {
        self.tags.is_empty() && self.ranges.is_empty() && self.text.is_empty()
    }

    pub fn ranges_len(&self) -> usize {
        self.ranges.len()
    }

    pub fn text(&self) -> &Text<U> {
        &self.text
    }
}

/// The text in a given area.
pub struct Text<U>
where
    U: Ui,
{
    inner: InnerText,
    tags: Tags,
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
        let mut tags = Tags::default();
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
        let mut tags = Tags::default();
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
    pub(crate) fn print(&self, end_node: &mut EndNode<U>, print_info: PrintInfo) {
        if end_node.is_active {
            end_node.label.set_as_active();
        }

        let config = &end_node.config;
        let label = &mut end_node.label;

        label.start_printing(config);

        let line_start_ch = {
            let cur_line = self.inner.char_to_line(print_info.first_ch);
            self.inner.line_to_char(cur_line)
        };

        let mut chars = self
            .inner
            .chars_at(line_start_ch)
            .enumerate()
            .map(|(index, ch)| (index + print_info.first_ch, ch));

        let mut tags = self.tags.iter_at(line_start_ch).peekable();
        let mut form_former = config.palette.form_former();

        let mut skip_counter = print_info.first_ch - line_start_ch;
        let mut last_ch = 'a';
        let mut skip_rest_of_line = false;

        while let Some((index, ch)) = chars.next() {
            trigger_on_char::<U>(&mut tags, index, label, &mut form_former);

            if skip_counter > 0 || (skip_rest_of_line && ch != '\n') {
                skip_counter = skip_counter.saturating_sub(1);
                continue;
            } else {
                skip_rest_of_line = false;
            }

            match print_ch::<U>(ch, last_ch, label, config, print_info.x_shift) {
                PrintStatus::NextLine => skip_rest_of_line = true,
                PrintStatus::NextChar => {}
                PrintStatus::Finished => break,
            }

            last_ch = ch;
        }

        end_node.label.stop_printing();
    }

    /// Merges `String`s with the body of text, given a range to replace.
    fn merge_text(&mut self, edit: impl AsRef<str>, old: Range<usize>) {
        let edit = edit.as_ref();
        let new = old.start..(old.start + edit.chars().count());

        self.inner.replace(old.start..old.end, edit);

        self.tags.transform_range(old, new);
    }

    pub(crate) fn apply_change(&mut self, change: &Change) {
        self.merge_text(&change.added_text, change.taken_range());
    }

    pub(crate) fn undo_change(&mut self, change: &Change, chars: isize) {
        let start = change.start.saturating_add_signed(chars);
        let end = change.added_end().saturating_add_signed(chars);
        self.merge_text(&change.taken_text, start..end);
    }

    pub fn inner(&self) -> &InnerText {
        &self.inner
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len_chars() == 0
    }

    /// Removes the tags for all the cursors, used before they are expected to move.
    pub(crate) fn remove_cursor_tags(&mut self, cursors: &[Cursor], main_index: usize) {
        for (index, cursor) in cursors.iter().enumerate() {
            let Range { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = cursor_tags(index == main_index);

            let pos_list = [
                (start, start_tag),
                (end, end_tag),
                (cursor.caret().true_char(), caret_tag),
            ];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.into_iter().skip(no_selection) {
                self.tags.insert(tag, self.lock, pos);
            }
        }
    }

    /// Adds the tags for all the cursors, used after they are expected to have moved.
    pub(crate) fn add_cursor_tags(&mut self, cursors: &[Cursor], main_index: usize) {
        for (index, cursor) in cursors.iter().enumerate() {
            let Range { start, end } = cursor.range();

            for ch_index in [start, end].into_iter() {
                self.tags.remove(ch_index, self.lock);
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
}

/// Prints the given character, taking configuration options into account.
fn print_ch<U>(
    ch: char,
    last_ch: char,
    label: &mut U::Label,
    config: &Config,
    x_shift: usize,
) -> PrintStatus
where
    U: Ui,
{
    match ch {
        '\t' => label.print('\t', x_shift),
        '\n' => {
            label.print(config.new_line_char.get_new_line_ch(last_ch), x_shift);
            label.next_line()
        }
        _ => label.print(ch, x_shift),
    }
}

fn trigger_on_char<'a, U>(
    tags: &mut Peekable<impl Iterator<Item = (usize, Tag)>>,
    byte: usize,
    label: &mut U::Label,
    form_former: &mut FormFormer,
) where
    U: Ui,
{
    while let Some((tag_byte, tag)) = tags.peek() {
        if byte == *tag_byte {
            tag.trigger(label, form_former);
            tags.next();
        } else {
            break;
        }
    }
}

pub enum PrintStatus {
    NextLine,
    NextChar,
    Finished,
}

// NOTE: The defaultness in here, when it comes to `last_main`, may cause issues in the future.
/// Information about how to print the file on the `Label`.
#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct PrintInfo {
    /// The index of the first [char] that should be printed on the screen.
    pub first_ch: usize,
    /// How shifted the text is to the left.
    pub x_shift: usize,
    /// The last position of the main cursor.
    pub last_main: Pos,
}

impl PrintInfo {
    /// Scrolls the `PrintInfo` vertically by a given amount, on a given file.
    pub fn scroll_vertically<U>(&mut self, mut d_y: i32, text: &Text<U>)
    where
        U: Ui,
    {
        //if d_y > 0 {
        //    let mut lines_iter = text.lines().iter().skip(self.top_row);

        //    while let Some(line) = lines_iter.next() {
        //        let wrap_count = line.iter_wraps().count();
        //        if (wrap_count + 1) as i32 > d_y {
        //            self.top_wraps = d_y as usize;
        //            break;
        //        } else {
        //            self.top_row += 1;
        //            d_y -= (wrap_count + 1) as i32;
        //        }
        //    }
        //} else if d_y < 0 {
        //    let mut lines_iter = text.lines().iter().take(self.top_row).rev();

        //    while let Some(line) = lines_iter.next() {
        //        let wrap_count = line.iter_wraps().count();
        //        if ((wrap_count + 1) as i32) < d_y {
        //            self.top_wraps = -d_y as usize;
        //            break;
        //        } else {
        //            self.top_row -= 1;
        //            d_y += (wrap_count + 1) as i32;
        //        }
        //    }
        //}
    }

    //pub fn scroll_horizontally<U>(&mut self, d_x: i32, text: &Rope, end_node: &EndNode<U>)
    //where
    //    U: Ui,
    //{
    //    let mut max_d = 0;
    //    for index in text.printed_lines(end_node.label.area().height(), self) {
    //        let line = &text.lines()[index];
    //        let line_d = end_node
    //            .label
    //            .get_width(line.text.as_str(), &end_node.config.tab_places);
    //        max_d = max_d.max(line_d);
    //    }

    //    self.x_shift = min(self.x_shift.saturating_add_signed(d_x as isize), max_d);
    //}

    /// Scrolls up.
    fn scroll_up_to_gap<U>(&mut self, target: Pos, inner: &InnerText, end_node: &EndNode<U>)
    where
        U: Ui,
    {
        let label = &end_node.label;
        let config = &end_node.config();
        let min_dist = config.scrolloff.y_gap;
        let (wrap_method, tab_places) = (config.wrap_method, &config.tab_places);

        let slice = inner.slice(..target.true_char());
        let mut lines = slice.lines_at(target.row()).reversed();

        let mut accum = 0;
        while let Some(line) = lines.next() {
            accum += 1 + label.wrap_count(line, wrap_method, tab_places);
            if accum >= min_dist {
                // `accum - gap` is the amount of wraps that should be offscreen.
                self.first_ch = label.col_at_wrap(line, accum - min_dist, wrap_method, tab_places);
                break;
            }
        }

        // This means we can't scroll up anymore.
        if accum < min_dist {
            self.first_ch = 0;
        }
    }

    /// Scrolls down.
    fn scroll_down_to_gap<U>(&mut self, target: Pos, inner: &InnerText, end_node: &EndNode<U>)
    where
        U: Ui,
    {
        let label = &end_node.label;
        let config = &end_node.config();
        let max_dist = label.area().height() - config.scrolloff.y_gap;
        let (wrap_method, tab_places) = (config.wrap_method, &config.tab_places);

        let slice = inner.slice(..target.true_char());
        let mut lines = slice.lines_at(target.row()).reversed();
        let mut lines_to_top = target.true_row() - inner.char_to_line(self.first_ch);

        let mut accum = 0;
        while let Some(line) = lines.next() {
            lines_to_top -= 1;
            accum += 1 + label.wrap_count(line, wrap_method, tab_places);
            if accum >= max_dist {
                // `accum - gap` is the amount of wraps that should be offscreen.
                self.first_ch = label.col_at_wrap(line, accum - max_dist, wrap_method, tab_places);
                break;
            // We have reached the top of the screen before the accum equaled gap.
            // This means that no scrolling actually needs to take place.
            } else if lines_to_top == 0 {
                break;
            }
        }
    }

    /// Scrolls the file horizontally, usually when no wrapping is being used.
    fn scroll_hor_to_gap<U>(&mut self, target: Pos, inner: &InnerText, end_node: &EndNode<U>)
    where
        U: Ui,
    {
        let label = &end_node.label;
        let config = end_node.config();
        let max_dist = label.area().width() - config.scrolloff.x_gap;
        let (wrap_method, tab_places) = (config.wrap_method, &config.tab_places);

        let slice = inner.slice(..target.true_char());
        let line = inner.line(target.true_row());

        let target_dist = label.get_width(line, tab_places);
        self.x_shift = target_dist.saturating_sub(max_dist);
    }

    /// Updates the print info.
    pub fn update<U>(&mut self, target: Pos, inner: &InnerText, end_node: &EndNode<U>)
    where
        U: Ui,
    {
        if let WrapMethod::NoWrap = end_node.config().wrap_method {
            self.scroll_hor_to_gap(target, inner, end_node);
        }

        if target < self.last_main {
            self.scroll_up_to_gap(target, inner, end_node);
        } else if target > self.last_main {
            self.scroll_down_to_gap(target, inner, end_node);
        }

        self.last_main = target;
    }
}

//pub(crate) fn update_range<U>(
//    text: &mut Text<U>, range: TextRange, max_line: usize, node: &EndNode<U>,
//) where
//    U: Ui,
//{
//    if let Some(match_manager) = &mut text.match_manager {
//        let line = &text.lines[max_line];
//        let max_pos = TextPos::default().translate(&text.lines, max_line, line.char_count());
//
//        let start = TextPos {
//            row: range.start.row,
//            col: 0,
//            byte: range.start.byte - get_byte(&text.lines[range.start.row].text(),
//     range.start.col),    };
//
//        let len = text.lines[range.end.row].text().len();
//        let end = TextPos {
//            row: range.end.row,
//            col: text.lines[range.end.row].char_count(),
//            byte: range.end.byte - get_byte(&text.lines[range.end.row].text(), range.end.col) +
//     len,    };
//
//        let range = TextRange { start, end };
//
//        let line_infos = match_manager.match_range(text.lines.as_slice(), range, max_pos);
//
//        for (line_info, line_num) in line_infos {
//            text.lines[line_num].info = line_info;
//            text.lines[line_num].update_line_info(node);
//        }
//    }
//}
//
fn cursor_tags(is_main: bool) -> (Tag, Tag, Tag) {
    if is_main {
        (
            Tag::MainCursor,
            Tag::PushForm(MAIN_SEL),
            Tag::PopForm(MAIN_SEL),
        )
    } else {
        (
            Tag::MainCursor,
            Tag::PushForm(EXTRA_SEL),
            Tag::PopForm(EXTRA_SEL),
        )
    }
}
