pub mod reader;

use std::{
    cmp::min,
    iter::Peekable,
    ops::{ControlFlow, RangeInclusive},
};

use ropey::Rope;

use self::reader::MutTextReader;
use crate::{
    config::{Config, WrapMethod},
    get_byte_at_col,
    history::{Change, Splice},
    position::{Cursor, Pos, Range},
    tags::{
        form::{FormFormer, FormPalette, EXTRA_SEL_ID, MAIN_SEL_ID},
        CharTag, LineFlags, LineInfo,
    },
    ui::{Area, EndNode, Label, Ui},
};

// TODO: move this to a more general file.
/// A line in the text file.
#[derive(Debug)]
pub struct TextLine {
    /// The text on the line.
    text: String,

    /// Information about a line.
    pub(crate) info: LineInfo,
}

impl TextLine {
    /// Returns the line's indentation, in the number of spaces.
    pub fn indent(&self, config: &Config) -> usize {
        let mut indent_sum = 0;

        for ch in self.text.chars() {
            indent_sum += match ch {
                ' ' => 1,
                '\t' => config.tab_places.spaces_on_col(indent_sum),
                _ => break,
            };
        }

        indent_sum as usize
    }

    pub fn get_dist_to_col<U>(&self, col: usize, end_node: &EndNode<U>) -> usize
    where
        U: Ui,
    {
        let byte = self.text().char_indices().map(|(byte, _)| byte).take(col).last().unwrap_or(0);
        let prior_text = &self.text()[..byte];
        end_node.label.get_width(prior_text, &end_node.config().tab_places)
    }

    /// Returns the byte index of a given column.
    pub fn get_line_byte_at(&self, col: usize) -> usize {
        if self.info.line_flags.contains(LineFlags::PURE_ASCII) {
            col
        } else {
            self.text.char_indices().nth(col).unwrap_or((self.text.len(), ' ')).0
        }
    }

    /// Returns how many characters are in the line.
    pub fn char_count(&self) -> usize {
        if self.info.line_flags.contains(LineFlags::PURE_ASCII) {
            self.text.len()
        } else {
            self.text.chars().count()
        }
    }

    ///////////////////////////////////////////////////////////////////////////////////////////
    // MASSIVE CHANGES ARE INCOMING FOR PRINTING, EVERYTHING IS A BIT TERRIBLE AT THE MOMENT //
    ///////////////////////////////////////////////////////////////////////////////////////////
    /// Prints a line in a given position, skipping `skip` characters.
    ///
    /// Returns true if we should try printing the next line.
    #[inline]
    fn print<U>(&self, end_node: &mut EndNode<U>, x_shift: usize, skip: usize) -> bool
    where
        U: Ui,
    {
        let (label, config) = (&mut end_node.label, &end_node.config);

        let skip = if let WrapMethod::NoWrap = config.wrap_method {
            // The leftover here represents the amount of characters that should not be printed,
            // for example, complex emoji may occupy several cells that should be empty, in the
            // case that part of the emoji is located before the first column.
            label.col_at_dist(self.text.as_str(), x_shift, &config.tab_places)
        } else {
            skip
        };

        // Iterate through the tags before the first unskipped character.
        let mut form_former = config.palette.form_former();
        let tags_to_skip = self.trigger_skipped::<U>(skip, label, &mut form_former);
        let mut tags = self.info.char_tags.iter().skip(tags_to_skip).peekable();

        // As long as `![' ', '\t', '\n'].contains(last_ch)` initially, we're good.
        let mut last_ch = 'a';

        let mut d_x = 0;

        // To possibly print a cursor one byte after the end of the last line.
        let extra_ch = [(self.text.len(), ' ')];
        for (byte, ch) in self.text.char_indices().skip_while(|&(b, _)| b < skip).chain(extra_ch) {
            d_x += 1;
            if let WrapMethod::NoWrap = config.wrap_method {
                if d_x > label.area_mut().width() {
                    return label.next_line().is_ok();
                }
            }

            last_ch = ch;
        }

        label.print(' ');

        true
    }

    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn text(&self) -> &String {
        &self.text
    }
}

impl<S> From<S> for TextLine
where
    S: ToString,
{
    fn from(value: S) -> Self {
        TextLine { text: value.to_string(), info: LineInfo::default() }
    }
}

#[derive(Default)]
pub struct TextLineBuilder {
    forms: Vec<u16>,
}

impl TextLineBuilder {
    /// Returns a new instance of `TextLineBuilder`, and removes every form inside a bracket pair.
    pub fn format_and_create(text: &mut String, palette: &FormPalette) -> Self {
        let mut form_indices = Vec::new();
        let mut formless_text = text.clone();

        let mut last_start = None;
        for (start, _) in text.match_indices('[').chain([(text.len(), "[")]) {
            if let Some(mut last_start) = last_start {
                if let Some(mut last_end) = text[(last_start + 1)..start].find(']') {
                    last_end += last_start;
                    last_start += 1;
                    let (_, form_index) = palette.get_from_name(&text[last_start..=last_end]);
                    form_indices.push(form_index);
                    let removed_len = text.len() - formless_text.len();
                    last_start -= removed_len;
                    last_end -= removed_len;
                    formless_text.replace_range(last_start..=last_end, "");
                }
            }

            last_start = Some(start);
        }

        *text = formless_text;

        TextLineBuilder { forms: form_indices }
    }

    /// Takes in a string with empty bracket pairs and places forms according to their positions.
    pub fn form_text_line(&self, text: impl ToString) -> TextLine {
        let text = text.to_string();
        let mut formless_text = text.to_string();
        let mut info = LineInfo::default();
        let mut removed_len = 0;

        for (index, (start, _)) in text.match_indices("[]").enumerate() {
            let start = start - removed_len;
            formless_text.replace_range(start..=(start + 1), "");
            info.char_tags.insert((start, CharTag::PushForm(self.forms[index])));
            if index > 0 {
                info.char_tags.insert((start, CharTag::PopForm(self.forms[index - 1])));
            }
            removed_len += 2;
        }

        TextLine { text: formless_text, info }
    }

    /// Makes it so this `TextLineBuilder`
    pub fn format_and_extend(&mut self, text: &mut String, palette: &FormPalette) {
        let builder = TextLineBuilder::format_and_create(text, palette);
        self.forms.extend_from_slice(&builder.forms);
    }

    /// Returns the amount of forms inserted into `TextLine`s.
    pub fn form_count(&self) -> usize {
        self.forms.len()
    }
}

impl<const N: usize> From<[u16; N]> for TextLineBuilder {
    fn from(value: [u16; N]) -> Self {
        Self { forms: Vec::from(value) }
    }
}

/// The text in a given area.
#[derive(Default)]
pub struct Text<U>
where
    U: Ui,
{
    rope: Rope,
    line_info: Vec<LineInfo>,
    _replacements: Vec<(Vec<TextLine>, RangeInclusive<usize>, bool)>,
    _readers: Vec<Box<dyn MutTextReader<U>>>,
}

// TODO: Properly implement _replacements.
impl<U> Text<U>
where
    U: Ui,
{
    /// Prints the contents of a given area in a given `EndNode`.
    pub(crate) fn print(&self, end_node: &mut EndNode<U>, print_info: PrintInfo) {
        if end_node.is_active {
            end_node.label.set_as_active();
        }
        end_node.label.start_printing();
        let label = &mut end_node.label;
        let config = &end_node.config;

        let mut cur_line = self.rope.char_to_line(print_info.first_ch);
        let line_start_ch = self.rope.line_to_char(cur_line);

        let mut chars = self.rope.chars_at(line_start_ch);
        let mut tags = self.iter_tags_from(cur_line);
        let mut form_former = end_node.config().palette.form_former();

        let mut skip_counter = print_info.first_ch - line_start_ch;
        let mut last_ch = 'a';
        let mut cur_byte = 0;
        let mut skip_rest_of_line = false;
        while let Some(ch) = chars.next() {
            trigger_on_byte::<U>(&mut tags, cur_byte, label, &mut form_former);

            if skip_counter > 0 || (skip_rest_of_line && ch != '\n') {
                skip_counter -= 1;
                continue;
            } else {
                skip_rest_of_line = false;
            }

            match print_ch::<U>(ch, last_ch, label, config, print_info.x_shift) {
                PrintStatus::NextLine => {
                    skip_rest_of_line = true;
                    cur_byte = 0;
                    cur_line += 1;
                }
                PrintStatus::NextChar => cur_byte += ch.len_utf8(),
                PrintStatus::Finished => break,
            }
        }

        end_node.label.stop_printing();
    }

    fn iter_tags_from(&self, line: usize) -> Peekable<impl Iterator<Item = &(usize, CharTag)>> {
        self.line_info[line..]
            .iter()
            .map(|LineInfo { char_tags, .. }| char_tags.iter())
            .flatten()
            .peekable()
    }

    // This is more efficient than using the `merge_edit()` function.
    /// Merges `String`s with the body of text, given a range to replace.
    fn merge_text(&mut self, edit: impl AsRef<str>, range: Range) {
        self.rope.remove(range.start.byte..range.end.byte);
        self.rope.insert(range.start.byte, edit.as_ref());
    }

    pub(crate) fn apply_change(&mut self, change: &Change) {
        self.merge_text(&change.added_text, change.splice.taken_range());
    }

    pub(crate) fn undo_change(&mut self, change: &Change, splice: &Splice) {
        self.merge_text(&change.taken_text, splice.added_range());
    }

    pub fn lines(&self) -> &Vec<TextLine> {
        &self.rope
    }

    pub fn is_empty(&self) -> bool {
        self.rope.len() == 1 && self.rope[0].text.as_str() == ""
    }

    /// Removes the tags for all the cursors, used before they are expected to move.
    pub(crate) fn remove_cursor_tags(&mut self, cursors: &[Cursor], main_index: usize) {
        for (index, cursor) in cursors.iter().enumerate() {
            let Range { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = cursor_tags(index == main_index);

            let pos_list = [(start, start_tag), (end, end_tag), (cursor.caret(), caret_tag)];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.iter().skip(no_selection) {
                let Some(line) = self.rope.get_mut(pos.row) else { return; };
                let byte = line.get_line_byte_at(pos.col);
                line.info.char_tags.remove_first(|(cmp_byte, cmp_tag)| {
                    cmp_byte as usize == byte && cmp_tag == *tag
                });
            }
        }
    }

    /// Adds the tags for all the cursors, used after they are expected to have moved.
    pub(crate) fn add_cursor_tags(&mut self, cursors: &[Cursor], main_index: usize) {
        for (index, cursor) in cursors.iter().enumerate() {
            let Range { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = cursor_tags(index == main_index);

            let pos_list = [(start, start_tag), (end, end_tag), (cursor.caret(), caret_tag)];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.iter().skip(no_selection) {
                let Some(line) = self.rope.get_mut(pos.row) else { return; };
                let byte = line.get_line_byte_at(pos.col);
                line.info.char_tags.insert((byte, *tag));
            }
        }
    }

    pub(crate) fn rope(&self) -> &Rope {
        &self.rope
    }
}

/// Prints the given character, taking configuration options into account.
fn print_ch<U>(
    ch: char, last_ch: char, label: &mut U::Label, config: &Config, x_shift: usize,
) -> PrintStatus
where
    U: Ui,
{
    match ch {
        '\t' => label.print('\t', x_shift),
        '\n' => {
            label.print(config.show_new_line.get_new_line_ch(last_ch), x_shift);
            if label.next_line().is_ok() { PrintStatus::NextLine } else { PrintStatus::Finished }
        }
        _ => label.print(ch, x_shift),
    }
}

fn trigger_on_byte<'a, U>(
    tags: &mut Peekable<impl Iterator<Item = &'a (usize, CharTag)>>, byte: usize,
    label: &mut U::Label, form_former: &mut FormFormer,
) where
    U: Ui,
{
    while let Some(&(tag_byte, tag)) = tags.peek() {
        if byte == *tag_byte {
            tags.next();
            tag.trigger(label, form_former);
        } else {
            break;
        }
    }
}

impl<U> From<String> for Text<U>
where
    U: Ui,
{
    fn from(value: String) -> Self {
        Text {
            rope: Rope::from(value),
            line_info: Vec::new(),
            _replacements: Vec::new(),
            _readers: Vec::new(),
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

    pub fn scroll_horizontally<U>(&mut self, d_x: i32, text: &Text<U>, end_node: &EndNode<U>)
    where
        U: Ui,
    {
        //let mut max_d = 0;
        //for index in text.printed_lines(end_node.label.area().height(), self) {
        //    let line = &text.lines()[index];
        //    let line_d = end_node.label.get_width(line.text.as_str(),
        // &end_node.config.tab_places);    max_d = max(max_d, line_d);
        //}

        //self.x_shift = min(self.x_shift.saturating_add_signed(d_x as isize), max_d);
    }

    /// Scrolls up or down, assuming that the lines cannot wrap.
    fn calibrate_vertically<U>(&mut self, target: Pos, height: usize, end_node: &EndNode<U>)
    where
        U: Ui,
    {
        let scrolloff = end_node.config().scrolloff;

        if target.row > self.top_row + height - scrolloff.d_y {
            self.top_row += target.row + scrolloff.d_y - self.top_row - height;
        } else if target.row < self.top_row + scrolloff.d_y && self.top_row != 0 {
            self.top_row -= (self.top_row + scrolloff.d_y) - target.row
        }
    }

    /// Scrolls up, assuming that the lines can wrap.
    fn calibrate_up<U>(
        &mut self, target: Pos, mut d_y: usize, text: &Text<U>, end_node: &EndNode<U>,
    ) where
        U: Ui,
    {
        //let scrolloff = end_node.config().scrolloff;
        //let lines_iter = text.lines().iter().take(target.row);

        //// If the target line is above the top line, no matter what, a new top line is needed.
        //let mut needs_new_top_line = target.row < self.top_row;

        //for (index, line) in lines_iter.enumerate().rev() {
        //    if index != target.row {
        //        d_y += 1 + line.iter_wraps().count();
        //    };

        //    if index == self.top_row {
        //        // This means we ran into the top line too early, and must scroll up.
        //        if d_y < scrolloff.d_y + self.top_wraps {
        //            needs_new_top_line = true;
        //        // If this happens, we're in the middle of the screen, and don't need to scroll.
        //        } else if !needs_new_top_line {
        //            break;
        //        }
        //    }

        //    if needs_new_top_line && (d_y >= scrolloff.d_y || index == 0) {
        //        self.top_row = index;
        //        self.top_wraps = d_y.saturating_sub(scrolloff.d_y);
        //        break;
        //    }
        //}
    }

    /// Scrolls down, assuming that the lines can wrap.
    fn calibrate_down<U>(
        &mut self, target: Pos, mut d_y: usize, text: &Text<U>, height: usize,
        end_node: &EndNode<U>,
    ) where
        U: Ui,
    {
        //let mut scrolloff = end_node.config().scrolloff;
        //scrolloff.d_y = min(scrolloff.d_y, height);
        //let lines_iter = text.lines().iter().take(target.row + 1);
        //let mut top_offset = 0;

        //for (index, line) in lines_iter.enumerate().rev() {
        //    if index != target.row {
        //        d_y += 1 + line.iter_wraps().count();
        //    }

        //    if index == self.top_row {
        //        top_offset = self.top_wraps
        //    };

        //    if d_y + scrolloff.d_y >= height + top_offset {
        //        self.top_row = index;
        //        // If this equals 0, that means the distance has matched up perfectly,
        //        // i.e. the distance between the new `info.top_line` is exactly what's
        //        // needed for the full height. If it's greater than 0, `info.top_wraps`
        //        // needs to adjust where the line actually begins to match up.
        //        self.top_wraps = d_y + scrolloff.d_y - height;
        //        break;
        //    }

        //    // If this happens first, we're in the middle of the screen, and don't need to
        // scroll.    if index == self.top_row {
        //        break;
        //    }
        //}
    }

    /// Scrolls the file horizontally, usually when no wrapping is being used.
    fn calibrate_horizontally<U>(
        &mut self, target: Pos, text: &Text<U>, width: usize, end_node: &EndNode<U>,
    ) where
        U: Ui,
    {
        let mut scrolloff = end_node.config().scrolloff;
        scrolloff.d_x = min(scrolloff.d_x, width);

        if let WrapMethod::NoWrap = end_node.config().wrap_method {
            let target_line = &text.rope[target.row];
            let distance = end_node.get_width(&target_line.text[..target.col]);

            // If the distance is greater, it means that the cursor is out of bounds.
            if distance > self.x_shift + width - end_node.config().scrolloff.d_x {
                // Shift by the amount required to keep the cursor in bounds.
                self.x_shift = distance + scrolloff.d_x - width;
            // Check if `info.x_shift` is already at 0, if it is, no scrolling is dones.
            } else if distance < self.x_shift + scrolloff.d_x {
                self.x_shift = distance.saturating_sub(end_node.config().scrolloff.d_x);
            }
        }
    }

    /// Updates the print info.
    pub fn update<U>(&mut self, target: Pos, text: &Text<U>, end_node: &EndNode<U>)
    where
        U: Ui,
    {
        let (wrap_method, tab_places) =
            (&end_node.config().wrap_method, &end_node.config().tab_places);
        let wrap_method = end_node.config().wrap_method;
        let (height, width) = (end_node.label.area().height(), end_node.label.area().width());

        let old = self.last_main;

        let line = &text.lines()[min(old.row, text.lines().len() - 1)];
        let current_byte = line.get_line_byte_at(old.col);
        let cur_wraps = end_node.label.wrap_count(line.text(), wrap_method, tab_places);

        let line = &text.lines()[target.row];
        let target_byte = line.get_line_byte_at(target.col);
        let old_wraps = end_node.label.wrap_count(line.text(), wrap_method, tab_places);

        if let WrapMethod::NoWrap = wrap_method {
            self.calibrate_vertically(target, height, end_node);
            self.calibrate_horizontally(target, text, width, end_node);
        } else if target.row < old.row || (target.row == old.row && old_wraps < cur_wraps) {
            self.calibrate_up(target, old_wraps, text, end_node);
        } else {
            self.calibrate_down(target, old_wraps, text, height, end_node);
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
fn cursor_tags(is_main: bool) -> (CharTag, CharTag, CharTag) {
    if is_main {
        (CharTag::MainCursor, CharTag::PushForm(MAIN_SEL_ID), CharTag::PopForm(MAIN_SEL_ID))
    } else {
        (CharTag::MainCursor, CharTag::PushForm(EXTRA_SEL_ID), CharTag::PopForm(EXTRA_SEL_ID))
    }
}
