pub mod reader;

use std::{
    cmp::{max, min},
    iter::Peekable,
    ops::RangeInclusive,
};

use self::reader::MutTextReader;
use crate::{
    action::{Change, Splice, TextRange},
    config::{Config, WrapMethod},
    cursor::{TextCursor, TextPos},
    get_byte_at_col,
    tags::{
        form::{FormFormer, FormPalette, EXTRA_SEL_ID, MAIN_SEL_ID},
        CharTag, LineFlags, LineInfo, MatchManager,
    },
    ui::{Area, EndNode, Label, Ui},
};

// TODO: move this to a more general file.
/// A line in the text file.
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
        let (col_byte, _) = self.text().char_indices().take(col).last().unwrap();
        let prior_text = &self.text()[..col_byte];
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

    /// Returns an iterator over the wrapping bytes of the line.
    pub fn iter_wraps(&self) -> impl Iterator<Item = usize> + '_ {
        self.info.char_tags.iter_wraps()
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
            label.get_col_at_dist(self.text.as_str(), x_shift, &config.tab_places)
        } else {
            skip
        };

        if let Some(first_wrap_col) = self.iter_wraps().next() {
            if skip >= first_wrap_col as usize && config.wrap_indent {
                (0..self.indent(config)).for_each(|_| label.print(' '));
            }
        }

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
            if !self.trigger_on_byte::<U>(&mut tags, byte, label, config, &mut form_former) {
                return false;
            }

            d_x += 1;
            if let WrapMethod::NoWrap = config.wrap_method {
                if d_x > label.area_mut().width() {
                    return true;
                }
            }

            match ch {
                '\t' => (0..4).for_each(|_| label.print(' ')),
                '\n' => {
                    label.print(config.show_new_line.get_new_line_ch(last_ch));
                    return label.next_line().is_ok();
                }
                _ => label.print(ch),
            }
            last_ch = ch;
        }

        label.print(' ');

        true
    }

    fn trigger_on_byte<'a, U>(
        &self, tags: &mut Peekable<impl Iterator<Item = &'a (usize, CharTag)>>, byte: usize,
        label: &mut U::Label, config: &Config, form_former: &mut FormFormer,
    ) -> bool
    where
        U: Ui,
    {
        while let Some(&(tag_byte, tag)) = tags.peek() {
            if byte == *tag_byte {
                tags.next();
                // If this is the first printed character of `top_line`, we don't wrap.
                let indent = config.usable_indent::<U>(self, label);
                if !tag.trigger(label, form_former, indent) {
                    return false;
                }
            } else {
                break;
            }
        }
        true
    }

    fn trigger_skipped<U>(
        &self, skip: usize, label: &mut U::Label, form_former: &mut FormFormer,
    ) -> usize
    where
        U: Ui,
    {
        let mut tags_iter = self.info.char_tags.iter().take_while(|(byte, _)| skip > *byte);
        let mut last_checked_index = 0;
        while let Some((byte, tag)) = tags_iter.next() {
            last_checked_index += 1;
            if let CharTag::PushForm(_) | CharTag::PopForm(_) = tag {
                tag.trigger(label, form_former, 0);
            }
        }
        last_checked_index
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
pub struct Text<U>
where
    U: Ui,
{
    pub lines: Vec<TextLine>,
    replacements: Vec<(Vec<TextLine>, RangeInclusive<usize>, bool)>,
    readers: Vec<Box<dyn MutTextReader<U>>>,
}

impl<U> Default for Text<U>
where
    U: Ui,
{
    fn default() -> Self {
        Text { lines: vec![TextLine::from("")], replacements: Vec::new(), readers: Vec::new() }
    }
}

// TODO: Properly implement replacements.
impl<U> Text<U>
where
    U: Ui,
{
    /// Returns a new instance of `Text`.
    pub fn new(text: String) -> Self {
        Text {
            lines: text.split_inclusive('\n').map(|l| TextLine::from(l)).collect(),
            replacements: Vec::new(),
            readers: Vec::new(),
        }
    }

    /// Prints the contents of a given area in a given `EndNode`.
    pub(crate) fn print(&self, end_node: &mut EndNode<U>, print_info: PrintInfo) {
        if end_node.is_active {
            end_node.label.set_as_active();
        }
        end_node.label.start_printing();

        // Print the `top_line`.
        let top_line = &self.lines[print_info.top_row];
        let skip = if print_info.top_wraps > 0 {
            top_line.iter_wraps().nth(print_info.top_wraps - 1).unwrap() + 1
        } else {
            0
        };
        top_line.print::<U>(end_node, print_info.x_shift, skip as usize);

        // Prints other lines until it can't anymore.
        for line in self.lines.iter().skip(print_info.top_row + 1) {
            if !line.print::<U>(end_node, print_info.x_shift, 0) {
                break;
            }
        }

        end_node.label.stop_printing();
    }

    /// Returns a list of all line indices that would be printed in a given node.
    ///
    /// The first number is the `TextLine`'s index, and the second is the amount of visible lines
    /// on the screen the `TextLine` would occupy.
    pub fn printed_lines(&self, height: usize, print_info: &PrintInfo) -> Vec<usize> {
        let height_sum = print_info.top_wraps + height;
        let mut printed_lines = Vec::new();
        let mut lines_iter = self.lines.iter().enumerate();

        // List the top line.
        let top_line = lines_iter.nth(print_info.top_row).unwrap();
        let mut d_y = 1 + top_line.1.iter_wraps().count();
        for _ in 0..min(d_y - print_info.top_wraps, height) {
            printed_lines.push(top_line.0);
        }

        // List all the other lines.
        while let (Some((index, line)), true) = (lines_iter.next(), d_y < height_sum) {
            let line_count = 1 + line.iter_wraps().count();
            for _ in 0..min(line_count, height_sum - d_y) {
                printed_lines.push(index);
            }
            d_y += line_count;
        }

        printed_lines
    }

    // This is more efficient than using the `merge_edit()` function.
    /// Merges `String`s with the body of text, given a range to replace.
    fn merge_text(&mut self, edit: &Vec<String>, range: TextRange) {
        let lines = &mut self.lines;

        if range.lines().count() == 1 && edit.len() == 1 {
            let line = &mut lines[range.start.row];
            let first_byte = get_byte_at_col(range.start.col, &line.text);
            let last_byte = get_byte_at_col(range.end.col, &line.text);
            line.text.replace_range(first_byte..last_byte, edit[0].as_str());
        } else {
            let first_line = &lines[range.start.row];
            let first_byte = get_byte_at_col(range.start.col, &first_line.text);
            let last_line = &lines[range.end.row];
            let last_byte = get_byte_at_col(range.end.col, &last_line.text);

            let first_amend = &first_line.text[..first_byte];
            let last_amend = &last_line.text[last_byte..];

            let mut edit = edit.clone();

            edit.first_mut().unwrap().insert_str(0, first_amend);
            edit.last_mut().unwrap().push_str(last_amend);

            let edit: Vec<TextLine> = edit.into_iter().map(|l| TextLine::from(l)).collect();

            lines.splice(range.lines(), edit);
        }
    }

    pub(crate) fn apply_change(&mut self, change: &Change) {
        self.merge_text(&change.added_text, change.splice.taken_range());
    }

    pub(crate) fn undo_change(&mut self, change: &Change, splice: &Splice) {
        self.merge_text(&change.taken_text, splice.added_range());
    }

    pub fn lines(&self) -> &Vec<TextLine> {
        &self.lines
    }

    pub fn is_empty(&self) -> bool {
        self.lines.len() == 1 && self.lines[0].text.as_str() == ""
    }

    /// Removes the tags for all the cursors, used before they are expected to move.
    pub(crate) fn remove_cursor_tags(&mut self, cursors: &[TextCursor], main_index: usize) {
        for (index, cursor) in cursors.iter().enumerate() {
            let TextRange { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = if index == main_index {
                (CharTag::MainCursor, CharTag::PushForm(MAIN_SEL_ID), CharTag::PopForm(MAIN_SEL_ID))
            } else {
                (CharTag::ExtraCursor, CharTag::PushForm(EXTRA_SEL_ID), CharTag::PopForm(EXTRA_SEL_ID))
            };
            let pos_list = [(start, start_tag), (end, end_tag), (cursor.caret(), caret_tag)];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.iter().skip(no_selection) {
                let Some(line) = self.lines.get_mut(pos.row) else { return; };
                let byte = line.get_line_byte_at(pos.col);
                line.info.char_tags.remove_first(|(cmp_byte, cmp_tag)| {
                    cmp_byte as usize == byte && cmp_tag == *tag
                });
            }
        }
    }

    /// Adds the tags for all the cursors, used after they are expected to have moved.
    pub(crate) fn add_cursor_tags(&mut self, cursors: &[TextCursor], main_index: usize) {
        for (index, cursor) in cursors.iter().enumerate() {
            let TextRange { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = if index == main_index {
                (CharTag::MainCursor, CharTag::PushForm(MAIN_SEL_ID), CharTag::PopForm(MAIN_SEL_ID))
            } else {
                (CharTag::ExtraCursor, CharTag::PushForm(EXTRA_SEL_ID), CharTag::PopForm(EXTRA_SEL_ID))
            };

            let pos_list = [(start, start_tag), (end, end_tag), (cursor.caret(), caret_tag)];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.iter().skip(no_selection) {
                let Some(line) = self.lines.get_mut(pos.row) else { return; };
                let byte = line.get_line_byte_at(pos.col);
                line.info.char_tags.insert((byte, *tag));
            }
        }
    }

    ///// Iterates on the chars of the `Text`, starting from a specific `TextPos`.
    //pub fn iter_from(&self, pos: TextPos) -> impl Iterator<Item = char> + '_ {
    //    self.lines.iter().skip(pos.row).flat_map(|line| line.text.chars()).skip(pos.col)
    //}

    ///// Iterates on the chars of the `Text`, starting from a specific `TextPos`, in reverse.
    //pub fn rev_iter_from(&self, pos: TextPos) -> impl Iterator<Item = char> + '_ {
    //    self.lines
    //        .iter()
    //        .rev()
    //        .skip(self.lines.len() - 1 - pos.row)
    //        .flat_map(|line| line.text.char_indices().rev())
    //        .skip(self.lines[pos.row].text.chars().count() - 1 - pos.col)
    //        .map(|(_, ch)| ch)
    //}

    //pub fn iter_items_from(&self, pos: TextPos) -> impl Iterator<Item = TextItem> + '_ {
    //    let first_line_byte = pos.byte - self.lines[pos.row].get_line_byte_at(pos.col);
    //    let mut tags =
    //        self.lines.iter().skip(pos.row).flat_map(|line|
    // line.info.char_tags.iter()).peekable().skip_while(|(byte, _)| byte + first_line_byte <
    // pos.byte);    self.lines.iter().skip(pos.row).flat_map(|line|
    // line.text.chars()).skip(pos.col)
    //}
}

enum TextItem<'a> {
    Char(char),
    Tag(&'a CharTag),
}

impl<S, U> From<S> for Text<U>
where
    S: ToString,
    U: Ui,
{
    fn from(value: S) -> Self {
        Text {
            lines: value.to_string().split_inclusive('\n').map(|l| TextLine::from(l)).collect(),
            replacements: Vec::new(),
            readers: Vec::new(),
        }
    }
}

// NOTE: The defaultness in here, when it comes to `last_main`, may cause issues in the future.
/// Information about how to print the file on the `Label`.
#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct PrintInfo {
    /// How many times the row at the top wraps around before being shown.
    pub top_wraps: usize,
    /// The index of the row at the top of the screen.
    pub top_row: usize,
    /// How shifted the text is to the left.
    pub x_shift: usize,
    /// The last position of the main cursor.
    pub last_main: TextPos,
}

impl PrintInfo {
    /// Scrolls the `PrintInfo` vertically by a given amount, on a given file.
    pub fn scroll_vertically<U>(&mut self, mut d_y: i32, text: &Text<U>)
    where
        U: Ui,
    {
        if d_y > 0 {
            let mut lines_iter = text.lines().iter().skip(self.top_row);

            while let Some(line) = lines_iter.next() {
                let wrap_count = line.iter_wraps().count();
                if (wrap_count + 1) as i32 > d_y {
                    self.top_wraps = d_y as usize;
                    break;
                } else {
                    self.top_row += 1;
                    d_y -= (wrap_count + 1) as i32;
                }
            }
        } else if d_y < 0 {
            let mut lines_iter = text.lines().iter().take(self.top_row).rev();

            while let Some(line) = lines_iter.next() {
                let wrap_count = line.iter_wraps().count();
                if ((wrap_count + 1) as i32) < d_y {
                    self.top_wraps = -d_y as usize;
                    break;
                } else {
                    self.top_row -= 1;
                    d_y += (wrap_count + 1) as i32;
                }
            }
        }
    }

    pub fn scroll_horizontally<U>(&mut self, d_x: i32, text: &Text<U>, end_node: &EndNode<U>)
    where
        U: Ui,
    {
        let mut max_d = 0;
        for index in text.printed_lines(end_node.label.area().height(), self) {
            let line = &text.lines()[index];
            let line_d = end_node.label.get_width(line.text.as_str(), &end_node.config.tab_places);
            max_d = max(max_d, line_d);
        }

        self.x_shift = min(self.x_shift.saturating_add_signed(d_x as isize), max_d);
    }

    /// Scrolls up or down, assuming that the lines cannot wrap.
    fn calibrate_vertically<U>(&mut self, target: TextPos, height: usize, end_node: &EndNode<U>)
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
        &mut self, target: TextPos, mut d_y: usize, text: &Text<U>, end_node: &EndNode<U>,
    ) where
        U: Ui,
    {
        let scrolloff = end_node.config().scrolloff;
        let lines_iter = text.lines().iter().take(target.row);

        // If the target line is above the top line, no matter what, a new top line is needed.
        let mut needs_new_top_line = target.row < self.top_row;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.row {
                d_y += 1 + line.iter_wraps().count();
            };

            if index == self.top_row {
                // This means we ran into the top line too early, and must scroll up.
                if d_y < scrolloff.d_y + self.top_wraps {
                    needs_new_top_line = true;
                // If this happens, we're in the middle of the screen, and don't need to scroll.
                } else if !needs_new_top_line {
                    break;
                }
            }

            if needs_new_top_line && (d_y >= scrolloff.d_y || index == 0) {
                self.top_row = index;
                self.top_wraps = d_y.saturating_sub(scrolloff.d_y);
                break;
            }
        }
    }

    /// Scrolls down, assuming that the lines can wrap.
    fn calibrate_down<U>(
        &mut self, target: TextPos, mut d_y: usize, text: &Text<U>, height: usize,
        end_node: &EndNode<U>,
    ) where
        U: Ui,
    {
        let mut scrolloff = end_node.config().scrolloff;
        scrolloff.d_y = min(scrolloff.d_y, height);
        let lines_iter = text.lines().iter().take(target.row + 1);
        let mut top_offset = 0;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.row {
                d_y += 1 + line.iter_wraps().count();
            }

            if index == self.top_row {
                top_offset = self.top_wraps
            };

            if d_y + scrolloff.d_y >= height + top_offset {
                self.top_row = index;
                // If this equals 0, that means the distance has matched up perfectly,
                // i.e. the distance between the new `info.top_line` is exactly what's
                // needed for the full height. If it's greater than 0, `info.top_wraps`
                // needs to adjust where the line actually begins to match up.
                self.top_wraps = d_y + scrolloff.d_y - height;
                break;
            }

            // If this happens first, we're in the middle of the screen, and don't need to scroll.
            if index == self.top_row {
                break;
            }
        }
    }

    /// Scrolls the file horizontally, usually when no wrapping is being used.
    fn calibrate_horizontally<U>(
        &mut self, target: TextPos, text: &Text<U>, width: usize, end_node: &EndNode<U>,
    ) where
        U: Ui,
    {
        let mut scrolloff = end_node.config().scrolloff;
        scrolloff.d_x = min(scrolloff.d_x, width);

        if let WrapMethod::NoWrap = end_node.config().wrap_method {
            let target_line = &text.lines[target.row];
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
    pub fn update<U>(&mut self, target: TextPos, text: &Text<U>, end_node: &EndNode<U>)
    where
        U: Ui,
    {
        let wrap_method = end_node.config().wrap_method;
        let (height, width) = (end_node.label.area().height(), end_node.label.area().width());

        let old = self.last_main;

        let line = &text.lines()[min(old.row, text.lines().len() - 1)];
        let current_byte = line.get_line_byte_at(old.col);
        let cur_wraps = line.iter_wraps().take_while(|&c| c <= current_byte).count();

        let line = &text.lines()[target.row];
        let target_byte = line.get_line_byte_at(target.col);
        let old_wraps = line.iter_wraps().take_while(|&c| c <= target_byte).count();

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

pub(crate) fn update_range<U>(
    text: &mut Text<U>, range: TextRange, max_line: usize, node: &EndNode<U>,
) where
    U: Ui,
{
    //if let Some(match_manager) = &mut text.match_manager {
    //    let line = &text.lines[max_line];
    //    let max_pos = TextPos::default().translate(&text.lines, max_line, line.char_count());

    //    let start = TextPos {
    //        row: range.start.row,
    //        col: 0,
    //        byte: range.start.byte - get_byte(&text.lines[range.start.row].text(),
    // range.start.col),    };

    //    let len = text.lines[range.end.row].text().len();
    //    let end = TextPos {
    //        row: range.end.row,
    //        col: text.lines[range.end.row].char_count(),
    //        byte: range.end.byte - get_byte(&text.lines[range.end.row].text(), range.end.col) +
    // len,    };

    //    let range = TextRange { start, end };

    //    let line_infos = match_manager.match_range(text.lines.as_slice(), range, max_pos);

    //    for (line_info, line_num) in line_infos {
    //        text.lines[line_num].info = line_info;
    //        text.lines[line_num].update_line_info(node);
    //    }
    //}
}

fn cursor_tags(is_main: bool) -> (CharTag, CharTag, CharTag) {
    if is_main {
        (CharTag::MainCursor, CharTag::PushForm(MAIN_SEL_ID), CharTag::PopForm(MAIN_SEL_ID))
    } else {
        (CharTag::MainCursor, CharTag::PushForm(EXTRA_SEL_ID), CharTag::PopForm(EXTRA_SEL_ID))
    }
}
