use std::{
    cmp::{max, min},
    ops::RangeInclusive,
    slice::Iter,
};

use crate::{
    action::{get_byte, Change, Splice, TextRange},
    config::{Config, RwData, WrapMethod},
    cursor::{TextCursor, TextPos},
    get_byte_at_col,
    tags::{form::FormPalette, CharTag, LineFlags, LineInfo, MatchManager},
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
    /// Returns the line's indentation.
    pub fn indent<L, A>(&self, label: &L, config: &Config) -> usize
    where
        L: Label<A>,
        A: Area,
    {
        let mut indent_sum = 0;

        for ch in self.text.chars() {
            if ch == ' ' || ch == '\t' {
                indent_sum += get_char_len(ch, indent_sum, label, config);
            } else {
                break;
            }
        }

        indent_sum as usize
    }

    /// Returns the byte index of a given column.
    pub fn get_line_byte_at(&self, col: usize) -> usize {
        if self.info.line_flags.contains(LineFlags::PURE_ASCII) {
            col
        } else {
            self.text.char_indices().nth(col).unwrap_or((self.text.len(), ' ')).0
        }
    }

    /// Returns the visual distance to a certain column.
    pub(crate) fn get_distance_to_col<U>(
        &self, col: usize, label: &U::Label, config: &Config,
    ) -> usize
    where
        U: Ui,
    {
        let mut width = 0;

        if self.info.line_flags.contains(LineFlags::PURE_1_COL) {
            width = col
        } else {
            for ch in self.text.chars().take(col) {
                width += if ch == '\t' {
                    config.tab_places.get_tab_len(width, label)
                } else {
                    label.get_char_len(ch)
                };
            }
        }

        width
    }

    /// Returns the column and byte found at visual distance from 0. Also returns any leftovers.
    ///
    /// The leftover number is positive if the width of the characters is greater (happens if the
    /// last checked character has a width greater than 1), and 0 otherwise.
    pub(crate) fn get_col_at_distance<U>(
        &self, min_dist: usize, label: &U::Label, config: &Config,
    ) -> (usize, usize)
    where
        U: Ui,
    {
        if self.info.line_flags.contains(LineFlags::PURE_1_COL) {
            if self.info.line_flags.contains(LineFlags::PURE_ASCII) {
                let byte = min(min_dist, self.text.len() - 1);

                (byte, 0)
            } else {
                match self.text.chars().enumerate().nth(min_dist) {
                    Some((col, _)) => (col, 0),
                    None => {
                        let count = self.text.chars().count();

                        (count - 1, 0)
                    }
                }
            }
        } else {
            let (mut col, mut distance) = (0, 0);

            let mut text_iter = self.text.chars().enumerate();

            while let Some((new_col, ch)) = text_iter.next() {
                col = new_col;

                if distance >= min_dist {
                    break;
                }

                distance += get_char_len(ch, distance, label, config);
            }

            (col, distance.saturating_sub(min_dist))
        }
    }

    // TODO: Eventually will include syntax highlighting, hover info, etc.
    /// Updates the information for a line in the file.
    ///
    /// Returns `true` if the screen needs a full refresh.
    fn update_line_info<U>(&mut self, node: &EndNode<U>)
    where
        U: Ui,
    {
        let label = node.label.read();
        let config = node.config().read();

        self.info.line_flags.set(LineFlags::PURE_ASCII, self.text.is_ascii());
        self.info.line_flags.set(
            LineFlags::PURE_1_COL,
            !self.text.chars().any(|c| label.get_char_len(c) > 1 || c == '\t'),
        );

        if !matches!(config.wrap_method, WrapMethod::NoWrap) {
            self.parse_wrapping::<U>(&label, &config);
        }
    }

    pub(crate) fn parse_wrapping<U>(&mut self, label: &<U>::Label, config: &Config)
    where
        U: Ui,
    {
        let indent = if config.wrap_indent { self.indent(label, config) } else { 0 };
        let indent = if indent < label.area().width() { indent } else { 0 };

        // Clear all `WrapppingChar`s from `char_tags`.
        self.info.char_tags.retain(|(_, t)| !matches!(t, CharTag::WrapppingChar));

        let mut distance = 0;
        let mut indent_wrap = 0;
        let area = *label.area();

        // TODO: Add an enum parameter signifying the wrapping type.
        // Wrapping at the final character at the width of the area.
        if self.info.line_flags.contains(LineFlags::PURE_1_COL | LineFlags::PURE_ASCII) {
            distance = area.width();
            while distance < self.text.len() {
                self.info.char_tags.insert((distance as u32, CharTag::WrapppingChar));

                indent_wrap = indent;

                distance += area.width() - indent_wrap;
            }
        } else {
            for (index, ch) in self.text.char_indices() {
                distance += get_char_len(ch, distance, label, config);

                if distance > area.width() - indent_wrap {
                    distance = get_char_len(ch, distance, label, config);

                    self.info.char_tags.insert((index as u32, CharTag::WrapppingChar));

                    indent_wrap = indent;
                }
            }
        }
    }

    /// Returns an iterator over the wrapping bytes of the line.
    pub fn wrap_iter(&self) -> impl Iterator<Item = u32> + '_ {
        self.info
            .char_tags
            .vec()
            .iter()
            .filter(|(_, t)| matches!(t, CharTag::WrapppingChar))
            .map(|(c, _)| *c)
    }

    /// Returns how many characters are in the line.
    pub fn char_count(&self) -> usize {
        if self.info.line_flags.contains(LineFlags::PURE_ASCII) {
            self.text.len()
        } else {
            self.text.chars().count()
        }
    }

    // NOTE: It always prints at `x = 0`, `x` in pos is treated here as an `x_shift`.
    /// Prints a line in a given position, skipping `skip` characters.
    ///
    /// Returns true if we should try printing the next line.
    #[inline]
    fn print<U>(
        &self, label: &mut U::Label, config: &Config, palette: &mut FormPalette, x_shift: usize,
        skip: usize,
    ) -> bool
    where
        U: Ui,
    {
        let (skip, d_x) = if let WrapMethod::NoWrap = config.wrap_method {
            // The leftover here represents the amount of characters that should not be printed,
            // for example, complex emoji may occupy several cells that should be empty, in the
            // case that part of the emoji is located before the first column.
            self.get_col_at_distance::<U>(x_shift, label, config)
        } else {
            (skip, 0)
        };

        let mut d_x = d_x as usize;
        (0..d_x).for_each(|_| label.print(' '));

        if let Some(first_wrap_col) = self.wrap_iter().next() {
            if skip >= first_wrap_col as usize && config.wrap_indent {
                (0..self.indent(label, config)).for_each(|_| label.print(' '));
            }
        }

        // Iterate through the tags before the first unskipped character.
        let (mut tags, mut cur_tag) = self.trigger_skipped::<U>(skip, label, palette);
        // As long as `![' ', '\t', '\n'].contains(last_ch)` initially, we're good.
        let mut last_ch = 'a';

        let text = self.text.char_indices().skip_while(|&(b, _)| b < skip);
        for (byte, ch) in text {
            let char_width = get_char_len(ch, d_x + x_shift, label, config);

            while let Some(&(tag_byte, tag)) = cur_tag {
                if byte == tag_byte as usize {
                    cur_tag = tags.next();

                    // If this is the first printed character of `top_line`, we don't wrap.
                    if let (CharTag::WrapppingChar, true) = (tag, d_x == 0) {
                        continue;
                    } else {
                        let indent = config.usable_indent(self, label);
                        if !tag.trigger(label, palette, indent) {
                            return false;
                        }
                    }
                } else {
                    break;
                }
            }

            d_x += char_width;
            if let WrapMethod::NoWrap = config.wrap_method {
                if d_x > label.area_mut().width() {
                    return true;
                }
            }

            match ch {
                '\t' => (0..char_width).for_each(|_| label.print(' ')),
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

    fn trigger_skipped<U>(
        &self, skip: usize, label: &mut U::Label, palette: &mut FormPalette,
    ) -> (Iter<(u32, CharTag)>, Option<&(u32, CharTag)>)
    where
        U: Ui,
    {
        let mut tags_iter = self.info.char_tags.vec().iter();
        let mut current_char_tag = None;
        while let Some(tag) = tags_iter.next() {
            if tag.0 as usize >= skip {
                current_char_tag = Some(tag);
                break;
            }
            if matches!(tag.1, CharTag::PushForm(_)) || matches!(tag.1, CharTag::PopForm(_)) {
                tag.1.trigger(label, palette, 0);
            }
        }
        (tags_iter, current_char_tag)
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
            info.char_tags.insert((start as u32, CharTag::PushForm(self.forms[index])));
            if index > 0 {
                info.char_tags.insert((start as u32, CharTag::PopForm(self.forms[index - 1])));
            }
            removed_len += 2;
        }

        TextLine { text: formless_text, info }
    }

    /// Makes it so this `TextLineBuilder`
    pub fn extend(&mut self, text: &mut String, palette: &FormPalette) {
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
pub struct Text {
    pub lines: Vec<TextLine>,
    replacements: Vec<(Vec<TextLine>, RangeInclusive<usize>, bool)>,
    pub(crate) match_manager: Option<MatchManager>,
}

impl Default for Text {
    fn default() -> Self {
        Text { lines: vec![TextLine::from("")], replacements: Vec::new(), match_manager: None }
    }
}

// TODO: Properly implement replacements.
impl Text {
    /// Returns a new instance of `Text`.
    pub fn new(text: String, match_manager: Option<MatchManager>) -> Self {
        Text {
            lines: text.split_inclusive('\n').map(|l| TextLine::from(l)).collect(),
            replacements: Vec::new(),
            match_manager,
        }
    }

    pub(crate) fn update_lines(&mut self, node: &EndNode<impl Ui>) {
        for line in &mut self.lines {
            line.update_line_info(node);
        }
    }

    /// Prints the contents of a given area in a given `EndNode`.
    pub(crate) fn print<U>(&self, end_node: &mut EndNode<U>, print_info: PrintInfo)
    where
        U: Ui,
    {
        let mut label = end_node.label.write();
        let config = end_node.config.read();
        let mut palette = end_node.palette.write();

        if end_node.is_active {
            label.set_as_active();
        }
        label.start_printing();

        // Print the `top_line`.
        let top_line = &self.lines[print_info.top_row];
        let top_wraps = print_info.top_wraps;
        let skip = if top_wraps > 0 { top_line.wrap_iter().nth(top_wraps - 1).unwrap() } else { 0 };
        top_line.print::<U>(&mut label, &config, &mut palette, print_info.x_shift, skip as usize);

        // Prints other lines until it can't anymore.
        for line in self.lines.iter().skip(print_info.top_row + 1) {
            if !line.print::<U>(&mut label, &config, &mut palette, print_info.x_shift, 0) {
                break;
            }
        }

        palette.clear_applied();

        label.stop_printing();
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
        let mut d_y = 1 + top_line.1.wrap_iter().count();
        for _ in 0..min(d_y - print_info.top_wraps, height) {
            printed_lines.push(top_line.0);
        }

        // List all the other lines.
        while let (Some((index, line)), true) = (lines_iter.next(), d_y < height_sum) {
            let line_count = 1 + line.wrap_iter().count();
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
    pub(crate) fn remove_cursor_tags(&mut self, cursors: &[TextCursor], main_cursor_index: usize) {
        for (index, cursor) in cursors.iter().enumerate() {
            let TextRange { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = if index == main_cursor_index {
                (CharTag::MainCursor, CharTag::MainSelStart, CharTag::MainSelEnd)
            } else {
                (CharTag::SecondaryCursor, CharTag::SecondarySelStart, CharTag::SecondarySelStart)
            };
            let pos_list = [(start, start_tag), (end, end_tag), (cursor.caret(), caret_tag)];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.iter().skip(no_selection) {
                if let Some(line) = self.lines.get_mut(pos.row) {
                    let byte = line.get_line_byte_at(pos.col);
                    line.info.char_tags.remove_first(|(n, t)| n as usize == byte && t == *tag);
                }
            }
        }
    }

    /// Adds the tags for all the cursors, used after they are expected to have moved.
    pub(crate) fn add_cursor_tags(&mut self, cursors: &[TextCursor], main_cursor_index: usize) {
        for (index, cursor) in cursors.iter().enumerate() {
            let TextRange { start, end } = cursor.range();
            let (caret_tag, start_tag, end_tag) = if index == main_cursor_index {
                (CharTag::MainCursor, CharTag::MainSelStart, CharTag::MainSelEnd)
            } else {
                (CharTag::SecondaryCursor, CharTag::SecondarySelStart, CharTag::SecondarySelStart)
            };

            let pos_list = [(start, start_tag), (end, end_tag), (cursor.caret(), caret_tag)];

            let no_selection = if start == end { 2 } else { 0 };

            for (pos, tag) in pos_list.iter().skip(no_selection) {
                if let Some(line) = self.lines.get_mut(pos.row) {
                    let byte = line.get_line_byte_at(pos.col) as u32;
                    line.info.char_tags.insert((byte, *tag));
                }
            }
        }
    }
}

impl<S> From<S> for Text
where
    S: ToString,
{
    fn from(value: S) -> Self {
        Text {
            lines: value.to_string().split_inclusive('\n').map(|l| TextLine::from(l)).collect(),
            replacements: Vec::new(),
            match_manager: None,
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
    pub fn scroll_vertically(&mut self, mut d_y: i32, text: &Text) {
        if d_y > 0 {
            let mut lines_iter = text.lines().iter().skip(self.top_row);

            while let Some(line) = lines_iter.next() {
                let wrap_count = line.wrap_iter().count();
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
                let wrap_count = line.wrap_iter().count();
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

    pub fn scroll_horizontally<U>(&mut self, d_x: i32, text: &Text, node: &EndNode<U>)
    where
        U: Ui,
    {
        let mut max_d = 0;
        let label = node.label.read();
        let config = node.config().read();

        for index in text.printed_lines(label.area().height(), self) {
            let line = &text.lines()[index];
            let line_d = line.get_distance_to_col::<U>(line.char_count(), &label, &config);
            max_d = max(max_d, line_d);
        }

        self.x_shift = min(self.x_shift.saturating_add_signed(d_x as isize), max_d);
    }

    /// Scrolls up or down, assuming that the lines cannot wrap.
    fn calibrate_vertically<U>(
        &mut self, target: TextPos, height: usize, end_node: &RwData<EndNode<U>>,
    ) where
        U: Ui,
    {
        let scrolloff = end_node.read().config().read().scrolloff;

        if target.row > self.top_row + height - scrolloff.d_y {
            self.top_row += target.row + scrolloff.d_y - self.top_row - height;
        } else if target.row < self.top_row + scrolloff.d_y && self.top_row != 0 {
            self.top_row -= (self.top_row + scrolloff.d_y) - target.row
        }
    }

    /// Scrolls up, assuming that the lines can wrap.
    fn calibrate_up<U>(
        &mut self, target: TextPos, mut d_y: usize, text: &Text, end_node: &RwData<EndNode<U>>,
    ) where
        U: Ui,
    {
        let scrolloff = end_node.read().config().read().scrolloff;
        let lines_iter = text.lines().iter().take(target.row);

        // If the target line is above the top line, no matter what, a new top line is needed.
        let mut needs_new_top_line = target.row < self.top_row;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.row {
                d_y += 1 + line.wrap_iter().count();
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
        &mut self, target: TextPos, mut d_y: usize, text: &Text, height: usize,
        end_node: &RwData<EndNode<U>>,
    ) where
        U: Ui,
    {
        let mut scrolloff = end_node.read().config().read().scrolloff;
        scrolloff.d_y = min(scrolloff.d_y, height);
        let lines_iter = text.lines().iter().take(target.row + 1);
        let mut top_offset = 0;

        for (index, line) in lines_iter.enumerate().rev() {
            if index != target.row {
                d_y += 1 + line.wrap_iter().count();
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
        &mut self, target: TextPos, text: &Text, width: usize, end_node: &RwData<EndNode<U>>,
    ) where
        U: Ui,
    {
        let node = end_node.read();
        let config = node.config().read();
        let label = node.label.read();
        let mut scrolloff = config.scrolloff;
        scrolloff.d_x = min(scrolloff.d_x, width);

        if let WrapMethod::NoWrap = config.wrap_method {
            let target_line = &text.lines[target.row];
            let distance = target_line.get_distance_to_col::<U>(target.col, &label, &config);

            // If the distance is greater, it means that the cursor is out of bounds.
            if distance > self.x_shift + width - config.scrolloff.d_x {
                // Shift by the amount required to keep the cursor in bounds.
                self.x_shift = distance + scrolloff.d_x - width;
            // Check if `info.x_shift` is already at 0, if it is, no scrolling is dones.
            } else if distance < self.x_shift + scrolloff.d_x {
                self.x_shift = distance.saturating_sub(config.scrolloff.d_x);
            }
        }
    }

    /// Updates the print info.
    pub fn update<U>(&mut self, target: TextPos, text: &Text, end_node: &RwData<EndNode<U>>)
    where
        U: Ui,
    {
        let node = end_node.read();
        let wrap_method = node.config().read().wrap_method;
        let (height, width) = (node.label.read().area().height(), node.label.read().area().width());
        drop(node);

        let old = self.last_main;

        let line = &text.lines()[min(old.row, text.lines().len() - 1)];
        let current_byte = line.get_line_byte_at(old.col);
        let cur_wraps = line.wrap_iter().take_while(|&c| c <= current_byte as u32).count();

        let line = &text.lines()[target.row];
        let target_byte = line.get_line_byte_at(target.col);
        let old_wraps = line.wrap_iter().take_while(|&c| c <= target_byte as u32).count();

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

fn get_char_len<L, A>(ch: char, col: usize, label: &L, config: &Config) -> usize
where
    L: Label<A>,
    A: Area,
{
    if ch == '\t' {
        config.tab_places.get_tab_len(col, label)
    } else {
        label.get_char_len(ch)
    }
}

pub(crate) fn update_range(
    text: &mut Text, range: TextRange, max_line: usize, node: &EndNode<impl Ui>,
) {
    if let Some(match_manager) = &mut text.match_manager {
        let line = &text.lines[max_line];
        let max_pos = TextPos::default().translate(&text.lines, max_line, line.char_count());

        let start = TextPos {
            row: range.start.row,
            col: 0,
            byte: range.start.byte - get_byte(&text.lines[range.start.row].text(), range.start.col),
        };

        let len = text.lines[range.end.row].text().len();
        let end = TextPos {
            row: range.end.row,
            col: text.lines[range.end.row].char_count(),
            byte: range.end.byte - get_byte(&text.lines[range.end.row].text(), range.end.col) + len,
        };

        let range = TextRange { start, end };

        let line_infos = match_manager.match_range(text.lines.as_slice(), range, max_pos);

        for (line_info, line_num) in line_infos {
            text.lines[line_num].info = line_info;
            text.lines[line_num].update_line_info(node);
        }
    }
}
