use std::{cmp::min, ops::RangeInclusive};

use crate::{
    action::{extend_edit, get_byte, TextRange},
    config::{Config, TabPlaces, WrapMethod},
    cursor::{TextCursor, TextPos},
    layout::PrintInfo,
    tags::{CharTag, Form, LineFlags, LineInfo, MatchManager},
    ui::{EndNode, Ui},
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
    /// Returns a new instance of `TextLine`.
    pub fn new(text: String) -> TextLine {
        let info = LineInfo::default();

        TextLine { text, info }
    }

    /// Returns the line's indentation.
    pub fn indent(&self, node: &EndNode<impl Ui>) -> usize {
        let mut indent_sum = 0;

        for ch in self.text.chars() {
            if ch == ' ' || ch == '\t' {
                indent_sum += get_char_width(ch, indent_sum, node);
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
    pub fn get_distance_to_col(&self, col: usize, node: &EndNode<impl Ui>) -> usize {
        let mut width = 0;

        if self.info.line_flags.contains(LineFlags::PURE_1_COL) {
            width = col
        } else {
            for ch in self.text.chars().take(col) {
                width += if ch == '\t' {
                    node.config().tab_places.get_tab_len(width, node)
                } else {
                    node.get_char_len(ch)
                };
            }
        }

        width
    }

    /// Returns the column and byte found at visual distance from 0. Also returns any leftovers.
    ///
    /// The leftover number is positive if the width of the characters is greater (happens if the
    /// last checked character has a width greater than 1), and 0 otherwise.
    pub fn get_col_at_distance(&self, min_dist: usize, node: &EndNode<impl Ui>) -> (usize, usize) {
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

                distance += get_char_width(ch, distance, node);
            }

            (col, distance.saturating_sub(min_dist))
        }
    }

    // TODO: Eventually will include syntax highlighting, hover info, etc.
    /// Updates the information for a line in the file.
    ///
    /// Returns `true` if the screen needs a full refresh.
    pub fn update_line_info(&mut self, options: &Config, node: &EndNode<impl Ui>) {
        self.info.line_flags.set(LineFlags::PURE_ASCII, self.text.is_ascii());
        self.info.line_flags.set(
            LineFlags::PURE_1_COL,
            !self.text.chars().any(|c| node.get_char_len(c) > 1 || c == '\t'),
        );

        self.parse_wrapping(options, node);
    }

    pub fn parse_wrapping(&mut self, options: &Config, node: &EndNode<impl Ui>) {
        let indent = if options.wrap_indent { self.indent(node) } else { 0 };
        let indent = if indent < node.width() { indent } else { 0 };

        // Clear all `WrapppingChar`s from `char_tags`.
        self.info.char_tags.retain(|(_, t)| !matches!(t, CharTag::WrapppingChar));

        let mut distance = 0;
        let mut indent_wrap = 0;

        // TODO: Add an enum parameter signifying the wrapping type.
        // Wrapping at the final character at the width of the area.
        if self.info.line_flags.contains(LineFlags::PURE_1_COL | LineFlags::PURE_ASCII) {
            distance = node.width();
            while distance < self.text.len() {
                self.info.char_tags.insert((distance as u32, CharTag::WrapppingChar));

                indent_wrap = indent;

                distance += node.width() - indent_wrap;
            }
        } else {
            for (index, ch) in self.text.char_indices() {
                distance += get_char_width(ch, distance, node);

                if distance > node.width() - indent_wrap {
                    distance = get_char_width(ch, distance, node);

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
    /// Returns the amount of wrapped lines that were printed.
    #[inline]
    pub(crate) fn print(
        &self, node: &mut EndNode<impl Ui>, x_shift: usize, skip: usize, forms: &[Form],
    ) -> bool {
        let (skip, d_x) = if let WrapMethod::NoWrap = node.options().wrap_method {
            // The leftover here represents the amount of characters that should not be printed,
            // for example, complex emoji may occupy several cells that should be empty, in the
            // case that part of the emoji is located before the first column.
            self.get_col_at_distance(x_shift, node)
        } else {
            (skip, 0)
        };

        let mut d_x = d_x as usize;
        (0..d_x).for_each(|_| node.print(' '));

        if let Some(first_wrap_col) = self.wrap_iter().next() {
            if skip >= first_wrap_col as usize && node.options().wrap_indent {
                (0..self.indent(node)).for_each(|_| node.print(' '));
            }
        }

        //// NOTE: This is a freakishly large number of tags to be in a single line.
        //// NOTE: If a line you wrote has this many tags, frankly, you're a bad programmer.
        //let pre_skip = if tags.vec().len() < 300 {
        //    0
        //// If, somehow, `len >= 300`, we look back at 100 lines back, to complete any forms
        //// that could possibly show up.
        //} else {
        //    match tags.vec().iter().enumerate().find(|(_, (c, _))| (*c as usize) >= skip) {
        //        Some((first_shown_tag, _)) => first_shown_tag.saturating_sub(100),
        //        None => tags.vec().len().saturating_sub(100),
        //    }
        //};

        //// Iterating from 10 character tags back, until the first tag is printed.
        //let tags_iter = tags.vec().iter().skip(pre_skip).take_while(|(c, _)| (*c as usize) <
        // skip);

        // Iterate through the tags before the first unskipped character.
        let mut tags_iter = self.info.char_tags.vec().iter();
        let mut current_char_tag = None;
        while let Some(tag) = tags_iter.next() {
            if tag.0 as usize >= skip {
                current_char_tag = Some(tag);
                break;
            }
            if matches!(tag.1, CharTag::PushForm(_)) || matches!(tag.1, CharTag::PopForm(_)) {
                tag.1.trigger(node, forms, 0);
            }
        }

        let wrap_indent = if node.options().wrap_indent { self.indent(node) } else { 0 };
        // If `wrap_indent >= area.width()`, indenting on wraps becomes impossible.
        let wrap_indent = if wrap_indent < node.width() { wrap_indent } else { 0 };

        let text_iter = self.text.char_indices().skip_while(|&(b, _)| b < skip);
        for (byte, ch) in text_iter {
            let char_width = get_char_width(ch, d_x + x_shift, node);

            while let Some(&(tag_byte, tag)) = current_char_tag {
                if byte == tag_byte as usize {
                    current_char_tag = tags_iter.next();

                    // If this is the first printed character of `top_line`, we don't wrap.
                    if let (CharTag::WrapppingChar, true) = (tag, d_x == 0) {
                        continue;
                    } else {
                        if !tag.trigger(node, forms, wrap_indent) {
                            node.clear_form();
                            return false;
                        }
                    }
                } else {
                    break;
                }
            }

            d_x += char_width;
            if let WrapMethod::NoWrap = node.options().wrap_method {
                if d_x > node.width() {
                    break;
                }
            }

            if ch == '\t' {
                // `repeat()` would use string allocation (I think).
                (0..char_width).for_each(|_| node.print(' '));
            } else if ch == '\n' {
                node.print(' ');
            } else {
                node.print(ch);
            }
        }

        node.clear_form();
        if !node.next_line() { false } else { true }
    }

    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn text(&self) -> &str {
        &self.text.as_str()
    }
}

/// The text in a given area.
#[derive(Default)]
pub struct Text {
    pub(crate) lines: Vec<TextLine>,
    replacements: Vec<(Vec<TextLine>, RangeInclusive<usize>, bool)>,
    pub(crate) match_manager: Option<MatchManager>,
}

// TODO: Properly implement replacements.
impl Text {
    pub fn new(text: String, match_manager: Option<MatchManager>) -> Self {
        Text {
            lines: text.split_inclusive('\n').map(|l| TextLine::new(l.to_string())).collect(),
            replacements: Vec::new(),
            match_manager,
        }
    }

    /// Prints the contents of a given area in a given `EndNode`impl Container, .
    pub fn print(&self, node: &mut EndNode<impl Ui>, print_info: PrintInfo) {
        node.start_printing();

        // Print the `top_line`.
        let top_line = &self.lines[print_info.top_line];
        let top_wraps = print_info.top_wraps;
        let skip = if top_wraps > 0 { top_line.wrap_iter().nth(top_wraps - 1).unwrap() } else { 0 };
        let forms = self.match_manager.as_ref().map(|m| m.forms()).unwrap_or(&[]);
        top_line.print(node, print_info.x_shift, skip as usize, forms);

        // Prints other lines until it can't anymore.
        for line in self.lines.iter().skip(print_info.top_line) {
            if !line.print(node, print_info.x_shift, 0, forms) {
                break;
            }
        }

        node.stop_printing();
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
        let top_line = lines_iter.nth(print_info.top_line).unwrap();
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

    pub(crate) fn splice(&mut self, range: TextRange, edit: impl ToString, max_line: usize) {
        let edit_lines = edit.to_string().split_inclusive('\n').map(|t| String::from(t)).collect();

        let old = self.lines[range.lines()].iter().map(|l| l.text()).collect();

        let new_lines = extend_edit(old, edit_lines, range).0;
        let new_lines: Vec<TextLine> = new_lines.iter().map(|l| TextLine::new(l.clone())).collect();

        self.lines.splice(range.lines(), new_lines);
    }

    /// Splices a given edit on the selection of a cursor.
    pub fn splice_on_cursor(&mut self, cursor: &TextCursor, edit: impl ToString) {
        self.splice(cursor.range(), edit, self.lines.len() - 1);
    }

    pub fn lines(&self) -> &[TextLine] {
        self.lines.as_slice()
    }
}

pub fn get_char_width(ch: char, col: usize, node: &EndNode<impl Ui>) -> usize {
    if ch == '\t' {
        node.config().tab_places.get_tab_len(col, &node)
    } else {
        node.get_char_len(ch)
    }
}

pub(crate) fn update_range(
    text: &mut Text, range: TextRange, max_line: usize, node: &EndNode<impl Ui>,
) {
    if let Some(match_manager) = &mut text.match_manager {
        let line = &text.lines[max_line];
        let max_pos = range.end.translate_to(&text.lines, max_line, line.char_count());

        let start = TextPos {
            line: range.start.line,
            col: 0,
            byte: range.start.byte
                - get_byte(&text.lines[range.start.line].text(), range.start.col),
        };

        let len = text.lines[range.end.line].text().len();
        let end = TextPos {
            line: range.end.line,
            col: text.lines[range.end.line].char_count(),
            byte: range.end.byte - get_byte(&text.lines[range.end.line].text(), range.end.col)
                + len,
        };

        let range = TextRange { start, end };

        let line_infos = match_manager.match_range(text.lines.as_slice(), range, max_pos);

        for (line_info, line_num) in line_infos {
            text.lines[line_num].info = line_info;
            text.lines[line_num].update_line_info(node.options(), node);
        }
    }
}
