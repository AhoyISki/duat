use std::cmp::min;

use unicode_width::UnicodeWidthChar;

use crate::{
    action::{History, TextRange},
    config::{PrintOptions,ParsecOptionss, TabPlaces, WrapMethod, ConfigOptions},
    cursor::{TextCursor, TextPos},
    layout::PrintInfo,
    tags::{CharTag, Form, LineFlags, LineInfo, MatchManager},
    ui::{Area, ChildNode},
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
    pub fn indent(&self, tabs: &TabPlaces) -> usize {
        let mut indent_sum = 0;

        for ch in self.text.chars() {
            if ch == ' ' || ch == '\t' {
                indent_sum += get_char_width(ch, indent_sum, tabs);
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
    pub fn get_distance_to_col(&self, col: usize, tabs: &TabPlaces) -> usize {
        let mut width = 0;

        if self.info.line_flags.contains(LineFlags::PURE_1_COL) {
            width = col
        } else {
            for ch in self.text.chars().take(col) {
                width += if ch == '\t' {
                    tabs.get_tab_len(width)
                } else {
                    UnicodeWidthChar::width(ch).unwrap_or(1)
                };
            }
        }

        width
    }

    /// Returns the column and byte found at visual distance from 0. Also returns any leftovers.
    ///
    /// The leftover number is positive if the width of the characters is greater (happens if the
    /// last checked character has a width greater than 1), and 0 otherwise.
    pub fn get_col_at_distance(&self, min_dist: usize, tabs: &TabPlaces) -> (usize, usize) {
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

                distance += get_char_width(ch, distance, tabs);
            }

            (col, distance.saturating_sub(min_dist))
        }
    }

    // TODO: Eventually will include syntax highlighting, hover info, etc.
    /// Updates the information for a line in the file.
    ///
    /// Returns `true` if the screen needs a full refresh.
    pub fn update_line_info(&mut self, options: &ConfigOptions, width: usize) {
        self.info.line_flags.set(LineFlags::PURE_ASCII, self.text.is_ascii());
        self.info.line_flags.set(
            LineFlags::PURE_1_COL,
            !self.text.chars().any(|c| UnicodeWidthChar::width(c).unwrap_or(1) > 1 || c == '\t'),
        );

		self.parse_wrapping(options, width);
    }
    
	pub fn parse_wrapping(&mut self, options: &ConfigOptions, width: usize) {
        let indent = if options.wrap_indent { self.indent(&options.tab_places) } else { 0 };
        let indent = if indent < width { indent } else { 0 };

        // Clear all `WrapppingChar`s from `char_tags`.
        self.info.char_tags.retain(|(_, t)| !matches!(t, CharTag::WrapppingChar));

        let mut distance = 0;
        let mut indent_wrap = 0;

        // TODO: Add an enum parameter signifying the wrapping type.
        // Wrapping at the final character at the width of the area.
        if self.info.line_flags.contains(LineFlags::PURE_1_COL | LineFlags::PURE_ASCII) {
            distance = width;
            while distance < self.text.len() {
                self.info.char_tags.insert((distance as u32, CharTag::WrapppingChar));

                indent_wrap = indent;

                distance += width - indent_wrap;
            }
        } else {
            for (index, ch) in self.text.char_indices() {
                distance += get_char_width(ch, distance, &options.tab_places);

                if distance > width - indent_wrap {
                    distance = get_char_width(ch, distance, &options.tab_places);

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
    pub(crate) fn print<A>(
        &self, node: &mut ChildNode<A>, x_shift: usize, skip: usize, options: &ConfigOptions,
        forms: &[Form],
    ) -> bool
    where
        A: Area,
    {
        let (skip, d_x) = if let WrapMethod::NoWrap = options.wrap_method {
            // The leftover here represents the amount of characters that should not be printed,
            // for example, complex emoji may occupy several cells that should be empty, in the
            // case that part of the emoji is located before the first column.
            self.get_col_at_distance(x_shift, &options.tab_places)
        } else {
            (skip, 0)
        };

        let mut d_x = d_x as usize;
        (0..d_x).for_each(|_| node.area.print(' '));

        let char_width = |c, x| {
            if self.info.line_flags.contains(LineFlags::PURE_1_COL) {
                1
            } else {
                get_char_width(c, x, &options.tab_places)
            }
        };

        if let Some(first_wrap_col) = self.wrap_iter().next() {
            if skip >= first_wrap_col as usize && options.wrap_indent {
                (0..self.indent(&options.tab_places)).for_each(|c| node.area.print(' '));
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
        //let tags_iter = tags.vec().iter().skip(pre_skip).take_while(|(c, _)| (*c as usize) < skip);

		// Iterate through the tags before the first unskipped character.
        let mut tags_iter = self.info.char_tags.vec().iter();
        let mut current_char_tag = None;
        while let Some(tag) = tags_iter.next() {
            if tag.0 as usize >= skip {
                current_char_tag = Some(tag);
                break;
            } if matches!(tag.1, CharTag::PushForm(_)) || matches!(tag.1, CharTag::PopForm(_)) {
                tag.1.trigger(node, forms, 0);
            }
        }

        let wrap_indent = if options.wrap_indent { self.indent(&options.tab_places) } else { 0 };
        // If `wrap_indent >= area.width()`, indenting on wraps becomes impossible.
        let wrap_indent = if wrap_indent < node.area.width() { wrap_indent } else { 0 };

        let text_iter = self.text.char_indices().skip_while(|&(b, _)| b < skip);
        for (byte, ch) in text_iter {
            let char_width = char_width(ch, d_x + x_shift);

            while let Some(&(tag_byte, tag)) = current_char_tag {
                if byte == tag_byte as usize {
                    current_char_tag = tags_iter.next();

                    // If this is the first printed character of `top_line`, we don't wrap.
                    if let (CharTag::WrapppingChar, true) = (tag, d_x == 0) {
                        continue;
                    } else {
                        if !tag.trigger(node, forms, wrap_indent) {
                            node.area.clear_form();
                            return false;
                        }
                    }
                } else {
                    break;
                }
            }

            d_x += char_width;
            if let WrapMethod::NoWrap = options.wrap_method {
                if d_x > node.area.width() {
                    break;
                }
            }

            if ch == '\t' {
                // `repeat()` would use string allocation (I think).
                (0..char_width).for_each(|_| node.area.print(' '));
            } else if ch == '\n' {
                node.area.print(' ');
            } else {
                node.area.print(ch);
            }
        }

        node.area.clear_form();
        if !node.area.next_line() { false } else { true }
    }

    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn text(&self) -> &str {
        &self.text.as_str()
    }
}

/// File text and cursors.
pub struct File<A>
where
    A: Area,
{
    /// The lines of the file.
    pub lines: Vec<TextLine>,

    /// Where on the file to start printing.
    print_info: PrintInfo,

    /// The area allocated to the file.
    pub area: A,

    /// The options related to files.
    pub options: PrintOptions,

    /// The edtiting cursors on the file.
    pub cursors: Vec<TextCursor>,
    /// The index of the main cursor. The file "follows it".
    pub main_cursor: usize,

    /// The history of edits on this file.
    pub history: History,

    /// The manager for character tags and file flag mutation.
    tag_manager: MatchManager,
}

impl<A> File<A>
where
    A: Area {
    /// Applies a splice to the file.
    pub fn splice_edit<S>(&mut self, edit: Vec<S>, old_range: TextRange)
    where
        S: ToString,
    {
        let (edits, new_range) = self.history.add_change(&mut self.lines, edit, old_range);

        let edits: Vec<TextLine> = edits.iter().map(|l| TextLine::new(l)).collect();
        self.lines.splice(old_range.lines(), edits);

        match_range(&mut self.lines, new_range, &self.area, &self.options, &mut self.tag_manager);

        for cursor in &mut self.cursors {
            let new_pos = if cursor.target() >= old_range.end {
                let mut new_pos = cursor.target();
                new_pos = new_pos.col_add(new_range.end).col_sub(old_range.end);
                new_pos.line += new_range.end.line;
                new_pos.line -= old_range.end.line;

                new_pos
            } else if cursor.target() > old_range.start {
                min(cursor.target(), new_range.end)
            } else {
                continue;
            };

            cursor.move_to(new_pos, &self.lines, &self.options)
        }
    }

    /// Undoes the last moment in history.
    pub fn undo(&mut self) {
        let (splices, print_info) = match self.history.undo(&mut self.lines) {
            Some((changes, print_info)) => (changes, print_info),
            None => return,
        };
        self.print_info = print_info.unwrap_or(self.print_info);

        let mut cursors = self.cursors.iter_mut();
        let mut new_cursors = Vec::new();

        for splice in &splices {
            if let Some(cursor) = cursors.next() {
                cursor.move_to(splice.taken_end(), &self.lines, &self.options);
            } else {
                new_cursors.push(TextCursor::new(
                    splice.taken_end(),
                    &self.lines,
                    &self.options.tabs,
                ));
            }

            let range = TextRange { start: splice.start(), end: splice.taken_end() };
            match_range(&mut self.lines, range, &self.area, &self.options, &mut self.tag_manager);
        }

        self.cursors.extend(new_cursors);
    }

    /// Re-does the last moment in history.
    pub fn redo(&mut self) {
        let (splices, print_info) = match self.history.redo(&mut self.lines) {
            Some((changes, print_info)) => (changes, print_info),
            None => return,
        };
        self.print_info = print_info.unwrap_or(self.print_info);

        let mut cursors = self.cursors.iter_mut();
        let mut new_cursors = Vec::new();

        for splice in &splices {
            if let Some(cursor) = cursors.next() {
                cursor.move_to(splice.added_end(), &self.lines, &self.options);
            } else {
                new_cursors.push(TextCursor::new(
                    splice.added_end(),
                    &self.lines,
                    &self.options.tabs,
                ));
            }

            let range = TextRange { start: splice.start(), end: splice.added_end() };
            match_range(&mut self.lines, range, &self.area, &self.options, &mut self.tag_manager);
        }

        self.cursors.extend(new_cursors);
    }

    /// Prints the file, according to its current position.
    pub fn print_file(&mut self) {
    }

    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn print_info(&self) -> PrintInfo {
        self.print_info
    }
}

pub fn get_char_width(ch: char, col: usize, tabs: &TabPlaces) -> usize {
    if ch == '\t' { tabs.get_tab_len(col) } else { UnicodeWidthChar::width(ch).unwrap_or(1) }
}
