use std::{cmp::max, ops::{Range, RangeFrom, RangeBounds}};

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use crate::{
    config::{FileOptions, TabPlaces, WrapMethod},
    output::{OutputArea, OutputPos, StyledChar},
    action::{Selection, Insertion},
};

use crate::cursor::{CursorPos, FileCursor, FilePos};

// TODO: move this to a more general file.
/// A line in the text file.
pub struct TextLine {
    /// Which columns on the line should wrap around.
    wrap_cols: Vec<usize>,
    /// The text on the line.
    text: Vec<StyledChar>,
    /// The line's indentation.
    indent: usize,

    // Since `text.len()` wouldn't take into account the visual width of cols.
    /// The visual lenght of the line, taking into account double width chars.
    width: usize,
}

impl TextLine {
    /// Returns a new instance of `TextLine`
    pub fn new(text: &str, tabs: &TabPlaces) -> TextLine {
        let mut file_line = TextLine {
            wrap_cols: Vec::new(),
            text: Vec::new(),
            indent: 0,
            width: 0,
        };

        for grapheme in text.graphemes(true) {
            let width = UnicodeWidthStr::width(grapheme);

            file_line.text.push(StyledChar::new(grapheme, width));
        }

        file_line.width += file_line.text.len();

        file_line
    }

    /// Replaces a range in a line with text.
    ///
    /// The range may be empty, as in, `0..0`;
    fn splice_text<R: RangeBounds<usize>>(&mut self, text: &str, range: R) {
        let mut processed_text = Vec::new();

        for grapheme in text.graphemes(true) {
            let width = UnicodeWidthStr::width(grapheme);

            processed_text.push(StyledChar::new(grapheme, width));
        }
        
        self.text.splice(range, processed_text);
    }

    /// Calculates, sets, and returns the line's indentation.
    pub fn calculate_indentation(&mut self, tabs: &TabPlaces) -> usize {
        let mut indent = 0;
        for ch in &self.text {
            if ch.ch.content() == " " || ch.ch.content() == "\t" {
                indent += ch.width(indent, tabs);
            } else {
                break;
            }
        }
        self.indent = indent as usize;
        indent as usize
    }

    /// Returns the visual distance to a certain column.
    pub fn get_distance_to_col(&self, col: usize, tabs: &TabPlaces) -> u16 {
        let mut width = 0;
        for col in self.text.get(0..col).expect("invalid col") {
            width += col.width(width, tabs);
        }
        width
    }

    /// Returns the column found at a certain visual distance from 0. Also returns any leftovers.
    ///
    /// The leftover number is positive if the width of the characters is greater (happens if the
    /// last character has a width greater than 1), and it is negative if the distance is greater
    /// (happens if there aren't enough characters to cover the distance.
    pub fn get_col_at_distance(&self, dist: u16, tabs: &TabPlaces) -> (usize, i32) {
        let (mut index, mut width) = (0, 0);
        let mut text_iter = self.text.iter();
        while width < dist {
            match text_iter.next() {
                Some(ch) => {
                    width += ch.width(width, tabs);
                    index += 1;
                }
                None => break,
            }
        }
        (index, width as i32 - dist as i32)
    }

    /// Parses the wrapping of a single line.
    fn parse_wrapping(&mut self, width: u16, options: &FileOptions) {
        self.wrap_cols.clear();

        let mut cur_x = 0;
        let mut wrap_indentation = 0;
        // TODO: Add an enum parameter signifying the wrapping type.
        // Wrapping at the final character at the width of the area.
        for (index, ch) in self.text.iter_mut().enumerate() {
            cur_x += ch.width(cur_x, &options.tabs) as u16;

            if cur_x == width - wrap_indentation {
                cur_x = 0;
                self.wrap_cols.push(index);
                ch.is_wrapping = true;
                if options.wrap_indent {
                    wrap_indentation = self.indent as u16;
                }
            } else {
                ch.is_wrapping = false;
            }
        }
    }

    // TODO: Make this a generic free function, and make areas do bounds checking.
    /// Prints a line in a given position, skipping `skip` characters.
    ///
    /// Returns the amount of lines that were printed.
    #[inline]
    fn print<T: OutputArea>(
        &self, area: &mut T, mut pos: OutputPos, skip: usize,
        options: &FileOptions, x_shift: u16, ) -> u16 {
        // Moves the printing cursor to the beginning of the line.
        area.move_cursor(pos);

        let mut printed_lines = 1;
        let mut col = 0;

        let (skip, leftover) = if let WrapMethod::NoWrap = options.wrap_method {
            self.get_col_at_distance(x_shift, &options.tabs)
        } else {
            (skip, 0)
        };
        // if x_shift > 1 { panic!("{}, {}, {}", skip, leftover, x_shift) }

        area.print(" ".repeat(max(0, leftover) as usize));

        for ch in self.text().iter().skip(skip) {
            if let WrapMethod::NoWrap = options.wrap_method {
                if col + ch.width(col, &options.tabs) > area.width() {
                    break;
                }
            }

            if ch.ch.content() == "\t" {
                let tab_len = options.tabs.get_tab_len(col + x_shift);
                area.print(" ".repeat(tab_len as usize));
                col += tab_len;
            } else {
                area.print(ch.ch.clone());
                col += ch.width(col, &options.tabs);
            }

            if ch.is_wrapping {
                printed_lines += 1;
                pos.y += 1;
                if pos.y > area.height() {
                    break;
                }
                area.move_cursor(pos);
                if options.wrap_indent && pos.x == 0 {
                    area.print(" ".repeat(self.indent));
                }
            }
        }

        // Erasing anything that is leftover
        let width = area.width();
        if pos.y <= area.height() {
            // NOTE: Eventually will be improved when issue #53667 on rust-lang gets closed.
            if let WrapMethod::Width = options.wrap_method {
                area.print(" ".repeat((width - col % width) as usize));
            } else if col < width {
                area.print(" ".repeat((width - col % width) as usize));
            }
        }

        printed_lines
    }
    
    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn wrap_cols(&self) -> &Vec<usize> {
        &self.wrap_cols
    }

    pub fn text(&self) -> &Vec<StyledChar> {
        &self.text
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn indent(&self) -> usize {
        self.indent
    }
}

/// File text and cursors.
pub struct File<T> {
    /// The lines of the file.
    pub lines: Vec<TextLine>,
    /// The index of the line at the top of the screen.
    top_line: usize,
    /// The number of times the top line should wrap.
    top_wraps: usize,

    /// The leftmost col shown on the screen.
    x_shift: u16,

    /// The area allocated to the file.
    pub area: T,

    /// The options related to files.
    pub options: FileOptions,

    /// The edtiting cursors on the file.
    pub cursors: Vec<FileCursor>,
    /// The index of the main cursor. The file "follows it".
    pub main_cursor: usize,
}

impl<T: OutputArea> File<T> {
    /// Returns a new instance of `File<T>`, given a `Vec<FileLine>`.
    pub fn new(lines: Vec<TextLine>, options: FileOptions, area: T) -> File<T> {
        let mut file = File {
            lines,
            // TODO: Remember last session.
            top_line: 0,
            top_wraps: 0,
            x_shift: 0,
            area,
            options,
            cursors: Vec::new(),
            main_cursor: 0,
        };

        file.cursors
            .push(FileCursor::new(FilePos { col: 0, line: 0 }, &file));

        file
    }

    /// Returns an `Some<&StyledChar>` if it exists, and `None` if it doesn't.
    pub fn get_char(&self, pos: FilePos) -> Option<&StyledChar> {
        match self.lines.get(pos.line) {
            Some(line) => line.text.get(pos.col),
            None => None,
        }
    }

    /// Parses the wrapping for all the lines in the file.
    ///
    /// * This should only be called when the wrap_type or width change.
    pub fn parse_wrapping(&mut self) {
        match self.options.wrap_method {
            WrapMethod::Width => {
                for line in self.lines.iter_mut() {
                    line.parse_wrapping(self.area.width(), &self.options);
                }
            }
            WrapMethod::NoWrap => {
                for line in self.lines.iter_mut() {
                    line.wrap_cols.clear();
                }
            }
            _ => {}
        }
    }

    /// Inserts text at a given position in the file.
    ///
    /// - If it returns true, the whole screen should be updated.
    /// - It also returns the updated file position, given the insertion.
    pub fn insert_text(&mut self, pos: FilePos, insertion: &Insertion) -> (FilePos, bool) {
        let line = &mut self.lines.get_mut(pos.line).unwrap();
        let first_insert = insertion.lines().get(0).unwrap();

        if insertion.has_new_line() || insertion.lines().len() > 1 {
            let cutoff_text = line.text.drain(pos.col..).map(|c| c.ch.content().clone())
                                                        .collect::<String>();
            
            if insertion.has_new_line() {
                self.lines.insert(pos.line + 1, TextLine::new(first_insert, &self.options.tabs));
                let line = &mut self.lines.get_mut(pos.line + 1).unwrap();

                line.calculate_indentation(&self.options.tabs);
            } else {
                line.splice_text(first_insert, pos.col..);
                line.calculate_indentation(&self.options.tabs);
            }

            for (index, line) in insertion.lines().iter().enumerate().skip(1) {
                self.lines.insert(pos.line + index + 1, TextLine::new(line, &self.options.tabs));
                let line = &mut self.lines.get_mut(pos.line + index).unwrap();

                if index == insertion.lines().len() - 1 {
                    line.splice_text(&cutoff_text, line.text.len()..);
                }

                line.calculate_indentation(&self.options.tabs);
            }

            (pos, true)

        // If the first check returns false, that means that there is just one line, that must be
        // inserted at `pos`, without creating any new lines.
        } else {

            line.splice_text(first_insert, pos.col..pos.col);

            let old_wrapping = line.wrap_cols.len();
            line.calculate_indentation(&self.options.tabs);
            if let WrapMethod::Width = self.options.wrap_method {
                line.parse_wrapping(self.area.width(), &self.options);
            }
            let new_pos = FilePos { line: pos.line, col: pos.col + first_insert.len() };

            // If the wrapping changes, the whole screen needs to be redrawn.
            (new_pos, line.wrap_cols.len() != old_wrapping)
        }
    }

    pub fn splice_text(
        &mut self, start: FilePos, end: FilePos,
        insertion: Option<&Insertion>) -> (FilePos, bool) {
        assert!(end >= start);
        let start_line = self.lines.get_mut(start.line).unwrap();

        match insertion {
            Some(_) => {(start, true)},
            None => {
                if start.line == end.line {
                    start_line.splice_text("", start.col..end.col);
                } else {
                    start_line.text.drain(start.col..);

                    let end_line = self.lines.get(end.line).unwrap();
                    let ending_text = end_line.text.get(end.col..).unwrap().to_vec();

                    self.lines.drain((start.line + 1)..=end.line);

                    let start_line = self.lines.get_mut(start.line).unwrap();
                    start_line.text.extend(ending_text);
                }
                let start_line = self.lines.get_mut(start.line).unwrap();
                start_line.calculate_indentation(&self.options.tabs);
                if let WrapMethod::Width = self.options.wrap_method {
                    start_line.parse_wrapping(self.area.width(), &self.options);
                }
                (start, true)
            }
        }
    }

    /// Updates the file's scrolling and checks if it has scrolled.
    pub fn update_scroll(&mut self) -> bool {
        let scrolloff = self.options.scrolloff;

        let mut pos = self
            .cursors
            .get(self.main_cursor)
            .expect("cursor not found")
            .pos;

        // Scroll if the cursor surpasses the soft cap of the cursor_zone.
        let mut has_scrolled = false;

        // Vertical check:
        while pos.y > (self.area.height() - scrolloff.y) as i32 {
            if self.lines.get(self.top_line).unwrap().wrap_cols.len() > self.top_wraps {
                self.top_wraps += 1;
            } else {
                self.top_line += 1;
                self.top_wraps = 0;
            }
            has_scrolled = true;
            pos.y -= 1;
            self.cursors.iter_mut().for_each(|c| c.pos.y -= 1);
        }

        // Stop scrolling when the line at the top of the screen is already the first line.
        while pos.y < scrolloff.y as i32 && !(self.top_line == 0 && self.top_wraps == 0) {
            if self.top_wraps > 0 {
                self.top_wraps -= 1;
            } else {
                self.top_line -= 1;
                let top_line = self.lines.get(self.top_line).expect("invalid line");
                self.top_wraps = top_line.wrap_cols.len();
            }
            // Moves the cursors down by one, to keep them in the same place in the
            // file.
            has_scrolled = true;
            pos.y += 1;
            self.cursors.iter_mut().for_each(|c| c.pos.y += 1);
        }

        let current = self
            .cursors
            .get(self.main_cursor)
            .expect("cursor not found")
            .current();
        let line = self.lines.get(current.line).unwrap();

        // Horizontal check, done only when the screen can scroll horizontally:
        if let WrapMethod::NoWrap = self.options.wrap_method {
            let distance = line.get_distance_to_col(current.col, &self.options.tabs);

            // If the distance is greater, it means that the cursor is out of bounds.
            if distance >= self.x_shift + self.area.width() - scrolloff.x {
                // Shift by the amount required to keep the cursor in bounds.
                self.x_shift = distance + scrolloff.x + 1 - self.area.width();
                has_scrolled = true;
            // Check if `self.x_shift` is already at 0, if it is, no scrolling is dones.
            } else if distance <= self.x_shift + scrolloff.x && self.x_shift != 0 {
                self.x_shift = distance.saturating_sub(scrolloff.x);
                has_scrolled = true;
            }
            // If the screen moves `x` forward, cursors must move `x` backward to keep them steady.
            self.cursors
                .iter_mut()
                .for_each(|c| c.pos.x -= self.x_shift as i32);
        }

        has_scrolled
    }

    /// Prints the file, according to its current position.
    pub fn print_file(&mut self, force: bool) {
        // Saving the current positions to print the lines where cursors have been.
        let saved: Vec<(usize, CursorPos, u16)> = self
            .cursors.iter().map(|c| (c.current().line, c.pos, c.wraps())).collect();

        // Updates the information for each cursor in the file.
        self.cursors.iter_mut().for_each(|c| c.update(&self.lines, &self.options));

        // Checks if said update has caused the file to scroll.
        let has_scrolled = self.update_scroll();

        // The line at the top of the screen and the amount of hidden columns.
        let skip = if self.top_wraps > 0 {
            let line = self.lines.get(self.top_line).expect("invalid line");
            *line.wrap_cols().get(self.top_wraps - 1).unwrap() + 1
        } else {
            0
        };

        // If the file has scrolled, reprint the whole screen.
        if has_scrolled || force {
            let area = &mut self.area;

            let mut line_origin = OutputPos { x: 0, y: 0 };

            // Prints the first line and updates where to print next.
            let line = self.lines.get(self.top_line).expect("invalid line");
            line_origin.y += line.print(area, line_origin, skip, &self.options, self.x_shift);

            // Prints the remaining lines
            for line in self.lines.iter().skip(self.top_line + 1) {
                if line_origin.y > area.height() {
                    break;
                }
                line_origin.y += line.print(area, line_origin, 0, &self.options, self.x_shift);
            }

            // Clears the lines where nothing has been printed.
            for _ in line_origin.y..=self.area.height() {
                self.area.move_cursor(line_origin);
                self.area.print(" ".repeat(self.area.width() as usize));
                line_origin.y += 1;
            }
        // If it hasn't, only reprint the lines where cursors have been in.
        } else {
            let mut printed_lines = Vec::new();

            for (current, prev) in self.cursors.iter().zip(saved) {
                let line = self.lines.get(prev.0).expect("invalid line");
                let mut line_origin: OutputPos = prev.1.into();

                line_origin.x = 0;
                // Printing will never happen at a negative `y`.
                line_origin.y = max(0, line_origin.y - prev.2);

                if !printed_lines.contains(&current.current().line) {
                    line_origin.y += if prev.0 == self.top_line {
                        line.print(
                            &mut self.area,
                            line_origin,
                            skip,
                            &self.options,
                            self.x_shift,
                        )
                    } else if line_origin.y < self.area.height() {
                        line.print(&mut self.area, line_origin, 0, &self.options, self.x_shift)
                    } else {
                        0
                    };

                    printed_lines.push(prev.0);
                }
            }
        }
    }

    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn top_line(&self) -> usize {
        self.top_line
    }

    pub fn top_wraps(&self) -> usize {
        self.top_wraps
    }

    pub fn x_shift(&self) -> u16 {
        self.x_shift
    }
}
