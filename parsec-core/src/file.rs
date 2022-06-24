use std::cmp::{min, max};

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use crate::{
    output::{OutputArea, OutputPos, StyledChar},
    config::{FileOptions, WrapMethod, TabPlaces}
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
    pub fn new(text: &str, tabs: &TabPlaces) -> TextLine  {
        let mut file_line = TextLine {
            wrap_cols: Vec::new(),
            text: Vec::new(),
            indent: 0,
            width: 0
        };
        
        let mut indenting = true;

        for grapheme in text.graphemes(true) {
            // TODO: Add variable tab size.
            // NOTE: This probably has some obscure bugs embeded in it.
            if indenting && grapheme == " " {
                file_line.indent += 1;
            } else if indenting && grapheme == "\t" {
                file_line.indent += tabs.get_tab_len(file_line.indent as u16) as usize;
            } else { indenting = false; }

            let width = UnicodeWidthStr::width(grapheme);

            file_line.text.push(StyledChar::new(grapheme, width));
        }

        file_line.width += file_line.text.len();

        file_line
    }

    /// Returns the position of the wrapping columns.
    pub fn wrap_cols(&self) -> &Vec<usize> {
        &self.wrap_cols
    }

    /// Returns the contents of the line.
    pub fn text(&self) -> &Vec<StyledChar> {
        &self.text
    }

    /// Returns the width of the line.
    pub fn width(&self) -> usize {
        self.width
    }

    /// Returns the indentation of the line.
    pub fn indent(&self) -> usize {
        self.indent
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
                if options.wrap_indent { wrap_indentation = self.indent as u16; }
            }
        }
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
    /// The leftover number is positive if the width of the characters is greater (happens if the last
    /// character has a width greater than 1), and it is negative if the distance is greater (happens if
    /// there aren't enough characters to cover the distance.
    pub fn get_col_at_distance(&self, dist: u16, tabs: &TabPlaces) -> (usize, i32) {
        let (mut index, mut width) = (0, 0);
        let mut text_iter = self.text.iter();
        while width < dist {
            match text_iter.next() {
                Some(ch) => {
                    width += ch.width(width, tabs);
                    index += 1;
                },
                None => break
            }
        }
        (index, width as i32 - dist as i32 )
    }

    // TODO: Make this a generic free function, and make areas do bounds checking.
    /// Prints a line in a given position, skipping `skip` characters.
    ///
    /// Returns the amount of lines that were printed.
    #[inline]
    fn print<T: OutputArea>(
        &self, area: &mut T, mut pos: OutputPos, skip: usize,
        options: &FileOptions, x_shift: u16) -> u16 {
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
                if col + ch.width(col, &options.tabs) > area.width() { break; }
            }

            if ch.text.content() == "\t" {
                let tab_len = options.tabs.get_tab_len(col);
                area.print(" ".repeat(tab_len as usize));
                col += tab_len; 
            } else {
                area.print(ch.text.clone());
                col += ch.width(col, &options.tabs);
            }
            
            if ch.is_wrapping {
                printed_lines +=1;
                pos.y += 1;
                if pos.y > area.height() { break; }
                area.move_cursor(pos);
                if options.wrap_indent && pos.x == 0 { area.print(" ".repeat(self.indent)); }
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
    /// Returns a reference to `top_line`
    pub fn top_line(&self) -> &usize {
        &self.top_line
    }

    /// Returns a reference to `top_line`
    pub fn top_wraps(&self) -> &usize {
        &self.top_wraps
    }

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

        file.cursors.push(FileCursor::new(FilePos {col: 0, line: 0}, &file));

        file
    }

    /// Returns an `Some<&StyledChar>` if it exists, and `None` if it doesn't.
    pub fn get_char(&self, pos: FilePos) -> Option<&StyledChar> {
        match self.lines.get(pos.line) {
            Some(line) => line.text.get(pos.col),
            None => None,
        }
    }

    /// Scrolls the file up by one line
    ///
    /// * If it returns false, it means it is not possible to scroll up.
    fn scroll_up(&mut self) -> bool {
        if self.top_line == 0 && self.top_wraps == 0 {
            false
        } else {
            if self.top_wraps > 0 {
                self.top_wraps -= 1;
            } else {
                self.top_line -= 1;
                let top_line = self.lines.get(self.top_line).expect("invalid line");
                self.top_wraps = top_line.wrap_cols.len();
            }
            // Moves the cursors down by one, to keep them in the same place in the
            // file.
            self.cursors.iter_mut().for_each(|c| c.pos.y += 1);
            true
        }
    }

    /// Scrolls the file down by one line
    fn scroll_down(&mut self) -> bool {
        let top_line = self.lines.get(self.top_line).expect("invalid line");

        if top_line.wrap_cols.len() > self.top_wraps {
            self.top_wraps += 1;
        } else {
            self.top_line += 1;
            self.top_wraps = 0;
        }

        // Moves the cursors up by one, to keep them in the same place in the file.
        self.cursors.iter_mut().for_each(|c| c.pos.y -= 1);

        true
    }

    fn scroll_left(&mut self) -> bool {
        if self.x_shift > 0 {
            self.x_shift -= 1;
            self.cursors.iter_mut().for_each(|c| c.pos.x += 1);
            
            true
        } else {
            false
        }
    }

    fn scroll_right(&mut self) -> bool {
        self.x_shift += 1;
        self.cursors.iter_mut().for_each(|c| c.pos.x -= 1);

        true
    }

    /// Parses the wrapping for all the lines in the file
    ///
    /// * This should only be called when the wrap_type or width change.
    pub fn parse_wrapping(&mut self) {
        match self.options.wrap_method {
            WrapMethod::Width => {
                for line in self.lines.iter_mut() {
                    line.parse_wrapping(self.area.width(), &self.options);
                }
            },
            WrapMethod::NoWrap => {
                for line in self.lines.iter_mut() {
                    line.wrap_cols.clear();
                }
            },
            _ => {},
        }
    }

    /// Updates the file and checks if it has scrolled.
    pub fn has_scrolled(&mut self) -> bool {
        let scrolloff = self.options.scrolloff;

        let mut pos = self.cursors.get(self.main_cursor).expect("cursor not found").pos;

        // Scroll if the cursor surpasses the soft cap of the cursor_zone.
        let mut has_scrolled = false;

        // Vertical check:
        while pos.y > (self.area.height() - scrolloff.y) as i32 {
            has_scrolled |= self.scroll_down();
            pos.y -= 1;
        }
        while pos.y < scrolloff.y as i32 {
            has_scrolled |= self.scroll_up();
            pos.y += 1;
        }

        let current = self.cursors.get(self.main_cursor).expect("cursor not found").current();
        let line = self.lines.get(current.line).unwrap();

        // Horizontal check, done only when the screen can scroll horizontally:
        if let WrapMethod::NoWrap = self.options.wrap_method {
            let distance = line.get_distance_to_col(current.col, &self.options.tabs);
            if distance >= self.x_shift + self.area.width() - scrolloff.x {
                self.x_shift = distance + scrolloff.x + 1 - self.area.width();
                has_scrolled = true;
            } else if distance <= self.x_shift + scrolloff.x && self.x_shift != 0 {
                self.x_shift = max(distance as i32 - scrolloff.x as i32 , 0) as u16;
                has_scrolled = true;
            }
            self.cursors.iter_mut().for_each(|c| c.pos.x -= self.x_shift as i32);
        }

        has_scrolled
    }

    /// Prints the file, according to its current position.
    pub fn print_file(&mut self, force: bool) {

        // Saving the current positions to print the lines where cursors have been.
        let saved: Vec<(usize, CursorPos, u16)> = self.cursors.iter()
            .map(|c| (c.current().line, c.pos, c.wraps())).collect();

        // Updates the information for each cursor in the file.
        self.cursors.iter_mut().for_each(|c| c.update(&self.lines, &self.options));

        // Checks if said update has caused the file to scroll.
        let has_scrolled = self.has_scrolled();

        // The line at the top of the screen and the amount of hidden columns.
        let skip = if self.top_wraps > 0 {
            let line = self.lines.get(self.top_line).expect("invalid line");
            *line.wrap_cols().get(self.top_wraps - 1).unwrap() + 1
        } else {
            0
        };

        if has_scrolled || force {
            let area = &mut self.area;

            let mut line_origin = OutputPos { x: 0, y: 0 };

            // Prints the first line and updates where to print next.
            let line = self.lines.get(self.top_line).expect("invalid line");
            line_origin.y += line.print(area, line_origin, skip, &self.options, self.x_shift);

            // Prints the remaining lines
            for line in self.lines.iter().skip(self.top_line + 1) {
                if line_origin.y > area.height() { break; }
                line_origin.y += line.print(area, line_origin, 0, &self.options, self.x_shift);
            }

            // Clears until the end of the screen.
            for _ in line_origin.y..=self.area.height() {
                self.area.move_cursor(line_origin);
                self.area.print(" ".repeat(self.area.width() as usize));
                line_origin.y += 1;
            }
        } else {
            let area = &mut self.area;

            let mut printed_lines = Vec::new();

            for (current, prev) in self.cursors.iter().zip(saved) {
                let line = self.lines.get(prev.0).expect("invalid line");
                let mut line_origin: OutputPos = prev.1.into();

                line_origin.x = 0;
                // Printing will never happen at a negative `y`.
                line_origin.y = max(0, line_origin.y - prev.2);

                if !printed_lines.contains(&current.current().line) {
                    line_origin.y += if prev.0 == self.top_line {
                        line.print(area, line_origin, skip, &self.options, self.x_shift)
                    } else if line_origin.y < area.height() {
                        line.print(area, line_origin, 0, &self.options, self.x_shift)
                    } else {
                        0
                    };

                    printed_lines.push(prev.0);
                }
            }
        }
    }
}


