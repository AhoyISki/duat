use std::cmp::max;

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use crate::{
    output::{OutputArea, OutputPos, StyledChar},
    config::{FileOptions, WrapType}
};

use crate::cursor::{CursorPos, FileCursor, FilePos};

// TODO: Make this a generic free function, and make areas do bounds checking.
/// Prints a line in a given position, skipping `skip` characters.
///
/// Returns the amount of lines that were printed.
#[inline]
fn print_line<T: OutputArea>(
    line: &TextLine, area: &mut T, mut pos: OutputPos,
    skip: usize, wrap: WrapType) -> u16 {
    // Moves the printing cursor to the beginning of the line.
    area.move_cursor(pos);

    let mut printed_lines = 1;

    let mut col = 0;

    for ch in line.text().iter().skip(skip) {
        if let WrapType::NoWrap = wrap {
            if col + ch.width > area.width() as usize { break; }
        }
        
        area.print_and_style(ch.clone());
        col += 1;

        if ch.is_wrapping {
            printed_lines += 1;
            pos.y += 1;
            if pos.y > area.height() { break; }
            area.move_cursor(pos);
        }
    }

    // Erasing anything that is leftover
    let width = area.width() as usize;
    let erase_count = width - col % width;
    area.print(" ".repeat(erase_count));

    printed_lines
}

// TODO: move this to a more general file.
/// A line in the text file.
pub struct TextLine {
    /// Which columns on the line should wrap around.
    wrap_cols: Vec<usize>,
    /// The text on the line.
    text: Vec<StyledChar>,
    /// The line's indentation.
    indent: u8,

    // Since `text.len()` wouldn't take into account the visual width of cols.
    /// The visual lenght of the line, taking into account double width chars.
    width: usize,
}

impl TextLine {
    /// Returns a new instance of `FileLine`
    pub fn new(text: &str) -> TextLine  {
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
            if indenting && grapheme == " " || grapheme == "\t" {
                   file_line.indent += UnicodeWidthStr::width(grapheme) as u8;
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

    /// Parses the wrapping of a single line.
    fn parse_wrapping(&mut self, width: u16) {
        self.wrap_cols.clear();
        let mut index = width as usize - 1;

        // TODO: Add an enum parameter signifying the wrapping type.
        // Wrapping at the final character at the width of the area.
        for _ in 0..(self.width / width as usize) {
            self.wrap_cols.push(index);
            self.text.get_mut(index).expect("no char").is_wrapping = true;
            index += width as usize;
        }
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

    /// The area allocated to the file.
    pub area: T,

    /// The options related to files.
    options: FileOptions,

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
    fn scroll_down(&mut self) {
        let top_line = self.lines.get(self.top_line).expect("invalid line");

        if top_line.wrap_cols.len() > self.top_wraps {
            self.top_wraps += 1;
        } else {
            self.top_line += 1;
            self.top_wraps = 0;
        }

        // Moves the cursors up by one, to keep them in the same place in the file.
        self.cursors.iter_mut().for_each(|c| c.pos.y -= 1);
    }

    /// Parses the wrapping for all the lines in the file
    ///
    /// * This should only be called when the wrap_type or width change.
    pub fn parse_wrapping(&mut self) {
        match self.options.wrap_type {
            WrapType::Width => {
                for line in self.lines.iter_mut() {
                    line.parse_wrapping(self.area.width());
                }
            },
            WrapType::NoWrap => {
                for line in self.lines.iter_mut() {
                    line.wrap_cols.clear();
                }
            },
            _ => {},
        }
    }

    /// Checks if the file has scrolled.
    pub fn has_scrolled(&mut self) -> bool {
        let scrolloff = self.options.scrolloff;

        // Updates the information for each cursor in the file.
        self.cursors.iter_mut().for_each(|c| c.update(&self.lines, &self.area));

        let cursor = self.cursors.get(self.main_cursor).expect("cursor not found");

        // Scroll if the cursor surpasses the soft cap of the cursor_zone.
        if cursor.pos.y > (self.area.height() - scrolloff) as i32 {
            for _ in (self.area.height() - scrolloff) as i32..cursor.pos.y {
                self.scroll_down();
            }
            true
        } else if cursor.pos.y < scrolloff as i32 {
            for _ in cursor.pos.y ..scrolloff as i32 {
                if !self.scroll_up() { return false }
            }
            true
        } else {
            false
        }
    }

    /// Prints the file, according to its current position.
    pub fn print_file(&mut self, force: bool) {

        // Saving the current positions to print the lines where cursors have been.
        let wrap_type = self.options.wrap_type;
        let saved: Vec<(usize, CursorPos, u16)> = self.cursors.iter()
            .map(|c| (c.current().line, c.pos, c.wraps())).collect();

        // Updating the cursors on the file.
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
            line_origin.y += print_line::<T>(line, area, line_origin, skip, wrap_type);

            // Prints the remaining lines
            for line in self.lines.iter().skip(self.top_line + 1) {
                if line_origin.y > area.height() { break; }
                line_origin.y += print_line::<T>(line, area, line_origin, 0, wrap_type);
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
                        print_line::<T>(line, area, line_origin, skip, wrap_type)
                    } else if line_origin.y < area.height() {
                        print_line::<T>(line, area, line_origin, 0, wrap_type)
                    } else {
                        0
                    };

                    printed_lines.push(prev.0);
                }
            }
        }
    }
}


