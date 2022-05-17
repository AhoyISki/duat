use crossterm::{
    self,
    cursor::MoveTo,
    style::{
        Print,
        ContentStyle,
        //Attribute,
        //Color,
        PrintStyledContent,
        StyledContent
    },
    terminal,
    QueueableCommand,
};

use std::path::PathBuf;
use std::fs;
use std::io;
use std::cmp::{min, max};

use unicode_segmentation::{UnicodeSegmentation, /*GraphemeCursor*/};

use unicode_width::UnicodeWidthStr;

// Any use of the terms col and line refers specifically to a position in the file,
// x and y are reserved for positions on the screen.

#[derive(Copy, Clone)]
pub struct TerminalArea {
    origin: TermPos,
    end: TermPos,
}

impl TerminalArea {
    fn dims(&self) -> (u16, u16) {
        (self.end.x - self.origin.x, self.end.y - self.origin.y)
    }
}

// TODO: move this to a more general file.
// TODO: In the future, this shouldn't be public, probably.
#[derive(Copy, Clone)]
pub struct TermPos {
    pub x: u16,
    pub y: u16,
}

#[derive(Copy, Clone)]
pub struct FilePos {
    pub col: usize,
    pub line: usize,
}

struct Cursor {
    x: u16,
    y: u16,
    current: FilePos,
    target: FilePos,
    desired_col: usize,
    wraps: u16
    // TODO: Eventually add a selection to the cursor.
}

// TODO: move this to a more general file.
struct StyledGrapheme {
    col: StyledContent<String>,
    width: usize,
}

struct FileLine {
    wrap_places: Vec<u16>,
    text: Vec<StyledGrapheme>,
    indentation: u8,

    // Since the width function doesn't take into account the width of cols.
    width: usize,
}

impl FileLine {
    fn set_wrap_places(&mut self, width: u16) {
        self.wrap_places.clear();
        let mut index = width - 1;

        // TODO: Add an enum parameter signifying the wrapping type.
        // Wrapping at the final character at the width of the area.
        for _ in 0..(self.width / width as usize) {
            self.wrap_places.push(index);
            index += width;
        }
    }
}

pub struct FileBuffer {
    content: Vec<FileLine>,

    // Position is origin inclusive, and end exclusive.
    area: Option<TerminalArea>,

    // Info about which line is on the top of the area.
    top_line: usize,
    top_line_wraps: u32,

    // It is a vector because modules might make use of multiple cursors.
    cursors: Vec<Cursor>,

    // TODO: Move options to a centralized option place.
    // TODO: Turn this into a more versatile enum.
    do_wrap_lines: bool,
}

impl FileBuffer {
    pub fn from(file_path: PathBuf) -> io::Result<FileBuffer> {
        // TODO: In the future, this will not panic!
        let file_contents = fs::read_to_string(file_path).expect("file not found");

        let mut file_buffer = FileBuffer {
            content: Vec::new(),
            area: None,
            top_line: 0,
            top_line_wraps: 0,

            // TODO: Move this to a function that creates new cursors.
            cursors: vec![ Cursor {
                x: 0,
                y: 0,
                current: FilePos { col: 0, line: 0 },
                target: FilePos { col: 0, line: 0 },
                desired_col: 0,
                wraps: 0,
            } ],

            do_wrap_lines: true,
        };

        for line_text in file_contents.lines() {
            let mut line = FileLine {
                wrap_places: Vec::new(),
                text: Vec::new(),
                indentation: 0,
                // So characters can be appended to the end of lines.
                width: 0
            };

            let mut parsing_indentation = true;

            for grapheme in line_text.graphemes(true) {
                // TODO: Add an algorithm for indentation computation.
                // TODO: Add variable tab size.
                // NOTE: This probably has some obscure bugs embeded in it.
                if parsing_indentation && grapheme == " " || grapheme == "\t" {
                       line.indentation += UnicodeWidthStr::width(grapheme) as u8;
                } else { parsing_indentation = false; }

                line.text.push(StyledGrapheme {
                    col: StyledContent::new(
                        ContentStyle::new(), String::from(grapheme)),
                    style_is_final: false,
                    width: UnicodeWidthStr::width(grapheme),
                });
            }

            line.width += line.text.len();

            file_buffer.content.push(line);
        }

        Ok(file_buffer)
    }

    fn term_update_cursor(&mut self, cursor_num: usize) {
        let (width, _height) = self.area.expect("area not assigned").dims();

        let mut cursor = self.cursors.get_mut(cursor_num).expect("invalid cursor");
        let (col, line) = (cursor.target.col as i32, cursor.target.line as i32);

        let text_line = self.content.get(line as usize).expect("invalid line");
        let line_wraps = text_line.wrap_places.len() as i32;

        // Sum of "wrapping leftovers" from wrapped lines before col.
        let offset_sum = text_line.wrap_places.iter().filter(|&c| *c < col as u16)
                                  .map(|&c| (width as i32 - (c % width + 1) as i32))
                                  .sum::<i32>();


        // TODO: Calculate in relation to (width - indentation) with the possibility
        // of that being only on the first line, for wrapped lines that start at 0
        let mut d_y = if cursor.target.line > cursor.current.line {
            (offset_sum + col) / width as i32
        } else if cursor.target.line < cursor.current.line {
            -(offset_sum + col) / width as i32
        } else {
            // The cursor.wraps is only subtracted when the line doesn't change,
            // since the current wrapping position could be greater than 0, which
            // would mean a reduced or reversed cursor movement on the y axis.
            (offset_sum + col) / width as i32 - cursor.wraps as i32 
        };

        // The range shouldn't include the wrapped lines of the last line.
        let line_range = if (line as usize) > cursor.current.line {
            d_y -= cursor.wraps as i32;
            cursor.current.line..(line as usize)
        } else if (line as usize) < cursor.current.line {
            d_y += cursor.wraps as i32;
            (line as usize)..cursor.current.line
        } else { 0..0 };

        for line in self.content.get(line_range).expect("reading line failed") {
            d_y += 1 + line.wrap_places.len() as i32;
        }

        // If the line updated before cursor.y, the first check would always be true.
        cursor.y = if line as usize >= cursor.current.line {
            (cursor.y as i32 + d_y) as u16
        } else {
            (cursor.y as i32 - d_y) as u16
        };
        cursor.current.line = line as usize;

        // cursor.x needs the updated values of the number of wraps and column.
        cursor.current.col = col as usize;
        cursor.wraps = ((offset_sum + col) / width as i32) as u16;
        cursor.x = (col as i32 + offset_sum - (width * cursor.wraps) as i32) as u16;
    }

    /// Moves the cursor right on the file
    ///
    /// - If `update == true`, the function calls `self.term_update_cursor()`. This
    /// is useful for "chaining" multiple movements at once, so you can update only
    /// the last movement.
    /// 
    pub fn move_cursor_right(&mut self, cursor_num: usize, update: bool) {
        let cursor = self.cursors.get_mut(cursor_num).expect("cursor failed");
        let line = self.content.get(cursor.current.line).expect("cursor failed");

        // TODO: Maybe add an option to change this 0 into the indentation.
        // TODO: Add an option to finish movement at the end of a line.
        if cursor.target.col == line.width {
            if let Some(_) = self.content.get(cursor.current.line + 1) {
                cursor.target.col = 0;
                cursor.target.line += 1;
            }
        } else {
            cursor.target.col += 1;
            cursor.desired_col = cursor.target.col;
        }

        if update { self.term_update_cursor(cursor_num); }
    }

    /// Moves the cursor left on the file
    ///
    /// - If `update == true`, the function calls `self.term_update_cursor()`. This
    /// is useful for "chaining" multiple movements at once, so you can update only
    /// the last movement.
    /// 
    pub fn move_cursor_left(&mut self, cursor_num: usize, update: bool) {
        let cursor = self.cursors.get_mut(cursor_num).expect("invalid cursor");

        if cursor.target.col == 0 {
            if cursor.current.line != 0 {
                if let Some(line) = self.content.get(cursor.current.line - 1) {
                    cursor.target.col = line.width;
                    cursor.desired_col = cursor.target.col;
                    cursor.target.line -= 1;
                }
            }
        } else {
            cursor.target.col -= 1;
            cursor.desired_col = cursor.target.col;
        }

        if update { self.term_update_cursor(cursor_num); }
    }

    /// Moves the cursor down on the file
    ///
    /// - If `update == true`, the function calls `self.term_update_cursor()`. This
    /// is useful for "chaining" multiple movements at once, so you can update only
    /// the last movement.
    /// 
    pub fn move_cursor_down(&mut self, cursor_num: usize, update: bool) {
        let cursor = self.cursors.get_mut(cursor_num).expect("invalid cursor");

        if let Some(line) = self.content.get(cursor.target.line + 1) {
            cursor.target.col = min(cursor.desired_col, line.width);
            cursor.target.line += 1;
        }

        if update { self.term_update_cursor(cursor_num); }
    }

    /// Moves the cursor up on the file
    ///
    /// - If `update == true`, the function calls `self.term_update_cursor()`. This
    /// is useful for "chaining" multiple movements at once, so you can update only
    /// the last movement.
    /// 
    pub fn move_cursor_up(&mut self, cursor_num: usize, update: bool) {
        let cursor = self.cursors.get_mut(cursor_num).expect("cursor failed");

        if cursor.target.line != 0 {
            if let Some(line) = self.content.get(cursor.target.line - 1) {
                cursor.target.col = min(cursor.desired_col, line.width);
                cursor.target.line -= 1;
            }
        }

        if update { self.term_update_cursor(cursor_num); }
    }

    pub fn parse_wrapping(&mut self) {
        if !self.do_wrap_lines {
            for line in self.content.iter_mut() {
                line.wrap_places.clear();
            }
        } else {
            let (width, _) = self.area.expect("area not assigned").dims();

            for line in self.content.iter_mut() {
                line.set_wrap_places(width);
            }
        }
    }
}

// TODO: move this to a more general file
pub trait PrintableArea<'a> {
    fn print_contents(&self, stdout: &mut io::Stdout) -> crossterm::Result<()>;

    fn allocate_area(&self, origin: TermPos, end: TermPos) -> TerminalArea {
        let (width, height) = terminal::size().expect("terminal size failed");
        let origin = TermPos { x: max(origin.x, 0), y: max(origin.y, 0) };
        let end = TermPos { x: min(end.x, width), y: min(end.y, height) };
        TerminalArea { origin, end }
    }

    fn request_area(&mut self, origin: TermPos, end: TermPos); }

impl PrintableArea<'_> for FileBuffer {
    /* TODO: Finish this function */
    fn print_contents(&self, stdout: &mut io::Stdout) -> crossterm::Result<()> {
        let area = self.area.expect("area not assigned");
        let (width, height) = area.dims();
        let width = width as usize;

        let mut h_offset: u16 = 0;

        for line in self.content.iter().skip(self.top_line) {
            if h_offset == height { break; };

            stdout.queue(MoveTo(area.origin.x, area.origin.y + h_offset))?;

            for (col, styled_char) in line.text.iter().enumerate() {
                if !self.do_wrap_lines && col + styled_char.width > width { break; }

                stdout.queue(PrintStyledContent(styled_char.col.clone()))?;

                if line.wrap_places.contains(&(col as u16)) {
                    h_offset += 1;
                    stdout.queue(MoveTo(
                        area.origin.x,
                        area.origin.y + h_offset))?;
                } 
            }
            h_offset += 1;
        }
        let cursor = self.cursors.get(0).expect("cursor doesn't exist");
        let status = format!(
            "Term: {}x{}, File: {}x{}, width: {}, wraps: {}                      ",
            cursor.x, cursor.y, cursor.current.col, cursor.current.line, width,
            cursor.wraps);
                
        stdout.queue(MoveTo(0, 50))?
              .queue(Print(status))?;
        stdout.queue(MoveTo(area.origin.x + cursor.x, area.origin.y + cursor.y))?;

        Ok(())
    }

    fn request_area(&mut self, origin: TermPos, end: TermPos) {
        let prev_area = self.area;
        self.area = Some(self.allocate_area(origin, end));

        match prev_area {
            Some(area) => {
                if area.dims() != self.area.expect("area not set").dims() {
                    self.parse_wrapping();
                }
            },
            None => self.parse_wrapping(),
        };
    }
}
