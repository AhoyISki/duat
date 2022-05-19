#[cfg(feature = "parsec-term")]
pub use parsec_term::{ TermArea, TermPos };

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
// TODO: move this to a more general file.
// TODO: In the future, this shouldn't be public, probably.
#[derive(Copy, Clone)]
pub struct FilePos {
    pub col: usize,
    pub line: usize,
}

#[derive(Copy, Clone)]
pub struct CursorPos {
    pub x: i32,
    pub y: i32,
}

struct Cursor {
    pos: CursorPos,
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

    // This information is in two places to reduce the number of operations done
    // when moving the cursor.
    is_wrapping: bool,
}

struct FileLine {
    wrap_cols: Vec<u16>,
    text: Vec<StyledGrapheme>,
    indentation: u8,

    // Since the width function doesn't take into account the width of cols.
    width: usize,
}

impl FileLine {
    fn parse_wrapping(&mut self, width: u16) {
        self.wrap_cols.clear();
        let mut index = width - 1;

        // TODO: Add an enum parameter signifying the wrapping type.
        // Wrapping at the final character at the width of the area.
        for _ in 0..(self.width / width as usize) {
            self.wrap_cols.push(index);
            self.text.get_mut(index as usize).expect("no char").is_wrapping = true;
            index += width;
        }
    }
}

enum WrapType {
    Width,
    Capped(u16),
    Word,
    NoWrap,
}

pub struct FileBuffer {
    content: Vec<FileLine>,

    // Position is origin inclusive, and end exclusive.
    area: Option<TermArea>,

    // Info about which line is on the top of the area.
    top_line: usize,
    top_wraps: usize,

    // It is a vector because modules might make use of multiple cursors.
    cursors: Vec<Cursor>,

    ////////////////////////////////////////////////////////////////////////////
    // Options
    ////////////////////////////////////////////////////////////////////////////
    // TODO: Move options to a centralized option place.
    wrap_type: WrapType,
    cursor_height_buffer: u16,
    cursor_width_buffer: u16,
}

impl FileBuffer {
    pub fn from(file_path: PathBuf) -> io::Result<FileBuffer> {
        // TODO: In the future, this will not panic!
        let file_contents = fs::read_to_string(file_path).expect("file not found");

        let mut file_buffer = FileBuffer {
            content: Vec::new(),
            area: None,
            top_line: 0,
            top_wraps: 0,

            // TODO: Move this to a function that creates new cursors.
            cursors: vec![ Cursor {
                pos: CursorPos { x: 0, y: 0 },
                current: FilePos { col: 0, line: 0 },
                target: FilePos { col: 0, line: 0 },
                desired_col: 0,
                wraps: 0,
            } ],

            // NOTE: Temporary
            wrap_type: if cfg!(feature = "width") {
                WrapType::Width
            } else {
                WrapType::NoWrap
            },
            cursor_height_buffer: 0,
            cursor_width_buffer: 0,
        };

        for line_text in file_contents.lines() {
            let mut line = FileLine {
                wrap_cols: Vec::new(),
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
                    width: UnicodeWidthStr::width(grapheme),
                    is_wrapping: false
                });
            }

            line.width += line.text.len();

            file_buffer.content.push(line);
        }

        Ok(file_buffer)
    }

    fn term_update_cursor(&mut self, cursor_num: usize) {
        let (width, height) = self.area.expect("area not assigned").dims();

        let mut cursor = self.cursors.get_mut(cursor_num).expect("invalid cursor");
        let (col, line) = (cursor.target.col as i32, cursor.target.line as i32);

        let text_line = self.content.get(line as usize).expect("invalid line");
        let line_wraps = text_line.wrap_cols.len() as i32;

        // Sum of "wrapping leftovers" from wrapped lines before col.
        let offset = text_line.wrap_cols.iter().filter(|&c| *c < col as u16)
                                  .map(|&c| (width as i32 - (c % width + 1) as i32))
                                  .sum::<i32>();


        // TODO: Calculate in relation to (width - indentation) with the possibility
        // of that being only on the first line, for wrapped lines that start at 0
        let mut d_y = if cursor.target.line > cursor.current.line {
            (offset + col) / width as i32
        } else if cursor.target.line < cursor.current.line {
            -(offset + col) / width as i32
        } else {
            // The cursor.wraps is only subtracted when the line doesn't change,
            // since the current wrapping position could be greater than 0, which
            // would mean a reduced or reversed cursor movement on the y axis.
            (offset + col) / width as i32 - cursor.wraps as i32 
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
            d_y += 1 + line.wrap_cols.len() as i32;
        }

        // If the line updated before cursor.y, this would always default to false.
        // d_y needs to be negative if the cursor is moving up the file.
        if cursor.current.line > line as usize { d_y *= -1; };
        // This variable is used so that actual cursor positions are never negative.
        cursor.pos.y = cursor.pos.y as i32 + d_y;

        cursor.current.line = line as usize;

        // cursor.x needs the updated values of the number of wraps and column.
        cursor.current.col = col as usize;
        cursor.wraps = ((offset + col) / width as i32) as u16;
        cursor.pos.x = col as i32 + offset - (width * cursor.wraps) as i32;

        // Scroll if the cursor surpasses the soft cap of the cursor_zone.
        if cursor.pos.y  < self.cursor_height_buffer as i32 {
            for _ in cursor.pos.y ..self.cursor_height_buffer as i32 {
                if !self.scroll_up() { break; }
            }
        } else if cursor.pos.y > (height - self.cursor_height_buffer) as i32 {
            for _ in (height - self.cursor_height_buffer) as i32..cursor.pos.y {
                self.scroll_down();
            }
        }
    }

    /// Scrolls the file down by one line
    ///
    fn scroll_down(&mut self) {
        let top_line = self.content.get(self.top_line).expect("line not found");

        if top_line.wrap_cols.len() > self.top_wraps {
            self.top_wraps += 1;
        } else {
            self.top_line += 1;
            self.top_wraps = 0;
        }

        self.cursors.iter_mut().for_each(|cursor| cursor.pos.y -= 1);
    }

    /// Scrolls the file up by one line
    ///
    /// - If it returns false, it means it is not possible to scroll up.
    /// 
    fn scroll_up(&mut self) -> bool {
        if self.top_line == 0 && self.top_wraps == 0 {
            false
        } else {
            if self.top_wraps > 0 {
                self.top_wraps -= 1;
            } else {
                self.top_line -= 1;
                let top_line = self.content.get(self.top_line)
                                           .expect("line not found");
                self.top_wraps = top_line.wrap_cols.len();
            }
            self.cursors.iter_mut().for_each(|cursor| cursor.pos.y += 1);
            true
        }
    }

    /// Moves the cursor right on the file
    ///
    /// - If `update == true`, the current cursor position will update
    /// 
    pub fn move_cursor_right(&mut self, cursor_num: usize, update: bool) {
        let cursor = self.cursors.get_mut(cursor_num).expect("cursor failed");
        let line = self.content.get(cursor.target.line).expect("cursor failed");

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
    /// - If `update == true`, the current cursor position will update
    /// 
    pub fn move_cursor_left(&mut self, cursor_num: usize, update: bool) {
        let cursor = self.cursors.get_mut(cursor_num).expect("invalid cursor");

        if cursor.target.col == 0 && cursor.target.line != 0{
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
    /// - If `update == true`, the current cursor position will update
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
    /// - If `update == true`, the current cursor position will update
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

    fn parse_wrapping(&mut self) {
        match self.wrap_type {
            WrapType::Width => {
                let (width, _) = self.area.expect("area not assigned").dims();

                for line in self.content.iter_mut() {
                    line.parse_wrapping(width);
                }
            },
            WrapType::NoWrap => {
                for line in self.content.iter_mut() {
                    line.wrap_cols.clear();
                }
            },
            _ => {},
        }
    }
}

// TODO: move this to a more general file
pub trait PrintableArea<'a> {
    fn print_contents(&self, stdout: &mut io::Stdout) -> crossterm::Result<()>;

    fn allocate_area(&self, origin: TermPos, end: TermPos) -> TermArea {
        let (width, height) = terminal::size().expect("crossterm failed");
        let origin = TermPos { x: max(origin.x, 0), y: max(origin.y, 0) };
        let end = TermPos { x: min(end.x, width), y: min(end.y, height) };
        TermArea { origin, end }
    }

    fn request_area(&mut self, origin: TermPos, end: TermPos); }

impl PrintableArea<'_> for FileBuffer {
    /* TODO: Finish this function */
    fn print_contents(&self, stdout: &mut io::Stdout) -> crossterm::Result<()> {
        let area = self.area.expect("area not assigned");
        let (width, height) = area.dims();
        let width = width as usize;

        let mut h_offset: u16 = 0;
        let mut was_first_line_placed = false;

        for line in self.content.iter().skip(self.top_line) {
            if h_offset > height { break; };

            stdout.queue(MoveTo(area.origin.x, area.origin.y + h_offset))?;

            let skip = if !was_first_line_placed && self.top_wraps > 0 {
                was_first_line_placed = true;
                *line.wrap_cols.get(self.top_wraps - 1).expect("no col") as usize + 1
            } else { 0 };

            let mut count = 0;

            for col in line.text.iter().skip(skip) {
                if let WrapType::NoWrap = self.wrap_type {
                    if count + col.width > width { break; }
                }

                stdout.queue(PrintStyledContent(col.col.clone()))?;

                count = count + 1;

                if col.is_wrapping {
                    h_offset += 1;
                    if h_offset > height { break; }
                    stdout.queue(MoveTo(area.origin.x, area.origin.y + h_offset))?;
                } 
            }

            // Erasing anything that is leftover
            let mut blank_space = String::new();
            for _ in 0..(width - count % width) {
                blank_space += " ";
            }
            stdout.queue(Print(blank_space))?;

            h_offset += 1;
        }
        let cursor = self.cursors.get(0).expect("cursor doesn't exist");
        let status = format!(
            "Term: {}x{}, File: {}x{}, width: {}, wraps: {}                      ",
            cursor.pos.x, cursor.pos.y, cursor.current.col, cursor.current.line,
            width, cursor.wraps);
                
        stdout.queue(MoveTo(0, 50))?
              .queue(Print(status))?
              .queue(MoveTo(
                  area.origin.x + cursor.pos.x as u16,
                  area.origin.y + cursor.pos.y as u16))?;

        Ok(())
    }

    fn request_area(&mut self, origin: TermPos, end: TermPos) {
        let prev_area = self.area;
        self.area = Some(self.allocate_area(origin, end));
        let mut area_changed = false;

        match prev_area {
            Some(area) => {
                if area.dims() != self.area.expect("area not set").dims() {
                    area_changed = true
                }
            },
            None => area_changed = true,
        };

        if area_changed {
            self.parse_wrapping();
        }
    }
}
