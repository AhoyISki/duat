use std::{
    path::PathBuf,
    fs,
    cmp::min,
};

use crate::{
    map_actions,
    impl_input_handler,
    input::{
        InputHandler,
        ModeList,
    },
    output::{
        OutputArea,
        StyledChar,
        OutputPos,
    },
    config::{
        WrapType,
        FileOptions,
    }
};

use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;
use crossterm::event::{
    KeyEvent,
    KeyCode,
    KeyModifiers
};

/// A position used for cursors.
/// 
/// This object indicates where each cursor is, in relation to the output
/// area. This means that the cursors should be able to have negative
/// positions. In y for previous lines, and in x for previous columns.
#[derive(Copy, Clone)]
pub struct CursorPos {
    pub x: i32,
    pub y: i32
}

// Any use of the terms col and line refers specifically to a position in the file,
// x and y are reserved for positions on the screen.
// TODO: move this to a more general file.
// TODO: In the future, this shouldn't be public, probably.
/// A position in the file (line and character address).
#[derive(Copy, Clone)]
pub struct FilePos {
    pub col: usize,
    pub line: usize,
}

/// A cursor in the text file. This is an editing cursor, not a printing cursor.
pub struct FileCursor {
    /// Position of the cursor on screen
    pos: CursorPos,

    /// Current position of the cursor in the file
    current: FilePos,
    /// Target position of the cursor in the file
    target: FilePos,

    /// Column that the cursor wants to be in
    ///
    /// If the cursor moves to a line that is at least as wide as the desired_col,
    /// it will be placed in the desired_col. If the line is shorter, it will be
    /// placed in the last column of the line.
    desired_col: usize,

    /// How many times the cursor position wraps in the line.
    wraps: u16
    // TODO: Eventually add a selection to the cursor.
}

impl FileCursor {
    /// Returns the cursor's position on the screen
    pub fn pos(&self) -> &CursorPos {
        &self.pos
    }

    /// Returns the cursor's position on the file
    pub fn current(&self) -> &FilePos {
        &self.current
    }
}

// TODO: move this to a more general file.
/// A line in the text file.
pub struct FileLine {
    wrap_cols: Vec<u16>,
    text: Vec<StyledChar>,
    indent: u8,

    // Since the width function doesn't take into account the width of cols.
    width: usize,
}

impl FileLine {
    /// Returns the columns in the line that wrap.
    pub fn wrap_cols(&self) -> &Vec<u16> {
        &self.wrap_cols
    }

    /// Returns the contents of the line.
    pub fn text(&self) -> &Vec<StyledChar> {
        &self.text
    }

    /// Parses the wrapping of a single line.
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

// NOTE: This struct should strive to be completely UI agnostic, i.e., it should
// work wether the app is used in a terminal or in a GUI.
pub struct FileHandler<T: OutputArea> {
    /// The contents of the file
    pub file: Vec<FileLine>,

    // Where exactly on the screen the origin and end of the area are placed is not
    // important here.
    /// The area allocated to the status line.
    pub status_line_area: T,
    /// The area allocated to the line numbers.
    pub line_num_area: T,
    /// The area allocated to the file.
    pub file_area: T,

    // TODO: Move these into a "FileState" struct.
    // Info about which line is on the top of the area.
    /// The line at the top of the screen.
    top_line: usize,
    /// How many times the line at the top of the screen wraps around.
    top_wraps: usize,

    /// Options related to files
    pub options: FileOptions,

    /// List of active cursors in the file.
    cursors: Vec<FileCursor>,

    /// List of mapped modes for file editing.
    mappings: ModeList<FileHandler<T>>,
}

impl<T: OutputArea> FileHandler<T> {
    /// Returns a new instance of ContentArea
    pub fn new(mut area: T, path: PathBuf, options: FileOptions) -> FileHandler<T> {
        // TODO: In the future, this will not panic!
        // TODO: Move this to a more general file.
        let raw_file = fs::read_to_string(path).expect("file not found");

        // Used for calculating the file and line numbers areas.
        let mut file_len = 0;
        let mut file_area;

        let mut file_handler = FileHandler {
            file: {
                let mut file = Vec::new();
                for line_text in raw_file.lines() {
                    let mut line = FileLine {
                        wrap_cols: Vec::new(),
                        text: Vec::new(),
                        indent: 0,
                        // So characters can be appended to the end of lines.
                        width: 0
                    };

                    let mut indenting = true;

                    for grapheme in line_text.graphemes(true) {
                        // TODO: Add variable tab size.
                        // NOTE: This probably has some obscure bugs embeded in it.
                        if indenting && grapheme == " " || grapheme == "\t" {
                               line.indent += UnicodeWidthStr::width(grapheme) as u8;
                        } else { indenting = false; }

                        let width = UnicodeWidthStr::width(grapheme);

                        line.text.push(StyledChar::new(grapheme, width));
                    }

                    line.width += line.text.len();

                    file.push(line);
                };

                file_len = file.len();

                file
            },

            status_line_area: {
                file_area = area.partition_y(area.height() - 2);
                area
            },
            line_num_area: {
                let mut line_num_width = 3;
                let mut num_exp        = 10;
                    
                while file_len > num_exp {
                    num_exp *= 10;
                    line_num_width += 1;
                }
                file_area.partition_x(line_num_width)
            },
            file_area,

            // TODO: Remember last session.
            top_line: 0,
            top_wraps: 0,

            options,
            cursors: vec![ FileCursor {
                pos: CursorPos { x: 0, y: 0 },
                current: FilePos { col: 0, line: 0 },
                target: FilePos { col: 0, line: 0 },
                desired_col: 0,
                wraps: 0,
            } ],

            mappings: ModeList::new(),
        };

        map_actions! {
            file_handler: FileHandler<T>, mappings;
            "default" => [
                // Move all cursors down.
                key: (KeyCode::Down, KeyModifiers::NONE) => {
                    |handler: &mut FileHandler<T>| {
                        for i in 0..handler.cursors.len() {
                            handler.move_cursor_down(i, true);
                        }
                    }
                },
                // Move all cursors up.
                key: (KeyCode::Up, KeyModifiers::NONE) => {
                    |handler: &mut FileHandler<T>| {
                        for i in 0..handler.cursors.len() {
                            handler.move_cursor_up(i, true);
                        }
                    }
                },
                // Move all cursors left.
                key: (KeyCode::Left, KeyModifiers::NONE) => {
                    |handler: &mut FileHandler<T>| {
                        for i in 0..handler.cursors.len() {
                            handler.move_cursor_left(i, true);
                        }
                    }
                },
                // Move all cursors right.
                key: (KeyCode::Right, KeyModifiers::NONE) => {
                    |handler: &mut FileHandler<T>| {
                        for i in 0..handler.cursors.len() {
                            handler.move_cursor_right(i, true);
                        }
                    }
                },
            ]
        }

        file_handler
    }

    /// Updates the position of a cursor on the screen
    ///
    /// - It will update self.pos, and self.current will be equal to self.target.
    fn update_pos(&mut self, cursor_num: usize) {
        let (width, height) = (self.file_area.width(), self.file_area.height());

        let x_spacing = self.options.cursor_x_spacing;
        let y_spacing = self.options.cursor_y_spacing;

        let mut cursor = self.cursors.get_mut(cursor_num).expect("invalid cursor");
        let (col, line) = (cursor.target.col as i32, cursor.target.line as i32);

        let text_line = self.file.get(line as usize).expect("invalid line");

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

        for line in self.file.get(line_range).expect("reading line failed") {
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
        if cursor.pos.y  < y_spacing as i32 {
            for _ in cursor.pos.y ..y_spacing as i32 {
                if !self.scroll_up() { break; }
            }
        } else if cursor.pos.y > (height - y_spacing) as i32 {
            for _ in (height - y_spacing) as i32..cursor.pos.y {
                self.scroll_down();
            }
        }

        self.print_screen();
    }

    /// Scrolls the file down by one line
    fn scroll_down(&mut self) {
        let top_line = self.file.get(self.top_line).expect("line not found");

        if top_line.wrap_cols.len() > self.top_wraps {
            self.top_wraps += 1;
        } else {
            self.top_line += 1;
            self.top_wraps = 0;
        }

        // Moves the cursors up by one, to keep them in the same place in the file.
        self.cursors.iter_mut().for_each(|cursor| cursor.pos.y -= 1);
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
                let top_line = self.file.get(self.top_line).expect("line not found");
                self.top_wraps = top_line.wrap_cols.len();
            }
            // Moves the cursors down by one, to keep them in the same place in the
            // file.
            self.cursors.iter_mut().for_each(|cursor| cursor.pos.y += 1);
            true
        }
    }

    /// Moves the cursor right on the file
    pub fn move_cursor_right(&mut self, cursor_num: usize, update_pos: bool) {
        let cursor = self.cursors.get_mut(cursor_num).expect("cursor not found");
        let line = self.file.get(cursor.target.line).expect("line not found");

        // TODO: Maybe add an option to change this 0 into the indentation.
        // TODO: Add an option to finish movement at the end of a line.
        if cursor.target.col == line.width {
            if let Some(_) = self.file.get(cursor.current.line + 1) {
                cursor.target.col = 0;
                cursor.target.line += 1;
            }
        } else {
            cursor.target.col += 1;
            cursor.desired_col = cursor.target.col;
        }

        if update_pos { self.update_pos(cursor_num); }
    }

    /// Moves the cursor left on the file
    pub fn move_cursor_left(&mut self, cursor_num: usize, update_pos: bool) {
        let cursor = self.cursors.get_mut(cursor_num).expect("invalid cursor");

        if cursor.target.col == 0 && cursor.target.line != 0{
            if cursor.current.line != 0 {
                if let Some(line) = self.file.get(cursor.current.line - 1) {
                    cursor.target.col = line.width;
                    cursor.desired_col = cursor.target.col;
                    cursor.target.line -= 1;
                }
            }
        } else {
            cursor.target.col -= 1;
            cursor.desired_col = cursor.target.col;
        }

        if update_pos { self.update_pos(cursor_num); }
    }

    /// Moves the cursor down on the file
    pub fn move_cursor_down(&mut self, cursor_num: usize, update_pos: bool){
        let cursor = self.cursors.get_mut(cursor_num).expect("invalid cursor");

        if let Some(line) = self.file.get(cursor.target.line + 1) {
            cursor.target.col = min(cursor.desired_col, line.width);
            cursor.target.line += 1;
        }

        if update_pos { self.update_pos(cursor_num); }
    }

    /// Moves the cursor up on the file
    pub fn move_cursor_up(&mut self, cursor_num: usize, update_pos: bool) {
        let cursor = self.cursors.get_mut(cursor_num).expect("cursor failed");

        if cursor.target.line != 0 {
            if let Some(line) = self.file.get(cursor.target.line - 1) {
                cursor.target.col = min(cursor.desired_col, line.width);
                cursor.target.line -= 1;
            }
        }

        if update_pos { self.update_pos(cursor_num); }
    }

    /// Parses the wrapping for all the lines in the file
    ///
    /// * This should only be called when the wrap_type or width change.
    pub fn parse_wrapping(&mut self) {
        match self.options.wrap_type {
            WrapType::Width => {
                for line in self.file.iter_mut() {
                    line.parse_wrapping(self.file_area.width());
                }
            },
            WrapType::NoWrap => {
                for line in self.file.iter_mut() {
                    line.wrap_cols.clear();
                }
            },
            _ => {},
        }
    }

    /// Prints a single line in a given position, skipping `skip` characters.
    #[inline]
    fn print_line(&mut self, line: usize, pos: &mut OutputPos, skip: usize){
        // Moves the printing cursor to the beginning of the line.
        self.file_area.move_cursor(*pos);

        let line = if let Some(line) = self.file.get(line) {
            line
        } else {
            pos.y += 1;
            return
        };
        let mut col = 0;

        for ch in line.text().iter().skip(skip) {
            if let WrapType::NoWrap = self.options.wrap_type {
                if col + ch.width > self.file_area.width() as usize { break; }
            }

            self.file_area.print_and_style(ch.clone());
            col += 1;

            if ch.is_wrapping {
                pos.y += 1;
                if pos.y > self.file_area.height() { break; }
                self.file_area.move_cursor(*pos);
            }
        }

        // Erasing anything that is leftover
        let width = self.file_area.width() as usize;
        let erase_count = width - col % width;
        self.file_area.print(" ".repeat(erase_count));

        pos.y += 1;
    }

    /* TODO: Finish this function */
    /// Prints the contents of the file from line on the file.
    #[inline]
    pub fn print_screen(&mut self) {
        let mut line_origin = OutputPos { x: 0, y: 0 };

        // The line at the top of the screen and how many characters aren't shown.
        let top_line = self.file.get(self.top_line).unwrap();
        let skip = if self.top_wraps > 0 {
            *top_line.wrap_cols().get(self.top_wraps - 1).unwrap() as usize + 1
        } else {
            0
        };

        // Prints the first line and updates where to print next.
        self.print_line(self.top_line, &mut line_origin, skip);

        // Prints the remaining lines
        let mut line = self.top_line + 1;
        while line_origin.y <= self.file_area.height() {
            self.print_line(line, &mut line_origin, 0);

            line += 1;
        }
        let cursor = self.cursors.get(0).expect("cursor doesn't exist");
        let status = format!(
            "Term: {}×{}, File: {}×{}, width: {}",
            cursor.pos().x, cursor.pos().y,
            cursor.current().col, cursor.current().line, self.file_area.width()
        );

        let temp_status = OutputPos { x: 0, y: 50 };
        self.file_area.move_cursor(temp_status);
        self.file_area.print(status);
        self.file_area.move_cursor((*cursor.pos()).into());

        self.file_area.flush();
    }
}

impl_input_handler!(FileHandler<T>, mappings);
