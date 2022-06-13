use std::cmp::min;

use crossterm::style::{
    Stylize,
    Attribute::Reverse,
};

use crate::output::{OutputArea, StyledChar};

use super::file::{FileLine, File};

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
    pub pos: CursorPos,
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
    wraps: u16,

    /// The text under the cursor.
    cursor_text: StyledChar,
    // TODO: Eventually add a selection to the cursor.
}

impl FileCursor {
    // NOTE: Basically a clone of `update()`, might refactor later.
    /// Returns a new instance of `FileCursor`.
    pub fn new<T: OutputArea>(pos: FilePos, file: &File<T>) -> FileCursor {
        let width = file.area.width();

        // NOTE: This may not need to crash in the future.
        let line = file.lines.get(pos.line).expect("invalid line");
        
        let offset = line.wrap_cols().iter().filter(|&c| *c < pos.col)
                         .map(|&c| width - ((c as u16) % width + 1)).sum::<u16>();

        let wraps = (offset + pos.col as u16) / width;

        let (line_range, direction) = if pos.line > *file.top_line() {
            (pos.line..*file.top_line(), 1)
        } else if pos.line < *file.top_line() {
            (*file.top_line()..pos.line, -1)
        } else { (0..0, 0) };

        let mut y = wraps as i32;
        for line in file.lines.get(line_range).unwrap() {
            y += direction * (1 + line.wrap_cols().len() as i32);
        }

        let cursor_pos = CursorPos {
            x: ((pos.col as u16 + offset) - (width * wraps)) as i32,
            y,
        };

        FileCursor {
            pos: cursor_pos,
            current: pos,
            target: pos,
            desired_col: pos.col,
            wraps,
            cursor_text: {
                let mut ch = file.get_char(pos).expect("invalid character").clone();

                ch.text = ch.text.attribute(Reverse);

                ch
            }
        }
    }

    /// Returns the cursor's position on the file.
    pub fn current(&self) -> &FilePos {
        &self.current
    }

    /// Returns the cursor's position on the screen.
    pub fn target(&self) -> &FilePos {
        &self.target
    }

    /// Moves the cursor up on the file.
    pub fn move_up(&mut self, lines: &Vec<FileLine>) {
        if self.target.line != 0 {
            if let Some(line) = lines.get(self.target.line - 1) {
                self.target.col = min(self.desired_col, line.text().len());
                self.target.line -= 1;
            }
        }
    }

    /// Moves the cursor down on the file.
    pub fn move_down(&mut self, lines: &Vec<FileLine>){
        if let Some(line) = lines.get(self.target.line + 1) {
            self.target.col = min(self.desired_col, line.text().len());
            self.target.line += 1;
        }
    }

    /// Moves the cursor left on the file.
    pub fn move_left(&mut self, lines: &Vec<FileLine>) {
        if self.target.col == 0 {
            if self.current.line != 0 {
                if let Some(line) = lines.get(self.current.line - 1) {
                    self.target.col = line.text().len();
                    self.desired_col = self.target.col;
                    self.target.line -= 1;
                }
            }
        } else {
            self.target.col -= 1;
            self.desired_col = self.target.col;
        }
    }

    /// Moves the cursor right on the file.
    pub fn move_right(&mut self, lines: &Vec<FileLine>) {
        let line = lines.get(self.target.line).expect("invalid line");

        // TODO: Maybe add an option to change this 0 into the indentation.
        // TODO: Add an option to finish movement at the end of a line.
        if self.target.col == line.text().len() {
            if let Some(_) = lines.get(self.current.line + 1) {
                self.target.col = 0;
                self.target.line += 1;
            }
        } else {
            self.target.col += 1;
            self.desired_col = self.target.col;
        }
    }

    /// Updates the position of the cursor on the terminal.
    pub fn update<T: OutputArea>(&mut self, lines: &Vec<FileLine>, area: &T) {
        let width = area.width();

        let text = lines.get(self.target.line).expect("invalid line");
        // The sum of the distance between the wrapping char and the last column in
        // the allocated area, for every wrapping char before `target.col`. 
        let offset = text.wrap_cols().iter().filter(|&c| *c < self.target.col)
                         .map(|&c| width - ((c as u16) % width + 1)).sum::<u16>();

        // TODO: Calculate in relation to (width - indentation) with the possibility
        // of that being only on the first line, for wrapped lines that start at 0
        let target_wraps = ((offset + self.target.col as u16) / width) as i32;

        // The range shouldn't include the wrapped lines of the last line.
        let (line_range, direction) = if self.target.line > self.current.line {
            (self.current.line..self.target.line, 1)
        } else if self.target.line < self.current.line {
            (self.target.line..self.current.line, -1)
        } else { (0..0, 0) };

        // The calculated `d_y`, taking into wrappings into account.
        let mut d_y = 0;
        for line in lines.get(line_range).expect("invalid line") {
            d_y += direction * (1 + line.wrap_cols().len() as i32);
        }

        self.wraps = target_wraps as u16;
        // `y` is the combination of the initial position `pos.y`, the height between
        // lines `d_y`, the amount of times the cursor wraps around `target_wraps`
        // minus the amount of times the cursor originally wraped around `wraps`.
        self.pos.y = self.pos.y + d_y + target_wraps - self.wraps as i32;
        self.pos.x = (self.target.col as u16 + offset - (self.wraps * width)) as i32;
        self.current.line = self.target.line;
        self.current.col = self.target.col;
    }

    // TODO: implement styling and other cursor shapes
    pub fn print<T: OutputArea>(&self, area: &mut T) {
        area.move_cursor(self.pos.into());
        area.print_and_style(self.cursor_text.clone());
    }
}
