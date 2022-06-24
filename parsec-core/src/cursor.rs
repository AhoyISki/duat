use std::cmp::{min, max};

use crate::{output::OutputArea, config::{TabPlaces, FileOptions}};

use super::file::{TextLine, File};

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
    desired_x: usize,

    /// How many times the cursor position wraps in the line.
    wraps: u16,

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
            desired_x: pos.col,
            wraps,
        }
    }

    /// Returns the cursor's position on the file.
    pub fn current(&self) -> FilePos {
        self.current
    }

    /// Returns the cursor's position on the screen.
    pub fn target(&self) -> FilePos {
        self.target
    }

    /// Returns the amount of times the cursor wraps around the line.
    pub fn wraps(&self) -> u16 {
        self.wraps
    }

    /// Moves the cursor vertically on the file. May also cause horizontal movement.
    pub fn move_vertically(&mut self, lines: &Vec<TextLine>, count: i32, tabs: &TabPlaces) {
        self.target.line = (self.target.line as i32 + count).clamp(0, lines.len() as i32 - 1) as usize;

        let mut text_iter = lines.get(self.target.line).expect("invalid line").text().iter().enumerate();

        let mut total_width = 0;
        self.target.col = 0;
        // In vertical movement, the `desired_x` dictates in what column the cursor will be placed.
        while let Some((index, ch)) = text_iter.next() {
            total_width += ch.width(total_width, tabs) as u16;
            self.target.col = index;
            if total_width > self.desired_x as u16 { break }
        };
    }

    /// Moves the cursor horizontally on the file. May also cause vertical movement.
    pub fn move_horizontally(&mut self, lines: &Vec<TextLine>, count: i32, tabs: &TabPlaces) {
        let mut new_col = self.target.col as i32 + count;

        if count >= 0 {
            let mut line_iter = lines.iter().enumerate().skip(self.target.line);
            // Subtract line lenghts until a column is within the line's bounds.
            while let Some((index, line)) = line_iter.next() {
                self.target.line = index;
                if new_col <= line.text().len() as i32 || index == lines.len() - 1 { break }
                new_col -= line.text().len() as i32 + 1;
            }
        } else {
            let mut line_iter = lines.iter().enumerate().rev().skip(lines.len() - self.target.line);
            // Add line lenghts until the column is positive or equal to 0, making it valid.
            while let Some((index, line)) = line_iter.next() {
                if new_col >= 0 { break }
                new_col += line.text().len() as i32 + 1;
                self.target.line = index;
            }
        }

        let line = lines.get(self.target.line).unwrap();
        self.target.col = new_col.clamp(0, line.text().len() as i32) as usize;
        self.desired_x = line.get_distance_to_col(self.target.col, tabs) as usize;
    }

    /// Updates the position of the cursor on the terminal.
    pub fn update(&mut self, lines: &Vec<TextLine>, options: &FileOptions) {
        let target_line = lines.get(self.target.line).expect("invalid line");
        // TODO: Calculate in relation to (width - indentation) with the possibility
        // of that being only on the first line, for wrapped lines that start at 0
        let (target_x, target_wraps) = {
            let (target_wraps, first_col) =
            match target_line.wrap_cols().iter().enumerate().rfind(|&c| *c.1 < self.target.col) {
                Some((index, col)) => (index + 1, col + 1),
                None => (0, 0),
            };

            let mut target_x = target_line.get_distance_to_col(self.target.col, &options.tabs)
                - target_line.get_distance_to_col(first_col, &options.tabs);
            if target_wraps > 0 && options.wrap_indent {
                target_x += target_line.indent() as u16;
            }

            (target_x, target_wraps as u16)
        };

        // The range shouldn't include the wrapped lines of the last line.
        let (line_range, direction) = if self.target.line > self.current.line {
            (self.current.line..self.target.line, 1)
        } else if self.target.line < self.current.line {
            (self.target.line..self.current.line, -1)
        } else { (0..0, 0) };

        // The calculated `d_y`, taking wrappings into account.
        let mut d_y = 0;
        for line in lines.get(line_range).expect("invalid line") {
            d_y += direction * (1 + line.wrap_cols().len() as i32);
        }

        // `y` is the combination of the initial position `pos.y`, the height between
        // lines `d_y`, the amount of times the cursor wraps around `target_wraps`
        // minus the amount of times the cursor originally wraped around `wraps`.
        self.pos.y = self.pos.y + d_y + target_wraps as i32 - self.wraps as i32;
        self.wraps = target_wraps as u16;
        self.pos.x = target_x as i32;
        self.current.line = self.target.line;
        self.current.col = self.target.col;
    }
}
