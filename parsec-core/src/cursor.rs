use std::{cmp, ops};

use crate::{
    output::{OutputArea, TextChar},
    config::{TabPlaces, FileOptions},
};

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
/// A position in a `Vec<String>` (line and character address).
#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord)]
pub struct TextPos {
    pub col: usize,
    pub line: usize,
}

impl ops::Add for TextPos {
    type Output = TextPos;

    fn add(self, rhs: Self) -> Self::Output {
        TextPos { line: self.line + rhs.line, col: self.col + rhs.col }
    }
}

impl ops::Sub for TextPos {
    type Output = TextPos;

    fn sub(self, rhs: Self) -> Self::Output {
        TextPos { line: self.line - rhs.line, col: self.col - rhs.col }
    }
}

// This is done because if the line of `self` is bigger than the line of `other`, it doesn't matter
// what the column of `other` is, `self > other`, but if the line is the same, that's when columns
// should actually be compared. `#[derive(PartialOrd, Ord)]` would just do a blind comparison.
impl cmp::PartialOrd for TextPos {
    fn ge(&self, other: &Self) -> bool {
        self.line > other.line || (self.line == other.line && self.col >= other.col)
    }

    fn gt(&self, other: &Self) -> bool {
        self.line > other.line || (self.line == other.line && self.col > other.col)
    }

    fn le(&self, other: &Self) -> bool {
        self.line < other.line || (self.line == other.line && self.col <= other.col)
    }

    fn lt(&self, other: &Self) -> bool {
        self.line < other.line || (self.line == other.line && self.col < other.col)
    }

    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        if self > other {
            Some(cmp::Ordering::Greater)
        } else if self < other {
            Some(cmp::Ordering::Less)
        } else {
            Some(cmp::Ordering::Equal)
        }
    }
}

/// A cursor in the text file. This is an editing cursor, not a printing cursor.
pub struct FileCursor {
    /// Position of the cursor on screen.
    pub pos: CursorPos,
    /// Current position of the cursor in the file.
    current: TextPos,
    /// Target position of the cursor in the file.
    target: TextPos,
    /// An anchor for a selection.
    anchor: Option<TextPos>,

    /// Column that the cursor wants to be in.
    ///
    /// If the cursor moves to a line that is at least as wide as the desired_col,
    /// it will be placed in the desired_col. If the line is shorter, it will be
    /// placed in the last column of the line.
    desired_x: usize,

    /// How many times the cursor position wraps in the line.
    wraps: u16,
}

impl FileCursor {
    // NOTE: Basically a clone of `update()`, might refactor later.
    /// Returns a new instance of `FileCursor`.
    pub fn new<T: OutputArea>(pos: TextPos, file: &File<T>) -> FileCursor {
        let width = file.area.width();

        // NOTE: This may not need to crash in the future.
        let line = file.lines.get(pos.line).expect("invalid line");
        
        let offset = line.wrap_cols().iter().filter(|&c| *c < pos.col)
                         .map(|&c| width - ((c as u16) % width + 1)).sum::<u16>();

        let wraps = (offset + pos.col as u16) / width;

        let (line_range, direction) = if pos.line > file.top_line() {
            (pos.line..file.top_line(), 1)
        } else if pos.line < file.top_line() {
            (file.top_line()..pos.line, -1)
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
            anchor: None,
            desired_x: pos.col,
            wraps,
        }
    }

    /// Moves the cursor vertically on the file. May also cause horizontal movement.
    pub fn move_ver(&mut self, count: i32, lines: &Vec<TextLine>, tabs: &TabPlaces) {
        let line = self.target.line;
        self.target.line = (line as i32 + count).clamp(0, lines.len() as i32 - 1) as usize;

        let mut text_iter =
            lines.get(self.target.line).expect("invalid line").text().iter().enumerate();

        let mut total_width = 0;
        self.target.col = 0;
        // In vertical movement, the `desired_x` dictates in what column the cursor will be placed.
        while let Some((index, text_char)) = text_iter.next() {
            if let TextChar::Primary(main_char) = text_char {
                total_width += main_char.width(total_width, tabs) as u16;
                self.target.col = index;
                if total_width > self.desired_x as u16 { break }
            }
        };
    }

    /// Moves the cursor horizontally on the file. May also cause vertical movement.
    pub fn move_hor(&mut self, count: i32, lines: &Vec<TextLine>, tabs: &TabPlaces) {
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
            let mut line_iter = lines.iter().enumerate().rev()
                                     .skip(lines.len() - self.target.line);
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

    /// Moves the cursor to a position in the file.
    ///
    /// - If the position isn't valid, it will move to the "maximum" position allowed.
    /// - Unlike `move_ver` and `move_hor`, this function will update the file, as it assumes that
    /// you are moving once, and it doesn't make sense move to two places without updating.
    pub fn move_to(&mut self, pos: TextPos, lines: &Vec<TextLine>, options: &FileOptions) {
        let line = pos.line.clamp(0, lines.len());
        self.target = TextPos {
            line, 
            col: pos.col.clamp(0, lines.get(line).unwrap().text().len()),
        };
        self.desired_x = lines.get(line).unwrap()
                              .get_distance_to_col(self.target.col, &options.tabs) as usize;

        self.update(lines, options);
    }

	/// Sets the position of the anchor to be the same as the current cursor position in the file.
	///
	/// The `anchor` and `current` can act as a range of text on the file.
	pub fn set_anchor(&mut self) {
    	self.anchor = Some(self.current);
	}

	/// Unsets the anchor.
	///
	/// This is done so the cursor no longer has a valid selection.
	pub fn unset_anchor(&mut self) {
    	self.anchor = None;
	}

    /// Updates the position of the cursor on the terminal.
    ///
    /// - This function does not take horizontal scrolling into account.
    pub fn update(&mut self, lines: &Vec<TextLine>, options: &FileOptions) {
        // If the target hasn't changed/the cursor has already been updated, nothing needs to
        // be done.
        if self.target == self.current { return }

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

    
    ////////////////////////////////
    // Getters
    ////////////////////////////////
    /// Returns the cursor's position on the file.
    pub fn current(&self) -> TextPos {
        self.current
    }

    /// Returns the cursor's position on the screen.
    pub fn target(&self) -> TextPos {
        self.target
    }

	/// Returns the cursor's anchor on the file.
	pub fn anchor(&self) -> Option<TextPos> {
    	self.anchor
	}

    /// Returns the amount of times the cursor wraps around the line.
    pub fn wraps(&self) -> u16 {
        self.wraps
    }
}
