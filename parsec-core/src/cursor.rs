use super::file::TextLine;
use crate::{
    config::{FileOptions, TabPlaces},
    file::get_char_width,
};

/// A position used for cursors.
///
/// This object indicates where each cursor is, in relation to the output
/// area. This means that the cursors should be able to have negative
/// positions. In y for previous lines, and in x for previous columns.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct CursorPos {
    pub x: i32,
    pub y: i32,
}

// Any use of the terms col and line refers specifically to a position in the file,
// x and y are reserved for positions on the screen.
// TODO: move this to a more general file.
// TODO: In the future, this shouldn't be public, probably.
/// A position in a `Vec<String>` (line and character address).
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TextPos {
    pub col: usize,
    pub byte: usize,
    pub line: usize,
}

impl TextPos {
    pub fn move_line(&self, line: usize) -> TextPos {
        TextPos { line: self.line + line, ..*self }
    }

	/// Subtracts conditionally depending on if the positions are in the same line.
	pub fn line_aware_add(&self, other: TextPos) -> TextPos {
    	if self.line == other.line {
        	*self + other
    	} else {
        	TextPos { col: self.col, byte: self.byte, line: self.line + other.line }
    	}
	}

	/// Subtracts conditionally depending on if the positions are in the same line.
	pub fn line_aware_sub(&self, other: TextPos) -> TextPos {
    	if self.line == other.line {
        	*self - other
    	} else {
        	TextPos { col: self.col, byte: self.byte, line: self.line - other.line }
    	}
	}
}

impl std::ops::Add for TextPos {
    type Output = TextPos;

    fn add(self, rhs: Self) -> Self::Output {
        TextPos { line: self.line + rhs.line, byte: self.byte + rhs.byte, col: self.col + rhs.col }
    }
}

impl std::ops::AddAssign for TextPos {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl std::ops::Sub for TextPos {
    type Output = TextPos;

    fn sub(self, rhs: Self) -> Self::Output {
        TextPos { line: self.line - rhs.line, byte: self.byte - rhs.byte, col: self.col - rhs.col }
    }
}

impl std::ops::SubAssign for TextPos {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

// This is done because if the line of `self` is bigger than the line of `other`, it doesn't matter
// what the column of `other` is, `self > other`, but if the line is the same, that's when columns
// should actually be compared. `#[derive(PartialOrd, Ord)]` would just do a blind comparison.
impl std::cmp::PartialOrd for TextPos {
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

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self > other {
            Some(std::cmp::Ordering::Greater)
        } else if self < other {
            Some(std::cmp::Ordering::Less)
        } else {
            Some(std::cmp::Ordering::Equal)
        }
    }
}

impl Ord for TextPos {
    fn clamp(self, min: Self, max: Self) -> Self
    where
        Self: Sized,
    {
        if self < min {
            min
        } else if self > max {
            max
        } else {
            self
        }
    }

    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        unsafe { self.partial_cmp(other).unwrap_unchecked() }
    }

    fn max(self, other: Self) -> Self
    where
        Self: Sized,
    {
        if other > self { other } else { self }
    }

    fn min(self, other: Self) -> Self
    where
        Self: Sized,
    {
        if other < self { other } else { self }
    }
}

/// A cursor in the text file. This is an editing cursor, not a printing cursor.
#[derive(Debug)]
pub struct FileCursor {
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
}

impl FileCursor {
    // NOTE: Basically a clone of `update()`, might refactor later.
    /// Returns a new instance of `FileCursor`.
    pub fn new(pos: TextPos, lines: &Vec<TextLine>, tabs: &TabPlaces) -> FileCursor {
        let line = lines.get(pos.line).unwrap();
        FileCursor {
            current: pos,
            target: pos,
            anchor: None,
            desired_x: line.get_distance_to_col(pos.col, tabs),
        }
    }

    /// Moves the cursor vertically on the file. May also cause horizontal movement.
    pub fn move_ver(&mut self, count: i32, lines: &Vec<TextLine>, tabs: &TabPlaces) {
        let line = self.target.line;
        self.target.line = (line as i32 + count).clamp(0, lines.len() as i32 - 1) as usize;

        let mut text_iter = lines[self.target.line].text().char_indices().enumerate();

        let mut total_width = 0;
        self.target.col = 0;
        // In vertical movement, the `desired_x` dictates in what column the cursor will be placed.
        while let Some((col, (byte, ch))) = text_iter.next() {
            total_width += get_char_width(ch, total_width, tabs);
            self.target.col = col;
            self.target.byte = byte;
            if total_width > self.desired_x {
                break;
            }
        }
    }

    /// Moves the cursor horizontally on the file. May also cause vertical movement.
    pub fn move_hor(&mut self, count: i32, lines: &Vec<TextLine>, tabs: &TabPlaces) {
        let mut col = self.target.col as i32 + count;

        if count >= 0 {
            let mut line_iter = lines.iter().enumerate().skip(self.target.line);
            // Subtract line lenghts until a column is within the line's bounds.
            while let Some((index, line)) = line_iter.next() {
                self.target.line = index;
                if col <= line.char_count() as i32 {
                    break;
                }
                col -= line.char_count() as i32 + 1;
            }
        } else {
            let mut line_iter = lines.iter().enumerate().take(self.target.line).rev();
            // Add line lenghts until the column is positive or equal to 0, making it valid.
            while let Some((index, line)) = line_iter.next() {
                if col >= 0 {
                    break;
                }
                col += line.char_count() as i32 + 1;
                self.target.line = index;
            }
        }

        let line = lines.get(self.target.line).unwrap();
        self.target.col = col.clamp(0, line.text().len() as i32) as usize;
        self.target.byte = line.get_byte_at(col as usize);
        self.desired_x = line.get_distance_to_col(self.target.col, tabs) as usize;
    }

    /// Moves the cursor to a position in the file.
    ///
    /// - If the position isn't valid, it will move to the "maximum" position allowed.
    /// - Unlike `move_ver` and `move_hor`, this function will update the file, as it assumes that
    /// you are moving once, and it doesn't make sense move to two places without updating.
    pub fn move_to(&mut self, pos: TextPos, lines: &Vec<TextLine>, options: &FileOptions) {
        let line = pos.line.clamp(0, lines.len());
        let col = pos.col.clamp(0, lines.get(line).unwrap().text().len());
        let byte = lines[line].get_byte_at(col);

        self.target = TextPos { line, byte, col };
        let line = lines.get(line).unwrap();
        self.desired_x = line.get_distance_to_col(self.target.col, &options.tabs) as usize;
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
    pub fn update(&mut self, lines: &Vec<TextLine>) {
        // If the target hasn't changed/the cursor has already been updated, nothing needs to
        // be done.
        if self.target == self.current {
            return;
        }

        let line = lines.get(self.target.line).unwrap();

        self.current = self.target;
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
}
