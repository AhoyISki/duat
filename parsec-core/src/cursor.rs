use std::cmp::{max, min};

use super::file::TextLine;
use crate::{
    action::TextRange,
    config::{Config, TabPlaces},
    ui::{EndNode, Ui},
    saturating_add_signed
};

// NOTE: `col` and `line` are line based, while `byte` is file based.
/// A position in a `Vec<String>` (line and character address).
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct TextPos {
    pub col: usize,
    pub byte: usize,
    pub line: usize,
}

impl TextPos {
    pub fn translate_to(self, lines: &[TextLine], line: usize, col: usize) -> TextPos {
        let mut new = TextPos { line, col: 0, ..self };
        new.byte = saturating_add_signed(new.byte, get_byte_distance(lines, self, new));
        new
    }

    /// Adds columns given `self.line == other.line`.
    pub fn col_add(&self, other: TextPos) -> TextPos {
        if self.line == other.line { TextPos { line: self.line, ..(*self + other) } } else { *self }
    }

    /// Subtracts columns given `self.line == other.line`.
    pub fn col_sub(&self, other: TextPos) -> TextPos {
        if self.line == other.line { TextPos { line: self.line, ..(*self - other) } } else { *self }
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
// should be compared. `#[derive(PartialOrd, Ord)]` would just do a "both are bigger" comparison.
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

// Same deal here, for some reason, `Impl Ord` doesn't seem to take my implementation of
// `PartialOrd`, when considering how to implement `Ord` and just goes for the "both are bigger"
// comparison.
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
#[derive(Debug, Clone)]
pub struct TextCursor {
    // The `current` adn `target` positions pretty much exist solely for more versatile comparisons
    // of movement when printing to the screen.
    // Often, you'll see `old_target` being used as a variable instead of `current`. This is
    // because the user may want to execute multiple movements in rapid succession without
    // printing to the screen, and `current` is just a useful reminder of where the cursor was
    // the last the screen was printed.
    /// Current position of the cursor in the file.
    prev: TextPos,

    /// Target position of the cursor in the file.
    cur: TextPos,

    /// An anchor for a selection.
    anchor: Option<TextPos>,

    /// Column that the cursor wants to be in.
    ///
    /// If the cursor moves to a line that is at least as wide as the desired_col,
    /// it will be placed in the desired_col. If the line is shorter, it will be
    /// placed in the last column of the line.
    desired_x: usize,
}

impl TextCursor {
    /// Returns a new instance of `FileCursor`.
    pub fn new(pos: TextPos, lines: &[TextLine], node: &EndNode<impl Ui>) -> TextCursor {
        let line = lines.get(pos.line).unwrap();
        TextCursor {
            prev: pos,
            cur: pos,
            // This should be fine.
            anchor: None,
            desired_x: line.get_distance_to_col_node(pos.col, node),
        }
    }

    /// Moves the cursor vertically on the file. May also cause horizontal movement.
    pub fn move_vertically(&mut self, count: i32, lines: &Vec<TextLine>, node: &EndNode<impl Ui>) {
        let old_target = self.cur;

        let line = self.cur.line;
        self.cur.line = (line as i32 + count).clamp(0, lines.len() as i32 - 1) as usize;
        let line = &lines[self.cur.line];

        // In vertical movement, the `desired_x` dictates in what column the cursor will be placed.
        (self.cur.col, _) = line.get_col_at_distance(self.desired_x, &node.raw());

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        self.cur.byte = (self.cur.byte as isize
            + get_byte_distance(lines, old_target, self.cur))
            as usize;
    }

    /// Moves the cursor horizontally on the file. May also cause vertical movement.
    pub fn move_horizontally(
        &mut self, count: i32, lines: &Vec<TextLine>, node: &EndNode<impl Ui>,
    ) {
        let old_target = self.cur;
        let mut col = self.cur.col as i32 + count;

        if count >= 0 {
            let mut line_iter = lines.iter().enumerate().skip(self.cur.line);
            // Subtract line lenghts until a column is within the line's bounds.
            while let Some((index, line)) = line_iter.next() {
                self.cur.line = index;
                if col < line.char_count() as i32 {
                    break;
                }
                col -= line.char_count() as i32;
            }
        } else {
            let mut line_iter = lines.iter().enumerate().take(self.cur.line).rev();
            // Add line lenghts until the column is positive or equal to 0, making it valid.
            while let Some((index, line)) = line_iter.next() {
                if col >= 0 {
                    break;
                }
                col += line.char_count() as i32;
                self.cur.line = index;
            }
        }

        let line = lines.get(self.cur.line).unwrap();
        self.cur.col = col.clamp(0, line.text().len() as i32) as usize;

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        self.cur.byte = (self.cur.byte as isize
            + get_byte_distance(lines, old_target, self.cur))
            as usize;

        self.desired_x = line.get_distance_to_col(self.cur.col, &node.raw()) as usize;
    }

    /// Moves the cursor to a position in the file.
    ///
    /// - If the position isn't valid, it will move to the "maximum" position allowed.
    /// - This command sets `desired_x`.
    pub fn move_to(&mut self, pos: TextPos, lines: &Vec<TextLine>, node: &EndNode<impl Ui>) {
        let old_target = self.cur;

        self.cur.line = pos.line.clamp(0, lines.len());
        self.cur.col = pos.col.clamp(0, lines.get(self.cur.line).unwrap().text().len());

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        self.cur.byte = (self.cur.byte as isize
            + get_byte_distance(lines, old_target, self.cur))
            as usize;

        let line = lines.get(self.cur.line).unwrap();
        self.desired_x = line.get_distance_to_col(self.cur.col, &node.raw());
    }

    /// Sets the position of the anchor to be the same as the current cursor position in the file.
    ///
    /// The `anchor` and `current` act as a range of text on the file.
    pub fn set_anchor(&mut self) {
        self.anchor = Some(self.cur);
    }

    /// Unsets the anchor.
    ///
    /// This is done so the cursor no longer has a valid selection.
    pub fn unset_anchor(&mut self) {
        self.anchor = None;
    }

    /// Returns the range between `target` and `anchor`.
    ///
    /// If `anchor` isn't set, returns an empty range on `target`.
    pub fn range(&self) -> TextRange {
        let anchor = self.anchor.unwrap_or(self.cur);

        TextRange { start: min(self.cur, anchor), end: max(self.cur, anchor) }
    }

    /// Updates the position of the cursor on the terminal.
    ///
    /// - This function does not take horizontal scrolling into account.
    pub fn update(&mut self) {
        self.prev = self.cur;
    }

    ////////////////////////////////
    // Getters
    ////////////////////////////////
    /// Returns the cursor's position on the file.
    pub fn prev(&self) -> TextPos {
        self.prev
    }

    /// Returns the cursor's position on the screen.
    pub fn cur(&self) -> TextPos {
        self.cur
    }

    /// Returns the cursor's anchor on the file.
    pub fn anchor(&self) -> Option<TextPos> {
        self.anchor
    }
}

// This function is just a much more efficient way of getting the byte in a position than having to
// add up every line's len, reducing possibly thousands of -- albeit cheap -- operations to usually
// only 30.
// NOTE: It could still be made more efficient.
/// Returns the difference in byte index between two positions in a `Vec<TextLine>`.
///
/// Returns positive if `target > current`, negative if `target < current`, 0 otherwise.
pub fn get_byte_distance(lines: &[TextLine], current: TextPos, target: TextPos) -> isize {
    let mut distance = lines[target.line].get_line_byte_at(target.col) as isize;
    distance -= lines[current.line].get_line_byte_at(current.col) as isize;

    let (direction, range) = if target.line > current.line {
        (1, current.line..target.line)
    } else if target.line < current.line {
        (-1, target.line..current.line)
    } else {
        return distance;
    };

    lines[range].iter().for_each(|l| distance += direction * (l.text().len() as isize));

    distance
}
