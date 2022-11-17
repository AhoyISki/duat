use std::cmp::{max, min};

use super::file::TextLine;
use crate::{
    action::{Change, Splice, TextRange},
    layout::SpliceAdder,
    saturating_add_signed,
    ui::{EndNode, Ui},
};

// NOTE: `col` and `line` are line based, while `byte` is file based.
/// A position in a `Vec<String>` (line and character address).
#[derive(Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TextPos {
    pub byte: usize,
    pub col: usize,
    pub row: usize,
}

impl std::fmt::Debug for TextPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("col: {}, line: {}, byte: {}", self.col, self.row, self.byte))
    }
}

impl TextPos {
    /// Creates a new cursor, based on this `TextPosition`, translated in lines and columns.
    pub fn calibrated_cursor(self, lines: &[TextLine], line: usize, col: usize) -> TextPos {
        let mut new = TextPos { row: line, col, ..self };
        new.byte = saturating_add_signed(new.byte, get_byte_distance(lines, self, new));
        new
    }

    /// Adds columns given `self.line == other.line`.
    pub fn col_add(&self, other: TextPos) -> TextPos {
        if self.row == other.row {
            TextPos { row: self.row, ..(*self + other) }
        } else {
            *self
        }
    }

    /// Subtracts columns given `self.line == other.line`.
    pub fn col_sub(&self, other: TextPos) -> TextPos {
        if self.row == other.row {
            TextPos { row: self.row, ..(*self - other) }
        } else {
            *self
        }
    }

    pub fn calibrate(&mut self, splice: &Splice) {
        let Splice { start, added_end, taken_end } = splice;
        if *self > *start {
            // The column will only change if the `TextPos` is in the same line.
            if self.row == taken_end.row {
                self.col += added_end.col - taken_end.col;
            }

            self.byte += added_end.byte - taken_end.byte;
            // The line of the cursor will increase if the edit has more lines than the original
            // selection, and vice-versa.
            self.row += added_end.row - taken_end.row;
        }
    }
}

impl std::ops::Add for TextPos {
    type Output = TextPos;

    fn add(self, rhs: Self) -> Self::Output {
        TextPos {
            row: self.row + rhs.row,
            byte: self.byte + rhs.byte,
            col: if self.row == rhs.row { self.col + rhs.col } else { self.col },
        }
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
        TextPos {
            row: self.row - rhs.row,
            byte: self.byte - rhs.byte,
            col: if self.row == rhs.row { self.col - rhs.col } else { self.col },
        }
    }
}

impl std::ops::SubAssign for TextPos {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

/// A cursor in the text file. This is an editing cursor, not a printing cursor.
#[derive(Debug)]
pub struct TextCursor {
    // The `prev` and `cur` positions pretty much exist solely for more versatile comparisons
    // of movement when printing to the screen.
    /// Previous position of the cursor in the file.
    prev: Option<TextPos>,

    /// Current position of the cursor in the file.
    cur: TextPos,

    /// An anchor for a selection.
    anchor: Option<TextPos>,

    /// A change, temporarily associated to this cursor for faster access.
    pub(crate) change: Option<Change>,

    /// Column that the cursor wants to be in.
    ///
    /// If the cursor moves to a line that is at least as wide as the desired_col,
    /// it will be placed in the desired_col. If the line is shorter, it will be
    /// placed in the last column of the line.
    desired_x: usize,
}

impl Clone for TextCursor {
    fn clone(&self) -> Self {
        TextCursor { prev: None, desired_x: self.cur.col, change: None, ..*self }
    }
}

impl TextCursor {
    /// Returns a new instance of `FileCursor`.
    pub fn new(pos: TextPos, lines: &[TextLine], node: &EndNode<impl Ui>) -> TextCursor {
        let line = lines.get(pos.row).unwrap();
        TextCursor {
            prev: None,
            cur: pos,
            // This should be fine.
            anchor: None,
            change: None,
            desired_x: line.get_distance_to_col_node(pos.col, node),
        }
    }

    /// Internal vertical movement function.
    pub(crate) fn move_ver_inner(
        &mut self, count: i32, lines: &Vec<TextLine>, node: &EndNode<impl Ui>,
    ) {
        let old_target = self.cur;
        let cur = &mut self.cur;

        let line = cur.row;
        cur.row = (line as i32 + count).clamp(0, lines.len() as i32 - 1) as usize;
        let line = &lines[cur.row];

        // In vertical movement, the `desired_x` dictates in what column the cursor will be placed.
        (cur.col, _) = line.get_col_at_distance(self.desired_x, &node.raw());

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        cur.byte = (cur.byte as isize + get_byte_distance(lines, old_target, *cur)) as usize;
    }

    /// Internal horizontal movement function.
    pub(crate) fn move_hor_inner(
        &mut self, count: i32, lines: &Vec<TextLine>, node: &EndNode<impl Ui>,
    ) {
        let old_cur = self.cur;
        let cur = &mut self.cur;
        let mut col = old_cur.col as i32 + count;

        if count >= 0 {
            let mut line_iter = lines.iter().enumerate().skip(old_cur.row);
            // Subtract line lenghts until a column is within the line's bounds.
            while let Some((index, line)) = line_iter.next() {
                cur.row = index;
                if col < line.char_count() as i32 {
                    break;
                }
                col -= line.char_count() as i32;
            }
        } else {
            let mut line_iter = lines.iter().enumerate().take(old_cur.row).rev();
            // Add line lenghts until the column is positive or equal to 0, making it valid.
            while let Some((index, line)) = line_iter.next() {
                if col >= 0 {
                    break;
                }
                col += line.char_count() as i32;
                cur.row = index;
            }
        }

        let line = lines.get(cur.row).unwrap();
        cur.col = col.clamp(0, line.text().len() as i32) as usize;

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        cur.byte = (cur.byte as isize + get_byte_distance(lines, old_cur, *cur)) as usize;

        self.desired_x = line.get_distance_to_col(cur.col, &node.raw()) as usize;
    }

    /// Internal absolute movement function. Assumes that `pos` is not calibrated.
    pub(crate) fn move_to_inner(
        &mut self, pos: TextPos, lines: &Vec<TextLine>, node: &EndNode<impl Ui>,
    ) {
        let old_target = self.cur;
        let cur = &mut self.cur;

        cur.row = pos.row.clamp(0, lines.len());
        cur.col = pos.col.clamp(0, lines[pos.row].text().len());

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        cur.byte = (cur.byte as isize + get_byte_distance(lines, old_target, pos)) as usize;

        let line = lines.get(pos.row).unwrap();
        self.desired_x = line.get_distance_to_col(pos.col, &node.raw());
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
        self.prev = Some(self.cur);
    }

    ////////////////////////////////
    // Getters
    ////////////////////////////////
    /// Returns the cursor's position on the file.
    pub fn prev(&self) -> TextPos {
        self.prev.unwrap_or(self.cur)
    }

    /// Returns the cursor's position on the screen.
    pub fn cur(&self) -> TextPos {
        self.cur
    }

    /// Returns the cursor's anchor on the file.
    pub fn anchor(&self) -> Option<TextPos> {
        self.anchor
    }

	/// Calibrates a cursor's positions based on some splice.
    pub(crate) fn calibrate(&mut self, splice_adder: &SpliceAdder) {
        relative_add(&mut self.cur, splice_adder);
        self.anchor.as_mut().map(|a| relative_add(a, splice_adder));
        if let Some(change) = &mut self.change {
            change.splice.calibrate_on_adder(splice_adder);
        };
    }

	/// Commits the change and empties it.
    pub fn commit(&mut self) -> Option<Change> {
        self.change.take()
    }
}

pub fn relative_add(pos: &mut TextPos, splice_adder: &SpliceAdder) {
    pos.row = saturating_add_signed(pos.row, splice_adder.lines);
    pos.byte = saturating_add_signed(pos.byte, splice_adder.bytes);
    if pos.row == splice_adder.last_line {
        pos.col = saturating_add_signed(pos.col, splice_adder.cols);
        //if pos.row == 4 { panic!("{:#?}, {}", pos, splice_adder.cols); }
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
    let mut distance = lines[target.row].get_line_byte_at(target.col) as isize;
    distance -= lines[current.row].get_line_byte_at(current.col) as isize;

    let (direction, range) = if target.row > current.row {
        (1, current.row..target.row)
    } else if target.row < current.row {
        (-1, target.row..current.row)
    } else {
        return distance;
    };

    lines[range].iter().for_each(|l| distance += direction * (l.text().len() as isize));

    distance
}
