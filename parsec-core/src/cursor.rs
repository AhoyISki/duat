use std::cmp::{max, min};

use super::file::TextLine;
use crate::{
    action::{Change, Splice, TextRange},
    get_byte_at_col,
    layout::{file_widget::FileWidget, Widget},
    saturating_add_signed,
    ui::{EndNode, Ui}, empty_edit,
};

// NOTE: `col` and `line` are line based, while `byte` is file based.
/// A position in a `Vec<String>` (line and character address).
#[derive(Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TextPos {
    pub(crate) byte: usize,
    pub(crate) col: usize,
    pub(crate) row: usize,
}

impl std::fmt::Debug for TextPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("col: {}, line: {}, byte: {}", self.col, self.row, self.byte))
    }
}

impl TextPos {
    /// Calculates a new `TextPos`, given a target position, in rows and columns.
    pub fn translate(self, lines: &[TextLine], row: usize, col: usize) -> TextPos {
        let mut new = TextPos { row, col, ..self };
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
    pub(crate) fn move_ver(&mut self, count: i32, lines: &Vec<TextLine>, node: &EndNode<impl Ui>) {
        let old_target = self.cur;
        let cur = &mut self.cur;

        let line = cur.row;
        cur.row = (line as i32 + count).clamp(0, lines.len() as i32 - 1) as usize;
        let line = &lines[cur.row];

        // In vertical movement, the `desired_x` dictates in what column the cursor will be placed.
        (cur.col, _) = line.get_col_at_distance(self.desired_x, &node.raw());

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        cur.byte = saturating_add_signed(cur.byte, get_byte_distance(lines, old_target, *cur));
    }

    /// Internal horizontal movement function.
    pub(crate) fn move_hor(&mut self, count: i32, lines: &Vec<TextLine>, node: &EndNode<impl Ui>) {
        let old_cur = self.cur;
        let cur = &mut self.cur;
        let mut col = old_cur.col as i32 + count;

        if count >= 0 && cur.row < lines.len() - 1 {
            let mut line_iter = lines.iter().enumerate().skip(old_cur.row);
            // Subtract line lenghts until a column is within the line's bounds.
            while let Some((index, line)) = line_iter.next() {
                cur.row = index;
                if col < line.char_count() as i32 {
                    break;
                }
                col -= line.char_count() as i32;
            }
        } else if count < 0 && cur.row > 0 {
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
        cur.col = col.clamp(0, (line.text().len() - 1) as i32) as usize;

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        cur.byte = (cur.byte as isize + get_byte_distance(lines, old_cur, *cur)) as usize;

        self.desired_x = line.get_distance_to_col(cur.col, &node.raw()) as usize;
    }

    /// Internal absolute movement function. Assumes that `pos` is not calibrated.
    pub(crate) fn move_to(&mut self, pos: TextPos, lines: &Vec<TextLine>, node: &EndNode<impl Ui>) {
        let cur = &mut self.cur;

        // TODO: Change this to `saturating_sub_signed` once that gets merged.
        cur.byte = saturating_add_signed(cur.byte, get_byte_distance(lines, *cur, pos));

        cur.row = pos.row.clamp(0, lines.len());
        cur.col = pos.col.clamp(0, lines[cur.row].char_count());

        let line = lines.get(pos.row).unwrap();
        self.desired_x = line.get_distance_to_col(pos.col, &node.raw());
    }

    /// Internal absolute movement function. Assumes that `pos` is a valid position.
    pub(crate) fn move_to_calibrated(
        &mut self, pos: TextPos, lines: &Vec<TextLine>, node: &EndNode<impl Ui>,
    ) {
        self.cur = pos;
        let line = lines.get(pos.row).unwrap();
        self.desired_x = line.get_distance_to_col(pos.col, &node.raw());
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

    /// Merges the `TextCursor`s selection with another `TextRange`.
    pub fn merge(&mut self, range: &TextRange) {
        if let Some(anchor) = &mut self.anchor {
            if self.cur > *anchor {
                self.cur = max(self.cur, range.end);
                *anchor = min(*anchor, range.start);
            } else {
                *anchor = max(*anchor, range.end);
                self.cur = min(self.cur, range.start);
            }
        } else {
            self.cur = max(self.cur, range.end);
            self.anchor = Some(range.start);
        }
    }
}

pub struct EditCursor<'a>(&'a mut TextCursor, &'a mut SpliceAdder);

impl<'a> EditCursor<'a> {
    pub fn new(cursor: &'a mut TextCursor, splice_adder: &'a mut SpliceAdder) -> Self {
        EditCursor(cursor, splice_adder)
    }

    pub fn cursor_range(&self) -> TextRange {
        self.0.range()
    }

    pub fn calibrate_adder(&mut self, splice: &Splice) {
        self.1.calibrate(splice)
    }

    pub fn calibrate_cursor(&mut self, splice: &Splice) {
        self.0.cur.calibrate(splice);
        for opt in [&mut self.0.anchor, &mut self.0.prev] {
            if let Some(anchor) = opt {
                anchor.calibrate(splice);
            }
        }
    }

    /// Tries to merge the `TextCursor`'s associated `Change` with another one.
    ///
    /// Returns `None` if `TextCursor` has no `Change`, and only returns `Some(_)` when the merger
    /// was not successful, so that the old `Change` can be replaced and commited.
    pub(crate) fn try_merge(&mut self, edit: &mut Change) -> Option<Change> {
        if let Some(orig) = &mut self.0.change {
            let orig_taken = orig.splice.taken_range();
            let mut orig_added = orig.splice.added_range();
            let mut edit_taken = edit.splice.taken_range();

            // Case where the original change is intercepted at the end, or the middle.
            if edit_taken.start >= orig_added.start && orig_added.end >= edit_taken.start {
                let splice = &mut orig.splice;
                // If the change also includes part of the original file.
                if edit_taken.end > orig_added.end {
                    // Replace the text of "edit", that's part of "orig"'s added text, with nothing.
                    let on_orig = TextRange { start: edit_taken.start, end: orig_added.end };
                    let mut file_text = edit.taken_text.clone();
                    merge_edit(&mut file_text, &empty_edit(), edit_taken.start, on_orig);

                    // Append the remaining text to the end of "orig"'s taken text.
                    let end_range = TextRange { start: orig_taken.end, end: orig_taken.end };
                    merge_edit(&mut orig.taken_text, &file_text, splice.start, end_range);
                    move_end(&empty_edit(), &file_text, &mut splice.taken_end, end_range);

                    // Change what parts of "orig" are taken out by "edit".
                    edit_taken.end = orig_added.end;
                }
                merge_edit(&mut orig.added_text, &edit.added_text, splice.start, edit_taken);
                move_end(&edit.taken_text, &edit.added_text, &mut splice.added_end, edit_taken);
            // Case where the original change is intercepted at the beginning.
            } else if orig_added.start >= edit_taken.start && edit_taken.end >= orig_added.start {
                let splice = &mut edit.splice;
                merge_edit(&mut edit.taken_text, &orig.taken_text, splice.start, orig_added);
                move_end(&orig.added_text, &orig.taken_text, &mut splice.taken_end, orig_added);
                self.0.change = Some(edit.clone());
            // If the changes don't intercep at all, they cannot be merged.
            } else {
                return self.0.change.replace(edit.clone());
            };

            return None;
        }

        self.0.change.replace(edit.clone())
    }
}

pub struct MoveCursor<'a>(&'a mut TextCursor);

impl<'a> MoveCursor<'a> {
    pub fn new(cursor: &'a mut TextCursor) -> Self {
        MoveCursor(cursor)
    }
    ////////// Public movement functions

    /// Moves the cursor vertically on the file. May also cause horizontal movement.
    pub fn move_ver(&mut self, count: i32, file: &FileWidget<impl Ui>) {
        self.0.move_ver(count, file.text().read().lines(), file.end_node());
    }

    /// Moves the cursor horizontally on the file. May also cause vertical movement.
    pub fn move_hor(&mut self, count: i32, file: &FileWidget<impl Ui>) {
        self.0.move_hor(count, file.text().read().lines(), file.end_node());
    }

    /// Moves the cursor to a position in the file.
    ///
    /// - If the position isn't valid, it will move to the "maximum" position allowed.
    /// - This command sets `desired_x`.
    pub fn move_to(&mut self, pos: TextPos, file: &FileWidget<impl Ui>) {
        self.0.move_to(pos, file.text().read().lines(), file.end_node());
    }

    /// Sets the position of the anchor to be the same as the current cursor position in the file.
    ///
    /// The `anchor` and `current` act as a range of text on the file.
    pub fn set_anchor(&mut self) {
        self.0.anchor = Some(self.0.cur());
    }

    /// Unsets the anchor.
    ///
    /// This is done so the cursor no longer has a valid selection.
    pub fn unset_anchor(&mut self) {
        self.0.anchor = None;
    }
}

// NOTE: This is dependant on the list of cursors being sorted, if it is not, that is UB.
/// A helper, for dealing with recalibration of cursors.
#[derive(Default, Debug)]
pub struct SpliceAdder {
    pub last_pos: TextPos,
    pub lines: isize,
    pub bytes: isize,
    pub cols: isize,
}

impl SpliceAdder {
    // NOTE: It depends on the `Splice`s own calibration by the previous state of `self`.
    /// Calibrates, given a splice.
    pub(crate) fn calibrate(&mut self, splice: &Splice) {
        self.lines += splice.added_end.row as isize - splice.taken_end.row as isize;
        self.bytes += splice.added_end.byte as isize - splice.taken_end.byte as isize;
        let sum = splice.added_end.col as isize - splice.taken_end.col as isize;
        if self.last_pos.row == splice.taken_end.row {
            self.cols += sum;
        } else {
            self.cols = sum;
        }
        self.last_pos = splice.added_end;
    }
}

/// A simple function to add the values of a `SpliceAdder` correctly.
pub fn relative_add(pos: &mut TextPos, splice_adder: &SpliceAdder) {
    pos.row = saturating_add_signed(pos.row, splice_adder.lines);
    pos.byte = saturating_add_signed(pos.byte, splice_adder.bytes);
    if pos.row == splice_adder.last_pos.row {
        pos.col = saturating_add_signed(pos.col, splice_adder.cols);
    }
}

// This function is just a much more efficient way of getting the byte in a position than having to
// add up every line's len, reducing possibly thousands of -- albeit cheap -- operations to usually
// only 30.
// NOTE: It could still be made more efficient.
/// Returns the difference in byte index between two positions in a `Vec<TextLine>`.
///1
/// Returns positive if `target > current`, negative if `target < current`, 0 otherwise.
pub fn get_byte_distance(lines: &[TextLine], prev: TextPos, cur: TextPos) -> isize {
    let mut distance = lines[cur.row].get_line_byte_at(cur.col) as isize;
    distance -= lines[prev.row].get_line_byte_at(prev.col) as isize;

    let (direction, range) = if cur.row > prev.row {
        (1, prev.row..cur.row)
    } else if cur.row < prev.row {
        (-1, cur.row..prev.row)
    } else {
        return distance;
    };

    lines[range].iter().for_each(|l| distance += direction * (l.text().len() as isize));

    distance
}

/// Merges two `String`s into one, given a start and a range to cut off.
fn merge_edit(orig: &mut Vec<String>, edit: &Vec<String>, start: TextPos, range: TextRange) {
    let range = TextRange { start: range.start - start, end: range.end - start };

    let first_line = &orig[range.start.row];
    let first_byte = get_byte_at_col(range.start.col, first_line).unwrap_or(first_line.len());
    let last_line = &orig[range.end.row];
    let last_byte = get_byte_at_col(range.end.col, last_line).unwrap_or(last_line.len());

    if range.lines().count() == 1 && edit.len() == 1 {
        orig[range.start.row].replace_range(first_byte..last_byte, edit[0].as_str());
    } else {
        let first_amend = &orig[range.start.row][..first_byte];
        let last_amend = &orig[range.end.row][last_byte..];

        let mut edit = edit.clone();

        edit.first_mut().unwrap().insert_str(0, first_amend);
        edit.last_mut().unwrap().push_str(last_amend);

        orig.splice(range.lines(), edit);
    }
}

fn move_end(taken: &Vec<String>, added: &Vec<String>, end: &mut TextPos, range: TextRange) {
    if end.row == range.end.row {
        let diff = if added.len() == 1 { range.end.col - range.start.col } else { range.end.col };
        end.col += added.last().unwrap().chars().count() - diff;
    }
    end.row += added.len() - taken.len();
    let edit_len = added.iter().map(|l| l.len()).sum::<usize>();
    let orig_len = taken.iter().map(|l| l.len()).sum::<usize>();
    end.byte += edit_len - orig_len;
}

/// Returns the text in the given range of `TextLine`s.
pub fn get_text_in_range(text: &Vec<TextLine>, range: TextRange) -> Vec<String> {
    let mut lines = Vec::with_capacity(range.lines().count());
    let first_byte = get_byte_at_col(range.start.col, text[range.start.row].text()).unwrap();
    let last_byte = get_byte_at_col(range.end.col, text[range.end.row].text()).unwrap();

    if range.lines().count() == 1 {
        lines.push(text[range.start.row].text()[first_byte..last_byte].to_string());
    } else {
        lines.push(text[range.start.row].text()[first_byte..].to_string());
        for line in text.iter().take(range.end.row - 1).skip(range.start.row + 1) {
            lines.push(line.text().to_string());
        }
        lines.push(text.get(range.end.row).unwrap().text()[..last_byte].to_string());
    }

    lines
}
