use std::{
    cmp::{max, min},
    fmt::Display,
};

use crate::{
    action::{Moment, Splice, TextRange},
    get_byte_at_col,
    widgets::{file_widget::FileWidget, Widget},
    text::TextLine,
    ui::{EndNode, Ui},
};

// NOTE: `col` and `line` are line based, while `byte` is file based.
/// A position in a `Vec<String>` (line and character address).
#[derive(Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct TextPos {
    pub(crate) byte: usize,
    pub(crate) col: usize,
    pub(crate) row: usize,
}

impl TextPos {
    /// Calculates a new `TextPos`, given a target position, in rows and columns.
    pub fn translate(self, lines: &[TextLine], row: usize, col: usize) -> TextPos {
        let mut new = TextPos { row, col, ..self };
        new.byte = new.byte.saturating_add_signed(get_byte_distance(lines, self, new));
        new
    }

    // NOTE: It assumes that the `TextPos` is not contained in `splice`.
    /// Calibrates a `TextPos`, given a `Splice`.
    pub fn calibrate_on_splice(&mut self, splice: &Splice) {
        let Splice { start, taken_end, added_end } = splice;
        if *self > *start {
            // The column will only change if the `TextPos` is in the same line.
            if self.row == taken_end.row {
                self.col += added_end.col - taken_end.col;
            }

            self.row += added_end.row - taken_end.row;
            self.byte += added_end.byte - taken_end.byte;
        }
    }

    pub fn calibrate_on_adder(&mut self, splice_adder: &SpliceAdder) {
        self.row = self.row.saturating_add_signed(splice_adder.lines);
        self.byte = self.byte.saturating_add_signed(splice_adder.bytes);
        if self.row == splice_adder.last_row {
            self.col = self.col.saturating_add_signed(splice_adder.cols);
        }
    }

    /// The byte (relative to the beginning of the file) of this `TextPos`.
    pub fn byte(&self) -> usize {
        self.byte
    }

    /// The column of this `TextPos`.
    pub fn col(&self) -> usize {
        self.col
    }

    /// The row of this `TextPos`.
    pub fn row(&self) -> usize {
        self.row
    }
}

impl std::fmt::Display for TextPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.col, self.row))
    }
}

impl std::fmt::Debug for TextPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("col: {}, line: {}, byte: {}", self.col, self.row, self.byte))
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
#[derive(Debug, Copy)]
pub struct TextCursor {
    /// Current position of the cursor in the file.
    caret: TextPos,

    /// An anchor for a selection.
    anchor: Option<TextPos>,

    /// The index to a `Change` in the current `Moment`, used for greater efficiency.
    pub(crate) assoc_index: Option<usize>,

    /// Column that the cursor wants to be in.
    ///
    /// If the cursor moves to a line that is at least as wide as the desired_col,
    /// it will be placed in the desired_col. If the line is shorter, it will be
    /// placed in the last column of the line.
    desired_x: usize,
}

impl TextCursor {
    /// Returns a new instance of `FileCursor`.
    pub fn new<U>(pos: TextPos, lines: &[TextLine], node: &EndNode<U>) -> TextCursor
    where
        U: Ui,
    {
        let label = node.label.read();
        let config = node.config().read();
        let line = lines.get(pos.row).unwrap();
        TextCursor {
            caret: pos,
            // This should be fine.
            anchor: None,
            assoc_index: None,
            desired_x: line.get_distance_to_col::<U>(pos.col, &label, &config),
        }
    }

    /// Internal vertical movement function.
    pub(crate) fn move_ver<U>(&mut self, count: i32, lines: &Vec<TextLine>, node: &EndNode<U>)
    where
        U: Ui,
    {
        let old_target = self.caret;
        let cur = &mut self.caret;

        let line = cur.row;
        cur.row = (line as i32 + count).clamp(0, lines.len() as i32 - 1) as usize;
        let line = &lines[cur.row];

        // In vertical movement, the `desired_x` dictates in what column the cursor will be placed.
        let label = node.label.read();
        let config = node.config().read();
        (cur.col, _) = line.get_col_at_distance::<U>(self.desired_x, &label, &config);

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        cur.byte = cur.byte.saturating_add_signed(get_byte_distance(lines, old_target, *cur));
    }

    /// Internal horizontal movement function.
    pub(crate) fn move_hor<U>(&mut self, count: i32, lines: &Vec<TextLine>, node: &EndNode<U>)
    where
        U: Ui,
    {
        let old_caret = self.caret;
        let caret = &mut self.caret;
        let mut col = old_caret.col as i32 + count;

        if count >= 0 && caret.row < lines.len() - 1 {
            let mut line_iter = lines.iter().enumerate().skip(old_caret.row);
            // Subtract line lenghts until a column is within the line's bounds.
            while let Some((index, line)) = line_iter.next() {
                caret.row = index;
                if col < line.char_count() as i32 {
                    break;
                }
                col -= line.char_count() as i32;
            }
        } else if count < 0 && caret.row > 0 {
            let mut line_iter = lines.iter().enumerate().take(old_caret.row).rev();
            // Add line lenghts until the column is positive or equal to 0, making it valid.
            while let Some((index, line)) = line_iter.next() {
                if col >= 0 {
                    break;
                }
                col += line.char_count() as i32;
                caret.row = index;
            }
        }

        let line = lines.get(caret.row).unwrap();
        caret.col = col.clamp(0, line.char_count() as i32 - 1) as usize;

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        caret.byte = caret.byte.saturating_add_signed(get_byte_distance(lines, old_caret, *caret));

        let label = node.label.read();
        let config = node.config().read();
        self.desired_x = line.get_distance_to_col::<U>(caret.col, &label, &config) as usize;
    }

    /// Internal absolute movement function. Assumes that `pos` is not calibrated.
    pub(crate) fn move_to<U>(&mut self, pos: TextPos, lines: &Vec<TextLine>, node: &EndNode<U>)
    where
        U: Ui,
    {
        let cur = &mut self.caret;

        // TODO: Change this to `saturating_sub_signed` once that gets merged.
        cur.byte = cur.byte.saturating_add_signed(get_byte_distance(lines, *cur, pos));

        cur.row = pos.row.clamp(0, lines.len());
        cur.col = pos.col.clamp(0, lines[cur.row].char_count());

        let line = lines.get(pos.row).unwrap();

        let label = node.label.read();
        let config = node.config().read();
        self.desired_x = line.get_distance_to_col::<U>(pos.col, &label, &config);
    }

    /// Internal absolute movement function. Assumes that `pos` is a valid position.
    pub(crate) fn move_to_calibrated<U>(
        &mut self, pos: TextPos, lines: &Vec<TextLine>, node: &EndNode<U>,
    ) where
        U: Ui,
    {
        self.caret = pos;
        let line = lines.get(pos.row).unwrap();

        let label = node.label.read();
        let config = node.config().read();
        self.desired_x = line.get_distance_to_col::<U>(pos.col, &label, &config);
    }

    /// Returns the range between `target` and `anchor`.
    ///
    /// If `anchor` isn't set, returns an empty range on `target`.
    pub fn range(&self) -> TextRange {
        let anchor = self.anchor.unwrap_or(self.caret);

        TextRange { start: min(self.caret, anchor), end: max(self.caret, anchor) }
    }

    /// Returns the cursor's position on the screen.
    pub fn caret(&self) -> TextPos {
        self.caret
    }

    /// Calibrates a cursor's positions based on some splice.
    pub(crate) fn calibrate_on_adder(&mut self, splice_adder: &SpliceAdder) {
        self.assoc_index.as_mut().map(|i| i.saturating_add_signed(splice_adder.change_diff));
        self.caret.calibrate_on_adder(splice_adder);
        self.anchor.as_mut().map(|anchor| anchor.calibrate_on_adder(splice_adder));
    }

    /// Merges the `TextCursor`s selection with another `TextRange`.
    pub fn merge(&mut self, range: &TextRange) {
        if let Some(anchor) = &mut self.anchor {
            if self.caret > *anchor {
                self.caret = max(self.caret, range.end);
                *anchor = min(*anchor, range.start);
            } else {
                *anchor = max(*anchor, range.end);
                self.caret = min(self.caret, range.start);
            }
        } else {
            self.caret = max(self.caret, range.end);
            self.anchor = Some(range.start);
        }
    }

    /// Checks wether or not the `TextCursor` is still intersecting its last `Change`.
    ///
    /// If it is not, dissassociates itself with it.
    pub fn change_range_check(&mut self, moment: &Moment) {
        if let Some(assoc_change) = self.assoc_index {
            if let Some(change) = moment.changes.get(assoc_change) {
                if !change.added_range().intersects(&self.range()) {
                    self.assoc_index = None;
                }
            } else {
                self.assoc_index = None;
            }
        }
    }

    /// Sets the position of the anchor to be the same as the current cursor position in the file.
    ///
    /// The `anchor` and `current` act as a range of text on the file.
    pub fn set_anchor(&mut self) {
        self.anchor = Some(self.caret)
    }

    /// Unsets the anchor.
    ///
    /// This is done so the cursor no longer has a valid selection.
    pub fn unset_anchor(&mut self) {
        self.anchor = None;
    }

    pub fn anchor(&self) -> Option<TextPos> {
        self.anchor
    }

    /// The byte (relative to the beginning of the file) of the caret.
    pub fn byte(&self) -> usize {
        self.caret.byte
    }

    /// The column of the caret.
    pub fn col(&self) -> usize {
        self.caret.col
    }

    /// The row of the caret.
    pub fn row(&self) -> usize {
        self.caret.row
    }
}

impl Display for TextCursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.caret.row, self.caret.col))
    }
}

impl Clone for TextCursor {
    fn clone(&self) -> Self {
        TextCursor { desired_x: self.caret.col, assoc_index: None, ..*self }
    }
}

/// A cursor that can edit text in its selection, but can't move the selection in any way.
#[derive(Debug)]
pub struct Editor<'a> {
    pub cursor: &'a mut TextCursor,
    pub splice_adder: &'a mut SpliceAdder,
}

impl<'a> Editor<'a> {
    /// Returns a new instance of `EditCursor`.
    pub fn new(cursor: &'a mut TextCursor, splice_adder: &'a mut SpliceAdder) -> Self {
        Editor { cursor, splice_adder }
    }

    /// Calibrate the cursor with a `Splice`.
    pub fn set_cursor_on_splice<U>(&mut self, splice: &Splice, file_widget: &FileWidget<U>)
    where
        U: Ui,
    {
        let caret = &mut self.cursor.caret;
        let end_node = &file_widget.end_node().read();
        let text = file_widget.text();
        if let Some(anchor) = &mut self.cursor.anchor {
            if anchor > caret {
                self.cursor.move_to_calibrated(splice.added_end, &text.lines, end_node);
                self.cursor.set_anchor();
                self.cursor.move_to_calibrated(splice.start, &text.lines, end_node);
            } else {
                self.cursor.move_to_calibrated(splice.start, &text.lines, end_node);
                self.cursor.set_anchor();
                self.cursor.move_to_calibrated(splice.added_end, &text.lines, end_node);
            }
        }
    }

    pub fn calibrate_pos(&self, mut pos: TextPos) -> TextPos {
        pos.calibrate_on_adder(&self.splice_adder);
        pos
    }

    pub(crate) fn calibrate_end_anchor(&mut self) {
        if let Some(anchor) = self.cursor.anchor.as_mut() {
            if anchor > &mut self.cursor.caret {
                anchor.calibrate_on_adder(&self.splice_adder);
            }
        }
    }
}

/// A cursor that can move and alter the selection, but can't edit the file.
#[derive(Debug)]
pub struct Mover<'a>(&'a mut TextCursor);

impl<'a> Mover<'a> {
    /// Returns	a new instance of `MoveCursor`.
    pub fn new(cursor: &'a mut TextCursor) -> Self {
        Mover(cursor)
    }
    ////////// Public movement functions

    /// Moves the cursor vertically on the file. May also cause horizontal movement.
    pub fn move_ver(&mut self, count: i32, file: &FileWidget<impl Ui>) {
        self.0.move_ver(count, file.text().lines(), &file.end_node().read());
        if let Some(moment) = file.history().current_moment() {
            self.0.change_range_check(moment)
        }
    }

    /// Moves the cursor horizontally on the file. May also cause vertical movement.
    pub fn move_hor(&mut self, count: i32, file: &FileWidget<impl Ui>) {
        self.0.move_hor(count, file.text().lines(), &file.end_node().read());
        if let Some(moment) = file.history().current_moment() {
            self.0.change_range_check(moment)
        }
    }

    /// Moves the cursor to a position in the file.
    ///
    /// - If the position isn't valid, it will move to the "maximum" position allowed.
    /// - This command sets `desired_x`.
    pub fn move_to(&mut self, caret: TextPos, file: &FileWidget<impl Ui>) {
        self.0.move_to(caret, file.text().lines(), &file.end_node().read());
        if let Some(moment) = file.history().current_moment() {
            self.0.change_range_check(moment)
        }
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn anchor(&self) -> Option<TextPos> {
        self.0.anchor
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn caret(&self) -> TextPos {
        self.0.caret
    }

    /// Returns and takes the anchor of the `TextCursor`.
    pub fn take_anchor(&mut self) -> Option<TextPos> {
        self.0.anchor.take()
    }

    /// Sets the position of the anchor to be the same as the current cursor position in the file.
    ///
    /// The `anchor` and `current` act as a range of text on the file.
    pub fn set_anchor(&mut self) {
        self.0.set_anchor()
    }

    /// Unsets the anchor.
    ///
    /// This is done so the cursor no longer has a valid selection.
    pub fn unset_anchor(&mut self) {
        self.0.unset_anchor()
    }

    /// Wether or not the anchor is set.
    pub fn anchor_is_set(&mut self) -> bool {
        self.0.anchor.is_some()
    }

    /// Switches the caret and anchor of the `TextCursor`.
    pub fn switch_ends(&mut self) {
        if let Some(anchor) = self.0.anchor {
            self.0.anchor = Some(self.0.caret);
            self.0.caret = anchor;
        }
    }

	/// Places the caret at the beginning of the selection.
    pub fn set_caret_on_start(&mut self) {
        let range = self.0.range();
        self.0.caret = range.start;
        self.0.anchor.as_mut().map(|anchor| *anchor = range.end);
    }

	/// Places the caret at the end of the selection.
    pub fn set_caret_on_end(&mut self) {
        let range = self.0.range();
        self.0.caret = range.end;
        self.0.anchor.as_mut().map(|anchor| *anchor = range.start);
    }
}

// NOTE: This is dependant on the list of cursors being sorted, if it is not, that is UB.
/// A helper, for dealing with recalibration of cursors.
#[derive(Default, Debug)]
pub struct SpliceAdder {
    pub lines: isize,
    pub bytes: isize,
    pub cols: isize,
    pub last_row: usize,
    pub change_diff: isize,
}

impl SpliceAdder {
    /// Returns a new instance of `SpliceAdder`, calibrated by a `Splice`.
    pub fn new(splice: &Splice) -> Self {
        let mut splice_adder = SpliceAdder::default();
        splice_adder.calibrate(splice);
        splice_adder
    }

    /// Resets the column change if the row has changed.
    pub fn reset_cols(&mut self, start: &TextPos) {
        if start.row > self.last_row {
            self.cols = 0;
        }
    }

    // NOTE: It depends on the `Splice`s own calibration by the previous state of `self`.
    /// Calibrates, given a splice.
    pub(crate) fn calibrate(&mut self, splice: &Splice) {
        self.lines += splice.added_end.row as isize - splice.taken_end.row as isize;
        self.bytes += splice.added_end.byte as isize - splice.taken_end.byte as isize;
        self.cols += splice.added_end.col as isize - splice.taken_end.col as isize;
        self.last_row = splice.added_end.row;
    }
}

// This function is just a much more efficient way of getting the byte in a position than having to
// add up every line's len, reducing possibly thousands of -- albeit cheap -- operations to usually
// only 30.
// NOTE: It could still be made more efficient.
/// Returns the difference in byte index between two positions in a `Vec<TextLine>`.
///1
/// Returns positive if `new > old`, negative if `new < old`, 0 otherwise.
pub fn get_byte_distance(lines: &[TextLine], old: TextPos, new: TextPos) -> isize {
    let mut distance = lines[new.row].get_line_byte_at(new.col) as isize;
    distance -= lines[old.row].get_line_byte_at(old.col) as isize;

    let (direction, range) = if new.row > old.row {
        (1, old.row..new.row)
    } else if new.row < old.row {
        (-1, new.row..old.row)
    } else {
        return distance;
    };

    lines[range].iter().for_each(|l| distance += direction * (l.text().len() as isize));

    distance
}

/// Returns the text in the given range of `TextLine`s.
pub fn get_text_in_range(text: &Vec<TextLine>, range: TextRange) -> Vec<String> {
    let mut lines = Vec::with_capacity(range.lines().count());
    let first_byte = get_byte_at_col(range.start.col, text[range.start.row].text());
    let last_byte = get_byte_at_col(range.end.col, text[range.end.row].text());

    if range.lines().count() == 1 {
        lines.push(text[range.start.row].text()[first_byte..last_byte].to_string());
    } else {
        lines.push(text[range.start.row].text()[first_byte..].to_string());
        for line in text.iter().take(range.end.row).skip(range.start.row + 1) {
            lines.push(line.text().to_string());
        }
        lines.push(text.get(range.end.row).unwrap().text()[..last_byte].to_string());
    }

    lines
}
