use std::{
    cmp::{max, min, Ordering},
    fmt::Display, ops::RangeInclusive,
};

use crate::{
    history::{Change, History, Moment, Splice},
    get_byte_at_col, split_string_lines,
    text::{PrintInfo, Text, TextLine},
    ui::{EndNode, Label, Ui},
};

// NOTE: `col` and `line` are line based, while `byte` is file based.
/// A position in a `Vec<String>` (line and character address).
#[derive(Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    pub(crate) byte: usize,
    pub(crate) col: usize,
    pub(crate) row: usize,
}

impl Pos {
    /// Calculates a new `TextPos`, given a target position, in rows and columns.
    pub fn translate(self, lines: &[TextLine], row: usize, col: usize) -> Pos {
        let mut new = Pos { row, col, ..self };
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

    /// The byte (relative to the beginning of the file) of self. Indexed at 1. Intended only
    /// for displaying by the end user. For internal use, see `true_byte()`.
    pub fn byte(&self) -> usize {
        self.byte + 1
    }

    /// The column of self. Indexed at 1. Intended only for displaying by the end user. For
    /// internal use, see `true_col()`.
    pub fn col(&self) -> usize {
        self.col + 1
    }

    /// The row of self. Indexed at 1. Intended only for displaying by the end user. For
    /// internal use, see `true_row()`.
    pub fn row(&self) -> usize {
        self.row + 1
    }

    /// The byte (relative to the beginning of the file) of self. Indexed at 0.
    pub fn true_byte(&self) -> usize {
        self.byte
    }

    /// The column of self. Indexed at 0.
    pub fn true_col(&self) -> usize {
        self.col
    }

    /// The row of self. Indexed at 0.
    pub fn true_row(&self) -> usize {
        self.row
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.col + 1, self.row + 1))
    }
}

impl std::ops::Add for Pos {
    type Output = Pos;

    fn add(self, rhs: Self) -> Self::Output {
        Pos {
            row: self.row + rhs.row,
            byte: self.byte + rhs.byte,
            col: if self.row == rhs.row { self.col + rhs.col } else { self.col },
        }
    }
}

impl std::ops::AddAssign for Pos {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl std::ops::Sub for Pos {
    type Output = Pos;

    fn sub(self, rhs: Self) -> Self::Output {
        Pos {
            row: self.row - rhs.row,
            byte: self.byte - rhs.byte,
            col: if self.row == rhs.row { self.col - rhs.col } else { self.col },
        }
    }
}

impl std::ops::SubAssign for Pos {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs;
    }
}

/// A range in a file, containing rows, columns, and bytes (from the beginning);
#[derive(Default, Clone, Copy)]
pub struct Range {
    pub start: Pos,
    pub end: Pos,
}

impl Range {
    /// Creates an empty [TextRange] from a single position.
    pub fn empty_at(pos: Pos) -> Self {
        Range { start: pos, end: pos }
    }

    /// Returns a range with all the lines involved in the edit.
    pub fn lines(&self) -> RangeInclusive<usize> {
        self.start.row..=self.end.row
    }

    /// Returns true if a given [Change] is contained within [self].
    pub fn contains<R>(&self, other: &R) -> bool
    where
        R: Into<Range> + Copy,
    {
        let range: Range = (*other).into();
        self.start <= range.start && self.end >= range.end
    }

    /// Wether or not two [TextRange]s intersect eachother.
    pub fn intersects(&self, other: &Range) -> bool {
        (other.start >= self.start && self.end >= other.start)
            || (other.end >= self.start && self.end >= other.end)
    }

    /// Wether or not [self] intersects the starting position of another [Change].
    pub fn at_start(&self, other: &Range) -> bool {
        self.start <= other.start && other.start <= self.end && other.end >= self.end
    }

    /// Fuse two ranges into one.
    pub fn merge(&mut self, other: Range) {
        self.start = min(self.start, other.start);
        self.end = max(self.end, other.end);
    }

    /// Returns the amount of columns in the last line of the range.
    pub fn last_col_diff(&self) -> usize {
        if self.lines().count() == 1 { self.end.col - self.start.col } else { self.end.col }
    }

    /// Returns [Ordering::Equal] if [self.at_start(other)], otherwise, returns as expected.
    pub fn at_start_ord(&self, other: &Range) -> Ordering {
        if self.at_start(other) {
            Ordering::Equal
        } else if other.start > self.end {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    }

    /// Returns [Ordering::Equal] if [other.at_start(self)], otherwise, returns as expected.
    pub fn at_end_ord(&self, other: &Range) -> Ordering {
        if other.at_start(self) {
            Ordering::Equal
        } else if other.end > self.end {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    }
}

impl From<Pos> for Range {
    fn from(value: Pos) -> Self {
        Range { start: value, end: value }
    }
}

/// A cursor in the text file. This is an editing cursor, not a printing cursor.
#[derive(Default, Copy)]
pub struct Cursor {
    /// Current position of the cursor in the file.
    caret: Pos,

    /// An anchor for a selection.
    anchor: Option<Pos>,

    /// The index to a `Change` in the current `Moment`, used for greater efficiency.
    pub(crate) assoc_index: Option<usize>,

    /// Column that the cursor wants to be in.
    ///
    /// If the cursor moves to a line that is at least as wide as the desired_col,
    /// it will be placed in the desired_col. If the line is shorter, it will be
    /// placed in the last column of the line.
    desired_x: usize,
}

impl Cursor {
    /// Returns a new instance of `FileCursor`.
    pub fn new<U>(pos: Pos, lines: &[TextLine], end_node: &EndNode<U>) -> Cursor
    where
        U: Ui,
    {
        let line = lines.get(pos.row).unwrap();
        Cursor {
            caret: pos,
            // This should be fine.
            anchor: None,
            assoc_index: None,
            desired_x: line.get_dist_to_col(pos.col, end_node),
        }
    }

    /// Internal vertical movement function.
    pub(crate) fn move_ver<U>(&mut self, count: i32, lines: &Vec<TextLine>, end_node: &EndNode<U>)
    where
        U: Ui,
    {
        let old_target = self.caret;
        let cur = &mut self.caret;

        let line = cur.row;
        cur.row = (line as i32 + count).clamp(0, lines.len() as i32 - 1) as usize;
        let line = &lines[cur.row];

        // In vertical movement, the `desired_x` dictates in what column the cursor will be placed.
        let config = end_node.config();
        cur.col = end_node.label.get_col_at_dist(line.text(), self.desired_x, &config.tab_places);

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        cur.byte = cur.byte.saturating_add_signed(get_byte_distance(lines, old_target, *cur));
    }

    /// Internal horizontal movement function.
    pub(crate) fn move_hor<U>(&mut self, count: i32, lines: &Vec<TextLine>, end_node: &EndNode<U>)
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
        caret.col = if caret.row < lines.len() - 1 {
            col.clamp(0, line.char_count() as i32 - 1) as usize
        } else {
            // The cursor should be able to go one character beyound the end of the text.
            col.clamp(0, line.char_count() as i32) as usize
        };

        // NOTE: Change this to `saturating_sub_signed` once that gets merged.
        caret.byte = caret.byte.saturating_add_signed(get_byte_distance(lines, old_caret, *caret));

        self.desired_x = line.get_dist_to_col::<U>(caret.col, &end_node);
    }

    /// Internal absolute movement function. Assumes that `pos` is not calibrated.
    pub(crate) fn move_to<U>(&mut self, pos: Pos, lines: &Vec<TextLine>, end_node: &EndNode<U>)
    where
        U: Ui,
    {
        let cur = &mut self.caret;

        // TODO: Change this to `saturating_sub_signed` once that gets merged.
        cur.byte = cur.byte.saturating_add_signed(get_byte_distance(lines, *cur, pos));

        cur.row = pos.row.clamp(0, lines.len());
        cur.col = pos.col.clamp(0, lines[cur.row].char_count());

        let line = lines.get(pos.row).unwrap();

        self.desired_x = line.get_dist_to_col::<U>(pos.col, &end_node);
    }

    /// Internal absolute movement function. Assumes that `pos` is a valid position.
    pub(crate) fn move_to_calibrated<U>(
        &mut self, pos: Pos, lines: &Vec<TextLine>, end_node: &EndNode<U>,
    ) where
        U: Ui,
    {
        self.caret = pos;
        let line = lines.get(pos.row).unwrap();

        self.desired_x = line.get_dist_to_col::<U>(pos.col, &end_node);
    }

    /// Returns the range between `target` and `anchor`.
    ///
    /// If `anchor` isn't set, returns an empty range on `target`.
    pub fn range(&self) -> Range {
        let anchor = self.anchor.unwrap_or(self.caret);

        Range { start: min(self.caret, anchor), end: max(self.caret, anchor) }
    }

    /// Returns the cursor's position on the screen.
    pub fn caret(&self) -> Pos {
        self.caret
    }

    /// Calibrates a cursor's positions based on some splice.
    pub(crate) fn calibrate_on_adder(&mut self, splice_adder: &SpliceAdder) {
        self.assoc_index.as_mut().map(|i| i.saturating_add_signed(splice_adder.change_diff));
        self.caret.calibrate_on_adder(splice_adder);
        self.anchor.as_mut().map(|anchor| anchor.calibrate_on_adder(splice_adder));
    }

    /// Merges the `TextCursor`s selection with another `TextRange`.
    pub fn merge(&mut self, range: &Range) {
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

    pub(crate) fn place_anchor(&mut self, pos: Pos) {
        self.anchor = Some(pos);
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

    pub fn anchor(&self) -> Option<Pos> {
        self.anchor
    }

    /// The byte (relative to the beginning of the file) of the caret. Indexed at 1. Intended only
    /// for displaying by the end user. For internal use, see `true_byte()`.
    pub fn byte(&self) -> usize {
        self.caret.byte + 1
    }

    /// The column of the caret. Indexed at 1. Intended only for displaying by the end user. For
    /// internal use, see `true_col()`.
    pub fn col(&self) -> usize {
        self.caret.col + 1
    }

    /// The row of the caret. Indexed at 1. Intended only for displaying by the end user. For
    /// internal use, see `true_row()`.
    pub fn row(&self) -> usize {
        self.caret.row + 1
    }

    /// The byte (relative to the beginning of the file) of the caret. Indexed at 0.
    pub fn true_byte(&self) -> usize {
        self.caret.byte
    }

    /// The column of the caret. Indexed at 0.
    pub fn true_col(&self) -> usize {
        self.caret.col
    }

    /// The row of the caret. Indexed at 0.
    pub fn true_row(&self) -> usize {
        self.caret.row
    }
}

impl Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.caret.row + 1, self.caret.col + 1))
    }
}

impl Clone for Cursor {
    fn clone(&self) -> Self {
        Cursor { desired_x: self.caret.col, assoc_index: None, ..*self }
    }
}

/// A cursor that can edit text in its selection, but can't move the selection in any way.
pub struct Editor<'a, U>
where
    U: Ui,
{
    cursor: &'a mut Cursor,
    text: &'a mut Text<U>,
    end_node: &'a EndNode<U>,
    print_info: Option<PrintInfo>,
    history: Option<&'a mut History>,
    splice_adder: &'a mut SpliceAdder,
}

impl<'a, U> Editor<'a, U>
where
    U: Ui,
{
    /// Returns a new instance of `Editor`.
    pub fn new(
        cursor: &'a mut Cursor, splice_adder: &'a mut SpliceAdder, text: &'a mut Text<U>,
        end_node: &'a EndNode<U>, history: Option<&'a mut History>, print_info: Option<PrintInfo>,
    ) -> Self {
        Self { cursor, splice_adder, text, history, end_node, print_info }
    }

    /// Calibrate the cursor with a `Splice`.
    pub fn set_cursor_on_splice(&mut self, splice: &Splice) {
        let caret = self.cursor.caret();
        if let Some(anchor) = self.cursor.anchor() {
            if anchor > caret {
                self.cursor.place_anchor(splice.added_end);
                self.cursor.move_to_calibrated(splice.start, &self.text.lines(), &self.end_node);
            } else {
                self.cursor.place_anchor(splice.start);
                self.cursor.move_to_calibrated(splice.added_end, self.text.lines(), &self.end_node);
            }
        }
    }

    pub fn calibrate_pos(&self, mut pos: Pos) -> Pos {
        pos.calibrate_on_adder(&self.splice_adder);
        pos
    }

    pub(crate) fn calibrate_end_anchor(&mut self) {
        if let Some(mut anchor) = self.cursor.anchor() {
            if anchor > self.cursor.caret() {
                anchor.calibrate_on_adder(&self.splice_adder);
                self.cursor.place_anchor(anchor);
            }
        }
    }

    pub(crate) fn calibrate_on_adder(&mut self) {
        self.cursor.calibrate_on_adder(&self.splice_adder);
    }

    pub(crate) fn reset_cols(&mut self) {
        self.splice_adder.reset_cols(&self.cursor.range().end);
    }

    /// Replaces the entire selection of the `TextCursor` with new text.
    pub fn replace(&mut self, edit: impl ToString) {
        let lines = split_string_lines(&edit.to_string());
        let change = Change::new(&lines, self.cursor.range(), &self.text.lines());
        let splice = change.splice;

        self.edit(change);

        self.set_cursor_on_splice(&splice);
    }

    /// Inserts new text directly behind the caret.
    pub fn insert(&mut self, edit: impl ToString) {
        let lines = split_string_lines(&edit.to_string());
        let change = Change::new(&lines, Range::from(self.cursor.caret()), &self.text.lines());

        self.edit(change);

        self.calibrate_end_anchor();
    }

    /// Edits the file with a cursor.
    fn edit(&mut self, change: Change) {
        //let added_range = change.added_range();
        self.splice_adder.calibrate(&change.splice);

        self.text.apply_change(&change);

        if let Some(history) = &mut self.history {
            let assoc_index = self.cursor.assoc_index;
            let (insertion_index, change_diff) =
                history.add_change(change, assoc_index, self.print_info.unwrap_or_default());
            self.cursor.assoc_index = Some(insertion_index);
            self.splice_adder.change_diff += change_diff;
        }

        //let max_line = max_line(&self.text, &self.print_info.unwrap_or_default(),
        // &self.end_node); update_range(&mut self.text, added_range, max_line,
        // &self.end_node);
    }
}

/// A cursor that can move and alter the selection, but can't edit the file.
pub struct Mover<'a, U>
where
    U: Ui,
{
    cursor: &'a mut Cursor,
    text: &'a Text<U>,
    end_node: &'a EndNode<U>,
    current_moment: Option<&'a Moment>,
}

impl<'a, U> Mover<'a, U>
where
    U: Ui,
{
    /// Returns a new instance of `Mover`.
    pub fn new(
        cursor: &'a mut Cursor, text: &'a Text<U>, end_node: &'a EndNode<U>,
        current_moment: Option<&'a Moment>,
    ) -> Self {
        Self { cursor, text, end_node, current_moment }
    }

    ////////// Public movement functions

    /// Moves the cursor vertically on the file. May also cause vertical movement.
    pub fn move_ver(&mut self, count: i32) {
        self.cursor.move_ver(count, self.text.lines(), self.end_node);
        if let Some(moment) = self.current_moment {
            self.cursor.change_range_check(moment)
        }
    }

    /// Moves the cursor horizontally on the file. May also cause vertical movement.
    pub fn move_hor(&mut self, count: i32) {
        self.cursor.move_hor(count, self.text.lines(), self.end_node);
        if let Some(moment) = self.current_moment {
            self.cursor.change_range_check(moment)
        }
    }

    /// Moves the cursor to a position in the file.
    ///
    /// - If the position isn't valid, it will move to the "maximum" position allowed.
    /// - This command sets `desired_x`.
    pub fn move_to(&mut self, caret: Pos) {
        self.cursor.move_to(caret, self.text.lines(), self.end_node);
        if let Some(moment) = self.current_moment {
            self.cursor.change_range_check(moment)
        }
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn anchor(&self) -> Option<Pos> {
        self.cursor.anchor
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn caret(&self) -> Pos {
        self.cursor.caret
    }

    /// Returns and takes the anchor of the `TextCursor`.
    pub fn take_anchor(&mut self) -> Option<Pos> {
        self.cursor.anchor.take()
    }

    /// Sets the position of the anchor to be the same as the current cursor position in the file.
    ///
    /// The `anchor` and `current` act as a range of text on the file.
    pub fn set_anchor(&mut self) {
        self.cursor.set_anchor()
    }

    /// Unsets the anchor.
    ///
    /// This is done so the cursor no longer has a valid selection.
    pub fn unset_anchor(&mut self) {
        self.cursor.unset_anchor()
    }

    /// Wether or not the anchor is set.
    pub fn anchor_is_set(&mut self) -> bool {
        self.cursor.anchor.is_some()
    }

    /// Switches the caret and anchor of the `TextCursor`.
    pub fn switch_ends(&mut self) {
        if let Some(anchor) = self.cursor.anchor {
            self.cursor.anchor = Some(self.cursor.caret);
            self.cursor.caret = anchor;
        }
    }

    /// Places the caret at the beginning of the selection.
    pub fn set_caret_on_start(&mut self) {
        let range = self.cursor.range();
        self.cursor.caret = range.start;
        self.cursor.anchor.as_mut().map(|anchor| *anchor = range.end);
    }

    /// Places the caret at the end of the selection.
    pub fn set_caret_on_end(&mut self) {
        let range = self.cursor.range();
        self.cursor.caret = range.end;
        self.cursor.anchor.as_mut().map(|anchor| *anchor = range.start);
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
    pub fn reset_cols(&mut self, start: &Pos) {
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
pub fn get_byte_distance(lines: &[TextLine], old: Pos, new: Pos) -> isize {
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
pub fn get_text_in_range(text: &Vec<TextLine>, range: Range) -> Vec<String> {
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
