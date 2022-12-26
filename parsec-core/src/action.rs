//! Parsec's way of editing text.
//!
//! This module contains all the operations that deal with editing a file's contents. The edits
//! happen by taking a range of lines from the original file and replacing it by a vector of lines,
//! equivalent to the original set of lines with an edit applied to it. The vast majority of the
//! time, this just involves taking one original line and placing one character on it (typing).
//!
//! This module also deals with the history system and undoing/redoing changes. The history system
//! works like this:
//!
//! Each file's `History` has a list of `Moment`s, and each `Moment` has a list of `Change`s and
//! one `PrintInfo`. `Change`s are splices that contain the original text, the text that
//! was added, their respective ending positions in the file, and a starting position.
//!
//! Whenever you undo a `Moment`, all of its splices are reversed on the file, in an order defined
//! by the starting position of each splice, and the file's `PrintInfo` is updated to the
//! `Moment`'s `PrintInfo`. We change the `PrintInfo` in order to send the user back to the position
//! he was in previously, as he can just look at the same place on the screen for the changes, which
//! I think of as much less jarring.
//!
//! Undoing/redoing `Moment`s also has the effect of moving all `FileCursor`s below the splice's
//! start to a new position, or creating a new `FileCursor` to take a change into effect. This has
//! some interesting implications. Since parsec wants to be able to emulate both vim and kakoune,
//! it needs to be able to adapt to both of its history systems.
//!
//! In vim, if you type text, move around, and type more text, all in insert mode, vim would
//! consider that to be 2 `Moment`s. To fully undo the action, you would have to press `u` twice.
//! Go ahead, try it. Parsec is consistent with this, you could make a history system that
//! considers any cursor movement to be a new `Moment`, and since all `Moment`s would only have 1
//! `Change`, multiple cursors would never happen by undoing/redoing, which is consistent with vim.
//!
//! In kakoune, if you do the same as in vim, and then undo, you will undo both actions at once,
//! and will now have two cursors. Parsec, again, can be consistent with this, you just have to put
//! both `Change`s in a single `Moment`, which is done by default.
//!
//! All this is to say that history management is an editor specific configuration. In vim, any
//! cursor movement should create a new `Moment`, in kakoune, any insertion of text is considered a
//! `Moment`, in most other text editors, a space, tab, new line, or movement, is what creates a
//! `Moment`, which is why `parsec-core` does not define how new moments are created.
use std::{
    cmp::{max, min, Ordering},
    ops::RangeInclusive,
};

use crate::{
    cursor::{get_text_in_range, relative_add, SpliceAdder, TextPos},
    empty_edit,
    file::TextLine,
    get_byte_at_col,
    layout::file_widget::PrintInfo,
    log_info,
};

/// A range in a file, containing rows, columns, and bytes (from the beginning);
#[derive(Debug, Default, Clone, Copy)]
pub struct TextRange {
    pub start: TextPos,
    pub end: TextPos,
}

impl TextRange {
    /// Creates an empty `TextRange` from a single position.
    pub fn empty_at(pos: TextPos) -> Self {
        TextRange { start: pos, end: pos }
    }
    
    /// Returns a range with all the lines involved in the edit.
    pub fn lines(&self) -> RangeInclusive<usize> {
        self.start.row..=self.end.row
    }

    /// Returns true if the other range is contained within this one.
    pub fn contains(&self, other: &TextRange) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    /// Wether or not two `TextRange`s intersect eachother.
    pub fn intersects(&self, other: &TextRange) -> bool {
        (other.start >= self.start && self.end > other.start)
            || (self.start >= other.start && other.end > self.start)
    }

    /// Wether or not `self` intersects the starting position of `other`.
    pub fn at_start(&self, other: &TextRange) -> bool {
        self.start <= other.start && other.start <= self.end
    }

    /// Fuse two ranges into one.
    pub fn merge(&mut self, other: TextRange) {
        self.start = min(self.start, other.start);
        self.end = max(self.end, other.end);
    }

    /// Returns the amount of columns in the last line of the range.
    pub fn last_col_diff(&self) -> usize {
        if self.lines().count() == 1 {
            self.end.col - self.start.col
        } else {
            self.end.col
        }
    }

    /// Returns `Ordering::Equal` if `self.at_start(other)`, otherwise, returns as expected.
    pub fn cross_ord(&self, other: &TextRange) -> Ordering {
        if self.at_start(other) {
            Ordering::Equal
        } else if other.start > self.end {
            Ordering::Greater
        } else {
            Ordering::Less
        }
    }
}

/// A range describing a splice operation;
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Splice {
    /// The start of both texts.
    pub(crate) start: TextPos,
    /// The end of the taken text.
    pub(crate) taken_end: TextPos,
    /// The end of the added text.
    pub(crate) added_end: TextPos,
}

impl Splice {
    ////////////////////////////////
    // Getters
    ////////////////////////////////
    pub fn start(&self) -> TextPos {
        self.start
    }

    pub fn added_end(&self) -> TextPos {
        self.added_end
    }

    pub fn taken_end(&self) -> TextPos {
        self.taken_end
    }

    pub fn added_range(&self) -> TextRange {
        TextRange { start: self.start, end: self.added_end }
    }

    pub fn taken_range(&self) -> TextRange {
        TextRange { start: self.start, end: self.taken_end }
    }

    pub fn calibrate(&mut self, splice: &Splice) {
        for pos in [&mut self.start, &mut self.taken_end, &mut self.added_end] {
            if *pos > splice.start {
                pos.calibrate(splice);
            }
        }
    }

    pub fn calibrate_on_adder(&mut self, splice_adder: &SpliceAdder) {
        for pos in [&mut self.start, &mut self.added_end, &mut self.taken_end] {
            relative_add(pos, &splice_adder);
        }
    }

    /// Returns a reversed version of the `Splice`.
    pub fn reverse(&self) -> Splice {
        Splice { added_end: self.taken_end, taken_end: self.added_end, ..*self }
    }
}

/// A change in a file, empty vectors indicate a pure insertion or deletion.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Change {
    /// The splice involving the two texts.
    pub splice: Splice,

    /// The text that was added in this change.
    pub added_text: Vec<String>,

    /// The text that was replaced in this change.
    pub taken_text: Vec<String>,
}

impl Change {
    pub fn new(lines: &Vec<String>, range: TextRange, text: &Vec<TextLine>) -> Self {
        let mut end = range.start;

        end.row += lines.len() - 1;
        end.byte += lines.iter().map(|l| l.len()).sum::<usize>();
        end.col = if lines.len() == 1 {
            range.start.col + lines[0].chars().count()
        } else {
            lines.last().unwrap().chars().count()
        };

        let taken_text = get_text_in_range(text, range);
        let splice = Splice { start: range.start, taken_end: range.end, added_end: end };

        Change { added_text: lines.clone(), taken_text, splice }
    }

    /// Returns the `TextRange` that was removed.
    pub fn taken_range(&self) -> TextRange {
        self.splice.taken_range()
    }

    /// Returns the `TextRange` that was added.
    pub fn added_range(&self) -> TextRange {
        self.splice.added_range()
    }

    /// Tries to merge the `TextCursor`'s associated `Change` with another one.
    ///
    /// Returns `None` if `TextCursor` has no `Change`, and only returns `Some(_)` when the merger
    /// was not successful, so that the old `Change` can be replaced and commited.
    pub(crate) fn try_merge(&mut self, edit: &Change) -> Result<(), ()> {
        if self.added_range().contains(&edit.taken_range()) {
            self.merge_contained(edit);
        } else if self.added_range().at_start(&edit.taken_range()) {
            self.merge_on_end(edit);
        } else if edit.taken_range().at_start(&self.added_range()) {
            self.merge_on_start(edit);
        } else {
            return Err(());
        }

        Ok(())
    }

    /// Merges a prior `edit`, assuming that it intersects the start of  `self`.
    pub(crate) fn merge_on_start(&mut self, edit: &Change) {
        let intersect = TextRange { start: self.splice.start, end: edit.splice.taken_end };
        splice_text(&mut self.added_text, &edit.added_text, self.splice.start, intersect);

        let mut edit_taken_text = edit.taken_text.clone();
        let empty_range = TextRange::empty_at(self.splice.start);
        splice_text(&mut edit_taken_text, &empty_edit(), edit.splice.start, intersect);
        splice_text(&mut self.taken_text, &edit_taken_text, self.splice.start, empty_range);

        merge_on_splice(&mut self.splice, &edit.splice);
    }

	/// Merges a new `edit`, assuming that it intersects the end of `self`.
    pub(crate) fn merge_on_end(&mut self, edit: &Change) {
        let intersect = TextRange { start: edit.splice.start, end: self.splice.added_end };
        splice_text(&mut self.added_text, &edit.added_text, self.splice.start, intersect);

        let mut edit_taken_text = edit.taken_text.clone();
        let empty_range = TextRange::empty_at(self.splice.taken_end);
        splice_text(&mut edit_taken_text, &empty_edit(), edit.splice.start, intersect);
        log_info(format_args!("{:#?}", edit_taken_text));
        splice_text(&mut self.taken_text, &edit_taken_text, self.splice.start, empty_range);

        merge_on_splice(&mut self.splice, &edit.splice);
    }

	/// Merges a new `edit`, assuming that it is completely contained within `self`.
    pub(crate) fn merge_contained(&mut self, edit: &Change) {
        splice_text(&mut self.added_text, &edit.added_text, self.splice.start, edit.taken_range());
        merge_on_splice(&mut self.splice, &edit.splice);
    }

    /// Merges a prior `edit`, assuming that it is completely contained within `self`.
    pub(crate) fn back_merge_contained(&mut self, edit: &Change) {
        splice_text(&mut self.taken_text, &edit.taken_text, self.splice.start, edit.added_range());
        merge_on_splice(&mut self.splice, &edit.splice);
    }

    /// Merges a prior `edit`, assuming that it intersects the start of `self`.
    pub(crate) fn back_merge_on_start(&mut self, edit: &Change) {
        let intersect = TextRange { start: self.splice.start, end: edit.splice.added_end };
        splice_text(&mut self.taken_text, &edit.taken_text, self.splice.start, intersect);

        let mut edit_added_text = edit.added_text.clone();
        let empty_range = TextRange::empty_at(self.splice.start);
        splice_text(&mut edit_added_text, &empty_edit(), edit.splice.start, intersect);
        splice_text(&mut self.added_text, &edit_added_text, self.splice.start, empty_range);

        merge_on_splice(&mut self.splice, &edit.splice);
    }
}

/// A moment in history, which may contain changes, or may just contain selections.
///
/// It also contains information about how to print the file, so that going back in time is less
/// jaring.
#[derive(Debug, Default)]
pub struct Moment {
    /// Where the file was printed at the time this moment happened.
    pub(crate) print_info: Option<PrintInfo>,
    /// A list of actions, which may be changes, or simply selections of text.
    pub(crate) changes: Vec<Change>,
}

impl Moment {
    /// First try to merge this change with as many changes as possible, then add it in.
    pub fn merge_or_insert(&mut self, mut change: Change) {
        let found_index = try_find_merge(&mut change, &mut self.changes);
        let mut first_index = None;

        // Then, try to merge with previous `Change`s.
        let mut change_iter = self.changes.iter().enumerate().take(found_index).rev();
        while let Some((index, cur_change)) = change_iter.next() {
            if change.taken_range().intersects(&cur_change.added_range()) {
                first_index = Some(index);
            } else {
                break;
            }
        }

        if let Some(first_index) = first_index {
            change.back_merge_on_start(&self.changes[first_index]);
            for old_change in self.changes.iter().take(found_index).skip(first_index + 1) {
                change.back_merge_contained(&old_change);
            }
            self.changes.splice(first_index..found_index, Vec::new());
        }

        self.changes.insert(first_index.unwrap_or(found_index), change);
    }
}

/// The history of edits, contains all moments.
#[derive(Debug)]
pub struct History {
    /// The list of moments in this file's editing history.
    moments: Vec<Moment>,
    /// The currently active moment.
    current_moment: usize,
}

impl History {
    /// Returns a new instance of `History`.
    pub fn new() -> History {
        History { moments: Vec::new(), current_moment: 0 }
    }

    /// Starts a new `Moment` if the `History` is at it's very beginning.
    pub fn start_if_needed(&mut self) {
        if self.current_moment == 0 {
            self.new_moment();
        }
    }

    /// Gets the current moment. Takes time travel into consideration.
    fn current_moment(&mut self) -> Option<&mut Moment> {
        self.moments.get_mut(self.current_moment - 1)
    }

    /// Adds a change to the current `Moment`, or adds it to a new one, if no `Moment`s exist.
    pub fn add_change(&mut self, change: Change) {
        log_info(format_args!("{:#?}", self));
        // Cut off any actions that take place after the current one. We don't really want trees.
        unsafe { self.moments.set_len(self.current_moment) };

        if let Some(moment) = self.current_moment() {
            moment.merge_or_insert(change);
        } else {
            self.new_moment();
            self.moments.last_mut().unwrap().changes.push(change.clone());
        }
    }

    /// Declares that the current moment is complete and moves to the next one.
    pub fn new_moment(&mut self) {
        // If the last moment in history is empty, we can keep using it.
        if self.current_moment().map_or(true, |m| !m.changes.is_empty()) {
            unsafe {
                self.moments.set_len(self.current_moment);
            }
            self.moments.push(Moment { print_info: None, changes: Vec::new() });
            self.current_moment += 1;
        }
    }

    /// Moves forwards in the timeline.
    pub fn move_forward(&mut self) -> Option<&Moment> {
        if self.current_moment == self.moments.len() {
            return None;
        } else {
            self.current_moment += 1;

            return Some(&self.moments[self.current_moment - 1]);
        }
    }

    /// Moves backwards in the timeline.
    pub fn move_backwards(&mut self) -> Option<&Moment> {
        if self.current_moment == 0 {
            None
        } else {
            self.current_moment -= 1;

            Some(&self.moments[self.current_moment])
        }
    }

    /// Sets the `PrintInfo` for the current `Moment`.
    pub fn set_print_info(&mut self, print_info: PrintInfo) {
        if let Some(moment) = self.current_moment() {
            moment.print_info = Some(print_info);
        }
    }
}

// Say you have text like this:
//
// ####$###########
// ########
// ###########§##
//
// And you want to replace the text at the positions $<=x<§ with %%%%%.
// You can just take the first part of the first line, and the last part of the last line and
// insert them on %%%%%. This way, you'll get:
//
// ####%%%%%§##
//
// And your edit is complete.
/// Returns an edit with the leftover original lines appended to it.
pub fn extend_edit(
    old: Vec<String>, mut edit: Vec<String>, range: TextRange,
) -> (Vec<String>, TextRange) {
    let start = range.start;

    let byte = start.byte + edit.iter().map(|l| l.len()).sum::<usize>();
    let last_edit_len = edit.last().unwrap().chars().count();

    // Where the byte of `range.start` is.
    let first_line = old.first().unwrap();
    let first_byte = get_byte(first_line, range.start.col);

    // Inserting the beginning of the first original line into the edit.
    let first_edit_line = edit.first_mut().unwrap();
    first_edit_line.insert_str(0, &first_line[..first_byte]);

    // Where the byte of `range.end` is.
    let last_line = old.last().unwrap();
    let last_byte = get_byte(last_line, range.end.col);

    // Appending the end of the last original line into the edit.
    let edit_len = edit.len();
    let last_edit_line = edit.last_mut().unwrap();
    last_edit_line.push_str(&last_line[last_byte..]);

    let added_range = TextRange {
        start: range.start,
        end: if edit_len == 1 {
            TextPos { row: start.row, byte, col: start.col + last_edit_len }
        } else {
            TextPos { row: start.row + edit.len() - 1, byte, col: last_edit_len }
        },
    };

    // The modified edit is what should be placed in the original vector of lines.
    (edit, added_range)
}

/// Gets the byte where a character starts on a given string.
pub fn get_byte(line: &str, col: usize) -> usize {
    line.char_indices().map(|(b, _)| b).nth(col).unwrap_or(line.len())
}

/// Merges two `String`s into one, given a starting position of the original and a range to cut off.
fn splice_text(orig: &mut Vec<String>, edit: &Vec<String>, start: TextPos, range: TextRange) {
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

/// Returns an index for a `Change` merger, merging if it does.
fn try_find_merge(change: &mut Change, changes: &mut Vec<Change>) -> usize {
    match changes.binary_search_by(|cmp| change.taken_range().cross_ord(&cmp.added_range())) {
        Ok(index) => {
            let mut cur_change = changes.remove(index);
            cur_change.merge_on_start(&change);
            *change = cur_change;

            index
        }
        Err(index) => index,
    }
}

fn merge_on_splice(orig: &mut Splice, edit: &Splice) {
    if orig.added_end.row == edit.taken_end.row {
        orig.taken_end.col += edit.taken_end.col.saturating_sub(orig.added_end.col);
        orig.taken_end.byte += edit.taken_end.byte.saturating_sub(orig.added_end.byte);
    } else if edit.taken_end.row > orig.added_end.row {
        orig.taken_end.col = edit.taken_end.col;
        orig.taken_end.row += edit.taken_end.row - orig.added_end.row;
        orig.taken_end.byte += edit.taken_end.byte - orig.added_end.byte;
    }

    if edit.taken_end > orig.added_end {
        orig.added_end = edit.added_end;
    } else {
        orig.added_end.move_by_range(&edit.added_range());
    }

    orig.start = min(edit.start, orig.start);
}
