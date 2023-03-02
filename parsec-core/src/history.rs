//! Parsec's history system.
//!
//! The history system is comprised of 2 concepts: [Moment]s and [Change]s. A [Moment] contains any
//! number of [Change]s, and by undoing/redoing a [Moment], all of them will be undone/redone. The
//! [History] struct, present in each file, holds the list of [Moment]s in that file's history, and
//! can be moved forwards or backwards in time.
//!
//! Undoing/redoing [Moment]s also has the effect of removing all [Cursor]s and placing new ones
//! where the [Change]s took place.
//!
//! The method by which Parsec's [History] works permits the replication of the history system of
//! many other editors, such as Vim/Neovim, which is strictly one [Change] per [Moment], or
//! Kakoune, where [Moment]s may contain as many [Change]s as is desired.
//!
//! [Cursor]: crate::cursor::Cursor
use std::cmp::min;

use crate::{
    get_byte_at_col,
    position::{get_text_in_range, Pos, Range, SpliceAdder},
    text::{PrintInfo, TextLine},
};

/// An object describing the starting and ending positions of a [Change]. 
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Splice {
    /// The start of both texts.
    pub(crate) start: Pos,
    /// The end of the taken text.
    pub(crate) taken_end: Pos,
    /// The end of the added text.
    pub(crate) added_end: Pos,
}

impl Splice {
    /// The start of the [Splice].
    pub fn start(&self) -> Pos {
        self.start
    }

	/// The final end of the [Splice].
    pub fn added_end(&self) -> Pos {
        self.added_end
    }

	/// The initial end of the [Splice].
    pub fn taken_end(&self) -> Pos {
        self.taken_end
    }

	/// The final [Range] of the [Splice]. 
    pub fn added_range(&self) -> Range {
        Range { start: self.start, end: self.added_end }
    }

	/// The initial [Range] of the [Splice]. 
    pub fn taken_range(&self) -> Range {
        Range { start: self.start, end: self.taken_end }
    }

	/// Calibrates [self] agains a [SpliceAdder].
	#[doc(hidden)]
    pub(crate) fn calibrate_on_adder(&mut self, splice_adder: &SpliceAdder) {
        for pos in [&mut self.start, &mut self.added_end, &mut self.taken_end] {
            pos.calibrate_on_adder(&splice_adder);
        }
    }

    /// Returns a reversed version of the [Splice].
    pub fn reverse(&self) -> Splice {
        Splice { added_end: self.taken_end, taken_end: self.added_end, ..*self }
    }

    /// Merges a new [Splice] into an old one.
    fn merge(&mut self, splice: &Splice) {
        if self.added_end.row == splice.taken_end.row {
            self.taken_end.col += splice.taken_end.col.saturating_sub(self.added_end.col);
            self.taken_end.byte += splice.taken_end.byte.saturating_sub(self.added_end.byte);
        } else if splice.taken_end.row > self.added_end.row {
            self.taken_end.col = splice.taken_end.col;
            self.taken_end.row += splice.taken_end.row - self.added_end.row;
            self.taken_end.byte += splice.taken_end.byte - self.added_end.byte;
        }

        if splice.taken_end >= self.added_end {
            self.added_end = splice.added_end;
        } else {
            self.added_end.calibrate_on_splice(splice);
        }

        self.start = min(splice.start, self.start);
    }
}

/// A change in a file, empty vectors indicate a pure insertion or deletion.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Change {
    /// The splice involving the two texts.
    pub splice: Splice,
    /// The text that was added in this change.
    pub added_text: Vec<String>,
    /// The text that was replaced in this change.
    pub taken_text: Vec<String>,
}

impl Change {
    /// Returns a new [Change].
    pub fn new(lines: &[String], range: Range, text: &Vec<TextLine>) -> Self {
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

        Change { added_text: Vec::from(lines), taken_text, splice }
    }

    /// Merges a new [Change], assuming that it intersects the start of [self].
    fn merge_on_start(&mut self, edit: &Change) {
        let intersect = Range { start: self.splice.start, end: edit.splice.taken_end };
        splice_text(&mut self.added_text, &edit.added_text, self.splice.start, intersect);

        let mut edit_taken_text = edit.taken_text.clone();
        let empty_range = Range::from(self.splice.start);
        splice_text(&mut edit_taken_text, &empty_edit(), edit.splice.start, intersect);
        splice_text(&mut self.taken_text, &edit_taken_text, self.splice.start, empty_range);

        self.splice.merge(&edit.splice);
    }

    /// Merges a new [Change], assuming that it is completely contained within [self].
    fn merge_contained(&mut self, edit: &Change) {
        splice_text(&mut self.added_text, &edit.added_text, self.splice.start, edit.taken_range());
        self.splice.merge(&edit.splice);
    }

    /// Merges a prior [Change], assuming that it is completely contained within [self].
    fn back_merge_contained(&mut self, edit: &Change) {
        splice_text(&mut self.taken_text, &edit.taken_text, self.splice.start, edit.added_range());
        let mut splice = edit.splice;
        splice.merge(&self.splice);
        self.splice = splice;
    }

    /// Merges a prior [Change], assuming that it intersects the start of [self].
    fn back_merge_on_start(&mut self, edit: &Change) {
        let intersect = Range { start: self.splice.start, end: edit.splice.added_end };
        splice_text(&mut self.taken_text, &edit.taken_text, self.splice.start, intersect);

        let mut edit_added_text = edit.added_text.clone();
        let empty_range = Range::from(self.splice.start);
        splice_text(&mut edit_added_text, &empty_edit(), edit.splice.start, intersect);
        splice_text(&mut self.added_text, &edit_added_text, self.splice.start, empty_range);

        let mut splice = edit.splice;
        splice.merge(&self.splice);
        self.splice = splice;
    }

    /// Merges a list of sorted [Change]s into one that overlaps all of them.
    fn merge_list(&mut self, changes: &[Change]) {
        let old_change = &changes[0];

        let start_offset = if old_change.splice.added_range().at_start(&self.splice.taken_range()) {
            1
        } else {
            0
        };
        for old_change in changes.iter().skip(start_offset).rev() {
            self.back_merge_contained(&old_change);
        }

        if start_offset == 1 {
            self.back_merge_on_start(&changes[0]);
        }
    }

    /// Returns the initial [Range].
    pub fn taken_range(&self) -> Range {
        self.splice.taken_range()
    }

    /// Returns the final [Range].
    pub fn added_range(&self) -> Range {
        self.splice.added_range()
    }
}

/// A moment in history, which may contain changes, or may just contain selections.
///
/// It also contains information about how to print the file, so that going back in time is less
/// jaring.
#[derive(Default)]
pub struct Moment {
    /// Where the file was printed at the time this moment started.
    pub(crate) starting_print_info: PrintInfo,
    /// Where the file was printed at the time this moment ended.
    pub(crate) ending_print_info: PrintInfo,
    /// A list of actions, which may be changes, or simply selections of text.
    pub(crate) changes: Vec<Change>,
}

impl Moment {
    /// First try to merge this change with as many changes as possible, then add it in.
    ///
    /// # Returns
    ///
    /// - The index where the change was inserted;
    /// - The number of changes that were added or subtracted during its insertion.
    fn add_change(&mut self, mut change: Change, assoc_index: Option<usize>) -> (usize, isize) {
        let splice_adder = SpliceAdder::new(&change.splice);

        let insertion_index = if let Some(index) = assoc_index {
            if self.changes[index].added_range().end == change.splice.start {
                index + 1
            } else {
                end_or_inner_merge(&mut change, &mut self.changes, index);
                index
            }
        } else {
            let index = find_intersecting_end(&change, &self.changes);
            end_or_inner_merge(&mut change, &mut self.changes, index);
            index
        };
        let merger_index = self.find_first_merger(&change, insertion_index);

        for change in &mut self.changes[insertion_index..] {
            change.splice.calibrate_on_adder(&splice_adder);
        }

        let (insertion_index, change_diff) = if let Some(merger_index) = merger_index {
            change.merge_list(&self.changes[merger_index..insertion_index]);
            self.changes.splice(merger_index..insertion_index, Vec::new());
            (merger_index, merger_index as isize - insertion_index as isize)
        } else {
            (insertion_index, 1)
        };

        self.changes.insert(merger_index.unwrap_or(insertion_index), change);

        (insertion_index, change_diff)
    }

    /// Searches for the first [Change] that can be merged with the one inserted on [last_index].
    fn find_first_merger(&self, change: &Change, last_index: usize) -> Option<usize> {
        let mut change_iter = self.changes.iter().enumerate().take(last_index).rev();
        let mut first_index = None;
        while let Some((index, cur_change)) = change_iter.next() {
            if change.taken_range().intersects(&cur_change.added_range()) {
                first_index = Some(index);
            } else {
                break;
            }
        }

        return first_index;
    }
}

/// The history of edits, contains all moments.
pub struct History {
    /// The list of moments in this file's editing history.
    moments: Vec<Moment>,
    /// The currently active moment.
    current_moment: usize,
}

impl History {
    /// Returns a new instance of [History].
    pub fn new() -> History {
        History { moments: Vec::new(), current_moment: 0 }
    }

    /// Gets a mutable reference to the current [Moment], if not at the very beginning.
    fn current_moment_mut(&mut self) -> Option<&mut Moment> {
        if self.current_moment > 0 { self.moments.get_mut(self.current_moment - 1) } else { None }
    }

    /// Gets a reference to the current [Moment], if not at the very beginning.
    pub fn current_moment(&self) -> Option<&Moment> {
        if self.current_moment > 0 { self.moments.get(self.current_moment - 1) } else { None }
    }

    /// Adds a [Change] to the current [Moment], or adds it to a new one, if no [Moment] exists.
    /// 
    /// # Returns
    ///
    /// - The index where the change was inserted;
    /// - The number of changes that were added or subtracted during its insertion.
    pub fn add_change(
        &mut self, change: Change, assoc_index: Option<usize>, print_info: PrintInfo,
    ) -> (usize, isize) {
        // Cut off any actions that take place after the current one. We don't really want trees.
        unsafe { self.moments.set_len(self.current_moment) };

        if let Some(moment) = self.current_moment_mut() {
            moment.ending_print_info = print_info;
            moment.add_change(change, assoc_index)
        } else {
            self.new_moment(print_info);
            self.moments.last_mut().unwrap().changes.push(change.clone());

            (0, 1)
        }
    }

    /// Declares that the current [Moment] is complete and starts a new one.
    pub fn new_moment(&mut self, print_info: PrintInfo) {
        // If the last moment in history is empty, we can keep using it.
        if self.current_moment_mut().map_or(true, |m| !m.changes.is_empty()) {
            unsafe {
                self.moments.set_len(self.current_moment);
            }
            self.moments.push(Moment {
                starting_print_info: print_info,
                ending_print_info: print_info,
                changes: Vec::new(),
            });
            self.current_moment += 1;
        }
    }

    /// Moves backwards in the [History], returning the last [Moment].
    ///
    /// If The [History] is already at the end, returns [None] instead.
    pub fn move_forward(&mut self) -> Option<&Moment> {
        if self.current_moment == self.moments.len() {
            return None;
        } else {
            if (&self.moments[self.current_moment]).changes.is_empty() {
                None
            } else {
                self.current_moment += 1;
                Some(&self.moments[self.current_moment - 1])
            }
        }
    }

    /// Moves backwards in the [History], returning the last [Moment].
    ///
    /// If The [History] is already at the start, returns [None] instead.
    pub fn move_backwards(&mut self) -> Option<&Moment> {
        if self.current_moment == 0 {
            None
        } else {
            self.current_moment -= 1;

            if (&self.moments[self.current_moment]).changes.is_empty() {
                self.move_backwards()
            } else {
                Some(&self.moments[self.current_moment])
            }
        }
    }
}

/// Merges a [String] into another, through a [Range] shifted by a [Pos].
fn splice_text(orig: &mut Vec<String>, edit: &Vec<String>, start: Pos, range: Range) {
    let range = Range { start: range.start - start, end: range.end - start };

    let first_line = &orig[range.start.row];
    let first_byte = get_byte_at_col(range.start.col, first_line);
    let last_line = &orig[range.end.row];
    let last_byte = get_byte_at_col(range.end.col, last_line);

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

/// Finds a [Change] inside of a [Vec<Change>] that intersects another at its end.
fn find_intersecting_end(change: &Change, changes: &Vec<Change>) -> usize {
    match changes.binary_search_by(|cmp| cmp.added_range().at_end_ord(&change.taken_range())) {
        Ok(index) => index,
        Err(index) => index,
    }
}

/// Tries to merge a [Change] that is intersecting the end or is contained in another.
fn end_or_inner_merge(new_change: &mut Change, changes: &mut Vec<Change>, index: usize) {
    if let Some(old_change) = changes.get_mut(index) {
        if new_change.taken_range().at_start(&old_change.added_range()) {
            let mut old_change = changes.remove(index);
            old_change.merge_on_start(&new_change);
            *new_change = old_change;
        } else if old_change.added_range().contains(&new_change.taken_range()) {
            let mut old_change = changes.remove(index);
            old_change.merge_contained(&new_change);
            *new_change = old_change;
        }
    }
}

/// An empty list of [String]s, representing an empty edit/file.
pub fn empty_edit() -> Vec<String> {
    vec![String::from("")]
}
