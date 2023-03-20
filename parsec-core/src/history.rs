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
use std::{
    cmp::{min, Ordering},
    ops::{Range, RangeBounds},
};

use crate::text::{inner_text::InnerText, PrintInfo};

/// A change in a file, empty vectors indicate a pure insertion or deletion.
#[derive(Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Change {
    /// The splice involving the two texts.
    pub start: usize,
    /// The text that was added in this change.
    pub added_text: String,
    /// The text that was replaced in this change.
    pub taken_text: String,
}

impl Change {
    /// Returns a new [Change].
    pub fn new(edit: impl ToString, range: impl RangeBounds<usize>, backing: &InnerText) -> Self {
        let edit = edit.to_string();
        let start = match range.start_bound() {
            std::ops::Bound::Included(&pos) => pos,
            std::ops::Bound::Excluded(&pos) => pos + 1,
            std::ops::Bound::Unbounded => 0,
        };

        let end = match range.end_bound() {
            std::ops::Bound::Included(&pos) => pos + 1,
            std::ops::Bound::Excluded(&pos) => pos,
            std::ops::Bound::Unbounded => backing.len_chars(),
        };

        let taken_text: String = backing.chars_at(start).take(end - start).collect();

        Change {
            start,
            added_text: edit,
            taken_text,
        }
    }

    /// In this function, it is assumed that [`self`] happened _after_ [`older`][Change].
    fn try_merge(&mut self, mut older: Change) -> Result<(), Change> {
        if older.added_range().contains(&self.start) {
            let removed_end = min(older.added_end(), self.taken_end());
            older
                .added_text
                .replace_range(older.start..removed_end, &self.added_text);
            older.taken_text.push_str(&self.taken_text[removed_end..]);

            *self = older;

            Ok(())
        } else if self.taken_range().contains(&older.start) {
            let removed_end = min(self.taken_end(), older.added_end());
            self.taken_text
                .replace_range(older.start..removed_end, &older.taken_text);
            self.added_text.push_str(&older.added_text[removed_end..]);

            Ok(())
        } else {
            Err(older)
        }
    }

    /// Returns the initial [Range].
    pub fn taken_range(&self) -> Range<usize> {
        self.start..(self.start + self.taken_text.chars().count())
    }

    /// Returns the final [Range].
    pub fn added_range(&self) -> Range<usize> {
        self.start..(self.start + self.added_text.chars().count())
    }

    /// Returns the end of the [Change], before it was applied.
    pub fn taken_end(&self) -> usize {
        self.start + self.taken_text.chars().count()
    }

    /// Returns the end of the [Change], after it was applied.
    pub fn added_end(&self) -> usize {
        self.start + self.added_text.chars().count()
    }

    /// An ordering function that returns [Ordering::Equal] if [cmp][Change] has its end within
    /// [self]'s `added_range`.
    fn contains_ord(&self, cmp: &Change) -> Ordering {
        if self.added_range().contains(&cmp.taken_end()) {
            Ordering::Equal
        } else if cmp.taken_end() > self.added_end() {
            Ordering::Less
        } else {
            Ordering::Greater
        }
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
        let initial_len = self.changes.len();
        let chars_diff = change.added_end() as isize - change.taken_end() as isize;

        let last_index = assoc_index.unwrap_or_else(|| self.find_last_merger(&change));
        let first_index = self
            .find_first_merger(&change, last_index)
            .unwrap_or(last_index);

        if let Some(prev_change) = self.changes.get_mut(last_index) {
            let taken_change = std::mem::take(prev_change);
            let changes_after = if let Err(taken_change) = change.try_merge(taken_change) {
                *prev_change = taken_change;
                self.changes.insert(last_index, change);
                last_index + 1
            } else {
                last_index
            };

            for change in &mut self.changes[changes_after..] {
                change.start = change.start.saturating_add_signed(chars_diff);
            }
        } else {
            self.changes.push(change);
        };

        let prior_changes = self
            .changes
            .drain(first_index..last_index)
            .collect::<Vec<Change>>();
        let added_change = self.changes.get_mut(last_index).unwrap();
        for prior_change in prior_changes {
            let _ = added_change.try_merge(prior_change);
        }

        (
            first_index,
            initial_len as isize - self.changes.len() as isize,
        )
    }

    /// Searches for the first [Change] that can be merged with the one inserted on [last_index].
    fn find_first_merger(&self, change: &Change, last_index: usize) -> Option<usize> {
        let mut change_iter = self.changes.iter().enumerate().take(last_index).rev();
        let mut first_index = None;
        while let Some((index, cur_change)) = change_iter.next() {
            if change.taken_range().contains(&cur_change.added_end()) {
                first_index = Some(index);
            } else {
                break;
            }
        }

        first_index
    }

    /// Finds a [Change] inside of a [Vec<Change>] that intersects another at its end.
    fn find_last_merger(&self, change: &Change) -> usize {
        match self
            .changes
            .binary_search_by(|cmp| cmp.contains_ord(change))
        {
            Ok(index) => index,
            Err(index) => index,
        }
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
        History {
            moments: Vec::new(),
            current_moment: 0,
        }
    }

    /// Gets a mutable reference to the current [Moment], if not at the very beginning.
    fn current_moment_mut(&mut self) -> Option<&mut Moment> {
        if self.current_moment > 0 {
            self.moments.get_mut(self.current_moment - 1)
        } else {
            None
        }
    }

    /// Gets a reference to the current [Moment], if not at the very beginning.
    pub fn current_moment(&self) -> Option<&Moment> {
        if self.current_moment > 0 {
            self.moments.get(self.current_moment - 1)
        } else {
            None
        }
    }

    /// Adds a [Change] to the current [Moment], or adds it to a new one, if no [Moment] exists.
    ///
    /// # Returns
    ///
    /// - The index where the change was inserted;
    /// - The number of changes that were added or subtracted during its insertion.
    pub fn add_change(
        &mut self,
        change: Change,
        assoc_index: Option<usize>,
        print_info: PrintInfo,
    ) -> (usize, isize) {
        // Cut off any actions that take place after the current one. We don't really want trees.
        unsafe { self.moments.set_len(self.current_moment) };

        if let Some(moment) = self.current_moment_mut() {
            moment.ending_print_info = print_info;
            moment.add_change(change, assoc_index)
        } else {
            self.new_moment(print_info);
            self.moments
                .last_mut()
                .unwrap()
                .changes
                .push(change.clone());

            (0, 1)
        }
    }

    /// Declares that the current [Moment] is complete and starts a new one.
    pub fn new_moment(&mut self, print_info: PrintInfo) {
        // If the last moment in history is empty, we can keep using it.
        if self
            .current_moment_mut()
            .map_or(true, |m| !m.changes.is_empty())
        {
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
