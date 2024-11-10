use std::ops::Range;

use super::{Point, Text};
use crate::binary_search_by_key_and_index;

/// The history of edits, contains all moments
#[derive(Default, Debug, Clone)]
pub struct History {
    /// The list of moments in this file's editing history
    moments: Vec<Moment>,
    /// The currently active moment
    current_moment: usize,
}

impl History {
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a [`Change`] to the current moment, or adds it to a new
    /// one, if no moment exists
    pub fn add_change(&mut self, guess_i: Option<usize>, change: Change) -> (usize, i32) {
        let is_last_moment = self.current_moment == self.moments.len();

        // Check, in order to prevent modification of earlier moments.
        let moment = if let Some(moment) = self.moments.last_mut()
            && is_last_moment
        {
            moment
        } else {
            self.new_moment();
            self.moments.last_mut().unwrap()
        };
        moment.add_change(guess_i, change)
    }

    /// Adds a [`Change`] without moving the ones ahead to comply
    ///
    /// # Safety
    ///
    /// This function should only be used by the [`EditHelper`], as it
    /// is expected that, after using it, you will call
    /// [`shift_range`] in order to keep the [`Change`]s ahead synced
    /// with the one being added.
    ///
    /// [`EditHelper`]: crate::mode::EditHelper
    pub unsafe fn add_desync_change(
        &mut self,
        guess_i: usize,
        change: Change,
        shift: (isize, isize, isize),
        sh_from: usize,
    ) -> (usize, i32) {
        let is_last_moment = self.current_moment == self.moments.len();

        // Check, in order to prevent modification of earlier moments.
        let moment = if let Some(moment) = self.moments.last_mut()
            && is_last_moment
        {
            moment
        } else {
            self.new_moment();
            self.moments.last_mut().unwrap()
        };
        moment.add_desync_change(guess_i, change, shift, sh_from)
    }

    /// Declares that the current moment is complete and starts a
    /// new one
    pub fn new_moment(&mut self) {
        // If the last moment in history is empty, we can keep using it.
        if self.moments.last().is_none_or(|m| !m.0.is_empty()) {
            unsafe {
                self.moments.set_len(self.current_moment);
            }
            self.moments.push(Moment(Vec::new()));
            self.current_moment += 1;
        }
    }

    /// Moves backwards in the [History], returning the last moment.
    ///
    /// If The [History] is already at the end, returns [None]
    /// instead.
    pub fn move_forward(&mut self) -> Option<&[Change]> {
        if self.current_moment == self.moments.len()
            || self.moments[self.current_moment].0.is_empty()
        {
            None
        } else {
            self.current_moment += 1;
            Some(&self.moments[self.current_moment - 1].0)
        }
    }

    /// Moves backwards in the [History], returning the last moment.
    ///
    /// If The [History] is already at the start, returns [None]
    /// instead.
    pub fn move_backwards(&mut self) -> Option<&[Change]> {
        if self.current_moment == 0 {
            None
        } else {
            self.current_moment -= 1;

            if self.moments[self.current_moment].0.is_empty() {
                self.move_backwards()
            } else {
                Some(&self.moments[self.current_moment].0)
            }
        }
    }

    pub fn changes_mut(&mut self) -> &mut [Change] {
        let is_last_moment = self.current_moment == self.moments.len();

        // Check, in order to prevent modification of earlier moments.
        if self.moments.last().is_none() || !is_last_moment {
            self.new_moment();
        }

        &mut self.moments.last_mut().unwrap().0
    }
}

/// A moment in history, which may contain changes, or may just
/// contain selections
///
/// It also contains information about how to print the file, so that
/// going back in time is less jaring.
#[derive(Default, Debug, Clone)]
pub struct Moment(Vec<Change>);

impl Moment {
    /// First try to merge this change with as many changes as
    /// possible, then add it in
    ///
    /// # Returns
    ///
    /// - The index where the change was inserted;
    /// - The number of changes that were added or subtracted during
    ///   its insertion.
    fn add_change(&mut self, guess_i: Option<usize>, change: Change) -> (usize, i32) {
        let b = change.added_end().byte() as isize - change.taken_end().byte() as isize;
        let c = change.added_end().char() as isize - change.taken_end().char() as isize;
        let l = change.added_end().line() as isize - change.taken_end().line() as isize;
        unsafe { self.add_change_inner(guess_i, change, (b, c, l), None) }
    }

    unsafe fn add_desync_change(
        &mut self,
        guess_i: usize,
        change: Change,
        shift: (isize, isize, isize),
        sh_from: usize,
    ) -> (usize, i32) {
        self.add_change_inner(Some(guess_i), change, shift, Some(sh_from))
    }

    unsafe fn add_change_inner(
        &mut self,
        guess_i: Option<usize>,
        mut change: Change,
        shift: (isize, isize, isize),
        sh_from: Option<usize>,
    ) -> (usize, i32) {
        // I assume here that if there is no sh_from, then this is not a
        // desync change insertion, so the changes ahead will also be
        // correctly synced, so there is no need to correct them.
        let sh_from = sh_from.unwrap_or(usize::MAX);
        let sh = |n: usize| if sh_from <= n { shift } else { (0, 0, 0) };

        let initial_len = self.0.len();

        let c_i = if let Some(guess_i) = guess_i
            && let Some(c) = self.0.get(guess_i)
            && c.start.shift_by(sh(guess_i)) <= change.start
            && change.start <= c.added_end().shift_by(sh(guess_i))
        {
            guess_i
        } else {
            let f = |i: usize, c: &Change| c.start.shift_by(sh(i));
            match binary_search_by_key_and_index(&self.0, change.start, f) {
                Err(i)
                    if let Some(prev_i) = i.checked_sub(1)
                        && change.start <= self.0[prev_i].added_end().shift_by(sh(prev_i)) =>
                {
                    i - 1
                }
                Ok(i) | Err(i) => i,
            }
        };

        let end_i = if self
            .0
            .get(c_i + 1)
            .is_none_or(|c| change.taken_end() < c.start.shift_by(sh(c_i + 1)))
        {
            c_i
        } else {
            let f = |i: usize, c: &Change| c.start.shift_by(sh(c_i + 1 + i));
            let (Ok(i) | Err(i)) =
                binary_search_by_key_and_index(&self.0[c_i + 1..], change.taken_end(), f);
            c_i + 1 + i
        };

        if let Some(prev_change) = self.0.get_mut(end_i) {
            let mut older = std::mem::take(prev_change);
            let prev_start = older.start;
            older.shift_by(sh(end_i));

            let changes_after = if let Some(mut older) = change.try_merge(older) {
                older.start = prev_start;
                *prev_change = older;
                self.0.insert(end_i, change);
                end_i + 2
            } else {
                *prev_change = change;
                end_i + 1
            };

            if shift != (0, 0, 0) && sh_from == usize::MAX {
                for change in &mut self.0[changes_after..] {
                    change.start = change.start.shift_by(shift)
                }
            }
        } else {
            self.0.insert(end_i, change);
        };

        let prior_changes: Vec<Change> = self.0.drain(c_i..end_i).collect();
        let added_change = self.0.get_mut(c_i).unwrap();
        for (i, mut c) in prior_changes.into_iter().enumerate() {
            c.shift_by(sh(c_i + 1));
            let _ = added_change.try_merge(c);
        }

        (c_i, self.0.len() as i32 - initial_len as i32)
    }
}

/// A change in a file, empty [`String`]s indicate a pure insertion or
/// deletion
#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Change {
    /// The starting `byte` of the [`Change`]
    start: Point,
    /// The text that was added in this change
    added_text: String,
    /// The text that was replaced in this change
    taken_text: String,
}

impl Change {
    /// Returns a new [Change].
    pub fn new(edit: impl ToString, range: (Point, Point), text: &Text) -> Self {
        let added_text = edit.to_string();
        let taken_text: String = text.strs_in_range(range).into_iter().collect();

        Change { start: range.0, added_text, taken_text }
    }

    /// Shifts the [`Change`] by a "signed point"
    pub(crate) fn shift_by(&mut self, shift: (isize, isize, isize)) {
        self.start = self.start.shift_by(shift);
    }

    /// In this function, it is assumed that `self` happened
    /// _after_ `newer`
    ///
    /// If the merger fails, the newer [`Change`] will be returned;
    pub fn try_merge(&mut self, mut older: Change) -> Option<Change> {
        if has_start_of(older.added_range(), self.taken_range()) {
            let fixed_end = older.added_end().min(self.taken_end());

            let start = self.start - older.start;
            let end = fixed_end - older.start;
            older
                .added_text
                .replace_range(start.byte()..end.byte(), &self.added_text);

            older
                .taken_text
                .push_str(&self.taken_text[(fixed_end.byte() - self.start.byte())..]);

            *self = older;

            None
        } else if has_start_of(self.taken_range(), older.added_range()) {
            let fixed_end = self.taken_end().min(older.added_end());

            let start = older.start - self.start;
            let end = fixed_end - self.start;
            self.taken_text
                .replace_range(start.byte()..end.byte(), &older.taken_text);

            self.added_text
                .push_str(&older.added_text[fixed_end.byte() - older.start.byte()..]);

            None
        } else {
            Some(older)
        }
    }

    /// The [`Point`] at the start of the change
    pub fn start(&self) -> Point {
        self.start
    }

    /// Returns the end of the [`Change`], before it was applied
    pub fn taken_end(&self) -> Point {
        self.start + Point::len_of(&self.taken_text)
    }

    /// Returns the end of the [`Change`], after it was applied
    pub fn added_end(&self) -> Point {
        self.start + Point::len_of(&self.added_text)
    }

    /// Returns the taken [`Range`]
    pub fn taken_range(&self) -> Range<usize> {
        self.start.byte()..self.taken_end().byte()
    }

    /// Returns the added [`Range`]
    pub fn added_range(&self) -> Range<usize> {
        self.start.byte()..self.added_end().byte()
    }

    /// The text that was taken on this [`Change`]
    pub fn added_text(&self) -> &str {
        &self.added_text
    }

    /// The text that was added by this [`Change`]
    pub fn taken_text(&self) -> &str {
        &self.taken_text
    }

    /// The difference in chars of the added and taken texts
    pub fn chars_diff(&self) -> isize {
        self.added_text.chars().count() as isize - self.taken_text.chars().count() as isize
    }
}

/// If `lhs` contains the start of`rhs`
fn has_start_of(lhs: Range<usize>, rhs: Range<usize>) -> bool {
    lhs.start <= rhs.start && rhs.start <= lhs.end
}
