//! The history for a [`Text`]
//!
//! The [`History`] is composed of [`Moment`]s, each having a list of
//! [`Change`]s. Whenever you [`undo`]/[`redo`], you are
//! undoing/redoing a whole [`Moment`], with all of its [`Change`]s,
//! all at once.
//!
//! This permits Vim style undoing (one [`Change`] per [`Moment`]) as
//! well as Kakoune style undoing (multiple [`Change`]s per
//! [`Moment`]).
//!
//! [`undo`]: Text::undo
//! [`redo`]: Text::redo
use std::ops::Range;

use super::{Point, Text};
use crate::binary_search_by_key_and_index;

/// The history of edits, contains all moments
#[derive(Default, Debug, Clone)]
pub struct History {
    moments: Vec<Moment>,
    current_moment: usize,
}

impl History {
    /// Creates a new [`History`]
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a [`Change`] to the current moment, or adds it to a new
    /// one, if no moment exists
    pub fn add_change(
        &mut self,
        guess_i: Option<usize>,
        change: Change<String>,
    ) -> (usize, i32, bool) {
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
        change: Change<String>,
        shift: (i32, i32, i32),
        sh_from: usize,
    ) -> (usize, i32, bool) {
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
        let is_last_moment = self.current_moment == self.moments.len();
        // If the last moment in history is empty, we can keep using it.
        if !is_last_moment || self.moments.last().is_none_or(|m| !m.0.is_empty()) {
            self.moments.truncate(self.current_moment);
            self.moments.push(Moment(Vec::new()));
            self.current_moment += 1;
        }
    }

    /// Moves backwards in the [History], returning the last moment.
    ///
    /// If The [History] is already at the end, returns [None]
    /// instead.
    pub fn move_forward(&mut self) -> Option<&[Change<String>]> {
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
    pub fn move_backwards(&mut self) -> Option<&[Change<String>]> {
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

    pub fn changes_mut(&mut self) -> &mut [Change<String>] {
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
/// going back in time is less jarring.
#[derive(Default, Debug, Clone)]
pub struct Moment(Vec<Change<String>>);

impl Moment {
    /// First try to merge this change with as many changes as
    /// possible, then add it in
    ///
    /// # Returns
    ///
    /// - The index where the change was inserted;
    /// - The number of changes that were added or subtracted during
    ///   its insertion.
    fn add_change(&mut self, guess_i: Option<usize>, change: Change<String>) -> (usize, i32, bool) {
        let b = change.added_end().byte() as i32 - change.taken_end().byte() as i32;
        let c = change.added_end().char() as i32 - change.taken_end().char() as i32;
        let l = change.added_end().line() as i32 - change.taken_end().line() as i32;
        unsafe { self.add_change_inner(guess_i, change, (b, c, l), None) }
    }

    /// First try to merge this change with as many changes as
    /// possible, then add it in
    ///
    /// # Safety
    ///
    /// This function, unlike [`add_change`], does not shift the
    /// [`Change`]s ahead of the inserted position, this means that
    /// one must be careful to do it themselves. This is done safely
    /// in the [`EditHelper`].
    ///
    /// # Returns
    ///
    /// - The index where the change was inserted;
    /// - The number of changes that were added or subtracted during
    ///   its insertion.
    ///
    /// [`add_change`]: Moment::add_change
    /// [`EditHelper`]: crate::mode::EditHelper
    unsafe fn add_desync_change(
        &mut self,
        guess_i: usize,
        change: Change<String>,
        shift: (i32, i32, i32),
        sh_from: usize,
    ) -> (usize, i32, bool) {
        self.add_change_inner(Some(guess_i), change, shift, Some(sh_from))
    }

    /// Inner insertion of a [`Change`]
    unsafe fn add_change_inner(
        &mut self,
        guess_i: Option<usize>,
        mut change: Change<String>,
        shift: (i32, i32, i32),
        sh_from: Option<usize>,
    ) -> (usize, i32, bool) {
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
            let f = |i: usize, c: &Change<String>| c.start.shift_by(sh(i));
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
            let f = |i: usize, c: &Change<String>| c.start.shift_by(sh(c_i + 1 + i));
            let (Ok(i) | Err(i)) =
                binary_search_by_key_and_index(&self.0[c_i + 1..], change.taken_end(), f);
            c_i + 1 + i
        };

        let merged_ahead = if let Some(prev_change) = self.0.get_mut(end_i) {
            let mut older = std::mem::take(prev_change);
            let prev_start = older.start;
            older.shift_by(sh(end_i));

            let (changes_after, merged) = if let Some(mut older) = change.try_merge(older) {
                older.start = prev_start;
                *prev_change = older;
                self.0.insert(end_i, change);
                (end_i + 2, false)
            } else {
                *prev_change = change;
                (end_i + 1, true)
            };

            if shift != (0, 0, 0) && sh_from == usize::MAX {
                for change in &mut self.0[changes_after..] {
                    change.start = change.start.shift_by(shift)
                }
            }
            merged
        } else {
            self.0.insert(end_i, change);
            false
        };

        let prior_changes: Vec<Change<String>> = self.0.drain(c_i..end_i).collect();
        let added_change = self.0.get_mut(c_i).unwrap();
        for (i, mut c) in prior_changes.into_iter().enumerate() {
            c.shift_by(sh(c_i + i));
            let _ = added_change.try_merge(c);
        }

        (c_i, self.0.len() as i32 - initial_len as i32, merged_ahead)
    }
}

/// A change in a file, with a start, taken text, and added text
#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Change<S: AsRef<str>> {
    start: Point,
    added: S,
    taken: S,
}

impl Change<String> {
    /// Returns a new [Change].
    pub fn new(edit: impl ToString, (start, end): (Point, Point), text: &Text) -> Self {
        let added = edit.to_string();
        let taken: String = text.strs_in(start.byte()..end.byte()).collect();
        Change { start, added, taken }
    }

    /// Returns a copyable [`Change`]
    pub fn as_ref(&self) -> Change<&str> {
        Change {
            start: self.start,
            added: &self.added,
            taken: &self.taken,
        }
    }

    /// In this function, it is assumed that `self` happened
    /// _after_ `newer`
    ///
    /// If the merger fails, the older [`Change`] will be returned;
    pub fn try_merge(&mut self, mut older: Self) -> Option<Self> {
        if has_start_of(older.added_range(), self.taken_range()) {
            let fixed_end = older.added_end().min(self.taken_end());

            let start = self.start - older.start;
            let end = fixed_end - older.start;
            let range = start.byte() as usize..end.byte() as usize;
            older.added.replace_range(range, &self.added);

            let range = (fixed_end.byte() - self.start.byte()) as usize..;
            older.taken.push_str(&self.taken[range]);

            *self = older;

            None
        } else if has_start_of(self.taken_range(), older.added_range()) {
            let fixed_end = self.taken_end().min(older.added_end());

            let start = older.start - self.start;
            let end = fixed_end - self.start;
            let range = start.byte() as usize..end.byte() as usize;
            self.taken.replace_range(range, &older.taken);

            let range = (fixed_end.byte() - older.start.byte()) as usize..;
            self.added.push_str(&older.added[range]);

            None
        } else {
            Some(older)
        }
    }
}

impl<'a> Change<&'a str> {
    /// Returns a new copyable [`Change`] from an insertion.
    pub fn str_insert(added_text: &'a str, start: Point) -> Self {
        Self { start, added: added_text, taken: "" }
    }
}

impl<S: AsRef<str>> Change<S> {
    /// Returns a reversed version of this [`Change`]
    pub fn reverse(&self) -> Change<&str> {
        Change {
            start: self.start,
            added: self.taken_text(),
            taken: self.added_text(),
        }
    }

    /// The [`Point`] at the start of the change
    pub fn start(&self) -> Point {
        self.start
    }

    /// Shifts the [`Change`] by a "signed point"
    pub(crate) fn shift_by(&mut self, shift: (i32, i32, i32)) {
        self.start = self.start.shift_by(shift);
    }

    /// Returns the end of the [`Change`], before it was applied
    pub fn taken_end(&self) -> Point {
        self.start + Point::len_of(&self.taken)
    }

    /// Returns the end of the [`Change`], after it was applied
    pub fn added_end(&self) -> Point {
        self.start + Point::len_of(&self.added)
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
        self.added.as_ref()
    }

    /// The text that was added by this [`Change`]
    pub fn taken_text(&self) -> &str {
        self.taken.as_ref()
    }

    /// The difference in chars of the added and taken texts
    pub fn chars_diff(&self) -> i32 {
        self.added.as_ref().chars().count() as i32 - self.taken.as_ref().chars().count() as i32
    }
}

impl Copy for Change<&str> {}

/// If `lhs` contains the start of`rhs`
fn has_start_of(lhs: Range<usize>, rhs: Range<usize>) -> bool {
    lhs.start <= rhs.start && rhs.start <= lhs.end
}
