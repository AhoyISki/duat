use std::ops::Range;

use super::{Point, PrintCfg, Text};
use crate::{mode::Cursors, ui::Area};

impl Text {
    fn undo_change(&mut self, change: &Change) {
        let start = change.start();
        let end = change.added_end();
        self.replace_range_inner((start, end), change.taken_text());
    }
}

/// The history of edits, contains all moments
#[derive(Default, Debug, Clone)]
pub struct History {
    /// The list of moments in this file's editing history
    moments: Vec<Vec<Change>>,
    /// The currently active moment
    current_moment: usize,
}

impl History {
    pub fn new() -> Self {
        Self::default()
    }

    /// Adds a [Change] to the current moment, or adds it to a new
    /// one, if no moment exists
    pub fn add_change(&mut self, change: Change) {
        let is_last_moment = self.current_moment == self.moments.len();

        // Check, in order to prevent modification of earlier moments.
        if let Some(moment) = self.moments.last_mut()
            && is_last_moment
        {
            moment.push(change);
        } else {
            self.new_moment();
            self.moments.last_mut().unwrap().push(change);
        }
    }

    /// Declares that the current moment is complete and starts a
    /// new one
    pub fn new_moment(&mut self) {
        // If the last moment in history is empty, we can keep using it.
        if self.moments.last().is_none_or(|m| !m.is_empty()) {
            unsafe {
                self.moments.set_len(self.current_moment);
            }
            self.moments.push(Vec::new());
            self.current_moment += 1;
        }
    }

    /// Undoes the last moment, if there was one
    pub fn undo(
        &mut self,
        text: &mut Text,
        area: &impl Area,
        cfg: &PrintCfg,
        cursors: &mut Cursors,
    ) {
        let Some(moment) = self.move_backwards() else {
            return;
        };

        cursors.clear();

        for (i, change) in moment.iter().enumerate().rev() {
            text.undo_change(change);

            cursors.insert_from_parts(change.start, change.taken_text.len(), text, area, cfg, i);
        }
    }

    /// Redoes the last moment in the [`History`], if there is one
    pub fn redo(
        &mut self,
        text: &mut Text,
        area: &impl Area,
        cfg: &PrintCfg,
        cursors: &mut Cursors,
    ) {
        let Some(moment) = self.move_forward() else {
            return;
        };

        cursors.clear();

        for (i, change) in moment.iter().enumerate() {
            text.apply_change(change);

            cursors.insert_from_parts(change.start, change.added_text.len(), text, area, cfg, i);
        }
    }

    /// Moves backwards in the [History], returning the last moment.
    ///
    /// If The [History] is already at the end, returns [None]
    /// instead.
    pub fn move_forward(&mut self) -> Option<&[Change]> {
        if self.current_moment == self.moments.len() || self.moments[self.current_moment].is_empty()
        {
            None
        } else {
            self.current_moment += 1;
            Some(&self.moments[self.current_moment - 1])
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

            if self.moments[self.current_moment].is_empty() {
                self.move_backwards()
            } else {
                Some(&self.moments[self.current_moment])
            }
        }
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
    pub fn new(edit: impl AsRef<str>, range: (Point, Point), text: &Text) -> Self {
        let added_text = edit.as_ref().to_string();
        let taken_text: String = text.strs_in_range(range).into_iter().collect();

        Change { start: range.0, added_text, taken_text }
    }

    /// In this function, it is assumed that `self` happened
    /// _before_ `newer`
    ///
    /// If the merger fails, the newer [`Change`] will be returned;
    pub fn try_merge(&mut self, mut newer: Change) -> Option<Change> {
        if has_start_of(self.added_range(), newer.taken_range()) {
            let fixed_end = self.added_end().min(newer.taken_end());

            let start = newer.start - self.start;
            let end = fixed_end - self.start;
            self.added_text
                .replace_range(start.byte()..end.byte(), &newer.added_text);

            self.taken_text
                .push_str(&newer.taken_text[(fixed_end.byte() - newer.start.byte())..]);

            None
        } else if has_start_of(newer.taken_range(), self.added_range()) {
            let fixed_end = newer.taken_end().min(self.added_end());

            let start = self.start - newer.start;
            let end = fixed_end - newer.start;
            newer
                .taken_text
                .replace_range(start.byte()..end.byte(), &self.taken_text);

            newer
                .added_text
                .push_str(&self.added_text[fixed_end.byte() - self.start.byte()..]);

            *self = newer;

			None
        } else {
            Some(newer)
        }
    }

    /// Shifts a change by a "signed point"
    ///
    /// This is mostly meant to be used by the [`EditHelper`], in
    /// order to shift [`Change`]s while editing earlier [`Cursor`]s
    ///
    /// [`EditHelper`]: crate::mode::EditHelper
    /// [`Cursor`]: crate::mode::Cursor
    pub fn shift_by(&mut self, shift: (isize, isize, isize)) {
        self.start.shift_by(shift);
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
    fn taken_range(&self) -> Range<usize> {
        self.start.byte()..self.taken_end().byte()
    }

    /// Returns the added [`Range`]
    fn added_range(&self) -> Range<usize> {
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
}

/// If `lhs` contains the start of`rhs`
fn has_start_of(lhs: Range<usize>, rhs: Range<usize>) -> bool {
    lhs.contains(&rhs.start) || lhs.end == rhs.start
}
