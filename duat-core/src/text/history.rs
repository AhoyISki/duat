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

use bincode::{Decode, Encode};

use super::{Point, Text};
use crate::{add_shifts, merging_range_by_guess_and_lazy_shift};

/// The history of edits, contains all moments
#[derive(Default, Debug, Clone, Encode, Decode)]
pub struct History {
    moments: Vec<Moment>,
    cur_moment: usize,
    new_moment: Option<(Moment, (usize, [i32; 3]))>,
    /// Used to update ranges on the File
    unproc_moment: Option<(Moment, (usize, [i32; 3]))>,
}

impl History {
    /// Creates a new [`History`]
    pub fn new() -> Self {
        Self::default()
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
    pub fn apply_change(&mut self, guess_i: Option<usize>, change: Change<String>) -> usize {
        let (moment, shift_state) = self.unproc_moment.get_or_insert_default();
        moment.add_change(guess_i, change.clone(), shift_state);

        let (moment, shift_state) = self.new_moment.get_or_insert_default();
        moment.add_change(guess_i, change, shift_state)
    }

    /// Declares that the current moment is complete and starts a
    /// new one
    pub fn new_moment(&mut self) {
        let Some((mut new_moment, (sh_from, shift))) = self.new_moment.take() else {
            return;
        };
        if shift != [0; 3] {
            for change in new_moment.0[sh_from..].iter_mut() {
                change.shift_by(shift);
            }
        }

        self.moments.truncate(self.cur_moment);
        self.moments.push(new_moment);
        self.cur_moment += 1;
    }

    /// Redoes the next [`Moment`], returning its [`Change`]s
    ///
    /// Applying these [`Change`]s in the order that they're given
    /// will result in a correct redoing.
    pub fn move_forward(&mut self) -> Option<Vec<Change<&str>>> {
        self.new_moment();
        if self.cur_moment == self.moments.len() {
            None
        } else {
            self.cur_moment += 1;
            Some(
                self.moments[self.cur_moment - 1]
                    .0
                    .iter()
                    .map(|c| c.as_ref())
                    .collect(),
            )
        }
    }

    /// Undoes a [`Moment`], returning its reversed [`Change`]s
    ///
    /// These [`Change`]s will already be shifted corectly, such that
    /// applying them in sequential order, without further
    /// modifications, will result in a correct undoing.
    pub fn move_backwards(&mut self) -> Option<Vec<Change<&str>>> {
        self.new_moment();
        if self.cur_moment == 0 {
            None
        } else {
            self.cur_moment -= 1;

            let mut shift = [0; 3];
            let iter = self.moments[self.cur_moment].0.iter().map(move |change| {
                let mut change = change.as_ref();
                change.shift_by(shift);

                shift = add_shifts(shift, change.reverse().shift());

                change.reverse()
            });
            Some(iter.collect())
        }
    }

    pub fn unprocessed_changes(&mut self) -> Option<Vec<Change<String>>> {
        self.unproc_moment
            .take()
            .map(|(mut moment, (sh_from, shift))| {
                if shift != [0; 3] {
                    for change in moment.0[sh_from..].iter_mut() {
                        change.shift_by(shift);
                    }
                }
                moment.0
            })
    }

    pub fn has_unprocessed_changes(&self) -> bool {
        self.unproc_moment.as_ref().is_some()
    }
}

/// A moment in history, which may contain changes, or may just
/// contain selections
///
/// It also contains information about how to print the file, so that
/// going back in time is less jarring.
#[derive(Default, Debug, Clone, Encode, Decode)]
pub struct Moment(Vec<Change<String>>);

impl Moment {
    /// First try to merge this change with as many changes as
    /// possible, then add it in
    fn add_change(
        &mut self,
        guess_i: Option<usize>,
        mut change: Change<String>,
        shift_state: &mut (usize, [i32; 3]),
    ) -> usize {
        let (sh_from, shift) = std::mem::take(shift_state);
        let new_shift = change.shift();

        // The range of changes that will be drained
        let c_range = merging_range_by_guess_and_lazy_shift(
            (&self.0, self.0.len()),
            (guess_i.unwrap_or(0), [change.start(), change.taken_end()]),
            (sh_from, shift, [0; 3], Point::shift_by),
            (Change::start, Change::added_end),
        );

        // If sh_from < c_range.end, I need to shift the changes between the
        // two, so that they match the shifting of the changes before sh_from
        if sh_from < c_range.end && shift != [0; 3] {
            for change in &mut self.0[sh_from..c_range.end] {
                change.shift_by(shift);
            }
        // If sh_from > c_range.end, There are now three shifted
        // states among ranges: The ones before c_range.start, between
        // c_range.end and sh_from, and after c_range.end.
        // I will update the second group so that it is shifted by
        // shift + change.shift(), that way, it will be shifted like
        // the first group.
        } else if sh_from > c_range.end && new_shift != [0; 3] {
            let shift = change.shift();
            for change in &mut self.0[c_range.end..sh_from] {
                change.shift_by(shift);
            }
        }

        for c in self.0.drain(c_range.clone()).rev() {
            change.try_merge(c);
        }

        let changes_taken = c_range.clone().count();
        let new_sh_from = if !(change.added_text() == "" && change.taken_text() == "") {
            self.0.insert(c_range.start, change);
            sh_from.saturating_sub(changes_taken).max(c_range.start) + 1
        } else {
            sh_from.saturating_sub(changes_taken).max(c_range.start)
        };
        // If there are no more Changes after this, don't set the shift_state.
        if new_sh_from < self.0.len() {
            let shift = add_shifts(shift, new_shift);
            *shift_state = (new_sh_from, shift);
        }

        c_range.start
    }

    pub fn changes(&self) -> &[Change<String>] {
        &self.0
    }
}

/// A change in a file, with a start, taken text, and added text
#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Encode, Decode)]
pub struct Change<S: AsRef<str>> {
    start: Point,
    added: S,
    taken: S,
    added_end: Point,
    taken_end: Point,
}

impl Change<String> {
    /// Returns a new [Change].
    pub fn new(edit: impl ToString, [p0, p1]: [Point; 2], text: &Text) -> Self {
        let added = {
            let edit = edit.to_string();
            // A '\n' must be kept at the end, no matter what.
            if p1 == text.len() && !edit.ends_with('\n') {
                edit + "\n"
            } else {
                edit
            }
        };

        let taken = text.strs(p0.byte()..p1.byte()).collect();
        let added_end = p0 + Point::len_of(&added);
        Change {
            start: p0,
            added,
            taken,
            added_end,
            taken_end: p1,
        }
    }

    /// Returns a copyable [`Change`]
    pub fn as_ref(&self) -> Change<&str> {
        Change {
            start: self.start,
            added: &self.added,
            taken: &self.taken,
            added_end: self.added_end,
            taken_end: self.taken_end,
        }
    }

    /// In this function, it is assumed that `self` happened
    /// _after_ `newer`
    ///
    /// If the merger fails, the older [`Change`] will be returned;
    pub fn try_merge(&mut self, mut older: Self) {
        if has_start_of(older.added_range(), self.taken_range()) {
            let fixed_end = older.added_end().min(self.taken_end());

            let start = self.start - older.start;
            let end = fixed_end - older.start;
            let range = start.byte()..end.byte();
            older.added.replace_range(range, &self.added);

            let range = (fixed_end.byte() - self.start.byte())..;
            older.taken.push_str(&self.taken[range]);

            *self = older;
        } else if has_start_of(self.taken_range(), older.added_range()) {
            let fixed_end = self.taken_end().min(older.added_end());

            let start = older.start - self.start;
            let end = fixed_end - self.start;
            let range = start.byte()..end.byte();
            self.taken.replace_range(range, &older.taken);

            let range = (fixed_end.byte() - older.start.byte())..;

            self.added.push_str(&older.added[range]);
        } else {
            unreachable!("Changes chosen that don't interact");
        }
        self.added_end = self.start + Point::len_of(&self.added);
        self.taken_end = self.start + Point::len_of(&self.taken);
    }
}

impl<'a> Change<&'a str> {
    /// Returns a new copyable [`Change`] from an insertion.
    pub fn str_insert(added_text: &'a str, start: Point) -> Self {
        Self {
            start,
            added: added_text,
            taken: "",
            added_end: start + Point::len_of(added_text),
            taken_end: start,
        }
    }

    /// This function should only be used with ghost text and builders
    pub(super) fn remove_nl(p0: Point) -> Self {
        Change {
            start: p0,
            added: "",
            taken: "\n",
            added_end: p0,
            taken_end: p0 + Point::len_of("\n"),
        }
    }
}

impl<S: AsRef<str>> Change<S> {
    /// Returns a reversed version of this [`Change`]
    pub fn reverse(self) -> Change<S> {
        Change {
            start: self.start,
            added: self.taken,
            taken: self.added,
            added_end: self.taken_end,
            taken_end: self.added_end,
        }
    }

    /// The [`Point`] at the start of the change
    pub fn start(&self) -> Point {
        self.start
    }

    /// Shifts the [`Change`] by a "signed point"
    pub(crate) fn shift_by(&mut self, shift: [i32; 3]) {
        self.start = self.start.shift_by(shift);
        self.added_end = self.added_end.shift_by(shift);
        self.taken_end = self.taken_end.shift_by(shift);
    }

    /// Returns the end of the [`Change`], before it was applied
    pub fn taken_end(&self) -> Point {
        self.taken_end
    }

    /// Returns the end of the [`Change`], after it was applied
    pub fn added_end(&self) -> Point {
        self.added_end
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

    /// The total shift caused by this [`Change`]
    pub fn shift(&self) -> [i32; 3] {
        [
            self.added_end().byte() as i32 - self.taken_end().byte() as i32,
            self.added_end().char() as i32 - self.taken_end().char() as i32,
            self.added_end().line() as i32 - self.taken_end().line() as i32,
        ]
    }
}

impl Copy for Change<&str> {}

/// If `lhs` contains the start of `rhs`
fn has_start_of(lhs: Range<usize>, rhs: Range<usize>) -> bool {
    lhs.start <= rhs.start && rhs.start <= lhs.end
}
