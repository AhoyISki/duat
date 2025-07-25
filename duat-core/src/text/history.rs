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
use parking_lot::Mutex;

use super::{Point, Text};
use crate::{add_shifts, merging_range_by_guess_and_lazy_shift};

/// The history of edits, contains all moments
#[derive(Default, Debug)]
pub struct History {
    moments: Vec<Moment>,
    cur_moment: usize,
    new_changes: Option<(Vec<Change>, (usize, [i32; 3]))>,
    /// Used to update ranges on the File
    unproc_changes: Mutex<Option<(Vec<Change>, (usize, [i32; 3]))>>,
    unproc_moments: Mutex<Vec<Moment>>,
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
    pub fn apply_change(&mut self, guess_i: Option<usize>, change: Change) -> usize {
        let (changes, shift_state) = self.unproc_changes.get_mut().get_or_insert_default();
        add_change(changes, guess_i, change.clone(), shift_state);

        let (changes, shift_state) = self.new_changes.get_or_insert_default();
        add_change(changes, guess_i, change, shift_state)
    }

    /// Declares that the current moment is complete and starts a
    /// new one
    pub fn new_moment(&mut self) {
        let Some((mut new_changes, (sh_from, shift))) = self.new_changes.take() else {
            return;
        };
        finish_shifting(&mut new_changes, sh_from, shift);

        if let Some((mut unproc_changes, (sh_from, shift))) = self.unproc_changes.get_mut().take() {
            finish_shifting(&mut unproc_changes, sh_from, shift);
            self.unproc_moments.get_mut().push(Moment {
                changes: Box::leak(Box::from(unproc_changes)),
                is_rev: false,
            });
        }

        self.moments.truncate(self.cur_moment);

        self.moments.push(Moment {
            changes: Box::leak(Box::from(new_changes)),
            is_rev: false,
        });
        self.cur_moment += 1;
    }

    /// Redoes the next [`Moment`], returning its [`Change`]s
    ///
    /// Applying these [`Change`]s in the order that they're given
    /// will result in a correct redoing.
    pub fn move_forward(&mut self) -> Option<Moment> {
        self.new_moment();
        if self.cur_moment == self.moments.len() {
            None
        } else {
            self.cur_moment += 1;

            let moment = self.moments[self.cur_moment - 1];
            self.unproc_moments.get_mut().push(moment);

            Some(moment)
        }
    }

    /// Undoes a [`Moment`], returning its reversed [`Change`]s
    ///
    /// These [`Change`]s will already be shifted corectly, such that
    /// applying them in sequential order, without further
    /// modifications, will result in a correct undoing.
    pub fn move_backwards(&mut self) -> Option<Moment> {
        self.new_moment();
        if self.cur_moment == 0 {
            None
        } else {
            self.cur_moment -= 1;

            let mut moment = self.moments[self.cur_moment];
            moment.is_rev = true;
            self.unproc_moments.get_mut().push(moment);

            Some(moment)
        }
    }

	/// The list of moments that have yet to be processed
    pub(crate) fn unprocessed_moments(&self) -> Vec<Moment> {
        let fresh_moment =
            self.unproc_changes
                .lock()
                .take()
                .map(|(mut changes, (sh_from, shift))| {
                    finish_shifting(&mut changes, sh_from, shift);

                    Moment {
                        changes: Box::leak(Box::from(changes)),
                        is_rev: false,
                    }
                });

        let mut unproc_moments = self.unproc_moments.lock();

        unproc_moments.extend(fresh_moment);

        std::mem::take(&mut unproc_moments)
    }
}

impl Encode for History {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        Encode::encode(&self.moments, encoder)?;
        Encode::encode(&self.cur_moment, encoder)?;
        Encode::encode(&self.new_changes, encoder)?;
        Encode::encode(&*self.unproc_changes.lock(), encoder)?;
        Encode::encode(&*self.unproc_moments.lock(), encoder)?;
        Ok(())
    }
}

impl<Context> Decode<Context> for History {
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        Ok(History {
            moments: Decode::decode(decoder)?,
            cur_moment: Decode::decode(decoder)?,
            new_changes: Decode::decode(decoder)?,
            unproc_changes: Mutex::new(Decode::decode(decoder)?),
            unproc_moments: Mutex::new(Decode::decode(decoder)?),
        })
    }
}

impl Clone for History {
    fn clone(&self) -> Self {
        Self {
            moments: self.moments.clone(),
            cur_moment: self.cur_moment,
            new_changes: self.new_changes.clone(),
            unproc_changes: Mutex::new(self.unproc_changes.lock().clone()),
            unproc_moments: Mutex::new(self.unproc_moments.lock().clone()),
        }
    }
}

/// A moment in history, which may contain changes, or may just
/// contain selections
///
/// It also contains information about how to print the file, so that
/// going back in time is less jarring.
#[derive(Clone, Copy, Default, Debug, Encode)]
pub struct Moment {
    changes: &'static [Change],
    is_rev: bool,
}

impl Moment {
    /// An [`Iterator`] over the [`Change`]s in this [`Moment`]
    ///
    /// These may represent forward or backwards [`Change`]s, forward
    /// for newly introduced [`Change`]s and backwards when undoing.
    pub fn changes(&self) -> impl ExactSizeIterator<Item = Change<&str>> + '_ {
        let mut shift = [0; 3];
        self.changes.iter().map(move |change| {
            if self.is_rev {
                let mut change = change.as_ref().reverse();
                change.shift_by(shift);

                shift = add_shifts(shift, change.shift());

                change
            } else {
                change.as_ref()
            }
        })
    }

    /// Returns the number of [`Change`]s in this [`Moment`]
    ///
    /// Can be `0`, since, in the case of [`Parser`]s, a call for
    /// [`parse`] is always sent, in order to update [`File`]s
    /// every time they are printed.
    ///
    /// [`Parser`]: crate::file::Parser
    /// [`parse`]: crate::file::Parser::parse
    /// [`File`]: crate::file::File
    pub fn len(&self) -> usize {
        self.changes.len()
    }

    /// Wether there are any [`Change`]s in this [`Moment`]
    ///
    /// This can happen when creating a [`Moment::default`].
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<Context> Decode<Context> for Moment {
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        Ok(Moment {
            changes: Vec::decode(decoder)?.leak(),
            is_rev: Decode::decode(decoder)?,
        })
    }
}

/// First try to merge this change with as many changes as
/// possible, then add it in
fn add_change(
    changes: &mut Vec<Change>,
    guess_i: Option<usize>,
    mut change: Change,
    shift_state: &mut (usize, [i32; 3]),
) -> usize {
    let (sh_from, shift) = std::mem::take(shift_state);
    let new_shift = change.shift();

    // The range of changes that will be drained
    let c_range = merging_range_by_guess_and_lazy_shift(
        (changes, changes.len()),
        (guess_i.unwrap_or(0), [change.start(), change.taken_end()]),
        (sh_from, shift, [0; 3], Point::shift_by),
        (Change::start, Change::added_end),
    );

    // If sh_from < c_range.end, I need to shift the changes between the
    // two, so that they match the shifting of the changes before sh_from
    if sh_from < c_range.end && shift != [0; 3] {
        for change in &mut changes[sh_from..c_range.end] {
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
        for change in &mut changes[c_range.end..sh_from] {
            change.shift_by(shift);
        }
    }

    for c in changes.drain(c_range.clone()).rev() {
        change.try_merge(c);
    }

    let changes_taken = c_range.clone().count();
    let new_sh_from = if !(change.added_str() == "" && change.taken_str() == "") {
        changes.insert(c_range.start, change);
        sh_from.saturating_sub(changes_taken).max(c_range.start) + 1
    } else {
        sh_from.saturating_sub(changes_taken).max(c_range.start)
    };
    // If there are no more Changes after this, don't set the shift_state.
    if new_sh_from < changes.len() {
        let shift = add_shifts(shift, new_shift);
        *shift_state = (new_sh_from, shift);
    }

    c_range.start
}

/// A change in a file, with a start, taken text, and added text
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Encode, Decode)]
pub struct Change<S = String> {
    start: Point,
    added: S,
    taken: S,
    added_end: Point,
    taken_end: Point,
}

impl Change {
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

        let taken = text.strs(p0..p1).unwrap().collect();
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
    pub fn str_insert(added_str: &'a str, start: Point) -> Self {
        Self {
            start,
            added: added_str,
            taken: "",
            added_end: start + Point::len_of(added_str),
            taken_end: start,
        }
    }

    pub(super) fn remove_last_nl(len: Point) -> Self {
        Self {
            start: len.rev('\n'),
            added: "",
            taken: "\n",
            added_end: len.rev('\n'),
            taken_end: len,
        }
    }
}

impl<S: std::borrow::Borrow<str>> Change<S> {
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
    pub fn taken_range(&self) -> Range<Point> {
        self.start..self.taken_end()
    }

    /// Returns the added [`Range`]
    pub fn added_range(&self) -> Range<Point> {
        self.start..self.added_end()
    }

    /// The text that was taken on this [`Change`]
    pub fn added_str(&self) -> &str {
        self.added.borrow()
    }

    /// The text that was added by this [`Change`]
    pub fn taken_str(&self) -> &str {
        self.taken.borrow()
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

/// If `lhs` contains the start of `rhs`
fn has_start_of(lhs: Range<Point>, rhs: Range<Point>) -> bool {
    lhs.start <= rhs.start && rhs.start <= lhs.end
}

fn finish_shifting(changes: &mut [Change], sh_from: usize, shift: [i32; 3]) {
    if shift != [0; 3] {
        for change in changes[sh_from..].iter_mut() {
            change.shift_by(shift);
        }
    }
}
