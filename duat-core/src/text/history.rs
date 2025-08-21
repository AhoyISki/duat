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
use std::{ops::Range, sync::Arc};

use bincode::{Decode, Encode};
use parking_lot::Mutex;

use super::{Point, Text};
use crate::{add_shifts, merging_range_by_guess_and_lazy_shift};

/// The history of edits, contains all moments
#[derive(Debug)]
pub struct History {
    moments: Vec<Moment>,
    cur_moment: usize,
    remote_moments: Arc<Mutex<Vec<Moment>>>,
}

impl History {
    /// Creates a new [`History`]
    pub fn new() -> Self {
        Self {
            moments: vec![Moment::default()],
            cur_moment: 0,
            remote_moments: Arc::default(),
        }
    }

    /// Adds a [`Change`] to the [`History`]
    pub fn apply_change(&mut self, guess_i: Option<usize>, change: Change) -> usize {
        self.moments.truncate(self.cur_moment + 1);

        let mut remote = self.remote_moments.lock();
        for moment in remote.iter_mut() {
            moment.add_change(guess_i, change.clone());
        }

        self.moments[self.cur_moment].add_change(guess_i, change)
    }

    /// Declares that the current moment is complete and starts a
    /// new one
    pub fn new_moment(&mut self) {
        if !self.moments[self.cur_moment].is_empty() {
            self.moments.truncate(self.cur_moment + 1);
            self.moments.push(Moment::default());
            self.cur_moment += 1;
        }
    }

    /// Redoes the next [`Moment`], returning its [`Change`]s
    ///
    /// Applying these [`Change`]s in the order that they're given
    /// will result in a correct redoing.
    pub(super) fn move_forward(&mut self) -> Option<impl ExactSizeIterator<Item = Change<&str>>> {
        if self.cur_moment == self.moments.len() {
            None
        } else {
            self.cur_moment += 1;

            Some(self.moments[self.cur_moment - 1].changes())
        }
    }

    /// Undoes a [`Moment`], returning its reversed [`Change`]s
    ///
    /// These [`Change`]s will already be shifted corectly, such that
    /// applying them in sequential order, without further
    /// modifications, will result in a correct undoing.
    pub(super) fn move_backwards(&mut self) -> Option<impl ExactSizeIterator<Item = Change<&str>>> {
        if self.cur_moment == 0 {
            None
        } else {
            self.cur_moment -= 1;

            let mut shift = [0; 3];
            Some(self.moments[self.cur_moment].changes().map(move |change| {
                let mut change = change.reverse();
                change.shift_by(shift);
                shift = add_shifts(shift, change.shift());
                change
            }))
        }
    }
}

impl Encode for History {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        Encode::encode(&self.moments, encoder)?;
        Encode::encode(&self.cur_moment, encoder)?;
        Encode::encode(&*self.remote_moments.lock(), encoder)?;
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
            remote_moments: Arc::new(Mutex::new(Decode::decode(decoder)?)),
        })
    }
}

impl Clone for History {
    fn clone(&self) -> Self {
        Self {
            moments: self.moments.clone(),
            cur_moment: self.cur_moment,
            remote_moments: Arc::default(),
        }
    }
}

/// A moment in history, which may contain changes, or may just
/// contain selections
///
/// It also contains information about how to print the file, so that
/// going back in time is less jarring.
#[derive(Clone, Default, Debug, Encode, Decode)]
pub struct Moment {
    changes: Vec<Change>,
    shift_state: (usize, [i32; 3]),
}

impl Moment {
    /// First try to merge this change with as many changes as
    /// possible, then add it in
    fn add_change(&mut self, guess_i: Option<usize>, mut change: Change) -> usize {
        let (from, shift) = self.shift_state;
        let new_shift = change.shift();

        // The range of changes that will be drained
        let m_range = merging_range_by_guess_and_lazy_shift(
            (&self.changes, self.changes.len()),
            (guess_i.unwrap_or(0), [change.start(), change.taken_end()]),
            (from, shift, [0; 3], Point::shift_by),
            (Change::start, Change::added_end),
        );

        // If sh_from < c_range.end, I need to shift the changes between the
        // two, so that they match the shifting of the changes before sh_from
        if from < m_range.end && shift != [0; 3] {
            for change in &mut self.changes[from..m_range.end] {
                change.shift_by(shift);
            }
        // Otherwise, the shifting will happen in reverse, and `from`
        // will be moved backwards until the point where m_range ends.
        // This is better for localized Changes.
        } else if from > m_range.end && new_shift != [0; 3] {
            for change in &mut self.changes[m_range.end..from] {
                change.shift_by([-shift[0], -shift[1], -shift[2]]);
            }
        }

        for c in self.changes.drain(m_range.clone()).rev() {
            change.try_merge(c);
        }

        // Don't add empty Changes
        let (from, shift) = if !(change.added_str() == "" && change.taken_str() == "") {
            let change_shift = change.shift();
            self.changes.insert(m_range.start, change);
            (m_range.start + 1, add_shifts(shift, change_shift))
        } else {
            (m_range.start, shift)
        };

        if from < self.changes.len() {
            self.shift_state = (from, shift);
        } else {
            self.shift_state = (0, [0; 3]);
        }

        m_range.start
    }

    /// An [`Iterator`] over the [`Change`]s in this [`Moment`]
    ///
    /// These may represent forward or backwards [`Change`]s, forward
    /// for newly introduced [`Change`]s and backwards when undoing.
    pub fn changes(&self) -> impl ExactSizeIterator<Item = Change<&str>> + '_ {
        let (from, shift) = self.shift_state;
        self.changes.iter().enumerate().map(move |(i, change)| {
            let mut change = change.as_ref();
            if i > from {
                change.shift_by(shift);
            }
            change
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

/// Can fetch the latest [`Moment`]
///
/// This struct is used to track the [`Change`]s throughout a
/// [`Text`].
pub(crate) struct MomentFetcher {
    change_lists: Arc<Mutex<Vec<Moment>>>,
    index: usize,
}

impl MomentFetcher {
    /// Gets the latest [`Moment`], emptying the list of [`Change`]s
    pub(crate) fn get_moment(&self) -> Moment {
        std::mem::take(&mut self.change_lists.lock()[self.index])
    }

    /// Wether there are any [`Change`]s remaining
    pub(crate) fn is_empty(&self) -> bool {
        self.change_lists.lock()[self.index].is_empty()
    }
}
