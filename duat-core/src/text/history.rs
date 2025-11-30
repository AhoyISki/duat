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
use std::{
    marker::PhantomData,
    ops::Range,
    sync::{Arc, Mutex},
};

use bincode::{BorrowDecode, Decode, Encode};

use super::{Point, Text};
use crate::{
    text::Bytes,
    utils::{add_shifts as add, merging_range_by_guess_and_lazy_shift},
};

/// The history of edits, contains all moments
#[derive(Default, Debug)]
pub struct History {
    new_moment: Moment,
    moments: Vec<Moment>,
    cur_moment: usize,
    fetcher_moments: Arc<Mutex<Vec<FetcherState>>>,
    saved_moment: Option<usize>,
}

impl History {
    /// Adds a [`Change`] to the [`History`]
    pub fn apply_change(
        &mut self,
        guess_i: Option<usize>,
        change: Change<'static, String>,
    ) -> usize {
        let mut remote = self.fetcher_moments.lock().unwrap();
        for state in remote.iter_mut() {
            state.add_change(change.clone());
        }

        self.new_moment.add_change(guess_i, change)
    }

    /// Declares that the current moment is complete and starts a
    /// new one
    pub fn new_moment(&mut self) {
        if self.new_moment.is_empty() {
            return;
        }

        self.moments.truncate(self.cur_moment);

        if let Some(saved_moment) = self.saved_moment
            && saved_moment >= self.cur_moment
        {
            self.saved_moment = None;
        }

        self.moments.push(std::mem::take(&mut self.new_moment));
        self.cur_moment += 1;
    }

    /// Redoes the next [`Moment`], returning its [`Change`]s
    ///
    /// Applying these [`Change`]s in the order that they're given
    /// will result in a correct redoing.
    pub(super) fn move_forward(
        &mut self,
    ) -> Option<(impl ExactSizeIterator<Item = Change<'_>>, bool)> {
        self.new_moment();
        if self.cur_moment == self.moments.len() {
            None
        } else {
            let mut remote = self.fetcher_moments.lock().unwrap();
            self.cur_moment += 1;

            let iter = self.moments[self.cur_moment - 1]
                .changes()
                .inspect(move |change| {
                    for state in remote.iter_mut() {
                        state.add_change(change.to_string_change());
                    }
                });

            let is_saved = self.saved_moment.is_some_and(|m| m == self.cur_moment);
            Some((iter, is_saved))
        }
    }

    /// Undoes a [`Moment`], returning its reversed [`Change`]s
    ///
    /// These [`Change`]s will already be shifted corectly, such that
    /// applying them in sequential order, without further
    /// modifications, will result in a correct undoing.
    pub(super) fn move_backwards(
        &mut self,
    ) -> Option<(impl ExactSizeIterator<Item = Change<'_>>, bool)> {
        self.new_moment();
        if self.cur_moment == 0 {
            None
        } else {
            let mut remote = self.fetcher_moments.lock().unwrap();
            self.cur_moment -= 1;

            let mut shift = [0; 3];
            let iter = self.moments[self.cur_moment].changes().map(move |change| {
                let mut change = change.reverse();
                change.shift_by(shift);
                shift = add(shift, change.shift());

                for state in remote.iter_mut() {
                    state.add_change(change.to_string_change());
                }

                change
            });

            let is_saved = self.saved_moment.is_some_and(|m| m == self.cur_moment);
            Some((iter, is_saved))
        }
    }

    /// Returns a new [`MomentFetcher`]
    pub(crate) fn new_fetcher(&self) -> MomentFetcher {
        let mut remote = self.fetcher_moments.lock().unwrap();
        remote.push(FetcherState::Alive(Moment::default()));
        MomentFetcher {
            list: self.fetcher_moments.clone(),
            index: remote.len() - 1,
        }
    }

    /// Prepare this `History` for reloading
    pub(super) fn prepare_for_reloading(&mut self) {
        self.fetcher_moments = Arc::default();
    }

    /// Declares that the current state of the [`Text`] was saved on
    /// disk
    pub(super) fn declare_saved(&mut self) {
        self.saved_moment = Some(self.cur_moment)
    }
}

impl Clone for History {
    fn clone(&self) -> Self {
        Self {
            new_moment: self.new_moment.clone(),
            moments: self.moments.clone(),
            cur_moment: self.cur_moment,
            fetcher_moments: Arc::default(),
            saved_moment: self.saved_moment,
        }
    }
}

/// A moment in history, which may contain changes, or may just
/// contain selections
///
/// It also contains information about how to print the buffer, so
/// that going back in time is less jarring.
#[derive(Clone, Default, Debug, Encode, Decode)]
pub struct Moment {
    changes: Vec<Change<'static, String>>,
    shift_state: (usize, [i32; 3]),
}

impl Moment {
    /// First try to merge this change with as many changes as
    /// possible, then add it in
    fn add_change(&mut self, guess_i: Option<usize>, mut change: Change<'static, String>) -> usize {
        let new_shift = change.shift();
        let (from, shift) = self.shift_state;

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
        } else if from > m_range.end && shift != [0; 3] {
            for change in &mut self.changes[m_range.end..from] {
                change.shift_by(shift.map(|i| -i));
            }
        }

        for c in self.changes.drain(m_range.clone()).rev() {
            change.try_merge(c);
        }

        // Don't add empty Changes
        let from = if change.added_str() != change.taken_str() {
            self.changes.insert(m_range.start, change);
            m_range.start + 1
        } else {
            m_range.start
        };

        if from < self.changes.len() {
            self.shift_state = (from, add(shift, new_shift));
        } else {
            self.shift_state = (0, [0; 3]);
        }

        m_range.start
    }

    /// An [`Iterator`] over the [`Change`]s in this [`Moment`]
    ///
    /// These may represent forward or backwards [`Change`]s, forward
    /// for newly introduced [`Change`]s and backwards when undoing.
    pub fn changes(&self) -> impl ExactSizeIterator<Item = Change<'_>> + '_ {
        let (from, shift) = self.shift_state;
        self.changes.iter().enumerate().map(move |(i, change)| {
            let mut change = change.as_ref();
            if i >= from {
                change.shift_by(shift);
            }
            change
        })
    }

    /// Returns the number of [`Change`]s in this [`Moment`]
    ///
    /// Can be `0`, since, in the case of [`Parser`]s, a call for
    /// [`parse`] is always sent, in order to update [`Buffer`]s
    /// every time they are printed.
    ///
    /// [`Parser`]: crate::buffer::Parser
    /// [`parse`]: crate::buffer::Parser::parse
    /// [`Buffer`]: crate::buffer::Buffer
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

/// A change in a buffer, with a start, taken text, and added text
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Change<'s, S = &'s str> {
    start: [i32; 3],
    added: S,
    taken: S,
    added_end: [i32; 3],
    taken_end: [i32; 3],
    _ghost: PhantomData<&'s str>,
}

impl Change<'static, String> {
    /// Returns a new [Change].
    pub fn new(edit: impl ToString, range: Range<Point>, text: &Text) -> Self {
        let added = {
            let edit = edit.to_string();
            // A '\n' must be kept at the end, no matter what.
            if range.start == text.len() && !edit.ends_with('\n') || range.end == text.len() {
                edit + "\n"
            } else {
                edit
            }
        };

        let taken = text.strs(range.clone()).unwrap().collect();
        let added_end = add(range.start.as_signed(), Point::len_of(&added).as_signed());
        Change {
            start: range.start.as_signed(),
            added,
            taken,
            added_end,
            taken_end: range.end.as_signed(),
            _ghost: PhantomData,
        }
    }

    /// Returns a copyable [`Change`]
    pub fn as_ref(&self) -> Change<'_, &str> {
        Change {
            start: self.start,
            added: &self.added,
            taken: &self.taken,
            added_end: self.added_end,
            taken_end: self.taken_end,
            _ghost: PhantomData,
        }
    }

    /// In this function, it is assumed that `self` happened
    /// _after_ `newer`
    ///
    /// If the merger fails, the older [`Change`] will be returned;
    pub fn try_merge(&mut self, mut older: Self) {
        if has_start_of(older.added_range(), self.taken_range()) {
            let fixed_end = older.added_end().min(self.taken_end());

            let start = sub(self.start, older.start);
            let end = sub(fixed_end.as_signed(), older.start);
            let range = start[0] as usize..end[0] as usize;
            older.added.replace_range(range, &self.added);

            let range = (fixed_end.byte() - self.start[0] as usize)..;

            older.taken.push_str(&self.taken[range]);

            *self = older;
        } else if has_start_of(self.taken_range(), older.added_range()) {
            let fixed_end = self.taken_end().min(older.added_end());

            let start = sub(older.start, self.start);
            let end = sub(fixed_end.as_signed(), self.start);
            let range = start[0] as usize..end[0] as usize;
            self.taken.replace_range(range, &older.taken);

            let range = (fixed_end.byte() - older.start[0] as usize)..;

            self.added.push_str(&older.added[range]);
        } else {
            panic!("Changes chosen that don't interact");
        }
        self.added_end = add(self.start, Point::len_of(&self.added).as_signed());
        self.taken_end = add(self.start, Point::len_of(&self.taken).as_signed());
    }
}

impl<'a> Change<'a> {
    /// Creates a [`Change<String>`] from a [`Change<&str>`]
    pub fn to_string_change(&self) -> Change<'static, String> {
        Change {
            start: self.start,
            added: self.added.to_string(),
            taken: self.taken.to_string(),
            added_end: self.added_end,
            taken_end: self.taken_end,
            _ghost: PhantomData,
        }
    }

    /// Returns a new copyable [`Change`] from an insertion.
    pub fn str_insert(added_str: &'a str, start: Point) -> Self {
        Self {
            start: start.as_signed(),
            added: added_str,
            taken: "",
            added_end: (start + Point::len_of(added_str)).as_signed(),
            taken_end: start.as_signed(),
            _ghost: PhantomData,
        }
    }

    pub(super) fn remove_last_nl(len: Point) -> Change<'static> {
        Change {
            start: len.rev('\n').as_signed(),
            added: "",
            taken: "\n",
            added_end: len.rev('\n').as_signed(),
            taken_end: len.as_signed(),
            _ghost: PhantomData,
        }
    }
}

impl<'s, S: std::borrow::Borrow<str>> Change<'s, S> {
    /// Returns a reversed version of this [`Change`]
    pub fn reverse(self) -> Self {
        Self {
            start: self.start,
            added: self.taken,
            taken: self.added,
            added_end: self.taken_end,
            taken_end: self.added_end,
            _ghost: PhantomData,
        }
    }

    /// Gets a [`Range<Point>`], from the start to the end of the
    /// affected lines
    ///
    /// For example, if you make an edit that transforms lines `1..=3`
    /// to lines `1..=5`, this function will return a [`Range`] that
    /// starts at the beginning of line 1, and ends at the end of line
    /// 5.
    ///
    /// > [!NOTE]
    /// >
    /// > This end of this range will come _after_ the last `\n`,
    /// > which means that, in that example, said point would have a
    /// > [`Point::line`] value equal to 6, _not_ 5, since it
    /// > represents both the end of line 5, and the beginning of line
    /// > 6.
    pub fn line_range(&self, bytes: &Bytes) -> Range<Point> {
        let start = bytes.point_at_line(self.start[2] as usize);
        let end_range = bytes.line_range(self.added_end[2] as usize);
        start..end_range.end
    }

    /// The [`Point`] at the start of the change
    pub fn start(&self) -> Point {
        to_point(self.start)
    }

    /// Returns the end of the [`Change`], before it was applied
    pub fn taken_end(&self) -> Point {
        to_point(self.taken_end)
    }

    /// Returns the end of the [`Change`], after it was applied
    pub fn added_end(&self) -> Point {
        to_point(self.added_end)
    }

    /// Returns the taken [`Range`]
    pub fn taken_range(&self) -> Range<Point> {
        self.start()..self.taken_end()
    }

    /// Returns the added [`Range`]
    pub fn added_range(&self) -> Range<Point> {
        self.start()..self.added_end()
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
            self.added_end[0] - self.taken_end[0],
            self.added_end[1] - self.taken_end[1],
            self.added_end[2] - self.taken_end[2],
        ]
    }

    /// Shifts the [`Change`] by a "signed point"
    pub(crate) fn shift_by(&mut self, shift: [i32; 3]) {
        self.start = add(self.start, shift);
        self.added_end = add(self.added_end, shift);
        self.taken_end = add(self.taken_end, shift);
    }
}

impl Encode for Change<'static, String> {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        Encode::encode(&self.start, encoder)?;
        Encode::encode(&self.added, encoder)?;
        Encode::encode(&self.taken, encoder)?;
        Encode::encode(&self.added_end, encoder)?;
        Encode::encode(&self.taken_end, encoder)?;
        Ok(())
    }
}

impl<Context> Decode<Context> for Change<'static, String> {
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        Ok(Self {
            start: Decode::decode(decoder)?,
            added: Decode::decode(decoder)?,
            taken: Decode::decode(decoder)?,
            added_end: Decode::decode(decoder)?,
            taken_end: Decode::decode(decoder)?,
            _ghost: PhantomData,
        })
    }
}

impl<'de, Context> BorrowDecode<'de, Context> for Change<'static, String> {
    fn borrow_decode<D: bincode::de::BorrowDecoder<'de, Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        Ok(Self {
            start: Decode::decode(decoder)?,
            added: Decode::decode(decoder)?,
            taken: Decode::decode(decoder)?,
            added_end: Decode::decode(decoder)?,
            taken_end: Decode::decode(decoder)?,
            _ghost: PhantomData,
        })
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
#[derive(Debug)]
pub(crate) struct MomentFetcher {
    list: Arc<Mutex<Vec<FetcherState>>>,
    index: usize,
}

impl MomentFetcher {
    /// Gets the latest [`Moment`], emptying the list of [`Change`]s
    pub(crate) fn get_moment(&self) -> Moment {
        self.list.lock().unwrap()[self.index].take().expect(
            "This should only return None if the MomentFetcher has been dropped, at which point \
             calling this function should not be possible.",
        )
    }
}

// This is done in order to prevent unnecessary updates to a Moment
// that will never be read, since its reader is gone.
impl Drop for MomentFetcher {
    fn drop(&mut self) {
        self.list.lock().unwrap()[self.index] = FetcherState::Dead;
    }
}

/// The state of a [`MomentFetcher`], can be alive or dead
///
/// This is used to prevent updates to [`Moment`]s that will never be
/// read.
#[derive(Clone, Debug, Encode, Decode)]
enum FetcherState {
    Alive(Moment),
    Dead,
}

impl FetcherState {
    /// Takes the [`Moment`] from this [`FetcherState`] if it's still
    /// alive, that is
    fn take(&mut self) -> Option<Moment> {
        match self {
            FetcherState::Alive(moment) => Some(std::mem::take(moment)),
            FetcherState::Dead => None,
        }
    }

    /// Adds a [`Change`] to this [`FetcherState`]
    fn add_change(&mut self, change: Change<'static, String>) {
        match self {
            FetcherState::Alive(moment) => {
                moment.add_change(None, change);
            }
            FetcherState::Dead => {}
        }
    }
}

/// Subtracts two `i32` arrays
fn sub(lhs: [i32; 3], rhs: [i32; 3]) -> [i32; 3] {
    [lhs[0] - rhs[0], lhs[1] - rhs[1], lhs[2] - rhs[2]]
}

/// Converts an `[i32; 3]` to a [`Point`]
fn to_point(signed: [i32; 3]) -> Point {
    Point::from_raw(signed[0] as usize, signed[1] as usize, signed[2] as usize)
}
