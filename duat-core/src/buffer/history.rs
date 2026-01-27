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
    iter::{Chain, Enumerate},
    marker::PhantomData,
    ops::Range,
    sync::{Arc, Mutex},
};

use bincode::{BorrowDecode, Decode, Encode};
use gap_buf::GapBuffer;

use super::{Point, Text};
use crate::{
    Ranges,
    buffer::{Buffer, BufferId},
    mode::Selections,
    text::{Bytes, Tags, TextRange},
    ui::Widget,
    utils::{add_shifts as add, merging_range_by_guess_and_lazy_shift},
};

/// The history of edits, contains all moments
#[derive(Debug)]
pub struct History {
    // Moments in regard to undoing/redoing
    new_moment: Moment,
    undo_redo_moments: Vec<Moment>,
    cur_moment: usize,
    // Moments in regard to BufferTrackers
    new_tracked_moment: Moment,
    tracked_moments: Vec<MomentOrTracking>,
    track_id: TrackId,
    // The moment where a [`Buffer`] was saved
    //
    // [`Buffer`]: crate::buffer::Buffer
    saved_moment: Option<usize>,
}

impl History {
    /// Returns a new `History`
    #[allow(clippy::new_without_default)]
    pub const fn new() -> Self {
        Self {
            new_moment: Moment::new(),
            undo_redo_moments: Vec::new(),
            cur_moment: 0,
            new_tracked_moment: Moment::new(),
            tracked_moments: Vec::new(),
            track_id: TrackId(0),
            saved_moment: None,
        }
    }

    /// Adds a [`Change`] to the [`History`]
    pub fn apply_change(
        &mut self,
        guess_i: Option<usize>,
        change: Change<'static, String>,
    ) -> usize {
        self.new_tracked_moment.add_change(guess_i, change.clone());
        self.new_moment.add_change(guess_i, change)
    }

    /// Declares that the current moment is complete and starts a
    /// new one
    pub fn new_moment(&mut self) {
        if self.new_moment.is_empty() {
            return;
        }

        self.undo_redo_moments.truncate(self.cur_moment);

        if let Some(saved_moment) = self.saved_moment
            && saved_moment >= self.cur_moment
        {
            self.saved_moment = None;
        }

        self.undo_redo_moments
            .push(std::mem::take(&mut self.new_moment));
        self.cur_moment += 1;
    }

    /// Redoes the next [`Moment`], returning its [`Change`]s
    ///
    /// Applying these [`Change`]s in the order that they're given
    /// will result in a correct redoing.
    pub(crate) fn move_forward(
        &mut self,
    ) -> Option<(impl ExactSizeIterator<Item = Change<'_>>, bool)> {
        self.new_moment();
        if self.cur_moment == self.undo_redo_moments.len() {
            None
        } else {
            self.cur_moment += 1;

            let iter = self.undo_redo_moments[self.cur_moment - 1]
                .iter()
                .enumerate()
                .map(|(i, change)| {
                    self.new_tracked_moment
                        .add_change(Some(i), change.to_string_change());
                    change
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
    pub(crate) fn move_backwards(
        &mut self,
    ) -> Option<(impl ExactSizeIterator<Item = Change<'_>>, bool)> {
        self.new_moment();
        if self.cur_moment == 0 {
            None
        } else {
            self.cur_moment -= 1;

            let new_fetched_moment = &mut self.new_tracked_moment;
            let iter = self.undo_redo_moments[self.cur_moment]
                .iter()
                .undone()
                .enumerate()
                .map(move |(i, change)| {
                    new_fetched_moment.add_change(Some(i), change.to_string_change());
                    change
                });

            let is_saved = self.saved_moment.is_some_and(|m| m == self.cur_moment);
            Some((iter, is_saved))
        }
    }

    /// Declares that the current state of the [`Text`] was saved on
    /// disk
    pub(super) fn declare_saved(&mut self) {
        self.saved_moment = Some(self.cur_moment)
    }

    fn get_latest_track_id(&mut self) -> TrackId {
        if !self.new_tracked_moment.is_empty() {
            let track_id = self.track_id.next();
            self.tracked_moments.extend([
                MomentOrTracking::Moment(std::mem::take(&mut self.new_tracked_moment)),
                MomentOrTracking::Tracking(1, track_id),
            ]);
            track_id
        } else if let Some(m_or_t) = self.tracked_moments.last_mut() {
            let MomentOrTracking::Tracking(count, track_id) = m_or_t else {
                panic!("Not supposed to happen dawg");
            };
            *count += 1;
            *track_id
        } else {
            let track_id = self.track_id.next();
            self.tracked_moments
                .push(MomentOrTracking::Tracking(1, track_id));
            track_id
        }
    }

    fn get_untracked_moments_for(&mut self, track_id: TrackId) -> &[MomentOrTracking] {
        let mut prev_tracker_i = None;
        let (i, tracker_count) = self
            .tracked_moments
            .iter_mut()
            .enumerate()
            .find_map(|(i, m_or_t)| match m_or_t {
                MomentOrTracking::Tracking(count, id) if *id == track_id => Some((i, count)),
                MomentOrTracking::Tracking(..) => {
                    prev_tracker_i = Some(i);
                    None
                }
                _ => None,
            })
            .unwrap();

        *tracker_count -= 1;

        if *tracker_count == 0 {
            if let Some(prev_tracker_i) = prev_tracker_i {
                self.tracked_moments.remove(i);

                // Since moments pile up very fast on this list, it is best to merge
                // the future moments of laggard trackers.
                let moment = self.tracked_moments.drain((prev_tracker_i + 1)..i).fold(
                    Moment::default(),
                    |mut moment, m_or_t| {
                        let MomentOrTracking::Moment(new_moment) = m_or_t else {
                            unreachable!();
                        };

                        let (from, shift) = new_moment.shift_state;

                        for (i, mut change) in new_moment.changes.into_iter().enumerate() {
                            change.shift_by(if i >= from { shift } else { [0; 3] });
                            moment.add_change(Some(i), change);
                        }

                        moment
                    },
                );

                let new_i = prev_tracker_i + 1;
                self.tracked_moments
                    .insert(new_i, MomentOrTracking::Moment(moment));

                &self.tracked_moments[new_i + 1..]
            } else {
                // No trackers care about prior changes, so just get rid of them.
                _ = self.tracked_moments.drain(..i + 1);
                &self.tracked_moments
            }
        } else {
            &self.tracked_moments[i + 1..]
        }
    }
}

impl Clone for History {
    fn clone(&self) -> Self {
        Self {
            new_moment: self.new_moment.clone(),
            undo_redo_moments: self.undo_redo_moments.clone(),
            cur_moment: self.cur_moment,
            new_tracked_moment: Moment::default(),
            tracked_moments: Vec::new(),
            track_id: TrackId::default(),
            saved_moment: self.saved_moment,
        }
    }
}

/// A moment in history, which may contain changes, or may just
/// contain selections
///
/// It also contains information about how to print the buffer, so
/// that going back in time is less jarring.
#[derive(Clone, Default, Debug)]
pub struct Moment {
    changes: GapBuffer<Change<'static, String>>,
    shift_state: (usize, [i32; 3]),
}

impl Moment {
    /// Returns a new `Moment`
    const fn new() -> Self {
        Self {
            changes: GapBuffer::new(),
            shift_state: (0, [0; 3]),
        }
    }
}

impl<Context> Decode<Context> for Moment {
    fn decode<D: bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        Ok(Moment {
            changes: GapBuffer::from_iter(Vec::<Change<'static, String>>::decode(decoder)?),
            shift_state: Decode::decode(decoder)?,
        })
    }
}

impl Encode for Moment {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        let changes = self.changes.iter().cloned().collect::<Vec<_>>();
        changes.encode(encoder)?;
        self.shift_state.encode(encoder)
    }
}

impl Moment {
    /// First try to merge this change with as many changes as
    /// possible, then add it in
    pub(crate) fn add_change(
        &mut self,
        guess_i: Option<usize>,
        mut change: Change<'static, String>,
    ) -> usize {
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
            for change in self.changes.range_mut(from..m_range.end).iter_mut() {
                change.shift_by(shift);
            }
        // Otherwise, the shifting will happen in reverse, and `from`
        // will be moved backwards until the point where m_range ends.
        // This is better for localized Changes.
        } else if from > m_range.end && shift != [0; 3] {
            for change in self.changes.range_mut(m_range.end..from).iter_mut() {
                change.shift_by(shift.map(|i| -i));
            }
        }

        match m_range.len() {
            1 => {
                let old = std::mem::replace(&mut self.changes[m_range.start], change);
                self.changes[m_range.start].try_merge(old);
            }
            _ => {
                let changes: Vec<_> = self.changes.drain(m_range.clone()).collect();
                for c in changes.into_iter().rev() {
                    change.try_merge(c);
                }
                self.changes.insert(m_range.start, change);
            }
        }

        let change = &self.changes[m_range.start];
        let new_from = if change.added_str() != change.taken_str() {
            m_range.start + 1
        } else {
            self.changes.remove(m_range.start);
            m_range.start
        };

        if new_from < self.changes.len() {
            self.shift_state = (new_from, add(shift, new_shift));
        } else {
            self.shift_state = (0, [0; 3]);
        }

        m_range.start
    }

    /// An [`ExactSizeIterator`] over the [`Change`]s in this
    /// [`Moment`]
    ///
    /// These `Change`s represent a
    pub fn iter(&self) -> Changes<'_> {
        Changes {
            moment: self,
            iter: self.changes.iter().enumerate(),
            undo_shift: None,
        }
    }

    /// Returns the number of [`Change`]s in this [`Moment`]
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
///
/// If you acquired this `Change` from a [`BufferTracker::parts`]
/// call, you need not worry about adding it to the ranges that need
/// to be updated, as that has already been done.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Change<'h, S = &'h str> {
    start: [i32; 3],
    added: S,
    taken: S,
    added_end: [i32; 3],
    taken_end: [i32; 3],
    _ghost: PhantomData<&'h str>,
}

impl Change<'static, String> {
    /// Returns a new [Change].
    pub fn new(edit: impl ToString, range: Range<Point>, text: &Text) -> Self {
        let added = {
            let edit = edit.to_string();
            // A '\n' must be kept at the end, no matter what.
            if (range.start == text.len() || range.end == text.len()) && !edit.ends_with('\n') {
                edit + "\n"
            } else {
                edit
            }
        };

        let taken = text.strs(range.clone()).unwrap().to_string();
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

impl<'h> Change<'h> {
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
    pub fn str_insert(added_str: &'h str, start: Point) -> Self {
        Self {
            start: start.as_signed(),
            added: added_str,
            taken: "",
            added_end: (start + Point::len_of(added_str)).as_signed(),
            taken_end: start.as_signed(),
            _ghost: PhantomData,
        }
    }

    pub(crate) fn remove_last_nl(len: Point) -> Change<'static> {
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

/// A tracker to keep up to date on changes to [`Buffer`]s
///
/// This struct is capable of tracking all the [`Change`]s that happen
/// to any `Buffer`. That happens through the
/// [`BufferTracker::parts`] method, which gives a
/// [`BufferParts`] struct, including the `Buffer`'s [`Bytes`],
/// [`Tags`], [`Selections`], as well as an [`ExactSizeIterator`] over
/// the `Change`s that took place since the last call to this
/// function, and a [`RangesToUpdate`] associated with the [`Buffer`]
///
/// The [`RangesToUpdate`] is a struct that keeps track of the ranges
/// where changes have taken place, and informs you on which ones need
/// to be updated based on what is currently visible on screen. At the
/// start, this will include the full range of the `Buffer`, since
/// Duat assumes that you will want to update the whole buffer.
///
/// This struct, alongside [`PerBuffer`], allows for a nice and
/// standardised workflow for "parsers", which essentially update the
/// [`Buffer`]s based on changes that took place and on what is
/// presently visible. One example of such a parser can be seen in the
/// `duat-treesitter` crate.
///
/// [`PerBuffer`]: super::PerBuffer
pub struct BufferTracker {
    tracked: Mutex<Vec<(BufferId, TrackId, Arc<Mutex<Ranges>>)>>,
}

impl BufferTracker {
    /// Returns a new `ChangesFetcher`
    ///
    /// This struct can be stored in a `static` variable, since this
    /// function is `const`. You can then use the same
    /// `ChangesFetcher` to fetch [`Change`]s from all [`Buffer`]s
    pub const fn new() -> Self {
        Self { tracked: Mutex::new(Vec::new()) }
    }

    /// Gets the [`BufferParts`] of a [`Buffer`]
    ///
    /// This struct consists of the normal [`TextParts`], ([`Bytes`],
    /// [`Tags`], [`Selections`]), but it also includes an
    /// [`Iterator`] over all the [`Change`]s that took place since
    /// the last time this function was called by this tracker on this
    /// `Buffer`.
    ///
    /// It is intended for borrowing the `Tags` mutably, whilst
    /// reading from the `Bytes`, `Selections` and the `Change`s that
    /// took place.
    ///
    /// Returns [`None`] when calling this function on a [`Buffer`]
    /// without first having called [`BufferTracker::register_buffer`]
    /// on it.
    ///
    /// [`TextParts`]: crate::text::TextParts
    /// [`Tags`]: crate::text::Tags
    /// [`Selections`]: crate::mode::Selections
    pub fn parts<'b>(&self, buf: &'b mut Buffer) -> Option<BufferParts<'b>> {
        let mut tracked = self.tracked.lock().unwrap();

        let buf_id = buf.buffer_id();

        let (_, old_track_id, ranges) = tracked.iter_mut().find(|(id, ..)| *id == buf_id)?;
        let new_track_id = buf.history.get_latest_track_id();

        let untracked_moments = buf.history.get_untracked_moments_for(*old_track_id);
        *old_track_id = new_track_id;

        let mut iter = untracked_moments.iter();

        let changes = FetchedChanges {
            current: iter.find_map(MomentOrTracking::as_moment).map(Moment::iter),
            iter,
        };

        let mut ranges_lock = ranges.lock().unwrap();
        for change in changes.clone() {
            ranges_lock.shift_by(
                change.start().byte(),
                change.added_end().byte() as i32 - change.taken_end().byte() as i32,
            );

            let range = change.added_range();
            ranges_lock.add(range.start.byte()..range.end.byte());
        }
        drop(ranges_lock);

        let parts = buf.text.parts();

        Some(BufferParts {
            bytes: parts.bytes,
            tags: parts.tags,
            selections: parts.selections,
            changes,
            ranges_to_update: RangesToUpdate {
                ranges: ranges.clone(),
                buf_len: parts.bytes.len().byte(),
                _ghost: PhantomData,
            },
        })
    }

    /// Registers a [`Buffer`] on the list of those that should be
    /// tracked
    ///
    /// Does nothing if the `Buffer` was already registered.
    pub fn register_buffer(&self, buf: &mut Buffer) {
        let mut tracked = self.tracked.lock().unwrap();

        if !tracked.iter().any(|(id, ..)| *id == buf.buffer_id()) {
            let track_id = buf.history.get_latest_track_id();

            let ranges = Arc::new(Mutex::new(Ranges::new(0..buf.text().len().byte())));
            tracked.push((buf.buffer_id(), track_id, ranges));
        }
    }
}

impl Default for BufferTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// This function is like [`TextParts`], but it includes information
/// about [`Change`]s that took place since the last call to
/// [`BufferTracker::parts`], as well as all the ranges of the
/// [`Text`] that still need updating.
///
/// [`TextParts`]: crate::text::TextParts
pub struct BufferParts<'b> {
    /// The [`Bytes`] of the [`Buffer`]
    pub bytes: &'b Bytes,
    /// The [`Tags`] of the [`Buffer`]
    ///
    /// This, unlike [`Bytes`], allows mutation in the form of
    /// [adding] and [removing] [`Tag`]s.
    ///
    /// [adding]: Tags::insert
    /// [removing]: Tags::remove
    /// [`Tag`]: crate::text::Tag
    pub tags: Tags<'b>,
    /// The [`Selections`] of the [`Buffer`]
    pub selections: &'b Selections,
    /// An [`ExactSizeIterator`] of all [`Change`]s that took place
    /// since the last call to [`BufferTracker::parts`]
    pub changes: FetchedChanges<'b>,
    /// A list of the ranges that need to be updated
    ///
    /// This should be used in conjunction with visible ranges
    /// acquired from a [`Handle<Buffer>`], in order to update only
    /// what is visible on screen.
    ///
    /// [`Handle<Buffer>`]: crate::context::Handle
    pub ranges_to_update: RangesToUpdate<'b>,
}

/// If `lhs` contains the start of `rhs`
fn has_start_of(lhs: Range<Point>, rhs: Range<Point>) -> bool {
    lhs.start <= rhs.start && rhs.start <= lhs.end
}

/// Subtracts two `i32` arrays
fn sub(lhs: [i32; 3], rhs: [i32; 3]) -> [i32; 3] {
    [lhs[0] - rhs[0], lhs[1] - rhs[1], lhs[2] - rhs[2]]
}

/// Converts an `[i32; 3]` to a [`Point`]
fn to_point(signed: [i32; 3]) -> Point {
    Point::from_raw(signed[0] as usize, signed[1] as usize, signed[2] as usize)
}

/// An [`Iterator`] over the [`Change`]s of a [`Moment`]
#[doc(hidden)]
#[derive(Debug, Clone)]
pub struct Changes<'h> {
    moment: &'h Moment,
    iter: Enumerate<
        Chain<
            std::slice::Iter<'h, Change<'static, String>>,
            std::slice::Iter<'h, Change<'static, String>>,
        >,
    >,
    undo_shift: Option<[i32; 3]>,
}

impl<'h> Changes<'h> {
    /// Converts this [`Iterator`] to one over the undone version of
    /// the [`Change`]s
    ///
    /// It also resets iteration to the start, so that you aren't
    /// iterating over "done" and "undone" versions of the `Change`s
    /// at once.
    ///
    /// Normally, [`Change::taken_str`] is the `&str` that was taken
    /// from the [`Text`] by this [`Change`]. This method assumes that
    /// you are undoing changes, so the `Change::taken_str` will now
    /// be the `&str` that was _added_ after the `Change`, and
    /// [`Change::added_str`] will be the `&str` that was taken.
    ///
    /// This method can be useful if you want to modify a [`Bytes`] in
    /// order to check out what it used to look like.
    pub fn undone(self) -> Self {
        Self {
            iter: self.moment.changes.iter().enumerate(),
            undo_shift: Some([0; 3]),
            ..self
        }
    }
}

impl<'h> Iterator for Changes<'h> {
    type Item = Change<'h>;

    fn next(&mut self) -> Option<Self::Item> {
        let change = self.iter.next().map(|(i, change)| {
            let (from, shift) = self.moment.shift_state;
            let mut change = change.as_ref();
            if i >= from {
                change.shift_by(shift);
            }
            change
        })?;

        Some(if let Some(undo_shift) = self.undo_shift.as_mut() {
            let mut change = change.reverse();
            change.shift_by(*undo_shift);
            *undo_shift = add(*undo_shift, change.shift());

            change
        } else {
            change
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}

impl<'h> ExactSizeIterator for Changes<'h> {}

/// Changes that took place since a [`BufferTracker`] last called
/// [`parts`]
///
/// This is an [`ExactSizeIterator`] that may comprise multiple
/// [`Moment`]s (or none at all), and iterates through them in the
/// order that they came in. Because of that, unlike [`Moment::iter`],
/// these [`Change`]s will not necessarily be ordered by byte index.
///
/// If you want to iterate on the changes multiple times, [cloning] it
/// is a zero cost operation.
///
/// [`parts`]: BufferTracker::parts
/// [cloning]: Clone::clone
#[derive(Clone, Debug)]
pub struct FetchedChanges<'h> {
    current: Option<Changes<'h>>,
    iter: std::slice::Iter<'h, MomentOrTracking>,
}

impl<'h> Iterator for FetchedChanges<'h> {
    type Item = Change<'h>;

    fn next(&mut self) -> Option<Self::Item> {
        self.current.as_mut()?.next().or_else(|| {
            self.current = self
                .iter
                .find_map(MomentOrTracking::as_moment)
                .map(Moment::iter);
            self.current.as_mut()?.next()
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.current
            .clone()
            .into_iter()
            .chain(
                self.iter
                    .clone()
                    .filter_map(MomentOrTracking::as_moment)
                    .map(Moment::iter),
            )
            .fold((0, Some(0)), |(low, up), changes| {
                (
                    low + changes.size_hint().0,
                    up.zip(changes.size_hint().1).map(|(l, r)| l + r),
                )
            })
    }
}

impl<'h> ExactSizeIterator for FetchedChanges<'h> {}

/// A list of [`Range<usize>`]s of byte indices in a [`Buffer`] that
/// need to be updated
///
/// The recommended way to use this struct is the following:
///
/// - Firstly, this should all probably happen in the
///   [`BufferUpdated`] hook, which is called right before printing to
///   the screen.
/// - Before calling [`BufferTracker::parts`], you should retrieve the
///   ranges that you care about updating, either through
///   [`Handle::full_printed_range`] or
///   [`Handle::printed_line_ranges`]. This is to update only what is
///   visible, conserving cpu resources.
/// - Then, with the received ranges, you can call
///   [`RangesToUpdate::cutoff`], [`intersecting`], or
///   [`select_from`], in order to filter out which ranges need to be
///   updated.
/// - If updating the ranges was successfull, you can call
///   [`RangesToUpdate::update_on`] or [`update_intersecting`], in
///   order to declare those ranges as updated.
///
/// [`BufferUpdated`]: crate::hook::BufferUpdated
/// [`Handle::full_printed_range`]: crate::context::Handle::full_printed_range
/// [`Handle::printed_line_ranges`]: crate::context::Handle::printed_line_ranges
/// [`intersecting`]: RangesToUpdate::intersecting
/// [`select_from`]: RangesToUpdate::select_from
/// [`update_intersecting`]: RangesToUpdate::update_intersecting
pub struct RangesToUpdate<'b> {
    ranges: Arc<Mutex<Ranges>>,
    buf_len: usize,
    _ghost: PhantomData<&'b Buffer>,
}

impl<'b> RangesToUpdate<'b> {
    /// Adds ranges to the list of those that need updating
    ///
    /// Later on, when duat prints on a range that intersects with
    /// these, you can call [`Handle::full_printed_range`] or
    /// [`Handle::printed_line_ranges`] in order to get visible
    /// ranges, and then call [`RangesToUpdate::cutoff`], or
    /// [`intersecting`], or [`select_from`], in order to get the
    /// ranges that need updating, which will include these ones that
    /// were added by this method, as well as those relating to the
    /// changes that took place in the [`Buffer`].
    ///
    /// Returns `true` if any range was added at all.
    ///
    /// [`Handle::full_printed_range`]: crate::context::Handle::full_printed_range
    /// [`Handle::printed_line_ranges`]: crate::context::Handle::printed_line_ranges
    /// [`intersecting`]: Self::intersecting
    /// [`select_from`]: Self::select_from
    pub fn add_ranges(&self, to_add: impl IntoIterator<Item = impl TextRange>) -> bool {
        let mut ranges = self.ranges.lock().unwrap();
        let mut has_changed = false;
        for range in to_add {
            has_changed |= ranges.add(range.to_range(self.buf_len));
        }
        has_changed
    }

    /// Declares that the ranges given by the iterator have been
    /// updated
    ///
    /// The visible iterator here should come from one of the methods
    /// on the [`Handle<Buffer>`] which returns some list of visible
    /// ranges. These include [`Handle::full_printed_range`] or
    /// [`Handle::printed_line_ranges`]. You can, of course, also
    /// feed only part of these lists, or some other arbitrary
    /// range, in order to declare those updated too.
    ///
    /// This function will then assume that you have successfully
    /// updated the ranges and will cut these out of the list. This
    /// will remove the intersections between the ranges that need to
    /// be updated and those of the iterator.
    ///
    /// It will _not_ completely remove ranges that partially
    /// intersect those of the iterator, only truncating them. If you
    /// want that behavior, see [`update_intersecting`]
    ///
    /// [`Handle<Buffer>`]: crate::context::Handle
    /// [`Handle::full_printed_range`]: crate::context::Handle::full_printed_range
    /// [`Handle::printed_line_ranges`]: crate::context::Handle::printed_line_ranges
    /// [`update_intersecting`]: Self::update_intersecting
    pub fn update_on(&self, visible: impl IntoIterator<Item = impl TextRange>) -> bool {
        let mut ranges = self.ranges.lock().unwrap();
        let mut has_changed = false;
        for range in visible {
            let range = range.to_range(self.buf_len);
            has_changed |= ranges.remove_on(range).count() > 0;
        }
        has_changed
    }

    /// Declares that any range intersecting with any of those on the
    /// iterator has been updated
    ///
    /// The visible iterator here should come from one of the methods
    /// on the [`Handle<Buffer>`] which returns some list of visible
    /// ranges. These include [`Handle::full_printed_range`] or
    /// [`Handle::printed_line_ranges`]. You can, of course, also
    /// feed only part of these lists, or some other arbitrary
    /// range, in order to declare those updated too.
    ///
    /// If you want a method that only removes the intersection with
    /// the ranges that need updating, see [`update_on`].
    ///
    /// [`Handle<Buffer>`]: crate::context::Handle
    /// [`Handle::full_printed_range`]: crate::context::Handle::full_printed_range
    /// [`Handle::printed_line_ranges`]: crate::context::Handle::printed_line_ranges
    /// [`update_on`]: Self::update_on
    pub fn update_intersecting(&self, visible: impl IntoIterator<Item = impl TextRange>) -> bool {
        let mut ranges = self.ranges.lock().unwrap();
        let mut has_changed = false;
        for range in visible {
            let range = range.to_range(self.buf_len);
            has_changed |= ranges.remove_intersecting(range).count() > 0;
        }
        has_changed
    }

    /// Returns a list of intersections between the ranges that need
    /// updating and those from an iterator
    ///
    /// The visible iterator here should come from one of the methods
    /// on the [`Handle<Buffer>`] which returns some list of visible
    /// ranges. These include [`Handle::full_printed_range`] or
    /// [`Handle::printed_line_ranges`]. You can, of course, also
    /// feed only part of these lists, or some other arbitrary
    /// range, in order to declare those updated too.
    ///
    /// Note that this returns only the intersecting bits. If you want
    /// a list of all ranges that at least partially intersect, see
    /// [`intersecting`]. If you want to do the opposite, i.e., select
    /// all ranges from the iterator that intersect with those from
    /// the list, see [`select_from`].
    ///
    /// This method is useful if you don't need update full lines,
    /// since it will only care about the ranges that have actually
    /// changed, which aren't full lines most of the time.
    /// [`select_from`] is more useful for updating full lines, since
    /// you can generate a list of printed lines from
    /// [`Handle::printed_line_ranges`], and select from those
    /// only the ranges that have had changes in them. This pattern is
    /// used, for example, by `duat-treesitter`.
    ///
    /// [`Handle<Buffer>`]: crate::context::Handle
    /// [`Handle::full_printed_range`]: crate::context::Handle::full_printed_range
    /// [`Handle::printed_line_ranges`]: crate::context::Handle::printed_line_ranges
    /// [`intersecting`]: Self::intersecting
    /// [`select_from`]: Self::select_from
    pub fn cutoff(&self, visible: impl IntoIterator<Item = impl TextRange>) -> Vec<Range<usize>> {
        let ranges = self.ranges.lock().unwrap();
        visible
            .into_iter()
            .flat_map(|range| ranges.iter_over(range.to_range(self.buf_len)))
            .collect()
    }

    /// Returns a list of all ranges that intersect with those from an
    /// iterator
    ///
    /// The visible iterator here should come from one of the methods
    /// on the [`Handle<Buffer>`] which returns some list of visible
    /// ranges. These include [`Handle::full_printed_range`] or
    /// [`Handle::printed_line_ranges`]. You can, of course, also
    /// feed only part of these lists, or some other arbitrary
    /// range, in order to declare those updated too.
    ///
    /// Note that this returns the full ranges, not just the parts
    /// that intersect. If you want a list of only the
    /// intersections, see [`cutoff`]. If you want to select the
    /// ranges in the iterator that intersect with those from the
    /// list, see [`select_from`].
    ///
    /// [`Handle<Buffer>`]: crate::context::Handle
    /// [`Handle::full_printed_range`]: crate::context::Handle::full_printed_range
    /// [`Handle::printed_line_ranges`]: crate::context::Handle::printed_line_ranges
    /// [`cutoff`]: Self::cutoff
    /// [`select_from`]: Self::select_from
    pub fn intersecting(
        &self,
        visible: impl IntoIterator<Item = impl TextRange>,
    ) -> Vec<Range<usize>> {
        let ranges = self.ranges.lock().unwrap();
        let mut intersecting = Vec::new();

        // There will almost never be more than 50 or so ranges in here, so
        // it's ok to be a little inefficient.
        // Feel free to optimize this if you wish to.
        for range in visible {
            for range in ranges.iter_intersecting(range.to_range(self.buf_len)) {
                if !intersecting.contains(&range) {
                    intersecting.push(range);
                }
            }
        }

        intersecting.sort_unstable_by(|lhs, rhs| lhs.start.cmp(&rhs.start));
        intersecting
    }

    /// Filters an iterator to a list of ranges that intersect with
    /// those that need updating
    ///
    /// The visible iterator here should come from one of the methods
    /// on the [`Handle<Buffer>`] which returns some list of visible
    /// ranges. These include [`Handle::full_printed_range`] or
    /// [`Handle::printed_line_ranges`]. You can, of course, also
    /// feed only part of these lists, or some other arbitrary
    /// range, in order to declare those updated too.
    ///
    /// This method is really useful if you want to update on full
    /// lines, but only do so if the lines have actually changed. If
    /// the lines have had changes, those will be in the
    /// [`RangesToUpdate`]. This method will check if the range of a
    /// line intersects with the ranges that need updating. If that is
    /// the case, that line will be kept in the returned [`Vec`].
    ///
    /// If you want a method that returns only the ranges that have
    /// actually changed, check out [`cutoff`]. If you want a method
    /// that that returns any range from the list that intersects with
    /// those of the iterator, check out [`intersecting`].
    ///
    /// [`Handle<Buffer>`]: crate::context::Handle
    /// [`Handle::full_printed_range`]: crate::context::Handle::full_printed_range
    /// [`Handle::printed_line_ranges`]: crate::context::Handle::printed_line_ranges
    /// [`cutoff`]: Self::cutoff
    /// [`intersecting`]: Self::intersecting
    pub fn select_from(
        &self,
        visible: impl IntoIterator<Item = impl TextRange>,
    ) -> Vec<Range<usize>> {
        let ranges = self.ranges.lock().unwrap();
        // There will almost never be more than 50 or so ranges in here, so
        // it's ok to be a little inefficient.
        // Feel free to optimize this if you wish to.
        visible
            .into_iter()
            .filter_map(|range| {
                let range = range.to_range(self.buf_len);
                ranges
                    .iter_intersecting(range.clone())
                    .next()
                    .map(|_| range)
            })
            .collect()
    }
}

impl<'b> std::fmt::Debug for RangesToUpdate<'b> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RangesToUpdate")
            .field("ranges", &*self.ranges.lock().unwrap())
            .field("buf_len", &self.buf_len)
            .finish_non_exhaustive()
    }
}

#[derive(Clone, Debug)]
enum MomentOrTracking {
    Moment(Moment),
    Tracking(usize, TrackId),
}

impl MomentOrTracking {
    #[must_use]
    fn as_moment(&self) -> Option<&Moment> {
        if let Self::Moment(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq)]
struct TrackId(usize);

impl TrackId {
    /// Returns the next logical `TrackId`, also updating `self`
    fn next(&mut self) -> Self {
        self.0 += 1;
        *self
    }
}
