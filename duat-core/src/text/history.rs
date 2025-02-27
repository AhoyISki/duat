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

use serde::{
    Deserialize, Serialize,
    de::{self, Visitor},
    ser::SerializeStruct,
};

use super::{Point, Text};

/// The history of edits, contains all moments
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct History {
    moments: Vec<Moment>,
    cur_moment: usize,
    new_moment: Option<(Moment, SyncState)>,
    /// Used to update ranges on the File
    unproc_moment: Option<(Moment, SyncState)>,
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
    pub fn add_change(&mut self, guess_i: Option<usize>, change: Change<String>) -> usize {
        let (unproc_moment, unproc_ss) = self.unproc_moment.get_or_insert_default();
        unproc_moment.add_change(guess_i, change.clone(), unproc_ss);

        let (new_moment, new_ss) = self.new_moment.get_or_insert_default();
        new_moment.add_change(guess_i, change, new_ss)
    }

    /// Declares that the current moment is complete and starts a
    /// new one
    pub fn new_moment(&mut self) {
        let Some((mut new_moment, mut new_ss)) = self.new_moment.take() else {
            return;
        };
        finish_synchronizing(&mut new_moment, &mut new_ss);

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

            let mut shift = (0, 0, 0);
            let iter = self.moments[self.cur_moment].0.iter().map(move |c| {
                let mut change = c.as_ref();
                change.shift_by(shift);

                shift.0 += change.taken_end().byte() as i32 - change.added_end().byte() as i32;
                shift.1 += change.taken_end().char() as i32 - change.added_end().char() as i32;
                shift.2 += change.taken_end().line() as i32 - change.added_end().line() as i32;

                change.reverse()
            });
            Some(iter.collect())
        }
    }

    pub fn unprocessed_changes(&mut self) -> Option<Vec<Change<String>>> {
        self.unproc_moment
            .take()
            .map(|(c, _)| c.0.into_iter().collect())
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
#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct Moment(Vec<Change<String>>);

impl Moment {
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
    fn add_change(
        &mut self,
        guess_i: Option<usize>,
        change: Change<String>,
        ss: &mut SyncState,
    ) -> usize {
        let (shift, sh_from) = (ss.shift, ss.sh_from);
        let b = change.added_end().byte() as i32 - change.taken_end().byte() as i32;
        let c = change.added_end().char() as i32 - change.taken_end().char() as i32;
        let l = change.added_end().line() as i32 - change.taken_end().line() as i32;

        let (change_i, sh_from) = match self.add_change_inner(guess_i, change, shift, sh_from) {
            Ok((change_i, sh_from)) => (change_i, sh_from),
            // A failure can happen if we are inserting before the `sh_from`, which would violate
            // the strict ordering of changes.
            // In order to remedy that, we synchronize the remaining changes and do it again with a
            // fresh SyncState. This should never fail.
            Err((guess_i, change)) => {
                finish_synchronizing(self, ss);
                *ss = SyncState::default();
                self.add_change_inner(Some(guess_i), change, (0, 0, 0), 0)
                    .unwrap()
            }
        };
        ss.sh_from = sh_from;
        ss.shift.0 += b;
        ss.shift.1 += c;
        ss.shift.2 += l;

        change_i
    }

    /// Inner insertion of a [`Change`]
    fn add_change_inner(
        &mut self,
        guess_i: Option<usize>,
        mut change: Change<String>,
        shift: (i32, i32, i32),
        sh_from: usize,
    ) -> Result<(usize, usize), (usize, Change<String>)> {
        let sh = |n: usize| if sh_from <= n { shift } else { (0, 0, 0) };

        // The range of changes that will be drained
        let c_range = if let Some(guess_i) = guess_i
            && let Some(c) = self.0.get(guess_i)
            && c.start.shift_by(sh(guess_i)) <= change.start
            && change.start <= c.added_end().shift_by(sh(guess_i))
        {
            // If we intersect the initial change, include it
            guess_i..guess_i + 1
        } else {
            let f = |i: usize, c: &Change<String>| c.start.shift_by(sh(i));
            match binary_search_by_key_and_index(&self.0, change.start, f) {
                // Same thing here
                Ok(i) => i..i + 1,
                Err(i) => {
                    // This is if we intersect the added part
                    if let Some(older_i) = i.checked_sub(1)
                        && change.start <= self.0[older_i].added_end().shift_by(sh(older_i))
                    {
                        older_i..i
                    // And here is if we intersect nothing on the
                    // start, no changes drained.
                    } else {
                        i..i
                    }
                }
            }
        };

        if c_range.start < sh_from {
            return Err((c_range.start, change));
        }

        // This block determines how far ahead this change will merge
        let c_range = if self
            .0
            .get(c_range.end)
            .is_none_or(|c| change.taken_end() < c.start.shift_by(sh(c_range.end)))
        {
            // If there is no change ahead, or it doesn't intersec, don't merge
            c_range
        } else {
            let start_i = c_range.start + 1;
            // Otherwise search ahead for another change to be merged
            let f = |i: usize, c: &Change<String>| c.start.shift_by(sh(start_i + i));
            match binary_search_by_key_and_index(&self.0[start_i..], change.taken_end(), f) {
                Ok(i) => c_range.start..start_i + i + 1,
                Err(i) => {
                    if let Some(older) = self.0.get(start_i + i)
                        && older.start.shift_by(sh(start_i + i)) <= change.taken_end()
                    {
                        c_range.start..start_i + i + 1
                    } else {
                        c_range.start..start_i + i
                    }
                }
            }
            // It will always be included
        };

        // Shift all ranges that preceed the end of the range.
        if shift != (0, 0, 0) && sh_from < c_range.end {
            for change in &mut self.0[sh_from..c_range.end] {
                change.shift_by(shift);
            }
        }

        for c in self.0.drain(c_range.clone()).rev() {
            change.try_merge(c);
        }

        let sh_from = if !(change.added_text() == "" && change.taken_text() == "") {
            self.0.insert(c_range.start, change);
            c_range.start + 1
        } else {
            c_range.start
        };

        Ok((c_range.start, sh_from))
    }

    pub fn changes(&self) -> &[Change<String>] {
        &self.0
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
            unreachable!("Files chosen that don't interact");
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
    pub fn reverse(self) -> Change<S> {
        Change {
            start: self.start,
            added: self.taken,
            taken: self.added,
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

impl Serialize for Change<String> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut ser_change = serializer.serialize_struct("Change", 3)?;

        ser_change.serialize_field("start", &self.start)?;
        ser_change.serialize_field("added", &self.added)?;
        ser_change.serialize_field("taken", &self.taken)?;

        ser_change.end()
    }
}

impl<'de> Deserialize<'de> for Change<String> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        static FIELDS: [&str; 3] = ["start", "added", "taken"];
        struct ChangeVisitor;

        impl<'de> Visitor<'de> for ChangeVisitor {
            type Value = Change<String>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("struct Change")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                let start = seq
                    .next_element()?
                    .ok_or(de::Error::invalid_length(0, &self))?;
                let added = seq
                    .next_element()?
                    .ok_or(de::Error::invalid_length(1, &self))?;
                let taken = seq
                    .next_element()?
                    .ok_or(de::Error::invalid_length(2, &self))?;
                Ok(Change { start, added, taken })
            }
        }

        deserializer.deserialize_struct("Change", &FIELDS, ChangeVisitor)
    }
}

/// If `lhs` contains the start of`rhs`
fn has_start_of(lhs: Range<usize>, rhs: Range<usize>) -> bool {
    lhs.start <= rhs.start && rhs.start <= lhs.end
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
struct SyncState {
    shift: (i32, i32, i32),
    sh_from: usize,
}

/// Binary searching by key taking into account the index
fn binary_search_by_key_and_index<T, K>(
    vec: &[T],
    key: K,
    f: impl Fn(usize, &T) -> K,
) -> std::result::Result<usize, usize>
where
    K: PartialEq + Eq + PartialOrd + Ord,
{
    let mut size = vec.len();
    let mut left = 0;
    let mut right = size;

    while left < right {
        let mid = left + size / 2;

        let k = f(mid, &vec[mid]);

        match k.cmp(&key) {
            std::cmp::Ordering::Less => left = mid + 1,
            std::cmp::Ordering::Equal => return Ok(mid),
            std::cmp::Ordering::Greater => right = mid,
        }

        size = right - left;
    }

    Err(left)
}

/// Finish synchronizing the unsynchronized changes
fn finish_synchronizing(moment: &mut Moment, ss: &mut SyncState) {
    if ss.shift != (0, 0, 0) {
        // You can't get to this point without having at least one moment.
        for change in &mut moment.0[ss.sh_from..] {
            change.shift_by(ss.shift);
        }
    }
}
