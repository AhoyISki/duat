//! The [`Selections`] and [`Selection`] structs
//!
//! This module just defines the underlying [`Selections`] struct, as
//! well as all of its components. This struct is used by [`Handle`]s
//! in order to modify [`Text`]s by manipulating [`Cursor`]s, which
//! are ultimately backed by the [`Selection`] struct.
//!
//! This module also defines [`VPoint`], which is essentially a
//! [`Point`] with more information inbued into it, most notably
//! various column distances, as well as their "desired values".
//!
//! [`Handle`]: crate::context::Handle
//! [`Text`]: crate::text::Text
//! [`Cursor`]: super::Cursor
use std::sync::Mutex;

use gap_buf::{GapBuffer, gap_buffer};

pub use self::cursor::{Selection, VPoint};
use crate::{
    buffer::Change,
    text::{Bytes, Point, TextRange},
    utils::{add_shifts, merging_range_by_guess_and_lazy_shift},
};

/// The list of [`Selection`]s in a [`Text`]
///
/// This list can contain any number of [`Selection`]s, and they
/// should be usable in whatever order the end user may want, without
/// breaking from, for example, modifications that should move cursors
/// backwards or ahead. If that is not the case, report it as a bug.
///
/// they are primarily meant to be interacted with from the
/// [`Handle`], with its [`edit_`] methods meant to efficiently
/// handle a large number of [`Selection`]s in an efficient manner,
/// although you can interact with them separately.
///
/// [`Handle`]: crate::context::Handle
/// [`edit_`]: crate::context::Handle::edit_all
/// [`Text`]: crate::text::Text
pub struct Selections {
    buf: GapBuffer<Selection>,
    main_i: usize,
    shift: Mutex<Shift>,
}

impl Selections {
    /// A new `Selections` with a set main [`Selection`]
    pub(crate) fn new(main: Selection) -> Self {
        Self {
            buf: gap_buffer![main],
            main_i: 0,
            shift: Mutex::default(),
        }
    }

    /// Returns a new empty `Selections`
    pub(crate) const fn new_empty() -> Self {
        Self {
            buf: GapBuffer::new(),
            main_i: 0,
            shift: Mutex::new(Shift { from: 0, by: [0; 3] }),
        }
    }

    ////////// Modification functions

    /// Sets the main [`Selection`]
    pub fn set_main(&mut self, new: usize) {
        self.main_i = new.min(self.buf.len().saturating_sub(1));
    }

    /// Rotates the main [`Selection`] by an amount
    pub fn rotate_main(&mut self, amount: i32) {
        self.main_i = (self.main_i as i32 + amount).rem_euclid(self.buf.len() as i32) as usize
    }

    /// Removes all [`Selection`]s
    pub fn clear(&mut self) {
        self.buf = GapBuffer::new();
        *self.shift.get_mut().unwrap() = Shift::default();
    }

    /// Removes all [`Selection`]s and adds a [default `Selection`] as
    /// main
    ///
    /// [default `Selection`]: Selection::default
    pub fn reset(&mut self) {
        self.remove_extras();
        self.buf[self.main_i] = Selection::default();
    }

    /// Removes all but the main [`Selection`]
    pub fn remove_extras(&mut self) {
        if !self.is_empty() {
            let cursor = self.buf.remove(self.main_i);
            let shift = std::mem::take(self.shift.get_mut().unwrap());
            if shift.from <= self.main_i && shift.by != [0; 3] {
                cursor.shift_by(shift.by);
            }
            self.buf = gap_buffer![cursor];
        }
        self.main_i = 0;
    }

    /// Corrects all [`Selection`]s, so that they no longer reference
    /// outdated data
    pub(crate) fn correct_all(&mut self, bytes: &mut Bytes) {
        for selection in &mut self.buf {
            selection.correct(bytes)
        }
    }

    ////////// Querying functions

    /// Gets the main [`Selection`]
    ///
    /// # Panics
    ///
    /// This method will panic if there are no `Selection`s. If you
    /// want a non-panicking method, see [`Selections::get_main`].
    #[track_caller]
    pub fn main(&self) -> &Selection {
        match self.get(self.main_i) {
            Some(main) => main,
            None => panic!("No main selection"),
        }
    }

    /// Gets the main [`Selection`], if there is one
    ///
    /// If you want a method that doesn't return an [`Option`] (for
    /// convenience), see [`Selections::main`].
    pub fn get_main(&self) -> Option<&Selection> {
        self.get(self.main_i)
    }

    /// Gets the `n`th [`Selection`] if there is one
    pub fn get(&self, n: usize) -> Option<&Selection> {
        if n >= self.len() {
            return None;
        }
        let mut shift = self.shift.lock().unwrap();
        if n >= shift.from && shift.by != [0; 3] {
            for cursor in self.buf.range(shift.from..n + 1).iter() {
                cursor.shift_by(shift.by);
            }
            if n + 1 < self.buf.len() {
                shift.from = n + 1;
            } else {
                *shift = Shift::default();
            }
        }

        self.buf.get(n)
    }

    /// Iterates over all [`Selection`]s in order
    ///
    /// Also tells you wether the [`Selection`] is the main selection
    /// or not.
    pub fn iter(&self) -> impl Iterator<Item = (&Selection, bool)> {
        let mut shift = *self.shift.lock().unwrap();

        self.buf.iter().enumerate().map(move |(i, selection)| {
            if i >= shift.from && shift.by != [0; 3] {
                selection.shift_by(shift.by);
                if i + 1 < self.buf.len() {
                    self.shift.lock().unwrap().from = i + 1;
                    shift.from = i + 1;
                } else {
                    *self.shift.lock().unwrap() = Shift::default();
                }
            }

            (selection, i == self.main_i)
        })
    }

    /// Iterates over all [`Selection`]s in a given [`TextRange`]
    ///
    /// Also tells you the index of the [`Selection`], as well as if
    /// it is the main selection or not.
    ///
    /// This [`Iterator`] *will* include [`Selection`]s that are
    /// partially contained.
    pub fn iter_within(
        &self,
        range: impl TextRange,
    ) -> impl Iterator<Item = (usize, &Selection, bool)> {
        let shift = *self.shift.lock().unwrap();
        let range = range.to_range(u32::MAX as usize);

        let m_range = merging_range_by_guess_and_lazy_shift(
            (&self.buf, self.buf.len()),
            (0, [range.start, range.end]),
            (shift.from, shift.by[0], 0, |byte, shift| {
                (byte as i32 + shift) as usize
            }),
            (
                |sel| sel.start_point().byte(),
                |sel| sel.end_point_excl().byte(),
            ),
        );

        let (s0, s1) = self.buf.range(m_range.clone()).as_slices();
        let iter = [s0, s1].into_iter().flatten().enumerate();
        iter.map(move |(i, selection)| {
            let i = i + m_range.start;
            if i >= shift.from && shift.by != [0; 3] {
                selection.shift_by(shift.by);
                if i + 1 < self.buf.len() {
                    self.shift.lock().unwrap().from = i + 1;
                } else {
                    *self.shift.lock().unwrap() = Shift::default();
                }
            }

            (i, selection, i == self.main_i)
        })
    }

    /// The index of the main [`Selection`]
    pub fn main_index(&self) -> usize {
        self.main_i
    }

    /// How many [`Selection`]s there are in the list
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    /// Returns [`true`] when there are no [`Selection`]s
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    ////////// Internal modification functions

    /// Inserts a [`Selection`] back from editing
    pub(crate) fn insert(
        &mut self,
        guess_i: usize,
        sel: Selection,
        main: bool,
    ) -> ([usize; 2], bool) {
        let mut shift = self.shift.lock().unwrap();
        let shift_from = shift.from.min(self.len());

        // The range of cursors that will be drained
        let m_range = merging_range_by_guess_and_lazy_shift(
            (&self.buf, self.buf.len()),
            (guess_i, [sel.start_point(), sel.end_point_excl()]),
            (shift_from, shift.by, [0; 3], Point::shift_by),
            (Selection::start_point, Selection::end_point_excl),
        );

        // Shift all ranges that preceed the end of the cursor's range.
        if shift_from < m_range.end && shift.by != [0; 3] {
            for cursor in self.buf.range(shift_from..m_range.end).into_iter() {
                cursor.shift_by(shift.by);
            }
        }

        // Get the minimum and maximum Points in the taken range, designate
        // those as the new Selection's bounds.
        let (caret, anchor, last_cursor_overhangs) = {
            let mut c_range = m_range.clone();
            let first = c_range.next().and_then(|i| self.buf.get(i));
            let last = c_range.last().and_then(|i| self.buf.get(i)).or(first);
            let start = first
                .map(|first| first.lazy_v_start().min(sel.lazy_v_start()))
                .unwrap_or(sel.lazy_v_start());
            let (end, last_sel_overhangs) = if let Some(last) = last
                && last.lazy_v_end() >= sel.lazy_v_end()
            {
                (last.lazy_v_end(), true)
            } else {
                (sel.lazy_v_end(), false)
            };

            if let Some(anchor) = sel.anchor() {
                match sel.caret() < anchor {
                    true => (start, Some(end), last_sel_overhangs),
                    false => (end, Some(start), last_sel_overhangs),
                }
            } else {
                (end, (start != end).then_some(start), last_sel_overhangs)
            }
        };

        let selection = Selection::from_v(caret, anchor, sel.change_i);
        self.buf.splice(m_range.clone(), [selection]);

        if main {
            self.main_i = m_range.start;
        } else if self.main_i >= m_range.start {
            self.main_i = (self.main_i + 1 - m_range.clone().count()).max(m_range.start)
        }

        // If there are no more Selections after this, don't set the
        // shift_state.
        let cursors_taken = m_range.clone().count();
        let new_shift_from = shift_from.saturating_sub(cursors_taken).max(m_range.start) + 1;
        if new_shift_from < self.buf.len() {
            shift.from = new_shift_from;
        } else {
            *shift = Shift::default()
        }

        ([m_range.start, cursors_taken], last_cursor_overhangs)
    }

    /// Applies a [`Change`] to the [`Selection`]s list
    ///
    /// Returns the number of [`Selection`]s that were removed
    pub(crate) fn apply_change(&mut self, guess_i: usize, change: Change<&str>) -> usize {
        let mut shift = self.shift.lock().unwrap();
        let shift_from = shift.from.min(self.len());

        // The range of cursors that will be drained
        let c_range = merging_range_by_guess_and_lazy_shift(
            (&self.buf, self.buf.len()),
            (guess_i, [change.start(), change.taken_end()]),
            (shift_from, shift.by, [0; 3], Point::shift_by),
            (Selection::start_point, Selection::end_point_excl),
        );

        // Since applied changes don't remove Selections, we need to shift all
        // Selections in the whole range. First by the original shift, in
        // order to update them to the latest shift leve, then by the
        // change.
        if c_range.end > shift_from && shift.by != [0; 3] {
            for cursor in self.buf.range(shift_from..c_range.end).into_iter() {
                cursor.shift_by(shift.by);
            }
        }
        let range = c_range.start..c_range.end.max(shift_from);
        for cursor in self.buf.range(range).into_iter() {
            cursor.shift_by_change(change);
        }

        let (cursors_taken, cursors_added) = {
            let mut cursors_taken = self.buf.splice(c_range.clone(), []);
            if let Some(first) = cursors_taken.next() {
                let last = cursors_taken.next_back().unwrap_or(first.clone());
                let (start, end) = (first.start_point(), last.end_point_excl());
                let merged = Selection::new(start, (start < end).then_some(end));
                drop(cursors_taken);
                self.buf.insert(c_range.start, merged);

                (c_range.len(), 1)
            } else {
                (0, 0)
            }
        };

        let from = shift_from.saturating_sub(cursors_taken).max(c_range.start) + cursors_added;
        if from < self.buf.len() {
            *shift = Shift {
                from,
                by: add_shifts(shift.by, change.shift()),
            };
        } else {
            *shift = Shift::default()
        }

        cursors_taken - cursors_added
    }

    /// Removes a [`Selection`], which might be brought back
    pub(crate) fn remove(&mut self, i: usize) -> Option<(Selection, bool)> {
        if i >= self.buf.len() {
            return None;
        }
        let shift = self.shift.get_mut().unwrap();

        if i >= shift.from && shift.by != [0; 3] {
            for cursor in self.buf.range(shift.from..i + 1).iter() {
                cursor.shift_by(shift.by);
            }
            if i + 1 < self.buf.len() {
                // i here, instead of i + 1, since this Selection is about to be
                // removed.
                shift.from = i;
            } else {
                *shift = Shift::default()
            }
        } else if i < shift.from {
            // If I am removing before shift_from, obviously the index of the
            // first unshifted Selection is moved back.
            shift.from -= 1;
        }

        let was_main = self.main_i == i;
        if self.main_i >= i {
            self.main_i = self.main_i.saturating_sub(1);
        }
        Some((self.buf.remove(i), was_main))
    }

    /// Ensures that there is at least one [`Selection`] on the list
    pub(crate) fn populate(&mut self) {
        if self.buf.is_empty() {
            self.main_i = 0;
            self.buf = gap_buffer![Selection::default()];
        }
    }
}

impl Clone for Selections {
    fn clone(&self) -> Self {
        Self {
            buf: self.buf.clone(),
            main_i: self.main_i,
            shift: Mutex::new(*self.shift.lock().unwrap()),
        }
    }
}

mod cursor {
    use std::{cmp::Ordering, ops::Range, sync::Mutex};

    use bincode::{Decode, Encode};

    use crate::{
        buffer::Change,
        opts::PrintOpts,
        text::{Bytes, Point, Text, TextIndex},
        ui::Area,
    };

    /// A cursor in the text buffer. This is an editing cursor, -(not
    /// a printing cursor.
    #[derive(Default, Encode, Decode)]
    pub struct Selection {
        caret: Mutex<LazyVPoint>,
        anchor: Mutex<Option<LazyVPoint>>,
        pub(in crate::mode::cursor) change_i: Option<u32>,
    }

    impl Selection {
        /// Returns a new instance of [`Selection`].
        pub(crate) fn new(caret: Point, anchor: Option<Point>) -> Self {
            Self {
                caret: Mutex::new(LazyVPoint::Unknown(caret)),
                anchor: Mutex::new(anchor.map(LazyVPoint::Unknown)),
                change_i: None,
            }
        }

        pub(super) fn from_v(
            caret: LazyVPoint,
            anchor: Option<LazyVPoint>,
            change_i: Option<u32>,
        ) -> Self {
            Self {
                caret: Mutex::new(caret),
                anchor: Mutex::new(anchor),
                change_i,
            }
        }

        /// Moves to specific, pre calculated [`Point`].
        #[track_caller]
        pub fn move_to(&mut self, idx: impl TextIndex, text: &Text) {
            let byte = idx.to_byte_index();
            if byte == self.caret().byte() {
                return;
            }
            *self.caret.get_mut().unwrap() =
                LazyVPoint::Unknown(text.point_at_byte(byte.min(text.len().byte() - 1)));
        }

        /// Internal horizontal movement function
        ///
        /// Returns `true` if the caret was moved
        pub fn move_hor(&mut self, by: i32, text: &Text) -> i32 {
            let by = by as isize;
            if by == 0 {
                return 0;
            };

            let caret = self.caret.get_mut().unwrap();

            // We move in chars, not bytes, but calculating char index can be
            // expensive, so do a rough estimate assuming that the text is ascii
            // only.
            let target_char = caret.point().char().saturating_add_signed(by);

            let point = if target_char == 0 {
                Point::default()
            } else if target_char >= text.last_point().char() {
                text.last_point()
            } else if by.abs() < 500 {
                if by > 0 {
                    text.chars_fwd(caret.point()..)
                        .unwrap()
                        .take(by as usize)
                        .fold(caret.point(), |point, (_, char)| point.fwd(char))
                } else {
                    text.chars_rev(..caret.point())
                        .unwrap()
                        .take(by.unsigned_abs())
                        .fold(caret.point(), |point, (_, char)| point.rev(char))
                }
            } else {
                text.point_at_char(target_char)
            };

            let moved = point.char() as i32 - caret.point().char() as i32;
            *caret = LazyVPoint::Unknown(point);
            moved
        }

        /// Internal vertical movement function.
        ///
        /// Returns `true` if the caret actually moved at all.
        pub fn move_ver(&mut self, by: i32, text: &Text, area: &Area, opts: PrintOpts) -> bool {
            if by == 0 {
                return false;
            }
            let caret = self.caret.get_mut().unwrap();
            let point = caret.point();

            let desired_col = match *caret {
                LazyVPoint::Unknown(_) => None,
                LazyVPoint::Known(vpoint) => Some(vpoint.desired_visual_col()),
                LazyVPoint::Desired { dvcol, .. } => Some(dvcol as usize),
            };

            let vpoint = area.move_ver(by, text, point, desired_col, opts);
            *caret = LazyVPoint::Known(vpoint);

            vpoint.point != point
        }

        /// Internal vertical movement function.
        ///
        /// Returns `true` if the caret actually moved at all.
        pub fn move_ver_wrapped(
            &mut self,
            by: i32,
            text: &Text,
            area: &Area,
            opts: PrintOpts,
        ) -> bool {
            if by == 0 {
                return false;
            };

            let caret = self.caret.get_mut().unwrap();
            let point = caret.point();

            let desired_col = match *caret {
                LazyVPoint::Unknown(_) => None,
                LazyVPoint::Known(vpoint) => Some(vpoint.desired_wrapped_col()),
                LazyVPoint::Desired { dwcol, .. } => Some(dwcol as usize),
            };

            let vpoint = area.move_ver_wrapped(by, text, point, desired_col, opts);
            *caret = LazyVPoint::Known(vpoint);

            vpoint.point != point
        }

        pub(crate) fn shift_by_change(&self, change: Change<&str>) {
            let mut caret = self.caret.lock().unwrap();

            let (shift, taken) = (change.shift(), change.taken_end());
            if caret.point() >= change.start() {
                let shifted_caret = caret.point().max(taken).shift_by(shift);
                *caret = LazyVPoint::Unknown(shifted_caret);
            }

            let mut anchor = self.anchor.lock().unwrap();
            if let Some(anchor) = &mut *anchor
                && anchor.point() >= change.start()
            {
                let shifted_anchor = anchor.point().max(taken).shift_by(shift);
                *anchor = LazyVPoint::Unknown(shifted_anchor);
            }
        }

        /// Assumes that both parts of the cursor are ahead of the
        /// shift
        pub(crate) fn shift_by(&self, shift: [i32; 3]) {
            let mut caret = self.caret.lock().unwrap();
            *caret = LazyVPoint::Unknown(caret.point().shift_by(shift));

            let mut anchor = self.anchor.lock().unwrap();
            if let Some(anchor) = &mut *anchor {
                *anchor = LazyVPoint::Unknown(anchor.point().shift_by(shift));
            }
        }

        /// Corrects this [`Selection`], so that it no longer assumes
        /// to be in the correct position
        pub(crate) fn correct(&mut self, bytes: &mut Bytes) {
            let mut caret = self.caret.lock().unwrap();
            *caret = LazyVPoint::Unknown(bytes.point_at_byte(caret.point().byte()));
            let point = caret.point();
            bytes.add_record([point.byte(), point.char(), point.line()]);

            let mut anchor = self.anchor.lock().unwrap();
            if let Some(anchor) = &mut *anchor {
                *anchor = LazyVPoint::Unknown(bytes.point_at_byte(anchor.point().byte()));
                let point = anchor.point();
                bytes.add_record([point.byte(), point.char(), point.line()]);
            }
        }

        ////////// Public movement functions

        /// Sets the position of the anchor to be the same as the
        /// current cursor position in the buffer
        ///
        /// The `anchor` and `current` act as a range of text on the
        /// buffer.
        pub fn set_anchor(&mut self) {
            *self.anchor.get_mut().unwrap() = Some(*self.caret.get_mut().unwrap())
        }

        /// Unsets the anchor, returning its byte index if it existed
        ///
        /// This is done so the cursor no longer has a valid
        /// selection.
        pub fn unset_anchor(&mut self) -> Option<Point> {
            self.anchor.get_mut().unwrap().take().map(|a| a.point())
        }

        /// Switches the position of the anchor and caret
        pub fn swap_ends(&mut self) {
            if let Some(anchor) = self.anchor.get_mut().unwrap() {
                std::mem::swap(self.caret.get_mut().unwrap(), anchor);
            }
        }

        /// Returns the byte index of this `Selection`'s `caret`
        pub fn caret(&self) -> Point {
            self.caret.lock().unwrap().point()
        }

        /// Returns the byte index of this `Selection`'s `anchor`, if
        /// there is one
        pub fn anchor(&self) -> Option<Point> {
            self.anchor.lock().unwrap().map(|lazy| lazy.point())
        }

        ////////// Range functions

        /// Returns the byte index range between the `caret` and
        /// `anchor`
        ///
        /// If `anchor` isn't set, returns an empty range on `caret`.
        ///
        /// # Note
        ///
        /// This range is _inclusive_, that is, it will include the
        /// character at the end. If you use it to replace a range in
        /// the [`Text`], know that this range will be truncated to
        /// not include the last `\n`, since it is not allowed to be
        /// removed.
        pub fn byte_range(&self, bytes: &Bytes) -> Range<usize> {
            self.start_point().byte()..self.end_point(bytes).byte()
        }

        /// The starting [`Point`] of this [`Selection`]
        pub fn start_point(&self) -> Point {
            if let Some(anchor) = *self.anchor.lock().unwrap() {
                anchor.point().min(self.caret.lock().unwrap().point())
            } else {
                self.caret.lock().unwrap().point()
            }
        }

        /// The ending [`Point`] of this [`Selection`]
        pub fn end_point(&self, bytes: &Bytes) -> Point {
            self.end_point_excl()
                .fwd(bytes.char_at(self.end_point_excl()).unwrap())
        }

        pub(super) fn end_point_excl(&self) -> Point {
            if let Some(anchor) = *self.anchor.lock().unwrap() {
                anchor.point().max(self.caret.lock().unwrap().point())
            } else {
                self.caret.lock().unwrap().point()
            }
        }

        /// Returns the range between `caret` and `anchor`.
        ///
        /// If `anchor` isn't set, returns a range that contains only
        /// the `caret`'s current `char`.
        pub fn point_range(&self, bytes: &Bytes) -> Range<Point> {
            self.start_point()..self.end_point(bytes)
        }

        /// Returns an exclusive range between `caret` and `anchor`
        ///
        /// If `anchor` isn't set, both [`Point`]s will be the same.
        pub fn point_range_excl(&self) -> Range<Point> {
            self.start_point()..self.end_point_excl()
        }

        ////////// VPoint functions

        /// Sets both the desired visual column, as well as the
        /// desired wrapped column
        pub fn set_desired_cols(&mut self, v: usize, w: usize) {
            let (v, w) = (v as u16, w as u16);
            let caret = self.caret.get_mut().unwrap();
            match caret {
                LazyVPoint::Known(vp) => {
                    vp.dvcol = v;
                    vp.dwcol = w;
                }
                LazyVPoint::Unknown(point) => {
                    *caret = LazyVPoint::Desired { point: *point, dvcol: v, dwcol: w }
                }
                LazyVPoint::Desired { dvcol, dwcol, .. } => (*dvcol, *dwcol) = (v, w),
            }
        }

        /// The visual caret of this [`Selection`]
        ///
        /// [`VPoint`]s include a lot more information than regular
        /// [`Point`]s, like visual distance form the left edge, what
        /// the desired distance is, etc.
        pub fn v_caret(&self, text: &Text, area: &Area, opts: PrintOpts) -> VPoint {
            let mut caret = self.caret.lock().unwrap();
            let vp = caret.calculate(text, area, opts);
            *caret = LazyVPoint::Known(vp);
            vp
        }

        /// The visual anchor of this [`Selection`], if it exists
        ///
        /// [`VPoint`]s include a lot more information than regular
        /// [`Point`]s, like visual distance form the left edge, what
        /// the desired distance is, etc.
        pub fn v_anchor(&self, text: &Text, area: &Area, opts: PrintOpts) -> Option<VPoint> {
            self.anchor.lock().unwrap().as_mut().map(|anchor| {
                let vp = anchor.calculate(text, area, opts);
                *anchor = LazyVPoint::Known(vp);
                vp
            })
        }

        /// The visual range between the caret and anchor of this
        /// [`Selection`]
        ///
        /// [`VPoint`]s include a lot more information than regular
        /// [`Point`]s, like visual distance form the left edge, what
        /// the desired distance is, etc.
        pub fn v_range(&self, text: &Text, area: &Area, opts: PrintOpts) -> [VPoint; 2] {
            let v_caret = self.v_caret(text, area, opts);
            let v_anchor = self.v_anchor(text, area, opts).unwrap_or(v_caret);
            [v_caret.min(v_anchor), v_caret.max(v_anchor)]
        }

        /// The starting [`LazyVPoint`]
        pub(super) fn lazy_v_start(&self) -> LazyVPoint {
            match *self.anchor.lock().unwrap() {
                Some(anchor) => self.caret.lock().unwrap().min(anchor),
                None => *self.caret.lock().unwrap(),
            }
        }

        /// The ending [`LazyVPoint`]
        pub(super) fn lazy_v_end(&self) -> LazyVPoint {
            match *self.anchor.lock().unwrap() {
                Some(anchor) => self.caret.lock().unwrap().max(anchor),
                None => *self.caret.lock().unwrap(),
            }
        }
    }

    impl Clone for Selection {
        fn clone(&self) -> Self {
            Self {
                caret: Mutex::new(*self.caret.lock().unwrap()),
                anchor: Mutex::new(*self.anchor.lock().unwrap()),
                change_i: self.change_i,
            }
        }
    }

    /// A struct meant to minimize calculations on very large numbers
    /// of [`Selection`]s
    #[derive(Clone, Copy, Eq, Encode, Decode)]
    pub(super) enum LazyVPoint {
        Unknown(Point),
        Known(VPoint),
        Desired {
            point: Point,
            dvcol: u16,
            dwcol: u16,
        },
    }

    impl LazyVPoint {
        fn point(&self) -> Point {
            match *self {
                LazyVPoint::Unknown(point) | LazyVPoint::Desired { point, .. } => point,
                LazyVPoint::Known(vp) => vp.point,
            }
        }

        /// Calculates the [`VPoint`], to be used sparingly
        fn calculate(self, text: &Text, area: &Area, opts: PrintOpts) -> VPoint {
            match self {
                Self::Known(vp) => vp,
                Self::Unknown(point) => area.move_ver(0, text, point, None, opts),
                Self::Desired { point, dvcol, dwcol } => {
                    let mut vp = area.move_ver(0, text, point, Some(dvcol as usize), opts);
                    vp.dvcol = dvcol;
                    vp.dwcol = dwcol;
                    vp
                }
            }
        }
    }

    impl Default for LazyVPoint {
        fn default() -> Self {
            Self::Desired {
                point: Point::default(),
                dvcol: 0,
                dwcol: 0,
            }
        }
    }

    #[allow(clippy::non_canonical_partial_ord_impl)]
    impl PartialOrd for LazyVPoint {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.point().cmp(&other.point()))
        }
    }

    impl Ord for LazyVPoint {
        fn cmp(&self, other: &Self) -> Ordering {
            self.partial_cmp(other).unwrap()
        }
    }

    impl PartialEq for LazyVPoint {
        fn eq(&self, other: &Self) -> bool {
            self.point() == other.point()
        }
    }

    /// A visual [`Point`], which includes more information
    ///
    /// Alongside the byte, char, and line of the [`Point`], this
    /// struct has:
    ///
    /// - Number of [`char`]s from the left edge
    /// - Number of visual cells from the left edge
    /// - Desired number of visual cells from the left edge
    /// - Number of wrapped cells from the left edge
    /// - Desired number of wrapped cells from the left edge
    ///
    /// The difference between visual cells and wrapped cells is that
    /// visual cells are essentially "The distance a [`Point`] would
    /// be if this line were not wrapped"
    ///
    /// Desired cells are used when moving vertically, since when you
    /// move a [`Selection`] up or down to a shorter line, then to a
    /// longer one, you expect the horizontal position to hold. This
    /// is applied both in [full line] and [wrapped line] vertical
    /// movement.
    ///
    /// [full line]: crate::mode::Cursor::move_ver
    /// [wrapped line]: crate::mode::Cursor::move_ver_wrapped
    #[derive(Default, Clone, Copy, Debug, Eq, Encode, Decode)]
    pub struct VPoint {
        point: Point,
        // No plan to support lines that are far too long
        ccol: u16,
        vcol: u16,
        dvcol: u16,
        wcol: u16,
        dwcol: u16,
    }

    impl VPoint {
        /// Returns a new `VPoint` from scratch
        pub fn new(point: Point, ccol: u16, vcol: u16, dvcol: u16, wcol: u16, dwcol: u16) -> Self {
            Self { point, ccol, vcol, dvcol, wcol, dwcol }
        }

        /// The byte index of this [`VPoint`]
        pub fn byte(&self) -> usize {
            self.point.byte()
        }

        /// The char index of this [`VPoint`]
        pub fn char(&self) -> usize {
            self.point.char()
        }

        /// The line index of this [`VPoint`]
        pub fn line(&self) -> usize {
            self.point.line()
        }

        /// Number of characters from the start of the line
        pub fn char_col(&self) -> usize {
            self.ccol as usize
        }

        /// Total space from the start of the line
        pub fn visual_col(&self) -> usize {
            self.vcol as usize
        }

        /// How much space there should be from the start of the line
        pub fn desired_visual_col(&self) -> usize {
            self.dvcol as usize
        }

        /// Total space from the left edge
        pub fn wrapped_col(&self) -> usize {
            self.wcol as usize
        }

        /// How much space there should be from the left edge
        pub fn desired_wrapped_col(&self) -> usize {
            self.dwcol as usize
        }
    }

    #[allow(clippy::non_canonical_partial_ord_impl)]
    impl PartialOrd for VPoint {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.point.cmp(&other.point))
        }
    }

    impl Ord for VPoint {
        fn cmp(&self, other: &Self) -> Ordering {
            self.partial_cmp(other).unwrap()
        }
    }

    impl PartialEq for VPoint {
        fn eq(&self, other: &Self) -> bool {
            self.point == other.point
        }
    }

    impl std::fmt::Debug for Selection {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Selection")
                .field("caret", &*self.caret.lock().unwrap())
                .field("anchor", &*self.anchor.lock().unwrap())
                .field("change_i", &self.change_i)
                .finish()
        }
    }

    impl std::fmt::Debug for LazyVPoint {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Known(vp) => write!(f, "Known({:?}, {})", vp.point, vp.dwcol),
                Self::Unknown(p_or_b) => write!(f, "Unknown({p_or_b:?}"),
                Self::Desired { point, dvcol, dwcol } => {
                    write!(f, "Desired({point:?}, {dvcol}, {dwcol})")
                }
            }
        }
    }
}

impl std::fmt::Debug for Selections {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct DebugShiftState(Shift);
        impl std::fmt::Debug for DebugShiftState {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:?}", self.0)
            }
        }

        f.debug_struct("Selections")
            .field("buf", &self.buf)
            .field("main_i", &self.main_i)
            .field("shift_sate", &DebugShiftState(*self.shift.lock().unwrap()))
            .finish()
    }
}

#[derive(Default, Debug, Clone, Copy)]
struct Shift {
    from: usize,
    by: [i32; 3],
}
