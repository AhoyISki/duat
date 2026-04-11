//! A helper struct for [`Mode`]s with [`Selections`].
//!
//! This struct can edit [`Text`] in a declarative way, freeing the
//! [`Mode`]s from worrying about synchronization of the
//! selections and dealing with editing the text directly.
//!
//! [`Mode`]: super::Mode
use std::{
    cell::Cell,
    ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
};

pub use self::selections::{Selection, Selections, VPoint};
use crate::{
    buffer::{Buffer, BufferId, Change},
    opts::PrintOpts,
    text::{Matches, Point, RegexHaystack, RegexPattern, Strs, Text, TextIndex, TextRange},
    ui::{Area, Widget},
};

/// The [`Selection`] and [`Selections`] structs.
mod selections;

macro_rules! sel {
    ($sel:expr) => {
        $sel.selections[$sel.sels_i].as_ref().unwrap().selection
    };
}

macro_rules! sel_mut {
    ($sel:expr) => {{
        let mod_sel = $sel.selections[$sel.sels_i].as_mut().unwrap();
        mod_sel.has_changed = true;
        &mut mod_sel.selection
    }};
}

/// A selection that can edit [`Text`], but can't alter selections.
///
/// This struct will be used only inside functions passed to the
/// [`edit_*`] family of methods from the [`Handle`].
///
/// To make edits, you can use three different functions. You can,
/// those being [`replace`], [`insert`], and [`append`]. [`replace`]
/// will completely replace the [`Selection`]'s selection. [`insert`]
/// will place text behind the `cursor`, and [`append`] will place it
/// after the `cursor`.
///
/// You can also move the [`Selection`]'s selection in many different
/// ways, which are described below, in the `impl` section for this
/// struct.
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat::prelude::*;
/// # fn test(mut pa: Pass, handle: &mut Handle) {
/// let sel = handle.edit_main(&mut pa, |mut s| {
///     s.set_anchor();
///     s.set_cursor_on_end();
///     s.replace("my replacement");
///     s.append(" and my edit");
///
///     s.swap_ends();
///     s.insert("This is ");
///     s.swap_ends();
///
///     s.move_hor(" and my edit".chars().count() as i32);
///     s.set_anchor();
///     s.move_hor(-("This is my replacement and my edit".chars().count() as i32));
///     s.selection().to_string()
/// });
///
/// assert_eq!(&sel, "This is my replacement and my edit");
/// # }
/// ```
///
/// [`edit_*`]: crate::context::Handle::edit_nth
/// [`Handle`]: crate::context::Handle
/// [`replace`]: SelectionMut::replace
/// [`insert`]: SelectionMut::insert
/// [`append`]: SelectionMut::append
pub struct SelectionMut<'w, W: Widget + ?Sized = crate::buffer::Buffer> {
    selections: &'w mut Vec<Option<ModSelection>>,
    sels_i: usize,
    initial: Selection,
    widget: &'w mut W,
    area: &'w Area,
    next_i: Option<&'w Cell<usize>>,
}

impl<'w, W: Widget + ?Sized> SelectionMut<'w, W> {
    /// Returns a new instance of [`SelectionMut`].
    pub(crate) fn new(
        selections: &'w mut Vec<Option<ModSelection>>,
        sels_i: usize,
        (widget, area): (&'w mut W, &'w Area),
        next_i: Option<&'w Cell<usize>>,
    ) -> Self {
        let initial = selections[sels_i].as_ref().unwrap().selection.clone();
        Self {
            selections,
            sels_i,
            initial,
            widget,
            area,
            next_i,
        }
    }

    ////////// Text editing

    /// Replaces the entire selection with new text.
    ///
    /// If there is a selection, then it is treated as _inclusive_,
    /// therefore, a selection where `cursor == anchor` will remove the
    /// character where the cursor is. If there is no selection, then
    /// this has the same effect as [`insert`]. If you wish to
    /// append to the `cursor` instead, see [`append`].
    ///
    /// After replacing the sele tion, if the `cursor` is behind the
    /// `anchor` (or in the same spot), it will be placed on the start
    /// of the selection, while the `anchor` will be placed on the
    /// new end. If it is ahead, it will be placed ahead.
    ///
    /// [`insert`]: Self::insert
    /// [`append`]: Self::append
    pub fn replace(&mut self, edit: impl ToString) {
        let change = {
            let edit = edit.to_string();
            let range = sel!(self).point_range(self.widget.text());
            let (p0, p1) = (range.start, range.end);
            let p1 = if self.anchor().is_some() { p1 } else { p0 };
            Change::new(edit, p0..p1, self.widget.text())
        };

        // Disconsider null changes.
        if change.added_str().len() < 10 && change.added_str() == change.taken_str() {
            return;
        }

        let (start, end) = (change.start(), change.added_end());

        self.edit(change.clone());

        let anchor_was_on_start = self.anchor_is_start();
        self.move_to(start..end);
        if !anchor_was_on_start {
            self.set_cursor_on_start();
        }
    }

    /// Inserts new text directly behind the `cursor`.
    ///
    /// If the `anchor` is ahead of the `cursor`, it will move forwards
    /// by the number of chars in the new text.
    ///
    /// If you wish to replace the selected text, see [`replace`], if
    /// you want to append after the `cursor` instead, see [`append`]
    ///
    /// [`replace`]: Self::replace
    /// [`append`]: Self::append
    pub fn insert(&mut self, edit: impl ToString) {
        let cursor_point = sel!(self).cursor();
        let range = cursor_point..cursor_point;
        let change = Change::new(edit.to_string(), range, self.widget.text());
        let (added, taken) = (change.added_end(), change.taken_end());

        self.edit(change);

        if let Some(anchor) = sel!(self).anchor()
            && anchor > sel!(self).cursor()
        {
            let new_anchor = anchor + added - taken;
            sel_mut!(self).swap_ends();
            sel_mut!(self).move_to(new_anchor, self.widget.text());
            sel_mut!(self).swap_ends();
        }
    }

    /// Appends new text directly after the `cursor`.
    ///
    /// If the `anchor` is ahead of the `cursor`, it will move forwards
    /// by the number of chars in the new text.
    ///
    /// If you wish to replace the selected text, see [`replace`], if
    /// you want to insert before the `cursor` instead, see [`insert`]
    ///
    /// [`replace`]: Self::replace
    /// [`insert`]: Self::insert
    pub fn append(&mut self, edit: impl ToString) {
        let cursor = sel!(self).cursor();
        let after = cursor.fwd(self.widget.text().char_at(cursor).unwrap());
        let change = Change::new(edit.to_string(), after..after, self.widget.text());
        let (added, taken) = (change.added_end(), change.taken_end());

        self.edit(change);

        if let Some(anchor) = sel!(self).anchor()
            && anchor > after
        {
            let new_anchor = anchor + added - taken;
            sel_mut!(self).swap_ends();
            sel_mut!(self).move_to(new_anchor, self.widget.text());
            sel_mut!(self).swap_ends();
        }
    }

    /// Edits the buffer with a [`Change`].
    fn edit(&mut self, change: Change<'static, String>) {
        let mut text = self.widget.text_mut();
        let (change_i, selections_taken) =
            text.apply_change(sel!(self).change_i.map(|i| i as usize), change);
        sel_mut!(self).change_i = change_i.map(|i| i as u32);

        // The Change may have happened before the index of the next curossr,
        // so we need to account for that.
        if let Some(change_i) = change_i
            && let Some(next_i) = self.next_i.as_ref()
            && change_i <= next_i.get()
        {
            next_i.set(next_i.get().saturating_sub(selections_taken));
        }
    }

    ////////// Movement functions

    /// Moves the selection horizontally. May cause vertical movement.
    ///
    /// Returns the distance traveled, in character indices
    #[track_caller]
    pub fn move_hor(&mut self, by: i32) -> i32 {
        if by == 0 {
            return 0;
        }
        sel_mut!(self).move_hor(by, self.widget.text())
    }

    /// Moves the selection vertically. May cause horizontal movement.
    ///
    /// Returns `true` if the cursor actually moved at all.
    #[track_caller]
    pub fn move_ver(&mut self, by: i32) -> bool {
        if by == 0 {
            return false;
        }
        sel_mut!(self).move_ver(by, self.widget.text(), self.area, self.widget.print_opts())
    }

    /// Moves the selection vertically a number of wrapped lines. May
    /// cause horizontal movement.
    ///
    /// Returns `true` if the cursor actually moved at all.
    #[track_caller]
    pub fn move_ver_wrapped(&mut self, count: i32) -> bool {
        if count == 0 {
            return false;
        }
        sel_mut!(self).move_ver_wrapped(
            count,
            self.widget.text(),
            self.area,
            self.widget.print_opts(),
        )
    }

    /// Moves the selection to a [`Point`] or a [range] of [`Point`]s.
    ///
    /// If you give it just a [`Point`], it will move the cursor,
    /// without affecting the anchor. If you give it a [range] of
    /// [`Point`]s, the anchor will be placed at the start, while the
    /// cursor will be placed at the end of said [range]. You can flip
    /// those positions with a function like [`swap_ends`].
    ///
    /// If a [`Point`] is not valid, it will be corrected and clamped
    /// to the lenght of the [`Text`].
    ///
    /// [range]: std::ops::RangeBounds
    /// [`swap_ends`]: Self::swap_ends
    #[track_caller]
    pub fn move_to(&mut self, point_or_points: impl CaretOrRange) {
        point_or_points.move_to(self);
    }

    /// Moves the selection to [`Point::default`], i.s., the start of
    /// the [`Text`].
    #[track_caller]
    pub fn move_to_start(&mut self) {
        sel_mut!(self).move_to(Point::default(), self.widget.text());
    }

    /// Moves the selection to a `line` and a `column`.
    ///
    /// - If the coords isn't valid, it will move to the "maximum"
    ///   position allowed.
    #[track_caller]
    pub fn move_to_coords(&mut self, line: usize, column: usize) {
        let point = self.text().point_at_coords(line, column);
        self.move_to(point);
    }

    /// Moves to a column on the current line.
    #[track_caller]
    pub fn move_to_col(&mut self, column: usize) {
        self.move_to_coords(self.cursor().line(), column);
    }

    /// Returns and takes the anchor of the [`Selection`], if there
    /// was one.
    pub fn unset_anchor(&mut self) -> Option<Point> {
        sel_mut!(self).unset_anchor()
    }

    /// Sets the `anchor` to the current `cursor`.
    pub fn set_anchor(&mut self) {
        sel_mut!(self).set_anchor()
    }

    /// Sets the `anchor` if it was not already set.
    ///
    /// Returns `true` if the anchor was set by this command.
    pub fn set_anchor_if_needed(&mut self) -> bool {
        if self.anchor().is_none() {
            sel_mut!(self).set_anchor();
            true
        } else {
            false
        }
    }

    /// Swaps the position of the `cursor` and `anchor`.
    pub fn swap_ends(&mut self) {
        sel_mut!(self).swap_ends();
    }

    /// Sets the cursor of the [`Selection`] on the start of the
    /// selection.
    ///
    /// Returns `true` if a swap occurred
    pub fn set_cursor_on_start(&mut self) -> bool {
        if let Some(anchor) = self.anchor()
            && anchor < self.cursor()
        {
            self.swap_ends();
            true
        } else {
            false
        }
    }

    /// Sets the cursor of the [`Selection`] on the end of the
    /// selection.
    ///
    /// Returns `true` if a swap occurred
    pub fn set_cursor_on_end(&mut self) -> bool {
        if let Some(anchor) = self.anchor()
            && anchor > self.cursor()
        {
            self.swap_ends();
            true
        } else {
            false
        }
    }

    ////////// Selection meta manipulation

    /// Resets the [`Selection`] to how it was before being modified.
    pub fn reset(&mut self) {
        *sel_mut!(self) = self.initial.clone();
    }

    /// Copies the current [`Selection`] in place.
    ///
    /// This will leave an additional [`Selection`] with the current
    /// selection. Do note that normal intersection rules apply, so if
    /// at the end of the movement, this selection intersects with any
    /// other, they will be merged into one.
    ///
    /// When this [`SelectionMut`] is dropped, like with normal
    /// [`SelectionMut`]s, its [`Selection`] will be added to the
    /// [`Selections`], unless you [destroy] it.
    ///
    /// [destroy]: Self::destroy
    pub fn copy(&mut self) -> SelectionMut<'_, W> {
        let copy = self.selections[self.sels_i].clone().unwrap();
        self.selections
            .push(Some(ModSelection { was_main: false, ..copy }));

        let sels_i = self.selections.len() - 1;
        SelectionMut::new(
            self.selections,
            sels_i,
            (self.widget, self.area),
            self.next_i,
        )
    }

    /// Destroys the current [`Selection`].
    ///
    /// Will not destroy it if it is the last [`Selection`] left
    ///
    /// If this was the main selection, the main selection will now be
    /// the selection immediately behind it.
    pub fn destroy(self) {
        // If there are other Selections in the list, or other copies still
        // lying around, the SelectionMut Selection can be destroyed.
        if self.widget.text().selections().is_empty()
            && self.selections.iter().flatten().count() <= 1
        {
            return;
        }

        if self.selections[self.sels_i].as_ref().unwrap().was_main {
            // If there were no selections on the list, the previous one added
            // will be set as the main selection.
            if self.widget.text().selections().is_empty() {
                let len = self.selections.len();
                let ranges = self
                    .selections
                    .get_disjoint_mut([self.sels_i..len, 0..self.sels_i])
                    .unwrap();

                let next_sel = ranges.into_iter().flatten().rev().flatten().next().unwrap();
                next_sel.was_main = true;
            } else {
                self.widget.text_mut().selections_mut().rotate_main(-1);
            }
        }

        self.selections[self.sels_i] = None;
    }

    /// Sets the "desired visual column".
    ///
    /// The desired visual column determines at what point in a line
    /// the cursor will be placed when moving [up and down] through
    /// lines of varying lengths.
    ///
    /// Will also set the "desired wrapped visual column", which is
    /// the same thing but used when moving vertically in a [wrapped]
    /// fashion.
    ///
    /// [up and down]: SelectionMut::move_ver
    /// [wrapped]: SelectionMut::move_ver_wrapped
    pub fn set_desired_vcol(&mut self, x: usize) {
        sel_mut!(self).set_desired_cols(x, x);
    }

    ////////// Iteration functions

    /// Wether the current selection matches a regex pattern.
    #[track_caller]
    pub fn matches_pat<R: RegexPattern>(&self, pat: R) -> bool {
        let range = sel!(self).byte_range(self.widget.text());
        match self.widget.text()[range].matches_pat(pat) {
            Ok(result) => result,
            Err(err) => panic!("{err}"),
        }
    }

    /// Returns an [`Iterator`] over the matches of a
    /// [`RegexPattern`].
    ///
    /// This `Iterator` normally covers the entire range of the
    /// [`Text`], however, there are methods that you can use to
    /// narrow it down to ranges relative to the `SelectionMut`'s
    /// [`cursor`].
    ///
    /// For example, [`SelectionMutMatches::from_cursor`] will narrow
    /// the searched range from the beginning of the cursor's
    /// `char` all the way until the end of the [`Text`].
    ///
    /// This `Iterator` also implements [`DoubleEndedIterator`], which
    /// means you can search in reverse as well.
    ///
    /// [`cursor`]: Self::cursor
    #[track_caller]
    pub fn search<R: RegexPattern>(&self, pat: R) -> SelectionMutMatches<'_, R> {
        let text = self.widget.text();
        let cursor = self.cursor();
        SelectionMutMatches {
            text_byte_len: text.len(),
            cursor_range: cursor.byte()..cursor.fwd(self.char()).byte(),
            matches: text.search(pat),
        }
    }

    ////////// Text queries

    /// Returns the [`char`] in the `cursor`.
    pub fn char(&self) -> char {
        self.text().char_at(sel!(self).cursor()).unwrap()
    }

    /// Returns the [`char`] at a given [`Point`].
    pub fn char_at(&self, i: impl TextIndex) -> Option<char> {
        self.text().char_at(i)
    }

    /// Returns the [`Selection`]'s selection.
    ///
    /// The reason why this return value is `IntoIter<&str, 2>` is
    /// because the [`Text`] utilizes an underlying [`GapBuffer`]
    /// to store the characters. This means that the text is
    /// always separated into two distinct chunks.
    ///
    /// If this [`Selection`]'s selection happens to be entirely
    /// within one of these chunks, the other `&str` will just be
    /// empty.
    ///
    /// [`GapBuffer`]: gap_buf::GapBuffer
    pub fn selection(&self) -> &Strs {
        let range = sel!(self).byte_range(self.text());
        &self.text()[range]
    }

    /// Returns the [`Strs`] for the given [`TextRange`].
    ///
    /// # Panics
    ///
    /// Panics if the range doesn't start and end in valid utf8
    /// boundaries. If you'd like to handle that scenario, check out
    /// [`SelectionMut::try_strs`].
    #[track_caller]
    pub fn strs(&self, range: impl TextRange) -> &Strs {
        &self.widget.text()[range]
    }

    /// Returns the [`Strs`] for the given [`TextRange`].
    ///
    /// It will return [`None`] if the range does not start or end in
    /// valid utf8 boundaries. If you expect the value to alway be
    /// `Some`, consider [`SelectionMut::strs`] isntead.
    pub fn try_strs(&self, range: impl TextRange) -> Option<&Strs> {
        self.widget.text().get(range)
    }

    /// Returns the length of the [`Text`], in [`Point`].
    pub fn len(&self) -> Point {
        self.text().end_point()
    }

    /// Returns the position of the last [`char`] if there is one.
    pub fn last_point(&self) -> Point {
        self.text().last_point()
    }

    /// Gets the current level of indentation.
    pub fn indent(&self) -> usize {
        self.widget
            .text()
            .line(self.cursor().line())
            .indent(self.opts())
    }

    /// Gets the indentation level on a given line.
    ///
    /// This is the total "amount of spaces", that is, how many `' '`
    /// character equivalents are here. This depends on your
    /// [`PrintOpts`] because of the `tabstop` field.
    #[track_caller]
    pub fn indent_on(&self, line: usize) -> usize {
        self.widget.text().line(line).indent(self.opts())
    }

    ////////// Selection queries

    /// Returns the `cursor`.
    pub fn cursor(&self) -> Point {
        sel!(self).cursor()
    }

    /// Returns the `anchor`.
    pub fn anchor(&self) -> Option<Point> {
        sel!(self).anchor()
    }

    /// The [`Point`] range of the [`Selection`].
    ///
    /// This is an _inclusive_ range (not Rust's [`RangeInclusive`]
    /// however), this means that, even if there is no anchor, the
    /// lenght of this range will always be at least 1.
    ///
    /// If you want an exclusive range, see
    /// [`SelectionMut::range_excl`].
    ///
    /// [`RangeInclusive`]: std::ops::RangeInclusive
    pub fn range(&self) -> Range<Point> {
        sel!(self).point_range(self.text())
    }

    /// An exclusive [`Point`] range of the [`Selection`].
    ///
    /// If you wish for an inclusive range, whose length is always
    /// greater than or equal to 1, see [`RangeInclusive`].
    pub fn range_excl(&self) -> Range<Point> {
        sel!(self).point_range_excl()
    }

    /// The [`VPoint`] range of the [`Selection`].
    ///
    /// Use only if you need the things that the [`VPoint`] provides,
    /// in order to preven extraneous calculations.
    pub fn v_cursor(&self) -> VPoint {
        sel!(self).v_cursor(self.widget.text(), self.area, self.widget.print_opts())
    }

    /// The [`VPoint`] of the anchor, if it exists.
    ///
    /// Use only if you need the things that the [`VPoint`] provides,
    /// in order to preven extraneous calculations.
    pub fn v_anchor(&self) -> Option<VPoint> {
        sel!(self).v_anchor(self.widget.text(), self.area, self.widget.print_opts())
    }

    /// Returns `true` if the `anchor` exists before the `cursor`.
    pub fn anchor_is_start(&self) -> bool {
        self.anchor().is_none_or(|anchor| anchor <= self.cursor())
    }

    /// Whether or not this is the main [`Selection`].
    pub fn is_main(&self) -> bool {
        self.selections[self.sels_i].as_ref().unwrap().was_main
    }

    /// The [`Text`] of the [`Widget`].
    pub fn text(&self) -> &Text {
        self.widget.text()
    }

    /// The [`PrintOpts`] in use.
    pub fn opts(&self) -> PrintOpts {
        self.widget.print_opts()
    }

    /// The [`Widget`] being modified.
    pub fn widget(&self) -> &W {
        self.widget
    }
}

impl SelectionMut<'_, Buffer> {
    /// A unique identifier for this [`Buffer`].
    ///
    /// This is more robust than identifying it by its path or name,
    /// or even [`PathKind`], since those could change, but this
    /// cannot.
    ///
    /// [`PathKind`]: crate::buffer::PathKind
    pub fn buffer_id(&self) -> BufferId {
        self.widget.buffer_id()
    }
}

impl<'a, W: Widget + ?Sized> std::fmt::Debug for SelectionMut<'a, W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SelectionMut")
            .field("selection", &sel!(self))
            .finish_non_exhaustive()
    }
}

/// An [`Iterator`] over the matches of a [`RegexPattern`].
///
/// This `Iterator` comes from searching from a [`SelectionMut`].
/// Because of that, it has methods for added convenience of search.
/// The methods [`to_cursor`], [`to_cursor_incl`], [`from_cursor`] and
/// [`from_cursor_excl`] will change the [`Range`] of searching to one
/// starting or ending on the `SelectionMut`'s [`cursor`].
///
/// [`to_cursor`]: SelectionMutMatches::to_cursor
/// [`to_cursor_incl`]: SelectionMutMatches::to_cursor_incl
/// [`from_cursor`]: SelectionMutMatches::from_cursor
/// [`from_cursor_excl`]: SelectionMutMatches::from_cursor_excl
/// [`cursor`]: SelectionMut::cursor
pub struct SelectionMutMatches<'s, R: RegexPattern> {
    text_byte_len: usize,
    cursor_range: Range<usize>,
    matches: Matches<'s, R>,
}

impl<'s, R: RegexPattern> SelectionMutMatches<'s, R> {
    /// Changes the [`TextRange`] to search on.
    ///
    /// This _will_ reset the [`Iterator`], if it was returning
    /// [`None`] before, it might start returning [`Some`] again if
    /// the pattern exists in the specified [`Range`]
    pub fn range(self, range: impl TextRange) -> Self {
        Self {
            matches: self.matches.range(range),
            ..self
        }
    }

    /// Searches over a range from the start of the cursor to the end
    /// of the [`Text`].
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # use duat::prelude::*;
    /// fn search_nth(pa: &mut Pass, handle: &Handle, n: usize, pat: &str) {
    ///     handle.edit_all(pa, |mut s| {
    ///         let mut nth = s.search(pat).from_cursor().nth(n);
    ///         if let Some(range) = nth {
    ///             s.move_to(range);
    ///         }
    ///     })
    /// }
    /// ```
    #[allow(clippy::wrong_self_convention)]
    pub fn from_cursor(self) -> Self {
        Self {
            matches: self
                .matches
                .range(self.cursor_range.start..self.text_byte_len),
            ..self
        }
    }

    /// Searches over a range from the end of the cursor to the end
    /// of the [`Text`].
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # use duat::prelude::*;
    /// fn next_paren_match(pa: &mut Pass, handle: &Handle) {
    ///     handle.edit_all(pa, |mut s| {
    ///         let mut start_count = 0;
    ///         let mut start_bound = None;
    ///         let end_bound = s
    ///             .search([r"\(", r"\)"])
    ///             .from_cursor_excl()
    ///             .find(|(id, range)| {
    ///                 start_count += ((*id == 0) as u32).saturating_sub((*id == 1) as u32);
    ///                 start_bound = (*id == 0 && start_count == 0).then_some(range.clone());
    ///                 start_count == 0 && *id == 1
    ///             });
    ///
    ///         if let (Some(start), Some((_, end))) = (start_bound, end_bound) {
    ///             s.move_to(start.start..end.end);
    ///         }
    ///     })
    /// }
    /// ```
    #[allow(clippy::wrong_self_convention)]
    pub fn from_cursor_excl(self) -> Self {
        Self {
            matches: self.matches.range(self.cursor_range.end..self.text_byte_len),
            ..self
        }
    }

    /// Searches over a range from the start of the [`Text`] to the
    /// start of the cursor's char.
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # use duat::prelude::*;
    /// fn remove_prefix(pa: &mut Pass, handle: &Handle) {
    ///     let prefix_pat = format!(r"{}*\z", handle.opts(pa).word_chars_regex());
    ///     handle.edit_all(pa, |mut s| {
    ///         let prefix_range = s.search(&prefix_pat).to_cursor().rev().next();
    ///         if let Some(range) = prefix_range {
    ///             s.move_to(range);
    ///             s.replace("");
    ///         }
    ///     })
    /// }
    /// ```
    #[allow(clippy::wrong_self_convention)]
    pub fn to_cursor(self) -> Self {
        Self {
            matches: self.matches.range(0..self.cursor_range.start),
            ..self
        }
    }

    /// Searches over a range from the start of the [`Text`] to the
    /// end of the cursor's char.
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # use duat::prelude::*;
    /// fn last_word_in_selection(pa: &mut Pass, handle: &Handle) {
    ///     let word_pat = format!(r"{}+", handle.opts(pa).word_chars_regex());
    ///     handle.edit_all(pa, |mut s| {
    ///         s.set_cursor_on_end();
    ///         let mut nth = s.search(&word_pat).to_cursor_incl().rev().next();
    ///         if let Some(range) = nth {
    ///             s.move_to(range)
    ///         } else {
    ///             s.reset()
    ///         }
    ///     })
    /// }
    /// ```
    #[allow(clippy::wrong_self_convention)]
    pub fn to_cursor_incl(self) -> Self {
        Self {
            matches: self.matches.range(0..self.cursor_range.end),
            ..self
        }
    }
}

impl<'s, R: RegexPattern> Iterator for SelectionMutMatches<'s, R> {
    type Item = R::Match;

    fn next(&mut self) -> Option<Self::Item> {
        self.matches.next()
    }
}

impl<'s, R: RegexPattern> DoubleEndedIterator for SelectionMutMatches<'s, R> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.matches.next_back()
    }
}

/// Does an action on every [`SelectionMut`].
pub(crate) fn on_each_sel<W: Widget + ?Sized>(
    widget: &mut W,
    area: &Area,
    mut func: impl FnMut(SelectionMut<W>),
) {
    let mut current = Vec::new();
    let mut next_i = Cell::new(0);

    while let Some((sel, was_main)) = widget.text_mut().selections_mut().remove(next_i.get()) {
        current.push(Some(ModSelection::new(sel, next_i.get(), was_main)));

        func(SelectionMut::new(
            &mut current,
            0,
            (widget, area),
            Some(&next_i),
        ));

        reinsert_selections(current.drain(..).flatten(), widget, Some(next_i.get_mut()));
    }
}

/// Reinsert edited [`Selections`].
#[inline]
pub(crate) fn reinsert_selections(
    mod_sels: impl Iterator<Item = ModSelection>,
    widget: &mut (impl Widget + ?Sized),
    mut next_i: Option<&mut usize>,
) {
    for mod_sel in mod_sels {
        let ([inserted_i, selections_taken], last_selection_overhangs) = widget
            .text_mut()
            .selections_mut()
            .insert(mod_sel.index, mod_sel.selection, mod_sel.was_main);

        if let Some(next_i) = next_i.as_mut()
            && inserted_i <= **next_i
        {
            let go_to_next = !last_selection_overhangs as usize;
            **next_i = next_i.saturating_sub(selections_taken).max(inserted_i) + go_to_next;
        }
    }
}

/// A struct representing the temporary state of a [`Selection`] in a.
/// [`SelectionMut`]
#[derive(Clone, Debug)]
pub(crate) struct ModSelection {
    selection: Selection,
    index: usize,
    was_main: bool,
    has_changed: bool,
}

impl ModSelection {
    /// Returns a new `ModSelection`.
    pub(crate) fn new(selection: Selection, index: usize, was_main: bool) -> Self {
        Self {
            selection,
            index,
            was_main,
            has_changed: false,
        }
    }
}

/// A position that a [`SelectionMut`] can move to.
///
/// This will come either in the form of [`Point`]s or byte indices
/// (in the form of `usize`). It can be a single value, like
/// `Point::default()` or `3`, in which case only the [cursor] will
/// move, not affecting the [anchor].
///
/// Or it could be a [range], like `p1..p2` or `..=1000`, in which
/// case the cursor will be placed at the end, while the anchor will be
/// placed at the start.
///
/// [cursor]: SelectionMut::cursor
/// [anchor]: SelectionMut::anchor
/// [range]: std::ops::RangeBounds
pub trait CaretOrRange {
    /// Internal movement function for monomorphization
    #[doc(hidden)]
    fn move_to<W: Widget + ?Sized>(self, sel: &mut SelectionMut<'_, W>);
}

impl CaretOrRange for Point {
    #[track_caller]
    fn move_to<W: Widget + ?Sized>(self, sel: &mut SelectionMut<'_, W>) {
        sel_mut!(sel).move_to(self, sel.widget.text());
    }
}

impl CaretOrRange for usize {
    #[track_caller]
    fn move_to<W: Widget + ?Sized>(self, sel: &mut SelectionMut<'_, W>) {
        sel_mut!(sel).move_to(sel.widget.text().point_at_byte(self), sel.widget.text())
    }
}

impl<Idx: TextIndex> CaretOrRange for Range<Idx> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized>(self, sel: &mut SelectionMut<'_, W>) {
        let range = self.start.to_byte_index()..self.end.to_byte_index();
        assert!(
            range.start <= range.end,
            "slice index start is larger than end"
        );

        sel_mut!(sel).move_to(range.start, sel.widget.text());
        if range.start < range.end {
            sel.set_anchor();
            sel_mut!(sel).move_to(range.end, sel.widget.text());
            if range.end < sel.widget.text().len() {
                sel.move_hor(-1);
            }
        } else {
            sel.unset_anchor();
        }
    }
}

impl<Idx: TextIndex> CaretOrRange for RangeInclusive<Idx> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized>(self, sel: &mut SelectionMut<'_, W>) {
        let range = self.start().to_byte_index()..=self.end().to_byte_index();
        assert!(
            range.start() <= range.end(),
            "slice index start is larger than end"
        );

        sel_mut!(sel).move_to(*range.start(), sel.widget.text());
        sel.set_anchor();
        sel_mut!(sel).move_to(*range.end(), sel.widget.text());
    }
}

impl<Idx: TextIndex> CaretOrRange for RangeFrom<Idx> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized>(self, sel: &mut SelectionMut<'_, W>) {
        let start = self.start.to_byte_index();
        sel_mut!(sel).move_to(start, sel.widget.text());
        if start < sel.text().len() {
            sel.set_anchor();
            sel_mut!(sel).move_to(sel.widget.text().len(), sel.widget.text());
            sel.move_hor(-1);
        } else {
            sel.unset_anchor();
        }
    }
}

impl<Idx: TextIndex> CaretOrRange for RangeTo<Idx> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized>(self, sel: &mut SelectionMut<'_, W>) {
        let end = self.end.to_byte_index().min(sel.text().last_point().byte());
        sel.move_to_start();
        if 0 < end {
            sel.set_anchor();
            sel_mut!(sel).move_to(end, sel.widget.text());
            sel.move_hor(-1);
        } else {
            sel.unset_anchor();
        }
    }
}

impl<Idx: TextIndex> CaretOrRange for RangeToInclusive<Idx> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized>(self, sel: &mut SelectionMut<'_, W>) {
        sel.move_to_start();
        sel.set_anchor();
        sel_mut!(sel).move_to(self.end, sel.widget.text());
    }
}

impl CaretOrRange for RangeFull {
    #[track_caller]
    fn move_to<W: Widget + ?Sized>(self, sel: &mut SelectionMut<'_, W>) {
        sel.move_to_start();
        sel.set_anchor();
        sel_mut!(sel).move_to(sel.widget.text().len(), sel.widget.text());
    }
}
