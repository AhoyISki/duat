//! A helper struct for [`Mode`]s with [`Selections`]
//!
//! This struct can edit [`Text`] in a declarative way, freeing the
//! [`Mode`]s from worrying about synchronization of the
//! selections and dealing with editing the text directly.
//!
//! [`Mode`]: super::Mode
use std::{
    cell::{Cell, RefMut},
    ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
    rc::Rc,
};

use lender::{Lender, Lending};

pub use self::selections::{Selection, Selections, VPoint};
use crate::{
    buffer::{Buffer, Parser},
    opts::PrintOpts,
    text::{Change, Lines, Point, RegexPattern, Searcher, Strs, Text, TextRange},
    ui::{Area, Widget},
};

/// The [`Selection`] and [`Selections`] structs
mod selections;

/// A selection that can edit [`Text`], but can't alter selections
///
/// This struct will be used only inside functions passed to the
/// [`edit_*`] family of methods from the [`Handle`].
///
/// To make edits, you can use three different functions. You can,
/// those being [`replace`], [`insert`], and [`append`]. [`replace`]
/// will completely replace the [`Selection`]'s selection. [`insert`]
/// will place text behind the `caret`, and [`append`] will place it
/// after the `caret`.
///
/// You can also move the [`Selection`]'s selection in many different
/// ways, which are described below, in the `impl` section for this
/// struct.
///
/// ```rust
/// # use duat_core::prelude::*;
/// # fn test<U: Ui, S>(mut pa: Pass, handle: &mut Handle<Buffer<U>, U, S>) {
/// let sel: String = handle.edit_main(&mut pa, |mut c| {
///     c.set_anchor();
///     c.set_caret_on_end();
///     c.replace("my replacement");
///     c.append(" and my edit");
///
///     c.swap_ends();
///     c.insert("This is ");
///     c.swap_ends();
///
///     c.move_hor(" and my edit".chars().count() as i32);
///     c.set_anchor();
///     c.move_hor(-("This is my replacement and my edit".chars().count() as i32));
///     c.selection().into_iter().collect()
/// });
///
/// assert_eq!(&sel, "This is my replacement and my edit");
/// # }
/// ```
///
/// [`edit_*`]: crate::context::Handle::edit_nth
/// [`Handle`]: crate::context::Handle
/// [`replace`]: Cursor::replace
/// [`insert`]: Cursor::insert
/// [`append`]: Cursor::append
pub struct Cursor<'a, W: Widget + ?Sized = crate::buffer::Buffer, S = ()> {
    initial: Selection,
    selection: Selection,
    n: usize,
    was_main: bool,
    widget: &'a mut W,
    area: &'a Area,
    next_i: Option<Rc<Cell<usize>>>,
    inc_searcher: &'a mut S,
    is_copy: bool,
}

impl<'a, W: Widget + ?Sized, S> Cursor<'a, W, S> {
    /// Returns a new instance of [`Cursor`]
    pub(crate) fn new(
        (selection, n, was_main): (Selection, usize, bool),
        (widget, area): (&'a mut W, &'a Area),
        next_i: Option<Rc<Cell<usize>>>,
        searcher: &'a mut S,
        is_copy: bool,
    ) -> Self {
        Self {
            initial: selection.clone(),
            selection,
            n,
            was_main,
            widget,
            area,
            next_i,
            inc_searcher: searcher,
            is_copy,
        }
    }

    ////////// Text editing

    /// Replaces the entire selection with new text
    ///
    /// If there is a selection, then it is treated as _inclusive_,
    /// therefore, a selection where `caret == anchor` will remove the
    /// character where the caret is. If there is no selection, then
    /// this has the same effect as [`insert`]. If you wish to
    /// append to the `caret` instead, see [`append`].
    ///
    /// After replacing the sele tion, if the `caret` is behind the
    /// `anchor` (or in the same spot), it will be placed on the start
    /// of the selection, while the `anchor` will be placed on the
    /// new end. If it is ahead, it will be placed ahead.
    ///
    /// [`insert`]: Self::insert
    /// [`append`]: Self::append
    pub fn replace(&mut self, edit: impl ToString) {
        let change = {
            let edit = edit.to_string();
            let range = self.selection.point_range(self.widget.text());
            let (p0, p1) = (range.start, range.end);
            let p1 = if self.anchor().is_some() { p1 } else { p0 };
            Change::new(edit, p0..p1, self.widget.text())
        };

        // Disconsider null changes.
        if change.added_str().len() < 10 && change.added_str() == change.taken_str() {
            return;
        }

        let (start, end) = (change.start(), change.added_end());

        self.edit(change);

        let anchor_was_on_start = self.anchor_is_start();
        self.move_to(start..end);
        if !anchor_was_on_start {
            self.set_caret_on_start();
        }
    }

    /// Inserts new text directly behind the `caret`
    ///
    /// If the `anchor` is ahead of the `caret`, it will move forwards
    /// by the number of chars in the new text.
    ///
    /// If you wish to replace the selected text, see [`replace`], if
    /// you want to append after the `caret` instead, see [`append`]
    ///
    /// [`replace`]: Self::replace
    /// [`append`]: Self::append
    pub fn insert(&mut self, edit: impl ToString) {
        let range = self.selection.caret()..self.selection.caret();
        let change = Change::new(edit.to_string(), range, self.widget.text());
        let (added, taken) = (change.added_end(), change.taken_end());

        self.edit(change);

        if let Some(anchor) = self.selection.anchor()
            && anchor > self.selection.caret()
        {
            let new_anchor = anchor + added - taken;
            self.selection.swap_ends();
            self.selection.move_to(new_anchor, self.widget.text());
            self.selection.swap_ends();
        }
    }

    /// Appends new text directly after the `caret`
    ///
    /// If the `anchor` is ahead of the `caret`, it will move forwards
    /// by the number of chars in the new text.
    ///
    /// If you wish to replace the selected text, see [`replace`], if
    /// you want to insert before the `caret` instead, see [`insert`]
    ///
    /// [`replace`]: Self::replace
    /// [`insert`]: Self::insert
    pub fn append(&mut self, edit: impl ToString) {
        let caret = self.selection.caret();
        let p = caret.fwd(self.widget.text().char_at(caret).unwrap());
        let change = Change::new(edit.to_string(), p..p, self.widget.text());
        let (added, taken) = (change.added_end(), change.taken_end());

        self.edit(change);

        if let Some(anchor) = self.selection.anchor()
            && anchor > p
        {
            let new_anchor = anchor + added - taken;
            self.selection.swap_ends();
            self.selection.move_to(new_anchor, self.widget.text());
            self.selection.swap_ends();
        }
    }

    /// Edits the buffer with a [`Change`]
    fn edit(&mut self, change: Change<'static, String>) {
        let text = self.widget.text_mut();
        let (change_i, selections_taken) =
            text.apply_change(self.selection.change_i.map(|i| i as usize), change);
        self.selection.change_i = change_i.map(|i| i as u32);

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

    /// Moves the selection horizontally. May cause vertical movement
    ///
    /// Returns the distance moved in chars.
    pub fn move_hor(&mut self, count: i32) -> i32 {
        self.selection.move_hor(count, self.widget.text())
    }

    /// Moves the selection vertically. May cause horizontal movement
    ///
    /// Returns the distance moved in lines.
    pub fn move_ver(&mut self, count: i32) -> i32 {
        self.selection.move_ver(
            count,
            self.widget.text(),
            self.area,
            self.widget.get_print_opts(),
        )
    }

    /// Moves the selection vertically a number of wrapped lines. May
    /// cause horizontal movement
    ///
    /// Returns the distance moved in wrapped lines.
    pub fn move_ver_wrapped(&mut self, count: i32) {
        self.selection.move_ver_wrapped(
            count,
            self.widget.text(),
            self.area,
            self.widget.get_print_opts(),
        );
    }

    /// Moves the selection to a [`Point`] or a [range] of [`Point`]s
    ///
    /// If you give it just a [`Point`], it will move the caret,
    /// without affecting the anchor. If you give it a [range] of
    /// [`Point`]s, the anchor will be placed at the start, while the
    /// caret will be placed at the end of said [range]. You can flip
    /// those positions with a function like [`swap_ends`].
    ///
    /// If a [`Point`] is not valid, it will be corrected and clamped
    /// to the lenght of the [`Text`].
    ///
    /// [range]: std::ops::RangeBounds
    /// [`swap_ends`]: Self::swap_ends
    pub fn move_to(&mut self, point_or_points: impl PointOrPoints) {
        point_or_points.move_to(self);
    }

    /// Moves the selection to [`Point::default`], i.e., the start of
    /// the [`Text`]
    pub fn move_to_start(&mut self) {
        self.selection.move_to(Point::default(), self.widget.text());
    }

    /// Moves the selection to a `line` and a `column`
    ///
    /// - If the coords isn't valid, it will move to the "maximum"
    ///   position allowed.
    pub fn move_to_coords(&mut self, line: usize, col: usize) {
        let [s, e] = self
            .text()
            .points_of_line(line.min(self.text().last_point().line()));
        let (p, _) = self
            .text()
            .chars_fwd(s..e)
            .unwrap()
            .take(col + 1)
            .last()
            .unzip();
        self.move_to(p.unwrap_or(e));
    }

    /// Moves to a column on the current line
    pub fn move_to_col(&mut self, col: usize) {
        let line = self.text().point_at_line(self.caret().line()).line();
        self.move_to_coords(line, col);
    }

    /// Returns and takes the anchor of the [`Selection`].
    pub fn unset_anchor(&mut self) -> Option<Point> {
        self.selection.unset_anchor()
    }

    /// Sets the `anchor` to the current `caret`
    pub fn set_anchor(&mut self) {
        self.selection.set_anchor()
    }

    /// Sets the `anchor` if it was not already set
    ///
    /// Returns `true` if the anchor was set by this command.
    pub fn set_anchor_if_needed(&mut self) -> bool {
        if self.anchor().is_none() {
            self.selection.set_anchor();
            true
        } else {
            false
        }
    }

    /// Swaps the position of the `caret` and `anchor`
    pub fn swap_ends(&mut self) {
        self.selection.swap_ends();
    }

    /// Sets the caret of the [`Selection`] on the start of the
    /// selection
    ///
    /// Returns `true` if a swap occurred
    pub fn set_caret_on_start(&mut self) -> bool {
        if let Some(anchor) = self.anchor()
            && anchor < self.caret()
        {
            self.swap_ends();
            true
        } else {
            false
        }
    }

    /// Sets the caret of the [`Selection`] on the end of the
    /// selection
    ///
    /// Returns `true` if a swap occurred
    pub fn set_caret_on_end(&mut self) -> bool {
        if let Some(anchor) = self.anchor()
            && anchor > self.caret()
        {
            self.swap_ends();
            true
        } else {
            false
        }
    }

    ////////// Selection meta manipulation

    /// Resets the [`Selection`] to how it was before being modified
    pub fn reset(&mut self) {
        self.selection = self.initial.clone();
    }

    /// Copies the current [`Selection`] in place
    ///
    /// This will leave an additional [`Selection`] with the current
    /// selection. Do note that normal intersection rules apply, so if
    /// at the end of the movement, this selection intersects with any
    /// other, they will be merged into one.
    ///
    /// When this [`Cursor`] is dropped, like with normal [`Cursor`]s,
    /// its [`Selection`] will be added to the [`Selections`], unless
    /// you [destroy] it.
    ///
    /// [destroy]: Self::destroy
    pub fn copy(&mut self) -> Cursor<'_, W, S> {
        Cursor::new(
            (self.selection.clone(), self.n, false),
            (self.widget, self.area),
            self.next_i.clone(),
            self.inc_searcher,
            true,
        )
    }

    /// Destroys the current [`Selection`]
    ///
    /// Will not destroy it if it is the last [`Selection`] left
    ///
    /// If this was the main selection, the main selection will now be
    /// the selection immediately behind it.
    pub fn destroy(mut self) {
        // If it is 1, it is actually 2, because this Selection is also part
        // of that list.
        if !self.widget.text().selections().is_empty() || self.is_copy {
            // Rc<Cell> needs to be manually dropped to reduce its counter.
            self.next_i.take();
            if self.was_main {
                self.widget.text_mut().selections_mut().rotate_main(-1);
            }
            // The destructor is what inserts the Selection back into the list, so
            // don't run it.
            std::mem::forget(self);
        } else {
            // Just to be explicit.
            drop(self);
        }
    }

    /// Sets the "desired visual column"
    ///
    /// The desired visual column determines at what point in a line
    /// the caret will be placed when moving [up and down] through
    /// lines of varying lengths.
    ///
    /// Will also set the "desired wrapped visual column", which is
    /// the same thing but used when moving vertically in a [wrapped]
    /// fashion.
    ///
    /// [up and down]: Cursor::move_ver
    /// [wrapped]: Cursor::move_ver_wrapped
    pub fn set_desired_vcol(&mut self, x: usize) {
        self.selection.set_desired_cols(x, x);
    }

    ////////// Iteration functions

    /// Iterates over the [`char`]s
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn chars_fwd(&self) -> impl Iterator<Item = (Point, char)> + '_ {
        self.widget.text().chars_fwd(self.caret()..).unwrap()
    }

    /// Iterates over the [`char`]s, in reverse
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn chars_rev(&self) -> impl Iterator<Item = (Point, char)> {
        self.widget.text().chars_rev(..self.caret()).unwrap()
    }

    /// Searches the [`Text`] for a regex
    ///
    /// The search will begin on the `caret`, and returns the bounding
    /// [`Point`]s, alongside the match. If an `end` is provided,
    /// the search will stop at the given [`Point`].
    ///
    /// # Panics
    ///
    /// If the regex is not valid, this method will panic.
    ///
    /// ```rust
    /// # use duat_core::prelude::*;
    /// fn search_nth_paren<U: Ui, S>(pa: &mut Pass, handle: &mut Handle<Buffer<U>, U, S>, n: usize) {
    ///     handle.edit_all(pa, |mut e| {
    ///         let mut nth = e.search_fwd('(', None).nth(n);
    ///         if let Some([p0, p1]) = nth {
    ///             e.move_to(p0);
    ///             e.set_anchor();
    ///             e.move_to(p1);
    ///         }
    ///     })
    /// }
    /// ```
    pub fn search_fwd<R: RegexPattern>(
        &self,
        pat: R,
        end: Option<Point>,
    ) -> impl Iterator<Item = R::Match> + '_ {
        let start = self.selection.caret();
        let text = self.widget.text();
        match end {
            Some(end) => text.search_fwd(pat, start..end).unwrap(),
            None => text.search_fwd(pat, start..text.len()).unwrap(),
        }
    }

    /// Searches the [`Text`] for a regex, in reverse
    ///
    /// The search will begin on the `caret`, and returns the bounding
    /// [`Point`]s, alongside the match. If a `start` is provided,
    /// the search will stop at the given [`Point`].
    ///
    /// # Panics
    ///
    /// If the regex is not valid, this method will panic.
    ///
    /// ```rust
    /// # use duat_core::prelude::*;
    /// fn search_nth_paren<U: Ui, S>(
    ///     pa: &mut Pass,
    ///     handle: &mut Handle<Buffer<U>, U, S>,
    ///     s: &str,
    ///     n: usize,
    /// ) {
    ///     handle.edit_all(pa, |mut e| {
    ///         let mut nth = e.search_rev(s, None).nth(n);
    ///         if let Some([p0, p1]) = nth {
    ///             e.move_to(p0);
    ///             e.set_anchor();
    ///             e.move_to(p1);
    ///         }
    ///     })
    /// }
    /// ```
    pub fn search_rev<R: RegexPattern>(
        &self,
        pat: R,
        start: Option<Point>,
    ) -> impl Iterator<Item = R::Match> + '_ {
        let end = self.selection.caret();
        let start = start.unwrap_or_default();
        let text = self.widget.text();
        text.search_rev(pat, start..end).unwrap()
    }

    /// Wether the current selection matches a regex pattern
    pub fn matches<R: RegexPattern>(&self, pat: R) -> bool {
        let range = self.selection.byte_range(self.widget.text());
        self.widget.text().matches(pat, range).unwrap()
    }

    ////////// Text queries

    /// Returns the [`char`] in the `caret`
    pub fn char(&self) -> char {
        self.text()
            .char_at(self.selection.caret())
            .unwrap_or_else(|| panic!("{:#?}\n{:#?}", self.selection.caret(), self.text()))
    }

    /// Returns the [`char`] at a given [`Point`]
    pub fn char_at(&self, p: Point) -> Option<char> {
        self.text().char_at(p)
    }

    /// Returns the [`Selection`]'s selection
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
    /// [`GapBuffer`]: gapbuf::GapBuffer
    pub fn selection(&self) -> Strs<'_> {
        let range = self.selection.byte_range(self.text());
        self.text().strs(range).unwrap()
    }

    /// Returns the [`Strs`] for the given [`TextRange`]
    ///
    /// [`GapBuffer`]: gapbuf::GapBuffer
    pub fn strs(&self, range: impl TextRange) -> Option<Strs<'_>> {
        self.widget.text().strs(range)
    }

    /// Returns the length of the [`Text`], in [`Point`]
    pub fn len(&self) -> Point {
        self.text().len()
    }

    /// Returns the position of the last [`char`] if there is one
    pub fn last_point(&self) -> Point {
        self.text().last_point()
    }

    /// An [`Iterator`] over the lines in a given [range]
    ///
    /// [range]: TextRange
    pub fn lines_on(&self, range: impl TextRange) -> Lines<'_> {
        self.widget.text().lines(range)
    }

    /// Gets the current level of indentation
    pub fn indent(&self) -> usize {
        self.widget
            .text()
            .indent(self.caret(), self.area, self.opts())
    }

    /// Gets the indentation level on the given [`Point`]
    pub fn indent_on(&self, p: Point) -> usize {
        self.widget.text().indent(p, self.area, self.opts())
    }

    ////////// Selection queries

    /// Returns the `caret`
    pub fn caret(&self) -> Point {
        self.selection.caret()
    }

    /// Returns the `anchor`
    pub fn anchor(&self) -> Option<Point> {
        self.selection.anchor()
    }

    /// The [`Point`] range of the [`Selection`]
    pub fn range(&self) -> Range<Point> {
        self.selection.point_range(self.text())
    }

    /// An exclusive [`Point`] range of the [`Selection`]
    pub fn range_excl(&self) -> Range<Point> {
        self.selection.point_range_excl()
    }

    /// The [`VPoint`] range of the [`Selection`]
    ///
    /// Use only if you need the things that the [`VPoint`] provides,
    /// in order to preven extraneous calculations
    pub fn v_caret(&self) -> VPoint {
        self.selection
            .v_caret(self.widget.text(), self.area, self.widget.get_print_opts())
    }

    /// The [`VPoint`] of the anchor, if it exists
    ///
    /// Use only if you need the things that the [`VPoint`] provides,
    /// in order to preven extraneous calculations
    pub fn v_anchor(&self) -> Option<VPoint> {
        self.selection
            .v_anchor(self.widget.text(), self.area, self.widget.get_print_opts())
    }

    /// Returns `true` if the `anchor` exists before the `caret`
    pub fn anchor_is_start(&self) -> bool {
        self.anchor().is_none_or(|anchor| anchor < self.caret())
    }

    /// Whether or not this is the main [`Selection`]
    pub fn is_main(&self) -> bool {
        self.was_main
    }

    /// The [`Text`] of the [`Widget`]
    pub fn text(&self) -> &Text {
        self.widget.text()
    }

    /// The [`PrintOpts`] in use
    pub fn opts(&self) -> PrintOpts {
        self.widget.get_print_opts()
    }
}

impl<S> Cursor<'_, Buffer, S> {
    /// Reads the [`Bytes`] and a [`Parser`]
    ///
    /// [`Bytes`]: crate::text::Bytes
    pub fn read_parser<Rd: Parser, Ret>(&self, read: impl FnOnce(&Rd) -> Ret) -> Option<Ret> {
        self.widget.read_parser(read)
    }
}

/// Incremental search functions, only available on [`IncSearcher`]s
///
/// [`IncSearcher`]: https://docs.rs/duat/latest/duat/modes/struct.IncSearcher.html
impl<W: Widget + ?Sized> Cursor<'_, W, Searcher> {
    /// Search incrementally from an [`IncSearch`] request
    ///
    /// This will match the Regex pattern from the current position of
    /// the caret. if `end` is [`Some`], the search will end at the
    /// requested [`Point`].
    ///
    /// [`IncSearch`]: https://docs.rs/duat/latest/duat/modes/struct.IncSearch.html
    pub fn search_inc_fwd(&mut self, end: Option<Point>) -> impl Iterator<Item = [Point; 2]> + '_ {
        let range = if let Some(end) = end {
            (self.selection.caret()..end).to_range(self.text().len().byte())
        } else {
            (self.selection.caret()..).to_range(self.text().len().byte())
        };
        self.inc_searcher.search_fwd(self.widget.text(), range)
    }

    /// Search incrementally from an [`IncSearch`] request in reverse
    ///
    /// This will match the Regex pattern from the current position of
    /// the caret in reverse. if `start` is [`Some`], the search will
    /// end at the requested [`Point`].
    ///
    /// [`IncSearch`]: https://docs.rs/duat/latest/duat/modes/struct.IncSearch.html
    pub fn search_inc_rev(
        &mut self,
        start: Option<Point>,
    ) -> impl Iterator<Item = [Point; 2]> + '_ {
        let range = if let Some(start) = start {
            (start..self.selection.caret()).to_range(self.text().len().byte())
        } else {
            (..self.selection.caret()).to_range(self.text().len().byte())
        };
        self.inc_searcher.search_rev(self.widget.text(), range)
    }

    /// Whether the [`Selection`]'s selection matches the
    /// [`IncSearch`] request
    ///
    /// [`IncSearch`]: https://docs.rs/duat/latest/duat/modes/struct.IncSearch.html
    pub fn matches_inc(&mut self) -> bool {
        let range = self.selection.byte_range(self.widget.text());
        self.inc_searcher
            .matches(self.widget.text().strs(range).unwrap().to_string().as_str())
    }
}

// SAFETY: In theory, it should be impossible to maintain a reference
// to W after it has dropped, since the Handle would be mutably
// borrowing from said W, and you can only get a Cursor from Handles.
// Thus, the only thing which may have been dropped is the Selections
// within, which are accounted for.
unsafe impl<#[may_dangle] 'a, W: Widget + ?Sized + 'a, S: 'a> Drop for Cursor<'a, W, S> {
    fn drop(&mut self) {
        let selection = std::mem::take(&mut self.selection);
        let ([inserted_i, selections_taken], last_selection_overhangs) = self
            .widget
            .text_mut()
            .selections_mut()
            .insert(self.n, selection, self.was_main);

        if let Some(next_i) = self.next_i.as_ref()
            && inserted_i <= next_i.get()
        {
            let go_to_next = !last_selection_overhangs as usize;
            next_i.set(
                next_i
                    .get()
                    .saturating_sub(selections_taken)
                    .max(inserted_i)
                    + go_to_next,
            )
        }
    }
}

impl<'a, W: Widget + ?Sized, S> std::fmt::Debug for Cursor<'a, W, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cursor")
            .field("selection", &self.selection)
            .finish_non_exhaustive()
    }
}

/// An [`Iterator`] overf all [`Cursor`]s
pub struct Cursors<'a, W: Widget + ?Sized, S> {
    next_i: Rc<Cell<usize>>,
    widget: &'a mut W,
    area: &'a Area,
    inc_searcher: RefMut<'a, S>,
}

impl<'a, W: Widget + ?Sized, S> Cursors<'a, W, S> {
    /// Creates a new [`Cursors`]
    pub(crate) fn new(
        next_i: usize,
        widget: &'a mut W,
        area: &'a Area,
        inc_searcher: RefMut<'a, S>,
    ) -> Self {
        Self {
            next_i: Rc::new(Cell::new(next_i)),
            widget,
            area,
            inc_searcher,
        }
    }
}

impl<'a, 'lend, W: Widget + ?Sized, S> Lending<'lend> for Cursors<'a, W, S> {
    type Lend = Cursor<'lend, W, S>;
}

impl<'a, W: Widget + ?Sized, S> Lender for Cursors<'a, W, S> {
    fn next<'lend>(&'lend mut self) -> Option<<Self as Lending<'lend>>::Lend> {
        let current_i = self.next_i.get();
        let (selection, was_main) = self.widget.text_mut().selections_mut().remove(current_i)?;

        Some(Cursor::new(
            (selection, current_i, was_main),
            (self.widget, self.area),
            Some(self.next_i.clone()),
            &mut self.inc_searcher,
            false,
        ))
    }
}

/// One or two [`Point`]s
pub trait PointOrPoints {
    /// Internal movement function for monomorphization
    #[doc(hidden)]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>);
}

impl PointOrPoints for Point {
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        cursor.selection.move_to(self, cursor.widget.text());
    }
}

impl PointOrPoints for Range<Point> {
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        assert!(
            self.start <= self.end,
            "slice index start is larger than end"
        );

        cursor.selection.move_to(self.start, cursor.widget.text());
        if self.start < self.end {
            cursor.set_anchor();
            cursor.selection.move_to(self.end, cursor.widget.text());
            if self.end < cursor.widget.text().len() {
                cursor.move_hor(-1);
            }
        }
    }
}

impl PointOrPoints for RangeInclusive<Point> {
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        assert!(
            self.start() <= self.end(),
            "slice index start is larger than end"
        );

        cursor
            .selection
            .move_to(*self.start(), cursor.widget.text());
        cursor.set_anchor();
        cursor.selection.move_to(*self.end(), cursor.widget.text());
    }
}

impl PointOrPoints for RangeFrom<Point> {
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        cursor.selection.move_to(self.start, cursor.widget.text());
        if self.start < cursor.text().len() {
            cursor.set_anchor();
            cursor
                .selection
                .move_to(cursor.widget.text().len(), cursor.widget.text());
            cursor.move_hor(-1);
        }
    }
}

impl PointOrPoints for RangeTo<Point> {
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        cursor.move_to_start();
        if Point::default() < self.end {
            cursor.set_anchor();
            cursor.selection.move_to(self.end, cursor.widget.text());
            cursor.move_hor(-1);
        }
    }
}

impl PointOrPoints for RangeToInclusive<Point> {
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        cursor.move_to_start();
        cursor.set_anchor();
        cursor.selection.move_to(self.end, cursor.widget.text());
    }
}

impl PointOrPoints for RangeFull {
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        cursor.move_to_start();
        cursor.set_anchor();
        cursor
            .selection
            .move_to(cursor.widget.text().len(), cursor.widget.text());
    }
}
