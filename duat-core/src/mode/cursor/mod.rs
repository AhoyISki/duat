//! A helper struct for [`Mode`]s with [`Selections`]
//!
//! This struct can edit [`Text`] in a declarative way, freeing the
//! [`Mode`]s from worrying about synchronization of the
//! selections and dealing with editing the text directly.
//!
//! [`Mode`]: super::Mode
use std::{
    cell::{Cell, RefMut},
    ops::{DerefMut, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
    rc::Rc,
};

use lender::{Lender, Lending};
use regex_cursor::regex_automata::hybrid::dfa::Cache;

pub use self::selections::{Selection, Selections, VPoint};
use crate::{
    buffer::{Buffer, BufferId},
    opts::PrintOpts,
    text::{
        Change, Lines, Matches, Point, RegexHaystack, RegexPattern, Searcher, Strs, Text,
        TextIndex, TextRange,
    },
    ui::{Area, Widget},
};

/// The [`Selection`] and [`Selections`] structs
mod selections;

macro_rules! sel {
    ($cursor:expr) => {
        $cursor.selections[$cursor.sels_i]
            .as_ref()
            .unwrap()
            .selection
    };
}

macro_rules! sel_mut {
    ($cursor:expr) => {{
        let mod_sel = $cursor.selections[$cursor.sels_i].as_mut().unwrap();
        mod_sel.has_changed = true;
        &mut mod_sel.selection
    }};
}

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
/// # duat_core::doc_duat!(duat);
/// # use duat::prelude::*;
/// # fn test(mut pa: Pass, handle: &mut Handle) {
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
    selections: &'a mut Vec<Option<ModSelection>>,
    sels_i: usize,
    initial: Selection,
    widget: &'a mut W,
    area: &'a Area,
    next_i: Option<Rc<Cell<usize>>>,
    inc_searcher: &'a mut S,
}

impl<'a, W: Widget + ?Sized, S> Cursor<'a, W, S> {
    /// Returns a new instance of [`Cursor`]
    pub(crate) fn new(
        selections: &'a mut Vec<Option<ModSelection>>,
        sels_i: usize,
        (widget, area): (&'a mut W, &'a Area),
        next_i: Option<Rc<Cell<usize>>>,
        searcher: &'a mut S,
    ) -> Self {
        let initial = selections[sels_i].as_ref().unwrap().selection.clone();
        Self {
            selections,
            sels_i,
            initial,
            widget,
            area,
            next_i,
            inc_searcher: searcher,
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
        let range = sel!(self).caret()..sel!(self).caret();
        let change = Change::new(edit.to_string(), range, self.widget.text());
        let (added, taken) = (change.added_end(), change.taken_end());

        self.edit(change);

        if let Some(anchor) = sel!(self).anchor()
            && anchor > sel!(self).caret()
        {
            let new_anchor = anchor + added - taken;
            sel_mut!(self).swap_ends();
            sel_mut!(self).move_to(new_anchor, self.widget.text());
            sel_mut!(self).swap_ends();
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
        let caret = sel!(self).caret();
        let p = caret.fwd(self.widget.text().char_at(caret).unwrap());
        let change = Change::new(edit.to_string(), p..p, self.widget.text());
        let (added, taken) = (change.added_end(), change.taken_end());

        self.edit(change);

        if let Some(anchor) = sel!(self).anchor()
            && anchor > p
        {
            let new_anchor = anchor + added - taken;
            sel_mut!(self).swap_ends();
            sel_mut!(self).move_to(new_anchor, self.widget.text());
            sel_mut!(self).swap_ends();
        }
    }

    /// Edits the buffer with a [`Change`]
    fn edit(&mut self, change: Change<'static, String>) {
        let text = self.widget.text_mut();
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

    /// Moves the selection horizontally. May cause vertical movement
    ///
    /// Returns the distance moved in chars.
    #[track_caller]
    pub fn move_hor(&mut self, count: i32) -> i32 {
        if count == 0 {
            return 0;
        }
        sel_mut!(self).move_hor(count, self.widget.text())
    }

    /// Moves the selection vertically. May cause horizontal movement
    ///
    /// Returns the distance moved in lines.
    #[track_caller]
    pub fn move_ver(&mut self, count: i32) -> i32 {
        if count == 0 {
            return 0;
        }
        sel_mut!(self).move_ver(
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
    #[track_caller]
    pub fn move_ver_wrapped(&mut self, count: i32) {
        if count == 0 {
            return;
        }
        sel_mut!(self).move_ver_wrapped(
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
    #[track_caller]
    pub fn move_to(&mut self, point_or_points: impl CaretOrRange) {
        point_or_points.move_to(self);
    }

    /// Moves the selection to [`Point::default`], i.c., the start of
    /// the [`Text`]
    #[track_caller]
    pub fn move_to_start(&mut self) {
        sel_mut!(self).move_to(Point::default(), self.widget.text());
    }

    /// Moves the selection to a `line` and a `column`
    ///
    /// - If the coords isn't valid, it will move to the "maximum"
    ///   position allowed.
    #[track_caller]
    pub fn move_to_coords(&mut self, line: usize, col: usize) {
        let range = self
            .text()
            .line_range(line.min(self.text().last_point().line()));
        let byte = self
            .text()
            .chars_fwd(range.clone())
            .unwrap()
            .map(|(p, _)| p.byte())
            .take(col + 1)
            .last();
        self.move_to(byte.unwrap_or(range.end.byte() - 1));
    }

    /// Moves to a column on the current line
    #[track_caller]
    pub fn move_to_col(&mut self, col: usize) {
        let line = self.text().point_at_line(self.caret().line()).line();
        self.move_to_coords(line, col);
    }

    /// Returns and takes the anchor of the [`Selection`].
    pub fn unset_anchor(&mut self) -> Option<Point> {
        sel_mut!(self).unset_anchor()
    }

    /// Sets the `anchor` to the current `caret`
    pub fn set_anchor(&mut self) {
        sel_mut!(self).set_anchor()
    }

    /// Sets the `anchor` if it was not already set
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

    /// Swaps the position of the `caret` and `anchor`
    pub fn swap_ends(&mut self) {
        sel_mut!(self).swap_ends();
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
        *sel_mut!(self) = self.initial.clone();
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
        self.selections
            .extend_from_within(self.sels_i..=self.sels_i);
        let sels_i = self.selections.len() - 1;
        Cursor::new(
            self.selections,
            sels_i,
            (self.widget, self.area),
            self.next_i.clone(),
            self.inc_searcher,
        )
    }

    /// Destroys the current [`Selection`]
    ///
    /// Will not destroy it if it is the last [`Selection`] left
    ///
    /// If this was the main selection, the main selection will now be
    /// the selection immediately behind it.
    pub fn destroy(self) {
        // If there are other Selections in the list, or other copies still
        // lying around, the Cursor Selection can be destroyed.
        if self.widget.text().selections().is_empty()
            && self.selections.iter().flatten().count() <= 1
        {
            return;
        }

        if self.selections[self.sels_i].as_ref().unwrap().was_main {
            self.widget.text_mut().selections_mut().rotate_main(-1);
        }

        self.selections[self.sels_i] = None;
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
        sel_mut!(self).set_desired_cols(x, x);
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

    /// Wether the current selection matches a regex pattern
    #[track_caller]
    pub fn matches_pat<R: RegexPattern>(&self, pat: R) -> bool {
        let range = sel!(self).byte_range(self.widget.text());
        match self.widget.text().strs(range).unwrap().matches_pat(pat) {
            Ok(result) => result,
            Err(err) => panic!("{err}"),
        }
    }

    /// Returns an [`Iterator`] over the matches of a [`RegexPattern`]
    ///
    /// This `Iterator` normally covers the entire range of the
    /// [`Text`], however, there are methods that you can use to
    /// narrow it down to ranges relative to the `Cursor`'s [`caret`].
    ///
    /// For example, [`CursorMatches::from_caret`] will narrow the
    /// searched range from the beginning of the caret's `char` all
    /// the way until the end of the [`Text`].
    ///
    /// This `Iterator` also implements [`DoubleEndedIterator`], which
    /// means you can search in reverse as well.
    ///
    /// [`caret`]: Self::caret
    #[track_caller]
    pub fn search<R: RegexPattern>(
        &self,
        pat: R,
    ) -> CursorMatches<'_, R, impl DerefMut<Target = Cache>> {
        let text = self.widget.text();
        let caret = self.caret();
        CursorMatches {
            text_byte_len: text.len().byte(),
            caret_range: caret.byte()..caret.fwd(self.char()).byte(),
            matches: text.search(pat),
        }
    }

    ////////// Text queries

    /// Returns the [`char`] in the `caret`
    pub fn char(&self) -> char {
        self.text()
            .char_at(sel!(self).caret())
            .unwrap_or_else(|| panic!("{:#?}\n{:#?}", sel!(self).caret(), self.text()))
    }

    /// Returns the [`char`] at a given [`Point`]
    pub fn char_at(&self, i: impl TextIndex) -> Option<char> {
        self.text().char_at(i)
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
        let range = sel!(self).byte_range(self.text());
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

    /// An [`Iterator`] over the lines the `Cursor`'s range
    pub fn lines(&self) -> Lines<'_> {
        self.widget.text().lines(self.range())
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
        sel!(self).caret()
    }

    /// Returns the `anchor`
    pub fn anchor(&self) -> Option<Point> {
        sel!(self).anchor()
    }

    /// The [`Point`] range of the [`Selection`]
    ///
    /// This is an _inclusive_ range (not Rust's [`RangeInclusive`]
    /// however), this means that, even if there is no anchor, the
    /// lenght of this range will always be at least 1.
    ///
    /// If you want an exclusive range, see [`Cursor::range_excl`]
    ///
    /// [`RangeInclusive`]: std::ops::RangeInclusive
    pub fn range(&self) -> Range<Point> {
        sel!(self).point_range(self.text())
    }

    /// An exclusive [`Point`] range of the [`Selection`]
    ///
    /// If you wish for an inclusive range, whose length is always
    /// greater than or equal to 1, see [`RangeInclusive`].
    pub fn range_excl(&self) -> Range<Point> {
        sel!(self).point_range_excl()
    }

    /// The [`VPoint`] range of the [`Selection`]
    ///
    /// Use only if you need the things that the [`VPoint`] provides,
    /// in order to preven extraneous calculations
    pub fn v_caret(&self) -> VPoint {
        sel!(self).v_caret(self.widget.text(), self.area, self.widget.get_print_opts())
    }

    /// The [`VPoint`] of the anchor, if it exists
    ///
    /// Use only if you need the things that the [`VPoint`] provides,
    /// in order to preven extraneous calculations
    pub fn v_anchor(&self) -> Option<VPoint> {
        sel!(self).v_anchor(self.widget.text(), self.area, self.widget.get_print_opts())
    }

    /// Returns `true` if the `anchor` exists before the `caret`
    pub fn anchor_is_start(&self) -> bool {
        self.anchor().is_none_or(|anchor| anchor <= self.caret())
    }

    /// Whether or not this is the main [`Selection`]
    pub fn is_main(&self) -> bool {
        self.selections[self.sels_i].as_ref().unwrap().was_main
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
    pub fn search_inc(&mut self) -> CursorMatches<'_, String, &mut Cache> {
        let caret = self.caret();
        CursorMatches {
            text_byte_len: self.text().len().byte(),
            caret_range: caret.byte()..caret.fwd(self.char()).byte(),
            matches: self.inc_searcher.search(self.widget.text()),
        }
    }

    /// Whether the [`Selection`]'s selection matches the
    /// [`IncSearch`] request
    ///
    /// [`IncSearch`]: https://docs.rs/duat/latest/duat/modes/struct.IncSearch.html
    pub fn matches_inc(&mut self) -> bool {
        let range = sel!(self).byte_range(self.widget.text());
        self.inc_searcher
            .matches_pat(self.widget.text().strs(range).unwrap().to_string().as_str())
    }
}

impl<S> Cursor<'_, Buffer, S> {
    /// A unique identifier for this [`Buffer`]
    ///
    /// This is more robust than identifying it by its path or name,
    /// or event [`PathKind`], since those could change, but this
    /// cannot.
    pub fn buffer_id(&self) -> BufferId {
        self.widget.buffer_id()
    }
}

impl<'a, W: Widget + ?Sized, S> std::fmt::Debug for Cursor<'a, W, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cursor")
            .field("selection", &sel!(self))
            .finish_non_exhaustive()
    }
}

/// An [`Iterator`] over the matches of a [`RegexPattern`]
///
/// This `Iterator` comes from searching from a [`Cursor`]. Because of
/// that, it has methods for added convenience of search. The methods
/// [`to_caret`], [`to_caret_incl`], [`from_caret`] and
/// [`from_caret_excl`] will change the [`Range`] of searching to one
/// starting or ending on the `Cursor`'s [`caret`].
///
/// [`to_caret`]: CursorMatches::to_caret
/// [`to_caret_incl`]: CursorMatches::to_caret_incl
/// [`from_caret`]: CursorMatches::from_caret
/// [`from_caret_excl`]: CursorMatches::from_caret_excl
/// [`caret`]: Cursor::caret
pub struct CursorMatches<'c, R, Cc>
where
    R: RegexPattern,
    Cc: DerefMut<Target = Cache>,
{
    text_byte_len: usize,
    caret_range: Range<usize>,
    matches: Matches<'c, Text, R, Cc>,
}

impl<'c, R, C> CursorMatches<'c, R, C>
where
    R: RegexPattern,
    C: DerefMut<Target = Cache>,
{
    /// Changes the [`TextRange`] to search on
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

    /// Searches over a range from the start of the caret to the end
    /// of the [`Text`]
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # use duat::prelude::*;
    /// fn search_nth(pa: &mut Pass, handle: &Handle, n: usize, pat: &str) {
    ///     handle.edit_all(pa, |mut c| {
    ///         let mut nth = c.search(pat).from_caret().nth(n);
    ///         if let Some(range) = nth {
    ///             c.move_to(range);
    ///         }
    ///     })
    /// }
    /// ```
    #[allow(clippy::wrong_self_convention)]
    pub fn from_caret(self) -> Self {
        Self {
            matches: self
                .matches
                .range(self.caret_range.start..self.text_byte_len),
            ..self
        }
    }

    /// Searches over a range from the end of the caret to the end
    /// of the [`Text`]
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # use duat::prelude::*;
    /// fn next_paren_match(pa: &mut Pass, handle: &Handle) {
    ///     handle.edit_all(pa, |mut c| {
    ///         let mut start_count = 0;
    ///         let mut start_bound = None;
    ///         let end_bound = c
    ///             .search([r"\(", r"\)"])
    ///             .from_caret_excl()
    ///             .find(|(id, range)| {
    ///                 start_count += ((*id == 0) as u32).saturating_sub((*id == 1) as u32);
    ///                 start_bound = (*id == 0 && start_count == 0).then_some(range.clone());
    ///                 start_count == 0 && *id == 1
    ///             });
    ///
    ///         if let (Some(start), Some((_, end))) = (start_bound, end_bound) {
    ///             c.move_to(start.start..end.end);
    ///         }
    ///     })
    /// }
    /// ```
    #[allow(clippy::wrong_self_convention)]
    pub fn from_caret_excl(self) -> Self {
        Self {
            matches: self.matches.range(self.caret_range.end..self.text_byte_len),
            ..self
        }
    }

    /// Searches over a range from the start of the [`Text`] to the
    /// start of the caret's char
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # use duat::prelude::*;
    /// fn remove_prefix(pa: &mut Pass, handle: &Handle) {
    ///     let prefix_pat = format!(r"{}*\z", handle.opts(pa).word_chars_regex());
    ///     handle.edit_all(pa, |mut c| {
    ///         let prefix_range = c.search(&prefix_pat).to_caret().rev().next();
    ///         if let Some(range) = prefix_range {
    ///             c.move_to(range);
    ///             c.replace("");
    ///         }
    ///     })
    /// }
    /// ```
    #[allow(clippy::wrong_self_convention)]
    pub fn to_caret(self) -> Self {
        Self {
            matches: self.matches.range(0..self.caret_range.start),
            ..self
        }
    }

    /// Searches over a range from the start of the [`Text`] to the
    /// end of the caret's char
    ///
    /// ```rust
    /// # duat_core::doc_duat!(duat);
    /// # use duat::prelude::*;
    /// fn last_word_in_selection(pa: &mut Pass, handle: &Handle) {
    ///     let word_pat = format!(r"{}+", handle.opts(pa).word_chars_regex());
    ///     handle.edit_all(pa, |mut c| {
    ///         c.set_caret_on_end();
    ///         let mut nth = c.search(&word_pat).to_caret_incl().rev().next();
    ///         if let Some(range) = nth {
    ///             c.move_to(range)
    ///         } else {
    ///             c.reset()
    ///         }
    ///     })
    /// }
    /// ```
    #[allow(clippy::wrong_self_convention)]
    pub fn to_caret_incl(self) -> Self {
        Self {
            matches: self.matches.range(0..self.caret_range.end),
            ..self
        }
    }
}

impl<'c, R, Cc> Iterator for CursorMatches<'c, R, Cc>
where
    R: RegexPattern,
    Cc: DerefMut<Target = Cache>,
{
    type Item = R::Match;

    fn next(&mut self) -> Option<Self::Item> {
        self.matches.next()
    }
}

impl<'c, R, Cc> DoubleEndedIterator for CursorMatches<'c, R, Cc>
where
    R: RegexPattern,
    Cc: DerefMut<Target = Cache>,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.matches.next_back()
    }
}

/// An [`Iterator`] overf all [`Cursor`]s
pub struct Cursors<'a, W: Widget + ?Sized, S> {
    current: Vec<Option<ModSelection>>,
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
            current: Vec::new(),
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
        reinsert_selections(
            self.current.drain(..).flatten(),
            self.widget,
            Some(&self.next_i),
        );

        let current_i = self.next_i.get();
        let (selection, was_main) = self.widget.text_mut().selections_mut().remove(current_i)?;
        self.current
            .push(Some(ModSelection::new(selection, current_i, was_main)));

        Some(Cursor::new(
            &mut self.current,
            0,
            (self.widget, self.area),
            Some(self.next_i.clone()),
            &mut self.inc_searcher,
        ))
    }
}

impl<'a, W: Widget + ?Sized, S> Drop for Cursors<'a, W, S> {
    fn drop(&mut self) {
        reinsert_selections(
            self.current.drain(..).flatten(),
            self.widget,
            Some(&self.next_i),
        );
    }
}

/// Reinsert edited [`Selections`]
#[inline]
pub(crate) fn reinsert_selections(
    selections: impl Iterator<Item = ModSelection>,
    widget: &mut (impl Widget + ?Sized),
    next_i: Option<&Cell<usize>>,
) {
    for mod_sel in selections {
        let ([inserted_i, selections_taken], last_selection_overhangs) = widget
            .text_mut()
            .selections_mut()
            .insert(mod_sel.index, mod_sel.selection, mod_sel.was_main);

        if let Some(next_i) = next_i
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

/// A struct representing the temporary state of a [`Selection`] in a
/// [`Cursor`]
#[derive(Clone, Debug)]
pub(crate) struct ModSelection {
    selection: Selection,
    index: usize,
    was_main: bool,
    has_changed: bool,
}

impl ModSelection {
    /// Returns a new `ModSelection`
    pub(crate) fn new(selection: Selection, index: usize, was_main: bool) -> Self {
        Self {
            selection,
            index,
            was_main,
            has_changed: false,
        }
    }
}

/// A position that a [`Cursor`] can move to
///
/// This will come either in the form of [`Point`]s or byte indices
/// (in the form of `usize`). It can be a single value, like
/// `Point::default()` or `3`, in which case only the [caret] will
/// move, not affecting the [anchor].
///
/// Or it could be a [range], like `p1..p2` or `..=1000`, in which
/// case the caret will be placed at the end, while the anchor will be
/// placed at the start.
///
/// [caret]: Cursor::caret
/// [anchor]: Cursor::anchor
/// [range]: std::ops::RangeBounds
pub trait CaretOrRange {
    /// Internal movement function for monomorphization
    #[doc(hidden)]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>);
}

impl CaretOrRange for Point {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        sel_mut!(cursor).move_to(self, cursor.widget.text());
    }
}

impl CaretOrRange for Range<Point> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        assert!(
            self.start <= self.end,
            "slice index start is larger than end"
        );

        sel_mut!(cursor).move_to(self.start, cursor.widget.text());
        if self.start < self.end {
            cursor.set_anchor();
            sel_mut!(cursor).move_to(self.end, cursor.widget.text());
            if self.end < cursor.widget.text().len() {
                cursor.move_hor(-1);
            }
        } else {
            cursor.unset_anchor();
        }
    }
}

impl CaretOrRange for RangeInclusive<Point> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        assert!(
            self.start() <= self.end(),
            "slice index start is larger than end"
        );

        sel_mut!(cursor).move_to(*self.start(), cursor.widget.text());
        cursor.set_anchor();
        sel_mut!(cursor).move_to(*self.end(), cursor.widget.text());
    }
}

impl CaretOrRange for RangeFrom<Point> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        sel_mut!(cursor).move_to(self.start, cursor.widget.text());
        if self.start < cursor.text().len() {
            cursor.set_anchor();
            sel_mut!(cursor).move_to(cursor.widget.text().len(), cursor.widget.text());
            cursor.move_hor(-1);
        } else {
            cursor.unset_anchor();
        }
    }
}

impl CaretOrRange for RangeTo<Point> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        cursor.move_to_start();
        if Point::default() < self.end {
            cursor.set_anchor();
            sel_mut!(cursor).move_to(self.end, cursor.widget.text());
            cursor.move_hor(-1);
        } else {
            cursor.unset_anchor();
        }
    }
}

impl CaretOrRange for RangeToInclusive<Point> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        cursor.move_to_start();
        cursor.set_anchor();
        sel_mut!(cursor).move_to(self.end, cursor.widget.text());
    }
}

impl CaretOrRange for usize {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        sel_mut!(cursor).move_to(
            cursor.widget.text().point_at_byte(self),
            cursor.widget.text(),
        )
    }
}

impl CaretOrRange for Range<usize> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        assert!(
            self.start <= self.end,
            "slice index start is larger than end"
        );

        sel_mut!(cursor).move_to(
            cursor.widget.text().point_at_byte(self.start),
            cursor.widget.text(),
        );
        if self.start < self.end {
            cursor.set_anchor();
            sel_mut!(cursor).move_to(
                cursor.widget.text().point_at_byte(self.end),
                cursor.widget.text(),
            );
            if self.end < cursor.widget.text().len().byte() {
                cursor.move_hor(-1);
            }
        } else {
            cursor.unset_anchor();
        }
    }
}

impl CaretOrRange for RangeInclusive<usize> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        assert!(
            self.start() <= self.end(),
            "slice index start is larger than end"
        );

        sel_mut!(cursor).move_to(
            cursor.widget.text().point_at_byte(*self.start()),
            cursor.widget.text(),
        );
        cursor.set_anchor();
        sel_mut!(cursor).move_to(
            cursor.widget.text().point_at_byte(*self.end()),
            cursor.widget.text(),
        );
    }
}

impl CaretOrRange for RangeFrom<usize> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        sel_mut!(cursor).move_to(
            cursor.widget.text().point_at_byte(self.start),
            cursor.widget.text(),
        );
        if self.start < cursor.text().len().byte() {
            cursor.set_anchor();
            sel_mut!(cursor).move_to(cursor.widget.text().len(), cursor.widget.text());
            cursor.move_hor(-1);
        } else {
            cursor.unset_anchor();
        }
    }
}

impl CaretOrRange for RangeTo<usize> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        cursor.move_to_start();
        if self.end > 0 {
            cursor.set_anchor();
            sel_mut!(cursor).move_to(
                cursor.widget.text().point_at_byte(self.end),
                cursor.widget.text(),
            );
            cursor.move_hor(-1);
        } else {
            cursor.unset_anchor();
        }
    }
}

impl CaretOrRange for RangeToInclusive<usize> {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        cursor.move_to_start();
        cursor.set_anchor();
        sel_mut!(cursor).move_to(
            cursor.widget.text().point_at_byte(self.end),
            cursor.widget.text(),
        );
    }
}

impl CaretOrRange for RangeFull {
    #[track_caller]
    fn move_to<W: Widget + ?Sized, S>(self, cursor: &mut Cursor<'_, W, S>) {
        cursor.move_to_start();
        if cursor.text().len() > Point::default() {
            cursor.set_anchor();
            sel_mut!(cursor).move_to(cursor.widget.text().len(), cursor.widget.text());
        }
    }
}
