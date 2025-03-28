//! A helper struct for [`Mode`]s with [`Cursors`]
//!
//! This struct can edit [`Text`] in a declarative way, freeing the
//! [`Mode`]s from worrying about synchronization of the
//! cursors and dealing with editing the text directly.
//!
//! [`Mode`]: super::Mode
use std::{cell::Cell, rc::Rc};

use lender::{Lender, Lending};

pub use self::cursors::{Cursor, Cursors, VPoint};
use crate::{
    cfg::PrintCfg,
    text::{Change, Point, Reader, RegexPattern, Searcher, Strs, Text, TextRange},
    ui::Area,
    widgets::{File, Widget},
};

/// The [`Cursor`] and [`Cursors`] structs
mod cursors;

/// A struct used by [`Mode`]s to edit [`Text`]
///
/// You will want to use this struct when editing [`Widget`]s
/// with [`Cursors`]. For example, let's say you want to create an
/// mode for the [`File`] widget:
///
/// ```rust
/// # use duat_core::{mode::{EditHelper, Mode, KeyEvent, Cursors}, ui::Ui, widgets::File};
/// /// A very basic example Mode.
/// #[derive(Clone)]
/// struct PlacesCharactersAndMoves;
///
/// impl<U: Ui> Mode<U> for PlacesCharactersAndMoves {
///     type Widget = File;
///     /* ... */
/// #   fn send_key(&mut self, key: KeyEvent, widget: &mut File, area: &U::Area) {
/// #       todo!();
/// #   }
/// # }
/// ```
///
/// In order to modify the widget, you must implement the
/// [`Mode::send_key`] method. In it, you receive the following:
///
/// - The [key].
/// - A [`&mut Self::Widget`].
/// - An [`Area`], which you can resize and modify it in other ways.
/// - The current [`Cursors`] of the widget, these should be modified
///   by the [`EditHelper`].
///
/// In a [`Mode`] without cursors, you'd probably want to run
/// [`Cursors::clear`], in order to make sure there are no cursors.
///
/// ```rust
/// # use duat_core::{
/// #     mode::{key, Cursors, EditHelper, Mode, KeyCode, KeyEvent}, ui::Ui, widgets::File,
/// # };
/// # #[derive(Clone)]
/// # struct PlacesCharactersAndMoves;
/// impl<U: Ui> Mode<U> for PlacesCharactersAndMoves {
/// #   type Widget = File;
///     /* ... */
///     fn send_key(&mut self, key: KeyEvent, widget: &mut File, area: &U::Area) {
///         match key {
///             // actions based on the key pressed
///             key!(KeyCode::Char('c')) => {
///                 /* Do something when the character 'c' is typed. */
///             }
///             /* Matching the rest of the keys */
/// #           _ => todo!()
///         }
///     }
/// # }
/// ```
///
/// (You can use the [`key!`] macro in order to match [`KeyEvent`]s).
///
/// With the `EditHelper`, you can modify [`Text`] in a simplified
/// way. This is done by two actions, [editing] and [moving]. You
/// can only do one of these on any number of cursors at the same
/// time.
///
/// ```rust
/// # use duat_core::{
/// #     mode::{ key, Cursors, EditHelper, Mode, KeyCode, KeyEvent, KeyMod}, ui::Ui, widgets::File,
/// # };
/// # #[derive(Clone)]
/// # struct PlacesCharactersAndMoves;
/// impl<U: Ui> Mode<U> for PlacesCharactersAndMoves {
/// #   type Widget = File;
///     /* ... */
///     fn send_key(&mut self, key: KeyEvent, file: &mut File, area: &U::Area) {
///         let mut helper = EditHelper::new(file, area);
///         
///         match key {
///             key!(KeyCode::Char(c)) => {
///                 helper.edit_many(.., |e| e.insert('c'));
///                 helper.move_many(.., |mut m| m.move_hor(1));
///             },
///             key!(KeyCode::Right, KeyMod::SHIFT) => {
///                 helper.move_many(.., |mut m| {
///                     if m.anchor().is_none() {
///                         m.set_anchor()
///                     }
///                     m.move_hor(1)
///                 })
///             }
///             key!(KeyCode::Right) => {
///                 helper.move_many(.., |mut m| {
///                     m.unset_anchor();
///                     m.move_hor(1)
///                 })
///             }
///             /* Predictable remaining implementations */
/// #           _ => todo!()
///         }
///     }
/// # }
/// ```
///
/// [`Mode`]: super::Mode
/// [`Text`]: crate::text::Text
/// [`PromptLine`]: crate::widgets::PromptLine
/// [`&mut Self::Widget`]: super::Mode::Widget
/// [`Mode::send_key`]: super::Mode::send_key
/// [key]: super::KeyEvent
/// [`Self::Widget`]: super::Mode::Widget
/// [`Some(cursors)`]: Some
/// [`Ui::Area`]: crate::ui::Ui::Area
/// [commands]: crate::cmd
/// [`key!`]: super::key
/// [`KeyEvent`]: super::KeyEvent
/// [editing]: Editor
/// [moving]: Mover
pub struct EditHelper<'a, W: Widget<A::Ui>, A: Area, S> {
    widget: &'a mut W,
    area: &'a A,
    inc_searcher: S,
}

impl<'a, W: Widget<A::Ui>, A: Area> EditHelper<'a, W, A, ()> {
    /// Returns a new instance of [`EditHelper`]
    pub fn new(widget: &'a mut W, area: &'a A) -> Self {
        widget.text_mut().enable_cursors();
        widget.cursors_mut().unwrap().populate();
        EditHelper { widget, area, inc_searcher: () }
    }
}

impl<W: Widget<A::Ui>, A: Area, S> EditHelper<'_, W, A, S> {
    ////////// Editing functions

    /// Edits on the main [`Cursor`] in the [`Text`]
    ///
    /// Once dropped, the [`Cursor`] in this [`Editor`] will be added
    /// back to the list of [`Cursor`]s, unless it is [destroyed]
    ///
    /// If you want to edit on the main cursor, see [`edit_main`], if
    /// you want to edit on many [`Cursor`]s, see [`edit_iter`].
    ///
    /// [destroyed]: Editor::destroy
    /// [`edit_main`]: Self::edit_main
    /// [`edit_iter`]: Self::edit_iter
    pub fn edit_nth(&mut self, n: usize) -> Editor<W, A, S> {
        let cursors = self.widget.cursors_mut().unwrap();
        let Some((cursor, was_main)) = cursors.remove(n) else {
            panic!("Cursor index {n} out of bounds");
        };

        Editor::new(
            cursor,
            n,
            was_main,
            self.widget,
            self.area,
            None,
            &mut self.inc_searcher,
        )
    }

    /// Edits on the main [`Cursor`] in the [`Text`]
    ///
    /// Once dropped, the [`Cursor`] in this [`Editor`] will be added
    /// back to the list of [`Cursor`]s, unless it is [destroyed]
    ///
    /// If you want to edit on the `nth` cursor, see [`edit_nth`], if
    /// you want to edit on many [`Cursor`]s, see [`edit_iter`].
    ///
    /// [destroyed]: Editor::destroy
    /// [`edit_nth`]: Self::edit_nth
    /// [`edit_iter`]: Self::edit_iter
    pub fn edit_main(&mut self) -> Editor<W, A, S> {
        let n = self.widget.cursors().unwrap().main_index();
        self.edit_nth(n)
    }

    /// A [`Lender`] over all [`Editor`]s of the [`Text`]
    ///
    /// This lets you easily iterate over all [`Cursor`]s, without
    /// having to worry about insertion affecting the order at which
    /// they are edited (like what repeated calls to [`edit_nth`]
    /// would do)
    ///
    /// Note however that you can't use a [`Lender`] (also known as a
    /// lending iterator) in a `for` loop, but you should be able
    /// to just `while let Some(e) = editors.next() {}` or
    /// `helper.edit_iter().for_each(|_| {})` instead.
    ///
    /// [`edit_nth`]: Self::edit_nth
    pub fn edit_iter<'b>(&'b mut self) -> EditIter<'b, W, A, S> {
        EditIter {
            next_i: Rc::new(Cell::new(0)),
            widget: self.widget,
            area: self.area,
            inc_searcher: &mut self.inc_searcher,
        }
    }

    ////////// Getter functions

    /// A shared reference to the [`Widget`]
    pub fn widget(&self) -> &W {
        self.widget
    }

    /// A mutable reference to the [`Widget`]
    pub fn widget_mut(&mut self) -> &mut W {
        self.widget
    }

    /// A shared reference to the [`Widget`]
    pub fn text(&self) -> &Text {
        self.widget.text()
    }

    /// A mutable reference to the [`Widget`]
    pub fn text_mut(&mut self) -> &mut Text {
        self.widget.text_mut()
    }

    /// A shared reference to the [`Widget`]
    pub fn cursors(&self) -> &Cursors {
        self.widget.text().cursors().unwrap()
    }

    /// A mutable reference to the [`Widget`]
    pub fn cursors_mut(&mut self) -> &mut Cursors {
        self.widget.text_mut().cursors_mut().unwrap()
    }

    /// Undoes the last moment in the history, if there is one
    pub fn undo(&mut self) {
        self.widget.text_mut().undo();
    }

    /// Redoes the last moment in the history, if there is one
    pub fn redo(&mut self) {
        self.widget.text_mut().redo();
    }

    /// Finishes the current moment and adds a new one to the history
    pub fn new_moment(&mut self) {
        self.widget.text_mut().new_moment();
    }

    /// The [`PrintCfg`] in use
    pub fn cfg(&self) -> PrintCfg {
        self.widget.print_cfg()
    }
}

impl<'a, A> EditHelper<'a, File, A, Searcher>
where
    A: Area,
{
    /// Returns a new instance of [`EditHelper`]
    pub fn new_inc(widget: &'a mut File, area: &'a A, searcher: Searcher) -> Self {
        let cfg = widget.print_cfg();
        if let Some(c) = widget.cursors_mut() {
            c.populate()
        }
        widget.text_mut().remove_cursors(area, cfg);

        EditHelper { widget, area, inc_searcher: searcher }
    }
}

/// A cursor that can edit [`Text`], but can't alter selections
///
/// This struct will be used only inside functions passed to the
/// [`edit_*`] family of methods from the [`EditHelper`].
///
/// To make edits, you can use two different functions. You can either
/// [`replace`] or you can [`insert`]. The former will completely
/// replace the [`Cursor`]'s selection, while the latter will only
/// place the edit before the position of the `caret`, which could be
/// either in the start or the end of the selection.
///
/// ```rust
/// # use duat_core::{mode::EditHelper, ui::Area, widgets::File};
/// # fn test<S>(helper: &mut EditHelper<File, impl Area, S>) {
/// helper.edit_main(|e| {
///     e.replace("my replacement");
///     e.insert(" and my edit");
/// });
/// helper.move_main(|mut m| {
///     m.move_hor(" and my edit".chars().count() as i32);
///     m.set_anchor();
///     m.move_hor(-("my replacement and my edit".chars().count() as i32));
///     let sel: String = m.selection().into_iter().collect();
///     assert_eq!(sel, "my replacement and my edit".to_string());
/// });
/// # }
/// ```
///
/// [`edit_*`]: EditHelper::edit_nth
/// [`replace`]: Editor::replace
/// [`insert`]: Editor::insert
pub struct Editor<'a, W: Widget<A::Ui>, A: Area, S> {
    initial: Cursor,
    cursor: Cursor,
    n: usize,
    was_main: bool,
    widget: &'a mut W,
    area: &'a A,
    next_i: Option<Rc<Cell<usize>>>,
    inc_searcher: &'a mut S,
}

impl<'a, W: Widget<A::Ui>, A: Area, S> Editor<'a, W, A, S> {
    /// Returns a new instance of [`Editor`]
    fn new(
        cursor: Cursor,
        n: usize,
        was_main: bool,
        widget: &'a mut W,
        area: &'a A,
        next_i: Option<Rc<Cell<usize>>>,
        searcher: &'a mut S,
    ) -> Self {
        Self {
            initial: cursor.clone(),
            cursor,
            n,
            was_main,
            widget,
            area,
            next_i,
            inc_searcher: searcher,
        }
    }

    ////////// Text editing

    /// Replaces the entire selection with new text
    ///
    /// If the `caret` is behind the `anchor` (or in the same spot),
    /// after replacing the selection, the `caret` will be placed on
    /// the start of the selection, while the `anchor` will be placed
    /// on the new end. If it is ahead, it will be placed ahead.
    ///
    /// If there is no selection, then this has the same effect as
    /// [`insert`]. If you wish to append to the `caret` instead, see
    /// [`append`].
    ///
    /// [`insert`]: Self::insert
    /// [`append`]: Self::append
    pub fn replace(&mut self, edit: impl ToString) {
        let change = {
            let edit = edit.to_string();
            let [p0, p1] = self.cursor.point_range(self.widget.text());
            Change::new(edit, [p0, p1], self.widget.text())
        };

        let edit_len = change.added_text().len();
        let end = change.added_end();

        self.edit(change);

        let text = self.widget.text();

        if let Some(anchor) = self.cursor.anchor()
            && anchor >= self.cursor.caret()
            && edit_len > 0
        {
            self.cursor.swap_ends();
            self.cursor.move_to(end, text);
            self.cursor.swap_ends();
        } else {
            self.cursor.unset_anchor();
            self.cursor.move_to(end, text);
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
        let range = [self.cursor.caret(), self.cursor.caret()];
        let change = Change::new(edit.to_string(), range, self.widget.text());
        let (added, taken) = (change.added_end(), change.taken_end());

        self.edit(change);

        if let Some(anchor) = self.cursor.anchor()
            && anchor > self.cursor.caret()
        {
            let new_anchor = anchor + added - taken;
            self.cursor.swap_ends();
            self.cursor.move_to(new_anchor, self.widget.text());
            self.cursor.swap_ends();
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
        let caret = self.cursor.caret();
        let p = caret.fwd(self.widget.text().char_at(caret).unwrap());
        let change = Change::new(edit.to_string(), [p, p], self.widget.text());
        let (added, taken) = (change.added_end(), change.taken_end());

        self.edit(change);

        if let Some(anchor) = self.cursor.anchor()
            && anchor > p
        {
            let new_anchor = anchor + added - taken;
            self.cursor.swap_ends();
            self.cursor.move_to(new_anchor, self.widget.text());
            self.cursor.swap_ends();
        }
    }

    /// Edits the file with a [`Change`]
    fn edit(&mut self, change: Change<String>) {
        let text = self.widget.text_mut();
        let change_i = text.apply_change(self.cursor.change_i.map(|i| i as usize), change);
        self.cursor.change_i = change_i.map(|i| i as u32);
    }

    ////////// Movement functions

    /// Moves the cursor horizontally. May cause vertical movement
    pub fn move_hor(&mut self, count: i32) {
        self.cursor.move_hor(count, self.widget.text());
    }

    /// Moves the cursor vertically. May cause horizontal movement
    pub fn move_ver(&mut self, count: i32) {
        self.cursor.move_ver(
            count,
            self.widget.text(),
            self.area,
            self.widget.print_cfg(),
        );
    }

    /// Moves the cursor vertically. May cause horizontal movement
    pub fn move_ver_wrapped(&mut self, count: i32) {
        self.cursor.move_ver_wrapped(
            count,
            self.widget.text(),
            self.area,
            self.widget.print_cfg(),
        );
    }

    /// Moves the cursor to a [`Point`]
    ///
    /// - If the position isn't valid, it will move to the "maximum"
    ///   position allowed.
    pub fn move_to(&mut self, point: Point) {
        self.cursor.move_to(point, self.widget.text());
    }

    /// Moves the cursor to a `line` and a `column`
    ///
    /// - If the coords isn't valid, it will move to the "maximum"
    ///   position allowed.
    pub fn move_to_coords(&mut self, line: usize, col: usize) {
        let at = self
            .text()
            .point_at_line(line.min(self.text().len().line()));
        let (point, _) = self.text().chars_fwd(at).take(col + 1).last().unwrap();
        self.move_to(point);
    }

    /// Returns and takes the anchor of the [`Cursor`].
    pub fn unset_anchor(&mut self) -> Option<Point> {
        self.cursor.unset_anchor()
    }

    /// Sets the `anchor` to the current `caret`
    pub fn set_anchor(&mut self) {
        self.cursor.set_anchor()
    }

    /// Swaps the position of the `caret` and `anchor`
    pub fn swap_ends(&mut self) {
        self.cursor.swap_ends();
    }

    /// Sets the caret of the [`Cursor`] on the start of the
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

    /// Sets the caret of the [`Cursor`] on the end of the
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

    ////////// Cursor meta manipulation

    /// Resets the [`Cursor`] to how it was before being modified
    pub fn reset(&mut self) {
        self.cursor = self.initial.clone();
    }

    /// Copies the current [`Cursor`] in place
    ///
    /// This will leave an additional [`Cursor`] with the current
    /// selection. Do note that normal intersection rules apply, so if
    /// at the end of the movement, this cursor intersects with any
    /// other, they will be merged into one.
    ///
    /// When this [`Editor`] is dropped, like with normal [`Editor`]s,
    /// its [`Cursor`] will be added to the [`Cursors`], unless you
    /// [destroy] it.
    ///
    /// [destroy]: Self::destroy
    pub fn copy(&mut self) -> Editor<W, A, S> {
        Editor::new(
            self.cursor.clone(),
            self.n,
            false,
            self.widget,
            self.area,
            self.next_i.clone(),
            self.inc_searcher,
        )
    }

    /// Destroys the current [`Cursor`]
    ///
    /// Will not destroy it if it is the last [`Cursor`] left
    ///
    /// If this was the main cursor, the main cursor will now be the
    /// cursor immediately behind it.
    pub fn destroy(mut self) {
        if self.widget.cursors().unwrap().len() > 1 {
            // Rc<Cell> needs to be manually dropped to reduce its counter.
            self.next_i.take();
            // The destructor is what inserts the Cursor back into the list, so
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
    /// [up and down]: Mover::move_ver
    /// [wrapped]: Mover::move_ver_wrapped
    pub fn set_desired_vcol(&mut self, x: usize) {
        self.cursor.set_desired_cols(x, x);
    }

    ////////// Iteration functions

    /// Iterates over the [`char`]s
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn chars_fwd(&self) -> impl Iterator<Item = (Point, char)> + '_ {
        self.widget.text().chars_fwd(self.caret())
    }

    /// Iterates over the [`char`]s, in reverse
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn chars_rev(&self) -> impl Iterator<Item = (Point, char)> + '_ {
        self.widget.text().chars_rev(self.caret())
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
    /// # use duat_core::{mode::EditHelper, ui::Area, widgets::File};
    /// fn search_nth_paren<S>(
    ///     helper: &mut EditHelper<File, impl Area, S>,
    ///     n: usize,
    /// ) {
    ///     helper.move_many(.., |mut m| {
    ///         let mut nth = m.search_fwd('(', None).nth(n);
    ///         if let Some([p0, p1]) = nth {
    ///             m.move_to(p0);
    ///             m.set_anchor();
    ///             m.move_to(p1);
    ///         }
    ///     })
    /// }
    /// ```
    pub fn search_fwd<R: RegexPattern>(
        &mut self,
        pat: R,
        end: Option<Point>,
    ) -> impl Iterator<Item = R::Match> + '_ {
        let start = self.cursor.caret().byte();
        let text = self.widget.text_mut();
        match end {
            Some(end) => text.search_fwd(pat, start..end.byte()).unwrap(),
            None => {
                let end = text.len().byte();
                text.search_fwd(pat, start..end).unwrap()
            }
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
    /// # use duat_core::{mode::EditHelper, ui::Area, widgets::File};
    /// fn search_nth_rev<S>(
    ///     helper: &mut EditHelper<File, impl Area, S>,
    ///     n: usize,
    ///     s: &str,
    /// ) {
    ///     helper.move_many(.., |mut m| {
    ///         let mut nth = m.search_rev(s, None).nth(n);
    ///         if let Some([p0, p1]) = nth {
    ///             m.move_to(p0);
    ///             m.set_anchor();
    ///             m.move_to(p1);
    ///         }
    ///     })
    /// }
    /// ```
    pub fn search_rev<R: RegexPattern>(
        &mut self,
        pat: R,
        start: Option<Point>,
    ) -> impl Iterator<Item = R::Match> + '_ {
        let end = self.cursor.caret().byte();
        let start = start.unwrap_or_default();
        let text = self.widget.text_mut();
        text.search_rev(pat, start.byte()..end).unwrap()
    }

    /// Wether the current selection matches a regex pattern
    pub fn matches<R: RegexPattern>(&mut self, pat: R) -> bool {
        let range = self.cursor.range(self.widget.text());
        self.widget.text_mut().matches(pat, range).unwrap()
    }

    ////////// Text queries

    /// Returns the [`char`] in the `caret`
    pub fn char(&self) -> char {
        self.text().char_at(self.cursor.caret()).unwrap()
    }

    /// Returns the [`char`] at a given [`Point`]
    pub fn char_at(&self, p: Point) -> Option<char> {
        self.text().char_at(p)
    }

    /// Returns the [`Cursor`]'s selection
    ///
    /// The reason why this return value is `IntoIter<&str, 2>` is
    /// because the [`Text`] utilizes an underlying [`GapBuffer`]
    /// to store the characters. This means that the text is
    /// always separated into two distinct chunks.
    ///
    /// If this [`Cursor`]'s selection happens to be entirely
    /// within one of these chunks, the other `&str` will just be
    /// empty.
    ///
    /// [`GapBuffer`]: gapbuf::GapBuffer
    pub fn selection(&self) -> Strs {
        let range = self.cursor.range(self.text());
        self.text().strs(range)
    }

    /// Returns the length of the [`Text`], in [`Point`]
    pub fn len(&self) -> Point {
        self.text().len()
    }

    /// Returns the position of the last [`char`] if there is one
    pub fn last_point(&self) -> Option<Point> {
        self.text().last_point()
    }

    /// An [`Iterator`] over the lines in a given [range]
    ///
    /// [range]: TextRange
    pub fn lines_on(
        &mut self,
        range: impl TextRange,
    ) -> impl DoubleEndedIterator<Item = (usize, &'_ str)> + '_ {
        self.widget.text_mut().lines(range)
    }

    /// Gets the current level of indentation
    pub fn indent(&self) -> usize {
        self.widget
            .text()
            .indent(self.caret(), self.area, self.cfg())
    }

    /// Gets the indentation level on the given [`Point`]
    pub fn indent_on(&self, p: Point) -> usize {
        self.widget.text().indent(p, self.area, self.cfg())
    }

    /// Gets a [`Reader`]'s [public facing API], if it exists
    ///
    /// [public facing API]: Reader::PublicReader
    pub fn get_reader<R: Reader>(&mut self) -> Option<R::PublicReader<'_>> {
        self.widget.text_mut().get_reader::<R>()
    }

    ////////// Cursor queries

    /// Returns the `caret`
    pub fn caret(&self) -> Point {
        self.cursor.caret()
    }

    /// Returns the `anchor`
    pub fn anchor(&self) -> Option<Point> {
        self.cursor.anchor()
    }

    pub fn v_caret(&self) -> VPoint {
        self.cursor
            .v_caret(self.widget.text(), self.area, self.widget.print_cfg())
    }

    pub fn v_anchor(&self) -> Option<VPoint> {
        self.cursor
            .v_anchor(self.widget.text(), self.area, self.widget.print_cfg())
    }

    /// Returns `true` if the `anchor` exists before the `caret`
    pub fn anchor_is_start(&self) -> bool {
        self.anchor().is_none_or(|anchor| anchor < self.caret())
    }

    /// Whether or not this is the main [`Cursor`]
    pub fn is_main(&self) -> bool {
        self.was_main
    }

    pub fn text(&self) -> &Text {
        self.widget.text()
    }

    /// The [`PrintCfg`] in use
    pub fn cfg(&self) -> PrintCfg {
        self.widget.print_cfg()
    }
}

/// Incremental search functions, only available on [`IncSearcher`]s
///
/// [`IncSearcher`]: crate::mode::IncSearcher
impl<W: Widget<A::Ui>, A: Area> Editor<'_, W, A, Searcher> {
    /// Search incrementally from an [`IncSearch`] request
    ///
    /// This will match the Regex pattern from the current position of
    /// the caret. if `end` is [`Some`], the search will end at the
    /// requested [`Point`].
    ///
    /// [`IncSearch`]: crate::mode::IncSearch
    pub fn search_inc_fwd(&mut self, end: Option<Point>) -> impl Iterator<Item = [Point; 2]> + '_ {
        self.inc_searcher
            .search_fwd(self.widget.text_mut(), (self.cursor.caret(), end))
    }

    /// Search incrementally from an [`IncSearch`] request in reverse
    ///
    /// This will match the Regex pattern from the current position of
    /// the caret in reverse. if `start` is [`Some`], the search will
    /// end at the requested [`Point`].
    ///
    /// [`IncSearch`]: crate::mode::IncSearch
    pub fn search_inc_rev(
        &mut self,
        start: Option<Point>,
    ) -> impl Iterator<Item = [Point; 2]> + '_ {
        self.inc_searcher
            .search_rev(self.widget.text_mut(), (start, self.cursor.caret()))
    }

    /// Whether the [`Cursor`]'s selection matches the [`IncSearch`]
    /// request
    ///
    /// [`IncSearch`]: crate::mode::IncSearch
    pub fn matches_inc(&mut self) -> bool {
        let range = self.cursor.range(self.widget.text());
        self.inc_searcher
            .matches(self.widget.text_mut().contiguous(range))
    }
}

// SAFETY: Since this struct only owns references to a Widget, Area,
// and maybe a Searcher, dropping it early doesn't drop anything with
// the exception of the Rc<Cell>, which should have its count
// decreased.
unsafe impl<#[may_dangle] 'a, W: Widget<A::Ui>, A: Area, S> Drop for Editor<'a, W, A, S> {
    fn drop(&mut self) {
        let cursors = self.widget.cursors_mut().unwrap();
        let cursor = std::mem::take(&mut self.cursor);
        let [inserted_i, cursors_taken] = cursors.insert(self.n, cursor, self.was_main);

        if let Some(next_i) = self.next_i.as_ref()
            && inserted_i <= next_i.get()
        {
            next_i.set(next_i.get().saturating_sub(cursors_taken).max(inserted_i) + 1)
        }
    }
}

pub struct EditIter<'a, W: Widget<A::Ui>, A: Area, S> {
    next_i: Rc<Cell<usize>>,
    widget: &'a mut W,
    area: &'a A,
    inc_searcher: &'a mut S,
}

impl<'a, 'lend, W: Widget<A::Ui>, A: Area, S> Lending<'lend> for EditIter<'a, W, A, S> {
    type Lend = Editor<'lend, W, A, S>;
}

impl<'a, W: Widget<A::Ui>, A: Area, S> Lender for EditIter<'a, W, A, S> {
    fn next<'lend>(&'lend mut self) -> Option<<Self as Lending<'lend>>::Lend> {
        let current_i = self.next_i.get();
        let (cursor, was_main) = self.widget.cursors_mut().unwrap().remove(current_i)?;
        Some(Editor::new(
            cursor,
            current_i,
            was_main,
            self.widget,
            self.area,
            Some(self.next_i.clone()),
            self.inc_searcher,
        ))
    }
}
