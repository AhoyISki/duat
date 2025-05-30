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
    ui::RawArea,
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
/// #   fn send_key(&mut self, key: KeyEvent, widget: &mut File, area: &U::RawArea) {
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
/// - An [`RawArea`], which you can resize and modify it in other ways.
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
///     fn send_key(&mut self, key: KeyEvent, widget: &mut File, area: &U::RawArea) {
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
/// #     mode::{ key, Cursors, EditHelper, Mode, KeyCode, KeyEvent, KeyMod},
/// #     Lender, ui::Ui, widgets::File,
/// # };
/// # #[derive(Clone)]
/// # struct PlacesCharactersAndMoves;
/// impl<U: Ui> Mode<U> for PlacesCharactersAndMoves {
/// #   type Widget = File;
///     /* ... */
///     fn send_key(&mut self, key: KeyEvent, file: &mut File, area: &U::RawArea) {
///         let mut helper = EditHelper::new(file, area);
///         
///         match key {
///             key!(KeyCode::Char(c)) => {
///                 helper.edit_iter().for_each(|mut e| {
///                     e.insert('c');
///                     e.move_hor(1);
///                 });
///             },
///             key!(KeyCode::Right, KeyMod::SHIFT) => {
///                 helper.edit_iter().for_each(|mut e| {
///                     if e.anchor().is_none() {
///                         e.set_anchor();
///                     }
///                     e.move_hor(1);
///                 });
///             }
///             key!(KeyCode::Right) => {
///                 helper.edit_iter().for_each(|mut e| {
///                     e.unset_anchor();
///                     e.move_hor(1);
///                 });
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
/// [`Ui::RawArea`]: crate::ui::Ui::RawArea
/// [commands]: crate::cmd
/// [`key!`]: super::key
/// [`KeyEvent`]: super::KeyEvent
/// [editing]: Editor
/// [moving]: Editor
pub struct EditHelper<'a, W: Widget<A::Ui>, A: RawArea, S> {
    widget: &'a mut W,
    area: &'a A,
    inc_searcher: S,
}

impl<'a, W: Widget<A::Ui>, A: RawArea> EditHelper<'a, W, A, ()> {
    /// Returns a new instance of [`EditHelper`]
    pub fn new(widget: &'a mut W, area: &'a A) -> Self {
        widget.text_mut().enable_cursors();
        widget.cursors_mut().unwrap().populate();
        EditHelper { widget, area, inc_searcher: () }
    }
}

impl<W: Widget<A::Ui>, A: RawArea, S> EditHelper<'_, W, A, S> {
    ////////// Editing functions

    /// Edits the nth [`Cursor`] in the [`Text`]
    ///
    /// Once dropped, the [`Cursor`] in this [`Editor`] will be added
    /// back to the list of [`Cursor`]s, unless it is [destroyed]
    ///
    /// If you want to edit on the main cursor, see [`edit_main`], if
    /// you want to edit on many [`Cursor`]s, see [`edit_iter`].
    ///
    /// Just like all other `edit` methods, this one will populate the
    /// [`Cursors`], so if there are no [`Cursor`]s, it will create
    /// one at [`Point::default`].
    ///
    /// [destroyed]: Editor::destroy
    /// [`edit_main`]: Self::edit_main
    /// [`edit_iter`]: Self::edit_iter
    pub fn edit_nth(&mut self, n: usize) -> Editor<W, A, S> {
        let cursors = self.widget.cursors_mut().unwrap();
        cursors.populate();
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

    /// Edits the main [`Cursor`] in the [`Text`]
    ///
    /// Once dropped, the [`Cursor`] in this [`Editor`] will be added
    /// back to the list of [`Cursor`]s, unless it is [destroyed]
    ///
    /// If you want to edit on the `nth` cursor, see [`edit_nth`],
    /// same for [`edit_last`], if you want to edit on many
    /// [`Cursor`]s, see [`edit_iter`].
    ///
    /// Just like all other `edit` methods, this one will populate the
    /// [`Cursors`], so if there are no [`Cursor`]s, it will create
    /// one at [`Point::default`].
    ///
    /// [destroyed]: Editor::destroy
    /// [`edit_nth`]: Self::edit_nth
    /// [`edit_last`]: Self::edit_last
    /// [`edit_iter`]: Self::edit_iter
    pub fn edit_main(&mut self) -> Editor<W, A, S> {
        self.edit_nth(self.cursors().main_index())
    }

    /// Edits the last [`Cursor`] in the [`Text`]
    ///
    /// Once dropped, the [`Cursor`] in this [`Editor`] will be added
    /// back to the list of [`Cursor`]s, unless it is [destroyed]
    ///
    /// If you want to edit on the `nth` cursor, see [`edit_nth`],
    /// same for [`edit_main`], if you want to edit on many
    /// [`Cursor`]s, see [`edit_iter`].
    ///
    /// Just like all other `edit` methods, this one will populate the
    /// [`Cursors`], so if there are no [`Cursor`]s, it will create
    /// one at [`Point::default`].
    ///
    /// [destroyed]: Editor::destroy
    /// [`edit_nth`]: Self::edit_nth
    /// [`edit_main`]: Self::edit_main
    /// [`edit_iter`]: Self::edit_iter
    pub fn edit_last(&mut self) -> Editor<W, A, S> {
        self.edit_nth(self.cursors().len().saturating_sub(1))
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
    /// Just like all other `edit` methods, this one will populate the
    /// [`Cursors`], so if there are no [`Cursor`]s, it will create
    /// one at [`Point::default`].
    ///
    /// [`edit_nth`]: Self::edit_nth
    pub fn edit_iter<'b>(&'b mut self) -> EditIter<'b, W, A, S> {
        self.cursors_mut().populate();
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
    A: RawArea,
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
/// To make edits, you can use three different functions. You can,
/// those being [`replace`], [`insert`], and [`append`]. [`replace`]
/// will completely replace the [`Cursor`]'s selection. [`insert`]
/// will place text behind the `caret`, and [`append`] will place it
/// after the `caret`.
///
/// You can also move the [`Cursor`]'s selection in many different
/// ways, which are described below, in the `impl` section for this
/// struct.
///
/// ```rust
/// # use duat_core::{mode::EditHelper, ui::RawArea, widgets::File};
/// # fn test<S>(helper: &mut EditHelper<File, impl RawArea, S>) {
/// let mut e = helper.edit_main();
/// e.replace("my replacement");
/// e.insert(" and my edit");
/// e.move_hor(" and my edit".chars().count() as i32);
/// e.set_anchor();
/// e.move_hor(-("my replacement and my edit".chars().count() as i32));
/// let sel: String = e.selection().into_iter().collect();
///
/// assert_eq!(&sel, "my replacement and my edit");
/// # }
/// ```
///
/// [`edit_*`]: EditHelper::edit_nth
/// [`replace`]: Editor::replace
/// [`insert`]: Editor::insert
/// [`append`]: Editor::append
pub struct Editor<'a, W: Widget<A::Ui>, A: RawArea, S> {
    initial: Cursor,
    cursor: Cursor,
    n: usize,
    was_main: bool,
    widget: &'a mut W,
    area: &'a A,
    next_i: Option<Rc<Cell<usize>>>,
    inc_searcher: &'a mut S,
}

impl<'a, W: Widget<A::Ui>, A: RawArea, S> Editor<'a, W, A, S> {
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

        let added_len = change.added_text().len();
        let end = change.added_end();

        self.edit(change);

        let caret_was_on_start = self.set_caret_on_end();

        self.move_to(end);
        if added_len > 0 {
            self.move_hor(-1);
        }
        if caret_was_on_start {
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
        let (change_i, cursors_taken) =
            text.apply_change(self.cursor.change_i.map(|i| i as usize), change);
        self.cursor.change_i = change_i.map(|i| i as u32);

        // The Change may have happened before the index of the next curossr,
        // so we need to account for that.
        if let Some((change_i, cursors_taken)) = change_i.zip(cursors_taken)
            && let Some(next_i) = self.next_i.as_ref()
            && change_i <= next_i.get()
        {
            next_i.set(next_i.get().saturating_sub(cursors_taken));
        }
    }

    ////////// Movement functions

    /// Moves the cursor horizontally. May cause vertical movement
    ///
    /// Returns the distance moved in chars.
    pub fn move_hor(&mut self, count: i32) -> i32 {
        self.cursor.move_hor(count, self.widget.text())
    }

    /// Moves the cursor vertically. May cause horizontal movement
    ///
    /// Returns the distance moved in lines.
    pub fn move_ver(&mut self, count: i32) -> i32 {
        self.cursor.move_ver(
            count,
            self.widget.text(),
            self.area,
            self.widget.print_cfg(),
        )
    }

    /// Moves the cursor vertically. May cause horizontal movement
    ///
    /// Returns the distance moved in wrapped lines.
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
        // If it is 1, it is actually 2, because this Cursor is also part of
        // that list.
        if !self.widget.cursors().unwrap().is_empty() {
            // Rc<Cell> needs to be manually dropped to reduce its counter.
            self.next_i.take();
            if self.was_main {
                self.widget.cursors_mut().unwrap().rotate_main(-1);
            }
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
    /// [up and down]: Editor::move_ver
    /// [wrapped]: Editor::move_ver_wrapped
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
    /// # use duat_core::{mode::EditHelper, ui::RawArea, widgets::File, Lender};
    /// fn search_nth_paren<S>(
    ///     helper: &mut EditHelper<File, impl RawArea, S>,
    ///     n: usize,
    /// ) {
    ///     helper.edit_iter().for_each(|mut e| {
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
    /// # use duat_core::{mode::EditHelper, ui::RawArea, widgets::File, Lender};
    /// fn search_nth_rev<S>(
    ///     helper: &mut EditHelper<File, impl RawArea, S>,
    ///     n: usize,
    ///     s: &str,
    /// ) {
    ///     helper.edit_iter().for_each(|mut e| {
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

    pub fn contiguous_in(&mut self, range: impl TextRange) -> &str {
        self.widget.text_mut().contiguous(range)
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

    pub fn range(&self) -> [Point; 2] {
        self.cursor.point_range(self.text())
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
impl<W: Widget<A::Ui>, A: RawArea> Editor<'_, W, A, Searcher> {
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

// SAFETY: In theory, it should be impossible to maintain a reference
// to W after it has dropped, since EditHelper would be mutably
// borrowing from said W, and you can only get an Editor from
// EditHelper. Thus. the only thing which may have been dropped is the
// Cursors within, which are accounted for.
// Also, since this is created by an EditHelper, it is created after
// it, and thus is dropped before it, making its &mut Cursor reference
// valid.
unsafe impl<#[may_dangle] 'a, W: Widget<A::Ui> + 'a, A: RawArea + 'a, S: 'a> Drop
    for Editor<'a, W, A, S>
{
    fn drop(&mut self) {
        let Some(cursors) = self.widget.cursors_mut() else {
            return;
        };
        let cursor = std::mem::take(&mut self.cursor);
        let ([inserted_i, cursors_taken], last_cursor_overhangs) =
            cursors.insert(self.n, cursor, self.was_main);

        if let Some(next_i) = self.next_i.as_ref()
            && inserted_i <= next_i.get()
        {
            let go_to_next = !last_cursor_overhangs as usize;
            next_i.set(next_i.get().saturating_sub(cursors_taken).max(inserted_i) + go_to_next)
        }
    }
}

pub struct EditIter<'a, W: Widget<A::Ui>, A: RawArea, S> {
    next_i: Rc<Cell<usize>>,
    widget: &'a mut W,
    area: &'a A,
    inc_searcher: &'a mut S,
}

impl<'a, 'lend, W: Widget<A::Ui>, A: RawArea, S> Lending<'lend> for EditIter<'a, W, A, S> {
    type Lend = Editor<'lend, W, A, S>;
}

impl<'a, W: Widget<A::Ui>, A: RawArea, S> Lender for EditIter<'a, W, A, S> {
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
