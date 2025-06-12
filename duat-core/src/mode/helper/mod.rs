//! A helper struct for [`Mode`]s with [`Cursors`]
//!
//! This struct can edit [`Text`] in a declarative way, freeing the
//! [`Mode`]s from worrying about synchronization of the
//! cursors and dealing with editing the text directly.
//!
//! [`Mode`]: super::Mode
use std::{
    cell::{Cell, RefMut},
    rc::Rc,
};

use lender::{Lender, Lending};

pub use self::cursors::{Cursor, Cursors, VPoint};
use crate::{
    cfg::PrintCfg,
    context::FileHandle,
    data::{Pass, RwData},
    file::{File, Reader},
    text::{Bytes, Change, Lines, Point, RegexPattern, Searcher, Strs, Text, TextRange},
    ui::{RawArea, Ui},
    widget::Widget,
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
/// # use duat_core::{mode::{EditHelper, Mode, KeyEvent, Cursors}, ui::Ui, file::File};
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
/// - An [`RawArea`], which you can resize and modify it in other
///   ways.
/// - The current [`Cursors`] of the widget, these should be modified
///   by the [`EditHelper`].
///
/// In a [`Mode`] without cursors, you'd probably want to run
/// [`Cursors::clear`], in order to make sure there are no cursors.
///
/// ```rust
/// # use duat_core::{
/// #     mode::{key, Cursors, EditHelper, Mode, KeyCode, KeyEvent}, ui::Ui, file::File,
/// # };
/// # #[derive(Clone)]
/// # struct PlacesCharactersAndMoves;
/// impl<U: Ui> Mode<U> for PlacesCharactersAndMoves {
/// #   type Widget = File<U>;
///     /* ... */
///     fn send_key(&mut self, key: KeyEvent, widget: &mut File<U>, area: &U::RawArea) {
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
/// #     Lender, ui::Ui, file::File,
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
pub struct EditHelper<W: Widget<U>, U: Ui, S> {
    widget: RwData<W>,
    area: U::Area,
    inc_searcher: S,
}

impl<W: Widget<U>, U: Ui> EditHelper<W, U, ()> {
    /// Returns an [`EditHelper`]
    pub fn new(pa: &mut Pass, widget: RwData<W>, area: U::Area) -> Self {
        widget.write(pa, |wid| wid.text_mut().enable_cursors());
        EditHelper { widget, area, inc_searcher: () }
    }
}

impl<U: Ui> EditHelper<File<U>, U, ()> {
    /// Returns an [`EditHelper`] for a given [`FileHandle`]
    pub fn from_handle(pa: &mut Pass, handle: FileHandle<U>) -> Self {
        handle.write(&mut *pa, |wid, _| wid.text_mut().enable_cursors());
        let (widget, area) = handle.get_related_widget(&*pa).unwrap();
        Self { widget, area, inc_searcher: () }
    }
}

impl<U: Ui> EditHelper<File<U>, U, Searcher> {
    /// Returns an [`EditHelper`] with incremental search
    pub fn new_inc(
        pa: &mut Pass,
        widget: RwData<File<U>>,
        area: U::Area,
        searcher: Searcher,
    ) -> Self {
        widget.write(pa, |wid| wid.text_mut().enable_cursors());

        EditHelper { widget, area, inc_searcher: searcher }
    }

    /// Returns an [`EditHelper`] with incremental search for a given
    /// [`FileHandle`]
    pub fn inc_from_handle(pa: &mut Pass, handle: FileHandle<U>, searcher: Searcher) -> Self {
        handle.write(pa, |wid, _| wid.text_mut().enable_cursors());

        let (widget, area) = handle.get_related_widget(pa).unwrap();

        EditHelper { widget, area, inc_searcher: searcher }
    }
}

impl<W: Widget<U>, U: Ui, S> EditHelper<W, U, S> {
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
    pub fn edit_nth<Ret>(
        &mut self,
        pa: &mut Pass,
        n: usize,
        edit: impl FnOnce(Editor<W, U::Area, S>) -> Ret,
    ) -> Ret {
        fn get_parts<'a, W: Widget<U>, U: Ui>(
            widget: &'a RwData<W>,
            pa: &mut Pass,
            n: usize,
        ) -> (Cursor, bool, RefMut<'a, W>) {
            let mut widget = widget.acquire_mut(pa);
            let cursors = widget.text_mut().cursors_mut().unwrap();
            cursors.populate();
            let Some((cursor, was_main)) = cursors.remove(n) else {
                panic!("Cursor index {n} out of bounds");
            };

            (cursor, was_main, widget)
        }

        let (cursor, was_main, mut widget) = get_parts(&self.widget, pa, n);

        edit(Editor::new(
            cursor,
            n,
            was_main,
            &mut *widget,
            &self.area,
            None,
            &mut self.inc_searcher,
        ))
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
    pub fn edit_main<Ret>(
        &mut self,
        pa: &mut Pass,
        edit: impl FnOnce(Editor<W, U::Area, S>) -> Ret,
    ) -> Ret {
        self.edit_nth(
            pa,
            self.read(&*pa, |wid| wid.text().cursors().unwrap().main_index()),
            edit,
        )
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
    pub fn edit_last<Ret>(
        &mut self,
        pa: &mut Pass,
        edit: impl FnOnce(Editor<W, U::Area, S>) -> Ret,
    ) -> Ret {
        self.edit_nth(
            pa,
            self.read(&*pa, |wid| wid.text().cursors().unwrap().len())
                .saturating_sub(1),
            edit,
        )
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
    pub fn edit_iter<Ret>(
        &mut self,
        pa: &mut Pass,
        edit: impl FnOnce(EditIter<'_, W, U::Area, S>) -> Ret,
    ) -> Ret {
        edit(self.get_iter(pa))
    }

    /// A shortcut for iterating over all cursors
    ///
    /// This is the equivalent of calling:
    ///
    /// ```rust
    /// # use duat_core::{data::Pass, mode::EditHelper, ui::Area, file::File};
    /// # fn test<A: Area>(pa: &mut Pass, helper: EditHelper<File, A, ()>) {
    /// helper.edit_iter(pa, |iter| iter.for_each(|e| { /* .. */ }));
    /// # }
    /// ```
    ///
    /// But it can't return a value, and is meant to reduce the
    /// indentation that will inevitably come from using the
    /// equivalent long form call.
    pub fn edit_all(&mut self, pa: &mut Pass, edit: impl FnMut(Editor<W, U::Area, S>)) {
        self.get_iter(pa).for_each(edit);
    }

    fn get_iter(&mut self, pa: &mut Pass) -> EditIter<'_, W, U::Area, S> {
        let mut widget = self.widget.acquire_mut(pa);
        let cursors = widget.text_mut().cursors_mut().unwrap();
        cursors.populate();
        EditIter {
            next_i: Rc::new(Cell::new(0)),
            widget,
            area: &self.area,
            inc_searcher: &mut self.inc_searcher,
        }
    }

    ////////// Getter functions

    /// Reads the [`W`] within
    ///
    /// [`W`]: Widget
    pub fn read<Ret>(&self, pa: &Pass, read: impl FnOnce(&W) -> Ret) -> Ret {
        let widget = self.widget.acquire(pa);
        read(&*widget)
    }

    /// Writes to the [`W`] within
    ///
    /// [`W`]: Widget
    pub fn write<Ret>(&self, pa: &mut Pass, write: impl FnOnce(&mut W) -> Ret) -> Ret {
        let mut widget = self.widget.acquire_mut(pa);
        write(&mut *widget)
    }

    /// Reads the [`Text`] of the [`Widget`]
    pub fn read_text<Ret>(&self, pa: &Pass, read: impl FnOnce(&Text) -> Ret) -> Ret {
        let widget = self.widget.acquire(pa);
        read(widget.text())
    }

    /// Writes to the [`Text`] of the [`Widget`]
    pub fn write_text<Ret>(&self, pa: &mut Pass, write: impl FnOnce(&mut Text) -> Ret) -> Ret {
        let mut widget = self.widget.acquire_mut(pa);
        write(widget.text_mut())
    }

    /// Reads the [`Cursors`] of the [`Widget`]
    pub fn read_cursors<Ret>(&self, pa: &Pass, read: impl FnOnce(&Cursors) -> Ret) -> Ret {
        let widget = self.widget.acquire(pa);
        read(widget.text().cursors().unwrap())
    }

    /// Writes to the [`Cursors`] of the [`Widget`]
    pub fn write_cursors<Ret>(
        &self,
        pa: &mut Pass,
        write: impl FnOnce(&mut Cursors) -> Ret,
    ) -> Ret {
        let mut widget = self.widget.acquire_mut(pa);
        write(widget.text_mut().cursors_mut().unwrap())
    }

    /// Clones the [`Text`] within
    pub fn clone_text(&self, pa: &Pass) -> Text {
        self.widget.clone_text(pa)
    }

    /// Replaces the [`Text`] within with a [`Default`] version
    pub fn take_text(&self, pa: &mut Pass) -> Text {
        self.widget.take_text(pa)
    }

    /// Replaces the [`Text`] of the [`Widget`], returning the
    /// previous value
    pub fn replace_text(&self, pa: &mut Pass, text: Text) -> Text {
        self.widget.replace_text(pa, text)
    }

    /// Undoes the last moment in the history, if there is one
    pub fn undo(&mut self, pa: &mut Pass) {
        self.widget.write(pa, |wid| wid.text_mut().undo());
    }

    /// Redoes the last moment in the history, if there is one
    pub fn redo(&mut self, pa: &mut Pass) {
        self.widget.write(pa, |wid| wid.text_mut().redo());
    }

    /// Finishes the current moment and adds a new one to the history
    pub fn new_moment(&mut self, pa: &mut Pass) {
        self.widget.write(pa, |wid| wid.text_mut().new_moment());
    }

    /// The [`PrintCfg`] in use
    pub fn cfg(&self, pa: &Pass) -> PrintCfg {
        self.widget.read(pa, |wid| wid.print_cfg())
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
/// # use duat_core::{mode::EditHelper, ui::RawArea, file::File};
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

    /// Moves the cursor vertically a number of wrapped lines. May
    /// cause horizontal movement
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

    /// Sets the `anchor` if it was not already set
    pub fn set_anchor_if_needed(&mut self) {
        if self.anchor().is_none() {
            self.cursor.set_anchor()
        }
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
        if !self.widget.text().cursors().unwrap().is_empty() {
            // Rc<Cell> needs to be manually dropped to reduce its counter.
            self.next_i.take();
            if self.was_main {
                self.widget
                    .text_mut()
                    .cursors_mut()
                    .unwrap()
                    .rotate_main(-1);
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
    /// # use duat_core::{mode::EditHelper, ui::RawArea, file::File, Lender};
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
    /// # use duat_core::{mode::EditHelper, ui::RawArea, file::File, Lender};
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

    /// Shifts the gap within the [`GapBuffer`] in order to return a
    /// contiguous `&str`
    ///
    /// [`GapBuffer`]: gapbuf::GapBuffer
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
    pub fn lines_on(&mut self, range: impl TextRange) -> Lines {
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

    ////////// Cursor queries

    /// Returns the `caret`
    pub fn caret(&self) -> Point {
        self.cursor.caret()
    }

    /// Returns the `anchor`
    pub fn anchor(&self) -> Option<Point> {
        self.cursor.anchor()
    }

    /// The [`Point`] range of the [`Cursor`]
    pub fn range(&self) -> [Point; 2] {
        self.cursor.point_range(self.text())
    }

    /// The [`VPoint`] range of the [`Cursor`]
    ///
    /// Use only if you need the things that the [`VPoint`] provides,
    /// in order to preven extraneous calculations
    pub fn v_caret(&self) -> VPoint {
        self.cursor
            .v_caret(self.widget.text(), self.area, self.widget.print_cfg())
    }

    /// The [`VPoint`] of the anchor, if it exists
    ///
    /// Use only if you need the things that the [`VPoint`] provides,
    /// in order to preven extraneous calculations
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

    /// The [`Text`] of the [`Widget`]
    pub fn text(&self) -> &Text {
        self.widget.text()
    }

    /// The [`PrintCfg`] in use
    pub fn cfg(&self) -> PrintCfg {
        self.widget.print_cfg()
    }
}

impl<U: Ui, S> Editor<'_, File<U>, U::Area, S> {
    /// Reads the [`Bytes`] and a [`Reader`]
    pub fn read_bytes_and_reader<R: Reader<U>, Ret>(
        &self,
        f: impl FnOnce(&Bytes, &R) -> Ret,
    ) -> Option<Ret> {
        // SAFETY: Since the creation of an Editor requires the use of a &mut
        // Pass, It should be safe to read a Reader, since it cannot be a
        // File.
        self.widget.get_reader().map(|reader| unsafe {
            reader.read_unsafe(|reader| f(self.widget.text().bytes(), reader))
        })
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
        let range = if let Some(end) = end {
            (self.cursor.caret()..end).to_range(self.text().len().byte())
        } else {
            (self.cursor.caret()..).to_range(self.text().len().byte())
        };
        self.inc_searcher.search_fwd(self.widget.text_mut(), range)
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
        let range = if let Some(start) = start {
            (start..self.cursor.caret()).to_range(self.text().len().byte())
        } else {
            (..self.cursor.caret()).to_range(self.text().len().byte())
        };
        self.inc_searcher.search_rev(self.widget.text_mut(), range)
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
        let Some(cursors) = self.widget.text_mut().cursors_mut() else {
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
    widget: RefMut<'a, W>,
    area: &'a A,
    inc_searcher: &'a mut S,
}

impl<'a, 'lend, W: Widget<A::Ui>, A: RawArea, S> Lending<'lend> for EditIter<'a, W, A, S> {
    type Lend = Editor<'lend, W, A, S>;
}

impl<'a, W: Widget<A::Ui>, A: RawArea, S> Lender for EditIter<'a, W, A, S> {
    fn next<'lend>(&'lend mut self) -> Option<<Self as Lending<'lend>>::Lend> {
        let current_i = self.next_i.get();
        let (cursor, was_main) = self
            .widget
            .text_mut()
            .cursors_mut()
            .unwrap()
            .remove(current_i)?;

        Some(Editor::new(
            cursor,
            current_i,
            was_main,
            &mut *self.widget,
            self.area,
            Some(self.next_i.clone()),
            self.inc_searcher,
        ))
    }
}
