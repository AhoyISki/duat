//! A helper struct for [`InputMethod`]s with [`Cursors`]
//!
//! This struct can edit [`Text`] in a declarative way, freeing the
//! [`InputMethod`]s from worrying about synchronization of the
//! cursors and dealing with editing the text directly.
//!
//! [`InputMethod`]: super::InputMethod
use std::{any::TypeId, ops::Range};

pub use self::cursors::{Cursor, Cursors};
use crate::{
    data::RwData,
    history::Change,
    text::{Point, PrintCfg, RegexPattern, Searcher, Text, WordChars},
    ui::Area,
    widgets::{ActiveWidget, File, PassiveWidget},
};

/// The [`Cursor`] and [`Cursors`] structs
mod cursors;

/// A struct used by [`InputMethod`]s to edit [`Text`]
///
/// You will want to use this struct when editing [`ActiveWidget`]s
/// with [`Cursors`]. For example, let's say you want to create an
/// input method for the [`File`] widget:
///
/// ```rust
/// # use duat_core::{
/// #     data::RwData, input::{EditHelper, InputMethod, KeyEvent, Cursors}, ui::Ui, widgets::File,
/// # };
/// /// A very basic example InputMethod.
/// struct PlacesCharactersAndMoves(Cursors);
///
/// impl<U: Ui> InputMethod<U> for PlacesCharactersAndMoves {
///     type Widget = File;
///     /* ... */
/// #   fn send_key(
/// #       &mut self,
/// #       key: KeyEvent,
/// #       widget: &RwData<Self::Widget>,
/// #       area: &U::Area,
/// #   ) {
/// #       todo!();
/// #   }
/// # }
/// ```
///
/// In order to modify the widget, you must implement the
/// [`InputMethod::send_key`] method. In it, you receive a
/// [key], an [`RwData<Self::Widget>`], that you use in order to
/// [`mutate`] or [`inspect`] the widget. You will also receive a
/// [`Ui::Area`], letting you do things like [resizing].
///
/// ```rust
/// # use duat_core::{
/// #     data::RwData, input::{key, Cursors, EditHelper, InputMethod, KeyCode, KeyEvent},
/// #     ui::Ui, widgets::File,
/// # };
/// # struct PlacesCharactersAndMoves(Cursors);
/// impl<U: Ui> InputMethod<U> for PlacesCharactersAndMoves {
/// #   type Widget = File;
///     /* ... */
///     fn send_key(
///         &mut self,
///         key: KeyEvent,
///         widget: &RwData<Self::Widget>,
///         area: &U::Area,
///     ) where
///         Self: Sized,
///     {
///         match key {
///             // actions based on the key pressed
///             key!(KeyCode::Char('c')) => {
///                 /* Do something when the 'c' is typed. */
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
/// #     data::RwData, input::{ key, Cursors, EditHelper, InputMethod, KeyCode, KeyEvent, KeyMod},
/// #     ui::Ui, widgets::File,
/// # };
/// # struct PlacesCharactersAndMoves(Cursors);
/// impl<U: Ui> InputMethod<U> for PlacesCharactersAndMoves {
/// #   type Widget = File;
///     /* ... */
///     fn send_key(
///         &mut self,
///         key: KeyEvent,
///         widget: &RwData<Self::Widget>,
///         area: &U::Area,
///     ) where
///         Self: Sized,
///     {
///         let mut helper = EditHelper::new(widget, area, &mut self.0);
///         
///         match key {
///             key!(KeyCode::Char(c)) => {
///                 helper.edit_on_each(|e| e.insert('c'));
///                 helper.move_each(|m| m.move_hor(1));
///             },
///             key!(KeyCode::Right, KeyMod::SHIFT) => {
///                 helper.move_each(|m| {
///                     if m.anchor().is_none() {
///                         m.set_anchor()
///                     }
///                     m.move_hor(1)
///                 })
///             }
///             key!(KeyCode::Right) => {
///                 helper.move_each(|m| {
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
/// [`InputMethod`]: super::InputMethod
/// [`Text`]: crate::text::Text
/// [`CommandLine`]: crate::widgets::CommandLine
/// [`RwData<Self::Widget>`]: RwData
/// [`mutate`]: RwData::mutate
/// [`inspect`]: RwData::inspect
/// [`InputMethod::send_key`]: super::InputMethod::send_key
/// [key]: super::KeyEvent
/// [resizing]: crate::ui::Area::constrain_ver
/// [`Ui::Area`]: crate::ui::Ui::Area
/// [commands]: crate::commands
/// [`key!`]: super::key
/// [`KeyEvent`]: super::KeyEvent
/// [editing]: Editor
/// [moving]: Mover
pub struct EditHelper<'a, W, A, S>
where
    W: ActiveWidget<A::Ui> + 'static,
    A: Area,
{
    widget: &'a RwData<W>,
    cursors: &'a mut Cursors,
    area: &'a A,
    cfg: PrintCfg,
    searcher: S,
}

impl<'a, W, A> EditHelper<'a, W, A, ()>
where
    W: ActiveWidget<A::Ui> + 'static,
    A: Area,
{
    /// Returns a new instance of [`EditHelper`]
    pub fn new(widget: &'a RwData<W>, area: &'a A, cursors: &'a mut Cursors) -> Self {
        let cfg = widget.read().print_cfg();
        EditHelper { widget, cursors, area, cfg, searcher: () }
    }
}

impl<W, A, S> EditHelper<'_, W, A, S>
where
    W: ActiveWidget<A::Ui> + 'static,
    A: Area,
{
    /// Edits on the `nth` [`Cursor`]'s selection
    ///
    /// Since the editing function takes [`Editor`] as an argument,
    /// you cannot change the selection of the [`Cursor`].
    ///
    /// If you want to move the `nth` cursor, see [`move_nth`],
    /// if you want to edit on the main cursor, see [`edit_on_main`],
    /// if you want to edit each cursor, see [`edit_on_each`].
    ///
    /// [`move_nth`]: Self::move_nth
    /// [`edit_on_main`]: Self::edit_on_main
    /// [`edit_on_each`]: Self::edit_on_each
    pub fn edit_on_nth(&mut self, mut edit: impl FnMut(&mut Editor<A, W>), n: usize) {
        let Some((mut cursor, was_main)) = self.cursors.remove(n) else {
            panic!("Cursor index {n} out of bounds.");
        };

        let mut widget = self.widget.write();
        let mut diff = Diff::default();

        edit(&mut Editor::<A, W>::new(
            &mut cursor,
            &mut widget,
            self.area,
            &self.cfg,
            &mut diff,
            was_main,
        ));

        self.cursors.insert_removed(was_main, cursor);
        self.cursors
            .shift(n, diff, widget.text(), self.area, &self.cfg);
    }

    /// Edits on each of the [`Cursor`]'s selection
    ///
    /// Since the editing function takes [`Editor`] as an argument,
    /// you cannot change the selection of the [`Cursor`].
    ///
    /// If you want to move each cursor, see [`move_each`],
    /// if you want to edit on a specific cursor, see [`edit_on_nth`]
    /// or [`edit_on_main`].
    ///
    /// [`move_each`]: Self::move_each
    /// [`edit_on_nth`]: Self::edit_on_nth
    /// [`edit_on_main`]: Self::edit_on_main
    pub fn edit_on_each(&mut self, mut f: impl FnMut(&mut Editor<A, W>)) {
        let removed_cursors: Vec<(Cursor, bool)> = self.cursors.drain().collect();

        let mut widget = self.widget.write();
        let mut diff = Diff::default();

        for (mut cursor, was_main) in removed_cursors.into_iter() {
            diff.shift_cursor(&mut cursor, widget.text(), self.area, &self.cfg);

            f(&mut Editor::new(
                &mut cursor,
                &mut widget,
                self.area,
                &self.cfg,
                &mut diff,
                was_main,
            ));

            self.cursors.insert_removed(was_main, cursor);
        }
    }

    /// Moves the nth [`Cursor`]'s selection
    ///
    /// Since the moving function takes [`Mover`] as an argument, this
    /// method cannot be used to change the [`Text`] in any way.
    ///
    /// At the end of the movement, if the cursor intersects any
    /// other, they will be merged into one.
    ///
    /// If you want to edit on the `nth` cursor, see [`edit_on_nth`],
    /// if you want to move the main cursor, see [`move_main`], if you
    /// want to move each cursor, see [`move_each`].
    ///
    /// [`edit_on_nth`]: Self::edit_on_nth
    /// [`move_main`]: Self::move_main
    /// [`move_each`]: Self::move_each
    pub fn move_nth(&mut self, mut mov: impl FnMut(&mut Mover<A, S>), n: usize) {
        let Some((mut cursor, was_main)) = self.cursors.remove(n) else {
            panic!("Cursor index {n} out of bounds.");
        };
        let mut widget = self.widget.write();

        mov(&mut Mover::new(
            &mut cursor,
            widget.text_mut(),
            self.area,
            &self.cfg,
            was_main,
            &mut self.searcher,
        ));

        self.cursors.insert_removed(was_main, cursor);
    }

    /// Moves each [`Cursor`]'s selection
    ///
    /// Since the moving function takes [`Mover`] as an argument, this
    /// method cannot be used to change the [`Text`] in any way.
    ///
    /// At the end of the movement, if any of the cursors intersect
    /// with each other, they will be merged into one.
    ///
    /// If you want to edit on each cursor, see [`edit_on_each`],
    /// if you want to move a specific cursor, see [`move_nth`]
    /// or [`move_main`].
    ///
    /// [`edit_on_each`]: Self::edit_on_each
    /// [`move_nth`]: Self::move_nth
    /// [`move_main`]: Self::move_main
    pub fn move_each<_T>(&mut self, mut mov: impl FnMut(&mut Mover<A, S>) -> _T) {
        let removed_cursors: Vec<(Cursor, bool)> = self.cursors.drain().collect();

        let mut widget = self.widget.write();

        for (mut cursor, was_main) in removed_cursors.into_iter() {
            mov(&mut Mover::new(
                &mut cursor,
                widget.text_mut(),
                self.area,
                &self.cfg,
                was_main,
                &mut self.searcher,
            ));

            self.cursors.insert_removed(was_main, cursor);
        }
    }

    /// Edits on the main [`Cursor`]'s selection
    ///
    /// Since the editing function takes [`Editor`] as an argument,
    /// you cannot change the selection of the [`Cursor`].
    ///
    /// If you want to move the main cursor, see [`move_main`],
    /// if you want to edit on the `nth` cursor, see [`edit_on_nth`],
    /// if you want to edit each cursor, see [`edit_on_each`].
    ///
    /// [`move_main`]: Self::move_main
    /// [`edit_on_nth`]: Self::edit_on_nth
    /// [`edit_on_each`]: Self::edit_on_each
    pub fn edit_on_main(&mut self, edit: impl FnMut(&mut Editor<A, W>)) {
        self.edit_on_nth(edit, self.cursors.main_index());
    }

    /// Moves the main [`Cursor`]'s selection
    ///
    /// Since the moving function takes [`Mover`] as an argument, this
    /// method cannot be used to change the [`Text`] in any way.
    ///
    /// At the end of the movement, if the cursor intersects any
    /// other, they will be merged into one.
    ///
    /// If you want to move the main cursor, see [`edit_on_main`],
    /// if you want to move the main cursor, see [`move_main`], if you
    /// want to move each cursor, see [`move_each`].
    ///
    /// [`edit_on_main`]: Self::edit_on_main
    /// [`move_main`]: Self::move_main
    /// [`move_each`]: Self::move_each
    pub fn move_main(&mut self, mov: impl FnMut(&mut Mover<A, S>)) {
        self.move_nth(mov, self.cursors.main_index());
    }

    /// Removes all but the main cursor from the list
    pub fn remove_extra_cursors(&mut self) {
        self.cursors.remove_extras();
    }

    /// Rotates the main cursor index forwards
    pub fn rotate_main_fwd(&mut self) {
        self.cursors.rotate_main_fwd()
    }

    /// Rotates the main cursor index backwards
    pub fn rotate_main_rev(&mut self) {
        self.cursors.rotate_main_rev()
    }

    /// The main [`Cursor`]
    pub fn main_cursor(&self) -> &Cursor {
        self.cursors.main()
    }

    /// Returns the `nth` [`Cursor`], if it exists
    pub fn get_cursor(&self, n: usize) -> Option<Cursor> {
        self.cursors.get(n)
    }

    /// The main cursor index
    pub fn main_cursor_index(&self) -> usize {
        self.cursors.main_index()
    }

    /// The amount of active [`Cursor`]s in the [`Text`]
    pub fn cursors_len(&self) -> usize {
        self.cursors.len()
    }
}

impl<A, S> EditHelper<'_, File, A, S>
where
    A: Area,
{
    /// Begins a new [`Moment`]
    ///
    /// A new `Moment` signifies a break in the history of this
    /// [`File`], that is, if you [`undo`], the changes prior to the
    /// creation of this `Moment` will be kept.
    ///
    /// [`Moment`]: crate::history::Moment
    /// [`undo`]: EditHelper::undo
    pub fn new_moment(&mut self) {
        self.widget.write().add_moment();
    }

    /// Undoes the last [`Moment`]
    ///
    /// [`Moment`]: crate::history::Moment
    pub fn undo(&mut self) {
        let mut widget = self.widget.write();
        widget.undo(self.area, self.cursors);
        <File as PassiveWidget<A::Ui>>::update(&mut widget, self.area);
    }

    /// Redoes the next [`Moment`]
    ///
    /// [`Moment`]: crate::history::Moment
    pub fn redo(&mut self) {
        let mut widget = self.widget.write();
        widget.redo(self.area, self.cursors);
        <File as PassiveWidget<A::Ui>>::update(&mut widget, self.area);
    }
}

impl<'a, 'b, A> EditHelper<'a, File, A, Searcher<'b>>
where
    A: Area,
{
    /// Returns a new instance of [`EditHelper`]
    pub fn new_inc(
        widget: &'a RwData<File>,
        area: &'a A,
        cursors: &'a mut Cursors,
        searcher: Searcher<'b>,
    ) -> Self {
        let cfg = {
            let mut file = widget.write();
            <File as ActiveWidget<A::Ui>>::text_mut(&mut file).remove_cursor_tags(cursors);
            <File as PassiveWidget<A::Ui>>::print_cfg(&file)
        };

        EditHelper { widget, cursors, area, cfg, searcher }
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
/// # use duat_core::{input::EditHelper, ui::Area, widgets::File};
/// # fn test<S>(helper: &mut EditHelper<File, impl Area, S>) {
/// helper.edit_on_main(|e| {
///     e.replace("my replacement");
///     e.insert(" and my edit");
/// });
/// helper.move_main(|m| {
///     m.move_hor(" and my edit".chars().count() as isize);
///     m.set_anchor();
///     m.move_hor(-("my replacement and my edit".chars().count() as isize));
///     let sel: String = m.selection().into_iter().collect();
///     assert_eq!(sel, "my replacement and my edit".to_string());
/// });
/// # }
/// ```
///
/// [`edit_*`]: EditHelper::edit_on_nth
/// [`replace`]: Editor::replace
/// [`insert`]: Editor::insert
pub struct Editor<'a, 'b, 'c, 'd, A, W>
where
    A: Area,
    W: ActiveWidget<A::Ui>,
{
    cursor: &'a mut Cursor,
    widget: &'b mut W,
    area: &'c A,
    cfg: &'a PrintCfg,
    diff: &'d mut Diff,
    is_main: bool,
}

impl<'a, 'b, 'c, 'd, A, W> Editor<'a, 'b, 'c, 'd, A, W>
where
    A: Area,
    W: ActiveWidget<A::Ui>,
{
    /// Returns a new instance of [`Editor`]
    fn new(
        cursor: &'a mut Cursor,
        widget: &'b mut W,
        area: &'c A,
        cfg: &'a PrintCfg,
        diff: &'d mut Diff,
        is_main: bool,
    ) -> Self {
        Self { cursor, widget, area, cfg, diff, is_main }
    }

    /// Replaces the entire selection with new text
    ///
    /// If the `caret` is behind the `anchor` (or in the same spot),
    /// after replacing the selection, the `caret` will be placed on
    /// the start of the selection, while the `anchor` will be placed
    /// on the new end.
    ///
    /// If there is no selection, then this has the same effect as
    /// [`insert`].
    ///
    /// [`insert`]: Self::insert
    pub fn replace(&mut self, edit: impl ToString) {
        let change = Change::new(edit.to_string(), self.cursor.range(), self.widget.text());
        let edit_len = change.added_text.len();
        let end = change.added_end();

        self.edit(change);

        let text = self.widget.text();
        let end_p = text.point_at(end);

        if let Some(anchor) = self.cursor.anchor()
            && anchor >= self.cursor.caret()
            && edit_len > 0
        {
            self.cursor.swap_ends();
            self.cursor.move_to(end_p, text, self.area, self.cfg);
            self.cursor.swap_ends();
        } else {
            self.cursor.unset_anchor();
            self.cursor.move_to(end_p, text, self.area, self.cfg);
        }
    }

    /// Inserts new text directly behind the `caret`
    ///
    /// The selection remains unaltered, if the `anchor` is ahead of
    /// the `caret`, it will move forwards by `edit.chars().count()`.
    ///
    /// If you wish to replace the selected text, see [`replace`]
    ///
    /// [`replace`]: Self::replace
    pub fn insert(&mut self, edit: impl ToString) {
        let range = self.cursor.byte()..self.cursor.byte();
        let change = Change::new(edit.to_string(), range, self.widget.text());
        let diff = change.chars_diff();

        self.edit(change);

        if let Some(anchor) = self.cursor.anchor()
            && anchor >= self.cursor.caret()
        {
            let text = self.widget.text();
            self.cursor.swap_ends();
            self.cursor.move_hor(diff, text, self.area, self.cfg);
            self.cursor.swap_ends();
        }
    }

    /// Edits the file with a [`Change`]
    fn edit(&mut self, change: Change) {
        self.widget.text_mut().apply_change(&change);
        self.diff.bytes += change.added_end() as isize - change.taken_end() as isize;

        if TypeId::of::<W>() == TypeId::of::<File>() {
            let file = unsafe { std::mem::transmute_copy::<&mut W, &mut File>(&self.widget) };

            let (insertion_index, change_diff) = file
                .history_mut()
                .add_change(change, self.cursor.assoc_index);
            self.cursor.assoc_index = Some(insertion_index);
            self.diff.changes += change_diff;
        }
    }

    pub fn is_main(&self) -> bool {
        self.is_main
    }
}

/// A cursor that can alter the selection, but can't edit
pub struct Mover<'a, A, S>
where
    A: Area,
{
    cursor: &'a mut Cursor,
    text: &'a mut Text,
    area: &'a A,
    cfg: &'a PrintCfg,
    is_main: bool,
    inc_matches: &'a mut S,
}

impl<'a, A, S> Mover<'a, A, S>
where
    A: Area,
{
    /// Returns a new instance of `Mover`
    fn new(
        cursor: &'a mut Cursor,
        text: &'a mut Text,
        area: &'a A,
        cfg: &'a PrintCfg,
        is_main: bool,
        inc_matches: &'a mut S,
    ) -> Self {
        Self {
            cursor,
            text,
            area,
            cfg,
            is_main,
            inc_matches,
        }
    }

    ////////// Public movement functions

    /// Moves the cursor horizontally. May cause vertical movement
    pub fn move_hor(&mut self, count: isize) {
        self.cursor.move_hor(count, self.text, self.area, self.cfg);
    }

    /// Moves the cursor vertically. May cause horizontal movement
    pub fn move_ver(&mut self, count: isize) {
        self.cursor.move_ver(count, self.text, self.area, self.cfg);
    }

    /// Moves the cursor vertically. May cause horizontal movement
    pub fn move_ver_wrapped(&mut self, count: isize) {
        self.cursor
            .move_ver_wrapped(count, self.text, self.area, self.cfg);
    }

    /// Moves the cursor to a [`Point`]
    ///
    /// - If the position isn't valid, it will move to the "maximum"
    ///   position allowed.
    /// - This command sets `desired_x`.
    pub fn move_to(&mut self, point: Point) {
        self.cursor.move_to(point, self.text, self.area, self.cfg);
    }

    /// Moves the cursor to a `line` and a `column`
    ///
    /// - If the coords isn't valid, it will move to the "maximum"
    ///   position allowed.
    /// - This command sets `desired_x`.
    pub fn move_to_coords(&mut self, line: usize, col: usize) {
        let point = self.text.point_at_line(line.min(self.text.len_lines()));
        let (point, _) = self.text.iter_chars_at(point).take(col + 1).last().unwrap();
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

    /// Places the `caret` at the beginning of the selection
    pub fn set_caret_on_start(&mut self) {
        if let Some(anchor) = self.cursor.anchor()
            && self.cursor.caret() > anchor
        {
            self.cursor.swap_ends();
        }
    }

    /// Places the `caret` at the end of the selection
    pub fn set_caret_on_end(&mut self) {
        if let Some(anchor) = self.cursor.anchor()
            && anchor > self.cursor.caret()
        {
            self.cursor.swap_ends();
        }
    }

    ////////// Lookup functions

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
    /// # use duat_core::{input::EditHelper, ui::Area, widgets::File};
    /// fn search_nth_paren<S>(
    ///     helper: &mut EditHelper<File, impl Area, S>,
    ///     n: usize,
    /// ) {
    ///     helper.move_each(|m| {
    ///         let mut nth = m.search('(', None).nth(n);
    ///         if let Some((start, end)) = nth {
    ///             m.move_to(start);
    ///             m.set_anchor();
    ///             m.move_to(end);
    ///         }
    ///     })
    /// }
    /// ```
    pub fn search<R: RegexPattern>(
        &mut self,
        pat: R,
        end: Option<Point>,
    ) -> impl Iterator<Item = R::Match> + '_ {
        self.text
            .search_from(pat, self.cursor.caret(), end)
            .unwrap()
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
    /// # use duat_core::{input::EditHelper, ui::Area, widgets::File};
    /// fn search_nth_rev<S>(
    ///     helper: &mut EditHelper<File, impl Area, S>,
    ///     n: usize,
    ///     s: &str,
    /// ) {
    ///     helper.move_each(|m| {
    ///         let mut nth = m.search_rev(s, None).nth(n);
    ///         if let Some((start, end)) = nth {
    ///             m.move_to(start);
    ///             m.set_anchor();
    ///             m.move_to(end);
    ///         }
    ///     })
    /// }
    /// ```
    pub fn search_rev<R: RegexPattern>(
        &mut self,
        pat: R,
        start: Option<Point>,
    ) -> impl Iterator<Item = R::Match> + '_ {
        self.text
            .search_from_rev(pat, self.cursor.caret(), start)
            .unwrap()
    }

    /// Returns the [`char`] in the `caret`
    pub fn char(&self) -> char {
        self.text.char_at(self.cursor.caret()).unwrap()
    }

    /// Returns the lenght of the [`Text`], in [`Point`]
    pub fn len_point(&self) -> Point {
        self.text.len_point()
    }

    /// Returns the position of the last [`char`] if there is one
    pub fn last_point(&self) -> Option<Point> {
        self.text.last_point()
    }

    /// Returns the [`PrintCfg`] in use
    pub fn cfg(&self) -> &PrintCfg {
        self.cfg
    }

    /// Returns the [`WordChars`] in use
    pub fn w_chars(&self) -> &WordChars {
        &self.cfg.word_chars
    }

    /// Iterates over the [`char`]s
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn iter(&self) -> impl Iterator<Item = (Point, char)> + '_ {
        self.text.iter_chars_at(self.caret())
    }

    /// Iterates over the [`char`]s, in reverse
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn iter_rev(&self) -> impl Iterator<Item = (Point, char)> + '_ {
        self.text.iter_chars_at_rev(self.caret())
    }

    /// Returns the `anchor`
    pub fn anchor(&self) -> Option<Point> {
        self.cursor.anchor()
    }

    /// Returns the `caret`
    pub fn caret(&self) -> Point {
        self.cursor.caret()
    }

    /// Returns `true` if the `anchor` exists before the `caret`
    pub fn anchor_is_start(&self) -> bool {
        self.anchor().is_none_or(|anchor| anchor < self.caret())
    }

    /// Returns the [`Cursor`]'s selection
    ///
    /// The reason why this return value is `[&str; 2]` is because the
    /// [`Text`] utilizes an underlying [`GapBuffer`] to store the
    /// characters. This means that the text is always separated into
    /// two distinct chunks.
    ///
    /// If this `Cursor`'s selection happens to be entirely within one
    /// of these chunks, the other `&str` will just be empty.
    ///
    /// [`GapBuffer`]: gapbuf::GapBuffer
    pub fn selection(&self) -> [&str; 2] {
        let anchor = self.anchor().unwrap_or(self.caret());
        let range = if anchor < self.caret() {
            (anchor, self.caret())
        } else {
            (self.caret(), anchor)
        };
        self.text.strs_in_point_range(range)
    }

    /// The range of the [`Cursor`]'s selection, in bytes
    pub fn byte_range(&self) -> Range<usize> {
        let anchor = self.anchor().unwrap_or(self.caret());
        if anchor < self.caret() {
            anchor.byte()..self.caret().byte()
        } else {
            self.caret().byte()..anchor.byte()
        }
    }

    pub fn is_main(&self) -> bool {
        self.is_main
    }
}

impl<A> Mover<'_, A, Searcher<'_>>
where
    A: Area,
{
    /// Search incrementally from an [`IncSearch`] request
    ///
    /// This will match the Regex pattern from the current position of
    /// the caret. if `end` is [`Some`], the search will end at the
    /// requested [`Point`].
    ///
    /// [`IncSearch`]: crate::widgets::IncSearch
    pub fn search_inc(&mut self, end: Option<Point>) -> impl Iterator<Item = (Point, Point)> + '_ {
        self.inc_matches
            .search_from(self.text, self.cursor.caret(), end)
    }

    /// Search incrementally from an [`IncSearch`] request in reverse
    ///
    /// This will match the Regex pattern from the current position of
    /// the caret in reverse. if `start` is [`Some`], the search will
    /// end at the requested [`Point`].
    ///
    /// [`IncSearch`]: crate::widgets::IncSearch
    pub fn search_inc_rev(
        &mut self,
        start: Option<Point>,
    ) -> impl Iterator<Item = (Point, Point)> + '_ {
        self.inc_matches
            .search_from_rev(self.text, self.cursor.caret(), start)
    }
}

/// An accumulator used specifically for editing with [`Editor`]s
#[derive(Default)]
struct Diff {
    bytes: isize,
    changes: isize,
}

impl Diff {
    /// Shifts a [`Cursor`] by the edits before it
    fn shift_cursor(&self, cursor: &mut Cursor, text: &Text, area: &impl Area, cfg: &PrintCfg) {
        cursor
            .assoc_index
            .as_mut()
            .map(|i| i.saturating_add_signed(self.changes));

        cursor.move_hor(self.bytes, text, area, cfg);
        if cursor.anchor().is_some() {
            cursor.swap_ends();
            cursor.move_hor(self.bytes, text, area, cfg);
            cursor.swap_ends();
        }
    }

    /// Returns true if the [`Text`] was altered to the same len, and
    /// no new changes took place
    fn no_change(&self) -> bool {
        self.bytes == 0 && self.changes == 0
    }
}
