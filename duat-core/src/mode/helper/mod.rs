//! A helper struct for [`Mode`]s with [`Cursors`]
//!
//! This struct can edit [`Text`] in a declarative way, freeing the
//! [`Mode`]s from worrying about synchronization of the
//! cursors and dealing with editing the text directly.
//!
//! [`Mode`]: super::Mode
pub use self::cursors::{Cursor, Cursors};
use crate::{
    binary_search_by_key_and_index,
    data::RwData,
    text::{Change, Point, PrintCfg, RegexPattern, Searcher, Text},
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
/// # use duat_core::{
/// #     data::RwData, mode::{EditHelper, Mode, KeyEvent, Cursors}, ui::Ui, widgets::File,
/// # };
/// /// A very basic example Mode.
/// #[derive(Clone)]
/// struct PlacesCharactersAndMoves;
///
/// impl<U: Ui> Mode<U> for PlacesCharactersAndMoves {
///     type Widget = File;
///     /* ... */
/// #   fn send_key(
/// #       &mut self,
/// #       key: KeyEvent,
/// #       widget: &RwData<Self::Widget>,
/// #       area: &U::Area,
/// #       cursors: &mut Cursors
/// #   ) {
/// #       todo!();
/// #   }
/// # }
/// ```
///
/// In order to modify the widget, you must implement the
/// [`Mode::send_key`] method. In it, you receive the following:
///
/// - The [key].
/// - An [`RwData`] of [`Self::Widget`].
/// - An [`Area`], which you can resize and modify it in other ways.
/// - The current [`Cursors`] of the widget, these should be modified
///   by the [`EditHelper`].
///
/// In a [`Mode`] without cursors, you'd probably want to run
/// [`Cursors::clear`], in order to make sure there are no cursors.
///
/// ```rust
/// # use duat_core::{
/// #     data::RwData, mode::{key, Cursors, EditHelper, Mode, KeyCode, KeyEvent},
/// #     ui::Ui, widgets::File,
/// # };
/// # #[derive(Clone)]
/// # struct PlacesCharactersAndMoves;
/// impl<U: Ui> Mode<U> for PlacesCharactersAndMoves {
/// #   type Widget = File;
///     /* ... */
///     fn send_key(
///         &mut self,
///         key: KeyEvent,
///         widget: &RwData<Self::Widget>,
///         area: &U::Area,
///         cursors: &mut Cursors
///     ) {
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
/// #     data::RwData, mode::{ key, Cursors, EditHelper, Mode, KeyCode, KeyEvent, KeyMod},
/// #     ui::Ui, widgets::File,
/// # };
/// # #[derive(Clone)]
/// # struct PlacesCharactersAndMoves;
/// impl<U: Ui> Mode<U> for PlacesCharactersAndMoves {
/// #   type Widget = File;
///     /* ... */
///     fn send_key(
///         &mut self,
///         key: KeyEvent,
///         widget: &RwData<Self::Widget>,
///         area: &U::Area,
///         cursors: &mut Cursors
///     ) {
///         cursors.make_excl();
///         let mut helper = EditHelper::new(widget, area, cursors);
///         
///         match key {
///             key!(KeyCode::Char(c)) => {
///                 helper.edit_each(|e| e.insert('c'));
///                 helper.move_each(|mut m| m.move_hor(1));
///             },
///             key!(KeyCode::Right, KeyMod::SHIFT) => {
///                 helper.move_each(|mut m| {
///                     if m.anchor().is_none() {
///                         m.set_anchor()
///                     }
///                     m.move_hor(1)
///                 })
///             }
///             key!(KeyCode::Right) => {
///                 helper.move_each(|mut m| {
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
/// Notice the [`Cursors::make_excl`]. In Duat, there are two types of
/// [`Cursors`], inclusive and exclusive. The only difference between
/// them is that in inclusive cursors, the selection includes the
/// character that the cursor is on, while that is not the case in
/// exclusive [`Cursors`].
///
/// [`Mode`]: super::Mode
/// [`Text`]: crate::text::Text
/// [`CommandLine`]: crate::widgets::CommandLine
/// [`RwData<Self::Widget>`]: RwData
/// [`mutate`]: RwData::mutate
/// [`inspect`]: RwData::inspect
/// [`Mode::send_key`]: super::Mode::send_key
/// [key]: super::KeyEvent
/// [`Self::Widget`]: super::Mode::Widget
/// [`Some(cursors)`]: Some
/// [`Ui::Area`]: crate::ui::Ui::Area
/// [commands]: crate::commands
/// [`key!`]: super::key
/// [`KeyEvent`]: super::KeyEvent
/// [editing]: Editor
/// [moving]: Mover
pub struct EditHelper<'a, W, A, S>
where
    W: Widget<A::Ui> + 'static,
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
    W: Widget<A::Ui> + 'static,
    A: Area,
{
    /// Returns a new instance of [`EditHelper`]
    pub fn new(widget: &'a RwData<W>, area: &'a A, cursors: &'a mut Cursors) -> Self {
        cursors.populate();
        let cfg = widget.read().print_cfg();
        EditHelper { widget, cursors, area, cfg, searcher: () }
    }
}

impl<W, A, S> EditHelper<'_, W, A, S>
where
    W: Widget<A::Ui> + 'static,
    A: Area,
{
    /// Edits on the `nth` [`Cursor`]'s selection
    ///
    /// Since the editing function takes [`Editor`] as an argument,
    /// you cannot change the selection of the [`Cursor`].
    ///
    /// If you want to move the `nth` cursor, see [`move_nth`],
    /// if you want to edit on the main cursor, see [`edit_main`],
    /// if you want to edit each cursor, see [`edit_each`].
    ///
    /// [`move_nth`]: Self::move_nth
    /// [`edit_main`]: Self::edit_main
    /// [`edit_each`]: Self::edit_each
    pub fn edit_nth(&mut self, edit: impl FnOnce(&mut Editor<A, W>), n: usize) {
        let Some((mut cursor, was_main)) = self.cursors.remove(n) else {
            panic!("Cursor index {n} out of bounds.");
        };

        let mut widget = self.widget.raw_write();
        let mut shift = (0, 0, 0);

        let c_i = {
            let changes = widget.text_mut().changes_mut();
            let start = cursor.start();

            if let Some(c_i) = cursor.change_i
                && let Some(c) = changes.get(c_i as usize)
                && (c.start() <= start && c.added_end() <= start)
            {
                c_i as usize
            } else {
                match changes.binary_search_by_key(&start, |c| c.start()) {
                    Err(i)
                        if let Some(prev_i) = i.checked_sub(1)
                            && start <= changes[prev_i].added_end() =>
                    {
                        prev_i
                    }
                    Ok(i) | Err(i) => i,
                }
            }
        };

        cursor.change_i = Some(c_i as u32);

        edit(&mut Editor::<A, W>::new(
            &mut cursor,
            &mut widget,
            self.area,
            &self.cfg,
            &mut shift,
            was_main,
            self.cursors.is_incl(),
            c_i,
            usize::MAX,
        ));

        self.cursors.insert(n, was_main, cursor);

        if shift != (0, 0, 0) {
            self.cursors
                .shift_by(n + 1, shift, widget.text(), self.area, &self.cfg);

            for change in widget.text_mut().changes_mut().iter_mut().skip(c_i + 1) {
                change.shift_by(shift);
            }
        }
    }

    /// Edits on each of the [`Cursor`]'s selection
    ///
    /// Since the editing function takes [`Editor`] as an argument,
    /// you cannot change the selection of the [`Cursor`].
    ///
    /// If you want to move each cursor, see [`move_each`],
    /// if you want to edit on a specific cursor, see [`edit_nth`]
    /// or [`edit_main`].
    ///
    /// [`move_each`]: Self::move_each
    /// [`edit_nth`]: Self::edit_nth
    /// [`edit_main`]: Self::edit_main
    pub fn edit_each(&mut self, mut f: impl FnMut(&mut Editor<A, W>)) {
        let removed: Vec<_> = self.cursors.drain().collect();

        let mut widget = self.widget.raw_write();
        let cfg = widget.print_cfg();
        let mut shift = (0, 0, 0);
        let mut sh_from = 0;

        for (i, (mut cursor, was_main)) in removed.into_iter().enumerate() {
            // A function that shifts a Point forwards in order to compare
            // correcly. This only happens if the point was not already shifted.
            let sh = |rhs: usize| if sh_from <= rhs { shift } else { (0, 0, 0) };

            let c_i = {
                cursor.shift_by(shift, widget.text(), self.area, &cfg);

                let (start, end) = cursor.point_range(self.cursors.is_incl(), widget.text());
                let changes = widget.text_mut().changes_mut();

                let (c_i, next_i) = if let Some(c_i) = cursor.change_i.map(|n| n as usize)
                    && let Some(c) = changes.get(c_i as usize)
                    && c.start().shift_by(sh(c_i)) <= start
                    && start <= c.added_end().shift_by(sh(c_i))
                {
                    (c_i as usize, c_i + 1)
                } else {
                    let f = |i: usize, c: &Change| c.start().shift_by(sh(i));
                    match binary_search_by_key_and_index(changes, start, f) {
                        Ok(i) => (i, i + 1),
                        Err(i)
                            if let Some(prev_i) = i.checked_sub(1)
                                && start <= changes[prev_i].added_end().shift_by(sh(prev_i)) =>
                        {
                            (prev_i, prev_i + 1)
                        }
                        Err(i) => (i, i),
                    }
                };

                // Check if the next change is being intersected.
                let end_i = if changes
                    .get(next_i)
                    .is_none_or(|c| end < c.start().shift_by(sh(c_i + 1)))
                {
                    next_i
                // If it is, find the last change that is also being
                // intersected.
                } else {
                    let f = |i: usize, c: &Change| c.start().shift_by(sh(next_i + i));
                    let (Ok(end_i) | Err(end_i)) =
                        binary_search_by_key_and_index(&changes[next_i..], end, f);
                    next_i + end_i
                };

                if shift != (0, 0, 0) {
                    for change in changes.iter_mut().take(end_i).skip(sh_from) {
                        change.shift_by(shift)
                    }
                }

                sh_from = end_i;

                c_i
            };

            cursor.change_i = Some(c_i as u32);
            let mut editor = Editor::new(
                &mut cursor,
                &mut *widget,
                self.area,
                &self.cfg,
                &mut shift,
                was_main,
                self.cursors.is_incl(),
                c_i,
                sh_from,
            );
            f(&mut editor);

            sh_from = (sh_from as i32 + editor.change_diff) as usize;

            self.cursors.insert(i, was_main, cursor);
        }

        let changes = widget.text_mut().changes_mut();
        if shift != (0, 0, 0) {
            for change in changes.iter_mut().skip(sh_from) {
                change.shift_by(shift);
            }
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
    /// If you want to edit on the `nth` cursor, see [`edit_nth`],
    /// if you want to move the main cursor, see [`move_main`], if you
    /// want to move each cursor, see [`move_each`].
    ///
    /// [`edit_nth`]: Self::edit_nth
    /// [`move_main`]: Self::move_main
    /// [`move_each`]: Self::move_each
    pub fn move_nth<_T>(&mut self, n: usize, mov: impl FnOnce(Mover<A, S>) -> _T) {
        let Some((cursor, is_main)) = self.cursors.remove(n) else {
            panic!("Cursor index {n} out of bounds.");
        };
        let mut widget = self.widget.raw_write();

        let mut cursor = Some(cursor);
        mov(Mover::new(
            &mut cursor,
            is_main,
            widget.text_mut(),
            self.area,
            self.cursors,
            self.cfg,
            &mut self.searcher,
        ));

        if let Some(cursor) = cursor {
            self.cursors.insert(n, is_main, cursor);
        }
    }

    /// Moves each [`Cursor`]'s selection
    ///
    /// Since the moving function takes [`Mover`] as an argument, this
    /// method cannot be used to change the [`Text`] in any way.
    ///
    /// At the end of the movement, if any of the cursors intersect
    /// with each other, they will be merged into one.
    ///
    /// If you want to edit on each cursor, see [`edit_each`],
    /// if you want to move a specific cursor, see [`move_nth`]
    /// or [`move_main`].
    ///
    /// [`edit_each`]: Self::edit_each
    /// [`move_nth`]: Self::move_nth
    /// [`move_main`]: Self::move_main
    pub fn move_each<_T>(&mut self, mut mov: impl FnMut(Mover<A, S>) -> _T) {
        let removed_cursors: Vec<(Cursor, bool)> = self.cursors.drain().collect();

        let mut widget = self.widget.raw_write();

        for (i, (cursor, is_main)) in removed_cursors.into_iter().enumerate() {
            let mut cursor = Some(cursor);
            mov(Mover::new(
                &mut cursor,
                is_main,
                widget.text_mut(),
                self.area,
                self.cursors,
                self.cfg,
                &mut self.searcher,
            ));

            if let Some(cursor) = cursor {
                self.cursors.insert(i, is_main, cursor);
            }
        }
    }

    /// Edits on the main [`Cursor`]'s selection
    ///
    /// Since the editing function takes [`Editor`] as an argument,
    /// you cannot change the selection of the [`Cursor`].
    ///
    /// If you want to move the main cursor, see [`move_main`],
    /// if you want to edit on the `nth` cursor, see [`edit_nth`],
    /// if you want to edit each cursor, see [`edit_each`].
    ///
    /// [`move_main`]: Self::move_main
    /// [`edit_nth`]: Self::edit_nth
    /// [`edit_each`]: Self::edit_each
    pub fn edit_main(&mut self, edit: impl FnOnce(&mut Editor<A, W>)) {
        self.edit_nth(edit, self.cursors.main_index());
    }

    /// Moves the main [`Cursor`]'s selection
    ///
    /// Since the moving function takes [`Mover`] as an argument, this
    /// method cannot be used to change the [`Text`] in any way.
    ///
    /// At the end of the movement, if the cursor intersects any
    /// other, they will be merged into one.
    ///
    /// If you want to move the main cursor, see [`edit_main`],
    /// if you want to move the main cursor, see [`move_main`], if you
    /// want to move each cursor, see [`move_each`].
    ///
    /// [`edit_main`]: Self::edit_main
    /// [`move_main`]: Self::move_main
    /// [`move_each`]: Self::move_each
    pub fn move_main<_T>(&mut self, mov: impl FnOnce(Mover<A, S>) -> _T) {
        self.move_nth(self.cursors.main_index(), mov);
    }

    /// Removes all but the main cursor from the list
    pub fn remove_extra_cursors(&mut self) {
        self.cursors.remove_extras();
    }

    pub fn rotate_main(&mut self, amount: i32) {
        self.cursors.rotate_main(amount);
    }

    /// The [`Cursors`] in use
    pub fn cursors(&self) -> &Cursors {
        self.cursors
    }

    /// The [`PrintCfg`] in use
    pub fn cfg(&self) -> PrintCfg {
        self.cfg
    }

    /// Returns the lenght of the [`Text`], in [`Point`]
    pub fn len(&self) -> Point {
        self.widget.read().text().len()
    }

    /// Returns the position of the last [`char`] if there is one
    pub fn last_point(&self) -> Option<Point> {
        self.widget.read().text().last_point()
    }

    /// Begins a new moment
    ///
    /// A new moment indicates a break in the history of the [`Text`],
    /// that is, if you [`undo`], the changes prior to the
    /// creation of this moment will be kept.
    ///
    /// [`undo`]: EditHelper::undo
    pub fn new_moment(&mut self) {
        self.widget.raw_write().text_mut().new_moment();
    }

    /// Undoes the last moment
    pub fn undo(&mut self) {
        let mut widget = self.widget.raw_write();
        let cfg = widget.print_cfg();
        widget.text_mut().undo(self.area, self.cursors, cfg);
        widget.update(self.area);
    }

    /// Redoes the next moment
    pub fn redo(&mut self) {
        let mut widget = self.widget.raw_write();
        let cfg = widget.print_cfg();
        widget.text_mut().redo(self.area, self.cursors, cfg);
        widget.update(self.area);
    }
}

impl<'a, A> EditHelper<'a, File, A, Searcher>
where
    A: Area,
{
    /// Returns a new instance of [`EditHelper`]
    pub fn new_inc(
        widget: &'a RwData<File>,
        area: &'a A,
        cursors: &'a mut Cursors,
        searcher: Searcher,
    ) -> Self {
        cursors.populate();
        let cfg = {
            let mut file = widget.raw_write();
            let cfg = <File as Widget<A::Ui>>::print_cfg(&file);
            <File as Widget<A::Ui>>::text_mut(&mut file).remove_cursors(cursors, area, cfg);
            <File as Widget<A::Ui>>::print_cfg(&file)
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
/// # use duat_core::{mode::EditHelper, ui::Area, widgets::File};
/// # fn test<S>(helper: &mut EditHelper<File, impl Area, S>) {
/// helper.edit_main(|e| {
///     e.replace("my replacement");
///     e.insert(" and my edit");
/// });
/// helper.move_main(|mut m| {
///     m.move_hor(" and my edit".chars().count() as isize);
///     m.set_anchor();
///     m.move_hor(-("my replacement and my edit".chars().count() as isize));
///     let sel: String = m.selection().into_iter().collect();
///     assert_eq!(sel, "my replacement and my edit".to_string());
/// });
/// # }
/// ```
///
/// [`edit_*`]: EditHelper::edit_nth
/// [`replace`]: Editor::replace
/// [`insert`]: Editor::insert
pub struct Editor<'a, 'b, A, W>
where
    A: Area,
    W: Widget<A::Ui>,
{
    cursor: &'a mut Cursor,
    widget: &'b mut W,
    area: &'b A,
    cfg: &'a PrintCfg,
    shift: &'a mut (isize, isize, isize),
    is_main: bool,
    is_incl: bool,
    change_i: usize,
    sh_from: usize,
    change_diff: i32,
}

impl<'a, 'b, A, W> Editor<'a, 'b, A, W>
where
    A: Area,
    W: Widget<A::Ui>,
{
    /// Returns a new instance of [`Editor`]
    #[allow(clippy::too_many_arguments)]
    fn new(
        cursor: &'a mut Cursor,
        widget: &'b mut W,
        area: &'b A,
        cfg: &'a PrintCfg,
        shift: &'a mut (isize, isize, isize),
        is_main: bool,
        is_incl: bool,
        change_i: usize,
        sh_from: usize,
    ) -> Self {
        Self {
            cursor,
            widget,
            area,
            cfg,
            shift,
            is_main,
            is_incl,
            change_i,
            sh_from,
            change_diff: 0,
        }
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
        let change = Change::new(
            edit.to_string(),
            self.cursor.point_range(self.is_incl, self.widget.text()),
            self.widget.text(),
        );
        let edit_len = change.added_text().len();
        let end = change.added_end();

        self.edit(change);

        let text = self.widget.text();

        if let Some(anchor) = self.cursor.anchor()
            && anchor >= self.cursor.caret()
            && edit_len > 0
        {
            self.cursor.swap_ends();
            self.cursor.move_to(end, text, self.area, self.cfg);
            self.cursor.swap_ends();
        } else {
            self.cursor.unset_anchor();
            self.cursor.move_to(end, text, self.area, self.cfg);
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
        let range = (self.cursor.caret(), self.cursor.caret());
        let change = Change::new(edit.to_string(), range, self.widget.text());
        let diff = change.chars_diff();

        self.edit(change);

        if let Some(anchor) = self.cursor.anchor()
            && anchor >= self.cursor.caret()
        {
            self.cursor.swap_ends();
            self.cursor
                .move_hor(diff, self.widget.text(), self.area, self.cfg);
            self.cursor.swap_ends();
        }
    }

    /// Edits the file with a [`Change`]
    fn edit(&mut self, change: Change) {
        let shift = *self.shift;
        self.shift.0 += change.added_end().byte() as isize - change.taken_end().byte() as isize;
        self.shift.1 += change.added_end().char() as isize - change.taken_end().char() as isize;
        self.shift.2 += change.added_end().line() as isize - change.taken_end().line() as isize;
        let (_, diff) = unsafe {
            self.widget
                .text_mut()
                .apply_desync_change(self.change_i, change, shift, self.sh_from)
        };

        self.change_diff += diff;
    }

    pub fn is_main(&self) -> bool {
        self.is_main
    }

    pub fn is_incl(&self) -> bool {
        self.is_incl
    }
}

/// A cursor that can alter the selection, but can't edit
pub struct Mover<'a, A, S>
where
    A: Area,
{
    cursor: &'a mut Option<Cursor>,
    is_main: bool,
    text: &'a mut Text,
    area: &'a A,
    cursors: &'a mut Cursors,
    cfg: PrintCfg,
    inc_searcher: &'a mut S,
}

impl<'a, A, S> Mover<'a, A, S>
where
    A: Area,
{
    /// Returns a new instance of `Mover`
    fn new(
        cursor: &'a mut Option<Cursor>,
        is_main: bool,
        text: &'a mut Text,
        area: &'a A,
        cursors: &'a mut Cursors,
        cfg: PrintCfg,
        inc_searcher: &'a mut S,
    ) -> Self {
        Self {
            cursor,
            is_main,
            text,
            area,
            cursors,
            cfg,
            inc_searcher,
        }
    }

    ////////// Movement functions

    /// Moves the cursor horizontally. May cause vertical movement
    pub fn move_hor(&mut self, count: isize) {
        let cursor = self.cursor.as_mut().unwrap();
        cursor.move_hor(count, self.text, self.area, &self.cfg);
    }

    /// Moves the cursor vertically. May cause horizontal movement
    pub fn move_ver(&mut self, count: isize) {
        let cursor = self.cursor.as_mut().unwrap();
        cursor.move_ver(count, self.text, self.area, &self.cfg);
    }

    /// Moves the cursor vertically. May cause horizontal movement
    pub fn move_ver_wrapped(&mut self, count: isize) {
        let cursor = self.cursor.as_mut().unwrap();
        cursor.move_ver_wrapped(count, self.text, self.area, &self.cfg);
    }

    /// Moves the cursor to a [`Point`]
    ///
    /// - If the position isn't valid, it will move to the "maximum"
    ///   position allowed.
    pub fn move_to(&mut self, point: Point) {
        let cursor = self.cursor.as_mut().unwrap();
        cursor.move_to(point, self.text, self.area, &self.cfg);
    }

    /// Moves the cursor to a `line` and a `column`
    ///
    /// - If the coords isn't valid, it will move to the "maximum"
    ///   position allowed.
    pub fn move_to_coords(&mut self, line: usize, col: usize) {
        let at = self.text.point_at_line(line.min(self.text.len().line()));
        let (point, _) = self.text.chars_fwd(at).take(col + 1).last().unwrap();
        self.move_to(point);
    }

    ////////// Cursor addition and removal

    /// Copies the current [`Cursor`] in place
    ///
    /// This will leave an additional [`Cursor`] with the current
    /// selection. Do note that normal intersection rules apply, so,
    /// if at the end of the movement, this cursor intersects with any
    /// other, one of them will be deleted.
    ///
    /// Returns the index of the new [`Cursor`], note that this might
    /// change throughout the movement function, as new cursors might
    /// be added before it, moving it ahead.
    pub fn copy(&mut self) -> usize {
        self.cursors.insert(0, false, self.cursor.unwrap())
    }

    /// Destroys the current [`Cursor`]
    ///
    /// Will not destroy it if it is the last [`Cursor`] left
    ///
    /// If this was the main cursor, the main cursor will now be the
    /// cursor immediately behind it.
    pub fn destroy(self) {
        if self.cursors.len() > 1 {
            *self.cursor = None;
        }
    }

    ////////// Anchor Manipulation

    /// Returns and takes the anchor of the [`Cursor`].
    pub fn unset_anchor(&mut self) -> Option<Point> {
        let cursor = self.cursor.as_mut().unwrap();
        cursor.unset_anchor()
    }

    /// Sets the `anchor` to the current `caret`
    pub fn set_anchor(&mut self) {
        let cursor = self.cursor.as_mut().unwrap();
        cursor.set_anchor()
    }

    /// Swaps the position of the `caret` and `anchor`
    pub fn swap_ends(&mut self) {
        let cursor = self.cursor.as_mut().unwrap();
        cursor.swap_ends();
    }

    ////////// Text queries

    /// Returns the [`char`] in the `caret`
    pub fn char(&self) -> char {
        self.text.char_at(self.cursor.unwrap().caret()).unwrap()
    }

    /// Returns the [`Cursor`]'s selection
    ///
    /// The reason why this return value is `[&str; 2]` is because the
    /// [`Text`] utilizes an underlying [`GapBuffer`] to store the
    /// characters. This means that the text is always separated into
    /// two distinct chunks.
    ///
    /// If this [`Cursor`]'s selection happens to be entirely within
    /// one of these chunks, the other `&str` will just be empty.
    ///
    /// [`GapBuffer`]: gapbuf::GapBuffer
    pub fn selection(&self) -> [&str; 2] {
        let anchor = self.anchor().unwrap_or(self.caret());
        let range = if anchor < self.caret() {
            (anchor, self.caret())
        } else {
            (self.caret(), anchor)
        };
        self.text.strs_in_range(range)
    }

    /// Returns the lenght of the [`Text`], in [`Point`]
    pub fn len(&self) -> Point {
        self.text.len()
    }

    /// Returns the position of the last [`char`] if there is one
    pub fn last_point(&self) -> Option<Point> {
        self.text.last_point()
    }

    ////////// Iteration functions

    /// Iterates over the [`char`]s
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn iter(&self) -> impl Iterator<Item = (Point, char)> + '_ {
        self.text.chars_fwd(self.caret())
    }

    /// Iterates over the [`char`]s, in reverse
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn iter_rev(&self) -> impl Iterator<Item = (Point, char)> + '_ {
        self.text.chars_rev(self.caret())
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
    ///     helper.move_each(|mut m| {
    ///         let mut nth = m.search('(', None).nth(n);
    ///         if let Some((start, end)) = nth {
    ///             m.move_to(start);
    ///             m.set_anchor();
    ///             m.move_to(end);
    ///         }
    ///     })
    /// }
    /// ```
    pub fn search_fwd<R: RegexPattern>(
        &mut self,
        pat: R,
        end: Option<Point>,
    ) -> impl Iterator<Item = R::Match> + '_ {
        let cursor = self.cursor.unwrap();
        self.text.search_fwd(pat, cursor.caret(), end).unwrap()
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
    ///     helper.move_each(|mut m| {
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
        self.text.search_rev(pat, self.caret(), start).unwrap()
    }

    ////////// Cursor queries

    /// Returns the `caret`
    pub fn caret(&self) -> Point {
        self.cursor.unwrap().caret()
    }

    /// Returns the `anchor`
    pub fn anchor(&self) -> Option<Point> {
        self.cursor.unwrap().anchor()
    }

    /// Returns `true` if the `anchor` exists before the `caret`
    pub fn anchor_is_start(&self) -> bool {
        self.anchor().is_none_or(|anchor| anchor < self.caret())
    }

    /// Wether or not this is the main [`Cursor`]
    pub fn is_main(&self) -> bool {
        self.is_main
    }

    /// Wether or not this cursor's selections are inclusive
    pub fn is_incl(&self) -> bool {
        self.cursors.is_incl()
    }
}

/// Incremental search functions, only available on [`IncSearcher`]s
///
/// [`IncSearcher`]: crate::mode::IncSearcher
impl<A> Mover<'_, A, Searcher>
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
    pub fn search_inc_fwd(
        &mut self,
        end: Option<Point>,
    ) -> impl Iterator<Item = (Point, Point)> + '_ {
        self.inc_searcher.search_fwd(self.text, self.caret(), end)
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
        self.inc_searcher.search_rev(self.text, self.caret(), start)
    }

    /// Wether the [`Cursor`]'s selection matches the [`IncSearch`]
    /// request
    ///
    /// [`IncSearch`]: crate::widgets::IncSearch
    pub fn matches_inc(&mut self) -> bool {
        let range = self.cursor.unwrap().range(self.cursors.is_incl());
        self.text.make_contiguous_in(range.clone());
        let str = unsafe { self.text.continuous_in_unchecked(range) };

        self.inc_searcher.matches(str)
    }
}
