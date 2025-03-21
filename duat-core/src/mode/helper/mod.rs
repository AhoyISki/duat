//! A helper struct for [`Mode`]s with [`Cursors`]
//!
//! This struct can edit [`Text`] in a declarative way, freeing the
//! [`Mode`]s from worrying about synchronization of the
//! cursors and dealing with editing the text directly.
//!
//! [`Mode`]: super::Mode
use std::ops::RangeBounds;

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
///         helper.cursors_mut().make_excl();
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
/// Notice the [`Cursors::make_excl`]. In Duat, there are two types of
/// [`Cursors`], inclusive and exclusive. The only difference between
/// them is that in inclusive cursors, the selection acts like a Rust
/// inclusive selection (`..=`), while in exclusive cursors, it acts
/// like an exclusive selection (`..`).
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
pub struct EditHelper<'a, W, A, S>
where
    W: Widget<A::Ui> + 'static,
    A: Area,
{
    widget: &'a mut W,
    area: &'a A,
    searcher: S,
}

impl<'a, W, A> EditHelper<'a, W, A, ()>
where
    W: Widget<A::Ui> + 'static,
    A: Area,
{
    /// Returns a new instance of [`EditHelper`]
    pub fn new(widget: &'a mut W, area: &'a A) -> Self {
        widget.text_mut().enable_cursors();
        widget.cursors_mut().unwrap().populate();
        EditHelper { widget, area, searcher: () }
    }
}

impl<W, A, S> EditHelper<'_, W, A, S>
where
    W: Widget<A::Ui> + 'static,
    A: Area,
{
    ////////// Editing functions

    /// Edits on the `nth` [`Cursor`]'s selection
    ///
    /// Since the editing function takes [`Editor`] as an argument,
    /// you cannot change the selection of the [`Cursor`].
    ///
    /// If you want to move the `nth` cursor, see [`move_nth`],
    /// if you want to edit on the main cursor, see [`edit_main`],
    /// if you want to edit each cursor, see [`edit_many`].
    ///
    /// [`move_nth`]: Self::move_nth
    /// [`edit_main`]: Self::edit_main
    /// [`edit_many`]: Self::edit_many
    pub fn edit_nth(&mut self, n: usize, edit: impl FnOnce(&mut Editor<A, W>)) {
        let cursors = self.widget.cursors_mut().unwrap();
        let Some((mut cursor, was_main)) = cursors.remove(n) else {
            panic!("Cursor index {n} out of bounds");
        };

        let mut shift = (0, 0, 0);

        edit(&mut Editor::<A, W>::new(
            &mut cursor,
            self.widget,
            self.area,
            &mut shift,
            was_main,
        ));

        let cursors = self.widget.cursors_mut().unwrap();
        cursors.insert(n, was_main, cursor);

        self.widget.text_mut().shift_cursors(n + 1, shift);
    }

    /// Edits on the main [`Cursor`]'s selection
    ///
    /// Since the editing function takes [`Editor`] as an argument,
    /// you cannot change the selection of the [`Cursor`].
    ///
    /// If you want to move the main cursor, see [`move_main`],
    /// if you want to edit on the `nth` cursor, see [`edit_nth`],
    /// if you want to edit each cursor, see [`edit_many`].
    ///
    /// [`move_main`]: Self::move_main
    /// [`edit_nth`]: Self::edit_nth
    /// [`edit_many`]: Self::edit_many
    pub fn edit_main(&mut self, edit: impl FnOnce(&mut Editor<A, W>)) {
        let n = self.widget.cursors().unwrap().main_index();
        self.edit_nth(n, edit);
    }

    /// Edits on a range of [`Cursor`]s
    ///
    /// Since the editing function takes [`Editor`] as an argument,
    /// you cannot change the selection of the [`Cursor`].
    ///
    /// If you want to move many cursors, see [`move_many`],
    /// if you want to edit on a specific cursor, see [`edit_nth`]
    /// or [`edit_main`].
    ///
    /// [`move_many`]: Self::move_many
    /// [`edit_nth`]: Self::edit_nth
    /// [`edit_main`]: Self::edit_main
    pub fn edit_many(
        &mut self,
        range: impl RangeBounds<usize> + Clone,
        mut f: impl FnMut(&mut Editor<A, W>),
    ) {
        let cursors = self.widget.cursors_mut().unwrap();
        let (start, end) = crate::get_ends(range, cursors.len());
        assert!(end <= cursors.len(), "Cursor index {end} out of bounds");
        let mut removed: Vec<(Cursor, bool)> = cursors.drain(start..).collect();

        let mut shift = (0, 0, 0);

        for (i, (mut cursor, was_main)) in removed.splice(..(end - start), []).enumerate() {
            let guess_i = i + start;
            cursor.shift_by(shift);

            let mut editor = Editor::new(&mut cursor, self.widget, self.area, &mut shift, was_main);
            f(&mut editor);

            self.widget
                .cursors_mut()
                .unwrap()
                .insert(guess_i, was_main, cursor);
        }

        for (i, (mut cursor, was_main)) in removed.into_iter().enumerate() {
            let guess_i = i + end;
            cursor.shift_by(shift);
            self.widget
                .cursors_mut()
                .unwrap()
                .insert(guess_i, was_main, cursor);
        }
    }

    ////////// Moving functions

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
    /// want to move each cursor, see [`move_many`].
    ///
    /// [`edit_nth`]: Self::edit_nth
    /// [`move_main`]: Self::move_main
    /// [`move_many`]: Self::move_many
    pub fn move_nth<_T>(&mut self, n: usize, mov: impl FnOnce(Mover<A, S>) -> _T) {
        let cfg = self.cfg();
        let text = self.widget.text_mut();
        let Some((cursor, is_main)) = text.cursors_mut().unwrap().remove(n) else {
            panic!("Cursor index {n} out of bounds");
        };

        let mut cursor = Some(cursor);

        mov(Mover::new(
            &mut cursor,
            is_main,
            text,
            self.area,
            cfg,
            &mut self.searcher,
        ));

        if let Some(cursor) = cursor {
            text.cursors_mut().unwrap().insert(n, is_main, cursor);
        }
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
    /// want to move each cursor, see [`move_many`].
    ///
    /// [`edit_main`]: Self::edit_main
    /// [`move_main`]: Self::move_main
    /// [`move_many`]: Self::move_many
    pub fn move_main<_T>(&mut self, mov: impl FnOnce(Mover<A, S>) -> _T) {
        let n = self.widget.cursors().unwrap().main_index();
        self.move_nth(n, mov);
    }

    /// Moves a range of [`Cursor`]'s selections
    ///
    /// Since the moving function takes [`Mover`] as an argument, this
    /// method cannot be used to change the [`Text`] in any way.
    ///
    /// At the end of the movement, if any of the cursors intersect
    /// with each other, they will be merged into one.
    ///
    /// If you want to edit on many cursors, see [`edit_many`],
    /// if you want to move a specific cursor, see [`move_nth`]
    /// or [`move_main`].
    ///
    /// [`edit_many`]: Self::edit_many
    /// [`move_nth`]: Self::move_nth
    /// [`move_main`]: Self::move_main
    pub fn move_many<_T>(
        &mut self,
        range: impl RangeBounds<usize> + Clone,
        mut mov: impl FnMut(Mover<A, S>) -> _T,
    ) {
        let cfg = self.cfg();
        let text = self.widget.text_mut();
        let cursors = text.cursors_mut().unwrap();
        let (start, end) = crate::get_ends(range.clone(), cursors.len());
        assert!(end <= cursors.len(), "Cursor index {end} out of bounds");
        let removed_cursors: Vec<(Cursor, bool)> = cursors.drain(range).collect();

        for (i, (cursor, is_main)) in removed_cursors.into_iter().enumerate() {
            let guess_i = i + start;
            let mut cursor = Some(cursor);
            mov(Mover::new(
                &mut cursor,
                is_main,
                text,
                self.area,
                cfg,
                &mut self.searcher,
            ));

            if let Some(cursor) = cursor {
                text.cursors_mut().unwrap().insert(guess_i, is_main, cursor);
            }
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

        EditHelper { widget, area, searcher }
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
pub struct Editor<'a, 'b, A, W>
where
    A: Area,
    W: Widget<A::Ui>,
{
    cursor: &'a mut Cursor,
    widget: &'b mut W,
    area: &'b A,
    shift: &'a mut (i32, i32, i32),
    is_main: bool,
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
        shift: &'a mut (i32, i32, i32),
        is_main: bool,
    ) -> Self {
        Self { cursor, widget, area, shift, is_main }
    }

    /// Replaces the entire selection with new text
    ///
    /// If the `caret` is behind the `anchor` (or in the same spot),
    /// after replacing the selection, the `caret` will be placed on
    /// the start of the selection, while the `anchor` will be placed
    /// on the new end. If it is ahead, it will be placed ahead.
    ///
    /// If there is no selection, then this has the same effect as
    /// [`insert`].
    ///
    /// [`insert`]: Self::insert
    pub fn replace(&mut self, edit: impl ToString) {
        let (p0, p1) = self.cursor.point_range();
        let change = Change::new(edit.to_string(), (p0, p1), self.widget.text());
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
    /// The selection remains unaltered, if the `anchor` is ahead of
    /// the `caret`, it will move forwards by `edit.chars().count()`.
    ///
    /// If you wish to replace the selected text, see [`replace`]
    ///
    /// [`replace`]: Self::replace
    pub fn insert(&mut self, edit: impl ToString) {
        let range = (self.cursor.caret(), self.cursor.caret());
        let change = Change::new(edit.to_string(), range, self.widget.text());
        let (added, taken) = (change.added_end(), change.taken_end());

        self.edit(change);

        if let Some(anchor) = self.cursor.anchor()
            && anchor >= self.cursor.caret()
        {
            let new_anchor = anchor + added - taken;
            self.cursor.swap_ends();
            self.cursor.move_to(new_anchor, self.widget.text());
            self.cursor.swap_ends();
        }
    }

    /// If there is a selection, acts like [`replace`], otherwise acts
    /// like [`insert`]
    ///
    /// This only makes a difference if your selections are
    /// [inclusive], since a [`replace`] when the anchor is [`None`]
    /// would still include one character.
    ///
    /// [`replace`]: Editor::replace
    /// [`insert`]: Editor::insert
    /// [inclusive]: Cursors::is_incl
    pub fn insert_or_replace(&mut self, edit: impl ToString) {
        if self.anchor().is_some() {
            self.replace(edit)
        } else {
            self.insert(edit)
        }
    }

    /// Edits the file with a [`Change`]
    fn edit(&mut self, change: Change<String>) {
        self.shift.0 += change.added_end().byte() as i32 - change.taken_end().byte() as i32;
        self.shift.1 += change.added_end().char() as i32 - change.taken_end().char() as i32;
        self.shift.2 += change.added_end().line() as i32 - change.taken_end().line() as i32;

        let text = self.widget.text_mut();
        let change_i = text.apply_change(self.cursor.change_i.map(|i| i as usize), change);
        self.cursor.change_i = change_i.map(|i| i as u32)
    }

    ////////// Iteration functions

    /// Iterates over the [`char`]s
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn iter(&self) -> impl Iterator<Item = (Point, char)> + '_ {
        self.widget.text().chars_fwd(self.caret())
    }

    /// Iterates over the [`char`]s, in reverse
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn iter_rev(&self) -> impl Iterator<Item = (Point, char)> + '_ {
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

    ////////// Anchor modification

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
    pub fn set_caret_on_start(&mut self) {
        if let Some(anchor) = self.anchor()
            && anchor < self.caret()
        {
            self.swap_ends();
        }
    }

    /// Sets the caret of the [`Cursor`] on the end of the
    /// selection
    pub fn set_caret_on_end(&mut self) {
        if let Some(anchor) = self.anchor()
            && anchor > self.caret()
        {
            self.swap_ends();
        }
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
        let anchor = self.anchor().unwrap_or(self.caret());
        let (start, end) = if anchor < self.caret() {
            (anchor, self.caret())
        } else {
            (self.caret(), anchor)
        };
        self.text().strs(start.byte()..end.byte())
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
    pub fn indent(&mut self) -> usize {
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

    pub fn v_caret(&mut self) -> VPoint {
        self.cursor
            .v_caret(self.widget.text(), self.area, self.widget.print_cfg())
    }

    pub fn v_anchor(&mut self) -> Option<VPoint> {
        self.cursor
            .v_anchor(self.widget.text(), self.area, self.widget.print_cfg())
    }

    /// Returns `true` if the `anchor` exists before the `caret`
    pub fn anchor_is_start(&self) -> bool {
        self.anchor().is_none_or(|anchor| anchor < self.caret())
    }

    /// Whether or not this is the main [`Cursor`]
    pub fn is_main(&self) -> bool {
        self.is_main
    }

    pub fn text(&self) -> &Text {
        self.widget.text()
    }

    /// The [`PrintCfg`] in use
    pub fn cfg(&self) -> PrintCfg {
        self.widget.print_cfg()
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
    cfg: PrintCfg,
    inc_searcher: &'a mut S,
    initial: Cursor,
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
        cfg: PrintCfg,
        inc_searcher: &'a mut S,
    ) -> Self {
        let initial = cursor.clone().unwrap();
        Self {
            cursor,
            is_main,
            text,
            area,
            cfg,
            inc_searcher,
            initial,
        }
    }

    ////////// Movement functions

    /// Moves the cursor horizontally. May cause vertical movement
    pub fn move_hor(&mut self, count: i32) {
        let cursor = self.cursor.as_mut().unwrap();
        cursor.move_hor(count, self.text);
    }

    /// Moves the cursor vertically. May cause horizontal movement
    pub fn move_ver(&mut self, count: i32) {
        let cursor = self.cursor.as_mut().unwrap();
        cursor.move_ver(count, self.text, self.area, self.cfg);
    }

    /// Moves the cursor vertically. May cause horizontal movement
    pub fn move_ver_wrapped(&mut self, count: i32) {
        let cursor = self.cursor.as_mut().unwrap();
        cursor.move_ver_wrapped(count, self.text, self.area, self.cfg);
    }

    /// Moves the cursor to a [`Point`]
    ///
    /// - If the position isn't valid, it will move to the "maximum"
    ///   position allowed.
    pub fn move_to(&mut self, point: Point) {
        let cursor = self.cursor.as_mut().unwrap();
        cursor.move_to(point, self.text);
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
    /// selection. Do note that normal intersection rules apply, so if
    /// at the end of the movement, this cursor intersects with any
    /// other, they will be merged into one.
    pub fn copy(&mut self) {
        let cursors = self.text.cursors_mut().unwrap();
        cursors.insert(0, false, self.cursor.clone().unwrap());
    }

    /// Copies the current [`Cursor`] and applies a function to it
    ///
    /// This will let you modify a new selection taken from the
    /// current [`Cursor`], and then will insert this new copy in
    /// the [`Cursors`] list. Do note that normal intersection
    /// rules apply, so if at the end of the movement, this cursor
    /// intersects with any other, they will be merged into one.
    pub fn copy_and(&mut self, mov: impl for<'b> FnOnce(Mover<'b, A, S>)) {
        let mut copy = Some(self.cursor.clone().unwrap());
        mov(Mover::new(
            &mut copy,
            false,
            self.text,
            self.area,
            self.cfg,
            self.inc_searcher,
        ));
        if let Some(copy) = copy {
            self.text.cursors_mut().unwrap().insert(0, false, copy);
        }
    }

    /// Destroys the current [`Cursor`]
    ///
    /// Will not destroy it if it is the last [`Cursor`] left
    ///
    /// If this was the main cursor, the main cursor will now be the
    /// cursor immediately behind it.
    pub fn destroy(self) {
        if self.text.cursors().unwrap().len() > 1 {
            *self.cursor = None;
        }
    }

    ////////// Selection manipulation

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

    /// Sets the caret of the [`Cursor`] on the start of the
    /// selection
    pub fn set_caret_on_start(&mut self) {
        if let Some(anchor) = self.anchor()
            && anchor < self.caret()
        {
            self.swap_ends();
        }
    }

    /// Sets the caret of the [`Cursor`] on the end of the
    /// selection
    pub fn set_caret_on_end(&mut self) {
        if let Some(anchor) = self.anchor()
            && anchor > self.caret()
        {
            self.swap_ends();
        }
    }

    /// Resets the [`Cursor`] to how it was before being modified
    pub fn reset(&mut self) {
        *self.cursor = Some(self.initial.clone())
    }

    ////////// Text queries

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
        let range = self.cursor.as_ref().unwrap().range();
        self.text.strs(range)
    }

    /// Returns the length of the [`Text`], in [`Point`]
    pub fn len(&self) -> Point {
        self.text.len()
    }

    /// Returns the position of the last [`char`] if there is one
    pub fn last_point(&self) -> Option<Point> {
        self.text.last_point()
    }

    /// Returns the [`char`] in the `caret`
    pub fn char(&self) -> char {
        self.text
            .char_at(self.cursor.as_ref().unwrap().caret())
            .unwrap()
    }

    /// Returns the [`char`] at a given [`Point`]
    pub fn char_at(&self, p: Point) -> Option<char> {
        self.text.char_at(p)
    }

    /// An [`Iterator`] over the lines in a given [range]
    ///
    /// [range]: TextRange
    pub fn lines_on(
        &mut self,
        range: impl TextRange,
    ) -> impl DoubleEndedIterator<Item = (usize, &'_ str)> + '_ {
        self.text.lines(range)
    }

    /// Gets the current level of indentation
    pub fn indent(&mut self) -> usize {
        self.text.indent(self.caret(), self.area, self.cfg)
    }

    /// Gets the indentation level on the given [`Point`]
    pub fn indent_on(&mut self, p: Point) -> usize {
        self.text.indent(p, self.area, self.cfg)
    }

    /// Gets a [`Reader`]'s [public facing API], if it exists
    ///
    /// [public facing API]: Reader::PublicReader
    pub fn get_reader<R: Reader>(&mut self) -> Option<R::PublicReader<'_>> {
        self.text.get_reader::<R>()
    }

    ////////// Iteration functions

    /// Iterates over the [`char`]s
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn fwd(&self) -> impl Iterator<Item = (Point, char)> + '_ {
        self.text.chars_fwd(self.caret())
    }

    /// Iterates over the [`char`]s, in reverse
    ///
    /// This iteration will begin on the `caret`. It will also include
    /// the [`Point`] of each `char`
    pub fn rev(&self) -> impl Iterator<Item = (Point, char)> + '_ {
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
        let start = self.cursor.as_ref().unwrap().caret();
        self.text.search_fwd(pat, (start, end)).unwrap()
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
        let end = self.cursor.as_ref().unwrap().caret();
        self.text.search_rev(pat, (start, end)).unwrap()
    }

    ////////// Behavior changes

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
        let cursor = self.cursor.as_mut().unwrap();
        cursor.set_desired_cols(x, x);
    }

    ////////// Queries

    /// Returns the `caret`
    pub fn caret(&self) -> Point {
        self.cursor.as_ref().unwrap().caret()
    }

    /// Returns the `anchor`
    pub fn anchor(&self) -> Option<Point> {
        self.cursor.as_ref().unwrap().anchor()
    }

    pub fn v_caret(&self) -> VPoint {
        self.cursor
            .as_ref()
            .unwrap()
            .v_caret(self.text, self.area, self.cfg)
    }

    pub fn v_anchor(&self) -> Option<VPoint> {
        self.cursor
            .as_ref()
            .and_then(|c| c.v_anchor(self.text, self.area, self.cfg))
    }

    /// Returns `true` if the `anchor` exists before the `caret`
    pub fn anchor_is_start(&self) -> bool {
        self.anchor().is_none_or(|anchor| anchor < self.caret())
    }

    /// Whether or not this is the main [`Cursor`]
    pub fn is_main(&self) -> bool {
        self.is_main
    }

    pub fn text(&self) -> &Text {
        self.text
    }

    /// The [`PrintCfg`] in use
    pub fn cfg(&self) -> PrintCfg {
        self.cfg
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
    /// [`IncSearch`]: crate::mode::IncSearch
    pub fn search_inc_fwd(&mut self, end: Option<Point>) -> impl Iterator<Item = [Point; 2]> + '_ {
        self.inc_searcher.search_fwd(self.text, (self.caret(), end))
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
            .search_rev(self.text, (start, self.caret()))
    }

    /// Whether the [`Cursor`]'s selection matches the [`IncSearch`]
    /// request
    ///
    /// [`IncSearch`]: crate::mode::IncSearch
    pub fn matches_inc(&mut self) -> bool {
        let range = self.cursor.as_ref().unwrap().range();
        self.inc_searcher.matches(self.text.contiguous(range))
    }
}
