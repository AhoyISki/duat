use std::{any::TypeId, ops::Range};

pub use self::cursors::{Cursor, Cursors};
use crate::{
    data::RwData,
    history::Change,
    text::{Pattern, Point, PrintCfg, Text, WordChars},
    ui::{Area, Ui},
    widgets::{ActiveWidget, File, PassiveWidget},
};

mod cursors;

/// A struct used by [`InputMethod`][crate::input::InputScheme]s to
/// edit [`Text`].
pub struct EditHelper<'a, W, U>
where
    W: ActiveWidget<U> + 'static,
    U: Ui,
{
    widget: &'a RwData<W>,
    cursors: &'a mut Cursors,
    area: &'a U::Area,
}

impl<'a, W, U> EditHelper<'a, W, U>
where
    W: ActiveWidget<U> + 'static,
    U: Ui,
{
    pub fn new(widget: &'a RwData<W>, area: &'a U::Area, cursors: &'a mut Cursors) -> Self {
        EditHelper {
            widget,
            cursors,
            area,
        }
    }

    /// Edits on every cursor selection in the list.
    /// Edits on the nth cursor's selection.
    pub fn edit_on_nth<F>(&mut self, mut edit: F, n: usize)
    where
        F: FnMut(&mut Editor<U, W>),
    {
        let Some(mut cursor) = self.cursors.get(n) else {
            panic!("Cursor index {n} out of bounds.");
        };

        let mut widget = self.widget.write();
        let mut diff = Diff::default();

        edit(&mut Editor::<U, W>::new(
            &mut cursor,
            &mut widget,
            self.area,
            &mut diff,
        ));

        let cfg = widget.print_cfg();

        self.cursors.replace(n, cursor);
        self.cursors.shift(n, diff, widget.text(), self.area, cfg);

        widget.update(self.area);
    }

    pub fn edit_on_each_cursor(&mut self, mut f: impl FnMut(&mut Editor<U, W>)) {
        let removed_cursors: Vec<Cursor> = self.cursors.drain().collect();

        let mut widget = self.widget.write();
        let mut diff = Diff::default();

        for (i, mut cursor) in removed_cursors.into_iter().enumerate() {
            let cfg = widget.print_cfg();
            diff.shift_cursor(&mut cursor, widget.text(), self.area, cfg);

            f(&mut Editor::new(
                &mut cursor,
                &mut widget,
                self.area,
                &mut diff,
            ));

            self.cursors.insert_removed(i, cursor);
        }

        widget.update(self.area);
    }

    /// Alters the nth cursor's selection.
    pub fn move_nth(&mut self, mut mov: impl FnMut(&mut Mover<U::Area>), n: usize) {
        let Some(mut cursor) = self.cursors.get(n) else {
            panic!("Cursor index {n} out of bounds.");
        };
        let mut widget = self.widget.write();

        mov(&mut Mover::new(
            &mut cursor,
            widget.text(),
            self.area,
            widget.print_cfg(),
        ));

        self.cursors.replace(n, cursor);
        widget.update(self.area);
    }

    /// Alters every selection on the list.
    pub fn move_each<_T>(&mut self, mut mov: impl FnMut(&mut Mover<U::Area>) -> _T) {
        let removed_cursors: Vec<Cursor> = self.cursors.drain().collect();

        let mut widget = self.widget.write();

        for (i, mut cursor) in removed_cursors.into_iter().enumerate() {
            mov(&mut Mover::new(
                &mut cursor,
                widget.text(),
                self.area,
                widget.print_cfg(),
            ));

            self.cursors.insert_removed(i, cursor);
        }

        widget.update(self.area);
    }

    /// Edits on the main cursor's selection.
    pub fn edit_on_main<F>(&mut self, edit: F)
    where
        F: FnMut(&mut Editor<U, W>),
    {
        self.edit_on_nth(edit, self.cursors.main_index());
    }

    /// Edits on the last cursor's selection.
    pub fn edit_on_last<F>(&mut self, edit: F)
    where
        F: FnMut(&mut Editor<U, W>),
    {
        let len = self.len_cursors();
        if len > 0 {
            self.edit_on_nth(edit, len - 1);
        }
    }

    /// Alters the main cursor's selection.
    pub fn move_main(&mut self, mov: impl FnMut(&mut Mover<U::Area>)) {
        self.move_nth(mov, self.cursors.main_index());
    }

    /// Alters the last cursor's selection.
    pub fn move_last(&mut self, mov: impl FnMut(&mut Mover<U::Area>)) {
        let len = self.len_cursors();
        if len > 0 {
            self.move_nth(mov, len - 1);
        }
    }

    pub fn remove_extra_cursors(&mut self) {
        self.cursors.remove_extras();
    }

    /// Rotates the main cursor index forward.
    pub fn rotate_main_fwd(&mut self) {
        self.cursors.rotate_main_fwd()
    }

    /// Rotates the main cursor index backwards.
    pub fn rotate_main_rev(&mut self) {
        self.cursors.rotate_main_rev()
    }

    pub fn main_cursor(&self) -> &Cursor {
        self.cursors.main()
    }

    pub fn get_cursor(&self, n: usize) -> Option<Cursor> {
        self.cursors.get(n)
    }

    /// The main cursor index.
    pub fn main_cursor_index(&self) -> usize {
        self.cursors.main_index()
    }

    /// The amount of active [`Cursor`]s in the [`Text`].
    pub fn len_cursors(&self) -> usize {
        self.cursors.len()
    }
}

impl<'a, U> EditHelper<'a, File, U>
where
    U: Ui,
{
    /// Begins a new [`Moment`][crate::history::Moment].
    pub fn new_moment(&mut self) {
        self.widget.write().add_moment();
    }

    /// Undoes the last [`Moment`][crate::history::Moment].
    pub fn undo(&mut self) {
        let mut widget = self.widget.write();
        widget.undo(self.area, self.cursors);
        <File as PassiveWidget<U>>::update(&mut widget, self.area);
    }

    /// Redoes the last [`Moment`][crate::history::Moment].
    pub fn redo(&mut self) {
        let mut widget = self.widget.write();
        widget.redo(self.area, self.cursors);
        <File as PassiveWidget<U>>::update(&mut widget, self.area);
    }
}

/// A cursor that can edit text in its selection, but can't move the
/// selection in any way.
pub struct Editor<'a, 'b, 'c, 'd, U, W>
where
    U: Ui,
    W: ActiveWidget<U>,
{
    cursor: &'a mut Cursor,
    widget: &'b mut W,
    area: &'c U::Area,
    edit_accum: &'d mut Diff,
}

impl<'a, 'b, 'c, 'd, U, W> Editor<'a, 'b, 'c, 'd, U, W>
where
    U: Ui,
    W: ActiveWidget<U>,
{
    /// Returns a new instance of `Editor`.
    fn new(
        cursor: &'a mut Cursor,
        widget: &'b mut W,
        area: &'c U::Area,
        edit_accum: &'d mut Diff,
    ) -> Self {
        Self {
            cursor,
            widget,
            area,
            edit_accum,
        }
    }

    /// Replaces the entire selection of the [`Cursor`] with new
    /// text.
    pub fn replace(&mut self, edit: impl ToString) {
        let change = Change::new(edit.to_string(), self.cursor.range(), self.widget.text());
        let (start, end) = (change.start, change.added_end());

        self.edit(change);

        let text = self.widget.text();
        let cfg = self.widget.print_cfg();
        let end_p = text.point_at(end);

        if let Some(anchor) = self.cursor.anchor()
            && anchor >= self.cursor.caret()
        {
            self.cursor.swap_ends();
            self.cursor.move_to(end_p, text, self.area, cfg);
            self.cursor.swap_ends();
        } else {
            self.cursor.move_to(end_p, text, self.area, cfg);
            if end != start {
                let start_p = text.point_at(start);
                self.cursor.set_anchor();
                self.cursor.move_to(start_p, text, self.area, cfg);
                self.cursor.swap_ends();
            }
        }
    }

    /// Inserts new text directly behind the caret.
    pub fn insert(&mut self, edit: impl ToString) {
        let range = self.cursor.byte()..self.cursor.byte();
        let change = Change::new(edit.to_string(), range, self.widget.text());
        let diff = change.chars_diff();

        self.edit(change);

        if let Some(anchor) = self.cursor.anchor()
            && anchor >= self.cursor.caret()
        {
            let text = self.widget.text();
            let cfg = self.widget.print_cfg();
            self.cursor.swap_ends();
            self.cursor.move_hor(diff, text, self.area, cfg);
            self.cursor.swap_ends();
        }
    }

    /// Edits the file with a cursor.
    fn edit(&mut self, change: Change) {
        self.widget.text_mut().apply_change(&change);
        self.edit_accum.bytes += change.added_end() as isize - change.taken_end() as isize;

        if TypeId::of::<W>() == TypeId::of::<File>() {
            let file = unsafe { std::mem::transmute_copy::<&mut W, &mut File>(&self.widget) };

            let (insertion_index, change_diff) = file
                .history_mut()
                .add_change(change, self.cursor.assoc_index);
            self.cursor.assoc_index = Some(insertion_index);
            self.edit_accum.changes += change_diff;
        }
    }
}

/// A cursor that can move and alter the selection, but can't edit the
/// file.
pub struct Mover<'a, A>
where
    A: Area,
{
    cursor: &'a mut Cursor,
    text: &'a Text,
    area: &'a A,
    cfg: &'a PrintCfg,
}

impl<'a, A> Mover<'a, A>
where
    A: Area,
{
    /// Returns a new instance of `Mover`.
    pub fn new(cursor: &'a mut Cursor, text: &'a Text, area: &'a A, cfg: &'a PrintCfg) -> Self {
        Self {
            cursor,
            text,
            area,
            cfg,
        }
    }

    ////////// Public movement functions

    /// Moves the cursor horizontally on the file. May also cause
    /// vertical movement.
    pub fn move_hor(&mut self, count: isize) {
        self.cursor.move_hor(count, self.text, self.area, self.cfg);
    }

    /// Moves the cursor vertically on the file. May also cause
    /// horizontal movement.
    pub fn move_ver(&mut self, count: isize) {
        self.cursor.move_ver(count, self.text, self.area, self.cfg);
    }

    /// Moves the cursor vertically on the file. May also cause
    /// horizontal movement.
    pub fn move_ver_wrapped(&mut self, count: isize) {
        self.cursor
            .move_ver_wrapped(count, self.text, self.area, self.cfg);
    }

    /// Moves the cursor to a [`Point`] in the file.
    ///
    /// - If the position isn't valid, it will move to the "maximum"
    ///   position allowed.
    /// - This command sets `desired_x`.
    pub fn move_to(&mut self, point: Point) {
        self.cursor.move_to(point, self.text, self.area, self.cfg);
    }

    /// Moves the cursor to a line and a column on the file.
    ///
    /// - If the coords isn't valid, it will move to the "maximum"
    ///   position allowed.
    /// - This command sets `desired_x`.
    pub fn move_to_coords(&mut self, line: usize, col: usize) {
        let point = self.text.point_at_line(line.min(self.text.len_lines()));
        let (point, _) = self.text.iter_chars_at(point).take(col + 1).last().unwrap();
        self.move_to(point);
    }

    /// Returns and takes the anchor of the `TextCursor`.
    pub fn unset_anchor(&mut self) -> Option<Point> {
        self.cursor.unset_anchor()
    }

    /// Sets the position of the anchor to be the same as the current
    /// cursor position in the file.
    ///
    /// The `anchor` and `current` act as a range of text on the file.
    pub fn set_anchor(&mut self) {
        self.cursor.set_anchor()
    }

    /// Switches the caret and anchor of the `TextCursor`.
    pub fn swap_ends(&mut self) {
        self.cursor.swap_ends();
    }

    /// Places the caret at the beginning of the selection.
    pub fn set_caret_on_start(&mut self) {
        if let Some(anchor) = self.cursor.anchor()
            && self.cursor.caret() > anchor
        {
            self.cursor.swap_ends();
        }
    }

    /// Places the caret at the beginning of the selection.
    pub fn set_caret_on_end(&mut self) {
        if let Some(anchor) = self.cursor.anchor()
            && anchor > self.cursor.caret()
        {
            self.cursor.swap_ends();
        }
    }

    ////////// Lookup functions
    pub fn search<P>(&self, pat: P) -> impl Iterator<Item = ((Point, Point), P::Match)> + 'a
    where
        P: Pattern<'a> + 'a,
    {
        self.text.search_from(self.cursor.caret(), pat)
    }

    pub fn search_rev<P>(&self, pat: P) -> impl Iterator<Item = ((Point, Point), P::Match)> + 'a
    where
        P: Pattern<'a> + 'a,
    {
        self.text.search_from_rev(self.cursor.caret(), pat)
    }

    pub fn find_ends(&self, pat: impl Pattern<'a> + 'a) -> Option<(Point, Point)> {
        self.text
            .search_from(self.cursor.caret(), pat)
            .next()
            .unzip()
            .0
    }

    pub fn find_ends_rev(&self, pat: impl Pattern<'a> + 'a) -> Option<(Point, Point)> {
        self.text
            .search_from_rev(self.cursor.caret(), pat)
            .next()
            .unzip()
            .0
    }

    pub fn char(&self) -> char {
        self.text.char_at(self.cursor.caret()).unwrap()
    }

    pub fn len_point(&self) -> Point {
        self.text.len_point()
    }

    pub fn last_point(&self) -> Option<Point> {
        self.text.last_point()
    }

    pub fn cfg(&self) -> &PrintCfg {
        self.cfg
    }

    pub fn w_chars(&self) -> &WordChars {
        &self.cfg.word_chars
    }

    pub fn iter(&self) -> impl Iterator<Item = (Point, char)> + '_ {
        self.text.iter_chars_at(self.caret())
    }

    pub fn iter_rev(&self) -> impl Iterator<Item = (Point, char)> + '_ {
        self.text.iter_chars_at_rev(self.caret())
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn anchor(&self) -> Option<Point> {
        self.cursor.anchor()
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn caret(&self) -> Point {
        self.cursor.caret()
    }

    pub fn anchor_is_start(&self) -> bool {
        self.anchor().is_none_or(|anchor| anchor < self.caret())
    }

    pub fn selection(&self) -> [&str; 2] {
        let anchor = self.anchor().unwrap_or(self.caret());
        let range = if anchor < self.caret() {
            (anchor, self.caret())
        } else {
            (self.caret(), anchor)
        };
        self.text.strs_in_point_range(range)
    }

    pub fn byte_range(&self) -> Range<usize> {
        let anchor = self.anchor().unwrap_or(self.caret());
        if anchor < self.caret() {
            anchor.byte()..self.caret().byte()
        } else {
            self.caret().byte()..anchor.byte()
        }
    }
}

/// An accumulator used specifically for editing with [`Editor<A>`]s.
#[derive(Default)]
pub struct Diff {
    pub bytes: isize,
    pub changes: isize,
}

impl Diff {
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

    fn no_change(&self) -> bool {
        self.bytes == 0 && self.changes == 0
    }
}
