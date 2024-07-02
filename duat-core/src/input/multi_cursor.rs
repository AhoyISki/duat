use std::{
    any::TypeId,
    cmp::Ordering,
    ops::{DerefMut, Range},
};

use crate::{
    data::RwData,
    history::Change,
    position::Cursor,
    text::{Point, PrintCfg, Text},
    ui::{Area, Ui},
    widgets::{ActiveWidget, File, PassiveWidget},
};

#[derive(Clone, Debug)]
pub struct Cursors {
    list: Vec<Cursor>,
    main: usize,
}

impl Cursors {
    pub fn new() -> Self {
        Self {
            list: vec![Cursor::default()],
            main: 0,
        }
    }

    pub fn remove_extras(&mut self) {
        let cursor = self.list[self.main].clone();
        self.list = vec![cursor];
        self.main = 0;
    }

    pub fn insert(&mut self, cursor: Cursor) {
        self.list.push(cursor)
    }

    pub fn insert_and_switch(&mut self, _cursor: Cursor) {}

    pub fn main(&self) -> &Cursor {
        &self.list[self.main]
    }

    pub fn nth(&self, index: usize) -> Option<Cursor> {
        self.list.get(index).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Cursor, bool)> {
        self.list
            .iter()
            .enumerate()
            .map(move |(index, cursor)| (cursor, index == self.main))
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn reset(&mut self) {
        self.list = vec![Cursor::default()]
    }

    pub(crate) fn clear(&mut self) {
        self.list.clear()
    }
}

impl Default for Cursors {
    fn default() -> Self {
        Self::new()
    }
}

/// A struct used by [`InputMethod`][crate::input::InputScheme]s to
/// edit [`Text`].
pub struct MultiCursorEditor<'a, W, U>
where
    W: ActiveWidget<U> + 'static,
    U: Ui,
{
    clearing_needed: bool,
    widget: &'a RwData<W>,
    cursors: &'a mut Cursors,
    area: &'a U::Area,
}

impl<'a, W, U> MultiCursorEditor<'a, W, U>
where
    W: ActiveWidget<U> + 'static,
    U: Ui,
{
    pub fn new(widget: &'a RwData<W>, area: &'a U::Area, cursors: &'a mut Cursors) -> Self {
        MultiCursorEditor {
            clearing_needed: false,
            widget,
            cursors,
            area,
        }
    }

    /// Edits on every cursor selection in the list.
    /// Edits on the nth cursor's selection.
    pub fn edit_on_nth<F>(&mut self, mut edit: F, index: usize)
    where
        F: FnMut(&mut Editor<U, W>),
    {
        assert!(index < self.len_cursors(), "Index {index} out of bounds.");
        if self.clearing_needed {
            self.clear_intersections();
            self.clearing_needed = false;
        }

        let mut edit_accum = EditAccum::default();
        let cursor = &mut self.cursors.list[index];

        let mut widget = self.widget.write();

        edit(&mut Editor::<U, W>::new(
            cursor,
            widget.deref_mut(),
            self.area,
            &mut edit_accum,
        ));

        let cfg = widget.print_cfg();

        for cursor in self.cursors.list.iter_mut().skip(index + 1) {
            edit_accum.shift_cursor(cursor, widget.text(), self.area, cfg);
        }

        widget.update(self.area);
    }

    pub fn edit_on_each_cursor(&mut self, mut f: impl FnMut(&mut Editor<U, W>)) {
        self.clear_intersections();
        let mut edit_accum = EditAccum::default();

        let mut widget = self.widget.write();

        for cursor in self.cursors.list.iter_mut() {
            let cfg = widget.print_cfg();
            edit_accum.shift_cursor(cursor, widget.text(), self.area, cfg);
            let mut editor = Editor::new(cursor, widget.deref_mut(), self.area, &mut edit_accum);
            f(&mut editor);
        }

        widget.update(self.area);
    }

    /// Alters the nth cursor's selection.
    pub fn move_nth(&mut self, mut f: impl FnMut(&mut Mover<U::Area>), index: usize) {
        let mut widget = self.widget.write();
        let cursor = &mut self.cursors.list[index];
        let mut mover = Mover::new(cursor, widget.text(), self.area, widget.print_cfg());
        f(&mut mover);

        let cursor = self.cursors.list.remove(index);
        let range = cursor.range();
        let new_index = match self
            .cursors
            .list
            .binary_search_by(|j| at_start_ord(&j.range(), &range))
        {
            Ok(index) => index,
            Err(index) => index,
        };
        self.cursors.list.insert(new_index, cursor);

        if self.cursors.main == index {
            self.cursors.main = new_index;
        }

        widget.update(self.area);
        self.clearing_needed = self.cursors.list.len() > 1;
    }

    /// Alters every selection on the list.
    pub fn move_each_cursor(&mut self, mut mov: impl FnMut(&mut Mover<U::Area>)) {
        let mut widget = self.widget.write();
        for cursor in self.cursors.list.iter_mut() {
            let mut mover = Mover::new(cursor, widget.text(), self.area, widget.print_cfg());
            mov(&mut mover);
        }

        // TODO: Figure out a better way to sort.
        self.cursors
            .list
            .sort_unstable_by(|j, k| at_start_ord(&j.range(), &k.range()));

        widget.update(self.area);
        self.clearing_needed = self.cursors.list.len() > 1;
    }

    /// Edits on the main cursor's selection.
    pub fn edit_on_main<F>(&mut self, edit: F)
    where
        F: FnMut(&mut Editor<U, W>),
    {
        self.edit_on_nth(edit, self.cursors.main);
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
        self.move_nth(mov, self.cursors.main);
    }

    /// Alters the last cursor's selection.
    pub fn move_last(&mut self, f: impl FnMut(&mut Mover<U::Area>)) {
        let len = self.len_cursors();
        if len > 0 {
            self.move_nth(f, len - 1);
        }
    }

    /// The main cursor index.
    pub fn main_cursor_index(&self) -> usize {
        self.cursors.main
    }

    /// Rotates the main cursor index forward.
    pub fn rotate_main_forward(&mut self) {
        let mut widget = self.widget.write();
        if self.cursors.list.len() <= 1 {
            return;
        }

        let main = &mut self.cursors.main;
        *main = if *main == self.cursors.list.len() - 1 {
            0
        } else {
            *main + 1
        };

        widget.update(self.area)
    }

    /// Rotates the main cursor index backwards.
    pub fn rotate_main_backwards(&mut self) {
        let mut widget = self.widget.write();
        if self.cursors.list.len() <= 1 {
            return;
        }

        let main = &mut self.cursors.main;
        *main = if *main == 0 {
            self.cursors.list.len() - 1
        } else {
            *main - 1
        };

        widget.update(self.area)
    }

    /// The amount of active [`Cursor`]s in the [`Text`].
    pub fn len_cursors(&self) -> usize {
        self.cursors.list.len()
    }

    pub fn main_cursor(&self) -> &Cursor {
        self.cursors.main()
    }

    pub fn nth_cursor(&self, index: usize) -> Option<Cursor> {
        self.cursors.list.get(index).cloned()
    }

    /// Removes all intersecting [`Cursor`]s from the list, keeping
    /// only the last from the bunch.
    fn clear_intersections(&mut self) {
        let widget = self.widget.read();
        let text = widget.text();
        let cfg = widget.print_cfg();

        let (mut start, mut end) = self.cursors.list[0].point_range();
        let mut last_index = 0;
        let mut to_remove = Vec::new();

        for (index, cursor) in self.cursors.list.iter_mut().enumerate().skip(1) {
            if cursors_merged(cursor, text, self.area, cfg, start, end) {
                to_remove.push(last_index);
            }
            (start, end) = cursor.point_range();
            last_index = index;
        }

        for index in to_remove.iter().rev() {
            self.cursors.list.remove(*index);
        }
    }
}

impl<'a, U> MultiCursorEditor<'a, File, U>
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
    edit_accum: &'d mut EditAccum,
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
        edit_accum: &'d mut EditAccum,
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
        let end_p = text.point_at(end).unwrap();

        if let Some(anchor) = self.cursor.anchor()
            && anchor >= self.cursor.caret()
        {
            self.cursor.swap_ends();
            self.cursor.move_to(end_p, text, self.area, cfg);
            self.cursor.swap_ends();
        } else {
            self.cursor.move_to(end_p, text, self.area, cfg);
            if end != start {
                let start_p = text.point_at(start).unwrap();
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
        let end = change.added_end();

        self.edit(change);

        let text = self.widget.text();
        let cfg = self.widget.print_cfg();
        let end_p = text.point_at(end).unwrap();

        if let Some(anchor) = self.cursor.anchor()
            && anchor >= self.cursor.caret()
        {
            self.cursor.swap_ends();
            self.cursor.move_to(end_p, text, self.area, cfg);
            self.cursor.swap_ends();
        } else {
            self.cursor.move_to(end_p, text, self.area, cfg);
        }
    }

    /// Edits the file with a cursor.
    fn edit(&mut self, change: Change) {
        self.widget.mut_text().apply_change(&change);
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

    /// Moves the cursor vertically on the file. May also cause
    /// horizontal movement.
    pub fn move_ver(&mut self, count: isize) {
        self.cursor.move_ver(count, self.text, self.area, self.cfg);
    }

    /// Moves the cursor horizontally on the file. May also cause
    /// vertical movement.
    pub fn move_hor(&mut self, count: isize) {
        self.cursor.move_hor(count, self.text, self.area, self.cfg);
    }

    /// Moves the cursor to a position in the file.
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
    pub fn move_to_coords(&mut self, _line: usize, _col: usize) {
        todo!();
        // let point = Point::from_coords(line, col, self.text);
        // self.cursor
        //     .move_to(point, self.text, self.area, self.print_cfg);
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn anchor(&self) -> Option<Point> {
        self.cursor.anchor()
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn caret(&self) -> Point {
        self.cursor.caret()
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

    /// Wether or not the anchor is set.
    pub fn anchor_is_set(&mut self) -> bool {
        self.cursor.anchor().is_some()
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
}

/// An accumulator used specifically for editing with [`Editor<A>`]s.
#[derive(Default)]
pub struct EditAccum {
    pub bytes: isize,
    pub changes: isize,
}

impl EditAccum {
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
}

fn points_cross(lhs: (Point, Point), rhs: (Point, Point)) -> bool {
    (lhs.0 > rhs.0 && rhs.1 > lhs.0) || (rhs.0 > lhs.0 && lhs.1 > rhs.0)
}

/// Compares the `left` and `right` [`Range`]s, returning an
/// [`Ordering`], based on the intersection at the start.
fn at_start_ord(left: &Range<usize>, right: &Range<usize>) -> Ordering {
    if left.end > right.start && right.start > left.start {
        std::cmp::Ordering::Equal
    } else if left.start > right.end {
        std::cmp::Ordering::Greater
    } else {
        std::cmp::Ordering::Less
    }
}

fn cursors_merged(
    cursor: &mut Cursor,
    text: &Text,
    area: &impl Area,
    cfg: &PrintCfg,
    start: Point,
    end: Point,
) -> bool {
    if !points_cross(cursor.point_range(), (start, end)) {
        return false;
    }

    let caret = cursor.caret();
    let Some(anchor) = cursor.anchor() else {
        cursor.move_to(start, text, area, cfg);
        cursor.set_anchor();
        cursor.move_to(caret.max(end), text, area, cfg);
        return true;
    };

    if anchor > caret {
        cursor.move_to(anchor.max(end), text, area, cfg);
        cursor.swap_ends();
        cursor.move_to(caret.min(start), text, area, cfg);
    } else {
        cursor.move_to(anchor.min(end), text, area, cfg);
        cursor.swap_ends();
        cursor.move_to(caret.max(start), text, area, cfg);
    }

    true
}
