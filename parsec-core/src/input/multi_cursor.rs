use std::{cmp::Ordering, ops::Range};

use crate::{
    data::{RwData},
    history::{Change, History},
    position::{Cursor, Point},
    text::{PrintCfg, Text},
    ui::Ui,
    widgets::ActiveWidget,
};

#[derive(Clone)]
pub struct Cursors {
    list: Vec<Cursor>,
    main: Option<usize>,
}

impl Cursors {
    pub fn new() -> Self {
        Self {
            list: Vec::new(),
            main: None,
        }
    }

    pub fn clear(&mut self) {
        self.list.clear();
        self.main = None;
    }

    pub fn insert(&mut self, cursor: Cursor) {
        self.list.push(cursor)
    }

    pub fn insert_and_switch(&mut self, cursor: Cursor) {}

    pub fn main(&self) -> Option<Cursor> {
        self.main.and_then(|main| self.list.get(main).cloned())
    }

    pub fn nth(&self, index: usize) -> Option<Cursor> {
        self.list.get(index).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Cursor, bool)> {
        // There will never be this many cursors man.
        let main = self.main.unwrap_or(usize::MAX);

        self.list
            .iter()
            .enumerate()
            .map(move |(index, cursor)| (cursor, index == main))
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Default for Cursors {
    fn default() -> Self {
        Self::new()
    }
}

/// A struct used by [`InputMethod`][crate::input::InputScheme]s to
/// edit [`Text`].
pub struct MultiCursorEditor<'a, H, U, W>
where
    U: Ui + 'static,
    W: ActiveWidget + 'static,
{
    clearing_needed: bool,
    widget: &'a RwData<W>,
    cursors: &'a mut Cursors,
    area: &'a U::Area,
    history: Option<&'a mut History>,
    _h: std::marker::PhantomData<H>,
}

impl<'a, U, W> MultiCursorEditor<'a, WithHistory, U, W>
where
    U: Ui,
    W: ActiveWidget + 'static,
{
    pub fn with_history(
        widget: &'a RwData<W>,
        cursors: &'a mut Cursors,
        area: &'a U::Area,
        history: &'a mut History,
    ) -> Self {
        MultiCursorEditor {
            clearing_needed: false,
            widget,
            cursors,
            area,
            history: Some(history),
            _h: std::marker::PhantomData,
        }
    }

    /// Begins a new [`Moment`][crate::history::Moment].
    pub fn new_moment(&mut self) {
        self.history.as_mut().unwrap().new_moment()
    }

    /// Undoes the last [`Moment`][crate::history::Moment].
    pub fn undo(&mut self) {
        let mut widget = self.widget.write();
        let cfg = widget.print_cfg().clone();

        self.history
            .as_mut()
            .unwrap()
            .undo(widget.mut_text(), self.area, self.cursors, cfg);

        widget.update(self.area);
    }

    /// Redoes the last [`Moment`][crate::history::Moment].
    pub fn redo(&mut self) {
        let mut widget = self.widget.write();
        let cfg = widget.print_cfg().clone();
        self.history
            .as_mut()
            .unwrap()
            .redo(widget.mut_text(), self.area, self.cursors, cfg);
        widget.update(self.area);
    }
}

impl<'a, U, W> MultiCursorEditor<'a, NoHistory, U, W>
where
    U: Ui,
    W: ActiveWidget + 'static,
{
    /// Returns a new instace of [`WidgetActor<U, AW>`].
    pub fn no_history(
        widget: &'a RwData<W>,
        cursors: &'a mut Cursors,
        area: &'a U::Area,
    ) -> MultiCursorEditor<'a, NoHistory, U, W> {
        MultiCursorEditor {
            clearing_needed: false,
            widget,
            cursors,
            area,
            history: None,
            _h: std::marker::PhantomData,
        }
    }
}

impl<'a, H, U, W> MultiCursorEditor<'a, H, U, W>
where
    U: Ui,
    W: ActiveWidget + 'static,
{
    /// Removes all intersecting [`Cursor`]s from the list, keeping
    /// only the last from the bunch.
    fn clear_intersections(&mut self) {
        let (mut start, mut end) = self.cursors.list[0].pos_range();
        let mut last_index = 0;
        let mut to_remove = Vec::new();

        for (index, cursor) in self.cursors.list.iter_mut().enumerate().skip(1) {
            if try_merge_selections(cursor, start, end) {
                to_remove.push(last_index);
            }
            (start, end) = cursor.pos_range();
            last_index = index;
        }

        for index in to_remove.iter().rev() {
            self.cursors.list.remove(*index);
        }
    }

    /// Edits on every cursor selection in the list.
    pub fn edit_on_each_cursor(&mut self, mut f: impl FnMut(&mut Editor)) {
        self.clear_intersections();
        let mut widget = self.widget.write();
        let mut edit_accum = EditAccum::default();

        for cursor in self.cursors.list.iter_mut() {
            let history = self.history.take();
            let mut editor = Editor::new(cursor, widget.mut_text(), &mut edit_accum, history);
            f(&mut editor);
            self.history = editor.history.take();
        }

        widget.update(self.area);
    }

    /// Alters every selection on the list.
    pub fn move_each_cursor<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Mover<U>),
    {
        let mut widget = self.widget.write();
        for cursor in self.cursors.list.iter_mut() {
            let mut mover = Mover::new(cursor, widget.text(), self.area, widget.print_cfg());
            f(&mut mover);
        }

        // TODO: Figure out a better way to sort.
        self.cursors
            .list
            .sort_unstable_by(|j, k| at_start_ord(&j.range(), &k.range()));

        widget.update(self.area);
        self.clearing_needed = self.cursors.list.len() > 1;
    }

    /// Alters the nth cursor's selection.
    pub fn move_nth<F>(&mut self, mut f: F, index: usize)
    where
        F: FnMut(&mut Mover<U>),
    {
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

        if self.cursors.main.is_some_and(|main| main == index) {
            self.cursors.main = Some(new_index)
        }

        widget.update(self.area);
        self.clearing_needed = self.cursors.list.len() > 1;
    }

    /// Alters the main cursor's selection.
    pub fn move_main<F>(&mut self, f: F)
    where
        F: FnMut(&mut Mover<U>),
    {
        if let Some(main) = self.cursors.main {
            self.move_nth(f, main);
        }
    }

    /// Alters the last cursor's selection.
    pub fn move_last<F>(&mut self, f: F)
    where
        F: FnMut(&mut Mover<U>),
    {
        let len = self.len_cursors();
        if len > 0 {
            self.move_nth(f, len - 1);
        }
    }

    /// Edits on the nth cursor's selection.
    pub fn edit_on_nth<F>(&mut self, mut f: F, index: usize)
    where
        F: FnMut(&mut Editor),
    {
        assert!(index < self.len_cursors(), "Index {index} out of bounds.");
        if self.clearing_needed {
            self.clear_intersections();
            self.clearing_needed = false;
        }

        let mut widget = self.widget.write();

        let mut edit_accum = EditAccum::default();
        let cursor = &mut self.cursors.list[index];
        let mut editor = Editor::new(
            cursor,
            widget.mut_text(),
            &mut edit_accum,
            self.history.take(),
        );
        f(&mut editor);
        self.history = editor.history;

        for cursor in self.cursors.list.iter_mut().skip(index + 1) {
            cursor
                .assoc_index
                .as_mut()
                .map(|i| i.saturating_add_signed(edit_accum.changes));
            cursor.caret.calibrate(edit_accum.chars, widget.text());
            if let Some(anchor) = cursor.anchor.as_mut() {
                anchor.calibrate(edit_accum.chars, widget.text())
            }
        }

        widget.update(self.area);
    }

    /// Edits on the main cursor's selection.
    pub fn edit_on_main<F>(&mut self, f: F)
    where
        F: FnMut(&mut Editor),
    {
        if let Some(main) = self.cursors.main {
            self.edit_on_nth(f, main);
        }
    }

    /// Edits on the last cursor's selection.
    pub fn edit_on_last<F>(&mut self, f: F)
    where
        F: FnMut(&mut Editor),
    {
        let len = self.len_cursors();
        if len > 0 {
            self.edit_on_nth(f, len - 1);
        }
    }

    /// The main cursor index.
    pub fn main_cursor_index(&self) -> Option<usize> {
        self.cursors.main
    }

    /// Rotates the main cursor index forward.
    pub fn rotate_main_forward(&mut self) {
        let mut widget = self.widget.write();
        if self.cursors.list.len() <= 1 {
            return;
        }

        if let Some(main) = self.cursors.main.as_mut() {
            *main = if *main == self.cursors.list.len() - 1 {
                0
            } else {
                *main + 1
            }
        }

        widget.update(self.area)
    }

    /// Rotates the main cursor index backwards.
    pub fn rotate_main_backwards(&mut self) {
        let mut widget = self.widget.write();
        if self.cursors.list.len() <= 1 {
            return;
        }

        if let Some(main_index) = self.cursors.main.as_mut() {
            *main_index = if *main_index == 0 {
                self.cursors.list.len() - 1
            } else {
                *main_index - 1
            }
        }

        widget.update(self.area)
    }

    /// The amount of active [`Cursor`]s in the [`Text`].
    pub fn len_cursors(&self) -> usize {
        self.cursors.list.len()
    }

    pub fn main_cursor(&self) -> Option<Cursor> {
        self.cursors
            .main
            .and_then(|main| self.cursors.list.get(main).cloned())
    }

    pub fn nth_cursor(&self, index: usize) -> Option<Cursor> {
        self.cursors.list.get(index).cloned()
    }
}

impl<'a, U, W> MultiCursorEditor<'a, WithHistory, U, W>
where
    U: Ui,
    W: ActiveWidget,
{
}

/// An accumulator used specifically for editing with [`Editor<U>`]s.
#[derive(Default)]
pub struct EditAccum {
    pub chars: isize,
    pub changes: isize,
}

/// A cursor that can edit text in its selection, but can't move the
/// selection in any way.
pub struct Editor<'a, 'b, 'c, 'd> {
    cursor: &'a mut Cursor,
    text: &'b mut Text,
    edit_accum: &'c mut EditAccum,
    history: Option<&'d mut History>,
}

impl<'a, 'b, 'c, 'd> Editor<'a, 'b, 'c, 'd> {
    /// Returns a new instance of `Editor`.
    fn new(
        cursor: &'a mut Cursor,
        text: &'b mut Text,
        edit_accum: &'c mut EditAccum,
        history: Option<&'d mut History>,
    ) -> Self {
        cursor
            .assoc_index
            .as_mut()
            .map(|i| i.saturating_add_signed(edit_accum.changes));
        cursor.caret.calibrate(edit_accum.chars, text);
        if let Some(anchor) = cursor.anchor.as_mut() {
            anchor.calibrate(edit_accum.chars, text)
        }

        Self {
            cursor,
            text,
            edit_accum,
            history,
        }
    }

    /// Replaces the entire selection of the `TextCursor` with new
    /// text.
    pub fn replace(&mut self, edit: impl ToString) {
        let change = Change::new(edit.to_string(), self.cursor.range(), self.text);
        let (start, end) = (change.start, change.added_end());

        self.edit(change);

        if let Some(anchor) = &mut self.cursor.anchor {
            if anchor.true_char() > self.cursor.caret.true_char() {
                *anchor = Point::new(end, self.text);
                return;
            }
        }

        self.cursor.caret = Point::new(end, self.text);
        self.cursor.anchor = Some(Point::new(start, self.text));
    }

    /// Inserts new text directly behind the caret.
    pub fn insert(&mut self, edit: impl ToString) {
        let range = self.cursor.caret.true_char()..self.cursor.caret.true_char();
        let change = Change::new(edit.to_string(), range, self.text);
        let (added_end, taken_end) = (change.added_end(), change.taken_end());

        self.edit(change);

        let ch_diff = added_end as isize - taken_end as isize;

        if let Some(anchor) = &mut self.cursor.anchor {
            if *anchor > self.cursor.caret {
                anchor.calibrate(ch_diff, self.text);
            }
        }
    }

    /// Edits the file with a cursor.
    fn edit(&mut self, change: Change) {
        self.text.apply_change(&change);
        self.edit_accum.chars += change.added_end() as isize - change.taken_end() as isize;

        if let Some(history) = &mut self.history {
            let (insertion_index, change_diff) =
                history.add_change(change, self.cursor.assoc_index);
            self.cursor.assoc_index = Some(insertion_index);
            self.edit_accum.changes += change_diff;
        }
    }
}

/// A cursor that can move and alter the selection, but can't edit the
/// file.
pub struct Mover<'a, U>
where
    U: Ui,
{
    cursor: &'a mut Cursor,
    text: &'a Text,
    area: &'a U::Area,
    print_cfg: &'a PrintCfg,
}

impl<'a, U> Mover<'a, U>
where
    U: Ui,
{
    /// Returns a new instance of `Mover`.
    pub fn new(
        cursor: &'a mut Cursor,
        text: &'a Text,
        area: &'a U::Area,
        print_cfg: &'a PrintCfg,
    ) -> Self {
        Self {
            cursor,
            text,
            area,
            print_cfg,
        }
    }

    ////////// Public movement functions

    /// Moves the cursor vertically on the file. May also cause
    /// horizontal movement.
    pub fn move_ver(&mut self, count: isize) {
        self.cursor
            .move_ver(count, self.text, self.area, &self.print_cfg);
    }

    /// Moves the cursor horizontally on the file. May also cause
    /// vertical movement.
    pub fn move_hor(&mut self, count: isize) {
        self.cursor
            .move_hor(count, self.text, self.area, &self.print_cfg);
    }

    /// Moves the cursor to a position in the file.
    ///
    /// - If the position isn't valid, it will move to the "maximum" position
    ///   allowed.
    /// - This command sets `desired_x`.
    pub fn move_to(&mut self, point: Point) {
        self.cursor
            .move_to(point, self.text, self.area, &self.print_cfg);
    }

    /// Moves the cursor to a line and a column on the file.
    ///
    /// - If the coords isn't valid, it will move to the "maximum" position
    ///   allowed.
    /// - This command sets `desired_x`.
    pub fn move_to_coords(&mut self, line: usize, col: usize) {
        let point = Point::from_coords(line, col, self.text);
        self.cursor
            .move_to(point, self.text, self.area, &self.print_cfg);
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn anchor(&self) -> Option<Point> {
        self.cursor.anchor
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn caret(&self) -> Point {
        self.cursor.caret
    }

    /// Returns and takes the anchor of the `TextCursor`.
    pub fn take_anchor(&mut self) -> Option<Point> {
        self.cursor.anchor.take()
    }

    /// Sets the position of the anchor to be the same as the current
    /// cursor position in the file.
    ///
    /// The `anchor` and `current` act as a range of text on the file.
    pub fn set_anchor(&mut self) {
        self.cursor.set_anchor()
    }

    /// Unsets the anchor.
    ///
    /// This is done so the cursor no longer has a valid selection.
    pub fn unset_anchor(&mut self) {
        self.cursor.unset_anchor()
    }

    /// Wether or not the anchor is set.
    pub fn anchor_is_set(&mut self) -> bool {
        self.cursor.anchor.is_some()
    }

    /// Switches the caret and anchor of the `TextCursor`.
    pub fn switch_ends(&mut self) {
        if let Some(anchor) = &mut self.cursor.anchor {
            std::mem::swap(anchor, &mut self.cursor.caret);
        }
    }

    /// Places the caret at the beginning of the selection.
    pub fn set_caret_on_start(&mut self) {
        if let Some(anchor) = &mut self.cursor.anchor {
            if *anchor < self.cursor.caret {
                std::mem::swap(anchor, &mut self.cursor.caret);
            }
        }
    }

    /// Places the caret at the beginning of the selection.
    pub fn set_caret_on_end(&mut self) {
        if let Some(anchor) = &mut self.cursor.anchor {
            if self.cursor.caret < *anchor {
                std::mem::swap(anchor, &mut self.cursor.caret);
            }
        }
    }
}

fn pos_intersects(left: (Point, Point), right: (Point, Point)) -> bool {
    (left.0 > right.0 && right.1 > left.0) || (right.0 > left.0 && left.1 > right.0)
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

fn try_merge_selections(cursor: &mut Cursor, start: Point, end: Point) -> bool {
    if !pos_intersects(cursor.pos_range(), (start, end)) {
        return false;
    }
    let Some(anchor) = cursor.anchor.as_mut() else {
        cursor.anchor = Some(start);
        cursor.caret = cursor.caret.max(end);
        return true;
    };

    if *anchor > cursor.caret {
        *anchor = (*anchor).max(end);
        cursor.caret = cursor.caret.min(start);
    } else {
        *anchor = (*anchor).min(start);
        cursor.caret = cursor.caret.max(end);
    }

    true
}

pub struct NoHistory;
pub struct WithHistory;
