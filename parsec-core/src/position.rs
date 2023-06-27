use std::ops::Range;

use crate::{
    history::{Change, History},
    text::{inner::InnerText, PrintCfg, Text},
    ui::{Area, Ui},
    widgets::EditAccum
};

// NOTE: `col` and `line` are line based, while `byte` is file based.
/// A position in a `Vec<String>` (line and character address).
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    byte: usize,
    char: usize,
    col: usize,
    line: usize
}

impl Pos {
    pub fn calibrate(&mut self, ch_diff: isize, inner: &InnerText) {
        self.char = self.char.saturating_add_signed(ch_diff);
        self.byte = inner.char_to_byte(self.char).unwrap();
        self.line = inner.char_to_line(self.char).unwrap();
        self.col = inner.char_from_line_start(self.char).unwrap();
    }

    pub fn new(ch_index: usize, inner: &InnerText) -> Pos {
        Pos {
            byte: inner.char_to_byte(ch_index).unwrap(),
            char: ch_index,
            col: inner.char_from_line_start(ch_index).unwrap(),
            line: inner.char_to_line(ch_index).unwrap()
        }
    }

    /// Returns the byte (relative to the beginning of the file),
    /// indexed at 1. Intended only for displaying by the end
    /// user. For a 0 indexed byte, see [true_byte()][Pos::true_byte].
    pub fn byte(&self) -> usize {
        self.byte + 1
    }

    /// Returns the char index (relative to the beginning of the
    /// file). Indexed at 1. Intended only for displaying by the
    /// end user. For a 0 indexed char index, see
    /// [true_char()](Self::true_char()).
    pub fn char(&self) -> usize {
        self.char + 1
    }

    /// Returns the column, indexed at 1. Intended only for displaying
    /// by the end user. For a 0 indexed column, see
    /// [true_col()](Self::true_col()).
    pub fn col(&self) -> usize {
        self.col + 1
    }

    /// Returns the line of self. Indexed at 1. Intended only for
    /// displaying by the end user. For a 0 indexed line, see
    /// [true_row()](Self::true_row()).
    pub fn row(&self) -> usize {
        self.line + 1
    }

    /// Returns the byte (relative to the beginning of the file) of
    /// self. Indexed at 0.
    pub fn true_byte(&self) -> usize {
        self.byte
    }

    /// Returns the char index (relative to the beginning of the
    /// file). Indexed at 0.
    pub fn true_char(&self) -> usize {
        self.char
    }

    /// Returns the column. Indexed at 0.
    pub fn true_col(&self) -> usize {
        self.col
    }

    /// Returns the line. Indexed at 0.
    pub fn true_row(&self) -> usize {
        self.line
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.col + 1, self.line + 1))
    }
}

/// A cursor in the text file. This is an editing cursor, not a
/// printing cursor.
#[derive(Default, Copy)]
pub struct Cursor {
    /// Current position of the cursor in the file.
    caret: Pos,

    /// An anchor for a selection.
    anchor: Option<Pos>,

    /// The index to a `Change` in the current `Moment`, used for
    /// greater efficiency.
    pub(crate) assoc_index: Option<usize>,

    /// Column that the cursor wants to be in.
    ///
    /// If the cursor moves to a line that is at least as wide as the
    /// desired_col, it will be placed in the desired_col. If the
    /// line is shorter, it will be placed in the last column of
    /// the line.
    desired_x: usize
}

impl Cursor {
    /// Returns a new instance of `FileCursor`.
    pub fn new<U>(pos: Pos, text: &Text<U>, area: &U::Area, cfg: &PrintCfg) -> Cursor
    where
        U: Ui
    {
        let line = text.iter_line(pos.line);
        Cursor {
            caret: pos,
            // This should be fine.
            anchor: None,
            assoc_index: None,
            desired_x: area.get_width(line.take(pos.true_col()), cfg, usize::MAX, true)
        }
    }

    /// Internal vertical movement function.
    pub(crate) fn move_ver<U>(
        &mut self, count: isize, text: &Text<U>, area: &U::Area, cfg: &PrintCfg
    ) where
        U: Ui
    {
        let caret = &mut self.caret;
        let max = text.len_lines().saturating_sub(1);
        caret.line = caret.line.saturating_add_signed(count).min(max);

        let line = text.iter_line(caret.line);

        // In vertical movement, the `desired_x` dictates in what column the
        // cursor will be placed.
        caret.col = area.col_at_dist(line, self.desired_x, cfg);

        caret.char = text.line_to_char(caret.line) + caret.col;
        caret.byte = text.char_to_byte(caret.char);
    }

    /// Internal horizontal movement function.
    pub(crate) fn move_hor<U>(
        &mut self, count: isize, text: &Text<U>, area: &U::Area, cfg: &PrintCfg
    ) where
        U: Ui
    {
        let caret = &mut self.caret;
        let max = text.len_chars().saturating_sub(1);
        caret.char = caret.char.saturating_add_signed(count).min(max);
        caret.byte = text.char_to_byte(caret.char);
        caret.line = text.char_to_line(caret.char);
        let line_char = text.line_to_char(caret.line);
        caret.col = caret.char - line_char;

        self.desired_x =
            area.get_width(text.iter_range(line_char..=caret.char), cfg, usize::MAX, true);
    }

    /// Internal absolute movement function. Assumes that the `col`
    /// and `line` of th [Pos] are correct.
    pub(crate) fn move_to<U>(&mut self, pos: Pos, text: &Text<U>, area: &U::Area, cfg: &PrintCfg)
    where
        U: Ui
    {
        let caret = &mut self.caret;

        caret.line = pos.line.min(text.len_lines().saturating_sub(1));
        let line_char = text.line_to_char(pos.line);
        caret.col = pos.col.min(text.iter_line_chars(caret.line).count());
        caret.char = text.line_to_char(caret.line) + caret.col;
        caret.byte = text.char_to_byte(caret.char);

        self.desired_x =
            area.get_width(text.iter_range(line_char..caret.char), cfg, usize::MAX, true);

        self.anchor = None;
    }

    /// Returns the range between `target` and `anchor`.
    ///
    /// If `anchor` isn't set, returns an empty range on `target`.
    pub fn range(&self) -> Range<usize> {
        let anchor = self.anchor.unwrap_or(self.caret);
        if anchor < self.caret {
            anchor.char..self.caret.char
        } else {
            self.caret.char..anchor.char
        }
    }

    /// Returns the range between `target` and `anchor`.
    ///
    /// If `anchor` isn't set, returns an empty range on `target`.
    pub fn pos_range(&self) -> (Pos, Pos) {
        let anchor = self.anchor.unwrap_or(self.caret);
        (self.caret.min(anchor), self.caret.max(anchor))
    }

    /// Returns the cursor's position on the screen.
    pub fn caret(&self) -> Pos {
        self.caret
    }

    /// Calibrates a cursor's positions based on some splice.
    pub(crate) fn calibrate_on_accum(&mut self, edit_accum: &EditAccum, inner: &InnerText) {
        self.assoc_index.as_mut().map(|i| i.saturating_add_signed(edit_accum.changes));
        self.caret.calibrate(edit_accum.chars, inner);
        self.anchor.as_mut().map(|anchor| anchor.calibrate(edit_accum.chars, inner));
    }

    /// Sets the position of the anchor to be the same as the current
    /// cursor position in the file.
    ///
    /// The `anchor` and `current` act as a range of text on the file.
    pub fn set_anchor(&mut self) {
        self.anchor = Some(self.caret)
    }

    /// Unsets the anchor.
    ///
    /// This is done so the cursor no longer has a valid selection.
    pub fn unset_anchor(&mut self) {
        self.anchor = None;
    }

    pub fn anchor(&self) -> Option<Pos> {
        self.anchor
    }

    /// The byte (relative to the beginning of the file) of the caret.
    /// Indexed at 1. Intended only for displaying by the end
    /// user. For internal use, see `true_byte()`.
    pub fn byte(&self) -> usize {
        self.caret.byte + 1
    }

    /// The column of the caret. Indexed at 1. Intended only for
    /// displaying by the end user. For internal use, see
    /// `true_col()`.
    pub fn col(&self) -> usize {
        self.caret.col + 1
    }

    /// The line of the caret. Indexed at 1. Intended only for
    /// displaying by the end user. For internal use, see
    /// `true_row()`.
    pub fn line(&self) -> usize {
        self.caret.line + 1
    }

    /// The byte (relative to the beginning of the file) of the caret.
    /// Indexed at 0.
    pub fn true_byte(&self) -> usize {
        self.caret.byte
    }

    /// The column of the caret. Indexed at 0.
    pub fn true_col(&self) -> usize {
        self.caret.col
    }

    /// The line of the caret. Indexed at 0.
    pub fn true_line(&self) -> usize {
        self.caret.line
    }

    pub(crate) fn try_merge(&mut self, start: Pos, end: Pos) -> Result<(), ()> {
        if !pos_intersects(self.pos_range(), (start, end)) {
            return Err(());
        }
        let Some(anchor) = self.anchor.as_mut() else {
            self.anchor = Some(start);
            self.caret = self.caret.max(end);
            return Ok(());
        };

        if *anchor > self.caret {
            *anchor = (*anchor).max(end);
            self.caret = self.caret.min(start);
        } else {
            *anchor = (*anchor).min(start);
            self.caret = self.caret.max(end);
        }

        Ok(())
    }
}

impl std::fmt::Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.caret.line + 1, self.caret.col + 1))
    }
}

impl Clone for Cursor {
    fn clone(&self) -> Self {
        Cursor {
            desired_x: self.caret.col,
            assoc_index: None,
            ..*self
        }
    }
}

/// A cursor that can edit text in its selection, but can't move the
/// selection in any way.
pub struct Editor<'a, U>
where
    U: Ui
{
    cursor: &'a mut Cursor,
    text: &'a mut Text<U>,
    edit_accum: &'a mut EditAccum,
    print_info: Option<U::PrintInfo>,
    history: Option<&'a mut History<U>>
}

impl<'a, U> Editor<'a, U>
where
    U: Ui
{
    /// Returns a new instance of `Editor`.
    pub fn new(
        cursor: &'a mut Cursor, text: &'a mut Text<U>, edit_accum: &'a mut EditAccum,
        print_info: Option<U::PrintInfo>, history: Option<&'a mut History<U>>
    ) -> Self {
        cursor.calibrate_on_accum(edit_accum, text.inner());
        Self {
            cursor,
            text,
            edit_accum,
            print_info,
            history
        }
    }

    /// Replaces the entire selection of the `TextCursor` with new
    /// text.
    pub fn replace(&mut self, edit: impl ToString) {
        let change = Change::new(edit.to_string(), self.cursor.range(), self.text.inner());
        let (start, end) = (change.start, change.added_end());

        self.edit(change);

        if let Some(anchor) = &mut self.cursor.anchor {
            if anchor.char > self.cursor.caret.char {
                *anchor = Pos::new(end, self.text.inner());
                return;
            }
        }

        self.cursor.caret = Pos::new(end, self.text.inner());
        self.cursor.anchor = Some(Pos::new(start, self.text.inner()));
    }

    /// Inserts new text directly behind the caret.
    pub fn insert(&mut self, edit: impl ToString) {
        let range = self.cursor.caret.char..self.cursor.caret.char;
        let change = Change::new(edit.to_string(), range, self.text.inner());
        let (added_end, taken_end) = (change.added_end(), change.taken_end());

        self.edit(change);

        let ch_diff = added_end as isize - taken_end as isize;

        if let Some(anchor) = &mut self.cursor.anchor {
            if *anchor > self.cursor.caret {
                anchor.calibrate(ch_diff, self.text.inner());
            }
        }
    }

    /// Edits the file with a cursor.
    fn edit(&mut self, change: Change) {
        self.text.apply_change(&change);
        self.edit_accum.chars += change.added_end() as isize - change.taken_end() as isize;

        if let Some(history) = &mut self.history {
            let assoc_index = self.cursor.assoc_index;
            let (insertion_index, change_diff) =
                history.add_change(change, assoc_index, self.print_info.unwrap_or_default());
            self.cursor.assoc_index = Some(insertion_index);
            self.edit_accum.changes += change_diff;
        }
    }
}

/// A cursor that can move and alter the selection, but can't edit the
/// file.
pub struct Mover<'a, U>
where
    U: Ui
{
    cursor: &'a mut Cursor,
    text: &'a Text<U>,
    area: &'a U::Area,
    print_cfg: PrintCfg
}

impl<'a, U> Mover<'a, U>
where
    U: Ui
{
    /// Returns a new instance of `Mover`.
    pub fn new(
        cursor: &'a mut Cursor, text: &'a Text<U>, area: &'a U::Area, print_cfg: PrintCfg
    ) -> Self {
        Self {
            cursor,
            text,
            area,
            print_cfg
        }
    }

    ////////// Public movement functions

    /// Moves the cursor vertically on the file. May also cause
    /// vertical movement.
    pub fn move_ver(&mut self, count: isize) {
        self.cursor.move_ver::<U>(count, &self.text, self.area, &self.print_cfg);
    }

    /// Moves the cursor horizontally on the file. May also cause
    /// vertical movement.
    pub fn move_hor(&mut self, count: isize) {
        self.cursor.move_hor::<U>(count, &self.text, self.area, &self.print_cfg);
    }

    /// Moves the cursor to a position in the file.
    ///
    /// - If the position isn't valid, it will move to the "maximum"
    ///   position allowed.
    /// - This command sets `desired_x`.
    pub fn move_to(&mut self, caret: Pos) {
        self.cursor.move_to::<U>(caret, &self.text, self.area, &self.print_cfg);
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn anchor(&self) -> Option<Pos> {
        self.cursor.anchor
    }

    /// Returns the anchor of the `TextCursor`.
    pub fn caret(&self) -> Pos {
        self.cursor.caret
    }

    /// Returns and takes the anchor of the `TextCursor`.
    pub fn take_anchor(&mut self) -> Option<Pos> {
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

fn pos_intersects(left: (Pos, Pos), right: (Pos, Pos)) -> bool {
    (left.0 > right.0 && right.1 > left.0) || (right.0 > left.0 && left.1 > right.0)
}
