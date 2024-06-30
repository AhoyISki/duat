use std::ops::Range;

use crate::{
    text::{ExactPos, IterCfg, PrintCfg, Text},
    ui::Area,
};

// NOTE: `col` and `line` are line based, while `byte` is file based.
/// A position in a [`Text`].
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Point {
    byte: usize,
    char: usize,
    line: usize,
    col: usize,
}

impl Point {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_byte(b: usize, text: &Text) -> Self {}

    pub fn from_coords(y: usize, x: usize, text: &Text) -> Self {
        let char = text.get_line_to_char(y);
        let char = if let Some(char) = char {
            char + x.min(text.iter_line_chars(y).count().saturating_sub(1))
        } else {
            text.len_chars()
        };

        Point::new()
    }

    pub fn calibrate(&mut self, ch_diff: isize, text: &Text) {
        let char = self.char.saturating_add_signed(ch_diff);
        *self = Point::new(char, text);
    }

    /// Returns the byte (relative to the beginning of the file) of
    /// self. Indexed at 0.
    pub fn byte(&self) -> usize {
        self.byte
    }

    /// Returns the char index (relative tow the beginning of the
    /// file). Indexed at 0.
    pub fn char(&self) -> usize {
        self.char
    }

    /// Returns the line. Indexed at 0.
    pub fn line(&self) -> usize {
        self.line
    }

    /// Returns the column. Indexed at 0.
    pub fn col(&self) -> usize {
        self.col
    }
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.col + 1, self.line + 1)
    }
}

/// A cursor in the text file. This is an editing cursor, not a
/// printing cursor.
#[derive(Default, Debug)]
pub struct Cursor {
    /// Current position of the cursor in the file.
    pub(crate) caret: Point,

    /// An anchor for a selection.
    pub(crate) anchor: Option<Point>,

    /// The index to a `Change` in the current `Moment`, used for
    /// greater efficiency.
    pub(crate) assoc_index: Option<usize>,

    /// Column that the cursor wants to be in.
    ///
    /// If the cursor moves to a line that is at least as wide as the
    /// desired_col, it will be placed in the desired_col. If the
    /// line is shorter, it will be placed in the last column of
    /// the line.
    desired_col: usize,
}

impl Cursor {
    /// Returns a new instance of [`Cursor`].
    pub fn new(point: Point, text: &Text, area: &impl Area, cfg: &PrintCfg) -> Cursor {
        Cursor {
            caret: point,
            // This should be fine.
            anchor: None,
            assoc_index: None,
            desired_col: area
                .rev_print_iter(text.rev_iter_at(point.char + 1), IterCfg::new(cfg))
                .next()
                .map(|(caret, _)| caret.x)
                .unwrap(),
        }
    }

    /// Internal vertical movement function.
    pub fn move_ver(&mut self, by: isize, text: &Text, area: &impl Area, cfg: &PrintCfg) {
        let cfg = IterCfg::new(cfg).dont_wrap();

        let rev_from_first_line = by < 0 && self.caret.line == 0;
        let fwd_from_last_line = by > 0 && self.caret.line + 1 == text.len_lines();
        if by == 0 || rev_from_first_line || fwd_from_last_line {
            return;
        }

        self.caret.char = if self.caret.line.saturating_add_signed(by) > text.len_lines() {
            text.len_chars()
        } else if by > 0 {
            area.print_iter(text.iter_at(self.caret.char).no_ghosts(), cfg)
                .filter_map(|(caret, item)| item.part.as_char().zip(Some((caret.x, item.real()))))
                .try_fold(0, |lfs, (byte, (x, pos))| {
                    let new_lfs = lfs + (byte == '\n') as isize;
                    match (lfs == by && x >= self.desired_col) || new_lfs > by {
                        true => std::ops::ControlFlow::Break(pos),
                        false => std::ops::ControlFlow::Continue(new_lfs),
                    }
                })
                .break_value()
                .unwrap_or(text.len_chars())
        } else if self.caret.line.checked_add_signed(by).is_none() {
            0
        } else {
            let start = area
                .rev_print_iter(text.rev_iter_at(self.caret.char).no_ghosts(), cfg)
                .filter_map(|(_, item)| item.part.as_char().zip(Some(item.real())))
                .try_fold(0, |lfs, (c, pos)| {
                    match (lfs - ((c == '\n') as isize)) < by {
                        true => std::ops::ControlFlow::Break(pos + 1),
                        false => std::ops::ControlFlow::Continue(lfs - ((c == '\n') as isize)),
                    }
                })
                .break_value()
                .unwrap_or(0);

            area.print_iter(text.iter_at(start).no_ghosts(), cfg)
                .filter_map(|(caret, item)| item.part.as_char().zip(Some((caret.x, item.real()))))
                .try_fold((), |_, (c, (x, pos))| {
                    match x >= self.desired_col || c == '\n' {
                        true => std::ops::ControlFlow::Break(pos),
                        false => std::ops::ControlFlow::Continue(()),
                    }
                })
                .break_value()
                .unwrap_or(text.len_chars())
        };

        self.caret.line = text.char_to_line(self.caret.char);
        self.caret.byte = text.char_to_byte(self.caret.char);

        // In vertical movement, the `desired_x` dictates in what column the
        // cursor will be placed.
        self.caret.col = area
            .rev_print_iter(text.rev_iter_at(self.caret.char + 1), cfg)
            .find_map(|(caret, item)| item.part.is_byte().then_some(caret.x))
            .unwrap();
    }

    /// Internal vertical movement function.
    pub fn move_ver_wrapped(
        &mut self,
        _by: isize,
        _text: &Text,
        _area: &impl Area,
        _cfg: &PrintCfg,
    ) {
        todo!()
    }

    /// Internal horizontal movement function.
    pub fn move_hor(&mut self, by: isize, text: &Text, area: &impl Area, cfg: &PrintCfg) {
        let cfg = IterCfg::new(cfg).dont_wrap();

        if by == 0 {
            return;
        }

        self.caret.char = if self.caret.char.saturating_add_signed(by) >= text.len_chars() {
            text.len_chars()
        } else if by > 0 {
            text.iter_at(self.caret.char)
                .no_ghosts()
                .filter_map(|item| item.part.as_char().and(Some(item.real())))
                .nth(by as usize)
                .unwrap_or(text.len_chars())
        } else if self.caret.char.checked_add_signed(by).is_none() {
            0
        } else {
            text.rev_iter_at(self.caret.char)
                .no_ghosts()
                .filter_map(|item| item.part.as_char().and(Some(item.real())))
                .nth(by.unsigned_abs() - 1)
                .unwrap_or(0)
        };

        self.caret.line = text.char_to_line(self.caret.char);
        self.caret.byte = text.char_to_byte(self.caret.char);

        let exact_pos = ExactPos::at_cursor_char(self.caret.char);
        self.caret.col = area
            .rev_print_iter(text.rev_iter_following(exact_pos), cfg)
            .find_map(|(caret, item)| item.part.is_byte().then_some(caret.x))
            .unwrap_or(0);
        self.desired_col = self.caret.col;
    }

    /// Moves to specific, pre calculated [`Point`].
    pub fn move_to(&mut self, point: Point, text: &Text, area: &impl Area, cfg: &PrintCfg) {
        self.caret = point;

        self.desired_col = area
            .rev_print_iter(text.rev_iter_at(point.char + 1), IterCfg::new(cfg))
            .next()
            .map(|(caret, _)| caret.x)
            .unwrap();
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
    pub fn pos_range(&self) -> (Point, Point) {
        let anchor = self.anchor.unwrap_or(self.caret);
        (self.caret.min(anchor), self.caret.max(anchor))
    }

    /// Returns the cursor's position on the screen.
    pub fn caret(&self) -> Point {
        self.caret
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

    pub fn anchor(&self) -> Option<Point> {
        self.anchor
    }

    /// The byte (relative to the beginning of the file) of the caret.
    /// Indexed at 0.
    pub fn byte(&self) -> usize {
        self.caret.byte
    }

    /// The char (relative to the beginning of the file) of the caret.
    /// Indexed at 0.
    pub fn char(&self) -> usize {
        self.caret.char
    }

    /// The column of the caret. Indexed at 0.
    pub fn col(&self) -> usize {
        self.caret.col
    }

    /// The line of the caret. Indexed at 0.
    pub fn line(&self) -> usize {
        self.caret.line
    }
}

impl std::fmt::Display for Cursor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.caret.line + 1, self.caret.col + 1)
    }
}

impl Clone for Cursor {
    fn clone(&self) -> Self {
        Cursor {
            desired_col: self.caret.col,
            assoc_index: None,
            ..*self
        }
    }
}
