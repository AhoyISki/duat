pub use self::cursor::Cursor;
use super::Diff;
use crate::{
    text::{Point, PrintCfg, Text},
    ui::Area,
};

#[derive(Clone, Debug)]
pub struct Cursors {
    list: Vec<Cursor>,
    main: usize,
    inclusive_ranges: bool,
}

impl Cursors {
    pub fn new_exclusive() -> Self {
        Self {
            list: vec![Cursor::new_at_0(false)],
            main: 0,
            inclusive_ranges: false,
        }
    }

    pub fn new_inclusive() -> Self {
        Self {
            list: vec![Cursor::new_at_0(true)],
            main: 0,
            inclusive_ranges: true,
        }
    }

    pub fn insert_from_parts(
        &mut self,
        point: Point,
        text: &Text,
        area: &impl Area,
        cfg: &PrintCfg,
    ) -> usize {
        let cursor = Cursor::new(point, text, area, cfg, self.inclusive_ranges);
        let start = cursor.range().start;
        let (Ok(i) | Err(i)) = self.list.binary_search_by_key(&start, |c| c.range().start);

        if !self.try_merge_on(i, &cursor) {
            self.list.insert(i, cursor);
        }

        i
    }

    pub fn rotate_main_fwd(&mut self) {
        match self.main == self.list.len() - 1 {
            true => self.main = 0,
            false => self.main += 1,
        }
    }

    pub fn rotate_main_rev(&mut self) {
        match self.main == 0 {
            true => self.main = self.list.len() - 1,
            false => self.main -= 1,
        }
    }

    pub fn remove_extras(&mut self) {
        let cursor = self.list[self.main].clone();
        self.list = vec![cursor];
        self.main = 0;
    }

    pub fn main(&self) -> &Cursor {
        &self.list[self.main]
    }

    pub fn get(&self, i: usize) -> Option<Cursor> {
        self.list.get(i).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Cursor, bool)> {
        self.list
            .iter()
            .enumerate()
            .map(move |(index, cursor)| (cursor, index == self.main))
    }

    pub fn main_index(&self) -> usize {
        self.main
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_inclusive(&self) -> bool {
        self.inclusive_ranges
    }

    pub fn reset(&mut self) {
        self.list = vec![Cursor::new_at_0(self.inclusive_ranges)]
    }

    pub(crate) fn clear(&mut self) {
        self.list.clear()
    }

    pub(super) fn insert_removed(&mut self, orig_i: usize, cursor: Cursor) -> usize {
        let start = cursor.range().start;
        let (Ok(i) | Err(i)) = self.list.binary_search_by_key(&start, |c| c.range().start);

        if !self.try_merge_on(i, &cursor) {
            self.list.insert(i, cursor)
        }

        if self.main == orig_i {
            self.main = i;
        } else if self.main == i {
            self.main += 1;
        };

        i
    }

    pub(super) fn replace(&mut self, orig_i: usize, cursor: Cursor) -> usize {
        let start = cursor.range().start;
        let (Ok(i) | Err(i)) = self.list.binary_search_by_key(&start, |c| c.range().start);

        let i = if self.try_merge_on(i, &cursor) {
            self.list.remove(orig_i);
            i - 1
        } else if orig_i == i {
            self.list[i] = cursor;
            i
        } else {
            self.list.insert(i, cursor);
            self.list.remove(orig_i);
            i - (orig_i < i) as usize
        };

        if self.main == orig_i {
            self.main = i;
        } else if self.main == i {
            self.main += 1;
        };

        i
    }

    pub(super) fn shift(
        &mut self,
        after: usize,
        diff: Diff,
        text: &Text,
        area: &impl Area,
        cfg: &PrintCfg,
    ) {
        if !diff.no_change() {
            for cursor in self.list.iter_mut().skip(after + 1) {
                diff.shift_cursor(cursor, text, area, cfg);
            }
        }
    }

    pub(super) fn drain(&mut self) -> impl Iterator<Item = Cursor> + '_ {
        self.list.drain(..)
    }

    fn try_merge_on(&mut self, i: usize, cursor: &Cursor) -> bool {
        let start = cursor.range().start;
        if let Some(prev_i) = i.checked_sub(1)
            && let Some(prev) = self.list.get_mut(prev_i)
            && prev.range().end > start
        {
            prev.merge_ahead(cursor.clone());
            true
        } else {
            false
        }
    }
}

mod cursor {
    use std::ops::Range;

    use crate::{
        text::{IterCfg, Point, PrintCfg, Text},
        ui::{Area, Caret},
    };

    /// A cursor in the text file. This is an editing cursor, not a
    /// printing cursor.
    #[derive(Debug)]
    pub struct Cursor {
        /// Current position of the cursor in the file.
        caret: VPoint,

        /// An anchor for a selection.
        anchor: Option<VPoint>,

        /// Wether or not the selection of this cursor is inclusive or
        /// not.
        is_inclusive: bool,

        /// The index to a `Change` in the current `Moment`, used for
        /// greater efficiency.
        pub(crate) assoc_index: Option<usize>,
    }

    impl Cursor {
        /// Returns a new instance of [`Cursor`].
        pub(super) fn new(
            point: Point,
            text: &Text,
            area: &impl Area,
            cfg: &PrintCfg,
            inclusive: bool,
        ) -> Cursor {
            Cursor {
                caret: VPoint::new(point, text, area, cfg),
                // This should be fine.
                anchor: None,
                assoc_index: None,
                is_inclusive: inclusive,
            }
        }

        pub fn new_at_0(inclusive: bool) -> Self {
            Self {
                is_inclusive: inclusive,
                caret: VPoint::default(),
                anchor: None,
                assoc_index: None,
            }
        }

        /// Moves to specific, pre calculated [`Point`].
        pub fn move_to(&mut self, point: Point, text: &Text, area: &impl Area, cfg: &PrintCfg) {
            let Some(last) = text.last_point() else {
                return;
            };
            self.caret = VPoint::new(point.min(last), text, area, cfg);
        }

        /// Internal horizontal movement function.
        pub fn move_hor(&mut self, by: isize, text: &Text, area: &impl Area, cfg: &PrintCfg) {
            let (Some(last), false) = (text.last_point(), by == 0) else {
                return;
            };
            let target = self.caret.point.char().saturating_add_signed(by);

            if target <= last.char() {
                let point = text.point_at_char(target);
                self.caret = VPoint::new(point, text, area, cfg);
            }
        }

        /// Internal vertical movement function.
        pub fn move_ver(&mut self, by: isize, text: &Text, area: &impl Area, cfg: &PrintCfg) {
            let (Some(last), false) = (text.last_point(), by == 0) else {
                return;
            };
            let cfg = IterCfg::new(cfg).dont_wrap();
            let dcol = self.caret.dcol;

            let point = {
                let target = self.caret.line().saturating_add_signed(by).min(last.line());
                let point = text.point_at_line(target);

                area.print_iter(text.iter_at(point), cfg)
                    .filter_map(|(caret, item)| Some(caret).zip(item.as_real_char()))
                    .find_map(|(Caret { x, len, .. }, (p, char))| {
                        (p.line() == target && (x + len > dcol || char == '\n')).then_some(p)
                    })
                    .unwrap_or(last)
            };

            self.caret.point = point;
            self.caret.vcol = vcol(point, text, area, cfg)
        }

        /// Internal vertical movement function.
        pub fn move_ver_wrapped(
            &mut self,
            by: isize,
            text: &Text,
            area: &impl Area,
            cfg: &PrintCfg,
        ) {
            if text.last_point().is_none() || by == 0 {
                return;
            };
            let cfg = IterCfg::new(cfg);
            let dwcol = self.caret.dwcol;

            let mut wraps = 0;
            let mut last_valid = self.caret();
            let mut new_wrap = false;

            let point = if by > 0 {
                let line_start = text.visual_line_start(self.caret.point);

                area.print_iter(text.iter_at(line_start), cfg)
                    .skip_while(|(_, item)| item.byte() <= self.byte())
                    .filter_map(|(caret, item)| {
                        wraps += caret.wrap as isize;
                        Some((caret, wraps)).zip(item.as_real_char())
                    })
                    .find_map(|((Caret { x, len, wrap }, wraps), (p, char))| {
                        new_wrap |= wrap;
                        if x + len > dwcol || char == '\n' {
                            if new_wrap {
                                new_wrap = false;
                                last_valid = p;
                            }
                            (wraps == by).then_some(p)
                        } else {
                            None
                        }
                    })
            } else {
                let end = text.points_after(self.caret.point).unwrap();

                area.rev_print_iter(text.rev_iter_at(end), cfg)
                    .filter_map(|(Caret { x, wrap, .. }, item)| {
                        let old_wraps = wraps;
                        wraps -= wrap as isize;
                        Some((x, old_wraps, wrap)).zip(item.as_real_char())
                    })
                    .find_map(|((x, wraps, wrap), (p, _))| {
                        if dwcol >= x || wrap {
                            if new_wrap {
                                new_wrap = false;
                                last_valid = p;
                            }
                            (wraps == by).then_some(p)
                        } else {
                            None
                        }
                    })
            };

            self.caret.point = point.unwrap_or(last_valid);
            self.caret.vcol = vcol(self.caret.point, text, area, cfg.dont_wrap())
        }

        /// Sets the position of the anchor to be the same as the
        /// current cursor position in the file.
        ///
        /// The `anchor` and `current` act as a range of text on the
        /// file.
        pub fn set_anchor(&mut self) {
            self.anchor = Some(self.caret)
        }

        /// Unsets the anchor.
        ///
        /// This is done so the cursor no longer has a valid
        /// selection.
        pub fn unset_anchor(&mut self) -> Option<Point> {
            self.anchor.take().map(|a| a.point)
        }

        /// Switches the position of the anchor and caret.
        pub fn swap_ends(&mut self) {
            if let Some(anchor) = self.anchor.as_mut() {
                std::mem::swap(&mut self.caret, anchor)
            }
        }

        /// Returns the cursor's position on the screen.
        pub fn caret(&self) -> Point {
            self.caret.point
        }

        pub fn anchor(&self) -> Option<Point> {
            self.anchor.map(|a| a.point)
        }

        /// The byte (relative to the beginning of the file) of the
        /// caret. Indexed at 0.
        pub fn byte(&self) -> usize {
            self.caret.byte()
        }

        /// The char (relative to the beginning of the file) of the
        /// caret. Indexed at 0.
        pub fn char(&self) -> usize {
            self.caret.char()
        }

        /// The column of the caret. Indexed at 0.
        pub fn vcol(&self) -> usize {
            self.caret.vcol()
        }

        /// The line of the caret. Indexed at 0.
        pub fn line(&self) -> usize {
            self.caret.line()
        }

        /// Returns the range between `target` and `anchor`.
        ///
        /// If `anchor` isn't set, returns an empty range on `target`.
        ///
        /// # Warning
        ///
        /// This function will return the range that is supposed
        /// to be replaced, if `self.is_inclusive()`, this means that
        /// it will return one more byte at the end, i.e. start..=end.
        pub fn range(&self) -> Range<usize> {
            let anchor = self.anchor.unwrap_or(self.caret);
            let (start, end) = if anchor < self.caret {
                (anchor.byte(), self.caret.byte())
            } else {
                (self.caret.byte(), anchor.byte())
            };

            match self.is_inclusive {
                true => start..(end + 1),
                false => start..end,
            }
        }

        /// Returns the range between `target` and `anchor`.
        ///
        /// If `anchor` isn't set, returns an empty range on `target`.
        ///
        /// # Warning
        ///
        /// Unlike [`Self::range()`], this function ignores the
        /// "inclusiveness" of the range.
        pub fn point_range(&self) -> (Point, Point) {
            let anchor = self.anchor.unwrap_or(self.caret);
            (
                self.caret.point.min(anchor.point),
                self.caret.point.max(anchor.point),
            )
        }

        pub fn is_inclusive(&self) -> bool {
            self.is_inclusive
        }

        pub(super) fn merge_ahead(&mut self, other: Cursor) {
            let other_end = if let Some(anchor) = other.anchor
                && anchor > other.caret
            {
                anchor
            } else {
                other.caret
            };

            if let Some(anchor) = self.anchor()
                && anchor > self.caret()
            {
                self.anchor = Some(other_end);
            } else {
                self.anchor = Some(self.caret);
                self.caret = other_end;
            }
        }
    }

    impl std::fmt::Display for Cursor {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}:{}", self.caret.line() + 1, self.caret.vcol() + 1)
        }
    }

    impl Clone for Cursor {
        fn clone(&self) -> Self {
            Cursor {
                assoc_index: None,
                ..*self
            }
        }
    }

    #[derive(Default, Debug, Clone, Copy, Eq)]
    struct VPoint {
        point: Point,
        vcol: usize,
        dcol: usize,
        dwcol: usize,
    }

    impl PartialOrd for VPoint {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.point.cmp(&other.point))
        }
    }

    impl Ord for VPoint {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.partial_cmp(other).unwrap()
        }
    }

    impl PartialEq for VPoint {
        fn eq(&self, other: &Self) -> bool {
            self.point == other.point
        }
    }

    impl VPoint {
        fn new(point: Point, text: &Text, area: &impl Area, cfg: &PrintCfg) -> Self {
            let cfg = IterCfg::new(cfg);
            let dwcol = vcol(point, text, area, cfg);
            let vcol = vcol(point, text, area, cfg.dont_wrap());
            Self {
                point,
                vcol,
                dcol: vcol,
                dwcol,
            }
        }

        fn byte(&self) -> usize {
            self.point.byte()
        }

        fn char(&self) -> usize {
            self.point.char()
        }

        fn line(&self) -> usize {
            self.point.line()
        }

        fn vcol(&self) -> usize {
            self.vcol
        }
    }

    fn vcol(point: Point, text: &Text, area: &impl Area, cfg: IterCfg) -> usize {
        if let Some(after) = text.points_after(point) {
            area.rev_print_iter(text.rev_iter_at(after), cfg)
                .find_map(|(caret, item)| item.part.is_char().then_some(caret.x))
                .unwrap_or(0)
        } else {
            area.rev_print_iter(text.rev_iter_at(text.len_point()), cfg)
                .find_map(|(caret, item)| item.part.is_char().then_some(caret.x + caret.len))
                .unwrap_or(0)
        }
    }
}
