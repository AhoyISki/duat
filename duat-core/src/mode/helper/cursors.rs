use std::cell::Cell;

use gapbuf::{GapBuffer, gap_buffer};
use serde::{Deserialize, Serialize, de::Visitor, ser::SerializeSeq};

pub use self::cursor::{Cursor, VPoint};
use crate::{add_shifts, merging_range_by_guess_and_lazy_shift, text::Change};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Cursors {
    buf: CursorGapBuffer,
    main_i: usize,
    shift_state: Cell<(usize, [i32; 3])>,
}

impl Cursors {
    ////////// Definition functions
    pub fn new() -> Self {
        Self {
            buf: CursorGapBuffer(gap_buffer![Cursor::default()]),
            main_i: 0,
            shift_state: Cell::new((0, [0; 3])),
        }
    }

    pub fn insert(&mut self, guess_i: usize, cursor: Cursor, was_main: bool) -> [usize; 2] {
        let (sh_from, shift) = self.shift_state.take();

        // The range of cursors that will be drained
        let c_range = merging_range_by_guess_and_lazy_shift(
            (&self.buf.0, self.buf.len()),
            (guess_i, [cursor.start(), cursor.end_excl()]),
            (sh_from, shift),
            (Cursor::start, Cursor::end_excl),
        );

        // Shift all ranges that preceed the end of the cursor's range.
        if sh_from < c_range.end && shift != [0; 3] {
            for cursor in self.buf.range(sh_from..c_range.end).into_iter() {
                cursor.shift_by(shift);
            }
        }

        // Get the minimum and maximum Points in the taken range, designate
        // those as the new Cursor's bounds.
        let (caret, anchor) = {
            let mut c_range = c_range.clone();
            let first = c_range.next().and_then(|i| self.get(i));
            let last = c_range.last().and_then(|i| self.get(i));
            let start = first
                .map(|c| c.lazy_v_start().min(cursor.lazy_v_start()))
                .unwrap_or(cursor.lazy_v_start());
            let end = last
                .map(|c| c.lazy_v_end().max(cursor.lazy_v_end()))
                .unwrap_or(cursor.lazy_v_end());

            if let Some(anchor) = cursor.anchor() {
                match cursor.caret() < anchor {
                    true => (start, Some(end)),
                    false => (end, Some(start)),
                }
            } else {
                (end, (start != end).then_some(start))
            }
        };

        let cursor = Cursor::from_v(caret, anchor, cursor.change_i);
        self.buf.splice(c_range.clone(), [cursor]);

        if was_main {
            self.main_i = c_range.start;
        } else if self.main_i > c_range.start {
            self.main_i = (self.main_i - c_range.clone().count()).max(c_range.start)
        }

        // If there are no more Cursors after this, don't set the shift_state.
        let cursors_taken = c_range.clone().count();
        if c_range.start + 1 < self.buf.len() {
            self.shift_state.set((
                sh_from.saturating_sub(cursors_taken).max(c_range.start) + 1,
                shift,
            ));
        }

        [c_range.start, cursors_taken]
    }

    /// Applies a [`Change`] to the [`Cursor`]s list
    pub(crate) fn apply_change(&mut self, guess_i: usize, change: Change<&str>) {
        let (sh_from, shift) = self.shift_state.take();

        // The range of cursors that will be drained
        let c_range = merging_range_by_guess_and_lazy_shift(
            (&self.buf.0, self.buf.len()),
            (guess_i, [change.start(), change.taken_end()]),
            (sh_from, shift),
            (Cursor::start, Cursor::end_excl),
        );

        // Since applied changes don't remove Cursors, we need to shift all
        // Cursors in the whole range. First by the original shift, in order
        // to update them to the latest shift leve, then by the change.
        if c_range.end > sh_from && shift != [0; 3] {
            for cursor in self.buf.range(sh_from..c_range.end).into_iter() {
                cursor.shift_by(shift);
            }
        }
        let range = c_range.start..c_range.end.max(sh_from);
        for cursor in self.buf.range(range).into_iter() {
            cursor.shift_by_change(change);
        }

        if c_range.end < self.buf.len() {
            self.shift_state
                .set((sh_from.max(c_range.end), add_shifts(shift, change.shift())));
        }
    }

    pub fn rotate_main(&mut self, amount: i32) {
        self.main_i = (self.main_i as i32 + amount).rem_euclid(self.buf.len() as i32) as usize
    }

    pub fn remove_extras(&mut self) {
        if !self.is_empty() {
            let cursor = self.buf.remove(self.main_i);
            let (sh_from, shift) = self.shift_state.take();
            if sh_from <= self.main_i {
                cursor.shift_by(shift);
            }
            self.buf = CursorGapBuffer(gap_buffer![cursor]);
        }
        self.main_i = 0;
    }

    pub fn get_main(&self) -> Option<&Cursor> {
        self.get(self.main_i)
    }

    pub fn get(&self, i: usize) -> Option<&Cursor> {
        if i >= self.len() {
            return None;
        }
        self.try_shift_until(i + 1);
        self.buf.get(i)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Cursor, bool)> {
        // Since we don't know how many Cursors will be iterated over, we
        // shift all cursors, just in case.
        let (sh_from, shift) = self.shift_state.take();
        if shift != [0; 3] {
            for cursor in self.buf.range(sh_from..).iter() {
                cursor.shift_by(shift);
            }
        }
        self.buf
            .iter()
            .enumerate()
            .map(move |(i, cursor)| (cursor, i == self.main_i))
    }

    pub fn main_index(&self) -> usize {
        self.main_i
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn clear(&mut self) {
        self.buf = CursorGapBuffer(GapBuffer::new());
        self.shift_state.take();
    }

    pub fn reset(&mut self) {
        self.remove_extras();
        self.buf[self.main_i] = Cursor::default();
    }

    pub(super) fn remove(&mut self, i: usize) -> Option<(Cursor, bool)> {
        if i >= self.buf.len() {
            return None;
        }
        self.try_shift_until(i + 1);
        let was_main = self.main_i == i;
        if self.main_i >= i {
            self.main_i = self.main_i.saturating_sub(1);
        }
        Some((self.buf.remove(i), was_main))
    }

    pub(super) fn populate(&mut self) {
        if self.buf.0.is_empty() {
            self.main_i = 0;
            self.buf.0 = gap_buffer![Cursor::default()];
        }
    }

    fn try_shift_until(&self, end: usize) {
        let (sh_from, shift) = self.shift_state.get();
        if end > sh_from && shift != [0; 3] {
            for cursor in self.buf.range(sh_from..end).iter() {
                cursor.shift_by(shift);
            }
            if end < self.buf.len() {
                self.shift_state.set((end, shift));
            } else {
                self.shift_state.take();
            }
        }
    }
}

impl Default for Cursors {
    fn default() -> Self {
        Self::new()
    }
}

mod cursor {
    use std::{cell::Cell, cmp::Ordering, ops::Range};

    use serde::{Deserialize, Serialize};

    use crate::{
        cfg::PrintCfg,
        text::{Change, Point, Text},
        ui::{Area, Caret},
    };

    /// A cursor in the text file. This is an editing cursor, -(not
    /// a printing cursor.
    #[derive(Default, Clone, Debug, Serialize, Deserialize)]
    pub struct Cursor {
        caret: Cell<LazyVPoint>,
        anchor: Cell<Option<LazyVPoint>>,
        pub(in crate::mode::helper) change_i: Option<u32>,
    }

    impl Cursor {
        /// Returns a new instance of [`Cursor`].
        pub fn new(caret: Point, anchor: Option<Point>, text: &Text) -> Self {
            let last = text.last_point().unwrap();
            let caret = caret.min(last);
            let anchor = anchor.map(|a| a.min(last));
            Self {
                caret: Cell::new(LazyVPoint::Unknown(caret)),
                anchor: Cell::new(anchor.map(LazyVPoint::Unknown)),
                change_i: None,
            }
        }

        pub(super) fn from_v(
            caret: LazyVPoint,
            anchor: Option<LazyVPoint>,
            change_i: Option<u32>,
        ) -> Self {
            Self {
                caret: Cell::new(caret),
                anchor: Cell::new(anchor),
                change_i,
            }
        }

        /// Moves to specific, pre calculated [`Point`].
        pub fn move_to(&mut self, p: Point, text: &Text) {
            if p == self.caret() {
                return;
            }
            let p = text.point_at(p.byte().min(text.last_point().unwrap().byte()));
            *self.caret.get_mut() = LazyVPoint::Unknown(p);
        }

        /// Internal horizontal movement function.
        pub fn move_hor(&mut self, by: i32, text: &Text) {
            let by = by as isize;
            let (Some(last), false) = (text.last_point(), by == 0) else {
                return;
            };
            let target = self.caret.get().point().char().saturating_add_signed(by);

            let p = if target == 0 {
                Point::default()
            } else if target >= last.char() {
                last
            } else if by.abs() < 500 {
                if by > 0 {
                    let (point, _) = text
                        .chars_fwd(self.caret())
                        .take(by as usize + 1)
                        .last()
                        .unwrap();
                    point
                } else {
                    let (point, _) = text
                        .chars_rev(self.caret())
                        .take(by.unsigned_abs())
                        .last()
                        .unwrap();
                    point
                }
            } else {
                text.point_at_char(target)
            };

            *self.caret.get_mut() = LazyVPoint::Unknown(p)
        }

        /// Internal vertical movement function.
        pub fn move_ver(&mut self, by: i32, text: &Text, area: &impl Area, cfg: PrintCfg) {
            let by = by as isize;
            let (Some(last), false) = (text.last_point(), by == 0) else {
                return;
            };

            *self.caret.get_mut() = LazyVPoint::Known({
                let vp = self.caret.get().calculate(text, area, cfg);
                let line_start = {
                    let target = self.caret.get().point().line().saturating_add_signed(by);
                    text.point_at_line(target.min(last.line()))
                };

                let mut wraps = 0;
                let mut vcol = 0;

                let (wcol, p) = area
                    .print_iter(text.iter_fwd(line_start), cfg.new_line_as('\n'))
                    .find_map(|(Caret { len, x, wrap }, item)| {
                        wraps += wrap as usize;
                        if let Some((p, char)) = item.as_real_char() {
                            if vcol + len as u16 > vp.dvcol || char == '\n' {
                                return Some((x as u16, p));
                            }
                        }
                        vcol += len as u16;
                        None
                    })
                    .unwrap_or((0, last));

                vp.known(p, (p.char() - line_start.char()) as u16, vcol, wcol)
            });
        }

        /// Internal vertical movement function.
        pub fn move_ver_wrapped(&mut self, by: i32, text: &Text, area: &impl Area, cfg: PrintCfg) {
            if text.last_point().is_none() || by == 0 {
                return;
            };
            let vp = self.caret.get().calculate(text, area, cfg);

            let mut wraps = 0;

            *self.caret.get_mut() = LazyVPoint::Known(if by > 0 {
                let line_start = text.point_at_line(vp.p.line());
                let mut vcol = vp.vcol;
                let mut last = (vp.vcol, vp.wcol, vp.p);
                let mut last_valid = (vp.vcol, vp.wcol, vp.p);

                let (vcol, wcol, p) = area
                    .print_iter(text.iter_fwd(line_start), cfg.new_line_as('\n'))
                    .skip_while(|(_, item)| item.byte() <= self.byte())
                    .find_map(|(Caret { x, len, wrap }, item)| {
                        wraps += wrap as i32;
                        if let Some((p, char)) = item.as_real_char() {
                            if (x..x + len).contains(&(vp.dwcol as u32)) || char == '\n' {
                                last_valid = (vcol, x as u16, p);
                                if wraps == by {
                                    return Some((vcol, x as u16, p));
                                }
                            } else if wraps > by {
                                return Some(last);
                            }
                            last = (vcol, x as u16, p);
                        }
                        vcol += len as u16;
                        None
                    })
                    .unwrap_or(last_valid);
                vp.known(p, (p.char() - line_start.char()) as u16, vcol, wcol)
            } else {
                let end_points = text.points_after(vp.p).unwrap();
                let mut just_wrapped = false;
                let mut last_valid = (vp.wcol, vp.p);

                let mut iter =
                    area.rev_print_iter(text.iter_rev(end_points), cfg.new_line_as('\n'));
                let wcol_and_p = iter.find_map(|(Caret { x, len, wrap }, item)| {
                    if let Some((p, _)) = item.as_real_char() {
                        if (x..x + len).contains(&(vp.dwcol as u32))
                            || (just_wrapped && x + len < vp.dwcol as u32)
                        {
                            last_valid = (x as u16, p);
                            if wraps == by {
                                return Some((x as u16, p));
                            }
                        }
                        just_wrapped = false;
                    }
                    wraps -= wrap as i32;
                    just_wrapped |= wrap;
                    None
                });

                if let Some((wcol, p)) = wcol_and_p {
                    let (ccol, vcol) = iter
                        .take_while(|(_, item)| item.as_real_char().is_none_or(|(_, c)| c != '\n'))
                        .fold((0, 0), |(ccol, vcol), (caret, item)| {
                            (ccol + item.is_real() as u16, vcol + caret.len as u16)
                        });
                    vp.known(p, ccol, vcol, wcol)
                } else {
                    let (wcol, p) = last_valid;
                    let (ccol, vcol) = area
                        .rev_print_iter(text.iter_rev(p), cfg.new_line_as('\n'))
                        .take_while(|(_, item)| item.as_real_char().is_none_or(|(_, c)| c != '\n'))
                        .fold((0, 0), |(ccol, vcol), (caret, item)| {
                            (ccol + item.is_real() as u16, vcol + caret.len as u16)
                        });
                    vp.known(p, ccol, vcol, wcol)
                }
            });
        }

        pub(crate) fn shift_by_change(&self, change: Change<&str>) {
            let (shift, taken) = (change.shift(), change.taken_end());
            if self.caret() >= change.start() {
                let shifted_caret = self.caret().max(taken).shift_by(shift);
                self.caret.set(LazyVPoint::Unknown(shifted_caret));
            }
            if let Some(anchor) = self.anchor.get()
                && anchor.point() >= change.start()
            {
                let shifted_anchor = anchor.point().max(taken).shift_by(shift);
                self.anchor.set(Some(LazyVPoint::Unknown(shifted_anchor)));
            }
        }

        /// Assumes tha both parts of the cursor are ahead of the
        /// shift
        pub(crate) fn shift_by(&self, shift: [i32; 3]) {
            let shifted_caret = self.caret().shift_by(shift);
            self.caret.set(LazyVPoint::Unknown(shifted_caret));
            if let Some(anchor) = self.anchor.get() {
                let shifted_anchor = anchor.point().shift_by(shift);
                self.anchor.set(Some(LazyVPoint::Unknown(shifted_anchor)));
            }
        }

        /// Sets the position of the anchor to be the same as the
        /// current cursor position in the file
        ///
        /// The `anchor` and `current` act as a range of text on the
        /// file.
        pub fn set_anchor(&mut self) {
            *self.anchor.get_mut() = Some(self.caret.get())
        }

        /// Unsets the anchor
        ///
        /// This is done so the cursor no longer has a valid
        /// selection.
        pub fn unset_anchor(&mut self) -> Option<Point> {
            self.anchor.take().map(|a| a.point())
        }

        /// Switches the position of the anchor and caret
        pub fn swap_ends(&mut self) {
            if let Some(anchor) = self.anchor.get_mut() {
                std::mem::swap(self.caret.get_mut(), anchor);
            }
        }

        /// Returns the cursor's position on the screen
        pub fn caret(&self) -> Point {
            self.caret.get().point()
        }

        pub fn anchor(&self) -> Option<Point> {
            self.anchor.get().map(|a| a.point())
        }

        /// The byte (relative to the beginning of the file) of the
        /// caret. Indexed at 0
        pub fn byte(&self) -> usize {
            self.caret.get().point().byte()
        }

        /// The char (relative to the beginning of the file) of the
        /// caret. Indexed at 0
        pub fn char(&self) -> usize {
            self.caret.get().point().char()
        }

        /// The line of the caret. Indexed at 0.
        pub fn line(&self) -> usize {
            self.caret.get().point().line()
        }

        /// Returns the range between `caret` and `anchor`.
        ///
        /// If `anchor` isn't set, returns an empty range on `caret`.
        ///
        /// A [`Cursor`]'s range will also never include the last
        /// character in a [`Text`], which must be a newline.
        ///
        /// # Warning
        ///
        /// This function will return the range that is supposed
        /// to be replaced, if `self.is_inclusive()`, this means that
        /// it will return one more byte at the end, i.e. start..=end.
        pub fn range(&self, text: &Text) -> Range<usize> {
            let [start, end] = self.point_range(text);
            start.byte()..end.byte()
        }

        /// The starting [`Point`] of this [`Cursor`]
        pub fn start(&self) -> Point {
            if let Some(anchor) = self.anchor.get() {
                anchor.point().min(self.caret.get().point())
            } else {
                self.caret.get().point()
            }
        }

        /// The ending [`Point`] of this [`Cursor`]
        pub fn end(&self, text: &Text) -> Point {
            let raw = self.end_excl();
            raw.fwd(text.char_at(raw).unwrap())
        }

        pub(crate) fn end_excl(&self) -> Point {
            if let Some(anchor) = self.anchor.get() {
                anchor.point().max(self.caret.get().point())
            } else {
                self.caret.get().point()
            }
        }

        pub(crate) fn tag_points(&self, text: &Text) -> (Point, Option<[Point; 2]>) {
            let caret = self.caret();
            if let Some(anchor) = self.anchor() {
                match anchor.cmp(&caret) {
                    Ordering::Less => (caret, Some([anchor, caret])),
                    Ordering::Equal => (caret, None),
                    Ordering::Greater => {
                        let end = anchor.fwd(text.char_at(anchor).unwrap());
                        (caret, Some([caret, end]))
                    }
                }
            } else {
                (caret, None)
            }
        }

        /// Returns the range between `target` and `anchor`.
        ///
        /// like [`Cursor::range`], this function will not include
        /// beyond the last character's [`Point`].
        ///
        /// If `anchor` isn't set, returns an empty range on `target`.
        pub fn point_range(&self, text: &Text) -> [Point; 2] {
            [self.start(), self.end(text)]
        }

        /// Sets both the desired visual column, as well as the
        /// desired wrapped column
        pub fn set_desired_cols(&mut self, v: usize, w: usize) {
            let (v, w) = (v as u16, w as u16);
            match self.caret.get_mut() {
                LazyVPoint::Known(vp) => {
                    vp.dvcol = v;
                    vp.dwcol = w;
                }
                LazyVPoint::Unknown(p) => {
                    *self.caret.get_mut() = LazyVPoint::Desired { p: *p, dvcol: v, dwcol: w }
                }
                LazyVPoint::Desired { dvcol, dwcol, .. } => (*dvcol, *dwcol) = (v, w),
            }
        }

        pub fn v_caret(&self, text: &Text, area: &impl Area, cfg: PrintCfg) -> VPoint {
            let vp = self.caret.get().calculate(text, area, cfg);
            self.caret.set(LazyVPoint::Known(vp));
            vp
        }

        pub fn v_anchor(&self, text: &Text, area: &impl Area, cfg: PrintCfg) -> Option<VPoint> {
            self.anchor.get().map(|anchor| {
                let vp = anchor.calculate(text, area, cfg);
                self.anchor.set(Some(LazyVPoint::Known(vp)));
                vp
            })
        }

        pub fn v_range(&self, text: &Text, area: &impl Area, cfg: PrintCfg) -> [VPoint; 2] {
            let v_caret = self.v_caret(text, area, cfg);
            let v_anchor = self.v_anchor(text, area, cfg).unwrap_or(v_caret);
            [v_caret.min(v_anchor), v_caret.max(v_anchor)]
        }

        pub(super) fn lazy_v_start(&self) -> LazyVPoint {
            match self.anchor.get() {
                Some(anchor) => self.caret.get().min(anchor),
                None => self.caret.get(),
            }
        }

        pub(super) fn lazy_v_end(&self) -> LazyVPoint {
            match self.anchor.get() {
                Some(anchor) => self.caret.get().max(anchor),
                None => self.caret.get(),
            }
        }
    }

    #[derive(Clone, Copy, Debug, Eq, Serialize, Deserialize)]
    pub(super) enum LazyVPoint {
        Known(VPoint),
        Unknown(Point),
        Desired { p: Point, dvcol: u16, dwcol: u16 },
    }

    impl LazyVPoint {
        fn point(self) -> Point {
            match self {
                LazyVPoint::Known(vp) => vp.p,
                LazyVPoint::Unknown(p) => p,
                LazyVPoint::Desired { p, .. } => p,
            }
        }

        fn calculate(self, text: &Text, area: &impl Area, cfg: PrintCfg) -> VPoint {
            match self {
                Self::Known(vp) => vp,
                Self::Unknown(p) => VPoint::new(p, text, area, cfg),
                Self::Desired { p, dvcol, dwcol } => {
                    let mut vp = VPoint::new(p, text, area, cfg);
                    vp.dvcol = dvcol;
                    vp.dwcol = dwcol;
                    vp
                }
            }
        }
    }

    impl Default for LazyVPoint {
        fn default() -> Self {
            Self::Desired { p: Point::default(), dvcol: 0, dwcol: 0 }
        }
    }

    impl PartialOrd for LazyVPoint {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.point().cmp(&other.point()))
        }
    }

    impl Ord for LazyVPoint {
        fn cmp(&self, other: &Self) -> Ordering {
            self.partial_cmp(other).unwrap()
        }
    }

    impl PartialEq for LazyVPoint {
        fn eq(&self, other: &Self) -> bool {
            self.point() == other.point()
        }
    }

    #[derive(Clone, Copy, Debug, Eq, Serialize, Deserialize)]
    pub struct VPoint {
        p: Point,
        // No plan to suppert lines that are far too long
        ccol: u16,
        vcol: u16,
        dvcol: u16,
        wcol: u16,
        dwcol: u16,
    }

    impl VPoint {
        fn new(p: Point, text: &Text, area: &impl Area, cfg: PrintCfg) -> Self {
            let [start, _] = text.points_of_line(p.line());
            let mut vcol = 0;

            let wcol = area
                .print_iter(text.iter_fwd(text.visual_line_start(start)), cfg)
                .find_map(|(caret, item)| {
                    if let Some((lhs, _)) = item.as_real_char()
                        && lhs == p
                    {
                        return Some(caret.x as u16);
                    }
                    vcol += caret.len as u16;
                    None
                })
                .unwrap_or(0);

            Self {
                p,
                ccol: (p.char() - start.char()) as u16,
                vcol,
                dvcol: vcol,
                wcol,
                dwcol: wcol,
            }
        }

        fn known(self, p: Point, ccol: u16, vcol: u16, wcol: u16) -> Self {
            Self { p, ccol, vcol, wcol, ..self }
        }

        /// The byte index of this [`VPoint`]
        pub fn byte(&self) -> usize {
            self.p.byte()
        }

        /// The char index of this [`VPoint`]
        pub fn char(&self) -> usize {
            self.p.char()
        }

        /// The line index of this [`VPoint`]
        pub fn line(&self) -> usize {
            self.p.line()
        }

        /// Number of characters from the start of the line
        pub fn char_col(&self) -> usize {
            self.ccol as usize
        }

        /// Total space from the start of the line
        pub fn visual_col(&self) -> usize {
            self.vcol as usize
        }

        /// How much space there should be from the start of the line
        pub fn desired_visual_col(&self) -> usize {
            self.dvcol as usize
        }

        /// Total space from the left edge
        pub fn wrapped_col(&self) -> usize {
            self.wcol as usize
        }

        /// How much space there should be from the left edge
        pub fn desired_wrapped_col(&self) -> usize {
            self.dwcol as usize
        }
    }

    impl PartialOrd for VPoint {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.p.cmp(&other.p))
        }
    }

    impl Ord for VPoint {
        fn cmp(&self, other: &Self) -> Ordering {
            self.partial_cmp(other).unwrap()
        }
    }

    impl PartialEq for VPoint {
        fn eq(&self, other: &Self) -> bool {
            self.p == other.p
        }
    }
}

#[derive(Clone)]
struct CursorGapBuffer(GapBuffer<Cursor>);

impl std::ops::Deref for CursorGapBuffer {
    type Target = GapBuffer<Cursor>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for CursorGapBuffer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::fmt::Debug for CursorGapBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl Serialize for CursorGapBuffer {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut ser_gapbuf = serializer.serialize_seq(Some(self.0.len()))?;

        for cursor in self.0.iter() {
            ser_gapbuf.serialize_element(cursor)?;
        }
        ser_gapbuf.end()
    }
}

impl<'de> Deserialize<'de> for CursorGapBuffer {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct GapBufferVisitor;

        impl<'v> Visitor<'v> for GapBufferVisitor {
            type Value = CursorGapBuffer;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "This visitor expected a sequence of Cursors")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'v>,
            {
                let mut buf = if let Some(len) = seq.size_hint() {
                    GapBuffer::with_capacity(len)
                } else {
                    GapBuffer::new()
                };

                while let Some(cursor) = seq.next_element()? {
                    buf.push_back(cursor);
                }

                Ok(CursorGapBuffer(buf))
            }
        }

        deserializer.deserialize_seq(GapBufferVisitor)
    }
}
