use std::ops::RangeBounds;

use gapbuf::{GapBuffer, gap_buffer};
use serde::{Deserialize, Serialize, de::Visitor, ser::SerializeSeq};

pub use self::cursor::Cursor;
use crate::{
    cfg::PrintCfg,
    text::{Point, Text},
    ui::Area,
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Cursors {
    buf: CursorGapBuffer,
    main_i: usize,
    is_incl: bool,
}

impl Cursors {
    ////////// Definition functions
    pub fn new_excl() -> Self {
        Self {
            buf: CursorGapBuffer(gap_buffer![Cursor::default()]),
            main_i: 0,
            is_incl: false,
        }
    }

    pub fn new_incl() -> Self {
        Self {
            buf: CursorGapBuffer(gap_buffer![Cursor::default()]),
            main_i: 0,
            is_incl: true,
        }
    }

    pub fn reset_on_byte(&mut self, b: usize, text: &Text, area: &impl Area, cfg: PrintCfg) {
        let point = text.point_at(b.min(text.len().byte()));
        let cursor = Cursor::new(point, text, area, cfg);
        self.buf = CursorGapBuffer(gap_buffer![cursor]);
        self.main_i = 0;
    }

    pub fn make_excl(&mut self) {
        self.is_incl = false;
    }

    pub fn make_incl(&mut self) {
        self.is_incl = true;
    }

    ////////// Insertion functions
    pub fn insert_from_parts(
        &mut self,
        guess_i: usize,
        point: Point,
        range: usize,
        text: &Text,
        area: &impl Area,
        cfg: PrintCfg,
    ) -> usize {
        let mut cursor = Cursor::new(point, text, area, cfg);
        let range = match self.is_incl {
            true => range.saturating_sub(1),
            false => range,
        };
        if range > 0 {
            cursor.set_anchor();
            cursor.move_hor(range as i32, text, area, cfg);
        }
        self.insert(guess_i, false, cursor)
    }

    pub(super) fn insert(&mut self, guess_i: usize, was_main: bool, cursor: Cursor) -> usize {
        // The range of cursors that will be drained
        let c_range = if let Some(prev_i) = guess_i.checked_sub(1)
            && let Some(prev) = self.get(prev_i)
            && prev.start() <= cursor.start()
            && cursor.start_v() <= prev.end_v()
        {
            prev_i..guess_i
        } else {
            let buf = self.buf.range(..);
            match binary_search_by_key(buf, cursor.start_v(), |c| c.start_v()) {
                Ok(i) => i..i + 1,
                Err(i) => {
                    if let Some(prev_i) = i.checked_sub(1)
                        && let Some(prev) = self.buf.get(prev_i)
                        && cursor.start_v() <= prev.end_v()
                    {
                        prev_i..i
                    } else {
                        i..i
                    }
                }
            }
        };

        // This block determines how far ahead this cursor will merge
        let c_range = if self
            .get(c_range.end)
            .is_none_or(|c| cursor.end_v() < c.start_v())
        {
            c_range
        } else {
            let buf = self.buf.range(..);
            match binary_search_by_key(buf, cursor.end_v(), |c| c.start_v()) {
                Ok(i) => c_range.start..i + 1,
                Err(i) => {
                    if let Some(prev) = self.buf.get(i)
                        && prev.start_v() < cursor.end_v()
                    {
                        c_range.start..i + 1
                    } else {
                        c_range.start..i
                    }
                }
            }
        };

        let mut c_range_iter = c_range.clone();
        let first = c_range_iter.next().and_then(|i| self.get(i));
        let last = c_range_iter.last().and_then(|i| self.get(i));
        let start = first
            .map(|c| c.start_v().min(cursor.start_v()))
            .unwrap_or(cursor.start_v());
        let end = last
            .map(|c| c.end_v().max(cursor.end_v()))
            .unwrap_or(cursor.end_v());

        let (caret, anchor) = if let Some(anchor) = cursor.anchor() {
            match cursor.caret() < anchor {
                true => (start, Some(end)),
                false => (end, Some(start)),
            }
        } else {
            (end, (start != end).then_some(start))
        };

        let cursor = Cursor::from_v(caret, anchor, cursor.change_i);
        self.buf.drain(c_range.clone());
        self.buf.insert(c_range.start, cursor);

        if was_main {
            self.main_i = c_range.start;
        } else if self.main_i > c_range.start {
            self.main_i = (self.main_i - c_range.clone().count()).max(c_range.start)
        }

        c_range.start
    }

    pub fn rotate_main(&mut self, amount: i32) {
        self.main_i = (self.main_i as i32 + amount).rem_euclid(self.buf.len() as i32) as usize
    }

    pub fn remove_extras(&mut self) {
        if !self.is_empty() {
            let cursor = self.buf[self.main_i];
            self.buf = CursorGapBuffer(gap_buffer![cursor]);
        }
        self.main_i = 0;
    }

    /// The main [`Cursor`] in use
    ///
    /// # Panics
    ///
    /// Will panic if there are no [`Cursor`]s
    pub fn main(&self) -> &Cursor {
        &self.buf[self.main_i]
    }

    pub fn get_main(&self) -> Option<Cursor> {
        self.get(self.main_i)
    }

    pub fn get(&self, i: usize) -> Option<Cursor> {
        self.buf.get(i).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Cursor, bool)> {
        self.buf
            .iter()
            .enumerate()
            .map(move |(index, cursor)| (cursor, index == self.main_i))
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

    pub fn is_incl(&self) -> bool {
        self.is_incl
    }

    pub fn clear(&mut self) {
        self.buf = CursorGapBuffer(GapBuffer::new())
    }

    pub fn reset(&mut self) {
        self.remove_extras();
        self.buf[self.main_i] = Cursor::default();
    }

    pub(super) fn remove(&mut self, i: usize) -> Option<(Cursor, bool)> {
        (i < self.buf.len()).then(|| {
            let was_main = self.main_i == i;
            if self.main_i >= i {
                self.main_i = self.main_i.saturating_sub(1);
            }
            (self.buf.remove(i), was_main)
        })
    }

    pub(crate) fn shift_by(
        &mut self,
        from: usize,
        shift: (i32, i32, i32),
        text: &Text,
        area: &impl Area,
        cfg: PrintCfg,
    ) {
        for cursor in self.buf.iter_mut().skip(from) {
            cursor.shift_by(shift, text, area, cfg);
        }
    }

    pub(super) fn drain(
        &mut self,
        range: impl RangeBounds<usize>,
    ) -> impl Iterator<Item = (Cursor, bool)> + '_ {
        let orig_main = self.main_i;
        self.main_i = 0;
        self.buf
            .drain(range)
            .enumerate()
            .map(move |(i, c)| match i == orig_main {
                true => (c, true),
                false => (c, false),
            })
    }

    pub(super) fn populate(&mut self) {
        if self.buf.0.is_empty() {
            self.main_i = 0;
            self.buf.0 = gap_buffer![Cursor::default()];
        }
    }
}

impl Default for Cursors {
    fn default() -> Self {
        Self::new_excl()
    }
}

mod cursor {
    use std::ops::Range;

    use serde::{Deserialize, Serialize};

    use crate::{
        cfg::{IterCfg, PrintCfg},
        text::{Point, Text},
        ui::{Area, Caret},
    };

    /// A cursor in the text file. This is an editing cursor, -(not
    /// a printing cursor.
    #[derive(Default, Clone, Copy, Debug, Serialize, Deserialize)]
    pub struct Cursor {
        caret: VPoint,
        anchor: Option<VPoint>,
        pub(in crate::mode::helper) change_i: Option<usize>,
    }

    impl Cursor {
        /// Returns a new instance of [`Cursor`].
        pub(super) fn new(point: Point, text: &Text, area: &impl Area, cfg: PrintCfg) -> Self {
            Self {
                caret: VPoint::new(point, text, area, cfg),
                // This should be fine.
                anchor: None,
                change_i: None,
            }
        }

        pub(super) fn from_v(
            caret: VPoint,
            anchor: Option<VPoint>,
            change_i: Option<usize>,
        ) -> Self {
            Self { caret, anchor, change_i }
        }

        /// Moves to specific, pre calculated [`Point`].
        pub fn move_to(&mut self, point: Point, text: &Text, area: &impl Area, cfg: PrintCfg) {
            if point == self.caret() {
                return;
            }
            let Some(last) = text.last_point() else {
                self.anchor = None;
                self.caret = VPoint::default();
                return;
            };
            self.caret = VPoint::new(point.min(last), text, area, cfg);
        }

        /// Internal horizontal movement function.
        pub fn move_hor(&mut self, by: i32, text: &Text, area: &impl Area, cfg: PrintCfg) {
            let by = by as isize;
            let (Some(last), false) = (text.last_point(), by == 0) else {
                return;
            };
            let target = self.caret.point.char().saturating_add_signed(by);

            let point = if target == 0 {
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

            self.caret = VPoint::new(point, text, area, cfg);
        }

        /// Internal vertical movement function.
        pub fn move_ver(&mut self, by: i32, text: &Text, area: &impl Area, cfg: PrintCfg) {
            let by = by as isize;
            let (Some(last), false) = (text.last_point(), by == 0) else {
                return;
            };
            let cfg = IterCfg::new(cfg).dont_wrap();
            let dcol = self.caret.dcol;

            let point = {
                let target = self.caret.line().saturating_add_signed(by).min(last.line());
                let point = text.point_at_line(target);

                area.print_iter(text.iter_fwd(point), cfg)
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
        pub fn move_ver_wrapped(&mut self, by: i32, text: &Text, area: &impl Area, cfg: PrintCfg) {
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

                area.print_iter(text.iter_fwd(line_start), cfg)
                    .skip_while(|(_, item)| item.byte() <= self.byte())
                    .filter_map(|(caret, item)| {
                        wraps += caret.wrap as i32;
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

                area.rev_print_iter(text.iter_rev(end), cfg)
                    .filter_map(|(Caret { x, wrap, .. }, item)| {
                        let old_wraps = wraps;
                        wraps -= wrap as i32;
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

        pub(crate) fn shift_by(
            &mut self,
            shift: (i32, i32, i32),
            text: &Text,
            area: &impl Area,
            cfg: PrintCfg,
        ) {
            let shifted_caret = self.caret().shift_by(shift);
            self.move_to(shifted_caret, text, area, cfg);
            if let Some(anchor) = self.anchor() {
                let shifted_anchor = anchor.shift_by(shift);
                self.swap_ends();
                self.move_to(shifted_anchor, text, area, cfg);
                self.swap_ends();
            }
        }

        /// Sets the position of the anchor to be the same as the
        /// current cursor position in the file
        ///
        /// The `anchor` and `current` act as a range of text on the
        /// file.
        pub fn set_anchor(&mut self) {
            self.anchor = Some(self.caret)
        }

        /// Unsets the anchor
        ///
        /// This is done so the cursor no longer has a valid
        /// selection.
        pub fn unset_anchor(&mut self) -> Option<Point> {
            self.anchor.take().map(|a| a.point)
        }

        /// Switches the position of the anchor and caret
        pub fn swap_ends(&mut self) {
            if let Some(anchor) = self.anchor.as_mut() {
                std::mem::swap(&mut self.caret, anchor);
            }
        }

        /// Returns the cursor's position on the screen
        pub fn caret(&self) -> Point {
            self.caret.point
        }

        pub fn anchor(&self) -> Option<Point> {
            self.anchor.map(|a| a.point)
        }

        /// The byte (relative to the beginning of the file) of the
        /// caret. Indexed at 0
        pub fn byte(&self) -> usize {
            self.caret.byte()
        }

        /// The char (relative to the beginning of the file) of the
        /// caret. Indexed at 0
        pub fn char(&self) -> usize {
            self.caret.char()
        }

        /// The line of the caret. Indexed at 0.
        pub fn line(&self) -> usize {
            self.caret.line()
        }

        /// The column of the caret. Indexed at 0
        pub fn vcol(&self) -> usize {
            self.caret.vcol()
        }

        pub fn anchor_vcol(&self) -> Option<usize> {
            self.anchor.map(|a| a.vcol())
        }

        pub fn desired_vcol(&self) -> usize {
            self.caret.dcol as usize
        }

        pub fn desired_anchor_vcol(&self) -> Option<usize> {
            self.anchor.map(|a| a.dcol as usize)
        }

        /// Returns the range between `target` and `anchor`.
        ///
        /// If `anchor` isn't set, returns an empty range on `target`.
        ///
        /// A [`Cursor`]'s range will also never include the last
        /// character in a [`Text`], which must be a newline.
        ///
        /// # Warning
        ///
        /// This function will return the range that is supposed
        /// to be replaced, if `self.is_inclusive()`, this means that
        /// it will return one more byte at the end, i.e. start..=end.
        pub fn range(&self, is_inclusive: bool, text: &Text) -> Range<usize> {
            let anchor = self.anchor.unwrap_or(self.caret);
            let (start, end) = if anchor < self.caret {
                (anchor.point, self.caret.point)
            } else {
                (self.caret.point, anchor.point)
            };

            let last = text.last_point();
            if let Some(last) = last {
                let len = text.char_at(end).unwrap().len_utf8();
                // This is so the last byte (a '\n') is never included in any
                // selection.
                let include = is_inclusive && last.byte() > end.byte();
                start.byte()..end.byte() + len * include as usize
            } else {
                0..0
            }
        }

        /// The starting [`Point`] of this [`Cursor`]
        pub fn start(&self) -> Point {
            if let Some(anchor) = self.anchor {
                anchor.point.min(self.caret.point)
            } else {
                self.caret.point
            }
        }

        /// Returns the range between `target` and `anchor`.
        ///
        /// like [`Cursor::range`], this function will not include
        /// beyond the last character's [`Point`].
        ///
        /// If `anchor` isn't set, returns an empty range on `target`.
        pub fn point_range(&self, is_incl: bool, text: &Text) -> (Point, Point) {
            let anchor = self.anchor.unwrap_or(self.caret);
            let mut end = self.caret.point.max(anchor.point);
            if is_incl
                && end.byte() + 1 != text.len().byte()
                && let Some(char) = text.char_at(end)
            {
                end = end.fwd(char)
            }
            (self.caret.point.min(anchor.point), end)
        }

        /// Sets the desired visual column
        ///
        /// The desired visual column determines at what point in a
        /// line the caret will be placed when moving up and
        /// down through lines of varying lengths.
        pub fn set_desired_v_col(&mut self, x: usize) {
            self.caret.dcol = x as u32;
        }

        /// Sets the desired wrapped visual column
        ///
        /// The desired wrapped visual column determines at what point
        /// in a line the caret will be placed when moving up
        /// and down through wrapped lines of varying lengths.
        pub fn set_desired_wrapped_v_col(&mut self, x: usize) {
            self.caret.dwcol = x as u32;
        }

        pub(super) fn start_v(&self) -> VPoint {
            match self.anchor {
                Some(anchor) => self.caret.min(anchor),
                None => self.caret,
            }
        }

        pub(super) fn end_v(&self) -> VPoint {
            match self.anchor {
                Some(anchor) => self.caret.max(anchor),
                None => self.caret,
            }
        }
    }

    impl std::fmt::Display for Cursor {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}:{}, {}",
                self.caret.line() + 1,
                self.caret.vcol() + 1,
                self.caret.dcol
            )
        }
    }

    #[derive(Default, Clone, Copy, Eq, Debug, Serialize, Deserialize)]
    pub struct VPoint {
        point: Point,
        vcol: u32,
        dcol: u32,
        dwcol: u32,
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
        fn new(point: Point, text: &Text, area: &impl Area, cfg: PrintCfg) -> Self {
            let cfg = IterCfg::new(cfg);
            let dwcol = vcol(point, text, area, cfg);
            let vcol = vcol(point, text, area, cfg.dont_wrap());
            Self { point, vcol, dcol: vcol, dwcol }
        }

        pub fn byte(&self) -> usize {
            self.point.byte()
        }

        pub fn char(&self) -> usize {
            self.point.char()
        }

        pub fn line(&self) -> usize {
            self.point.line()
        }

        pub fn vcol(&self) -> usize {
            self.vcol as usize
        }
    }

    fn vcol(point: Point, text: &Text, area: &impl Area, cfg: IterCfg) -> u32 {
        if let Some(after) = text.points_after(point) {
            area.rev_print_iter(text.iter_rev(after), cfg)
                .find_map(|(caret, item)| item.part.is_char().then_some(caret.x))
                .unwrap_or(0)
        } else {
            area.rev_print_iter(text.iter_rev(text.len()), cfg)
                .find_map(|(caret, item)| item.part.is_char().then_some(caret.x + caret.len))
                .unwrap_or(0)
        }
    }
}

/// Binary searching by keys for [`GapBuffer`]s
fn binary_search_by_key<K>(
    buf: gapbuf::Range<Cursor>,
    key: K,
    f: impl Fn(&Cursor) -> K,
) -> Result<usize, usize>
where
    K: PartialEq + Eq + PartialOrd + Ord,
{
    let mut size = buf.len();
    let mut left = 0;
    let mut right = size;

    while left < right {
        let mid = left + size / 2;

        let k = f(&buf[mid]);

        match k.cmp(&key) {
            std::cmp::Ordering::Less => left = mid + 1,
            std::cmp::Ordering::Equal => return Ok(mid),
            std::cmp::Ordering::Greater => right = mid,
        }

        size = right - left;
    }

    Err(left)
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
