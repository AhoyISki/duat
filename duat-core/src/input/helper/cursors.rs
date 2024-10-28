use gapbuf::{gap_buffer, GapBuffer};
use serde::{de::Visitor, ser::SerializeSeq, Deserialize, Serialize};

pub use self::cursor::Cursor;
use super::Diff;
use crate::{
    text::{Point, PrintCfg, Text},
    ui::Area,
};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Cursors {
    buf: CursorGapBuffer,
    main: usize,
    inclusive_ranges: bool,
}

impl Cursors {
    pub fn new_exclusive() -> Self {
        Self {
            buf: CursorGapBuffer(gap_buffer![Cursor::new_at_0(false)]),
            main: 0,
            inclusive_ranges: false,
        }
    }

    pub fn new_inclusive() -> Self {
        Self {
            buf: CursorGapBuffer(gap_buffer![Cursor::new_at_0(true)]),
            main: 0,
            inclusive_ranges: true,
        }
    }

    pub fn make_exclusive(&mut self) {
        self.inclusive_ranges = false;
        for cursor in self.buf.iter_mut() {
            cursor.set_inclusivity(false);
        }
    }

    pub fn make_inclusive(&mut self) {
        self.inclusive_ranges = true;
        for cursor in self.buf.iter_mut() {
            cursor.set_inclusivity(true);
        }
    }

    pub fn insert_from_parts(
        &mut self,
        point: Point,
        range: usize,
        text: &Text,
        area: &impl Area,
        cfg: &PrintCfg,
    ) -> usize {
        let mut cursor = Cursor::new(point, text, area, cfg, self.inclusive_ranges);

        let range = match self.inclusive_ranges {
            true => range.saturating_sub(1),
            false => range,
        };

        if range > 0 {
            cursor.set_anchor();
            cursor.move_hor(range as isize, text, area, cfg);
        }
        let start = cursor.range().start;
        let (Ok(i) | Err(i)) = binary_search_by_key(&self.buf, start, |c| c.range().start);

        if !self.try_merge_on(i, &mut cursor) {
            self.buf.insert(i, cursor);
        }

        i
    }

    pub fn rotate_main_fwd(&mut self) {
        match self.main == self.buf.len() - 1 {
            true => self.main = 0,
            false => self.main += 1,
        }
    }

    pub fn rotate_main_rev(&mut self) {
        match self.main == 0 {
            true => self.main = self.buf.len() - 1,
            false => self.main -= 1,
        }
    }

    pub fn remove_extras(&mut self) {
        let cursor = self.buf[self.main].clone();
        self.buf = CursorGapBuffer(gap_buffer![cursor]);
        self.main = 0;
    }

    pub fn main(&self) -> &Cursor {
        &self.buf[self.main]
    }

    pub fn get(&self, i: usize) -> Option<Cursor> {
        self.buf.get(i).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Cursor, bool)> {
        self.buf
            .iter()
            .enumerate()
            .map(move |(index, cursor)| (cursor, index == self.main))
    }

    pub fn main_index(&self) -> usize {
        self.main
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_inclusive(&self) -> bool {
        self.inclusive_ranges
    }

    pub fn reset(&mut self) {
        self.buf = CursorGapBuffer(gap_buffer![Cursor::new_at_0(self.inclusive_ranges)])
    }

    pub(crate) fn clear(&mut self) {
        self.buf.clear()
    }

    pub(super) fn remove(&mut self, i: usize) -> Option<(Cursor, bool)> {
        (i < self.buf.len()).then(|| {
            let was_main = self.main == i;
            if self.main > i {
                self.main -= 1;
            }
            (self.buf.remove(i), was_main)
        })
    }

    pub(super) fn insert_removed(&mut self, was_main: bool, mut cursor: Cursor) -> usize {
        let start = cursor.range().start;
        let (Ok(i) | Err(i)) = binary_search_by_key(&self.buf, start, |c| c.range().start);

        if was_main {
            self.main = i;
        }

        if self.try_merge_on(i, &mut cursor) {
            i - 1
        } else {
            self.buf.insert(i, cursor);
            if self.main > i {
                self.main += 1;
            }
            i
        }
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
            for cursor in self.buf.iter_mut().skip(after + 1) {
                diff.shift_cursor(cursor, text, area, cfg);
            }
        }
    }

    pub(super) fn drain(&mut self) -> impl Iterator<Item = (Cursor, bool)> + '_ {
        let orig_main = self.main;
        self.main = 0;
        self.buf
            .drain(..)
            .enumerate()
            .map(move |(i, c)| match i == orig_main {
                true => (c, true),
                false => (c, false),
            })
    }

    /// Tries to merge this cursor with a cursor behind and cursors
    /// ahead
    ///
    /// Returns `true` if the cursor behind got merged.
    fn try_merge_on(&mut self, i: usize, cursor: &mut Cursor) -> bool {
        while let Some(ahead) = self.buf.get(i)
            && cursor.range().end > ahead.range().start
        {
            cursor.merge_ahead(self.buf.remove(i));
            if self.main > i {
                self.main -= 1;
            }
        }
        if let Some(prev_i) = i.checked_sub(1)
            && let Some(prev) = self.buf.get_mut(prev_i)
            && prev.range().end > cursor.range().start
        {
            prev.merge_ahead(cursor.clone());
            if self.main > prev_i {
                self.main -= 1;
            }
            true
        } else {
            false
        }
    }
}

impl Default for Cursors {
    fn default() -> Self {
        Self {
            buf: CursorGapBuffer(gap_buffer![Cursor::new_at_0(false)]),
            main: 0,
            inclusive_ranges: false,
        }
    }
}

mod cursor {
    use std::ops::Range;

    use serde::{Deserialize, Serialize};

    use crate::{
        text::{IterCfg, Point, PrintCfg, Text},
        ui::{Area, Caret},
    };

    /// A cursor in the text file. This is an editing cursor, not
    /// a printing cursor.
    #[derive(Debug, Serialize, Deserialize)]
    pub struct Cursor {
        /// Current position of the cursor in the file.
        caret: VPoint,

        /// An anchor for a selection.
        anchor: Option<VPoint>,

        /// Wether or not the selection of this cursor is
        /// inclusive or not.
        is_inclusive: bool,

        /// The index to a `Change` in the current `Moment`, used
        /// for greater efficiency.
        pub(crate) assoc_index: Option<usize>,
    }

    impl Cursor {
        pub fn new_at_0(inclusive: bool) -> Self {
            Self {
                is_inclusive: inclusive,
                caret: VPoint::default(),
                anchor: None,
                assoc_index: None,
            }
        }

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

        /// Moves to specific, pre calculated [`Point`].
        pub fn move_to(&mut self, point: Point, text: &Text, area: &impl Area, cfg: &PrintCfg) {
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

        pub(super) fn set_inclusivity(&mut self, inclusive: bool) {
            self.is_inclusive = inclusive;
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
        pub fn column(&self) -> usize {
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

            if let Some(anchor) = self.anchor {
                if anchor.point > self.caret() {
                    self.anchor = Some(other_end.max(anchor));
                } else {
                    self.caret = other_end.max(self.caret)
                }
            } else {
                self.anchor = Some(other_end);
            }
        }
    }

    impl std::fmt::Display for Cursor {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}:{}, {}", self.caret.line() + 1, self.caret.vcol() + 1, self.caret.dcol)
        }
    }

    impl Clone for Cursor {
        fn clone(&self) -> Self {
            Cursor { assoc_index: None, ..*self }
        }
    }

    #[derive(Default, Debug, Clone, Copy, Eq, Serialize, Deserialize)]
    pub struct VPoint {
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
            Self { point, vcol, dcol: vcol, dwcol }
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

/// Binary searching by keys for [`GapBuffer`]s
fn binary_search_by_key<K>(
    buf: &GapBuffer<Cursor>,
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
