use std::ops::Range;

use crate::{
    text::{IterCfg, Point, PrintCfg, Text},
    ui::Area,
};

/// A cursor in the text file. This is an editing cursor, not a
/// printing cursor.
#[derive(Default, Debug)]
pub struct Cursor {
    /// Current position of the cursor in the file.
    caret: VPoint,

    /// An anchor for a selection.
    anchor: Option<VPoint>,

    /// The index to a `Change` in the current `Moment`, used for
    /// greater efficiency.
    pub(crate) assoc_index: Option<usize>,
}

impl Cursor {
    /// Returns a new instance of [`Cursor`].
    pub fn new(point: Point, text: &Text, area: &impl Area, cfg: &PrintCfg) -> Cursor {
        let cfg = IterCfg::new(cfg).dont_wrap();
        Cursor {
            caret: VPoint::new(point, text, area, cfg),
            // This should be fine.
            anchor: None,
            assoc_index: None,
        }
    }

    /// Moves to specific, pre calculated [`Point`].
    pub fn move_to(&mut self, point: Point, text: &Text, area: &impl Area, cfg: &PrintCfg) {
        let cfg = IterCfg::new(cfg).dont_wrap();
        self.caret = VPoint::new(point, text, area, cfg);
    }

    /// Internal horizontal movement function.
    pub fn move_hor(&mut self, by: isize, text: &Text, area: &impl Area, cfg: &PrintCfg) {
        if by == 0 {
            return;
        }

        let point = text
            .point_at(self.caret.byte().saturating_add_signed(by))
            .unwrap_or(text.max_point());
        let cfg = IterCfg::new(cfg).dont_wrap();
        self.caret = VPoint::new(point, text, area, cfg);
    }

    /// Internal vertical movement function.
    pub fn move_ver(&mut self, by: isize, text: &Text, area: &impl Area, cfg: &PrintCfg) {
        let cfg = IterCfg::new(cfg).dont_wrap();

        if by == 0 {
            return;
        }
        let target = self.caret.line().saturating_add_signed(by);
        let dcol = self.caret.dcol;

        let point = if self.caret.line().saturating_add_signed(by) > text.len_lines() {
            text.max_point()
        } else if by > 0 {
            area.print_iter(text.iter_at(self.caret.point), cfg)
                .filter_map(|(caret, item)| Some(caret.x).zip(item.as_real_char()))
                .find_map(|(x, (point, char))| {
                    (point.line() == target && (x >= dcol || char == '\n')).then_some(point)
                })
                .unwrap_or(text.max_point())
        } else if self.caret.line().checked_add_signed(by).is_none() {
            Point::default()
        } else {
            let mut prev_point = self.caret.point;
            area.rev_print_iter(text.rev_iter_at(self.caret.point), cfg)
                .filter_map(|(caret, item)| Some(caret.x).zip(item.as_real_char()))
                .find_map(|(x, (point, _))| {
                    if point.line() == target && dcol > x {
                        Some(prev_point)
                    } else {
                        prev_point = point;
                        None
                    }
                })
                .unwrap_or(Point::default())
        };

        self.caret = VPoint::new(point, text, area, cfg);
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

    /// The byte (relative to the beginning of the file) of the caret.
    /// Indexed at 0.
    pub fn byte(&self) -> usize {
        self.caret.byte()
    }

    /// The char (relative to the beginning of the file) of the caret.
    /// Indexed at 0.
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
    pub fn range(&self) -> Range<usize> {
        let anchor = self.anchor.unwrap_or(self.caret);
        if anchor < self.caret {
            anchor.byte()..self.caret.byte()
        } else {
            self.caret.byte()..anchor.byte()
        }
    }

    /// Returns the range between `target` and `anchor`.
    ///
    /// If `anchor` isn't set, returns an empty range on `target`.
    pub fn point_range(&self) -> (Point, Point) {
        let anchor = self.anchor.unwrap_or(self.caret);
        (
            self.caret.point.min(anchor.point),
            self.caret.point.max(anchor.point),
        )
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

#[derive(Default, Debug, Clone, Copy, Eq, Ord)]
struct VPoint {
    point: Point,
    vcol: usize,
    dcol: usize,
}

impl PartialOrd for VPoint {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.point.cmp(&other.point))
    }
}

impl PartialEq for VPoint {
    fn eq(&self, other: &Self) -> bool {
        self.point == other.point
    }
}

impl VPoint {
    fn new(point: Point, text: &Text, area: &impl Area, cfg: IterCfg) -> Self {
        let vcol = vcol(point, text, area, cfg);
        Self {
            point,
            vcol,
            dcol: vcol,
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
    let iter = text.rev_iter_following(point);

    area.rev_print_iter(iter, cfg)
        .find_map(|(caret, item)| item.part.is_char().then_some(caret.x))
        .unwrap_or(0)
}
