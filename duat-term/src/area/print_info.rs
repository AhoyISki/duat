use duat_core::{
    context::cache::{Decode, Encode},
    opts::PrintOpts,
    text::{Point, Text, TwoPoints},
};

use crate::{
    Coords,
    area::iter::{PrintedPlace, is_starting_points, print_iter, rev_print_iter},
};

/// Information about how to print the file on the `Label`.
#[derive(Default, Clone, Copy, PartialEq, Eq, Debug, Encode, Decode)]
#[bincode(crate = "duat_core::context::cache::bincode")]
pub struct PrintInfo {
    s_points: Option<TwoPoints>,
    x_shift: u32,
    prev_main: Point,
    prev_coords: Coords,
    vert_dist: u32,
}

impl PrintInfo {
    /// The starting [`TwoPoints`] of the [`PrintInfo`]
    pub(super) fn start_points(
        &mut self,
        coords: Coords,
        text: &Text,
        opts: PrintOpts,
    ) -> TwoPoints {
        self.prev_main = self.prev_main.min(text.last_point());

        let points = if let Some(s_points) = self.s_points
            && s_points <= text.len_points()
            && coords.width() == self.prev_coords.width()
            && coords.height() == self.prev_coords.height()
            && is_starting_points(text, s_points, coords.width(), opts)
        {
            s_points
        } else if coords.width() > 0 && coords.height() > 0 {
            self.set_first_start(coords, text, opts)
        } else {
            Default::default()
        };

        self.prev_coords = coords;

        points
    }

    /// The ending [`TwoPoints`] of the [`PrintInfo`]
    pub(super) fn end_points(&mut self, coords: Coords, text: &Text, opts: PrintOpts) -> TwoPoints {
        self.prev_main = self.prev_main.min(text.last_point());

        let points = if let Some(s_points) = self.s_points
            && coords.width() == self.prev_coords.width()
            && coords.height() == self.prev_coords.height()
        {
            s_points
        } else if coords.width() > 0 && coords.height() > 0 {
            self.set_first_start(coords, text, opts)
        } else {
            return Default::default();
        };

        let mut y = 0;

        let mut iter = print_iter(text, points, coords.width(), opts);

        self.prev_coords = coords;

        iter.find_map(|(PrintedPlace { wrap, .. }, item)| {
            y += (wrap && item.part.is_char()) as u32;
            (y > coords.height()).then_some(item.points())
        })
        .unwrap_or_else(|| text.len_points())
    }

    /// Prepares this [`PrintInfo`] for caching
    pub(super) fn for_caching(self) -> Self {
        Self { s_points: None, ..self }
    }

    /// Scrolls around a given [`Point`]
    pub(super) fn scroll_around(&mut self, p: Point, coords: Coords, text: &Text, opts: PrintOpts) {
        self.prev_main = self.prev_main.min(text.last_point());

        if coords.width() > 0 && coords.height() > 0 {
            if let Some(s_points) = self.s_points
                && coords.width() == self.prev_coords.width()
                && coords.height() == self.prev_coords.height()
            {
                self.scroll_ver_around(p, coords, text, opts, s_points);
            } else {
                self.prev_main = p;
                self.set_first_start(coords, text, opts);
            }
            self.scroll_hor_around(p, coords.width(), text, opts);
        }

        self.prev_coords = coords;
        self.prev_main = p;
    }

    /// Scrolls vertically
    pub(super) fn scroll_ver(&mut self, by: i32, coords: Coords, text: &Text, opts: PrintOpts) {
        self.prev_main = self.prev_main.min(text.last_point());

        let s_points = if let Some(s_points) = self.s_points
            && coords.width() == self.prev_coords.width()
            && coords.height() == self.prev_coords.height()
        {
            s_points
        } else {
            let s_points = self.set_first_start(coords, text, opts);
            self.s_points = Some(s_points);
            s_points
        };

        if by > 0 {
            let line_start = print_iter(text, s_points, coords.width(), opts)
                .filter_map(|(caret, item)| caret.wrap.then_some(item.points()))
                .take(by as usize + 1)
                .last()
                .unwrap_or_default();

            let cap = opts.wrap_width(coords.width()).unwrap_or(coords.width());
            let max_s_points = max_s_points(text, opts, coords.height(), cap);

            if line_start < max_s_points {
                self.s_points = Some(line_start);
            } else {
                self.s_points = Some(max_s_points);
            }
        } else {
            self.s_points = Some(
                rev_print_iter(text, s_points, coords.width(), opts)
                    .filter_map(|(caret, item)| caret.wrap.then_some(item.points()))
                    .nth(by.unsigned_abs() as usize - 1)
                    .unwrap_or_default(),
            );
        }

        self.prev_coords = coords;
    }

    pub(super) fn scroll_to_points(
        &mut self,
        points: TwoPoints,
        coords: Coords,
        text: &Text,
        opts: PrintOpts,
    ) {
        self.prev_main = self.prev_main.min(text.last_point());

        let cap = opts.wrap_width(coords.width()).unwrap_or(coords.width());

        let line_start = rev_print_iter(text, points, cap, opts)
            .filter_map(|(caret, item)| caret.wrap.then_some(item.points()))
            .next()
            .unwrap_or_default();

        let max_line_start = max_s_points(text, opts, coords.height(), cap);

        if line_start < max_line_start {
            self.s_points = Some(line_start);
        } else {
            self.s_points = Some(max_line_start);
        }
    }

    /// Scrolls down or up until the gap between the main cursor and
    /// the bottom of the widget is equal to `config.scrolloff.y_gap`.
    fn scroll_ver_around(
        &mut self,
        p: Point,
        coords: Coords,
        text: &Text,
        opts: PrintOpts,
        s_points: TwoPoints,
    ) {
        if self.prev_main == p {
            return;
        }

        let points = text.ghost_max_points_at(p.min(text.len()).byte());
        let after = text
            .points_after(points)
            .unwrap_or_else(|| text.len_points());

        let cap = opts.wrap_width(coords.width()).unwrap_or(coords.width());

        let mut below_dist = 0;
        let mut total_dist = 0;
        let mut wrapped_char = false;
        let mut iter = rev_print_iter(text, after, cap, opts)
            .filter_map(|(caret, item)| {
                wrapped_char |= caret.wrap;
                (item.part.is_char() && wrapped_char).then(|| {
                    wrapped_char = false;
                    item.points()
                })
            })
            .inspect(|points| {
                total_dist += 1;
                below_dist += (*points >= s_points) as u32;
            });

        let target = if self.prev_main > p {
            opts.scrolloff.y as usize
        } else {
            coords.height().saturating_sub(opts.scrolloff.y as u32 + 1) as usize
        };
        let first = iter.nth(target).unwrap_or_default();

        if (self.prev_main > p && first <= s_points) || (self.prev_main < p && first >= s_points) {
            self.s_points = Some(first);
            self.vert_dist = total_dist - 1;
        } else {
            iter.take_while(|points| *points >= s_points)
                .for_each(|_| {});

            self.vert_dist = below_dist - 1;
        }
    }

    /// Scrolls the file horizontally, usually when no wrapping is
    /// being used.
    fn scroll_hor_around(&mut self, p: Point, width: u32, text: &Text, opts: PrintOpts) {
        // Quick shortcut to avoid iteration.
        if opts.wrap_width(width).is_some_and(|cap| cap <= width) {
            self.x_shift = 0;
            return;
        }

        let (max_shift, caret_start, caret_end) = {
            let points = text.ghost_max_points_at(p.min(text.len()).byte());
            let after = text
                .points_after(points)
                .unwrap_or_else(|| text.len_points());

            let mut iter = rev_print_iter(text, after, width, opts);

            let (points, caret_start, caret_end) = iter
                .find_map(|(PrintedPlace { x, len, .. }, item)| {
                    let points = item.points();
                    item.part.as_char().and(Some((points, x, x + len)))
                })
                .unwrap_or((TwoPoints::default(), 0, 0));

            let max_shift = print_iter(text, points, width, opts)
                .take_while(|(caret, item)| !caret.wrap || item.points() == points)
                .last()
                .map(|(PrintedPlace { x, len, .. }, _)| x + len)
                .unwrap_or(0);

            (max_shift, caret_start, caret_end)
        };

        self.x_shift = self
            .x_shift
            .min(caret_start.saturating_sub(opts.scrolloff.x as u32))
            .max(if opts.force_scrolloff {
                (caret_end + opts.scrolloff.x as u32).saturating_sub(width)
            } else {
                (caret_end + opts.scrolloff.x as u32)
                    .min(max_shift)
                    .saturating_sub(width)
            });
    }

    /// Sets and returns the first [`TwoPoints`]
    fn set_first_start(&mut self, coords: Coords, text: &Text, opts: PrintOpts) -> TwoPoints {
        let cap = opts.wrap_width(coords.width());

        let points = text.ghost_max_points_at(self.prev_main.min(text.len()).byte());
        let after = text
            .points_after(points)
            .unwrap_or_else(|| text.len_points());

        let mut lines_traversed: u32 = 0;
        let points = rev_print_iter(text, after, cap.unwrap_or(coords.width()), opts)
            .filter_map(|(caret, item)| caret.wrap.then_some(item.points()))
            .inspect(|_| lines_traversed += 1)
            .nth(
                self.vert_dist
                    .max(opts.scrolloff.y as u32)
                    .min(coords.height().saturating_sub(opts.scrolloff.y as u32 + 1))
                    as usize,
            )
            .unwrap_or_default();

        // We don't want to count the the main cursor's line's wrap.
        self.vert_dist = lines_traversed.saturating_sub(1);
        self.s_points = Some(points);

        points
    }

    pub fn x_shift(&self) -> u32 {
        self.x_shift
    }
}

fn max_s_points(text: &Text, opts: PrintOpts, height: u32, cap: u32) -> TwoPoints {
    rev_print_iter(text, text.len_points(), cap, opts)
        .filter_map(|(caret, item)| caret.wrap.then_some(item.points()))
        .nth(if opts.allow_overscroll {
            opts.scrolloff.y.saturating_sub(1) as usize
        } else {
            height.saturating_sub(1) as usize
        })
        .unwrap_or_default()
}
