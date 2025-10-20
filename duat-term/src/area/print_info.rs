use std::io::Write;

use duat_core::{
    context::{Decode, Encode},
    opts::PrintOpts,
    text::{Point, Text, TwoPoints},
    ui::Caret,
};

use crate::{
    Coords,
    area::iter::{print_iter, rev_print_iter},
};

/// Information about how to print the file on the `Label`.
#[derive(Default, Clone, Copy, PartialEq, Eq, Debug, Encode, Decode)]
#[bincode(crate = "duat_core::context::bincode")]
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
        cfg: PrintOpts,
    ) -> TwoPoints {
        self.prev_main = self.prev_main.min(text.last_point());

        let points = if let Some(s_points) = self.s_points
            && coords.width() == self.prev_coords.width()
            && coords.height() == self.prev_coords.height()
        {
            s_points
        } else if coords.width() > 0 && coords.height() > 0 {
            self.set_first_start(coords, text, cfg)
        } else {
            Default::default()
        };

        self.prev_coords = coords;

        points
    }

    /// The ending [`TwoPoints`] of the [`PrintInfo`]
    pub(super) fn end_points(&mut self, coords: Coords, text: &Text, cfg: PrintOpts) -> TwoPoints {
        self.prev_main = self.prev_main.min(text.last_point());

        let s_points = if let Some(s_points) = self.s_points
            && coords.width() == self.prev_coords.width()
            && coords.height() == self.prev_coords.height()
        {
            s_points
        } else if coords.width() > 0 && coords.height() > 0 {
            self.set_first_start(coords, text, cfg)
        } else {
            return Default::default();
        };

        let (points, mut y) = if let Some(main) = text.selections().get_main()
            && main.caret() == self.prev_main
        {
            (TwoPoints::new_after_ghost(self.prev_main), self.vert_dist)
        } else {
            (s_points, 0)
        };

        let mut iter = print_iter(
            text,
            points,
            cfg.wrap_width(coords.width()).unwrap_or(coords.width()),
            cfg,
        );

        self.prev_coords = coords;

        iter.find_map(|(Caret { wrap, .. }, item)| {
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
    pub(super) fn scroll_around(&mut self, p: Point, coords: Coords, text: &Text, cfg: PrintOpts) {
        self.prev_main = self.prev_main.min(text.last_point());

        if coords.width() > 0 && coords.height() > 0 {
            if let Some(s_points) = self.s_points
                && coords.width() == self.prev_coords.width()
                && coords.height() == self.prev_coords.height()
            {
                self.scroll_ver_around(p, coords, text, cfg, s_points);
            } else {
                self.prev_main = p;
                self.set_first_start(coords, text, cfg);
            }
            self.scroll_hor_around(p, coords.width(), text, cfg);
        }

        self.prev_coords = coords;
        self.prev_main = p;
    }

    /// Scrolls vertically
    pub(super) fn scroll_ver(&mut self, by: i32, coords: Coords, text: &Text, cfg: PrintOpts) {
        self.prev_main = self.prev_main.min(text.last_point());

        let s_points = if let Some(s_points) = self.s_points
            && coords.width() == self.prev_coords.width()
            && coords.height() == self.prev_coords.height()
        {
            s_points
        } else {
            let s_points = self.set_first_start(coords, text, cfg);
            self.s_points = Some(s_points);
            s_points
        };

        let cap = cfg.wrap_width(coords.width()).unwrap_or(coords.width());

        if by > 0 {
            let line_start = print_iter(text, s_points, cap, cfg)
                .filter_map(|(caret, item)| caret.wrap.then_some(item.points()))
                .take(by as usize + 1)
                .last()
                .unwrap_or_default();

            let max_s_points = max_s_points(text, cfg, coords.height(), cap);

            if line_start < max_s_points {
                self.s_points = Some(line_start);
            } else {
                self.s_points = Some(max_s_points);
            }
        } else {
            self.s_points = Some(
                rev_print_iter(text, s_points, cap, cfg)
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
        cfg: PrintOpts,
    ) {
        self.prev_main = self.prev_main.min(text.last_point());

        let cap = cfg.wrap_width(coords.width()).unwrap_or(coords.width());

        let line_start = rev_print_iter(text, points, cap, cfg)
            .filter_map(|(caret, item)| caret.wrap.then_some(item.points()))
            .next()
            .unwrap_or_default();

        let max_line_start = max_s_points(text, cfg, coords.height(), cap);

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
        cfg: PrintOpts,
        s_points: TwoPoints,
    ) {
        if self.prev_main == p {
            return;
        }

        let points = text.ghost_max_points_at(p.byte());
        let after = text
            .points_after(points)
            .unwrap_or_else(|| text.len_points());

        let cap = cfg.wrap_width(coords.width()).unwrap_or(coords.width());

        let mut below_dist = 0;
        let mut total_dist = 0;
        let mut iter = rev_print_iter(text, after, cap, cfg)
            .filter_map(|(caret, item)| caret.wrap.then_some(item.points()))
            .inspect(|points| {
                total_dist += 1;
                below_dist += (*points >= s_points) as u32;
            });

        let target = if self.prev_main > p {
            cfg.scrolloff.y as usize
        } else {
            coords.height().saturating_sub(cfg.scrolloff.y as u32 + 1) as usize
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
        let cap = opts.wrap_width(width).unwrap_or(width);
        // Quick shortcut to avoid iteration.
        if cap <= width {
            self.x_shift = 0;
            return;
        }

        let (max_shift, start, end) = {
            let points = text.ghost_max_points_at(p.byte());
            let after = text
                .points_after(points)
                .unwrap_or_else(|| text.len_points());

            let mut iter = rev_print_iter(text, after, cap, opts);

            let (points, start, end) = iter
                .find_map(|(Caret { x, len, .. }, item)| {
                    let points = item.points();
                    item.part.as_char().and(Some((points, x, x + len)))
                })
                .unwrap_or((TwoPoints::default(), 0, 0));

            let line_len = print_iter(text, points, cap, opts)
                .take_while(|(caret, item)| !caret.wrap || item.points() == points)
                .last()
                .map(|(Caret { x, len, .. }, _)| x + len)
                .unwrap_or(0);

            (line_len, start, end)
        };

        self.x_shift = self
            .x_shift
            .min(start.saturating_sub(opts.scrolloff.x as u32))
            .max(if opts.force_scrolloff {
                (end + opts.scrolloff.x as u32).saturating_sub(width)
            } else {
                (end + opts.scrolloff.x as u32)
                    .min(max_shift)
                    .saturating_sub(width)
            });

        duat_core::context::debug!("{self.x_shift}");
    }

    /// Sets and returns the first [`TwoPoints`]
    fn set_first_start(&mut self, coords: Coords, text: &Text, cfg: PrintOpts) -> TwoPoints {
        let cap = cfg.wrap_width(coords.width());

        let points = text.ghost_max_points_at(self.prev_main.byte());
        let after = text
            .points_after(points)
            .unwrap_or_else(|| text.len_points());

        let mut lines_traversed: u32 = 0;
        let points = rev_print_iter(text, after, cap.unwrap_or(coords.width()), cfg)
            .filter_map(|(caret, item)| caret.wrap.then_some(item.points()))
            .inspect(|_| lines_traversed += 1)
            .nth(
                self.vert_dist
                    .max(cfg.scrolloff.y as u32)
                    .min(coords.height().saturating_sub(cfg.scrolloff.y as u32 + 1))
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

fn max_s_points(text: &Text, cfg: PrintOpts, height: u32, cap: u32) -> TwoPoints {
    rev_print_iter(text, text.len_points(), cap, cfg)
        .filter_map(|(caret, item)| caret.wrap.then_some(item.points()))
        .nth(if cfg.allow_overscroll {
            cfg.scrolloff.y.saturating_sub(1) as usize
        } else {
            height.saturating_sub(1) as usize
        })
        .unwrap_or_default()
}
