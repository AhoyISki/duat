use std::sync::atomic::Ordering;

use parsec_core::ui::Axis;

use super::VarPoint;
use crate::area::Coord;

/// What type of line should be used to [`Frame`] a given [`Rect`].
#[derive(Default, Clone, Copy, Debug)]
pub enum Brush {
    #[default]
    Regular,
    Thick,
    Dashed,
    ThickDashed,
    Double,
    Rounded,
    Ascii,
    Custom(char),
}

/// Detailings of a framed side of a [`Rect`], automatically kept up
/// to date.
#[derive(Debug)]
pub struct Edge {
    center: VarPoint,
    target: VarPoint,
    axis: Axis,
    pub frame: Frame,
}

impl Edge {
    /// Returns a new instance of [`Edge`].
    pub(super) fn new(center: VarPoint, target: VarPoint, axis: Axis, frame: Frame) -> Self {
        Self {
            center,
            target,
            axis,
            frame,
        }
    }

    /// The [`Coords`] that will be used to draw the line.
    pub fn line_coords(&self) -> EdgeCoords {
        let shift = self.center < self.target;
        let (x_shift, y_shift) = match self.axis {
            Axis::Horizontal => (!shift as usize, shift as usize),
            Axis::Vertical => (shift as usize, !shift as usize),
        };

        let target = match self.axis {
            Axis::Horizontal => Coord::new(
                self.target.x.value.load(Ordering::Acquire),
                self.center.y.value.load(Ordering::Acquire),
            ),
            Axis::Vertical => Coord::new(
                self.center.x.value.load(Ordering::Acquire),
                self.target.y.value.load(Ordering::Acquire),
            ),
        };
        let center = Coord::new(
            self.center.x.value.load(Ordering::Acquire) - x_shift,
            self.center.y.value.load(Ordering::Acquire) - y_shift,
        );
        if self.center < self.target {
            EdgeCoords::new(center, target, self.axis, self.frame.line())
        } else {
            EdgeCoords::new(target, center, self.axis, self.frame.line())
        }
    }

    /// Checks if the [`VarPoint`]s of a given [`Rect`] match with the
    /// ones of [`self`].
    pub(super) fn matches_vars(&self, tl: &VarPoint, br: &VarPoint) -> bool {
        self.center == *tl && self.target == *br || self.center == *br && self.target == *tl
    }
}

#[derive(Clone, Copy)]
pub struct EdgeCoords {
    pub tl: Coord,
    pub br: Coord,
    pub axis: Axis,
    pub line: Option<Brush>,
}

impl EdgeCoords {
    pub fn new(tl: Coord, br: Coord, axis: Axis, line: Option<Brush>) -> Self {
        Self { tl, br, axis, line }
    }

	#[allow(clippy::type_complexity)]
    pub fn crossing(
        &self,
        other: EdgeCoords,
    ) -> Option<(
        Coord,
        Option<Brush>,
        Option<Brush>,
        Option<Brush>,
        Option<Brush>,
    )> {
        if let Axis::Vertical = self.axis {
            if let Axis::Vertical = other.axis
                && self.br.x == other.tl.x
                && self.br.y + 2 == other.tl.y
            {
                let coord = Coord::new(self.br.x, self.br.y + 1);
                return Some((coord, None, self.line, None, other.line));
            // All perpendicular crossings will be iterated, so if two
            // perpendicular `Coords`, `a` and `b` cross, `self` will
            // be horizontal in one of the iterations, while `other`
            // will be vertical, that's when the rest of the logic on
            // this function is used.
            } else {
                return None;
            }
        }

        if let Axis::Horizontal = other.axis {
            if self.br.y == other.br.y && self.br.x + 2 == other.tl.x {
                let coord = Coord::new(self.br.x + 1, self.br.y);
                Some((coord, other.line, None, self.line, None))
            } else {
                None
            }
        } else if self.tl.x <= other.tl.x + 1 && other.tl.x <= self.br.x + 1 {
            let right_height = (other.tl.y..=other.br.y + 2).contains(&(self.br.y + 1));
            let right = match right_height && other.br.x <= self.br.x {
                true => self.line,
                false => None,
            };
            let up = match other.tl.y <= self.tl.y && self.br.y <= other.br.y + 1 {
                true => other.line,
                false => None,
            };
            let left = match right_height && self.tl.x <= other.tl.x {
                true => self.line,
                false => None,
            };
            let down = match self.br.y <= other.br.y && other.tl.y <= self.tl.y + 1 {
                true => other.line,
                false => None,
            };

            if up.is_some() || down.is_some() {
                let coord = Coord {
                    x: other.tl.x,
                    y: self.tl.y,
                };

                Some((coord, right, up, left, down))
            } else {
                None
            }
        } else {
            None
        }
    }
}

/// Configuration about how frame a [`Rect`] with a given [`Line`].
///
/// The ways in which a [`Rect`] can be framed are as follows:
///
/// - [`Empty`][Frame::Empty]: Do not frame at all.
/// - [`Surround`]: Frame on all sides.
/// - [`Border`]: Frame only when the [`Edge`] in question would separate two
///   [`Rect`]s (i.e. don't surround the application.
/// - [`Vertical`][Frame::Vertical]: Like [`Surround`], but only applies
///   vertical lines.
/// - [`VerBorder`][Frame::VerBorder]: Like [`Border`], but only applies
///   vertical lines.
/// - [`Horizontal`][Frame::Horizontal]: Like [`Surround`], but only applies
///   horizontal lines.
/// - [`HorBorder`][Frame::HorBorder]: Like [`Border`], but only applies
///   horizontal lines.
///
/// [`Surround`]: Frame::Surround
/// [`Border`]: Frame::Border
#[derive(Clone, Copy, Debug)]
pub enum Frame {
    Empty,
    Surround(Brush),
    Border(Brush),
    Vertical(Brush),
    VerBorder(Brush),
    Horizontal(Brush),
    HorBorder(Brush),
}

impl Default for Frame {
    fn default() -> Self {
        Self::Border(Brush::Regular)
    }
}

impl Frame {
    /// Assuming that the [`Rect`] in question is the main [`Rect`],
    /// determine which sides are supposed to be framed.
    pub fn main_edges(&self) -> (f64, f64) {
        match self {
            Frame::Surround(_) => (1.0, 1.0),
            Frame::Vertical(_) => (1.0, 0.0),
            Frame::Horizontal(_) => (0.0, 1.0),
            _ => (0.0, 0.0),
        }
    }

    /// The [`Line`] of [`self`], which is [`None`] in the
    /// [`Frame::Empty`] case.
    pub fn line(&self) -> Option<Brush> {
        match self {
            Frame::Empty => None,
            Frame::Surround(line)
            | Frame::Border(line)
            | Frame::Vertical(line)
            | Frame::VerBorder(line)
            | Frame::Horizontal(line)
            | Frame::HorBorder(line) => Some(*line),
        }
    }

    /// Given a [`Rect`]'s position and size, and the maximum
    /// allowable [`Coord`], determines which sides are supposed to be
    /// framed.
    pub(super) fn edges(&self, br: &VarPoint, tl: &VarPoint, max: Coord) -> (f64, f64, f64, f64) {
        let right = br.x.value.load(Ordering::Acquire) == max.x;
        let up = tl.y.value.load(Ordering::Acquire) == 0;
        let left = tl.x.value.load(Ordering::Acquire) == 0;
        let down = br.y.value.load(Ordering::Acquire) == max.y;

        let (up, left) = match self {
            Frame::Surround(_) => (up as usize as f64, left as usize as f64),
            Frame::Vertical(_) => (0.0, left as usize as f64),
            Frame::Horizontal(_) => (up as usize as f64, 0.0),
            _ => (0.0, 0.0),
        };

        let (down, right) = match self {
            Frame::Surround(_) => (1.0, 1.0),
            Frame::Vertical(_) => (0.0, 1.0),
            Frame::Horizontal(_) => (1.0, 0.0),
            Frame::Border(_) => (!down as usize as f64, !right as usize as f64),
            Frame::VerBorder(_) => (0.0, !right as usize as f64),
            Frame::HorBorder(_) => (!down as usize as f64, 0.0),
            Frame::Empty => (0.0, 0.0),
        };

        (right, up, left, down)
    }
}
