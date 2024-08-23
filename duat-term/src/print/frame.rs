use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
};

use duat_core::ui::Axis;

use crate::{area::Coord, print::VarPoint};

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
    pub width: Arc<AtomicUsize>,
    lhs: VarPoint,
    rhs: VarPoint,
    axis: Axis,
    fr: Frame,
}

impl Edge {
    /// Returns a new instance of [`Edge`].
    pub fn new(
        width: &Arc<AtomicUsize>,
        lhs: &VarPoint,
        rhs: &VarPoint,
        axis: Axis,
        fr: Frame,
    ) -> Self {
        Self {
            width: width.clone(),
            lhs: lhs.clone(),
            rhs: rhs.clone(),
            axis,
            fr,
        }
    }

    /// The [`Coords`] that will be used to draw the line.
    pub fn edge_coords(&self) -> Option<EdgeCoords> {
        if self.width.load(Ordering::Acquire) == 0 {
            return None;
        }

        let start = match self.axis {
            Axis::Horizontal => Coord::new(self.rhs.x().value(), self.lhs.y().value()),
            Axis::Vertical => Coord::new(self.lhs.x().value(), self.rhs.y().value()),
        };
        let end = match self.axis {
            Axis::Horizontal => Coord::new(self.lhs.x().value(), self.rhs.y().value()),
            Axis::Vertical => Coord::new(self.rhs.x().value(), self.lhs.y().value()),
        };

        Some(EdgeCoords::new(start, end, self.axis, self.fr.brush()))
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
    pub fn crossing(&self, other: EdgeCoords) -> Option<(Coord, [Option<Brush>; 4])> {
        if let Axis::Vertical = self.axis {
            if let Axis::Vertical = other.axis
                && self.br.x == other.tl.x
                && self.br.y + 2 == other.tl.y
            {
                let coord = Coord::new(self.br.x, self.br.y + 1);
                return Some((coord, [None, self.line, None, other.line]));
            } else {
                return other.crossing(*self);
            }
        }

        if let Axis::Horizontal = other.axis {
            if self.br.y == other.br.y && self.br.x + 2 == other.tl.x {
                let coord = Coord::new(self.br.x + 1, self.br.y);
                Some((coord, [other.line, None, self.line, None]))
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
                let coord = Coord { x: other.tl.x, y: self.tl.y };

                Some((coord, [right, up, left, down]))
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
/// - [`Border`]: Frame only when the [`Edge`] in question would
///   separate two [`Rect`]s (i.e. don't surround the application.
/// - [`Vertical`][Frame::Vertical]: Like [`Surround`], but only
///   applies vertical lines.
/// - [`VerBorder`][Frame::VerBorder]: Like [`Border`], but only
///   applies vertical lines.
/// - [`Horizontal`][Frame::Horizontal]: Like [`Surround`], but only
///   applies horizontal lines.
/// - [`HorBorder`][Frame::HorBorder]: Like [`Border`], but only
///   applies horizontal lines.
///
/// [`Surround`]: Frame::Surround
/// [`Border`]: Frame::Border
#[derive(Clone, Copy, Debug)]
pub enum Frame {
    /// No frame
    Empty,
    /// Frame the window's edges and borders between widgets
    Surround(Brush),
    /// Frame borders between widgets
    Border(Brush),
    /// Frame vertical window edges and borders between widgets
    Vertical(Brush),
    /// Frame vertical borders between widgets
    VerBorder(Brush),
    /// Frame horizontal window edges and borders between widgets
    Horizontal(Brush),
    /// Frame horizontal borders between widgets
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
    pub fn files_edges(&self) -> (f64, f64) {
        match self {
            Self::Surround(_) => (1.0, 1.0),
            Self::Vertical(_) => (1.0, 0.0),
            Self::Horizontal(_) => (0.0, 1.0),
            _ => (0.0, 0.0),
        }
    }

    pub fn border_edges(&self) -> (f64, f64) {
        match self {
            Self::Surround(_) | Self::Border(_) => (1.0, 1.0),
            Self::Vertical(_) | Self::VerBorder(_) => (1.0, 0.0),
            Self::Horizontal(_) | Self::HorBorder(_) => (0.0, 1.0),
            Self::Empty => (0.0, 0.0),
        }
    }

    pub fn border_edge_on(&self, axis: Axis) -> f64 {
        let (hor_fr, ver_fr) = self.border_edges();
        match axis {
            Axis::Horizontal => hor_fr,
            Axis::Vertical => ver_fr,
        }
    }

    pub fn files_egde_on(&self, axis: Axis) -> f64 {
        let (hor_fr, ver_fr) = self.files_edges();
        match axis {
            Axis::Horizontal => hor_fr,
            Axis::Vertical => ver_fr,
        }
    }

    /// The [`Line`] of [`self`], which is [`None`] in the
    /// [`Self::Empty`] case.
    pub fn brush(&self) -> Option<Brush> {
        match self {
            Self::Empty => None,
            Self::Surround(brush)
            | Self::Border(brush)
            | Self::Vertical(brush)
            | Self::VerBorder(brush)
            | Self::Horizontal(brush)
            | Self::HorBorder(brush) => Some(*brush),
        }
    }
}
