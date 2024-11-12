use std::sync::{
    Arc,
    atomic::{AtomicU32, Ordering},
};

use duat_core::ui::Axis;

use crate::{area::Coord, print::VarPoint};

/// What type of line should separate widgets
#[derive(Default, Clone, Copy, Debug)]
pub enum Brush {
    /// Uses `─`, `│`, `┐`
    #[default]
    Regular,
    /// Uses `━`, `┃`, `┓`
    Thick,
    /// Uses `╌`, `╎`, `┐`
    Dashed,
    /// Uses `╍`, `╏`, `┓`
    ThickDashed,
    /// Uses `═`, `║`, `╗`
    Double,
    /// Uses `─`, `│`, `╮`
    Rounded,
    /// Uses `-`, `|`, `+`
    Ascii,
    /// Uses `char` for all positions
    Custom(char),
}

/// Details of the right/bottom edge of a widget
#[derive(Debug)]
pub struct Edge {
    pub width: Arc<AtomicU32>,
    lhs: VarPoint,
    rhs: VarPoint,
    axis: Axis,
    fr: Frame,
}

impl Edge {
    /// Returns a new instance of [`Edge`].
    pub fn new(
        width: &Arc<AtomicU32>,
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
    fn new(tl: Coord, br: Coord, axis: Axis, line: Option<Brush>) -> Self {
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

/// Where to apply a [`Brush`] around widgets
///
/// This type serves to determine whether frames will be applied only
/// between widgets, around the whole application, or not at all:
///
/// - [`Empty`]: Do not frame at all.
/// - [`Surround`]: Frame on all sides.
/// - [`Border`]: Frame only around files.
/// - [`Vertical`]: Like [`Surround`], but only on vertical lines.
/// - [`VerBorder`]: Like [`Border`], but only on vertical lines.
/// - [`Horizontal`]: Like [`Surround`], but only on horizontal lines.
/// - [`HorBorder`]: Like [`Border`], but only on horizontal lines.
///
/// [`Empty`]: Frame::Empty
/// [`Surround`]: Frame::Surround
/// [`Border`]: Frame::Border
/// [`Vertical`]: Frame::Vertical
/// [`VerBorder`]: Frame::VerBorder
/// [`Horizontal`]: Frame::Horizontal
/// [`HorBorder`]: Frame::HorBorder
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
    /// Same as [`files_edges`], but on only one [`Axis`]
    ///
    /// [`files_edges`]: Self::files_edges
    pub(crate) fn files_edge_on(&self, axis: Axis) -> f64 {
        let (hor_fr, ver_fr) = self.files_edges();
        match axis {
            Axis::Horizontal => hor_fr,
            Axis::Vertical => ver_fr,
        }
    }

    /// Same as [`border_edges`], but on only one [`Axis`]
    ///
    /// [`border_edges`]: Self::border_edges
    pub(crate) fn border_edge_on(&self, axis: Axis) -> f64 {
        let (hor_fr, ver_fr) = self.border_edges();
        match axis {
            Axis::Horizontal => hor_fr,
            Axis::Vertical => ver_fr,
        }
    }

    /// The [`Brush`] in use, [`None`] for [`Frame::Empty`]
    fn brush(&self) -> Option<Brush> {
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

    /// The edges below and to the right of [`File`] regions
    ///
    /// [`File`]: duat_core::widgets::File
    fn files_edges(&self) -> (f64, f64) {
        match self {
            Self::Surround(_) => (1.0, 1.0),
            Self::Vertical(_) => (1.0, 0.0),
            Self::Horizontal(_) => (0.0, 1.0),
            _ => (0.0, 0.0),
        }
    }

    /// The edges below and to the right of the [`File`]s region
    ///
    /// [`File`]: duat_core::widgets::File
    fn border_edges(&self) -> (f64, f64) {
        match self {
            Self::Surround(_) | Self::Border(_) => (1.0, 1.0),
            Self::Vertical(_) | Self::VerBorder(_) => (1.0, 0.0),
            Self::Horizontal(_) | Self::HorBorder(_) => (0.0, 1.0),
            Self::Empty => (0.0, 0.0),
        }
    }
}
