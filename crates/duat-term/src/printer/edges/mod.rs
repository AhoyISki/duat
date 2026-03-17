use duat_core::ui::Axis;

use super::{VarPoint, variables::Variables};
use crate::area::Coord;

mod combinations;

pub use combinations::*;

/// What type of line should separate widgets
#[derive(Default, Clone, Copy, Debug)]
pub enum BorderStyle {
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
    lhs: VarPoint,
    rhs: VarPoint,
    axis: Axis,
    fr: Border,
}

impl Edge {
    /// Returns a new instance of [`Edge`].
    pub fn new(lhs: VarPoint, rhs: VarPoint, axis: Axis, fr: Border) -> Self {
        Self { lhs, rhs, axis, fr }
    }

    /// The [`Coords`] that will be used to draw the line.
    pub fn coords(&self, vars: &mut Variables) -> Option<EdgeCoords> {
        let (lhs, _) = vars.coord(self.lhs, false);
        let (rhs, _) = vars.coord(self.rhs, false);
        if lhs.x == rhs.x || lhs.y == rhs.y {
            return None;
        }

        let start = match self.axis {
            Axis::Horizontal => Coord::new(rhs.x, lhs.y),
            Axis::Vertical => Coord::new(lhs.x, rhs.y),
        };
        let end = match self.axis {
            Axis::Horizontal => Coord::new(lhs.x, rhs.y),
            Axis::Vertical => Coord::new(rhs.x, lhs.y),
        };

        Some(EdgeCoords::new(start, end, self.axis, self.fr.style()))
    }
}

#[derive(Clone, Copy)]
pub struct EdgeCoords {
    pub tl: Coord,
    pub br: Coord,
    pub axis: Axis,
    pub line: Option<BorderStyle>,
}

impl EdgeCoords {
    fn new(tl: Coord, br: Coord, axis: Axis, line: Option<BorderStyle>) -> Self {
        Self { tl, br, axis, line }
    }

    pub fn crossing(&self, other: EdgeCoords) -> Option<(Coord, [Option<BorderStyle>; 4])> {
        if let Axis::Vertical = self.axis {
            if let Axis::Vertical = other.axis {
                if self.br.x == other.tl.x && self.br.y + 2 == other.tl.y {
                    let coord = Coord::new(self.br.x, self.br.y + 1);
                    return Some((coord, [None, self.line, None, other.line]));
                } else {
                    return None;
                }
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

/// Where to apply a [`BorderStyle`] around widgets
///
/// This type serves to determine whether frames will be applied only
/// between widgets, around the whole application, or not at all:
///
/// - [`Empty`]: Do not frame at all.
/// - [`Surround`]: Border on all sides.
/// - [`Border`]: Border only around files.
/// - [`Vertical`]: Like [`Surround`], but only on vertical lines.
/// - [`VerBorder`]: Like [`Border`], but only on vertical lines.
/// - [`Horizontal`]: Like [`Surround`], but only on horizontal lines.
/// - [`HorBorder`]: Like [`Border`], but only on horizontal lines.
///
/// [`Empty`]: Border::Empty
/// [`Surround`]: Border::Surround
/// [`Border`]: Border::Border
/// [`Vertical`]: Border::Vertical
/// [`VerBorder`]: Border::VerBorder
/// [`Horizontal`]: Border::Horizontal
/// [`HorBorder`]: Border::HorBorder
#[derive(Clone, Copy, Debug)]
pub enum Border {
    /// No frame
    Empty,
    /// Border the window's edges and borders between widgets
    Surround(BorderStyle),
    /// Border borders between widgets
    Border(BorderStyle),
    /// Border vertical window edges and borders between widgets
    Vertical(BorderStyle),
    /// Border vertical borders between widgets
    VerBorder(BorderStyle),
    /// Border horizontal window edges and borders between widgets
    Horizontal(BorderStyle),
    /// Border horizontal borders between widgets
    HorBorder(BorderStyle),
}

impl Default for Border {
    fn default() -> Self {
        Self::Border(BorderStyle::Regular)
    }
}

impl Border {
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
        let (hor_border, ver_border) = self.border_edges();
        match axis {
            Axis::Horizontal => hor_border,
            Axis::Vertical => ver_border,
        }
    }

    /// The [`BorderStyle`] in use, [`None`] for [`Border::Empty`]
    fn style(&self) -> Option<BorderStyle> {
        match self {
            Self::Empty => None,
            Self::Surround(style)
            | Self::Border(style)
            | Self::Vertical(style)
            | Self::VerBorder(style)
            | Self::Horizontal(style)
            | Self::HorBorder(style) => Some(*style),
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
