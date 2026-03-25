use duat_core::ui::Axis;

use super::{VarPoint, variables::Variables};
use crate::area::Coord;

mod combinations;

pub use combinations::*;

/// What type of line should separate widgets.
#[derive(Default, Clone, Copy, Debug)]
pub enum BorderStyle {
    /// Uses `─`, `│`, `┐`.
    #[default]
    Regular,
    /// Uses `━`, `┃`, `┓`
    Thick,
    /// Uses `╌`, `╎`, `┐`.
    Dashed,
    /// Uses `╍`, `╏`, `┓`.
    ThickDashed,
    /// Uses `═`, `║`, `╗`.
    Double,
    /// Uses `─`, `│`, `╮`.
    Rounded,
    /// Uses `-`, `|`, `+`.
    Ascii,
    /// Uses `char` for all positions.
    Custom(char),
}

/// Details of the right/bottom edge of a widget.
#[derive(Debug, Clone)]
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

    /// The [`EdgeCoords`] that will be used to draw the line.
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

#[derive(Debug, Clone, Copy)]
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
    /// No border.
    Empty,
    /// Border on the window's edges and between buffer clusters.
    Surround(BorderStyle),
    /// Border between widgets.
    Border(BorderStyle),
    /// Border on vertical window edges and borders between buffer
    /// clusters.
    Vertical(BorderStyle),
    /// Border on vertical borders between buffer clusters.
    VerBorder(BorderStyle),
    /// Border on horizontal window edges and borders between buffer
    /// clusters.
    Horizontal(BorderStyle),
    /// Border on horizontal borders between buffer clusters.
    HorBorder(BorderStyle),
}

impl Default for Border {
    fn default() -> Self {
        Self::Border(BorderStyle::Regular)
    }
}

impl Border {
    /// The length of a border separating a buffer cluster from the
    /// edges on a given [`Axis`].
    pub(crate) fn buffers_len_on(&self, axis: Axis) -> f64 {
        let (hor_len, ver_len) = self.buffers_edges();
        match axis {
            Axis::Horizontal => hor_len,
            Axis::Vertical => ver_len,
        }
    }

    /// The length of a border on a given [`Axis`].
    pub(crate) fn len_on(&self, axis: Axis) -> f64 {
        let (hor_len, ver_len) = self.border_edges();
        match axis {
            Axis::Horizontal => hor_len,
            Axis::Vertical => ver_len,
        }
    }

    /// The [`BorderStyle`] in use, [`None`] for [`Border::Empty`].
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

    /// The edges below and to the right of [`Buffer`] regions.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    fn buffers_edges(&self) -> (f64, f64) {
        match self {
            Self::Surround(_) => (1.0, 1.0),
            Self::Vertical(_) => (1.0, 0.0),
            Self::Horizontal(_) => (0.0, 1.0),
            _ => (0.0, 0.0),
        }
    }

    /// The edges below and to the right of the [`Buffer`]s region.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    fn border_edges(&self) -> (f64, f64) {
        match self {
            Self::Surround(_) | Self::Border(_) => (1.0, 1.0),
            Self::Vertical(_) | Self::VerBorder(_) => (1.0, 0.0),
            Self::Horizontal(_) | Self::HorBorder(_) => (0.0, 1.0),
            Self::Empty => (0.0, 0.0),
        }
    }
}
