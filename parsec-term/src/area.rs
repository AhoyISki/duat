use std::{
    fmt::{Debug, Display},
    sync::atomic::Ordering
};

use crossterm::terminal;
use parsec_core::{
    data::RwData,
    ui::{self, Area as UiArea, Axis, PushSpecs, Side, Split}
};

use crate::{InnerWindow, Node};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Coord {
    pub x: u16,
    pub y: u16
}

impl Coord {
    pub fn new(x: u16, y: u16) -> Self {
        Self { x, y }
    }
}

impl Debug for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.y, self.x))
    }
}

impl Display for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.y, self.x))
    }
}

#[derive(Clone, Copy)]
pub struct Coords {
    pub tl: Coord,
    pub br: Coord
}

impl Coords {
    /// Returns a new instance of [`Coords`].
    pub fn new(tl: Coord, br: Coord) -> Self {
        Self { tl, br }
    }

    pub fn empty_on(&self, side: Side) -> Self {
        let tr = Coord::new(self.br.x, self.tl.y);
        let bl = Coord::new(self.tl.x, self.br.y);

        match side {
            Side::Top => Coords::new(self.tl, tr),
            Side::Right => Coords::new(tr, self.br),
            Side::Bottom => Coords::new(bl, self.br),
            Side::Left => Coords::new(self.tl, bl)
        }
    }

    pub fn len(&self, axis: Axis) -> usize {
        match axis {
            Axis::Horizontal => self.width(),
            Axis::Vertical => self.height()
        }
    }

    pub fn width(&self) -> usize {
        (self.br.x - self.tl.x) as usize
    }

    pub fn height(&self) -> usize {
        (self.br.y - self.tl.y) as usize
    }

    pub fn ortho_corner(&self, axis: Axis) -> Coord {
        match axis {
            Axis::Horizontal => Coord {
                x: self.br.x,
                y: self.tl.y
            },
            Axis::Vertical => Coord {
                x: self.tl.x,
                y: self.br.y
            }
        }
    }

    pub(crate) fn add_to_side(&mut self, side: Side, len_diff: i16) {
        match side {
            Side::Top => self.tl.y = self.tl.y.saturating_add_signed(-len_diff),
            Side::Left => self.tl.x = self.tl.x.saturating_add_signed(-len_diff),
            Side::Bottom => self.br.y = self.br.y.saturating_add_signed(len_diff),
            Side::Right => self.br.x = self.br.x.saturating_add_signed(len_diff)
        }
    }

    pub fn from_tl(&self, width: usize, height: usize) -> Self {
        Coords {
            tl: self.tl,
            br: Coord::new(self.tl.x + width as u16, self.tl.y + height as u16)
        }
    }

    pub fn from_origin(width: usize, height: usize) -> Self {
        Coords {
            tl: Coord::new(0, 0),
            br: Coord::new(width as u16, height as u16)
        }
    }
}

impl Debug for Coords {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{},{}:{}", self.tl.x, self.tl.y, self.br.x, self.br.y))
    }
}

impl Display for Coords {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{},{}", self.tl, self.br))
    }
}

/// An area in the window.
///
/// This area represents a rectangular region of the window, defined
/// by a [`Coords`]. It also contains information about its children
/// (if it has any).
///
/// An [`Area`] can be freely cloned and safely shared between threads
/// and will (usually) be found in Parsec's widget tree and as the
/// child of other [`Area`]s.
#[derive(Clone)]
pub struct Area {
    pub(crate) coords: RwData<Coords>,
    pub(crate) window: RwData<InnerWindow>,
    pub(crate) index: usize
}

impl Area {
    pub(crate) fn new(coords: Coords, index: usize, window: RwData<InnerWindow>) -> Self {
        Self {
            coords: RwData::new(coords),
            window,
            index
        }
    }

    /// Returns the maximum possible [`Area`], engulfing the entire
    /// window.
    pub(crate) fn total(index: usize, window: RwData<InnerWindow>) -> Self {
        let (max_x, max_y) = terminal::size().unwrap();
        let coords = Coords::new(Coord { x: 0, y: 0 }, Coord { x: max_x, y: max_y });

        Area {
            coords: RwData::new(coords),
            window,
            index
        }
    }

    /// The [`Coords`] of the [`Area`].
    pub fn coords(&self) -> Coords {
        *self.coords.read()
    }

    /// The top-left [`Coord`] of the [`Area`].
    pub fn tl(&self) -> Coord {
        self.coords.read().tl
    }

    /// The bottom-right [`Coord`] of the [`Area`].
    pub fn br(&self) -> Coord {
        self.coords.read().br
    }

    /// The length of [`self`], in a given [`Axis`].
    pub fn len(&self, axis: Axis) -> usize {
        if let Axis::Horizontal = axis {
            self.width()
        } else {
            self.height()
        }
    }

    /// Conforms a [`Split::Locked`] to a given length.
    fn conform_locked_split(&self, len: usize) {
        let mut window = self.window.write();
        let (child_index, parent) = window.find_mut_parent(self.index).unwrap();
        let (children, _) = parent.lineage.as_mut().unwrap();
        let (_, split) = &mut children[child_index];
        if let Split::Locked(_) = split {
            *split = Split::Locked(len);
        }
    }
}

impl Display for Area {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{},{}", self.coords.read().tl, self.coords.read().br))
    }
}

impl ui::Area for Area {
    fn width(&self) -> usize {
        self.coords.read().width()
    }

    fn height(&self) -> usize {
        self.coords.read().height()
    }

    fn request_len(&self, len: usize, side: Side) -> Result<(), ()> {
        // To signal to other readers that the `InnerWindow` has changed.
        drop(self.window.write());
        let req_axis = Axis::from(side);
        let window = self.window.read();
        let (child_index, parent) = window.find_parent(self.index).ok_or(())?;

        let (children, axis) = parent.lineage.as_ref().unwrap();
        let (child, _) = &children[child_index];

        // If the current len is already equal to the requested len,
        // nothing needs to be done.
        if child.resizable_len(req_axis, &window) == len {
            return Ok(());
        };

        if req_axis != *axis {
            let area = parent.area.clone();
            drop(window);
            area.request_len(len, side)
        } else if parent.resizable_len(req_axis, &window) < len {
            let new_parent_width = parent.area.len(req_axis) + len - {
                match req_axis {
                    Axis::Horizontal => child.area.width(),
                    Axis::Vertical => child.area.height()
                }
            };

            let area = parent.area.clone();
            drop(window);
            area.request_len(new_parent_width, side)
        } else {
            let ret = parent.set_child_len(child_index, len as u16, side);
            drop(window);
            self.conform_locked_split(len);
            ret
        }
    }

    fn bisect(&mut self, push_specs: PushSpecs, is_glued: bool) -> (usize, Option<usize>) {
        let PushSpecs { side, split } = push_specs;
        let axis = Axis::from(side);

        let resizable_len = self.window.inspect(|window| {
            window.find_node(self.index).unwrap().resizable_len(axis, &window)
        });

        if resizable_len < split.len() {
            self.request_len(resizable_len.max(split.len()), side).unwrap();
        }

        let mut window = self.window.write();
        let new_area_index = window.next_index.fetch_add(1, Ordering::Relaxed);

        let node = window.find_mut_node(self.index).unwrap();

        if let Some((children, _)) = node.lineage.as_ref().filter(|(_, other)| *other == axis) {
            let self_index = match side {
                Side::Bottom | Side::Right => children.len() - 1,
                Side::Top | Side::Left => 0
            };

            node.insert_new_area(self_index, split, side, new_area_index);

            (new_area_index, None)
        } else if let Some((child_index, parent)) = {
            if !is_glued {
                window.find_mut_parent(self.index).filter(|(_, parent)| {
                    parent.lineage.as_ref().is_some_and(|(_, other)| *other == axis)
                })
            } else {
                None
            }
        } {
            parent.insert_new_area(child_index, split, side, new_area_index);

            (new_area_index, None)
        } else {
            let new_child_index = window.next_index.fetch_add(1, Ordering::Relaxed);
            // NOTE: ???????
            let area = Area::new(self.coords(), self.index, self.window.clone());
            let new_parent = Node::new(area, None);

            let swapped_parent = window.find_mut_node(self.index).unwrap();
            let mut node = std::mem::replace(swapped_parent, new_parent);
            node.area.index = new_area_index;

            swapped_parent.lineage = Some((vec![(node, Split::default())], axis));

            swapped_parent.insert_new_area(0, split, side, new_child_index);

            (new_child_index, Some(new_area_index))
        }
    }

    fn request_width_to_fit(&self, _text: &str) -> Result<(), ()> {
        todo!()
    }
}
