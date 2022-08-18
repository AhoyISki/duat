use std::{cmp, fmt::Display, ops};

use crate::tags::{CharTag, Form};

/// A relative position where text is printed.
///
/// These should only be used to move the cursor responsible for printing
/// to the output, not the user's actual cursor. As they only print, they
/// cannot be negative. The postition is relative to a given `OutputArea`.
#[derive(Copy, Clone, Debug)]
pub struct OutputPos {
    pub x: u16,
    pub y: u16,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PrintInfo {
    /// The index of the line at the top of the screen.
    pub top_line: usize,
    /// The number of times the top line should wrap.
    pub top_wraps: usize,
    /// The leftmost col shown on the screen.
    pub x_shift: usize,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct AreaId(usize);

#[derive(Clone, Copy)]
pub enum Split {
    Dynamic(f32),
    Static,
}

#[derive(Clone, Copy)]
pub enum Axis {
    Horizontal,
    Vertical,
}

#[derive(Clone, Copy)]
pub enum NodeType {
    ParentNode { first: AreaId, second: AreaId, axis: Axis, split: Split },
    EndNode,
}

#[derive(Clone)]
pub struct AreaNode<A>
where
    A: Area,
{
    area: A,
    parent: Option<AreaId>,
    len: Option<usize>,
    id: AreaId,
    node_type: NodeType,
    class: String,
    /// If true, all mouse input in any of its children will be redirected to itself.
    master: bool,
}

#[derive(Clone, Copy)]
pub enum Length {
    Static(usize),
    Ratio(f32),
}

pub trait AreaManager {
    type Area: Area;

	/// Updates a specific area and its decendants.
    fn update_area(&self, areas: &mut Vec<AreaNode<Self::Area>>, id: AreaId);

	/// Updates all areas.
    fn update(&self, areas: &mut Vec<AreaNode<Self::Area>>);
}

pub enum Direction {
    Top,
    Right,
    Bottom,
    Left,
}

pub struct AreaNodeTree<M>
where
    M: AreaManager,
{
    handler: M,
    areas: Vec<AreaNode<M::Area>>,
    last_id: usize,
}

impl<M> AreaNodeTree<M>
where
    M: AreaManager,
{
    pub fn new(handler: M, area: M::Area, class: String) -> (AreaNodeTree<M>, AreaId) {
        let area_node_tree = AreaNodeTree {
            handler,
            areas: vec![AreaNode {
                area,
                parent: None,
                len: None,
                id: AreaId(0),
                node_type: NodeType::EndNode,
                class,
                master: true,
            }],
            last_id: 0,
        };

        (area_node_tree, AreaId(0))
    }

    /// Creates a new parent area, containing the old area and another newly created area.
    pub fn push(
        &mut self, id: AreaId, direction: Direction, len: Length, child_class: String,
        parent_class: Option<String>,
    ) -> (AreaId, AreaId) {
        let old_node = self.areas.iter_mut().find(|n| n.id == id).expect("AreaId does not exist!");
        old_node.parent = Some(AreaId(self.last_id + 2));
        let (old_id, old_parent) = (old_node.id, old_node.parent);

        assert!(old_node.len.is_none(), "The original area must be dinamically sized!");

        self.last_id += 1;
        self.areas.push(AreaNode {
            len: if let Length::Static(len) = len { Some(len) } else { None },
            id: AreaId(self.last_id),
            node_type: NodeType::EndNode,
            master: true,
            class: child_class,
            area: M::Area::new(),
            parent: Some(AreaId(self.last_id + 1)),
        });

        self.areas.push(AreaNode {
            len: None,
            id: AreaId(self.last_id + 1),
            node_type: {
                let (first, second, axis) = match direction {
                    Direction::Top => (AreaId(self.last_id), old_id, Axis::Vertical),
                    Direction::Right => (old_id, AreaId(self.last_id), Axis::Horizontal),
                    Direction::Bottom => (old_id, AreaId(self.last_id), Axis::Vertical),
                    Direction::Left => (AreaId(self.last_id), old_id, Axis::Horizontal),
                };

                let split = match len {
                    Length::Static(_) => Split::Static,
                    Length::Ratio(ratio) => Split::Dynamic(ratio),
                };

                NodeType::ParentNode { first, second, axis, split }
            },
            master: false,
            class: parent_class.unwrap_or(String::from("parsec-parent")),
            area: M::Area::new(),
            parent: old_parent,
        });
        self.last_id += 1;

        self.handler.update(&mut self.areas);

        (AreaId(self.last_id), AreaId(self.last_id - 1))
    }

    pub fn resize(&mut self, id: AreaId, new_len: usize) {
        let node = self.areas.iter_mut().find(|n| n.id == id).expect("AreaId does not exist!");

        match node.len {
            Some(ref mut len) => *len = new_len,
            None => panic!("You can only resize areas of static size!")
        };

        match node.parent {
            Some(id) => self.handler.update_area(&mut self.areas, id),
            None => self.handler.update(&mut self.areas)
        }
    }

    pub fn set_ratio(&mut self, id: AreaId, new_ratio: f32) {
        let node = self.areas.iter_mut().find(|n| n.id == id).expect("AreaId does not exist!");

        match node.node_type {
            NodeType::ParentNode { first: _, second: _, axis: _, mut split } => {
                match split {
                    Split::Dynamic(ref mut ratio) => *ratio = new_ratio,
                    Split::Static => panic!("You can't set a ratio to a static split!")
                }
            }
            NodeType::EndNode =>  panic!("You can't set a ratio to an end node!")
        }

        self.handler.update_area(&mut self.areas, id);
    }
}

pub trait Area: Copy + Clone {
    fn new() -> Self;

    fn width(&self) -> usize;

    fn height(&self) -> usize;
}

/// An area in the output (terminal or GUI).
///
/// Examples include: The file buffer, status line, line numbers, etc.
pub trait OutputArea {
    /// Wether or not this area can place multiple carets for the `SecondaryCursor` tag.
    fn can_place_secondary_cursor(&self) -> bool;

    /// Places a cursor on the screen
    ///
    /// #Panics
    ///
    /// * Panics if you try to place a `SecondaryCursor` in an area that can't do that.
    /// Check `can_place_secondary_cursor()` for this information.
    /// * Also panics if you place any tag that isn't `PrimaryCursor` or `SecondaryCursor`.
    fn place_cursor(&mut self, cursor: CharTag);

    /// Appends a normal form to the stack.
    fn push_form(&mut self, form: &Form, index: u16);

    /// Removes a normal form from the stack.
    fn pop_form(&mut self, index: u16);

    /// Appends a multi-line form to the stack.
    fn push_ml_form(&mut self, form: &Form, index: u16);

    /// Removes a multi-line form from the stack.
    fn pop_ml_form(&mut self, index: u16);

    /// Clears the normal forms from the stack, keeping only multi-line forms in.
    fn clear_normal_forms(&mut self);

    /// Clears both normal and multi-line forms from the stack.
    fn clear_all_forms(&mut self);

    /// Tells the area that we're about to start printing.
    fn start_print(&mut self);

    /// Prints text at the current printing cursor's position.
    fn print<T: Display>(&mut self, text: T);

    /// Tells the area that we're done printing
    fn finish_print(&mut self);

    /// Moves the relative printing cursor.
    ///
    /// Will change where the next characters will be printed, without wrapping.
    fn move_cursor(&mut self, pos: OutputPos);

    /// Returns the width of the area.
    fn width(&self) -> usize;

    /// Returns the height of the area
    fn height(&self) -> usize;

    /// Partitions the area on x, returning the area on the left.
    fn partition_x(&mut self, x: u16) -> Self;

    /// Partitions the area on x, returning the area at the top.
    fn partition_y(&mut self, y: u16) -> Self;
}

impl ops::Add for OutputPos {
    type Output = OutputPos;

    fn add(self, rhs: Self) -> Self::Output {
        OutputPos { x: self.x + rhs.x, y: self.y + rhs.y }
    }
}

impl cmp::PartialEq for OutputPos {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }

    fn ne(&self, other: &Self) -> bool {
        self.x != other.x || self.y != other.y
    }
}

impl cmp::PartialOrd for OutputPos {
    fn ge(&self, other: &Self) -> bool {
        self.x >= other.x && self.y >= other.y
    }

    fn gt(&self, other: &Self) -> bool {
        self.x > other.x && self.y > other.y
    }

    fn le(&self, other: &Self) -> bool {
        self.x <= other.x && self.y <= other.y
    }

    fn lt(&self, other: &Self) -> bool {
        self.x < other.x && self.y < other.y
    }

    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        if self > other {
            Some(cmp::Ordering::Greater)
        } else if self < other {
            Some(cmp::Ordering::Less)
        } else {
            Some(cmp::Ordering::Equal)
        }
    }
}
