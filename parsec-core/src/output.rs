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

pub struct AreaId(usize);

pub enum AreaSplit {
    /// When the first area has a static width/height, and the second area has a dynamic size.
    StaticFirst { len: usize },
    /// When the second area has a static width/height, and the first area has a dynamic size.
    StaticSecond { len: usize },
    /// When both areas have a dynamic size, dictated by a ratio between 0 and 1.
    Dynamic { ratio: f32 }
}

pub enum SplitDirection {
    Horizontal,
    Vertical
}

pub enum NodeType {
    ParentNode { first: AreaId, second: AreaId, direction: SplitDirection, split: AreaSplit },
    EndNode
}

pub struct AreaNode<T>
where
    T: Area {
    area: T,
    id: AreaId,
    node_type: NodeType,
    /// If true, all mouse input in any of its children will be redirected to itself.
    master: bool
}

pub struct AreaNodeArena<T>
where
    T: Area {
    areas: Vec<AreaNode<T>>,
    last_id: usize
}

impl<T> AreaNodeArena<T>
where
    T: Area {
    
    
}

pub trait Area {
    fn width(&self) -> usize;

    fn height(&self) -> usize;

    fn split(&mut self, split: SplitDirection, area_split: AreaSplit);
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
