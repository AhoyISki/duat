use crossterm::style::ContentStyle;
use unicode_width::UnicodeWidthStr;

use std::{ops, fmt::Display, cmp};

use crate::{cursor::CursorPos, config::TabPlaces};

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

// It's a string because of multi byte UTF-8 and graphemes, which can be several
// characters long.
/// A character containing text, an optional style, and boolean tags.
#[derive(Clone, Debug)]
pub struct MainChar {
    // Why not a `u8`, you may ask? I want as many characters as possible to be indexable at O(1)
    // speed, and this is the best way of achieving that. Plus, the size of `ch` is much smaller
    // than the size of `TaggedChar` anyway, since `contentStyle` has size 16, so it doesn't
    // really make that much difference, it just makes my life easier.
    /// The character's unicode codepoint.
    pub ch: char,
    // TODO: maybe (probably not) change this to Option<Rc<ContentStyle>>
    pub style: Option<ContentStyle>,

	/// The space occupied by the character, in cells.
	/// 
	/// If the character is the first in a grapheme, it will have the width of the whole grapheme,
	/// if it's any of the other characters in said grapheme, its width will be 0.
    width: u8,

	// TODO: Turn this into a list of boolean tags.
    pub is_wrapping: bool,
}

/// A character that will appear in text.
#[derive(Clone, Debug)]
pub enum TextChar {
    /// A tagged character, will either be a full grapheme or the start of one.
    Primary(MainChar),
    /// Simple grapheme extension, the only information it needs is its UTF-8 codepoint.
    Secondary(char),
}

#[derive(Debug, Clone, Copy)]
pub struct PrintInfo {
    /// The index of the line at the top of the screen.
    pub top_line: usize,
    /// The number of times the top line should wrap.
    pub top_wraps: usize,
    /// The leftmost col shown on the screen.
    pub x_shift: u16,
}

impl MainChar {
    /// Returns the width for the character. If it is a tab, calculates how big it should be.
    pub fn width(&self, x: u16, tabs: &TabPlaces) -> u16 {
        if self.ch == '\t' {
            tabs.get_tab_len(x) as u16
        } else {
            self.width as u16
        }
    }

    /// Returns a new insance of `StyledChar`.
    pub fn new(ch: char, width: u8) -> MainChar {
        MainChar { ch, style: None, width, is_wrapping: false }
    }

    /// Returns a new instance of `StyledChar` from an instance of `StyledContent`.
    pub fn new_styled(ch: char, style: ContentStyle, width: u8) -> MainChar {
        MainChar { ch, style: Some(style), width, is_wrapping: false }
    }
}

/// An area in the output (terminal or GUI).
///
/// Examples include: The file buffer, status line, line numbers, etc.
pub trait OutputArea {
    /// Prints styled text.
    /// 
    /// Prints content that has colors, italics, bold, etc.
    fn print_and_style(&mut self, text: MainChar);

    /// Prints plain text.
    /// 
    /// Will print to output without any styling whatsoever.
    fn print<T: Display>(&mut self, text: T);

    /// Moves the relative printing cursor.
    /// 
    /// Will change where the next characters will be printed, without wrapping.
    fn move_cursor(&mut self, pos: OutputPos);

    /// Returns the width of the area.
    fn width(&self) -> u16;

    /// Returns the height of the area
    fn height(&self) -> u16;

    /// Refreshes the area
    fn flush(&mut self);

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

impl ops::Add<CursorPos> for OutputPos {
    type Output = OutputPos;

    fn add(self, rhs: CursorPos) -> Self::Output {
        OutputPos { x: self.x + rhs.x as u16, y: self.y + rhs.y as u16 }
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

impl From<CursorPos> for OutputPos {
    fn from(pos: CursorPos) -> Self {
        OutputPos { x: pos.x as u16, y: pos.y as u16 }
    }
}

impl Display for OutputPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

