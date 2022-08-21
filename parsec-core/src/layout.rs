use std::{
    cmp::min,
    fmt::Display,
    ops::{self, RangeInclusive},
    path::PathBuf,
};

use crate::{
    cursor::FileCursor,
    file::TextLine,
    tags::{CharTag, Form},
    ui::{Area, ChildNode, AreaManager, ParentId},
};

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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct PrintInfo {
    /// The index of the line at the top of the screen.
    pub top_line: usize,
    /// The number of times the top line should wrap.
    pub top_wraps: usize,
    /// The leftmost col shown on the screen.
    pub x_shift: usize,
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

    /// Clears the normal forms from the stack, keeping only multi-line forms in.
    fn clear_forms(&mut self);

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

// TODO: Maybe set up the ability to print images as well.
/// An area where text will be printed to the screen.
pub trait PrintedArea {
    /// Gets the lines that should be printed to an area.
    fn text(&self) -> Text;

    /// Adapts a given text to a new size for its given area.
    fn resize(&mut self, area: &dyn Area);
}

/// An area where you can edit the text.
pub trait EditArea: PrintedArea {
    /// Gets the cursors on the area.
    ///
    /// # Returns
    ///
    /// * A list of cursors. This includes cursors that shouldn't be printed on screen.
    /// * The index of the main cursor. Most of the time, this will be 0.
    fn get_cursors(&mut self) -> (&mut Vec<FileCursor>, usize);
}

/// The text in a given area.
struct Text {
    print_info: PrintInfo,
    lines: Vec<TextLine>,
    replacements: Vec<(Vec<TextLine>, RangeInclusive<usize>)>,
}

// TODO: Properly implement replacements.
impl Text {
    /// Prints the contents of a given area in a given `ChildNode`.
    fn print<'a, A>(&'a self, node: &mut ChildNode<A>, forms: &[Form])
    where
        A: Area,
    {
        node.area.start_printing();

		// Print the `top_line`.
        let top_line = self.lines[self.print_info.top_line];
        let top_wraps = self.print_info.top_wraps;
        let skip = if top_wraps > 0 { top_line.wrap_iter().nth(top_wraps - 1).unwrap() } else { 0 };
        top_line.print(node, self.print_info.x_shift, skip as usize, node.options(), forms);

		// Prints other lines until it can't anymore.
        for line in self.lines.iter().skip(self.print_info.top_line) {
            if !line.print(node, self.print_info.x_shift, 0, node.options(), forms) {
                break;
            }
        }

        node.area.stop_printing();
    }

	/// Returns a lines of all lines that would be printed in a given node.
	///
	/// The first number is the `TextLine`'s index, and the second is the amount of visible lines
	/// on the screen the `TextLine` would occupy.
    fn printed_lines<A>(&self, node: &ChildNode<A>) -> Vec<(usize, usize)>
    where
        A: Area {
        let height_sum = self.print_info.top_wraps + node.area.height();
        let mut printed_lines = Vec::new();
        let mut lines_iter = self.lines.iter().enumerate();

        // List the top line.
        let top_line = lines_iter.nth(self.print_info.top_line).unwrap();
        let mut d_y = 1 + top_line.1.wrap_iter().count();
        printed_lines.push((top_line.0, min(d_y - self.print_info.top_wraps, node.area.height())));

		// List all the other lines.
		while let (Some((index, line)), true) = (lines_iter.next(), d_y < height_sum) {
    		let line_count = 1 + line.wrap_iter().count();
    		printed_lines.push((index, min(line_count, height_sum - d_y)));
    		d_y += line_count;
		}

		printed_lines
    }
}

pub trait Layout {
    fn new<M>(area_manager: M) -> Self
    where
        M: AreaManager;

    fn new_file_buffer(&mut self, path: PathBuf);
}

pub struct StatusArea();

pub struct FileArea {
    text: Text,
    cursors: Vec<FileCursor>,
}

impl FileArea {
	fn scroll_vertically(&mut self) {
        let info = &mut self.print_info;
        let scrolloff = self.options.scrolloff;

        let main_cursor = self.cursors.get(self.main_cursor).unwrap();
        let current = main_cursor.current();
        let target = main_cursor.target();

        // Vertical scroll check:
        if let WrapMethod::NoWrap = self.text.options.wrap_method {
            // If there is no wrapping, the check is much simpler, just check if the distance to
            // `info.top_line` is within `scrolloff.d_y` and `self.area.height() + scrolloff.d_y`,
            // If it's not, subtract the difference and add/subtract it from `info.top_line`.
            if target.line > info.top_line + self.area.height() - scrolloff.d_y {
                info.top_line += target.line + scrolloff.d_y - info.top_line - self.area.height();
            } else if target.line < info.top_line + scrolloff.d_y && info.top_line != 0 {
                info.top_line -= (info.top_line + scrolloff.d_y) - target.line;
            }
        } else {
            let line = &self.text.lines[current.line];
            let current_byte = line.get_line_byte_at(current.col);
            let current_wraps = line.wrap_iter().take_while(|&c| c <= current_byte as u32).count();

            let line = &self.text.lines[target.line];
            let target_byte = line.get_line_byte_at(target.col);
            let target_wraps = line.wrap_iter().take_while(|&c| c <= target_byte as u32).count();

            let lines_iter = self.text.lines[..=target.line].iter_mut();

            let mut d_y = target_wraps;

            // Case where we're moving down.
            if target.line > current.line
                || (target.line == current.line && target_wraps > current_wraps)
            {
                let mut top_offset = 0;
                for (index, line) in lines_iter.enumerate().rev() {
                    if index != target.line {
                        d_y += 1 + line.wrap_iter().count();
                    }

                    if index == info.top_line {
                        top_offset = info.top_wraps
                    };

                    // If this happens first, that means the distance between `target.line` and
                    // `info.top_line` is greater than allowed height of the cursor.
                    if d_y >= self.area.height() + top_offset - scrolloff.d_y {
                        info.top_line = index;
                        // If this equals 0, that means the distance has matched up perfectly,
                        // i.e. the distance between the new `info.top_line` is exactly what's
                        // needed for the full height. If it's greater than 0, `info.top_wraps`
                        // needs to adjust where the line actually begins to match up.
                        info.top_wraps = d_y + scrolloff.d_y - self.area.height();

                        break;
                    }

                    // If this happens first, we're in the middle of the screen, and don't need
                    // to change `info.top_line`.
                    if index == info.top_line {
                        break;
                    }
                }
            // Case where we're moving up.
            // `info.top_line` after implementing line folding.
            } else {
                // Set this flag immediately in this case, because the first line that checks out
                // will definitely be `info.top_line`.
                let mut needs_new_top_line = target.line < info.top_line;
                for (index, line) in lines_iter.enumerate().rev() {
                    // Add the vertical distance, as 1 line plus the times it wraps around.
                    // `target.line` was already added as `target_wraps`.
                    if index != target.line {
                        d_y += 1 + line.wrap_iter().count();
                    };

                    if index == info.top_line {
                        // This means we ran into the top line too early, and must scroll up.
                        // `info.top_wraps` is here because the top line might be partially off
                        // screen, and we'd be "comparing" only against the shown wraps, which is
                        // incorrect
                        if d_y < scrolloff.d_y + info.top_wraps {
                            needs_new_top_line = true;
                        // If this happens, we ran into `info.top_line` while below `scrolloff.y`,
                        // this means we're in the "middle" of the screen, and don't need to
                        // scroll.
                        } else if !needs_new_top_line {
                            break;
                        }
                    }

                    // In this case, we have either passed through `info.top_line` while too close,
                    // or not passed through, so a new `info.top_line` is behind the old one.
                    if needs_new_top_line && (d_y >= scrolloff.d_y || index == 0) {
                        info.top_line = index;
                        info.top_wraps = d_y.saturating_sub(scrolloff.d_y);

                        break;
                    }
                }
            }
        }

        // Horizontal scroll check, done only when the screen can scroll horizontally:
        if let WrapMethod::NoWrap = self.options.wrap_method {
            let target_line = &self.lines[target.line];
            let distance = target_line.get_distance_to_col(target.col, &self.options.tabs);

            // If the distance is greater, it means that the cursor is out of bounds.
            if distance > info.x_shift + self.area.width() - scrolloff.d_x {
                // Shift by the amount required to keep the cursor in bounds.
                info.x_shift = distance + scrolloff.d_x - self.area.width();
            // Check if `info.x_shift` is already at 0, if it is, no scrolling is dones.
            } else if distance < info.x_shift + scrolloff.d_x {
                info.x_shift = distance.saturating_sub(scrolloff.d_x);
            }
        }
	}
}

pub struct FileBuffer {
    path: PathBuf,
    file_area: FileArea,
}

struct OneStatusLayout {
    node: ParentId,
    status: StatusArea,
    side_areas: Vec<Box<dyn PrintedArea>>,
    files: Vec<FileBuffer>
}
