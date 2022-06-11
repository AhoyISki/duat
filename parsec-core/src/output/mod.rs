use std::ops;
use std::fmt::Display;
use std::cmp::{self, max, min};

use crossterm::event::KeyEvent;
use crossterm::style::{
    StyledContent,
    ContentStyle
};

#[derive(Clone)]
pub struct StyledChar {
    pub text: StyledContent<String>,
    pub width: usize,

    pub is_wrapping: bool,
}

impl StyledChar {
    pub fn new(grapheme: &str, width: usize) -> StyledChar {
        StyledChar {
            text: StyledContent::new(ContentStyle::new(), grapheme.to_string()),
            width,
            is_wrapping: false
        }
    }
}

/// A relative position where text is printed.
///
/// These should only be used to move the cursor responsible for printing
/// to the output, not the user's actual cursor. As they only print, they
/// cannot be negative. The postition is relative to a given output area.
#[derive(Copy, Clone, Debug)]
pub struct OutputPos {
    pub x: u16,
    pub y: u16,
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

/// A position used for cursors.
/// 
/// This object indicates where each cursor is, in relation to the output
/// area. This means that the cursors should be able to have negative
/// positions. In y for previous lines, and in x for previous columns.
#[derive(Copy, Clone)]
pub struct CursorPos {
    pub x: i32,
    pub y: i32
}

pub trait OutputArea {
    fn new(origin: OutputPos, end: OutputPos) -> Self;

    /// Allocates an origin and end to an area.
    ///
    /// Should not be called directly.
    fn allocate_area(
        &self, origin: OutputPos, end: OutputPos) -> (OutputPos, OutputPos)  {
        let (width, height) = (self.width(), self.height());
        let origin = OutputPos { x: max(origin.x, 0), y: max(origin.y, 0) };
        let end = OutputPos { x: min(end.x, width), y: min(end.y, height) };
        (origin, end)
    }

    /// Prints styled text.
    /// 
    /// Prints content that has colors, italics, bold, etc.
    fn print_styled(&mut self, text: StyledChar);

    /// Prints plain text.
    /// 
    /// Will print to output without any styling whatsoever.
    fn print_string(&mut self, text: String);

    /// Moves the relative printing cursor.
    /// 
    /// Will change where the next characters will be printed, without wrapping.
    fn move_cursor(&mut self, pos: OutputPos);

    /// Moves the relative printing cursor to the origin.
    /// 
    /// Will change where the next characters will be printed, without wrapping.
    fn move_cursor_to_origin(&mut self);

    /// Returns the width of the area.
    fn width(&self) -> u16;

    /// Returns the height of the area
    fn height(&self) -> u16;

    /// Refreshes the area
    fn flush(&mut self);
}

pub trait InputArea {
    /// Moves an user's actual cursor.
    ///
    /// Will move a text editing cursor, not the printing cursor of the area.
    fn move_cursor(&self, cursor: usize, pos: CursorPos);
}

/// How an area should react to window size changes.
/// 
/// - Static: No change in size, if the origin or end is outside of the window, the
///   area won't be shown.
/// - Maximum: Take up as much space as possible. The window will be stretched to
///   occupy a square that's not taken by anything else.
/// - Capped: If Some is given, the area will stretch until reaching that lenght.
///   If None is given, that dimension is allowed to stretch indefinitely.
// TODO: Add more elasticity modes:
#[derive(Debug)]
pub enum Elasticity {
    Static,
    // Here, the minimum sizes define when an area should no longer be displayed,
    // and the maximum sizes define at what point it should no longer grow.
    Maximum,
    Capped{ min_x: Option<u16>, min_y: Option<u16> },
}

pub trait WindowBuffer {
    type Window;

    fn new() -> Self::Window;

    fn dims(&self) -> (u16, u16);

    fn process_events(&mut self);
}

pub trait InputHandler {
    fn handle_key(&mut self, key: KeyEvent);

    fn bind_action(&mut self, name: Option<String>, key: Option<KeyEvent>);

    fn get_action_names(&self) -> Vec<String>;

    fn is_in_mode(&self, name: &str) -> bool;
}

pub struct MappedAction<T> {
    pub cmd: fn(&mut T),
    pub name: Option<String>,
    pub key: Option<KeyEvent>,
}

impl<T> MappedAction<T> {
    fn new(cmd: fn(&mut T), name: Option<String>, key: Option<KeyEvent>) -> Self {
        MappedAction { cmd, name, key }
    }
}

pub struct ModeList<T> {
    pub modes: Vec<Mode<T>>,
    pub current_mode: usize,
}

pub struct Mode<T> {
    pub name: String,
    pub actions: Vec<MappedAction<T>>,
    pub default_action: Option<fn(&mut T)>,
}

impl<T> Mode<T> {
    pub fn new(name: &str) -> Mode<T> {
        Mode {
            name: name.to_string(),
            actions: Vec::new(),
            default_action: None,
        }
    }

    pub fn add_action(&mut self, action: fn(&mut T), name: Option<String>,
        key: Option<KeyEvent>) {
        self.actions.push(MappedAction::new(action, name, key));
    }

    pub fn add_default_action(&mut self, action: fn(&mut T)) {
        self.default_action = Some(action);
    }
}

impl<T> ModeList<T> {
    pub fn new(name: &str) -> ModeList<T> {
        let mut mode_list = ModeList {
            modes: Vec::new(),
            current_mode: 0,
        };

        mode_list.modes.push(Mode::new(name));

        mode_list
    }

    pub fn add_mode(&mut self, name: &str) {
        self.modes.push(Mode::new(name));
    }
}

#[macro_export]
macro_rules! map_actions {
    ($handler:ident: $handler_type:ty, $mode_list:ident;
        $($mode:expr => [
            $(
                $((($code:expr, $modif:expr), $name:expr) => { $cmd:expr })?
                $(($lock_code:expr, $lock_modif:expr)     => { $lock_cmd:expr })?
                $(name: $free_name:expr                   => { $free_cmd:expr })?
            ),*,
            $(_ => $default_cmd:ident)?
        ]),*
    ) => {

        // This is so the compiler stops warning me about unused code.
        let mut index = -1;

        $(
            $handler.$mode_list.add_mode($mode);

            index += 1;

            let mode = $handler.$mode_list.modes.get_mut(index as usize).unwrap();

            $(
                $(
                    let key = KeyEvent::new($code, $modif);
                    mode.add_action($cmd, Some($name.to_string()), Some(key));
                )?
                $(
                    let key = KeyEvent::new($lock_code, $lock_modif);
                    mode.add_action($lock_cmd, None, Some(key));
                )?
                $(mode.add_action($free_cmd, Some($free_name.to_string()), None);)?
            )*
        )*
    }
}

#[macro_export]
macro_rules! impl_input_handler {
    ($handler_type:ty, $mode_list:ident) => {
        impl<T: OutputArea> InputHandler for $handler_type {
            fn handle_key(&mut self, key: KeyEvent) {
                let mode = self.$mode_list.modes.get(self.$mode_list.current_mode)
                                                .unwrap();

                for action in &mode.actions {
                    if let Some(action_key) = action.key {
                        if key == action_key {
                            (action.cmd)(self);
                            return;
                        }
                    }
                }
            }

            fn bind_action(&mut self, name: Option<String>, key: Option<KeyEvent>) {
                for mode in &mut self.$mode_list.modes {
                    for action in &mut mode.actions {
                        if name == action.name {
                            action.key = key;
                            return;
                        }
                    }
                }
            }

            fn get_action_names(&self) -> Vec<String> {
                let mut name_vec = Vec::new();

                for mode in &self.$mode_list.modes {
                    for action in &mode.actions {
                        if let Some(name) = &action.name {
                            name_vec.push(name.clone());
                        }
                    }
                }

                name_vec
            }

            fn is_in_mode(&self, name: &str) -> bool {
                let mode = self.$mode_list.modes.get(self.$mode_list.current_mode)
                                                .unwrap();

                mode.name == name
            }
        }
    }
}

// TODO: Allow the binding of multiple actions to the same key;
// TODO: Allow the binding to keys directly.
/// Enum detailing how to handle the addition of bindings with the same KeyEvent
pub enum MapppingAddition {
    Over,
    Under,
    Merge,
}
