use std::ops;
use std::fmt::Display;
use std::cmp::{self, max, min};

use crossterm::event::KeyEvent;
use crossterm::style::{
    StyledContent,
    ContentStyle
};

// It's a string because of multi byte UTF-8 and graphemes, which can be several
// characters long.
/// A character containing text and a style which applies to said text.
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
/// cannot be negative. The postition is relative to a given `OutputArea`.
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

/// An area in the output (terminal or GUI).
///
/// Examples include: The file buffer, status line, etc.
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

/// Handles inputs from the user.
///
/// This trait makes an object capable of taking in input and remapping actions.
pub trait InputHandler {
    /// Handles a `KeyEvent`.
    fn handle_key(&mut self, key: KeyEvent);

    /// Bind a named action to a `KeyEvent`.
    fn bind_action(&mut self, name: &str, key: KeyEvent);

    /// Returns a list of every action in every mode of the `InputHandler`.
    fn get_action_names(&self) -> Vec<String>;

    /// Returns true if the `InputHandler` is in a certain mode.
    fn is_in_mode(&self, name: &str) -> bool;
}

/// An action that executes a command on an instance of type T.
///
/// * If there is a key, it's already mapped when starting the app.
/// * If there is a name, it can be called from the command line.
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

/// A list of modes for a given `InputHandler` of type `T`.
pub struct ModeList<T> {
    pub modes: Vec<Mode<T>>,
    pub current_mode: usize,
}

/// A mode containing mappings, like vim's insert, normal and visual modes.
pub struct Mode<T> {
    pub name: String,
    pub actions: Vec<MappedAction<T>>,
    pub default_action: Option<fn(&mut T, &str)>,
}

impl<T> Mode<T> {
    pub fn new(name: &str) -> Mode<T> {
        Mode {
            name: name.to_string(),
            actions: Vec::new(),
            default_action: None,
        }
    }

    /// Adds an action to the mode.
    pub fn add_action(&mut self, action: fn(&mut T), name: Option<&str>,
        key: Option<KeyEvent>) {

        if let (None, None) = (name, key) {
            panic!("An unreachable option has been created");
        }
        self.actions.push(
            MappedAction::new(
                action,
                if let Some(name) = name {
                    Some(name.to_string())
                } else {
                    None
                },
                key
            )
        );
    }

    pub fn add_default_action(&mut self, action: fn(&mut T, &str)) {
        self.default_action = Some(action);
    }
}

impl<T> ModeList<T> {
    pub fn new() -> ModeList<T> {
        let mut mappings = ModeList {
            modes: Vec::new(),
            current_mode: 0,
        };

        mappings
    }

    pub fn add_mode(&mut self, name: &str) {
        self.modes.push(Mode::new(name));
    }
}

/// Maps actions to names and/or keys.
///
/// You should run this macro when creating a new instance of `InputHandler`.
///
/// # Examples
///
/// Usage:
/// 
/// ```
/// struct MyStruct {
///     ...,
///     mappings: Mappings<MyStruct>,
/// }
/// impl MyStruct {
///     fn new() -> MyStruct {
///         let handler = MyStruct {
///             ...,
///             mappings: Mappings::new(),
///         }
///
///         map_actions!(
///             handler: MyStruct, mappings;
///             // The first mode is the default mode of the `InputHandler`.
///             "first_mode" => [
///                 // Action mapped to a name and a KeyEvent. This action can be
///                 // remapped and called from the command line. It also has a
///                 // default KeyEvent.
///                 (key_code, key_modifiers), "action_name_1" => { method_1 },
///                 // Action mapped to a KeyEvent only. This action can only be
///                 // remapped through the original mapping. Without a name, it
///                 // can't be called from the command line. 
///                 key: (key_code, key_modifiers) => { method_2 },
///                 // Action mapped to a name only. This action can be remapped and
///                 // called from the command line, but it has no default mapping.
///                 name: "action_name_2" => { method_3 },
///                 // Default action. This action is only called if no other actions
///                 // have been. It will only be called on symbols, so things like
///                 // Esc, Del, and F keys won't be detected.
///                 _ => { default_method }
///             // Other modes may be accessed by changing mappings.current_mode
///             // to the correct index (0 for "first_mode", 1 for "second_mode").
///             ], "second_mode" => [
///                 // Actions can take the form of a closure. It's a simple way to
///                 // perform more complicated actions without creating new methods.
///                 key: (key_code, key_modifiers) => {
///                     |handler: &mut MyStruct| {
///                         handler.method_1();
///
///                         for _ in 0..3 {
///                             handler.default_method("q");
///                         }
///                     }
///                 }
///                 // A mode may lack a default action.
///             ]
///         )
///
///         ...
///
///         handler
///     }
///
///     // Actions cannot take any input, since the only thing they know is that they
///     // have been triggered.
///     fn method_1(&mut self) { ... }
///     fn method_2(&mut self) { ... }
///     fn method_3(&mut self) { ... }
///
///     // The default action takes in the symbol of the key that was pressed. It is
///     // mostly used to write text to the screen.
///     fn default_method(&mut self, &str) { ... }
/// }
/// ```
/// 
#[macro_export]
macro_rules! map_actions {
    ($handler:ident: $handler_type:ty, $mode_list:ident;
        $($mode:expr => [
            $(
                $(($code:expr, $modif:expr), $name:expr => { $cmd:expr })?
                $(key: ($lock_code:expr, $lock_modif:expr)   => { $lock_cmd:expr })?
                $(name: $free_name:expr                       => { $free_cmd:expr })?
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
                    mode.add_action($cmd, Some($name), Some(key));
                )?
                $(
                    let key = KeyEvent::new($lock_code, $lock_modif);
                    mode.add_action($lock_cmd, None, Some(key));
                )?
                $(mode.add_action($free_cmd, Some($free_name), None);)?
            )*
        )*
    }
}

/// Implements `InputHandler` automatically.
///
/// Note: This macro must be called alongside map_actions! for proper mapping
/// implementation.
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// struct MyStruct {
///     ...,
///     mappings: Mappings<MyStruct>,
/// }
///
/// impl MyStruct {
///     fn new() -> MyStruct {
///         map_actions!( ... );
///     }
/// }
///
/// impl_input_handler!(MyStruct, mappings);
/// ```
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

            fn bind_action(&mut self, name: &str, key: KeyEvent) {
                for mode in &mut self.$mode_list.modes {
                    for action in &mut mode.actions {
                        if let Some(option_name) = action.name.clone() {
                            if option_name == name {
                                action.key = Some(key);
                                return;
                            }
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
// TODO: Actually use this.
/// Enum detailing how to handle the addition of bindings with the same KeyEvent
pub enum MapppingAddition {
    Over,
    Under,
    Merge,
}
