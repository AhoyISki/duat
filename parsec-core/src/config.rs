use crate::output::OutputPos;

/// If and how to wrap lines at the end of the screen.
#[derive(Copy, Clone, Debug)]
pub enum WrapMethod {
    Width,
    Capped(u16),
    Word,
    NoWrap,
}

/// How to show the line numbers on screen.
#[derive(Copy, Clone, Debug)]
pub enum LineNumbers {
    None,
    Absolute,
    Relative,
    Hybrid
}

/// Where the tabs are placed on screen, can be regular or varied.
#[derive(Clone, Debug)]
pub enum TabPlaces {
    Regular(u16),
    Varied(Vec<u16>),
}

impl TabPlaces {
    /// Returns the amount of spaces between a position and the next tab place.
    pub fn get_tab_len(&self, x: u16) -> u16 {
        match self {
            TabPlaces::Regular(step) => (step - (x % step)),
            TabPlaces::Varied(steps) => {
                steps.iter().find(|&s| *s > x).expect("not enough tabs") - x
            }
        }
    }
}

// TODO: Move options to a centralized option place.
// TODO: Make these private.
/// Options specific to file printing.
#[derive(Clone, Debug)]
pub struct FileOptions {
    pub wrap_method: WrapMethod,
    pub scrolloff: OutputPos,
    pub line_numbers: LineNumbers,
    pub tabs: TabPlaces,
    pub wrap_indent: bool,
    pub tabs_as_spaces: bool,
}

/// The options of the text editor.
#[derive(Clone, Debug)]
pub struct Options {
    pub file_options: FileOptions,
}
