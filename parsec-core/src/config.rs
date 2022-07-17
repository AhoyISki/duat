/// If and how to wrap lines at the end of the screen.
#[derive(Copy, Clone, Debug)]
pub enum WrapMethod {
    Width,
    Capped(u16),
    Word,
    NoWrap,
}

// Pretty much only exists because i wanted one of these with usize as its builtin type.
#[derive(Copy, Clone, Debug)]
pub struct ScrollOff {
    pub d_y: usize,
    pub d_x: usize,
}

impl WrapMethod {
    pub fn is_wrapped(&self) -> bool {
        match self {
            WrapMethod::NoWrap => false,
            _ => true,
        }
    }
}

/// How to show the line numbers on screen.
#[derive(Copy, Clone, Debug)]
pub enum LineNumbers {
    None,
    Absolute,
    Relative,
    Hybrid,
}

/// Where the tabs are placed on screen, can be regular or varied.
#[derive(Clone, Debug)]
pub enum TabPlaces {
    Regular(usize),
    Varied(Vec<usize>),
}

impl TabPlaces {
    /// Returns the amount of spaces between a position and the next tab place.
    pub fn get_tab_len(&self, x: usize) -> usize {
        match self {
            TabPlaces::Regular(step) => (step - (x % step)),
            TabPlaces::Varied(steps) => {
                steps.iter().find(|&s| *s > x).expect("not enough tabs") - x
            },
        }
    }
}

// TODO: Move options to a centralized option place.
// TODO: Make these private.
/// Options specific to file printing.
#[derive(Clone, Debug)]
pub struct FileOptions {
    /// How to wrap the file.
    pub wrap_method: WrapMethod,
    /// The distance between the cursor and the edges of the screen when scrolling.
    pub scrolloff: ScrollOff,
    /// How to show the line numbers.
    pub line_numbers: LineNumbers,
    /// How to indent.
    pub tabs: TabPlaces,
    /// Wether to indent wrapped lines or not.
    pub wrap_indent: bool,
    /// Wether to convert tabs to spaces.
    pub tabs_as_spaces: bool,
}

/// The options of the text editor.
#[derive(Clone, Debug)]
pub struct Options {
    pub file_options: FileOptions,
}
