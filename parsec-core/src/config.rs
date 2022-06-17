/// If and how to wrap lines at the end of the screen.
#[derive(Copy, Clone, Debug)]
pub enum WrapType {
    Width,
    Capped(u16),
    Word,
    NoWrap,
}

#[derive(Copy, Clone, Debug)]
pub enum LineNumbers {
    None,
    Normal,
    Relative,
    Hybrid
}

// TODO: Move options to a centralized option place.
// TODO: Make these private.
/// Options specific to file printing.
#[derive(Copy, Clone, Debug)]
pub struct FileOptions {
    pub wrap_type: WrapType,
    pub scrolloff: u16,
    pub x_spacing: u16,
    pub line_numbers: LineNumbers,
}

/// The options of the text editor.
#[derive(Copy, Clone, Debug)]
pub struct Options {
    pub file_options: FileOptions,
}


