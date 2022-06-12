/// If and how to wrap lines at the end of the screen.
#[derive(Copy, Clone, Debug)]
pub enum WrapType {
    Width,
    Capped(u16),
    Word,
    NoWrap,
}

// TODO: Move options to a centralized option place.
// TODO: Make these private.
/// Options specific to file printing.
#[derive(Copy, Clone, Debug)]
pub struct FileOptions {
    pub wrap_type: WrapType,
    pub cursor_y_spacing: u16,
    pub cursor_x_spacing: u16,
}

/// The options of the text editor.
#[derive(Copy, Clone, Debug)]
pub struct Options {
    pub file_options: FileOptions,
}


