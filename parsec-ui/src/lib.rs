pub trait OutputArea {
    fn print(&self, text: dyn Printable);
}

pub trait CursorArea {
    fn move_cursor(&self, cursor: usize, pos: CursorPos);
}

pub trait Printable {}

#[derive(Copy, Clone)]
pub struct CursorPos {
    pub x: i32,
    pub y: i32
}
