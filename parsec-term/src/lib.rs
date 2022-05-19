#[derive(Copy, Clone)]
pub struct TermPos {
    pub x: u16,
    pub y: u16,
}

#[derive(Copy, Clone)]
pub struct TermArea {
    pub origin: TermPos,
    pub end: TermPos,
}

impl TermArea {
    pub fn dims(&self) -> (u16, u16) {
        (self.end.x - self.origin.x, self.end.y - self.origin.y)
    }

    pub fn contains(&self, pos: TermPos) -> bool {
        pos.x >= self.origin.x && pos.x < self.end.x &&
        pos.y >= self.origin.x && pos.y < self.end.y
    }
}
