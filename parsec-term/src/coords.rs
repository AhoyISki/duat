#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Coord {
    x: u16,
    y: u16,
}

impl Coord {
    pub fn new(x: u16, y: u16) -> Self {
        Self { x, y }
    }
}

impl Display for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.y, self.x))
    }
}

#[derive(Clone, Copy)]
pub struct Coords {
    tl: Coord,
    br: Coord,
}

impl Debug for Coords {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{},{}:{}", self.tl.x, self.tl.y, self.br.x, self.br.y))
    }
}

impl Coords {
    /// Returns a new instance of [`Coords`].
    pub fn new(tl: Coord, br: Coord) -> Self {
        Self { tl, br }
    }

    fn empty_on(&self, side: Side) -> Self {
        let tr = Coord::new(self.br.x, self.tl.y);
        let bl = Coord::new(self.tl.x, self.br.y);

        match side {
            Side::Top => Coords::new(self.tl, tr),
            Side::Right => Coords::new(tr, self.br),
            Side::Bottom => Coords::new(bl, self.br),
            Side::Left => Coords::new(self.tl, bl),
        }
    }

    fn len(&self, axis: Axis) -> usize {
        match axis {
            Axis::Horizontal => self.width(),
            Axis::Vertical => self.height(),
        }
    }

    fn width(&self) -> usize {
        (self.br.x - self.tl.x) as usize
    }

    fn height(&self) -> usize {
        (self.br.y - self.tl.y) as usize
    }

    fn ortho_corner(&self, axis: Axis) -> Coord {
        match axis {
            Axis::Horizontal => Coord {
                x: self.br.x,
                y: self.tl.y,
            },
            Axis::Vertical => Coord {
                x: self.tl.x,
                y: self.br.y,
            },
        }
    }
}

impl Coords {
    fn add_to_side(&mut self, side: Side, len_diff: i16) {
        match side {
            Side::Top => self.tl.y = self.tl.y.saturating_add_signed(-len_diff),
            Side::Left => self.tl.x = self.tl.x.saturating_add_signed(-len_diff),
            Side::Bottom => self.br.y = self.br.y.saturating_add_signed(len_diff),
            Side::Right => self.br.x = self.br.x.saturating_add_signed(len_diff),
        }
    }
}

impl Display for Coords {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{},{}", self.tl, self.br))
    }
}
