use cassowary::Variable;
use duat_core::ui::Axis;


/// A point on the screen, which can be calculated by [`cassowary`]
/// and interpreted by `duat_term`.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct VarPoint {
    y: Variable,
    x: Variable,
}

impl VarPoint {
    pub(super) fn new(x: Variable, y: Variable) -> Self {
        Self { y, x }
    }

    pub fn x(&self) -> Variable {
        self.x
    }

    pub fn y(&self) -> Variable {
        self.y
    }

    pub fn on_axis(&self, axis: Axis) -> Variable {
        match axis {
            Axis::Horizontal => self.x,
            Axis::Vertical => self.y,
        }
    }
}
