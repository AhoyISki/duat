use std::sync::{
    Arc,
    atomic::{AtomicBool, AtomicU32, Ordering},
};

use cassowary::Variable;
use duat_core::ui::Axis;

use super::SavedVar;
use crate::area::Coord;

/// A point on the screen, which can be calculated by [`cassowary`]
/// and interpreted by `duat_term`.
#[derive(Clone, PartialEq, PartialOrd)]
pub struct VarPoint {
    y: VarValue,
    x: VarValue,
}

impl VarPoint {
    pub(super) fn new(x: VarValue, y: VarValue) -> Self {
        Self { y, x }
    }

    /// Returns a new instance of [`VarPoint`]
    pub fn new_from_map(vars: &mut Vec<(Variable, SavedVar)>) -> Self {
        let vp = VarPoint { x: VarValue::new(), y: VarValue::new() };

        vars.push((vp.x.var, SavedVar::val(&vp.x)));
        vars.push((vp.y.var, SavedVar::val(&vp.y)));

        vp
    }

	/// Sets the values to `(0, 0)`
	///
	/// This is done when a widget is removed.
    pub fn set_to_zero(&self) {
        self.y.value.store(0, Ordering::Release);
        self.x.value.store(0, Ordering::Release);
    }

    pub fn coord(&self) -> Coord {
        let x = self.x.value.load(Ordering::Relaxed);
        let y = self.y.value.load(Ordering::Relaxed);

        Coord::new(x, y)
    }

    pub fn x_var(&self) -> Variable {
        self.x.var
    }

    pub fn y_var(&self) -> Variable {
        self.y.var
    }

    pub fn y(&self) -> &VarValue {
        &self.y
    }

    pub fn x(&self) -> &VarValue {
        &self.x
    }

    pub fn on_axis(&self, axis: Axis) -> &VarValue {
        match axis {
            Axis::Horizontal => &self.x,
            Axis::Vertical => &self.y,
        }
    }
}

impl std::fmt::Debug for VarPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{:?}: {}, {:?}: {}",
            self.x.var,
            self.x.value.load(Ordering::Relaxed),
            self.y.var,
            self.y.value.load(Ordering::Relaxed)
        ))
    }
}

/// A [`Variable`], attached to its value, which is automatically kept
/// up to date.
#[derive(Clone)]
pub struct VarValue {
    pub(super) var: Variable,
    pub(super) value: Arc<AtomicU32>,
    pub(super) has_changed: Arc<AtomicBool>,
}

impl VarValue {
    /// Returns a new instance of [`VarValue`]
    pub(super) fn new() -> Self {
        Self {
            var: Variable::new(),
            value: Arc::new(AtomicU32::new(0)),
            has_changed: Arc::new(AtomicBool::new(false)),
        }
    }

    pub fn var(&self) -> Variable {
        self.var
    }

    pub fn value(&self) -> u32 {
        self.value.load(Ordering::Acquire)
    }

    pub fn has_changed(&self) -> bool {
        self.has_changed.swap(false, Ordering::Release)
    }
}

impl PartialEq<VarValue> for VarValue {
    fn eq(&self, other: &VarValue) -> bool {
        self.var == other.var
    }
}

impl PartialOrd<VarValue> for VarValue {
    fn partial_cmp(&self, other: &VarValue) -> Option<std::cmp::Ordering> {
        Some(
            self.value
                .load(Ordering::Acquire)
                .cmp(&other.value.load(Ordering::Acquire)),
        )
    }
}

impl std::ops::BitOr<cassowary::WeightedRelation> for &VarValue {
    type Output = cassowary::PartialConstraint;

    fn bitor(self, rhs: cassowary::WeightedRelation) -> cassowary::PartialConstraint {
        self.var | rhs
    }
}

impl std::ops::BitOr<&VarValue> for cassowary::PartialConstraint {
    type Output = cassowary::Constraint;

    fn bitor(self, rhs: &VarValue) -> cassowary::Constraint {
        self | rhs.var
    }
}

impl std::ops::Add<f64> for &VarValue {
    type Output = cassowary::Expression;

    fn add(self, rhs: f64) -> cassowary::Expression {
        self.var + rhs
    }
}

impl std::ops::Add<&VarValue> for f64 {
    type Output = cassowary::Expression;

    fn add(self, rhs: &VarValue) -> cassowary::Expression {
        self + rhs.var
    }
}

impl std::ops::Add<&VarValue> for &VarValue {
    type Output = cassowary::Expression;

    fn add(self, rhs: &VarValue) -> cassowary::Expression {
        self.var + rhs.var
    }
}

impl std::ops::Add<cassowary::Term> for &VarValue {
    type Output = cassowary::Expression;

    fn add(self, rhs: cassowary::Term) -> cassowary::Expression {
        self.var + rhs
    }
}

impl std::ops::Add<&VarValue> for cassowary::Term {
    type Output = cassowary::Expression;

    fn add(self, rhs: &VarValue) -> cassowary::Expression {
        self + rhs.var
    }
}

impl std::ops::Add<cassowary::Expression> for &VarValue {
    type Output = cassowary::Expression;

    fn add(self, rhs: cassowary::Expression) -> cassowary::Expression {
        self.var + rhs
    }
}

impl std::ops::Add<&VarValue> for cassowary::Expression {
    type Output = cassowary::Expression;

    fn add(self, rhs: &VarValue) -> cassowary::Expression {
        self + rhs.var
    }
}

impl std::ops::Neg for &VarValue {
    type Output = cassowary::Term;

    fn neg(self) -> cassowary::Term {
        -self.var
    }
}

impl std::ops::Sub<f64> for &VarValue {
    type Output = cassowary::Expression;

    fn sub(self, rhs: f64) -> cassowary::Expression {
        self.var - rhs
    }
}

impl std::ops::Sub<&VarValue> for f64 {
    type Output = cassowary::Expression;

    #[allow(clippy::suspicious_arithmetic_impl)]
    fn sub(self, rhs: &VarValue) -> cassowary::Expression {
        self + rhs.var
    }
}

impl std::ops::Sub<&VarValue> for &VarValue {
    type Output = cassowary::Expression;

    fn sub(self, rhs: &VarValue) -> cassowary::Expression {
        self.var - rhs.var
    }
}

impl std::ops::Sub<cassowary::Term> for &VarValue {
    type Output = cassowary::Expression;

    fn sub(self, rhs: cassowary::Term) -> cassowary::Expression {
        self.var - rhs
    }
}

impl std::ops::Sub<&VarValue> for cassowary::Term {
    type Output = cassowary::Expression;

    fn sub(self, rhs: &VarValue) -> cassowary::Expression {
        self - rhs.var
    }
}

impl std::ops::Sub<cassowary::Expression> for &VarValue {
    type Output = cassowary::Expression;

    fn sub(self, rhs: cassowary::Expression) -> cassowary::Expression {
        self.var - rhs
    }
}

impl std::ops::Sub<&VarValue> for cassowary::Expression {
    type Output = cassowary::Expression;

    fn sub(self, rhs: &VarValue) -> cassowary::Expression {
        self - rhs.var
    }
}

impl std::ops::Mul<f64> for &VarValue {
    type Output = cassowary::Term;

    fn mul(self, rhs: f64) -> cassowary::Term {
        self.var * rhs
    }
}

impl std::ops::Mul<VarValue> for f64 {
    type Output = cassowary::Term;

    fn mul(self, rhs: VarValue) -> cassowary::Term {
        self * rhs.var
    }
}

impl std::ops::Div<f64> for VarValue {
    type Output = cassowary::Term;

    fn div(self, rhs: f64) -> cassowary::Term {
        self.var / rhs
    }
}
