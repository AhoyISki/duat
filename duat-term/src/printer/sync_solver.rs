use cassowary::{
    AddConstraintError, RemoveConstraintError, Solver, Variable, strength::STRONG,
};
use duat_core::ui::Axis;

use super::VarPoint;
use crate::Equality;

pub struct SyncSolver {
    solver: Solver,
    eqs_to_add: Vec<Equality>,
    eqs_to_remove: Vec<Equality>,
    floating: Vec<FloatingCenter>,
}

impl SyncSolver {
    pub fn new(max: &VarPoint, width: f64, height: f64) -> Self {
        let mut solver = Solver::new();
        let strong = STRONG + 3.0;
        solver.add_edit_variable(max.x(), strong).unwrap();
        solver.suggest_value(max.x(), width).unwrap();

        solver.add_edit_variable(max.y(), strong).unwrap();
        solver.suggest_value(max.y(), height).unwrap();

        SyncSolver {
            solver,
            eqs_to_add: Vec::new(),
            eqs_to_remove: Vec::new(),
            floating: Vec::new(),
        }
    }

    pub fn update(
        &mut self,
        change_max: bool,
        max: VarPoint,
        mut assign_floating: bool,
    ) -> Result<Vec<(Variable, f64)>, AddConstraintError> {
        for eq in self.eqs_to_remove.drain(..) {
            match self.solver.remove_constraint(&eq) {
                Ok(_) | Err(RemoveConstraintError::UnknownConstraint) => {}
                Err(err) => panic!("{err:?}"),
            }
        }
        self.solver.add_constraints(&self.eqs_to_add)?;
        self.eqs_to_add.clear();

        if change_max {
            let (width, height) = crossterm::terminal::size().unwrap();
            let (width, height) = (width as f64, height as f64);

            self.solver.suggest_value(max.x(), width).unwrap();
            self.solver.suggest_value(max.y(), height).unwrap();
        }

        let mut changes = Vec::new();
        let mut new_changes = self.solver.fetch_changes().to_vec();

        loop {
            let mut to_update = self
                .floating
                .iter()
                .filter(|fl| {
                    new_changes.iter().any(|(var, _)| fl.deps.contains(var)) || assign_floating
                })
                .peekable();

            if to_update.peek().is_none() {
                changes.extend(new_changes);
                break;
            }

            for floating in to_update {
                let max = match floating.axis {
                    Axis::Horizontal => self.solver.get_value(max.x),
                    Axis::Vertical => self.solver.get_value(max.y),
                };
                let lhs = self.solver.get_value(floating.deps[0]);
                let rhs = self.solver.get_value(floating.deps[1]);

                if let Some(len) = floating.desired_len
                    && (lhs >= len || max - rhs >= len)
                {
                    match (floating.prefers_before, lhs >= len, max - rhs >= len) {
                        (true, true, true | false) | (false, true, false) => {
                            self.solver.suggest_value(floating.len_var, len).unwrap();
                            self.solver
                                .suggest_value(floating.center_var, lhs - len / 2.0)
                                .unwrap();
                        }
                        (true, false, true) | (false, true | false, true) => {
                            self.solver.suggest_value(floating.len_var, len).unwrap();
                            self.solver
                                .suggest_value(floating.center_var, rhs + len / 2.0)
                                .unwrap();
                        }
                        (true | false, false, false) => unreachable!(),
                    };
                } else {
                    let value = if lhs > max - rhs {
                        lhs / 2.0
                    } else {
                        rhs + (max - rhs) / 2.0
                    };

                    self.solver
                        .suggest_value(floating.len_var, lhs.max(max - rhs))
                        .unwrap();
                    self.solver
                        .suggest_value(floating.center_var, value)
                        .unwrap();
                }
            }

            assign_floating = false;
            changes.append(&mut new_changes);
            new_changes = self.solver.fetch_changes().to_vec();
        }

        Ok(changes)
    }

    pub fn add_eqs(&mut self, eqs: impl IntoIterator<Item = Equality>) {
        self.eqs_to_add.extend(eqs);
    }

    pub fn remove_eqs(&mut self, eqs: impl IntoIterator<Item = Equality>) {
        let eqs: Vec<Equality> = eqs.into_iter().collect();
        if self.eqs_to_add.extract_if(.., |e| eqs.contains(e)).count() == 0 {
            self.eqs_to_remove.extend(eqs);
        }
    }

    pub fn new_floating_center(
        &mut self,
        variables: &mut super::variables::Variables,
        deps: [Variable; 2],
        len: Option<f32>,
        axis: Axis,
        prefers_before: bool,
    ) -> [Variable; 2] {
        let center_var = variables.new_var();
        let len_var = variables.new_var();

        self.solver
            .add_edit_variable(center_var, STRONG - 1.0)
            .unwrap();
        self.solver
            .add_edit_variable(len_var, STRONG - 1.0)
            .unwrap();

        self.floating.push(FloatingCenter {
            center_var,
            len_var,
            desired_len: len.map(|len| len as f64),
            deps,
            axis,
            prefers_before,
        });

        [center_var, len_var]
    }
}

/// Represents the "center" of a floaging [`Rect`]
///
/// This makes use of an edit [`Variable`], allowing for
/// dinamically positioned floating [`Rect`]s.
///
/// [`Rect`]: super::Rect
struct FloatingCenter {
    center_var: Variable,
    len_var: Variable,
    desired_len: Option<f64>,
    deps: [Variable; 2],
    axis: Axis,
    prefers_before: bool,
}
