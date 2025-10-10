use cassowary::{AddConstraintError, RemoveConstraintError, Solver, Variable, strength::STRONG};
use duat_core::{text::SpawnId, ui::Axis};

use super::VarPoint;
use crate::{Equality, area::Coord};

/// A synchronization wrapper around the cassowary constraint
/// [`Solver`]
pub struct SyncSolver {
    solver: Solver,
    eqs_to_add: Vec<Equality>,
    eqs_to_remove: Vec<Equality>,
    spawns: Vec<SpawnedCenter>,
}

impl SyncSolver {
    /// Returns a new `SyncSolver`
    ///
    /// The `SyncSolver` is responsible for efficiently adding and
    /// removing equalities from the internal cassowayr constraint
    /// [`Solver`], updating only once requested.
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
            spawns: Vec::new(),
        }
    }

    /// Adds and removes remaining equalities and fetches changes
    ///
    /// This function will also internally update the floating widgets
    /// by suggesting new valid values for them, possibly making them
    /// switch sides from their spawned regions.
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

        // Update the floating Widgets
        loop {
            let mut to_update = self
                .spawns
                .iter()
                .filter(|fl| {
                    new_changes.iter().any(|(var, _)| fl.deps.contains(*var)) || assign_floating
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
                let [lhs, rhs] = floating.deps.get_values(&self.solver);

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

    /// Adds equalities to the `SyncSolver`
    pub fn add_eqs(&mut self, eqs: impl IntoIterator<Item = Equality>) {
        self.eqs_to_add.extend(eqs);
    }

    /// Removes equalities from the `SyncSolver`
    pub fn remove_eqs(&mut self, eqs: impl IntoIterator<Item = Equality>) {
        let eqs: Vec<Equality> = eqs.into_iter().collect();
        if self.eqs_to_add.extract_if(.., |e| eqs.contains(e)).count() == 0 {
            self.eqs_to_remove.extend(eqs);
        }
    }

    /// Creates a new center for a widget spawned widget
    ///
    /// Returns the center variable for this new widget and the lenght
    /// for said widget in the given axis, respectively
    pub fn new_widget_spawned(
        &mut self,
        variables: &mut super::variables::Variables,
        [start, end]: [Variable; 2],
        len: Option<f32>,
        axis: Axis,
        prefers_before: bool,
    ) -> [Variable; 2] {
        let center_var = variables.new_var();
        let len_var = variables.new_var();

        self.solver
            .add_edit_variable(center_var, STRONG - 2.0)
            .unwrap();
        self.solver
            .add_edit_variable(len_var, STRONG - 2.0)
            .unwrap();

        self.spawns.push(SpawnedCenter {
            center_var,
            len_var,
            desired_len: len.map(|len| len as f64),
            deps: CenterDeps::Widget(start, end),
            axis,
            prefers_before,
        });

        [center_var, len_var]
    }

    /// Creates a new center for a text spawned widget
    ///
    /// Returns the center variable for this new widget and the lenght
    /// for said widget in the given axis, respectively. Also returns
    /// the [`VarPoint`] representing the top left corner of a
    /// terminal cell.
    pub fn new_text_spawned(
        &mut self,
        variables: &mut super::variables::Variables,
        id: SpawnId,
        len: Option<f32>,
        axis: Axis,
        prefers_before: bool,
    ) -> ([Variable; 2], VarPoint) {
        let center_var = variables.new_var();
        let len_var = variables.new_var();

        self.solver
            .add_edit_variable(center_var, STRONG - 2.0)
            .unwrap();
        self.solver
            .add_edit_variable(len_var, STRONG - 2.0)
            .unwrap();

        let tl = VarPoint::new(variables.new_var(), variables.new_var());
        self.solver.add_edit_variable(tl.x, STRONG - 1.0).unwrap();
        self.solver.add_edit_variable(tl.y, STRONG - 1.0).unwrap();

        self.spawns.push(SpawnedCenter {
            center_var,
            len_var,
            desired_len: len.map(|len| len as f64),
            deps: match axis {
                Axis::Horizontal => CenterDeps::TextHorizontal(id, tl, None),
                Axis::Vertical => CenterDeps::TextVertical(id, tl),
            },
            axis,
            prefers_before,
        });

        ([center_var, len_var], tl)
    }

    /// Moves the spawn point of a [`SpawnId`]
    ///
    /// The `char_width` argument is used to determine where the right
    /// edge lies. It's ignored for vertically spawned widgets
    pub fn move_spawn_to(&mut self, id: SpawnId, coord: Coord, char_width: u32) {
        let Some(center) = self.spawns.iter_mut().find(|c| c.deps.matches_id(id)) else {
            unreachable!("Whoopsie Daisy, that's not supposed to happen.");
        };

        match center.deps {
            CenterDeps::Widget(..) => unreachable!(),
            CenterDeps::TextHorizontal(_, tl, ref mut len) => {
                self.solver.suggest_value(tl.x, coord.x as f64).unwrap();
                self.solver.suggest_value(tl.y, coord.y as f64).unwrap();
                *len = Some(char_width)
            }
            CenterDeps::TextVertical(_, tl) => {
                self.solver.suggest_value(tl.x, coord.x as f64).unwrap();
                self.solver.suggest_value(tl.y, coord.y as f64).unwrap();
            }
        }
    }
}

/// Represents the "center" of a floaging [`Rect`]
///
/// This makes use of an edit [`Variable`], allowing for
/// dinamically positioned floating [`Rect`]s.
///
/// [`Rect`]: super::Rect
struct SpawnedCenter {
    center_var: Variable,
    len_var: Variable,
    desired_len: Option<f64>,
    deps: CenterDeps,
    axis: Axis,
    prefers_before: bool,
}

/// What kind of dependency a [`SpawnedCenter`] has
enum CenterDeps {
    Widget(Variable, Variable),
    TextHorizontal(SpawnId, VarPoint, Option<u32>),
    TextVertical(SpawnId, VarPoint),
}

impl CenterDeps {
    /// The current value of the edges that define a [`SpawnedCenter`]
    fn get_values(&self, solver: &Solver) -> [f64; 2] {
        match self {
            CenterDeps::Widget(start, end) => [start, end].map(|v| solver.get_value(*v)),
            CenterDeps::TextHorizontal(_, tl, len) => {
                let left = solver.get_value(tl.x);
                [left, left + len.unwrap_or(1) as f64]
            }
            CenterDeps::TextVertical(_, tl) => {
                let top = solver.get_value(tl.y);
                [top, top + 1.0]
            }
        }
    }

    /// Wether the dependencies contain a certain [`Variable`]
    fn contains(&self, var: Variable) -> bool {
        match self {
            CenterDeps::Widget(start, end) => *start == var || *end == var,
            CenterDeps::TextHorizontal(_, tl, _) => tl.x == var,
            CenterDeps::TextVertical(_, tl) => tl.y == var,
        }
    }

    /// Wether this dependency is tied to a given [`SpawnId`]
    fn matches_id(&self, other: SpawnId) -> bool {
        match self {
            CenterDeps::Widget(..) => false,
            CenterDeps::TextHorizontal(id, ..) | CenterDeps::TextVertical(id, _) => *id == other,
        }
    }
}
