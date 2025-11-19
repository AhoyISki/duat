use duat_core::ui::{Axis, SpawnId};
use kasuari::{AddConstraintError, Expression, RemoveConstraintError, Solver, Variable};

use super::VarPoint;
use crate::{DYN_SPAWN_POS_PRIO, Equality, SPAWN_DIMS_PRIO, area::Coord, layout::Frame};

/// A synchronization wrapper around the kasuari constraint
/// [`Solver`]
pub struct SyncSolver {
    solver: Solver,
    eqs_to_add: Vec<Equality>,
    eqs_to_remove: Vec<Equality>,
    spawns: Vec<Spawn>,
}

impl SyncSolver {
    /// Returns a new `SyncSolver`
    ///
    /// The `SyncSolver` is responsible for efficiently adding and
    /// removing equalities from the internal kasuari constraint
    /// [`Solver`], updating only once requested.
    pub fn new(max: VarPoint, width: f64, height: f64) -> Self {
        let mut solver = Solver::new();
        let strength = kasuari::Strength::STRONG;
        solver.add_edit_variable(max.x(), strength).unwrap();
        solver.suggest_value(max.x(), width).unwrap();

        solver.add_edit_variable(max.y(), strength).unwrap();
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
        self.solver.add_constraints(self.eqs_to_add.drain(..))?;

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
                .filter(|spawn| {
                    new_changes.iter().any(|(var, _)| spawn.deps.contains(*var)) || assign_floating
                })
                .peekable();

            if to_update.peek().is_none() {
                changes.extend(new_changes);
                break;
            }

            for spawn in to_update {
                let max = match spawn.axis {
                    Axis::Horizontal => self.solver.get_value(max.x),
                    Axis::Vertical => self.solver.get_value(max.y),
                };
                let [lhs, rhs] = spawn.deps.get_values(&self.solver);
                let [lhs_fr, rhs_fr] = spawn.show_frames.map(|show| show as usize as f64);
                let fr_len = lhs_fr + rhs_fr;

                if spawn.is_inside {
                    let available = rhs - lhs - fr_len;
                    let len = spawn.desired_len.unwrap_or(available).min(available);

                    self.solver.suggest_value(spawn.len_var, len).unwrap();
                    let c = if spawn.prefers_before {
                        lhs_fr + lhs + len / 2.0
                    } else {
                        rhs_fr + rhs - len / 2.0
                    };
                    self.solver.suggest_value(spawn.center_var, c).unwrap();
                } else if let Some(len) = spawn.desired_len
                    && (lhs >= len + fr_len || max - rhs >= len + fr_len)
                {
                    match (
                        spawn.prefers_before,
                        lhs >= len + fr_len,
                        max - rhs >= len + fr_len,
                    ) {
                        (true, true, true | false) | (false, true, false) => {
                            self.solver.suggest_value(spawn.len_var, len).unwrap();
                            self.solver
                                .suggest_value(spawn.center_var, lhs - (rhs_fr + len / 2.0))
                                .unwrap();
                        }
                        (true, false, true) | (false, true | false, true) => {
                            self.solver.suggest_value(spawn.len_var, len).unwrap();
                            self.solver
                                .suggest_value(spawn.center_var, rhs + (lhs_fr + len / 2.0))
                                .unwrap();
                        }
                        (true | false, false, false) => unreachable!(),
                    };
                } else if lhs <= fr_len && max - rhs <= fr_len {
                    self.solver.suggest_value(spawn.len_var, 0.0).unwrap();
                    self.solver.suggest_value(spawn.center_var, 0.0).unwrap();
                } else {
                    let center = if lhs > max - rhs {
                        lhs_fr + (lhs - fr_len) / 2.0
                    } else {
                        lhs_fr + rhs + (max - rhs - fr_len) / 2.0
                    };

                    self.solver
                        .suggest_value(spawn.len_var, lhs.max(max - rhs) - fr_len)
                        .unwrap();
                    self.solver.suggest_value(spawn.center_var, center).unwrap();
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

    /// Sets the [`Frame`] for a [`SpawnId`]
    pub fn set_frame(&mut self, id: SpawnId, frame: &Frame) {
        let Some(spawn) = self.spawns.iter_mut().find(|spawn| spawn.id == id) else {
            return;
        };

        match spawn.axis {
            Axis::Horizontal => spawn.show_frames = [frame.left, frame.right],
            Axis::Vertical => spawn.show_frames = [frame.above, frame.below],
        }
    }

    /// Creates a new center for a widget spawned widget
    ///
    /// Returns the center variable for this new widget and the lenght
    /// for said widget in the given axis, respectively
    pub fn new_widget_spawn(
        &mut self,
        spawn_id: SpawnId,
        variables: &mut super::variables::Variables,
        [tl, br]: [VarPoint; 2],
        (len, axis): (Option<f32>, Axis),
        (prefers_before, is_inside): (bool, bool),
    ) -> [Variable; 2] {
        let center_var = variables.new_var();
        let len_var = variables.new_var();

        self.solver
            .add_edit_variable(center_var, SPAWN_DIMS_PRIO)
            .unwrap();
        self.solver
            .add_edit_variable(len_var, SPAWN_DIMS_PRIO)
            .unwrap();

        self.spawns.push(Spawn {
            id: spawn_id,
            center_var,
            len_var,
            desired_len: len.map(|len| len as f64),
            deps: CenterDeps::Widget(tl, br, axis),
            axis,
            prefers_before,
            is_inside,
            show_frames: [false; 2],
        });

        [center_var, len_var]
    }

    /// Creates a new center for a text spawned widget
    ///
    /// Returns the center variable for this new widget and the lenght
    /// for said widget in the given axis, respectively. Also returns
    /// the [`VarPoint`] representing the top left corner of a
    /// terminal cell.
    pub fn new_text_spawn(
        &mut self,
        spawn_id: SpawnId,
        variables: &mut super::variables::Variables,
        len: Option<f32>,
        axis: Axis,
        prefers_before: bool,
    ) -> ([Variable; 2], VarPoint) {
        let center_var = variables.new_var();
        let len_var = variables.new_var();

        self.solver
            .add_edit_variable(center_var, SPAWN_DIMS_PRIO)
            .unwrap();
        self.solver
            .add_edit_variable(len_var, SPAWN_DIMS_PRIO)
            .unwrap();

        let tl = VarPoint::new(variables.new_var(), variables.new_var());
        self.solver
            .add_edit_variable(tl.x, DYN_SPAWN_POS_PRIO)
            .unwrap();
        self.solver
            .add_edit_variable(tl.y, DYN_SPAWN_POS_PRIO)
            .unwrap();

        self.spawns.push(Spawn {
            id: spawn_id,
            center_var,
            len_var,
            desired_len: len.map(|len| len as f64),
            deps: match axis {
                Axis::Horizontal => CenterDeps::TextHorizontal(tl, None),
                Axis::Vertical => CenterDeps::TextVertical(tl),
            },
            axis,
            prefers_before,
            is_inside: false,
            show_frames: [false; 2],
        });

        ([center_var, len_var], tl)
    }

    /// Moves the spawn point of a [`SpawnId`]
    ///
    /// The `char_width` argument is used to determine where the right
    /// edge lies. It's ignored for vertically spawned widgets
    pub fn move_spawn_to(&mut self, id: SpawnId, coord: Coord, char_width: u32) {
        let Some(center) = self.spawns.iter_mut().find(|spawn| spawn.id == id) else {
            unreachable!("Whoopsie Daisy, that's not supposed to happen.");
        };

        match center.deps {
            CenterDeps::Widget(..) => unreachable!(),
            CenterDeps::TextHorizontal(tl, ref mut len) => {
                self.solver.suggest_value(tl.x, coord.x as f64).unwrap();
                self.solver.suggest_value(tl.y, coord.y as f64).unwrap();
                *len = Some(char_width)
            }
            CenterDeps::TextVertical(tl) => {
                self.solver.suggest_value(tl.x, coord.x as f64).unwrap();
                self.solver.suggest_value(tl.y, coord.y as f64).unwrap();
            }
        }
    }

    /// Removes the spawn info associated with a [`SpawnId`]
    ///
    /// Returns all edit variables.
    pub fn remove_spawn_info(&mut self, id: SpawnId) -> Option<ReturnedEditVars> {
        self.spawns
            .extract_if(.., |spawn| spawn.id == id)
            .next()
            .map(|c| {
                self.solver.remove_edit_variable(c.center_var).unwrap();
                self.solver.remove_edit_variable(c.len_var).unwrap();
                if let CenterDeps::TextHorizontal(tl, _) | CenterDeps::TextVertical(tl) = c.deps {
                    self.solver.remove_edit_variable(tl.x).unwrap();
                    self.solver.remove_edit_variable(tl.y).unwrap();
                    ReturnedEditVars::TextSpawned([c.center_var, c.len_var, tl.x, tl.y])
                } else {
                    ReturnedEditVars::WidgetSpawned([c.center_var, c.len_var])
                }
            })
    }

    /// Returns the spawn info associated with a [`SpawnId`]
    ///
    /// This info consists of the following:
    ///
    /// - The `center` and `len` variables
    /// - The top left corner of the spawn target
    /// - The bottom right corner of the spawn target
    pub fn get_spawn_info(
        &self,
        id: SpawnId,
    ) -> Option<([Variable; 2], [Expression; 2], [Expression; 2])> {
        self.spawns
            .iter()
            .find(|spawn| spawn.id == id)
            .map(|center| {
                let (tl, br) = match center.deps {
                    CenterDeps::Widget(tl, br, _) => (tl, [br.x().into(), br.y().into()]),
                    CenterDeps::TextHorizontal(tl, char_width) => {
                        let width = char_width.unwrap_or(1);
                        (tl, [tl.x() + width as f32, tl.y() + 1.0])
                    }
                    CenterDeps::TextVertical(tl) => (tl, [tl.x() + 1.0, tl.y() + 1.0]),
                };

                (
                    [center.center_var, center.len_var],
                    [tl.x().into(), tl.y().into()],
                    br,
                )
            })
    }

    /// Sets the new desired length for a [`SpawnId`]
    pub fn set_spawn_len(&mut self, id: SpawnId, desired_len: Option<f64>) {
        let Some(center) = self.spawns.iter_mut().find(|spawn| spawn.id == id) else {
            unreachable!(
                "This should only be called when it is known that the Spawn still exists."
            );
        };

        center.desired_len = desired_len;
    }
}

/// Represents the "center" of a floaging [`Rect`]
///
/// This makes use of an edit [`Variable`], allowing for
/// dinamically positioned floating [`Rect`]s.
///
/// [`Rect`]: super::Rect
struct Spawn {
    id: SpawnId,
    center_var: Variable,
    len_var: Variable,
    desired_len: Option<f64>,
    deps: CenterDeps,
    axis: Axis,
    prefers_before: bool,
    is_inside: bool,
    show_frames: [bool; 2],
}

/// What kind of dependency a [`SpawnedCenter`] has
enum CenterDeps {
    Widget(VarPoint, VarPoint, Axis),
    TextHorizontal(VarPoint, Option<u32>),
    TextVertical(VarPoint),
}

impl CenterDeps {
    /// The current value of the edges that define a [`SpawnedCenter`]
    fn get_values(&self, solver: &Solver) -> [f64; 2] {
        match *self {
            CenterDeps::Widget(tl, br, axis) => [tl, br].map(|v| solver.get_value(v.on(axis))),
            CenterDeps::TextHorizontal(tl, len) => {
                let left = solver.get_value(tl.x);
                [left, left + len.unwrap_or(1) as f64]
            }
            CenterDeps::TextVertical(tl) => {
                let top = solver.get_value(tl.y);
                [top, top + 1.0]
            }
        }
    }

    /// Wether the dependencies contain a certain [`Variable`]
    fn contains(&self, var: Variable) -> bool {
        match *self {
            CenterDeps::Widget(tl, br, axis) => tl.on(axis) == var || br.on(axis) == var,
            CenterDeps::TextHorizontal(tl, _) => tl.x == var,
            CenterDeps::TextVertical(tl) => tl.y == var,
        }
    }
}

pub enum ReturnedEditVars {
    WidgetSpawned([Variable; 2]),
    TextSpawned([Variable; 4]),
}
