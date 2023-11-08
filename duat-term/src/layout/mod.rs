mod frame;
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
};

use cassowary::{strength::WEAK, Solver, Variable, WeightedRelation::*};
use duat_core::{
    data::RwData,
    ui::{Axis, Constraint, PushSpecs},
};

pub use self::frame::{Brush, Edge, EdgeCoords, Frame};
use self::rect::{set_ratios, Rect, Rects};
use crate::{area::Coord, AreaId, print::Printer};

mod rect;

/// A [`Variable`], attached to its value, which is automatically kept
/// up to date.
#[derive(Clone)]
struct VarValue {
    var: Variable,
    value: Arc<AtomicUsize>,
    has_changed: Arc<AtomicBool>,
}

impl VarValue {
    /// Returns a new instance of [`VarValue`]
    pub fn new() -> Self {
        Self {
            var: Variable::new(),
            value: Arc::new(AtomicUsize::new(0)),
            has_changed: Arc::new(AtomicBool::new(false)),
        }
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

/// A point on the screen, which can be calculated by [`cassowary`]
/// and interpreted by `duat_term`.
#[derive(Clone, PartialEq, PartialOrd)]
pub struct VarPoint {
    x: VarValue,
    y: VarValue,
}

impl VarPoint {
    /// Returns a new instance of [`VarPoint`]
    fn new(constrainer: &mut Vars) -> Self {
        let element = VarPoint {
            x: VarValue::new(),
            y: VarValue::new(),
        };

        constrainer.list.insert(
            element.x.var,
            (element.x.value.clone(), element.x.has_changed.clone()),
        );
        constrainer.list.insert(
            element.y.var,
            (element.y.value.clone(), element.y.has_changed.clone()),
        );

        element
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

/// A list of [`Constraint`] for [`Rect`]s to follow.
///
/// These [`Constraint`]s are specifically not related to a [`Rect`]s
/// location, in relation to other [`Rect`]s. They instead deal with
/// two things, affecting a [`Rect`]s lenght in its parent's [`Axis`]:
///
/// - `defined`: A [`Constraint`], provided by the user, which details
///   specific requests for the lenght of a [`Rect`].
/// - `ratio`: A [`Constraint`] which details the ratio between the
///   lenght of this [`Rect`] and the lenght of the [`Rect`] that
///   follows it, if there is any.
///
/// Both of these constraints are optional, and are meant to be
/// replaceable at runtime.
///
/// [`Constraint`]: Equality
#[derive(Default, Debug, Clone)]
pub struct Length {
    constraint: Option<(Equality, (Constraint, Axis))>,
    ratio: Option<(Equality, f64)>,
}

impl Length {
    /// Wether or not [`self`] has flexibility in terms of its lenght.
    fn is_resizable(&self) -> bool {
        matches!(
            self.constraint,
            Some((_, (Constraint::Min(_) | Constraint::Max(_), _))) | None
        )
    }
}

pub struct Vars {
    pub solver: Solver,
    list: HashMap<Variable, (Arc<AtomicUsize>, Arc<AtomicBool>)>,
}

impl Vars {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            solver: Solver::new(),
            list: HashMap::new(),
        }
    }

    /// Updates the value of all [`VarPoint`]s that have changed,
    /// returning true if any of them have.
    pub fn update(&mut self) {
        for (var, new) in self.solver.fetch_changes() {
            let (value, has_changed) = &self.list[var];

            let new = new.round() as usize;
            let old = value.swap(new, Ordering::Release);
            has_changed.store(old != new, Ordering::Release);
        }
    }

    pub fn add_equality(&mut self, constraint: Equality) {
        self.solver.add_constraint(constraint).unwrap();
    }

    pub fn add_equalities<'a>(&mut self, constraints: impl IntoIterator<Item = &'a Equality>) {
        self.solver.add_constraints(constraints).unwrap()
    }

    pub fn remove_equality(&mut self, constraint: &Equality) {
        self.solver.remove_constraint(constraint).unwrap();
    }
}

/// The overrall structure of a window on `duat_term`.
///
/// The [`Layout`] handles all of the [`Rect`]s inside of it,
/// including all of the [`Variable`]s and
/// [`Constraint`][Equality]s that define said [`Rect`]s.
/// All external interactions seeking to change these values do so
/// through the [`Layout`].
///
/// The [`Layout`] also handles the [`Edge`]s that are supposed to be
/// printed to the screen.
///
/// The [`Layout`] will also hold floating [`Rect`]s, once those
/// become a thing.
pub struct Layout {
    pub rects: Rects,
    max: Coord,
    pub active_id: AreaId,
    frame: Frame,
    edges: Vec<Edge>,
    pub vars: Vars,
    lines: RwData<Printer>,
}

impl Layout {
    /// Returns a new instance of [`Layout`], applying a given
    /// [`Frame`] to all inner [`Rect`]s.
    pub fn new(frame: Frame) -> Self {
        let max = {
            let (width, height) = crossterm::terminal::size().unwrap();
            Coord::new(width as usize, height as usize)
        };

        let mut vars = Vars::new();
        let rects = Rects::new(&mut vars);
        let main_id = rects.main.id();

        let mut layout = Layout {
            rects,
            max,
            active_id: main_id,
            frame,
            edges: Vec::new(),
            vars,
            lines: RwData::new(Printer::new(max)),
        };

        let (vars, edges) = (&mut layout.vars, &mut layout.edges);

        layout
            .rects
            .set_edges(main_id, Frame::Empty, vars, edges, max);

        vars.update();

        layout.rects.main.clear_equalities(vars);
        layout
            .rects
            .set_edges(main_id, layout.frame, vars, edges, max);

        vars.update();

        let lines = layout.lines.clone();
        std::thread::spawn(move || {
            loop {
                lines.write().print();
                std::thread::sleep(std::time::Duration::from_millis(7));
            }
        });

        layout
    }

    /// The index of the main [`Rect`], which holds all (non floating)
    /// others.
    pub fn main_index(&self) -> AreaId {
        self.rects.main.id()
    }

    /// Bisects a given [`Rect`] into two [`Rect`]s, returning the
    /// index of the new one.
    ///
    /// This bisection will sometimes cause the creation of a new
    /// parent to hold the bisected [`Rect`] and its new sibling. In
    /// these cases, an aditional index associated with the parent
    /// will be returned.
    ///
    /// If `glue_new`, and the bisected [`Rect`] was "not glued", a
    /// new parent will be created, which will act as the holder for
    /// all of its successors. If this parent is moved, the children
    /// will move with it.
    ///
    /// If `glue_new`, and the bisected [`Rect`] was already glued,
    /// the creation of a new parent will follow regular rules, but
    /// the children will still be "glued".
    pub fn bisect(
        &mut self,
        id: AreaId,
        specs: PushSpecs,
        cluster: bool,
    ) -> (AreaId, Option<AreaId>) {
        let (vars, edges) = (&mut self.vars, &mut self.edges);
        let axis = specs.axis();
        let (can_be_sibling, can_be_child) = {
            let parent_is_cluster = self
                .rects
                .get_parent(id)
                .map(|(_, parent)| parent.is_clustered())
                .unwrap_or(cluster);

            let child_is_cluster = self.rects.get(id).is_some_and(Rect::is_clustered);

            (parent_is_cluster == cluster, child_is_cluster == cluster)
        };

        // Check if the target's parent has the same `Axis`.
        let (parent, pos, new_parent_id) = if can_be_sibling
            && let Some((pos, parent)) = self.rects.get_parent_mut(id)
            && parent.aligns_with(axis)
        {
            let pos = match specs.comes_earlier() {
                true => pos,
                false => pos + 1,
            };

            (parent, pos, None)
        // Check if the target has the same `Axis`.
        } else if can_be_child
            && let Some(parent) = self.rects.get_mut(id)
            && parent.aligns_with(axis)
        {
            let index = match specs.comes_earlier() {
                true => 0,
                false => parent.children_len(),
            };

            (parent, index, None)
        // If all else fails, create a new parent to hold both `self`
        // and the new area.
        } else {
            let (new_parent_id, child) = {
                let rect = self.rects.get_mut(id).unwrap();
                let parent = Rect::new_parent_of(rect, axis, vars, cluster);

                (parent.id(), std::mem::replace(rect, parent))
            };
            let child_id = child.id();

            let pos = match specs.comes_earlier() {
                true => 0,
                false => 1,
            };

            let constraint = self.rects.take_constraint(new_parent_id, vars);

            self.rects.insert_child(0, new_parent_id, child);

            if let Some((constraint, axis)) = constraint {
                self.rects.set_constraint(child_id, constraint, axis, vars);
            }

            // If the child is clustered, the frame doesn't need to be redone.
            if !cluster {
                self.rects
                    .set_new_child_edges(child_id, self.frame, vars, edges, self.max)
            }

            let rect = self.rects.get_mut(new_parent_id).unwrap();
            (rect, pos, Some(new_parent_id))
        };
        
        let parent_id = parent.id();
        vars.update();

        let (temp_constraint, new_id) = {
            let new = Rect::new(vars);
            let new_id = new.id();

            self.rects.insert_child(pos, parent_id, new);

            if let Some(constraint) = specs.constraint() {
                self.rects.set_constraint(new_id, constraint, axis, vars);
            }

            // We initially set the frame to `Frame::Empty`, since that will make
            // the `Rect`s take their "full space". What this means is that, when
            // we calculate the real frame, using `self.frame`, we know for a fact
            // which `Rect`s reach the edge of the screen. Unfortunately, this
            // does mean that we have to run `prepare_child()` twice for each
            // affected area.
            self.rects
                .set_edges_around(new_id, Frame::Empty, vars, edges, self.max);

            // Add a constraint so that the new child `Rect` has a len equal to
            // `resizable_len / resizable_children`, a self imposed rule.
            // This will be useful later, when adding new ratio constraints to the
            // new child.
            let constraint = specs.constraint();
            if let Some(Constraint::Min(_) | Constraint::Max(_)) | None = constraint {
                let children = self.rects.get_siblings(new_id).unwrap();
                let axis = specs.axis();

                let (res_count, res_len) = children
                    .iter()
                    .filter(|(_, length)| length.is_resizable())
                    .fold((0, 0), |(count, len), (child, _)| {
                        (count + 1, len + child.len_value(axis))
                    });

                let temp_constraint = {
                    let (new, _) = &children[pos];
                    new.len(axis) | EQ(WEAK * 2.0) | (res_len as f64 / res_count as f64)
                };

                vars.solver.add_constraint(temp_constraint.clone()).unwrap();

                (Some(temp_constraint), new_id)
            } else {
                (None, new_id)
            }
        };

        vars.update();

        let parent = self.rects.get_mut(parent_id).unwrap();
        if let Some(Constraint::Min(_) | Constraint::Max(_)) | None = specs.constraint() {
            set_ratios(parent, pos, vars);
        }

        if let Some(temp_constraint) = temp_constraint {
            vars.solver.remove_constraint(&temp_constraint).unwrap();
        }

        vars.update();

        self.rects
            .set_edges_around(new_id, self.frame, vars, edges, self.max);

        vars.update();

        (new_id, new_parent_id)
    }

    /// The current value for the width of [`self`].
    pub fn width(&self) -> usize {
        self.rects.main.len_value(Axis::Horizontal)
    }

    /// The current value for the height of [`self`].
    pub fn height(&self) -> usize {
        self.rects.main.len_value(Axis::Vertical)
    }

    /// The [`Edge`]s that are supposed to be printed to the screen.
    pub fn edges(&self) -> &[Edge] {
        self.edges.as_ref()
    }

    pub fn get(&self, id: AreaId) -> Option<&Rect> {
        self.rects.get(id)
    }

    pub fn get_parent(&self, id: AreaId) -> Option<(usize, &Rect)> {
        self.rects.get_parent(id)
    }
}

type Equality = cassowary::Constraint;
