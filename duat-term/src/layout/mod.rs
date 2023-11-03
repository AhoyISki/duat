mod frame;
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc,
    },
};

use cassowary::{
    strength::{MEDIUM, REQUIRED, STRONG, WEAK},
    Constraint as CassowaryConstraint, Expression, Solver, Variable,
    WeightedRelation::*,
};
use duat_core::ui::{Axis, Constraint, PushSpecs};
pub use frame::{Brush, Edge, EdgeCoords, Frame};

use crate::{area::Coord, Area, AreaId};

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
    fn new() -> Self {
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
struct VarPoint {
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
/// [`Constraint`]: CassowaryConstraint
#[derive(Default, Debug, Clone)]
struct Constraints {
    defined: Option<(CassowaryConstraint, Constraint)>,
    ratio: Option<(CassowaryConstraint, f64)>,
}

impl Constraints {
    /// Wether or not [`self`] has flexibility in terms of its lenght.
    fn is_resizable(&self) -> bool {
        matches!(
            self.defined,
            Some((_, Constraint::Min(_) | Constraint::Max(_))) | None
        )
    }
}

/// An area on the screen, which can hold other [`Rect`]s or be host
/// for printing a [`Widget`][duat_core::widgets::Widget].
///
/// [`Rect`]s hold the [`Constraint`]s that define them, handling
/// which sides of the [`Rect`] should be equal to that of other
/// specific [`Rect`]s. These [`Constraint`]s also serve the role of
/// framing the [`Rect`] in cases where it is deemed necessary and
/// correct.
///
/// They can also be tied to other, ancestor [`Rect`]s, such that, by
/// moving the parent [`Rect`], you will be moving all of its children
/// along with it.
///
/// [`Constraint`]: CassowaryConstraint
#[derive(Debug)]
#[allow(clippy::type_complexity)]
pub struct Rect {
    id: AreaId,
    /// The index that this [`Rect`] is tied to.
    tl: VarPoint,
    br: VarPoint,
    edge_cons: Vec<CassowaryConstraint>,
    lineage: Option<(Vec<(Rect, Constraints)>, Axis, bool)>,
}

impl Rect {
    /// Returns a new instance of [`Rect`], already adding its
    /// [`Variable`]s to the list.
    fn new(constrainer: &mut Vars) -> Self {
        Rect {
            id: AreaId::new(),
            tl: VarPoint::new(constrainer),
            br: VarPoint::new(constrainer),
            edge_cons: Vec::new(),
            lineage: None,
        }
    }

    /// Returns a new [`Rect`], which is supposed to replace an
    /// existing [`Rect`], as its new parent.
    fn new_parent_of(rect: &mut Rect, axis: Axis, vars: &mut Vars, clustered: bool) -> Self {
        let parent = Rect {
            id: AreaId::new(),
            tl: rect.tl.clone(),
            br: rect.br.clone(),
            edge_cons: rect.edge_cons.clone(),
            lineage: Some((Vec::new(), axis, clustered)),
        };

        rect.edge_cons.clear();
        rect.tl = VarPoint::new(vars);
        rect.br = VarPoint::new(vars);

        parent
    }

    /// Sets the [`CassowaryConstraint`]s for the edges of [`self`],
    /// taking into account the potential frame, defined by [`Frame`]
    /// and the [`Rect`]'s position.
    fn set_constraints(
        &mut self,
        parent: &Rect,
        axis: Axis,
        frame: Frame,
        max: Coord,
        edges: &mut Vec<Edge>,
    ) -> (f64, f64) {
        self.edge_cons.extend([
            self.tl.x.var | GE(REQUIRED) | 0.0,
            self.tl.y.var | GE(REQUIRED) | 0.0,
            self.br.x.var | GE(REQUIRED) | self.tl.x.var,
            self.br.y.var | GE(REQUIRED) | self.tl.y.var,
        ]);

        edges.retain(|edge| !edge.matches_vars(&self.br, &self.tl));
        let (right, up, left, down) = if self.is_frameable(Some(parent)) {
            self.form_frame(frame, max, edges)
        } else {
            (0.0, 0.0, 0.0, 0.0)
        };

        let (para_left, para_right, start, end) = match axis.perp() {
            Axis::Vertical => (up, down, left, right),
            Axis::Horizontal => (left, right, up, down),
        };

        self.edge_cons.extend([
            self.start(axis.perp()) | EQ(REQUIRED) | (parent.start(axis.perp()) + para_left),
            (self.end(axis.perp()) + para_right) | EQ(REQUIRED) | parent.end(axis.perp()),
        ]);

        (start, end)
    }

    /// Sets the [`CassowaryConstraint`]s for the main [`Rect`], which
    /// is supposed to be parentless. It takes into account a possible
    /// [`Frame`].
    fn set_main_constraints(
        &mut self,
        frame: Frame,
        constrainer: &mut Vars,
        edges: &mut Vec<Edge>,
        max: Coord,
    ) {
        let (hor_edge, ver_edge) = if self.is_frameable(None) {
            frame.main_edges()
        } else {
            (0.0, 0.0)
        };

        edges.retain(|edge| !edge.matches_vars(&self.br, &self.tl));
        if hor_edge == 1.0 {
            edges.push(Edge::new(
                self.br.clone(),
                self.tl.clone(),
                Axis::Vertical,
                frame,
            ));
            edges.push(Edge::new(
                self.tl.clone(),
                self.br.clone(),
                Axis::Vertical,
                frame,
            ));
        }
        if ver_edge == 1.0 {
            edges.push(Edge::new(
                self.tl.clone(),
                self.br.clone(),
                Axis::Horizontal,
                frame,
            ));
            edges.push(Edge::new(
                self.br.clone(),
                self.tl.clone(),
                Axis::Horizontal,
                frame,
            ));
        }

        self.edge_cons = vec![
            self.tl.x.var | EQ(REQUIRED) | hor_edge,
            self.tl.y.var | EQ(REQUIRED) | ver_edge,
            self.br.x.var | EQ(REQUIRED) | (max.x as f64 - hor_edge),
            self.br.y.var | EQ(REQUIRED) | (max.y as f64 - ver_edge),
        ];

        constrainer.add_constraints(&self.edge_cons);
    }

    /// Removes all [`CassowaryConstraint`]s which define the edges of
    /// [`self`].
    fn clear_constraints(&mut self, vars: &mut Vars) {
        for constraint in self.edge_cons.drain(..) {
            vars.remove_constraint(&constraint);
        }
    }

    /// A [`Variable`], representing the "start" of [`self`], given an
    /// [`Axis`]. It will be the left or upper side of a [`Rect`].
    fn start(&self, axis: Axis) -> Variable {
        match axis {
            Axis::Horizontal => self.tl.x.var,
            Axis::Vertical => self.tl.y.var,
        }
    }

    /// A [`Variable`], representing the "start" of [`self`], given an
    /// [`Axis`]. It will be the right or lower side of a [`Rect`].
    fn end(&self, axis: Axis) -> Variable {
        match axis {
            Axis::Horizontal => self.br.x.var,
            Axis::Vertical => self.br.y.var,
        }
    }

    /// An [`Expression`] representing the lenght of [`self`] on a
    /// given [`Axis`].
    fn len(&self, axis: Axis) -> Expression {
        match axis {
            Axis::Horizontal => self.br.x.var - self.tl.x.var,
            Axis::Vertical => self.br.y.var - self.tl.y.var,
        }
    }

    /// The current value for the lenght of [`self`] on a given
    /// [`Axis`].
    fn len_value(&self, axis: Axis) -> usize {
        match axis {
            Axis::Horizontal => {
                self.br.x.value.load(Ordering::Relaxed) - self.tl.x.value.load(Ordering::Relaxed)
            }
            Axis::Vertical => {
                self.br.y.value.load(Ordering::Relaxed) - self.tl.y.value.load(Ordering::Relaxed)
            }
        }
    }

    /// The top left corner of [`self`].
    pub fn tl(&self) -> Coord {
        Coord {
            x: self.tl.x.value.load(Ordering::Acquire),
            y: self.tl.y.value.load(Ordering::Acquire),
        }
    }

    /// The bottom right corner of [`self`].
    pub fn br(&self) -> Coord {
        Coord {
            x: self.br.x.value.load(Ordering::Acquire),
            y: self.br.y.value.load(Ordering::Acquire),
        }
    }

    /// Wether or not [`self`] has children.
    pub fn is_parent(&self) -> bool {
        self.lineage.is_some()
    }

    /// Changes a child's defined constraint in [`Constraints`].
    pub fn change_child_constraint(
        &mut self,
        index: usize,
        defined: Constraint,
        vars: &mut Vars,
    ) -> bool {
        let (children, axis, _) = self.lineage.as_mut().unwrap();
        let axis = *axis;

        let (_, constraints) = &mut children[index];
        if !constraints
            .defined
            .as_ref()
            .is_some_and(|(_, cmp)| *cmp == defined)
        {
            if let Some((constraint, _)) = constraints.defined.take() {
                vars.remove_constraint(&constraint);
            }
            set_defined_constraint(defined, self, index, axis, vars);

            true
        } else {
            false
        }
    }

    /// The index that identifies [`self`].
    pub fn index(&self) -> AreaId {
        self.id
    }

    pub fn has_changed(&self) -> bool {
        let br_x_changed = self.br.x.has_changed.swap(false, Ordering::Release);
        let br_y_changed = self.br.y.has_changed.swap(false, Ordering::Release);
        let tl_x_changed = self.tl.x.has_changed.swap(false, Ordering::Release);
        let tl_y_changed = self.tl.y.has_changed.swap(false, Ordering::Release);

        br_x_changed || br_y_changed || tl_x_changed || tl_y_changed
    }

    /// Wheter or not [`self`] can be framed.
    fn is_frameable(&self, parent: Option<&Rect>) -> bool {
        if parent.is_some_and(|parent| parent.lineage.as_ref().unwrap().2) {
            false
        } else if let Some((.., clustered)) = &self.lineage {
            *clustered
        } else {
            true
        }
    }

    /// Forms the frame surrounding [`self`], considering its position
    /// and a [`Frame`].
    fn form_frame(&self, frame: Frame, max: Coord, edges: &mut Vec<Edge>) -> (f64, f64, f64, f64) {
        let (right, up, left, down) = frame.edges(&self.br, &self.tl, max);

        if right == 1.0 {
            edges.push(Edge::new(
                self.br.clone(),
                self.tl.clone(),
                Axis::Vertical,
                frame,
            ));
        }
        if up == 1.0 {
            edges.push(Edge::new(
                self.tl.clone(),
                self.br.clone(),
                Axis::Horizontal,
                frame,
            ));
        }
        if left == 1.0 {
            edges.push(Edge::new(
                self.tl.clone(),
                self.br.clone(),
                Axis::Vertical,
                frame,
            ));
        }
        if down == 1.0 {
            edges.push(Edge::new(
                self.br.clone(),
                self.tl.clone(),
                Axis::Horizontal,
                frame,
            ));
        }

        (right, up, left, down)
    }
}

impl PartialEq for Rect {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialEq<Area> for Rect {
    fn eq(&self, other: &Area) -> bool {
        self.id == other.index
    }
}

pub struct Rects {
    main: Rect,
    floating: Vec<Rect>,
}

impl Rects {
    fn new(main: Rect) -> Self {
        Self {
            main,
            floating: Vec::new(),
        }
    }

    /// Fetches the parent of the [`RwData<Rect>`] with the given
    /// index, including its positional index and the [`Axis`] of
    /// its children. Fetches the [`RwData<Rect>`] of the given
    /// index, if there is one.
    pub fn get(&self, id: AreaId) -> Option<&Rect> {
        fn fetch(rect: &Rect, id: AreaId) -> Option<&Rect> {
            if rect.id == id {
                Some(rect)
            } else {
                rect.lineage.as_ref().and_then(|(children, ..)| {
                    children.iter().find_map(|(child, _)| fetch(child, id))
                })
            }
        }

        std::iter::once(&self.main)
            .chain(&self.floating)
            .find_map(|rect| fetch(rect, id))
    }

    /// Fetches the [`Rect`] which holds the [`Rect`]
    /// of the given index.
    ///
    /// Also returns the child's "position", given an [`Axis`],
    /// going top to bottom or left to right.
    pub fn get_parent(&self, id: AreaId) -> Option<(usize, &Rect)> {
        std::iter::once(&self.main)
            .chain(&self.floating)
            .find_map(|rect| fetch_parent(rect, id))
    }

    /// Fetches the [`RwData<Rect>`] of the given index, if there is
    /// one.
    pub fn get_mut(&mut self, id: AreaId) -> Option<&mut Rect> {
        std::iter::once(&mut self.main)
            .chain(&mut self.floating)
            .find_map(|rect| fetch_mut(rect, id))
    }

    /// Fetches the [`RwData<Rect>`] which holds the [`RwData<Rect>`]
    /// of the given index.
    ///
    /// Also returns the child's "position", going top to bottom or
    /// left to right.
    pub fn get_parent_mut(&mut self, id: AreaId) -> Option<(usize, &mut Rect)> {
        let (pos, parent) = std::iter::once(&mut self.main)
            .chain(&mut self.floating)
            .find_map(|rect| fetch_parent(rect, id))?;
        let id = parent.id;

		let parent = self.get_mut(id)?;

        Some((pos, parent))
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

    pub fn add_constraint(&mut self, constraint: cassowary::Constraint) {
        self.solver.add_constraint(constraint).unwrap();
    }

    pub fn add_constraints<'a>(
        &mut self,
        constraints: impl IntoIterator<Item = &'a cassowary::Constraint>,
    ) {
        self.solver.add_constraints(constraints).unwrap()
    }

    pub fn remove_constraint(&mut self, constraint: &cassowary::Constraint) {
        self.solver.remove_constraint(constraint).unwrap();
    }
}

/// The overrall structure of a window on `duat_term`.
///
/// The [`Layout`] handles all of the [`Rect`]s inside of it,
/// including all of the [`Variable`]s and
/// [`Constraint`][CassowaryConstraint]s that define said [`Rect`]s.
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
    pub active_index: AreaId,
    frame: Frame,
    edges: Vec<Edge>,
    pub vars: Vars,
}

impl Layout {
    /// Returns a new instance of [`Layout`], applying a given
    /// [`Frame`] to all inner [`Rect`]s.
    pub fn new(frame: Frame) -> Self {
        let max = {
            let (width, height) = crossterm::terminal::size().unwrap();
            Coord::new(width as usize, height as usize)
        };
        let mut edges = Vec::new();
        let mut vars = Vars::new();
        let mut main = Rect::new(&mut vars);

        main.set_main_constraints(Frame::Empty, &mut vars, &mut edges, max);

        let active_index = main.id;
        let mut layout = Layout {
            rects: Rects::new(main),
            max,
            active_index,
            frame,
            edges,
            vars,
        };

        layout.vars.update();

        layout.rects.main.clear_constraints(&mut layout.vars);
        layout.rects.main.set_main_constraints(
            Frame::Empty,
            &mut layout.vars,
            &mut layout.edges,
            max,
        );

        layout.vars.update();

        layout
    }

    /// The index of the main [`Rect`], which holds all (non floating)
    /// others.
    pub fn main_index(&self) -> AreaId {
        self.rects.main.id
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
        let axis = Axis::from(specs);
        let (can_be_sibling, can_be_child) = {
            let parent_is_cluster = self
                .get_parent(id)
                .map(|(_, parent)| parent.lineage.as_ref().unwrap().2)
                .unwrap_or(cluster);

            let child_is_cluster = self
                .get(id)
                .unwrap()
                .lineage
                .as_ref()
                .is_some_and(|(.., clustered)| *clustered);

            (parent_is_cluster == cluster, child_is_cluster == cluster)
        };

        // Check for a parent of `self` with the same `Axis`.
        let (parent, pos, new_parent_id) = if can_be_sibling
            && let Some((pos, parent)) = self
                .rects
                .get_parent_mut(id)
                .filter(|(_, parent)| parent.lineage.as_ref().unwrap().1 == axis)
        {
            let pos = match specs.comes_earlier() {
                true => pos,
                false => pos + 1,
            };

            (parent, pos, None)
        // Check if the target's parent has the same clustering.
        } else if can_be_child
            && let Some(parent) = self.rects.get_mut(id).filter(|area| {
                area.lineage
                    .as_ref()
                    .is_some_and(|(_, cmp, _)| axis == *cmp)
            })
        {
            let index = match specs.comes_earlier() {
                true => 0,
                false => parent.lineage.as_ref().unwrap().0.len(),
            };

            (parent, index, None)
        // If all else fails, create a new parent to hold both `self`
        // and the new area.
        } else {
            let child_is_main = self.rects.main.id == id;
            let rect = self.rects.get_mut(id).unwrap();

            let (new_parent_id, child) = {
                let parent = Rect::new_parent_of(rect, axis, &mut self.vars, cluster);
                (parent.id, std::mem::replace(rect, parent))
            };

            let pos = match specs.comes_earlier() {
                true => 0,
                false => 1,
            };

            let defined = if let Some((pos, grandpa)) = self.rects.get_parent_mut(new_parent_id) {
                let (children, axis, _) = grandpa.lineage.as_mut().unwrap();
                let constraints = std::mem::take(&mut children[pos].1);
                constraints.defined.map(|(constraint, defined)| {
                    self.vars.remove_constraint(&constraint);
                    (defined, *axis)
                })
            } else {
                None
            };

            let rect = self.rects.get_mut(new_parent_id).unwrap();
            rect.lineage = Some((vec![(child, Constraints::default())], axis, cluster));

            if let Some((defined, axis)) = defined {
                set_defined_constraint(defined, rect, 0, axis, &mut self.vars);
            }

            // If the child is glued, the frame doesn't need to be redone.
            if !cluster {
                if child_is_main {
                    let (vars, edges) = (&mut self.vars, &mut self.edges);
                    rect.clear_constraints(vars);
                    rect.set_main_constraints(self.frame, vars, edges, self.max);
                } else {
                    let (pos, parent) = self.rects.get_parent_mut(new_parent_id).unwrap();
                    let (vars, edges) = (&mut self.vars, &mut self.edges);
                    prepare_child(parent, pos, vars, self.frame, self.max, edges)
                }
            }

            let rect = self.rects.get_mut(new_parent_id).unwrap();
            (rect, pos, Some(new_parent_id))
        };
        let parent_id = parent.id;

        self.vars.update();

        let (temp_constraint, new_id) = {
            let new = Rect::new(&mut self.vars);
            let new_id = new.id;

            let (len, axis) = parent
                .lineage
                .as_mut()
                .map(|(children, axis, _)| {
                    children.insert(pos, (new, Constraints::default()));
                    (children.len(), *axis)
                })
                .unwrap();

            if let Some(defined) = specs.constraint() {
                set_defined_constraint(defined, parent, pos, axis, &mut self.vars);
            }

            // We initially set the frame to `Frame::Empty`, since that will make
            // the `Rect`s take their "full space". What this means is that, when
            // we calculate the real frame, using `self.frame`, we know for a fact
            // which `Rect`s reach the edge of the screen. Unfortunately, this
            // does mean that we have to run `prepare_child()` twice for each
            // affected area.
            let edges = &mut self.edges;
            let next = (len - 1).checked_sub(pos + 1).map(|_| pos + 1);
            for &pos in [pos.checked_sub(1), Some(pos), next].iter().flatten() {
                prepare_child(parent, pos, &mut self.vars, Frame::Empty, self.max, edges);
            }

            // Add a constraint so that the new child `Rect` has a len equal to
            // `resizable_len / resizable_children`, a self imposed rule.
            // This will be useful later, when adding new ratio constraints to the
            // new child.
            let constraint = specs.constraint();
            if let Some(Constraint::Min(_) | Constraint::Max(_)) | None = constraint {
                let (children, axis, _) = parent.lineage.as_ref().unwrap();
                let (res_count, res_len) = children
                    .iter()
                    .filter(|(_, constraints)| constraints.is_resizable())
                    .fold((0, 0), |(count, len), (child, _)| {
                        (count + 1, len + child.len_value(*axis))
                    });

                let temp_constraint = {
                    let (new, _) = &children[pos];
                    new.len(*axis) | EQ(WEAK * 2.0) | (res_len as f64 / res_count as f64)
                };

                self.vars
                    .solver
                    .add_constraint(temp_constraint.clone())
                    .unwrap();

                (Some(temp_constraint), new_id)
            } else {
                (None, new_id)
            }
        };

        self.vars.update();

        let parent = self.rects.get_mut(parent_id).unwrap();
        if let Some(Constraint::Min(_) | Constraint::Max(_)) | None = specs.constraint() {
            set_ratio_constraints(parent, pos, &mut self.vars);
        }

        if let Some(temp_constraint) = temp_constraint {
            self.vars
                .solver
                .remove_constraint(&temp_constraint)
                .unwrap();
        }

        self.vars.update();

        let parent = self.rects.get_mut(parent_id).unwrap();
        let len = parent.lineage.as_ref().unwrap().0.len();
        let edges = &mut self.edges;
        // Second frame calculation, this time, using the real `self.frame`.
        let next = (len - 1).checked_sub(pos + 1).map(|_| pos + 1);
        for &pos in [pos.checked_sub(1), Some(pos), next].iter().flatten() {
            prepare_child(parent, pos, &mut self.vars, self.frame, self.max, edges);
        }

        self.vars.update();

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

/// Assigns all [`CassowaryConstraint`]s that are appropriate to a
/// given child, with the exception of defined and ratio constraints.
fn prepare_child(
    parent: &mut Rect,
    pos: usize,
    vars: &mut Vars,
    frame: Frame,
    max: Coord,
    edges: &mut Vec<Edge>,
) {
    let (mut children, axis, clustered) = parent.lineage.take().unwrap();
    parent.lineage = Some((Vec::new(), axis, clustered));
    let child = &mut children[pos].0;

    child.clear_constraints(vars);
    let (start, end) = child.set_constraints(parent, axis, frame, max, edges);

    if pos == 0 {
        let constraint = child.start(axis) | EQ(REQUIRED) | (parent.start(axis) + start);
        child.edge_cons.push(constraint);
    }

    // Previous children carry the `Constraint`s for the `start` of their
    // successors.
    let constraint = if let Some((next, _)) = children.get(pos + 1) {
        (children[pos].0.end(axis) + end) | EQ(REQUIRED) | next.start(axis)
    } else {
        (children[pos].0.end(axis) + end) | EQ(REQUIRED) | parent.end(axis)
    };

    let child = &mut children[pos].0;
    child.edge_cons.push(constraint);

    vars.add_constraints(&child.edge_cons);

    if let Some((children, ..)) = &mut child.lineage {
        let len = children.len();
        for pos in 0..len {
            prepare_child(child, pos, vars, frame, max, edges);
        }
    }

    parent.lineage = Some((children, axis, clustered));
}

/// Sets the ratio [`CassowaryConstraint`]s for the child of the given
/// index. This does include setting the [`CassowaryConstraint`] for
/// the previous child, if there is one.
fn set_ratio_constraints(parent: &mut Rect, pos: usize, vars: &mut Vars) {
    let &(_, axis, _) = parent.lineage.as_ref().unwrap();
    let (children, ..) = parent.lineage.as_mut().unwrap();

    let (new, _) = &children[pos];
    let new_len = new.len(axis);
    let new_len_value = new.len_value(axis);

    let prev = children.iter().take(pos);
    let resizable_pos = prev
        .filter(|(_, constraints)| constraints.is_resizable())
        .count();

    let mut children = children
        .iter_mut()
        .filter(|(_, constraints)| constraints.is_resizable());

    if resizable_pos > 0 {
        let (prev, constraints) = children.nth(resizable_pos - 1).unwrap();

        let ratio = if new_len_value == 0 {
            1.0
        } else {
            prev.len_value(axis) as f64 / new_len_value as f64
        };

        let constraint = prev.len(axis) | EQ(WEAK) | (ratio * new_len.clone());
        constraints.ratio = Some((constraint.clone(), ratio));
        vars.add_constraint(constraint);
    }

    let ratio = children.nth(1).map(|(next, _)| {
        let ratio = if next.len_value(axis) == 0 {
            1.0
        } else {
            new_len_value as f64 / next.len_value(axis) as f64
        };

        let constraint = new_len | EQ(WEAK) | (ratio * next.len(axis));
        vars.add_constraint(constraint.clone());

        (constraint, ratio)
    });

    parent.lineage.as_mut().unwrap().0[pos].1.ratio = ratio;
}

/// Returns a [`CassowaryConstraint`] representing the defined
/// [`Constraint`].
fn set_defined_constraint(
    defined: Constraint,
    parent: &mut Rect,
    pos: usize,
    axis: Axis,
    vars: &mut Vars,
) {
    let parent_len = parent.len(axis);

    let (children, ..) = parent.lineage.as_mut().unwrap();
    let (child, constraints) = &mut children[pos];

    let constraint = match defined {
        Constraint::Ratio(den, div) => {
            assert!(den < div, "Constraint::Ratio must be smaller than 1.");
            child.len(axis) | EQ(WEAK * 2.0) | (parent_len * (den as f64 / div as f64))
        }
        Constraint::Percent(percent) => {
            assert!(
                percent <= 100,
                "Constraint::Percent must be smaller than 100"
            );
            child.len(axis) | EQ(WEAK * 2.0) | (parent_len * (percent as f64 / 100.0))
        }
        Constraint::Length(len) => child.len(axis) | EQ(STRONG) | len,
        Constraint::Min(min) => child.len(axis) | GE(MEDIUM) | min,
        Constraint::Max(max) => child.len(axis) | LE(MEDIUM) | max,
    };

    vars.add_constraint(constraint.clone());

    constraints.defined = Some((constraint, defined));
}

fn fetch_parent(main: &Rect, id: AreaId) -> Option<(usize, &Rect)> {
    if main.id == id {
        return None;
    }
    let Some((children, ..)) = &main.lineage else {
        return None;
    };

    children.iter().enumerate().find_map(|(pos, (child, _))| {
        if child.id == id {
            Some((pos, main))
        } else {
            fetch_parent(child, id)
        }
    })
}

fn fetch_mut(rect: &mut Rect, id: AreaId) -> Option<&mut Rect> {
    if rect.id == id {
        Some(rect)
    } else {
        rect.lineage.as_mut().and_then(|(children, ..)| {
            children
                .iter_mut()
                .find_map(|(child, _)| fetch_mut(child, id))
        })
    }
}
