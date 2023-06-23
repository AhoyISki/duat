use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicBool, AtomicU16, AtomicUsize, Ordering},
        Arc
    }
};

use cassowary::{
    strength::{MEDIUM, REQUIRED, STRONG, WEAK},
    Constraint as CassowaryConstraint, Expression, Solver, Variable,
    WeightedRelation::*
};
use parsec_core::{
    data::RwData,
    log_info,
    ui::{Axis, Constraint, PushSpecs},
    widgets
};

use crate::{area::Coord, Coords};

fn unique_rect_index() -> usize {
    static INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

    INDEX_COUNTER.fetch_add(1, Ordering::SeqCst)
}

#[derive(Clone)]
struct VarValue {
    var: Variable,
    value: Arc<AtomicU16>
}

impl VarValue {
    fn new() -> Self {
        Self {
            var: Variable::new(),
            value: Arc::new(AtomicU16::new(0))
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
        if self.value.load(Ordering::Acquire) > other.value.load(Ordering::Acquire) {
            Some(std::cmp::Ordering::Greater)
        } else if self.value.load(Ordering::Acquire) < other.value.load(Ordering::Acquire) {
            Some(std::cmp::Ordering::Less)
        } else {
            Some(std::cmp::Ordering::Equal)
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
struct VarPoint {
    x: VarValue,
    y: VarValue
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

impl VarPoint {
    fn new(vars: &mut HashMap<Variable, Arc<AtomicU16>>) -> Self {
        let element = VarPoint {
            x: VarValue::new(),
            y: VarValue::new()
        };

        vars.insert(element.x.var, element.x.value.clone());
        vars.insert(element.y.var, element.y.value.clone());

        element
    }
}

#[derive(Default, Debug, Clone)]
struct Constraints {
    defined: Option<(CassowaryConstraint, Constraint)>,
    ratio: Option<(CassowaryConstraint, f64)>
}

impl Constraints {
    fn change_defined(
        &mut self, new: Option<Constraint>, parent: &Rect, index: usize, solver: &mut Solver
    ) {
        if let Some((constraint, _)) = self.defined.take() {
            solver.remove_constraint(&constraint).unwrap();
        }

        self.defined = new.map(|defined| {
            let &(_, axis) = parent.lineage.as_ref().unwrap();
            let constraint = defined_constraint(defined, parent, index, axis);
            solver.add_constraint(constraint.clone()).unwrap();

            (constraint, defined)
        });
    }

    fn is_resizable(&self) -> bool {
        match self.defined {
            Some((_, Constraint::Min(_) | Constraint::Max(_))) | None => true,
            _ => false
        }
    }
}

#[derive(Clone, Debug)]
pub struct Rect {
    index: usize,
    /// The index that this [`Rect`] is tied to.
    tied_index: Option<usize>,
    tl: VarPoint,
    br: VarPoint,
    edge_cons: Vec<CassowaryConstraint>,
    lineage: Option<(Vec<(RwData<Rect>, Constraints)>, Axis)>
}

// impl std::fmt::Debug for Rect {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) ->
// std::fmt::Result {         f.debug_struct("Rect")
//             .field("index", &self.index)
//             .field("tied_index", &self.tied_index)
//             .field("tl", &self.tl)
//             .field("br", &self.br)
//             .field(
//                 "edge_cons",
//
// &self.edge_cons.iter().skip(4).collect::<Vec<&
// CassowaryConstraint>>()             )
//             .field("children", &self.children)
//             .finish()
//     }
// }

impl Rect {
    fn new(vars: &mut HashMap<Variable, Arc<AtomicU16>>) -> Self {
        Rect {
            index: unique_rect_index(),
            tied_index: None,
            tl: VarPoint::new(vars),
            br: VarPoint::new(vars),
            edge_cons: Vec::new(),
            lineage: None
        }
    }

    fn new_parent_of(
        rect: &mut Rect, axis: Axis, vars: &mut HashMap<Variable, Arc<AtomicU16>>
    ) -> Self {
        let parent = Rect {
            index: unique_rect_index(),
            lineage: Some((Vec::new(), axis)),
            ..rect.clone()
        };

        rect.edge_cons.clear();
        rect.tl = VarPoint::new(vars);
        rect.br = VarPoint::new(vars);

        parent
    }

    fn clear_constraints(&mut self, solver: &mut Solver) {
        for constraint in self.edge_cons.drain(..) {
            solver.remove_constraint(&constraint).unwrap();
        }
    }

    fn set_constraints(
        &mut self, parent: &Rect, axis: Axis, frame: Frame, max: Coord, edges: &mut Vec<Edge>
    ) -> (f64, f64) {
        let axis = axis.perp();

        self.edge_cons.extend([
            self.tl.x.var | GE(REQUIRED) | 0.0,
            self.tl.y.var | GE(REQUIRED) | 0.0,
            self.br.x.var | GE(REQUIRED) | self.tl.x.var,
            self.br.y.var | GE(REQUIRED) | self.tl.y.var
        ]);

        let (right, up, left, down) = self.form_frame(frame, max, edges);

        let (para_left, para_right, start, end) = match axis {
            Axis::Vertical => (up, down, left, right),
            Axis::Horizontal => (left, right, up, down)
        };

        self.edge_cons.extend([
            self.start(axis) | EQ(REQUIRED) | parent.start(axis) + para_left,
            self.end(axis) + para_right | EQ(REQUIRED) | parent.end(axis)
        ]);

        (start, end)
    }

    fn set_main_constraints(
        &mut self, frame: Frame, solver: &mut Solver, edges: &mut Vec<Edge>, max: Coord
    ) {
        let (hor_edge, ver_edge) = if self.is_frameable() {
            frame.main_edges()
        } else {
            (0.0, 0.0)
        };

        edges.retain(|edge| !edge.matches_vars(&self.br, &self.tl));
        if hor_edge == 1.0 {
            edges.push(Edge::new(self.br.clone(), self.tl.clone(), Axis::Vertical, frame));
            edges.push(Edge::new(self.tl.clone(), self.br.clone(), Axis::Vertical, frame));
        }
        if ver_edge == 1.0 {
            edges.push(Edge::new(self.tl.clone(), self.br.clone(), Axis::Horizontal, frame));
            edges.push(Edge::new(self.br.clone(), self.tl.clone(), Axis::Horizontal, frame));
        }

        self.edge_cons = vec![
            self.tl.x.var | EQ(REQUIRED) | hor_edge,
            self.tl.y.var | EQ(REQUIRED) | ver_edge,
            self.br.x.var | GE(REQUIRED) | self.tl.x.var,
            self.br.y.var | GE(REQUIRED) | self.tl.y.var,
        ];

        solver.add_constraints(&self.edge_cons).unwrap();
        solver.suggest_value(self.br.x.var, max.x as f64 - hor_edge).unwrap();
        solver.suggest_value(self.br.y.var, max.y as f64 - ver_edge).unwrap();
    }

    fn start(&self, axis: Axis) -> Variable {
        match axis {
            Axis::Horizontal => self.tl.x.var,
            Axis::Vertical => self.tl.y.var
        }
    }

    fn end(&self, axis: Axis) -> Variable {
        match axis {
            Axis::Horizontal => self.br.x.var,
            Axis::Vertical => self.br.y.var
        }
    }

    fn len(&self, axis: Axis) -> Expression {
        match axis {
            Axis::Horizontal => self.br.x.var - self.tl.x.var,
            Axis::Vertical => self.br.y.var - self.tl.y.var
        }
    }

    fn len_value(&self, axis: Axis) -> u16 {
        match axis {
            Axis::Horizontal => {
                self.br.x.value.load(Ordering::Relaxed) - self.tl.x.value.load(Ordering::Relaxed)
            }
            Axis::Vertical => {
                self.br.y.value.load(Ordering::Relaxed) - self.tl.y.value.load(Ordering::Relaxed)
            }
        }
    }

    pub fn tl(&self) -> Coord {
        Coord {
            x: self.tl.x.value.load(Ordering::Acquire),
            y: self.tl.y.value.load(Ordering::Acquire)
        }
    }

    pub fn br(&self) -> Coord {
        Coord {
            x: self.br.x.value.load(Ordering::Acquire),
            y: self.br.y.value.load(Ordering::Acquire)
        }
    }

    pub fn is_parent(&self) -> bool {
        self.lineage.is_some()
    }

    pub fn children(&self) -> Option<impl Iterator<Item = &RwData<Rect>>> {
        self.lineage
            .as_ref()
            .map(|(children, _)| children.iter().map(|(child, _)| child))
    }

    pub fn change_child_constraints(
        &mut self, index: usize, constraint: Constraint, solver: &mut Solver
    ) {
        let (children, axis) = self.lineage.as_mut().unwrap();
        let axis = *axis;

        let (child, mut constraints) = children[index].clone();

        child.inspect(|child| {
            if child.meets_constraint(constraint, axis, self.len_value(axis) as f64) {
                return;
            }

            constraints.change_defined(Some(constraint), self, index, solver);
        });

        let (children, _) = self.lineage.as_mut().unwrap();
        children[index].1 = constraints;
    }

    fn meets_constraint(&self, constraint: Constraint, axis: Axis, parent_len: f64) -> bool {
        let cur_len = self.len_value(axis) as f64;
        match constraint {
            Constraint::Ratio(den, div) => cur_len / parent_len == den as f64 / div as f64,
            Constraint::Percent(perc) => cur_len == parent_len * perc as f64,
            Constraint::Length(len) => cur_len == len,
            Constraint::Min(min) => cur_len >= min,
            Constraint::Max(max) => cur_len <= max
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn is_frameable(&self) -> bool {
        if let Some((children, _)) = &self.lineage {
            let child = children[0].0.read();
            let children_tied = child.tied_index.is_some_and(|index| index == self.index);
            children_tied && self.tied_index.is_none()
        } else {
            self.tied_index.is_none()
        }
    }

    fn form_frame(&self, frame: Frame, max: Coord, edges: &mut Vec<Edge>) -> (f64, f64, f64, f64) {
        let (right, up, left, down) = if self.is_frameable() {
            frame.edges(&self.br, &self.tl, max)
        } else {
            (0.0, 0.0, 0.0, 0.0)
        };

        edges.retain(|edge| !edge.matches_vars(&self.br, &self.tl));
        if right == 1.0 {
            edges.push(Edge::new(self.br.clone(), self.tl.clone(), Axis::Vertical, frame));
        }
        if up == 1.0 {
            edges.push(Edge::new(self.tl.clone(), self.br.clone(), Axis::Horizontal, frame));
        }
        if left == 1.0 {
            edges.push(Edge::new(self.tl.clone(), self.br.clone(), Axis::Vertical, frame));
        }
        if down == 1.0 {
            edges.push(Edge::new(self.br.clone(), self.tl.clone(), Axis::Horizontal, frame));
        }

        (right, up, left, down)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Line {
    Regular,
    Thick,
    Dashed,
    ThickDashed,
    Double,
    Rounded,
    Ascii,
    Custom(char)
}

#[derive(Debug)]
pub struct Edge {
    center: VarPoint,
    target: VarPoint,
    axis: Axis,
    pub frame: Frame
}

impl Edge {
    fn new(center: VarPoint, target: VarPoint, axis: Axis, frame: Frame) -> Self {
        Self {
            center,
            target,
            axis,
            frame
        }
    }

    fn matches_vars(&self, tl: &VarPoint, br: &VarPoint) -> bool {
        self.center == *tl && self.target == *br || self.center == *br && self.target == *tl
    }

    pub fn line_coords(&self) -> Coords {
        let (c_shift, t_shift) = if self.center < self.target {
            (1, 0)
        } else {
            (0, 1)
        };

        let target = match self.axis {
            Axis::Horizontal => Coord {
                x: self.target.x.value.load(Ordering::Acquire) - t_shift,
                y: self.center.y.value.load(Ordering::Acquire) - c_shift
            },
            Axis::Vertical => Coord {
                x: self.center.x.value.load(Ordering::Acquire) - c_shift,
                y: self.target.y.value.load(Ordering::Acquire) - t_shift
            }
        };
        let center = Coord {
            x: self.center.x.value.load(Ordering::Acquire) - c_shift,
            y: self.center.y.value.load(Ordering::Acquire) - c_shift
        };
        if self.center < self.target {
            Coords {
                tl: center,
                br: target
            }
        } else {
            Coords {
                tl: target,
                br: center
            }
        }
    }
}

/// Configuration about how and wether to frame a [`Rect`].
///
/// All options follow an anti-clockwise ordering, starting from
/// right:
///
/// - edges: (horizontal, vertical),
/// - corners: (up right, up left, down left, down right),
/// - joints: (right ver, up hor, left ver, down hor, ver hor)
/// - sides_to_show: (right, up, left, down)
#[derive(Clone, Copy, Debug)]
pub enum Frame {
    Empty,
    Surround(Line),
    Border(Line),
    Horizontal(Line),
    HorBorder(Line),
    Vertical(Line),
    VerBorder(Line)
}

impl Frame {
    fn edges(&self, br: &VarPoint, tl: &VarPoint, max: Coord) -> (f64, f64, f64, f64) {
        // log_info!("\n{:?}\nbr: {:?}\ntl: {:?}\nmax: {:?}\n", self, br, tl,
        // max);
        let right = br.x.value.load(Ordering::Acquire) == max.x;
        let up = tl.y.value.load(Ordering::Acquire) == 0;
        let left = tl.x.value.load(Ordering::Acquire) == 0;
        let down = br.y.value.load(Ordering::Acquire) == max.y;

        let (up, left) = match self {
            Frame::Surround(_) => (up as usize as f64, left as usize as f64),
            Frame::Horizontal(_) => (0.0, left as usize as f64),
            Frame::Vertical(_) => (up as usize as f64, 0.0),
            _ => (0.0, 0.0)
        };

        let (down, right) = match self {
            Frame::Surround(_) => (1.0, 1.0),
            Frame::Horizontal(_) => (0.0, 1.0),
            Frame::Vertical(_) => (1.0, 0.0),
            Frame::Border(_) => (!down as usize as f64, !right as usize as f64),
            Frame::HorBorder(_) => (0.0, !right as usize as f64),
            Frame::VerBorder(_) => (!down as usize as f64, 0.0),
            Frame::Empty => (0.0, 0.0)
        };

        (right, up, left, down)
    }

    fn main_edges(&self) -> (f64, f64) {
        match self {
            Frame::Surround(_) => (1.0, 1.0),
            Frame::Horizontal(_) => (1.0, 0.0),
            Frame::Vertical(_) => (0.0, 1.0),
            _ => (0.0, 0.0)
        }
    }

    pub fn line(&self) -> Option<Line> {
        match self {
            Frame::Empty => None,
            Frame::Surround(line)
            | Frame::Border(line)
            | Frame::Horizontal(line)
            | Frame::HorBorder(line)
            | Frame::Vertical(line)
            | Frame::VerBorder(line) => Some(*line)
        }
    }
}

pub struct Layout {
    vars: HashMap<Variable, Arc<AtomicU16>>,
    main: RwData<Rect>,
    max: Coord,
    floating: Vec<RwData<Rect>>,
    frame: Frame,
    edges: Vec<Edge>,
    pub active_index: usize,
    pub solver: Solver,
    pub vars_changed: AtomicBool
}

impl std::fmt::Debug for Layout {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Layout")
            .field("main", &self.main)
            .field("active_index", &self.active_index)
            .field("solver", &self.solver)
            .finish()
    }
}

impl Layout {
    pub fn new(frame: Frame) -> Self {
        let max = {
            let (width, height) = crossterm::terminal::size().unwrap();
            Coord::new(width, height)
        };
        let mut edges = Vec::new();
        let mut solver = Solver::new();
        let mut vars = HashMap::new();

        let mut main = Rect::new(&mut vars);

        solver.add_edit_variable(main.br.x.var, STRONG * 2.0).unwrap();
        solver.add_edit_variable(main.br.y.var, STRONG * 2.0).unwrap();

        main.set_main_constraints(Frame::Empty, &mut solver, &mut edges, max);

        let active_index = main.index;
        let mut layout = Layout {
            vars,
            main: RwData::new(main),
            max,
            floating: Vec::new(),
            edges,
            frame,
            active_index,
            solver,
            vars_changed: AtomicBool::new(false)
        };

        layout.update();

        layout.main.mutate(|main| {
            main.clear_constraints(&mut layout.solver);
            main.set_main_constraints(Frame::Empty, &mut layout.solver, &mut layout.edges, max);
        });

        layout.update();

        layout
    }

    pub fn main_index(&self) -> usize {
        self.main.read().index
    }

    pub fn fetch_index(&self, index: usize) -> Option<RwData<Rect>> {
        std::iter::once(&self.main)
            .chain(&self.floating)
            .find_map(|rect| fetch_index(rect, index))
    }

    pub fn fetch_parent(&self, index: usize) -> Option<(RwData<Rect>, usize, Axis)> {
        std::iter::once(&self.main)
            .chain(&self.floating)
            .find_map(|rect| fetch_parent(rect, index))
    }

    pub fn update(&mut self) {
        let mut vars_changed = false;
        for (var, value) in self.solver.fetch_changes() {
            self.vars[var].store(value.round() as u16, Ordering::Release);
            vars_changed = true;
        }

        if vars_changed {
            self.vars_changed.store(true, Ordering::Release);
        }
    }

    pub fn bisect(
        &mut self, mut index: usize, specs: PushSpecs, is_glued: bool
    ) -> (usize, Option<usize>) {
        let axis = Axis::from(specs);
        if is_glued {
            if let Some(rect) = self.fetch_index(index) {
                index = rect.read().tied_index.unwrap_or(index);
            }
        }

        // Check for a parent of `self` with the same `Axis`.
        let (parent, index, new_parent_index) = if let (false, Some((parent, index, _))) =
            (is_glued, self.fetch_parent(index).filter(|(.., cmp)| axis == *cmp))
        {
            let index = match specs.comes_earlier() {
                true => index,
                false => index + 1
            };

            (parent, index, None)
        // Checking if `self` is a parent with the same `Axis`.
        } else if let Some(parent) = self
            .fetch_index(index)
            .filter(|area| area.read().lineage.as_ref().is_some_and(|(_, cmp)| axis == *cmp))
        {
            let index = match specs.comes_earlier() {
                true => 0,
                false => parent.read().lineage.as_ref().unwrap().0.len()
            };

            (parent, index, None)
        // If all else fails, create a new parent to hold both `self`
        // and the new area.
        } else {
            let rect = self.fetch_index(index).unwrap();
            let child_is_main = rect.ptr_eq(&self.main);

            let (new_parent_index, mut child) = rect.mutate(|rect| {
                let parent = Rect::new_parent_of(rect, axis, &mut self.vars);
                (parent.index, std::mem::replace(rect, parent))
            });

            if is_glued {
                child.tied_index = Some(new_parent_index);
            }

            let index = match specs.comes_earlier() {
                true => 0,
                false => 1
            };

            rect.mutate(|parent| {
                parent.lineage = Some((vec![(RwData::new(child), Constraints::default())], axis));
            });

            // If the child is glued, the frame doesn't need to be redone.
            if !is_glued {
                if child_is_main {
                    let (solver, edges) = (&mut self.solver, &mut self.edges);
                    let mut rect = rect.write();
                    rect.clear_constraints(solver);
                    rect.set_main_constraints(self.frame, solver, edges, self.max);
                } else {
                    let (parent, index, _) = self.fetch_parent(rect.read().index).unwrap();
                    let mut parent = parent.write();
                    let (solver, edges) = (&mut self.solver, &mut self.edges);
                    prepare_child(&mut parent, index, solver, self.frame, self.max, edges)
                }
            }

            (rect, index, Some(new_parent_index))
        };

        self.update();

        let (temp_constraint, new_index, is_7) = parent.mutate(|mut parent| {
            let mut new = Rect::new(&mut self.vars);
            let is_7 = format!("{:?}", new.br).chars().nth(6).unwrap() == '7';
            if is_glued {
                new.tied_index = new_parent_index.or(Some(index));
            }
            let new_index = new.index;

            let len = parent
                .lineage
                .as_mut()
                .map(|(children, _)| {
                    children.insert(index, (RwData::new(new), Constraints::default()));
                    children.len()
                })
                .unwrap();

            specs.constraint.map(|defined| {
                let constraint = defined_constraint(defined, parent, index, axis);
                let (children, _) = parent.lineage.as_mut().unwrap();
                let (_, constraints) = &mut children[index];

                self.solver.add_constraint(constraint.clone()).unwrap();

                constraints.defined = Some((constraint, defined));
            });

            // We initially set the frame to `Frame::Empty`, since that will make
            // the `Rect`s take their "full space". What this means is that, when
            // we calculate the real frame, using `self.frame`, we know for a fact
            // which `Rect`s reach the edge of the screen. Unfortunately, this
            // does mean that we have to run `prepare_child()` twice for each
            // affected area.
            let edges = &mut self.edges;
            let next = (len - 1).checked_sub(index + 1).map(|_| index + 1);
            for &index in [index.checked_sub(1), Some(index), next].iter().flatten() {
                prepare_child(parent, index, &mut self.solver, Frame::Empty, self.max, edges);
            }

            // Add a constraint so that the new child `Rect` has a len equal to
            // `resizable_len / resizable_children`, a self imposed rule.
            // This will be useful later, when adding new ratio constraints to the
            // new child.
            let constraint = specs.constraint;
            let temp = if let Some(Constraint::Min(_) | Constraint::Max(_)) | None = constraint {
                let (children, axis) = parent.lineage.as_ref().unwrap();
                let (res_count, res_len) = children
                    .iter()
                    .filter(|(_, constraints)| constraints.is_resizable())
                    .fold((0, 0), |(count, len), (child, _)| {
                        (count + 1, len + child.read().len_value(*axis))
                    });

                let temp_constraint = {
                    let (new, _) = children[index].clone();
                    let new = new.read();
                    new.len(*axis) | EQ(WEAK * 2.0) | res_len as f64 / res_count as f64
                };

                self.solver.add_constraint(temp_constraint.clone()).unwrap();

                (Some(temp_constraint), new_index)
            } else {
                (None, new_index)
            };

            (temp.0, temp.1, is_7)
        });

        self.update();

        parent.mutate(|parent| {
            set_ratio_constraints(specs.constraint, parent, index, &mut self.solver);

            if let Some(temp_constraint) = temp_constraint {
                self.solver.remove_constraint(&temp_constraint).unwrap();
            }
        });

        self.update();

        parent.mutate(|parent| {
            let len = parent.children().unwrap().count();
            let edges = &mut self.edges;
            // Second frame calculation, this time, using the real `self.frame`.
            let next = (len - 1).checked_sub(index + 1).map(|_| index + 1);
            for &index in [index.checked_sub(1), Some(index), next].iter().flatten() {
                prepare_child(parent, index, &mut self.solver, self.frame, self.max, edges);
            }
        });

        self.update();

        // log_info!("{:#?}", self.edges);

        (new_index, new_parent_index)
    }

    pub fn width(&self) -> u16 {
        self.main.read().len_value(Axis::Horizontal)
    }

    pub fn height(&self) -> u16 {
        self.main.read().len_value(Axis::Vertical)
    }

    pub fn edges(&self) -> &[Edge] {
        self.edges.as_ref()
    }
}

fn prepare_child(
    parent: &mut Rect, index: usize, solver: &mut Solver, frame: Frame, max: Coord,
    edges: &mut Vec<Edge>
) {
    let (children, axis) = parent.lineage.as_ref().unwrap();
    let mut child = children[index].0.write();

    child.clear_constraints(solver);
    let (start, end) = child.set_constraints(parent, *axis, frame, max, edges);

    if index == 0 {
        let constraint = child.start(*axis) | EQ(REQUIRED) | parent.start(*axis) + start;
        child.edge_cons.push(constraint);
    }

    // Previous children carry the `Constraint`s for the `start` of their
    // successors.
    let constraint = if let Some((next, _)) = children.get(index + 1) {
        child.end(*axis) + end | EQ(REQUIRED) | next.read().start(*axis)
    } else {
        child.end(*axis) + end | EQ(REQUIRED) | parent.end(*axis)
    };
    child.edge_cons.push(constraint);

    solver.add_constraints(&child.edge_cons).unwrap();

    if let Some((children, _)) = &child.lineage {
        let len = children.len();
        for index in 0..len {
            prepare_child(&mut child, index, solver, frame, max, edges);
        }
    }
}

/// Returns a new instance of [`Constraints`].
///
/// It assumes that the new child [`Rect`] was already added to
/// the list of children, but with a [`Default`] [`Constraints`].
fn set_ratio_constraints(
    defined: Option<Constraint>, parent: &mut Rect, index: usize, solver: &mut Solver
) {
    let &(_, axis) = parent.lineage.as_ref().unwrap();

    let ratio = if let Some(Constraint::Min(_) | Constraint::Max(_)) | None = defined {
        let (children, _) = parent.lineage.as_mut().unwrap();

        let (new, _) = children[index].clone();
        let new = new.read();

        let prev = children.iter().take(index);
        let res_index = prev.filter(|(_, constraints)| constraints.is_resizable()).count();

        let mut children =
            children.iter_mut().filter(|(_, constraints)| constraints.is_resizable());

        if res_index > 0 {
            let (prev, constraints) = children.nth(res_index - 1).unwrap();

            let prev = prev.read();
            let ratio = if new.len_value(axis) == 0 {
                1.0
            } else {
                prev.len_value(axis) as f64 / new.len_value(axis) as f64
            };

            let constraint = prev.len(axis) | EQ(WEAK) | ratio * new.len(axis);
            constraints.ratio = Some((constraint.clone(), ratio));
            solver.add_constraint(constraint).unwrap();
        }

        children.nth(1).map(|(next, _)| {
            let next = next.read();
            let ratio = if next.len_value(axis) == 0 {
                1.0
            } else {
                new.len_value(axis) as f64 / next.len_value(axis) as f64
            };

            let constraint = new.len(axis) | EQ(WEAK) | ratio * next.len(axis);
            solver.add_constraint(constraint.clone()).unwrap();

            (constraint, ratio)
        })
    } else {
        None
    };

    parent.lineage.as_mut().unwrap().0[index].1.ratio = ratio;
}

fn defined_constraint(
    constraint: Constraint, parent: &Rect, index: usize, axis: Axis
) -> CassowaryConstraint {
    let (children, _) = parent.lineage.as_ref().unwrap();
    let (child, _) = &children[index];
    let child = child.read();
    match constraint {
        Constraint::Ratio(den, div) => {
            assert!(den < div, "Constraint::Ratio must be smaller than 1.");
            child.len(axis) | EQ(WEAK * 2.0) | parent.len(axis) * (den as f64 / div as f64)
        }
        Constraint::Percent(percent) => {
            assert!(percent <= 100, "Constraint::Percent must be smaller than 100");
            child.len(axis) | EQ(WEAK * 2.0) | parent.len(axis) * (percent as f64 / 100.0)
        }
        Constraint::Length(len) => child.len(axis) | EQ(STRONG) | len,
        Constraint::Min(min) => child.len(axis) | GE(MEDIUM) | min,
        Constraint::Max(max) => child.len(axis) | LE(MEDIUM) | max
    }
}

fn fetch_index(rect: &RwData<Rect>, index: usize) -> Option<RwData<Rect>> {
    if rect.read().index == index {
        Some(rect.clone())
    } else {
        rect.inspect(|rect| {
            rect.lineage
                .as_ref()
                .map(|(children, _)| {
                    children.iter().find_map(|(child, _)| fetch_index(child, index))
                })
                .flatten()
        })
    }
}

fn fetch_parent(main: &RwData<Rect>, index: usize) -> Option<(RwData<Rect>, usize, Axis)> {
    let rect = main.read();

    if rect.index == index {
        return None;
    }
    let Some((children, side)) = &rect.lineage else {
        return None;
    };

    children.iter().enumerate().find_map(|(pos, (child, _))| {
        if child.read().index == index {
            Some((main.clone(), pos, *side))
        } else {
            fetch_parent(child, index)
        }
    })
}
