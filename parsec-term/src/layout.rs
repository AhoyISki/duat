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
    ui::{Axis, Constraint, PushSpecs}
};

use crate::area::Coord;

fn unique_rect_index() -> usize {
    static INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

    INDEX_COUNTER.fetch_add(1, Ordering::SeqCst)
}

#[derive(Clone)]
struct VarPoint {
    x_var: Variable,
    x_value: Arc<AtomicU16>,
    y_var: Variable,
    y_value: Arc<AtomicU16>
}

impl std::fmt::Debug for VarPoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{:?}: {}, {:?}: {}",
            self.x_var,
            self.x_value.load(Ordering::Relaxed),
            self.y_var,
            self.y_value.load(Ordering::Relaxed)
        ))
    }
}

impl VarPoint {
    fn new(vars: &mut HashMap<Variable, Arc<AtomicU16>>) -> Self {
        let element = VarPoint {
            x_var: Variable::new(),
            x_value: Arc::new(AtomicU16::new(0)),
            y_var: Variable::new(),
            y_value: Arc::new(AtomicU16::new(0))
        };

        vars.insert(element.x_var, element.x_value.clone());
        vars.insert(element.y_var, element.y_value.clone());

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
            let &(_, axis) = parent.children.as_ref().unwrap();
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

// impl std::fmt::Debug for Rect {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) ->
// std::fmt::Result {         f.debug_struct("Rect")
//             .field("index", &self.index)
//             .field("x", &self.tl)
//             .field("y", &self.br)
//             .field(
//                 "children",
//                 &self.children.as_ref().map(|(children, axis)| {
//                     (
//                         axis,
//                         children
//                             .iter()
//                             .map(|(rect, _)| rect)
//                             .cloned()
//                             .collect::<Vec<RwData<Rect>>>()
//                     )
//                 })
//             )
//             .finish()
//     }
// }
#[derive(Clone, Debug)]
pub struct Rect {
    index: usize,
    /// The index that this [`Rect`] is tied to.
    tied_index: Option<usize>,
    tl: VarPoint,
    br: VarPoint,
    edge_cons: Vec<CassowaryConstraint>,
    children: Option<(Vec<(RwData<Rect>, Constraints)>, Axis)>
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
            children: None
        }
    }

    fn new_parent_of(
        rect: &mut Rect, axis: Axis, vars: &mut HashMap<Variable, Arc<AtomicU16>>
    ) -> Self {
        let parent = Rect {
            index: unique_rect_index(),
            children: Some((Vec::new(), axis)),
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

    fn set_base_constraints(&mut self, parent: &Rect, axis: Axis) {
        self.edge_cons.extend([
            self.tl.x_var | GE(REQUIRED) | 0.0,
            self.tl.y_var | GE(REQUIRED) | 0.0,
            self.br.x_var | GE(REQUIRED) | self.tl.x_var,
            self.br.y_var | GE(REQUIRED) | self.tl.y_var
        ]);

        if let Axis::Horizontal = axis {
            self.edge_cons.extend([
                self.tl.y_var | EQ(REQUIRED) | parent.tl.y_var,
                self.br.y_var | EQ(REQUIRED) | parent.br.y_var
            ]);
        } else {
            self.edge_cons.extend([
                self.tl.x_var | EQ(REQUIRED) | parent.tl.x_var,
                self.br.x_var | EQ(REQUIRED) | parent.br.x_var
            ]);
        }
    }

    fn add_constraints(&self, solver: &mut Solver) {
        solver.add_constraints(&self.edge_cons).unwrap();
    }

    fn start(&self, axis: Axis) -> Variable {
        match axis {
            Axis::Horizontal => self.tl.x_var,
            Axis::Vertical => self.tl.y_var
        }
    }

    fn end(&self, axis: Axis) -> Variable {
        match axis {
            Axis::Horizontal => self.br.x_var,
            Axis::Vertical => self.br.y_var
        }
    }

    fn len(&self, axis: Axis) -> Expression {
        match axis {
            Axis::Horizontal => self.br.x_var - self.tl.x_var,
            Axis::Vertical => self.br.y_var - self.tl.y_var
        }
    }

    fn len_value(&self, axis: Axis) -> u16 {
        match axis {
            Axis::Horizontal => {
                self.br.x_value.load(Ordering::Relaxed) - self.tl.x_value.load(Ordering::Relaxed)
            }
            Axis::Vertical => {
                self.br.y_value.load(Ordering::Relaxed) - self.tl.y_value.load(Ordering::Relaxed)
            }
        }
    }

    pub fn tl(&self) -> Coord {
        Coord {
            x: self.tl.x_value.load(Ordering::Acquire),
            y: self.tl.y_value.load(Ordering::Acquire)
        }
    }

    pub fn br(&self) -> Coord {
        Coord {
            x: self.br.x_value.load(Ordering::Acquire),
            y: self.br.y_value.load(Ordering::Acquire)
        }
    }

    pub fn is_parent(&self) -> bool {
        self.children.is_some()
    }

    pub fn children(&self) -> Option<impl Iterator<Item = &RwData<Rect>>> {
        self.children
            .as_ref()
            .map(|(children, _)| children.iter().map(|(child, _)| child))
    }

    pub fn change_child_constraints(
        &mut self, index: usize, constraint: Constraint, solver: &mut Solver
    ) {
        let (children, axis) = self.children.as_mut().unwrap();
        let axis = *axis;

        let (child, mut constraints) = children[index].clone();

        child.inspect(|child| {
            if child.meets_constraint(constraint, axis, self.len_value(axis) as f64) {
                return;
            }

            constraints.change_defined(Some(constraint), self, index, solver);
        });

        let (children, _) = self.children.as_mut().unwrap();
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

    fn set_tied_index(&mut self, index: usize) {
        self.tied_index = Some(index);
        if let Some((children, _)) = &mut self.children {
            for (child, _) in children {
                child.write().set_tied_index(index);
            }
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

pub struct Layout {
    vars: HashMap<Variable, Arc<AtomicU16>>,
    main: RwData<Rect>,
    floating: Vec<RwData<Rect>>,
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
    pub fn new() -> Self {
        let (width, height) = crossterm::terminal::size().unwrap();
        let mut solver = Solver::new();

        let mut vars = HashMap::new();
        let mut main = Rect::new(&mut vars);
        let active_index = main.index;

        main.edge_cons = vec![
            main.tl.x_var | EQ(REQUIRED) | 0.0,
            main.tl.y_var | EQ(REQUIRED) | 0.0,
            main.br.x_var | GE(REQUIRED) | main.tl.x_var,
            main.br.y_var | GE(REQUIRED) | main.tl.y_var,
        ];

        solver.add_constraints(&main.edge_cons).unwrap();
        solver.add_edit_variable(main.br.x_var, STRONG * 2.0).unwrap();
        solver.suggest_value(main.br.x_var, width as f64).unwrap();
        solver.add_edit_variable(main.br.y_var, STRONG * 2.0).unwrap();
        solver.suggest_value(main.br.y_var, height as f64).unwrap();

        let mut layout = Layout {
            vars,
            main: RwData::new(main),
            floating: Vec::new(),
            active_index,
            solver,
            vars_changed: AtomicBool::new(false)
        };

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
        self.main.inspect(|main| {
            let width = main.br.x_var;
            let height = main.br.y_var;

            let (term_width, term_height) = crossterm::terminal::size().unwrap();

            self.solver.suggest_value(width, term_width as f64).unwrap();
            self.solver.suggest_value(height, term_height as f64).unwrap();
        });

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
            .filter(|area| area.read().children.as_ref().is_some_and(|(_, cmp)| axis == *cmp))
        {
            let index = match specs.comes_earlier() {
                true => 0,
                false => parent.read().children.as_ref().unwrap().0.len()
            };

            (parent, index, None)
        // If all else fails, create a new parent to hold both `self`
        // and the new area.
        } else {
            let rect = self.fetch_index(index).unwrap();

            let (new_parent_index, mut child) = rect.mutate(|rect| {
                let parent = Rect::new_parent_of(rect, axis, &mut self.vars);
                (parent.index, std::mem::replace(rect, parent))
            });

            if is_glued {
                child.set_tied_index(new_parent_index);
            }

            let index = match specs.comes_earlier() {
                true => 0,
                false => 1
            };

            rect.mutate(|parent| {
                parent.children = Some((vec![(RwData::new(child), Constraints::default())], axis));
            });

            (rect, index, Some(new_parent_index))
        };

        let (temp_constraint, new_index) = parent.mutate(|mut parent| {
            let mut new = Rect::new(&mut self.vars);
            if is_glued {
                new.tied_index = new_parent_index.or(Some(index));
            }
            let new_index = new.index;

            let len = parent
                .children
                .as_mut()
                .map(|(children, _)| {
                    children.insert(index, (RwData::new(new), Constraints::default()));
                    children.len()
                })
                .unwrap();

            specs.constraint.map(|defined| {
                let constraint = defined_constraint(defined, parent, index, axis);
                let (children, _) = parent.children.as_mut().unwrap();
                let (_, constraints) = &mut children[index];

                self.solver.add_constraint(constraint.clone()).unwrap();

                constraints.defined = Some((constraint, defined));
            });

            if index < len - 1 {
                set_child_vars(&mut parent, index + 1, &mut self.solver);
            }
            if index > 0 {
                set_child_vars(&mut parent, index - 1, &mut self.solver);
            }
            set_child_vars(&mut parent, index, &mut self.solver);

            // Add a constraint so that the new child `Rect` has a len equal to
            // `resizable_len / resizable_children`, a self imposed rule.
            // This will be useful later, when adding new ratio constraints to the
            // new child.

            if let Some(Constraint::Min(_) | Constraint::Max(_)) | None = specs.constraint {
                let (children, axis) = parent.children.as_ref().unwrap();
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
            }
        });

        self.update();

        parent.mutate(|parent| {
            set_ratio_constraints(specs.constraint, parent, index, &mut self.solver);

            if let Some(temp_constraint) = temp_constraint {
                self.solver.remove_constraint(&temp_constraint).unwrap();
            }
        });

        self.update();

        (new_index, new_parent_index)
    }
}

fn set_child_vars(parent: &mut Rect, index: usize, solver: &mut Solver) {
    let (children, axis) = parent.children.as_ref().unwrap();
    children[index].0.mutate(|child| {
        child.clear_constraints(solver);

        child.set_base_constraints(parent, *axis);

        if index == 0 {
            child.edge_cons.push(child.start(*axis) | EQ(REQUIRED) | parent.start(*axis));
        }

        // Previous children carry the `Constraint`s for the `start` of their
        // successors.
        let constraint = if let Some((next, _)) = children.get(index + 1) {
            child.end(*axis) | EQ(REQUIRED) | next.read().start(*axis)
        } else {
            child.end(*axis) | EQ(REQUIRED) | parent.end(*axis)
        };
        child.edge_cons.push(constraint);

        child.add_constraints(solver);

        if let Some((children, _)) = &child.children {
            let len = children.len();
            for index in 0..len {
                set_child_vars(child, index, solver);
            }
        }
    });
}

/// Returns a new instance of [`Constraints`].
///
/// It assumes that the new child [`Rect`] was already added to
/// the list of children, but with a [`Default`] [`Constraints`].
fn set_ratio_constraints(
    defined: Option<Constraint>, parent: &mut Rect, index: usize, solver: &mut Solver
) {
    let &(_, axis) = parent.children.as_ref().unwrap();

    let ratio = if let Some(Constraint::Min(_) | Constraint::Max(_)) | None = defined {
        let (children, _) = parent.children.as_mut().unwrap();

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

    parent.children.as_mut().unwrap().0[index].1.ratio = ratio;
}

fn defined_constraint(
    constraint: Constraint, parent: &Rect, index: usize, axis: Axis
) -> CassowaryConstraint {
    let (children, _) = parent.children.as_ref().unwrap();
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
            rect.children
                .as_ref()
                .map(|(children, _)| {
                    children.iter().find_map(|(child, _)| fetch_index(child, index))
                })
                .flatten()
        })
    }
}

fn fetch_parent(main: &RwData<Rect>, index: usize) -> Option<(RwData<Rect>, usize, Axis)> {
    let rect_lock = main.read();
    let Some((children, side)) = &rect_lock.children else {
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
