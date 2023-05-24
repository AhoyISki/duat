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

#[derive(Debug, Clone)]
struct Constraints {
    defined: Option<(CassowaryConstraint, Constraint)>,
    ratio: Option<(CassowaryConstraint, f64)>
}

impl Constraints {
    fn new(
        constraint: Option<Constraint>, parent: &mut Rect, new: &Rect, index: usize,
        solver: &mut Solver
    ) -> Self {
        let &(_, axis) = parent.children.as_ref().unwrap();

        let defined = constraint.map(|constraint| {
            let defined = match constraint {
                Constraint::Ratio(den, div) => {
                    assert!(den < div, "Constraint::Ratio must be smaller than 1.");
                    new.len(axis) | EQ(WEAK * 2.0) | parent.len(axis) * (den as f64 / div as f64)
                }
                Constraint::Percent(percent) => {
                    assert!(percent <= 100, "Constraint::Percent must be smaller than 100");
                    new.len(axis) | EQ(WEAK * 2.0) | parent.len(axis) * (percent as f64 / 100.0)
                }
                Constraint::Length(len) => new.len(axis) | EQ(STRONG) | len,
                Constraint::Min(min) => new.len(axis) | GE(MEDIUM) | min,
                Constraint::Max(max) => new.len(axis) | LE(MEDIUM) | max
            };

            solver.add_constraint(defined.clone()).unwrap();

            (defined, constraint)
        });

        let ratio = if let Some(Constraint::Min(_) | Constraint::Max(_)) | None = constraint {
            let parent_len = parent.len_value(axis) as f64;
            let (children, _) = parent.children.as_mut().unwrap();
            let resizables =
                children.iter().filter(|(_, constraints)| constraints.is_resizable()).count()
                    as f64
                    + 1.0;

            let mut children =
                children.iter_mut().filter(|(_, constraints)| constraints.is_resizable());

            let prev_child = if index > 0 {
                children.nth(index - 1)
            } else {
                None
            };

            let next_child = children.next();
            let mut next_len = None;

            let ret = next_child.map(|(next, _)| {
                let next = next.read();
                next_len = Some(next.len_value(axis) as f64);

                // The len of the new child will initially be equal to the parent's
                // len divided by the number of resizable children. The `len_ratio` is
                // the ratio needed, between this and the next_child`, in order to
                // achieve that.
                let ratio_val = parent_len / (resizables * next_len.unwrap());

                let constraint = new.len(axis) | EQ(WEAK) | ratio_val * next.len(axis);
                solver.add_constraint(constraint.clone()).unwrap();

                (constraint, ratio_val)
            });

            prev_child.map(|(prev, constraints)| {
                let prev = prev.read();

                let ratio_val = if let Some((constraint, _)) = &constraints.ratio {
                    solver.remove_constraint(constraint).unwrap();
                    // If `new_len == parent_len * next_len / resizables`,
                    // and previously `prev_len == next_len * ratio_val`,
                    // then `prev_len == new_len * resizables * ratio_val / parent_len`.
                    resizables * next_len.unwrap() / parent_len
                } else {
                    1.0
                };

                let constraint = prev.len(axis) | EQ(WEAK) | ratio_val * new.len(axis);
                constraints.ratio = Some((constraint.clone(), ratio_val));
                solver.add_constraint(constraint).unwrap();
            });

            ret
        } else {
            None
        };

        Constraints { defined, ratio }
    }

    fn remove(self, solver: &mut Solver) {
        if let Some((constraint, _)) = self.defined {
            solver.remove_constraint(&constraint).unwrap()
        }
        if let Some((constraint, _)) = self.ratio {
            solver.remove_constraint(&constraint).unwrap()
        }
    }

    fn is_resizable(&self) -> bool {
        match self.defined {
            Some((_, Constraint::Min(_) | Constraint::Max(_))) | None => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub struct Rect {
    index: usize,
    tl: VarPoint,
    br: VarPoint,
    edge_cons: Vec<CassowaryConstraint>,
    children: Option<(Vec<(RwData<Rect>, Constraints)>, Axis)>
}

// impl std::fmt::Debug for Rect {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) ->
// std::fmt::Result {        f.debug_struct("Rect")
//            .field("index", &self.index)
//            .field("x", &self.tl)
//            .field("y", &self.br)
//            .field("children", &self.children)
//            .finish()
//    }
//}

impl Rect {
    fn new(vars: &mut HashMap<Variable, Arc<AtomicU16>>) -> Self {
        Rect {
            index: unique_rect_index(),
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

    fn set_base_vars(&mut self, parent: &Rect, axis: Axis) {
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
        let (children, _) = self.children.as_mut().unwrap();

        let (child, mut constraints) = children.remove(index);

        child.inspect(|child| {
            let index = self.resizable_index(index);
            let new_constraints = Constraints::new(Some(constraint), self, &child, index, solver);

            let old_constraints = std::mem::replace(&mut constraints, new_constraints);
            old_constraints.remove(solver);
        });

        self.children.as_mut().unwrap().0.insert(index, (child, constraints));
    }

    fn resizable_index(&self, index: usize) -> usize {
        self.children
            .as_ref()
            .unwrap()
            .0
            .iter()
            .filter(|(_, constraints)| constraints.is_resizable())
            .take_while(|(child, _)| child.read().index < index)
            .count()
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
            .field("floating", &self.floating)
            .field("active_index", &self.active_index)
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
            self.vars[var].store(*value as u16, Ordering::Release);
            vars_changed = true;
        }
        if vars_changed {
            self.vars_changed.store(true, Ordering::Release);
        }

        log_info!("\n{:#?}", self);
    }

    pub fn bisect(
        &mut self, index: usize, specs: PushSpecs, _is_glued: bool
    ) -> (usize, Option<usize>) {
        let axis = Axis::from(specs);

        // Check for a parent of `self` with the same `Axis`.
        let (parent, index, new_parent_index) = if let Some((parent, index, _)) =
            self.fetch_parent(index).filter(|(.., cmp)| axis == *cmp)
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

            let (new_parent_index, child) = rect.mutate(|rect| {
                let parent = Rect::new_parent_of(rect, axis, &mut self.vars);
                (Some(parent.index), std::mem::replace(rect, parent))
            });

            let index = match specs.comes_earlier() {
                true => 0,
                false => 1
            };

            rect.mutate(|parent| {
                let index = parent.resizable_index(index);
                let constraints = Constraints::new(None, parent, &child, index, &mut self.solver);

                parent.children = Some((vec![(RwData::new(child), constraints)], axis));
            });

            (rect, index, new_parent_index)
        };

        let new_index = parent.mutate(|mut parent| {
            let new = Rect::new(&mut self.vars);
            let constraints = {
                let index = parent.resizable_index(index);
                Constraints::new(specs.constraint, &mut parent, &new, index, &mut self.solver)
            };
            let new_index = new.index;

            let len = parent
                .children
                .as_mut()
                .map(|(children, _)| {
                    children.insert(index, (RwData::new(new), constraints));
                    children.len()
                })
                .unwrap();

            if index < len - 1 {
                set_child_vars(&parent, index + 1, &mut self.solver);
            }
            if index > 0 {
                set_child_vars(&parent, index - 1, &mut self.solver);
            }
            set_child_vars(&parent, index, &mut self.solver);

            new_index
        });

        self.update();

        (new_index, new_parent_index)
    }
}

fn set_child_vars(parent: &Rect, index: usize, solver: &mut Solver) {
    let (children, axis) = parent.children.as_ref().unwrap();
    let mut child = children[index].0.write();
    child.clear_constraints(solver);
    child.set_base_vars(parent, *axis);

    // Previous children carry the `Constraint`s for the `start` of their
    // successors.
    if index == 0 {
        let constraint = child.start(*axis) | EQ(REQUIRED) | parent.start(*axis);
        child.edge_cons.push(constraint);
    }

    let constraint = if let Some((next, _)) = children.get(index + 1) {
        child.end(*axis) | EQ(REQUIRED) | next.read().start(*axis)
    } else {
        child.end(*axis) | EQ(REQUIRED) | parent.end(*axis)
    };
    child.edge_cons.push(constraint);
    child.add_constraints(solver);
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
