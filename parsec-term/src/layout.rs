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

#[derive(Clone)]
pub struct Rect {
    index: usize,
    tl: VarPoint,
    br: VarPoint,
    constraints: Vec<CassowaryConstraint>,
    pub len_constraint: Option<CassowaryConstraint>,
    pub children: Option<(Vec<RwData<Rect>>, Axis)>
}

impl std::fmt::Debug for Rect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Rect")
            .field("index", &self.index)
            .field("x", &self.tl)
            .field("y", &self.br)
            .field("children", &self.children)
            .finish()
    }
}

impl Rect {
    fn new(
        vars: &mut HashMap<Variable, Arc<AtomicU16>>, parent: Option<(&Rect, Constraint)>
    ) -> Self {
        let mut rect = Rect {
            index: unique_rect_index(),
            tl: VarPoint::new(vars),
            br: VarPoint::new(vars),
            constraints: Vec::new(),
            len_constraint: None,
            children: None
        };

        rect.len_constraint =
            parent.map(|(parent, constraint)| gen_constraint(constraint, &rect, parent));

        rect
    }

    fn new_parent_of(rect: &RwData<Rect>, vars: &mut HashMap<Variable, Arc<AtomicU16>>) -> Self {
        let mut rect = rect.write();
        let parent = Rect {
            index: unique_rect_index(),
            children: None,
            ..rect.clone()
        };

        rect.constraints.clear();
        rect.tl = VarPoint::new(vars);
        rect.br = VarPoint::new(vars);

        parent
    }

    fn clear_constraints(&mut self, solver: &mut Solver) {
        for constraint in self.constraints.drain(..) {
            solver.remove_constraint(&constraint).unwrap();
        }
        if let Some(constraint) = &self.len_constraint {
            solver.remove_constraint(constraint).unwrap();
        }
    }

    fn set_base_vars(&mut self, parent: &Rect, axis: Axis) {
        self.constraints.extend([
            self.tl.x_var | GE(REQUIRED) | 0.0,
            self.tl.y_var | GE(REQUIRED) | 0.0,
            self.br.x_var | GE(REQUIRED) | self.tl.x_var,
            self.br.y_var | GE(REQUIRED) | self.tl.y_var
        ]);

        if let Axis::Horizontal = axis {
            self.constraints.extend([
                self.tl.y_var | EQ(REQUIRED) | parent.tl.y_var,
                self.br.y_var | EQ(REQUIRED) | parent.br.y_var
            ]);
        } else {
            self.constraints.extend([
                self.tl.x_var | EQ(REQUIRED) | parent.tl.x_var,
                self.br.x_var | EQ(REQUIRED) | parent.br.x_var
            ]);
        }
    }

    fn add_constraints(&self, solver: &mut Solver) {
        solver.add_constraints(&self.constraints).unwrap();
        if let Some(constraint) = &self.len_constraint {
            solver.add_constraint(constraint.clone()).unwrap();
        }
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
        let mut main = Rect::new(&mut vars, None);
        let active_index = main.index;

        main.constraints = vec![
            main.tl.x_var | EQ(REQUIRED) | 0.0,
            main.tl.y_var | EQ(REQUIRED) | 0.0,
            main.br.x_var | GE(REQUIRED) | main.tl.x_var,
            main.br.y_var | GE(REQUIRED) | main.tl.y_var,
        ];

        solver.add_constraints(&main.constraints).unwrap();
        solver.add_edit_variable(main.br.x_var, STRONG).unwrap();
        solver.suggest_value(main.br.x_var, width as f64).unwrap();
        solver.add_edit_variable(main.br.y_var, STRONG).unwrap();
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

    fn update(&mut self) {
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
    }

    pub fn bisect(
        &mut self, index: usize, specs: PushSpecs, is_glued: bool
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
            let area = self.fetch_index(index).unwrap();
            let parent = Rect::new_parent_of(&area, &mut self.vars);
            let new_parent_index = Some(parent.index);

            let mut child = std::mem::replace(&mut *area.write(), parent);

            let index = match specs.comes_earlier() {
                true => 0,
                false => 1
            };

            if index == 0 {
                let parent = area.read();
                child.set_base_vars(&parent, axis);

                let constraint = child.end(axis) | EQ(REQUIRED) | parent.end(axis);
                child.constraints.push(constraint);
                child.add_constraints(&mut self.solver);
            }

            area.write().children = Some((vec![RwData::new(child)], axis));

            (area, index, new_parent_index)
        };

        let mut parent = parent.write();
        let (children, _) = parent.children.as_ref().unwrap();

        let mut new = Rect::new(&mut self.vars, Some((&parent, specs.constraint)));
        let new_index = new.index;
        new.set_base_vars(&parent, axis);

        // Previous children carry the `Constraint`s for the `start` of their
        // successors.
        if index > 0 {
            let mut prev = children.get(index - 1).unwrap().write();

            prev.clear_constraints(&mut self.solver);
            prev.set_base_vars(&parent, axis);

            let constraint = prev.end(axis) | EQ(REQUIRED) | new.start(axis);
            prev.constraints.push(constraint.clone());

            if index - 1 == 0 {
                let constraint = prev.start(axis) | EQ(REQUIRED) | parent.start(axis);
                prev.constraints.push(constraint);
            }

            prev.add_constraints(&mut self.solver);
        } else {
            let constraint = new.start(axis) | EQ(REQUIRED) | parent.start(axis);
            new.constraints.push(constraint);
        }

        if let Some(next) = children.get(index) {
            let constraint = new.end(axis) | EQ(REQUIRED) | next.read().start(axis);
            new.constraints.push(constraint);
        } else {
            let constraint = new.end(axis) | EQ(REQUIRED) | parent.end(axis);
            new.constraints.push(constraint);
        }
        prev.constraints.push(constraint);

        new.add_constraints(&mut self.solver);

        let (children, _) = parent.children.as_mut().unwrap();
        children.insert(index, RwData::new(new));
        drop(parent);

        self.update();

        (new_index, new_parent_index)
    }
}

fn fetch_index(rect: &RwData<Rect>, index: usize) -> Option<RwData<Rect>> {
    if rect.read().index == index {
        Some(rect.clone())
    } else {
        rect.inspect(|rect| {
            rect.children
                .as_ref()
                .map(|(children, _)| children.iter().find_map(|child| fetch_index(child, index)))
                .flatten()
        })
    }
}

fn fetch_parent(rect: &RwData<Rect>, index: usize) -> Option<(RwData<Rect>, usize, Axis)> {
    let rect_lock = rect.read();
    let Some((children, side)) = &rect_lock.children else {
        return None;
    };

    children.iter().enumerate().find_map(|(pos, child)| {
        if child.read().index == index {
            Some((rect.clone(), pos, *side))
        } else {
            fetch_parent(child, index)
        }
    })
}

pub fn gen_constraint(constraint: Constraint, child: &Rect, parent: &Rect) -> CassowaryConstraint {
    let &(_, axis) = parent.children.as_ref().unwrap();
    match constraint {
        Constraint::Ratio(den, div) => {
            assert!(den < div, "Constraint::Ratio must be smaller than 1.");
            child.len(axis) | EQ(WEAK) | parent.len(axis) * (den as f64 / div as f64)
        }
        Constraint::Percent(percent) => {
            assert!(percent <= 100, "Constraint::Percent must be smaller than 100");
            child.len(axis) | EQ(WEAK) | parent.len(axis) * (percent as f64 / 100f64)
        }
        Constraint::Length(len) => child.len(axis) | EQ(STRONG) | len,
        Constraint::Min(min) => child.len(axis) | GE(MEDIUM) | min,
        Constraint::Max(max) => child.len(axis) | LE(MEDIUM) | max
    }
}
