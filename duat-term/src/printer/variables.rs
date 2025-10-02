use std::collections::HashMap;

use cassowary::Variable;
use crossterm::{
    cursor,
    style::{Print, ResetColor, SetStyle},
};
use duat_core::{form::Form, ui::Axis};

use super::{Frame, VarPoint, edges::Edge};
    use super::{edges::EdgeCoords, stdout::Stdout};
use crate::{
    Brush,
    area::Coord,
    queue,
};

pub struct Variables {
    list: HashMap<Variable, (u32, usize)>,
    edges: Vec<(Variable, Edge)>,
    variable_fn: fn() -> Variable,
}

impl Variables {
    /// Returns a new instance of [`Variables`]
    pub fn new() -> Self {
        Self {
            list: HashMap::new(),
            edges: Vec::new(),
            variable_fn: Variable::new,
        }
    }

    ////////// Area setup functions

    /// Returns a new [`Variable`]
    pub fn new_var(&mut self) -> Variable {
        let var = (self.variable_fn)();
        self.list.insert(var, (0, 0));
        var
    }

    /// Returns a new [`VarPoint`]
    pub fn new_point(&mut self) -> VarPoint {
        VarPoint::new(self.new_var(), self.new_var())
    }

    /// Returns a new [`Variable`] for an [`Edge`]
    pub fn set_edge(&mut self, [lhs, rhs]: [VarPoint; 2], axis: Axis, fr: Frame) -> Variable {
        let var = (self.variable_fn)();
        self.edges.push((var, Edge::new(lhs, rhs, axis.perp(), fr)));
        var
    }

    ////////// Layout modification functions

    /// Removes a [`Variable`] to put in another window
    pub fn remove(&mut self, var: Variable) {
        self.list.remove(&var);
    }

    /// Inserts a [`Variable`] which came from another window
    pub fn insert(&mut self, var: Variable) {
        self.list.insert(var, (0, 0));
    }

    /// Removes an [`Edge`]
    pub fn remove_edge(&mut self, var: Variable) {
        self.edges.retain(|(v, _)| v != &var);
    }

    ////////// Updating functions

    /// Updates the [`Variable`]'s values, according to changes
    pub fn update_variables(&mut self, changes: Vec<(Variable, f64)>) {
        for (var, new) in changes {
            // If a Variable is not in this list, it is an edge's width, which is
            // never read, and as such, does not need to be updated.
            let Some((value, changes)) = self.list.get_mut(&var) else {
                continue;
            };

            let new = new.round() as u32;
            *changes += (*value != new) as usize;
            *value = new;
        }
    }

    /// Prints the [`Edge`]s
    pub fn print_edges(&mut self, stdout: &mut Stdout, edge_form: Form) {
        let edges: Vec<EdgeCoords> = {
            let edges = std::mem::take(&mut self.edges);
            let coords = edges.iter().filter_map(|(_, e)| e.coords(self)).collect();
            self.edges = edges;
            coords
        };

        let mut crossings = Vec::<(Coord, [Option<Brush>; 4])>::new();

        for (i, &coords) in edges.iter().enumerate() {
            if let Axis::Horizontal = coords.axis {
                let char = match coords.line {
                    Some(line) => super::edges::horizontal(line, line),
                    None => unreachable!(),
                };
                let line = char
                    .to_string()
                    .repeat((coords.br.x - coords.tl.x + 1) as usize);
                queue!(
                    stdout,
                    cursor::MoveTo(coords.tl.x as u16, coords.tl.y as u16),
                    ResetColor,
                    SetStyle(edge_form.style),
                    Print(line)
                )
            } else {
                let char = match coords.line {
                    Some(line) => super::edges::vertical(line, line),
                    None => unreachable!(),
                };

                for y in (coords.tl.y)..=coords.br.y {
                    queue!(
                        stdout,
                        cursor::MoveTo(coords.tl.x as u16, y as u16),
                        ResetColor,
                        SetStyle(edge_form.style),
                        Print(char)
                    )
                }
            }

            for &other_coords in edges[(i + 1)..].iter() {
                if let Some((coord, sides)) = coords.crossing(other_coords) {
                    let prev_crossing = crossings.iter_mut().find(|(c, ..)| *c == coord);
                    if let Some((_, [right, up, left, down])) = prev_crossing {
                        *right = right.or(sides[0]);
                        *up = up.or(sides[1]);
                        *left = left.or(sides[2]);
                        *down = down.or(sides[3]);
                    } else {
                        crossings.push((coord, sides));
                    }
                }
            }
        }

        for (coord, [right, up, left, down]) in crossings {
            queue!(
                stdout,
                cursor::MoveTo(coord.x as u16, coord.y as u16),
                SetStyle(edge_form.style),
                Print(super::edges::crossing(right, up, left, down, true))
            )
        }
    }

    ////////// Querying functions

    /// Returns the value of a given [`Variable`], and its changes
    ///
    /// If `is_printing`, [`Printer::has_changed`] will now return
    /// `false`
    ///
    /// [`Printer::has_changed`]: super::Printer::has_changed
    pub fn value(&mut self, var: Variable, is_printing_now: bool) -> (u32, usize) {
        let (value, changes) = self.list.get_mut(&var).unwrap();
        if is_printing_now {
            (*value, std::mem::take(changes))
        } else {
            (*value, 0)
        }
    }

    /// Returns the value of a given [`VarPoint`]
    ///
    /// If `is_printing`, [`Printer::has_changed`] will now return
    /// `false`
    ///
    /// [`Printer::has_changed`]: super::Printer::has_changed
    pub fn coord(&mut self, var_point: VarPoint, is_printing: bool) -> (Coord, bool) {
        let (x, x_changes) = self.value(var_point.x(), is_printing);
        let (y, y_change) = self.value(var_point.y(), is_printing);
        (Coord::new(x, y), x_changes + y_change != 0)
    }

    /// Wether a [`Variable`] has been changed
    pub fn has_changed(&self, var: Variable) -> bool {
        self.list.get(&var).unwrap().1 > 0
    }
}
