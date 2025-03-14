use std::{
    fmt::Alignment,
    io::Write,
    sync::atomic::{AtomicBool, Ordering},
};

use cassowary::Variable;
use crossterm::{
    cursor::{self, MoveTo, MoveToColumn, MoveToNextLine},
    execute,
    style::Attribute,
    terminal,
};
use duat_core::{cfg::IterCfg, form, ui::Axis};
use sync_solver::SyncSolver;
use variables::Variables;

use crate::{AreaId, Coords, Equality, Mutex, area::Coord, layout::Rect, queue, style};

mod frame;
mod line;

pub use self::frame::{Brush, Frame};

pub struct Printer {
    sync_solver: Mutex<SyncSolver>,
    vars: Mutex<Variables>,
    lines: Mutex<Vec<(AreaId, Box<Lines>)>>,
    max: VarPoint,
    has_to_print_edges: AtomicBool,
}

impl Printer {
    /// Returns a new instance of [`Printer`]
    pub(crate) fn new() -> Self {
        let (vars, sync_solver, max) = {
            let mut vars = Variables::new();
            let (width, height) = crossterm::terminal::size().unwrap();
            let (width, height) = (width as f64, height as f64);

            let max = vars.new_point();
            let sync_solver = SyncSolver::new(&max, width, height);

            (vars, sync_solver, max)
        };

        Self {
            sync_solver: Mutex::new(sync_solver),
            vars: Mutex::new(vars),
            lines: Mutex::new(Vec::new()),
            max,
            has_to_print_edges: AtomicBool::new(false),
        }
    }

    ////////// Area setup functions

    /// Adds a new [`VarPoint`] to the list of [`Variable`]s and
    /// returns it
    pub fn new_point(&self) -> VarPoint {
        self.vars.lock().new_point()
    }

    /// Creates a new edge from the two [`VarPoint`]s
    ///
    /// This function will return the [`Variable`] representing the
    /// `width` of that edge. It can only have a value of `1` or `0`.
    pub fn set_edge(&self, lhs: VarPoint, rhs: VarPoint, axis: Axis, fr: Frame) -> Variable {
        self.vars.lock().set_edge([lhs, rhs], axis, fr)
    }

    /// Adds [`Equality`]s to the solver
    pub fn add_eqs(&self, eqs: impl IntoIterator<Item = Equality>) {
        self.sync_solver.lock().add_eqs(eqs);
    }

    ////////// Layout modification functions

    /// Removes [`Equality`]s from the solver
    pub fn remove_eqs(&self, eqs: impl IntoIterator<Item = Equality>) {
        // If there is no SavedVar, then the first term is a frame.
        self.sync_solver.lock().remove_eqs(eqs);
    }

    /// Removes an edge from the list of edges
    pub fn remove_edge(&self, edge: Variable) {
        self.vars.lock().remove_edge(edge);
    }

    /// Takes the [`Variables`] of a [`Rect`]
    ///
    /// This is done when swapping two [`Rect`]s from different
    /// windows.
    pub fn remove_rect(&self, rect: &mut Rect) -> [Variable; 4] {
        self.sync_solver.lock().remove_eqs(rect.drain_eqs());

        let mut vars = self.vars.lock();
        let [tl, br] = rect.var_points();
        if let Some(edge) = rect.edge() {
            vars.remove_edge(edge);
        }
        for var in [tl.x(), tl.y(), br.x(), br.y()] {
            vars.remove(var);
        }

        [tl.x(), tl.y(), br.x(), br.y()]
    }

    /// Inserts the [`Variables`] taken from a [`Rect`]
    pub fn insert_rect_vars(&self, new_vars: [Variable; 4]) {
        let mut vars = self.vars.lock();
        for var in new_vars {
            vars.insert(var);
        }
    }

    ////////// Updating functions

    /// Updates the value of all [`VarPoint`]s that have changed,
    /// returning true if any of them have.
    pub fn update(&self, change_max: bool) {
        let changes = {
            let mut ss = self.sync_solver.lock();
            ss.update(change_max, self.max).unwrap();
            ss.fetch_changes().to_vec()
        };

        let mut vars = self.vars.lock();
        vars.update_variables(changes);
        self.has_to_print_edges.store(true, Ordering::Relaxed);
        // Drop here, so self.updates and self.vars update "at the same time"
        drop(vars)
    }

    pub fn replace(
        &self,
        old_eqs: impl IntoIterator<Item = Equality>,
        new_eqs: impl IntoIterator<Item = Equality>,
    ) {
        let mut ss = self.sync_solver.lock();
        ss.remove_eqs(old_eqs);
        ss.add_eqs(new_eqs);
    }

    pub fn replace_and_update(
        &self,
        old_eqs: impl IntoIterator<Item = Equality>,
        new_eqs: impl IntoIterator<Item = Equality>,
        change_max: bool,
    ) {
        let changes = {
            let mut ss = self.sync_solver.lock();
            ss.remove_eqs(old_eqs);
            ss.add_eqs(new_eqs);
            ss.update(change_max, self.max).unwrap();
            ss.fetch_changes().to_vec()
        };

        let mut vars = self.vars.lock();
        vars.update_variables(changes);
        self.has_to_print_edges.store(true, Ordering::Relaxed);
        // Drop here, so self.updates and self.vars update "at the same time"
        drop(vars)
    }

    pub fn print(&self) {
        static CURSOR_IS_REAL: AtomicBool = AtomicBool::new(false);

        let stdout = if self.has_to_print_edges.swap(false, Ordering::Relaxed) {
            let mut stdout = std::io::stdout().lock();
            let id = form::id_of!("Frame");
            let edge_form = form::from_id(id);
            self.vars.lock().print_edges(&mut stdout, edge_form);
            Some(stdout)
        } else {
            None
        };

        let list: Vec<(AreaId, Box<Lines>)> = std::mem::take(&mut self.lines.lock());
        if list.is_empty() {
            return;
        }

        let mut stdout = stdout.unwrap_or_else(|| std::io::stdout().lock());
        let max = list.last().unwrap().1.coords().br;

        execute!(stdout, terminal::BeginSynchronizedUpdate).unwrap();
        queue!(stdout, cursor::Hide, MoveTo(0, 0));

        for y in 0..max.y {
            let mut x = 0;

            let iter = list.iter().flat_map(|(_, lines)| lines.on(y));

            for (bytes, start, end) in iter {
                if x != start {
                    queue!(stdout, MoveToColumn(start as u16));
                }

                stdout.write_all(bytes).unwrap();

                x = end;
            }

            queue!(stdout, MoveToNextLine(1));
        }

        let cursor_was_real = if let Some(was_real) = list
            .iter()
            .filter_map(|(_, lines)| lines.real_cursor)
            .reduce(|prev, was_real| prev || was_real)
        {
            CURSOR_IS_REAL.store(was_real, Ordering::Relaxed);
            was_real
        } else {
            CURSOR_IS_REAL.load(Ordering::Relaxed)
        };

        if cursor_was_real {
            queue!(stdout, cursor::RestorePosition, cursor::Show);
        }

        execute!(stdout, terminal::EndSynchronizedUpdate).unwrap();
    }

    /// Returns a new [`Lines`], a struct used to print to the screen
    pub fn lines(&self, [tl, br]: [VarPoint; 2], shift: u32, cfg: IterCfg) -> Lines {
        let coords = self.coords([tl, br], true);
        let cap = cfg.wrap_width(coords.width());
        let area = (coords.width() * coords.height()) as usize;
        let mut cutoffs = Vec::with_capacity(coords.height() as usize);
        cutoffs.push(0);

        Lines {
            bytes: Vec::with_capacity(area * 2),
            cutoffs,
            coords,
            real_cursor: None,

            line: Vec::new(),
            len: 0,
            positions: Vec::new(),
            gaps: Gaps::Left,
            default_gaps: Gaps::Left,

            shift,
            cap,
        }
    }

    /// Sends the finished [`Lines`], off to be printed
    pub fn send(&self, id: AreaId, lines: Lines) {
        let mut list = self.lines.lock();
        // This area may have been sent without being printed, in that case,
        // we just remove it.
        // Also, areas that intersect with this one came from a previous
        // organization of areas, so they should also be removed.
        list.retain(|(i, l)| *i != id && !l.coords().intersects(lines.coords()));

        let Err(i) = list.binary_search_by_key(&lines.coords(), |(_, lines)| lines.coords()) else {
            unreachable!("Colliding Lines should have been removed already");
        };
        list.insert(i, (id, Box::new(lines)))
    }

    ////////// Querying functions

    /// The maximum [`VarPoint`], i.e. the bottom right of the screen
    pub fn max(&self) -> &VarPoint {
        &self.max
    }

    /// Gets the current value of the [`Variable`]
    ///
    /// If `is_printing` [`Printer::has_changed`] will now return
    /// `false`
    pub fn value(&self, var: Variable, is_printing: bool) -> u32 {
        let (value, _) = self.vars.lock().value(var, is_printing);
        value
    }

    /// Gets the current [`Coord`] of a [`VarPoint`]
    ///
    /// If `is_printing` [`Printer::has_changed`] will now return
    /// `false`
    pub fn coord(&self, var_point: VarPoint, is_printing: bool) -> Coord {
        let mut vars = self.vars.lock();
        let (x, _) = vars.value(var_point.x(), is_printing);
        let (y, _) = vars.value(var_point.y(), is_printing);
        Coord::new(x, y)
    }

    /// Gets [`Coords`] from two [`VarPoint`]s
    pub fn coords(&self, var_points: [VarPoint; 2], is_printing: bool) -> Coords {
        let mut vars = self.vars.lock();
        let (tl, _) = vars.coord(var_points[0], is_printing);
        let (br, _) = vars.coord(var_points[1], is_printing);
        Coords::new(tl, br)
    }

    /// Wether a [`Variable`] has changed
    pub fn coords_have_changed(&self, [tl, br]: [VarPoint; 2]) -> bool {
        let vars = self.vars.lock();
        for var in [tl.x(), tl.y(), br.x(), br.y()] {
            if vars.has_changed(var) {
                return true;
            }
        }
        false
    }
}

unsafe impl Send for Printer {}
unsafe impl Sync for Printer {}

mod variables {
    use std::collections::HashMap;

    use cassowary::Variable;
    use crossterm::{
        cursor,
        style::{Print, ResetColor, SetStyle},
    };
    use duat_core::{form::Form, ui::Axis};

    use super::{Frame, VarPoint, frame::Edge};
    use crate::{Brush, area::Coord, print::frame::EdgeCoords, queue};

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
        pub fn print_edges(&mut self, stdout: &mut std::io::StdoutLock, edge_form: Form) {
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
                        Some(line) => super::line::horizontal(line, line),
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
                        Some(line) => super::line::vertical(line, line),
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
                    Print(super::line::crossing(right, up, left, down, true))
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
        pub fn coord(&mut self, var_point: VarPoint, is_printing: bool) -> (Coord, usize) {
            let (x, x_changes) = self.value(var_point.x(), is_printing);
            let (y, y_changes) = self.value(var_point.y(), is_printing);
            (Coord::new(x, y), x_changes + y_changes)
        }

        /// Wether a [`Variable`] has been changed
        pub fn has_changed(&self, var: Variable) -> bool {
            self.list.get(&var).unwrap().1 > 0
        }
    }
}

mod sync_solver {
    use cassowary::{
        AddConstraintError, RemoveConstraintError, Solver, Variable, strength::STRONG,
    };

    use super::VarPoint;
    use crate::Equality;

    pub struct SyncSolver {
        solver: Solver,
        eqs_to_add: Vec<Equality>,
        eqs_to_remove: Vec<Equality>,
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
            }
        }

        pub fn update(
            &mut self,
            change_max: bool,
            max: VarPoint,
        ) -> Result<(), AddConstraintError> {
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

            Ok(())
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

        pub fn fetch_changes(&mut self) -> &[(Variable, f64)] {
            self.solver.fetch_changes()
        }
    }
}

pub struct Lines {
    // Values that will be sent
    bytes: Vec<u8>,
    cutoffs: Vec<usize>,
    coords: Coords,
    real_cursor: Option<bool>,

    // Temporary things
    line: Vec<u8>,
    len: u32,
    positions: Vec<(usize, u32)>,
    gaps: Gaps,
    default_gaps: Gaps,

    // Outside information
    shift: u32,
    cap: u32,
}

impl Lines {
    pub fn push_char(&mut self, char: char, len: u32) {
        self.len += len;
        let mut bytes = [0; 4];
        char.encode_utf8(&mut bytes);
        self.positions.push((self.line.len(), len));
        self.line.extend(&bytes[..char.len_utf8()]);
    }

    pub fn realign(&mut self, alignment: Alignment) {
        self.gaps = match alignment {
            Alignment::Left => Gaps::Left,
            Alignment::Right => Gaps::Right,
            Alignment::Center => Gaps::Center,
        };
        self.default_gaps = self.gaps.clone();
    }

    pub fn add_spacer(&mut self) {
        match &mut self.gaps {
            Gaps::Left | Gaps::Right | Gaps::Center => {
                self.gaps = Gaps::Spacers(vec![self.line.len()])
            }
            Gaps::Spacers(items) => items.push(self.line.len()),
        };
    }

    pub fn show_real_cursor(&mut self) {
        self.real_cursor = Some(true);
    }

    pub fn hide_real_cursor(&mut self) {
        self.real_cursor = Some(false);
    }

    fn on(&self, y: u32) -> Option<(&[u8], u32, u32)> {
        let (tl, br) = (self.coords.tl, self.coords.br);

        if (tl.y..br.y).contains(&y) {
            let y = y - tl.y;
            let start = self.cutoffs[y as usize];

            let bytes = match self.cutoffs.get(y as usize + 1) {
                Some(end) => &self.bytes[start..*end],
                None => &self.bytes[start..],
            };

            Some((bytes, tl.x, br.x))
        } else {
            None
        }
    }

    pub fn end_line(&mut self, painter: &duat_core::form::Painter) {
        const BLANK: [u8; 1000] = [b' '; 1000];
        let mut default_form = painter.get_default();
        default_form.style.attributes.set(Attribute::Reset);

        let spacers = if let Gaps::Spacers(bytes) = &self.gaps {
            let mut sps_len = self.cap - self.len;
            while sps_len % bytes.len() as u32 > 0 {
                sps_len += 1;
            }
            let diff = sps_len - (self.cap - self.len);
            (0..bytes.len() as u32)
                .map(|i| (sps_len / bytes.len() as u32) - (i < diff) as u32)
                .collect()
        } else {
            Vec::new()
        };

        let (start_i, start_d) = {
            let mut dist = match &self.gaps {
                Gaps::Left => 0,
                Gaps::Right => self.cap - self.len,
                Gaps::Center => (self.cap - self.len) / 2,
                Gaps::Spacers(bytes) => spacers[0] * (bytes[0] == 0) as u32,
            };

            let Some(&(start, len)) = self.positions.iter().find(|(_, len)| {
                dist += len;
                dist > self.shift
                // Situation where the line is empty, from having no
                // characters or from being shifted out of sight.
            }) else {
                style!(self.bytes, default_form.style);
                self.bytes
                    .extend_from_slice(&BLANK[..self.coords.width() as usize]);
                self.cutoffs.push(self.bytes.len());

                self.line.clear();
                self.positions.clear();
                self.len = 0;
                return;
            };

            // If the character is cut by the start, don't print it.
            if dist - len < self.shift {
                let str = unsafe { std::str::from_utf8_unchecked(&self.line[start..]) };
                let char = str.chars().next().unwrap();
                (start + char.len_utf8(), dist - self.shift)
            } else {
                (start, dist - len - self.shift)
            }
        };

        let (end_i, end_d) = {
            let mut dist = match &self.gaps {
                Gaps::Left => self.len,
                Gaps::Right => self.cap,
                Gaps::Center => self.len + (self.cap - self.len) / 2,
                Gaps::Spacers(bytes) => {
                    let last = *spacers.last().unwrap();
                    self.cap - last * (*bytes.last().unwrap() == self.line.len()) as u32
                }
            };

            let Some(&(end, len)) = self.positions.iter().rev().find(|(_, len)| {
                dist -= len;
                dist < self.shift + self.coords.width()
            }) else {
                style!(self.bytes, default_form.style);
                self.bytes
                    .extend_from_slice(&BLANK[..self.coords.width() as usize]);
                self.cutoffs.push(self.bytes.len());

                self.line.clear();
                self.positions.clear();
                self.len = 0;
                return;
            };

            // If the character is cut by the end, don't print it.
            if dist + len > self.shift + self.coords.width() {
                (end, dist - self.shift)
            } else {
                let str = unsafe { std::str::from_utf8_unchecked(&self.line[end..]) };
                let char = str.chars().next().unwrap();
                (end + char.len_utf8(), dist + len - self.shift)
            }
        };

        style!(self.bytes, default_form.style);
        self.bytes.extend_from_slice(&BLANK[..start_d as usize]);

        self.add_ansi(start_i);

        if let Gaps::Spacers(bytes) = &mut self.gaps {
            let spacers = bytes
                .iter()
                .zip(spacers)
                .skip_while(|(b, _)| **b <= start_i)
                .take_while(|(b, _)| **b <= end_i);

            let mut start = start_i;

            for (&end, len) in spacers {
                self.bytes.extend_from_slice(&self.line[start..end]);
                self.bytes.extend_from_slice(&BLANK[..len as usize]);
                start = end;
            }

            self.bytes.extend_from_slice(&self.line[start..end_i]);
        } else {
            self.bytes.extend_from_slice(&self.line[start_i..end_i]);

            if self.coords.width() > end_d {
                style!(self.bytes, default_form.style);
                self.bytes
                    .extend_from_slice(&BLANK[..(self.coords.width() - end_d) as usize]);
            }
        }

        self.cutoffs.push(self.bytes.len());

        self.line.clear();
        self.positions.clear();
        self.gaps = self.default_gaps.clone();
        self.len = 0;
    }

    fn add_ansi(&mut self, start_i: usize) {
        let mut adding_ansi = false;
        for &b in &self.line[..start_i] {
            if b == 0x1b {
                adding_ansi = true;
                self.bytes.push(0x1b)
            } else if b == b'm' && adding_ansi {
                adding_ansi = false;
                self.bytes.push(b'm')
            } else if adding_ansi {
                self.bytes.push(b)
            }
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.line.is_empty()
    }

    pub fn coords(&self) -> Coords {
        self.coords
    }

    pub fn cap(&self) -> u32 {
        self.cap
    }
}

impl std::fmt::Debug for Lines {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bytes = unsafe { core::str::from_utf8_unchecked(&self.bytes) };
        let line = unsafe { core::str::from_utf8_unchecked(&self.line) };
        f.debug_struct("Lines")
            .field("bytes", &bytes)
            .field("cutoffs", &self.cutoffs)
            .field("coords", &self.coords)
            .field("real_cursor", &self.real_cursor)
            .field("line", &line)
            .field("len", &self.len)
            .field("shift", &self.shift)
            .field("cap", &self.cap)
            .field("positions", &self.positions)
            .field("align", &self.gaps)
            .finish()
    }
}

impl Write for Lines {
    /// For writing *ONLY* crossterm commands
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.line.extend(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone)]
enum Gaps {
    Left,
    Right,
    Center,
    Spacers(Vec<usize>),
}

/// A point on the screen, which can be calculated by [`cassowary`]
/// and interpreted by `duat_term`.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct VarPoint {
    y: Variable,
    x: Variable,
}

impl VarPoint {
    pub(super) fn new(x: Variable, y: Variable) -> Self {
        Self { y, x }
    }

    pub fn x(&self) -> Variable {
        self.x
    }

    pub fn y(&self) -> Variable {
        self.y
    }

    pub fn on_axis(&self, axis: Axis) -> Variable {
        match axis {
            Axis::Horizontal => self.x,
            Axis::Vertical => self.y,
        }
    }
}
