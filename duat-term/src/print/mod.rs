use std::{
    collections::HashMap,
    io::{stdout, Write},
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc, Mutex,
    },
};

use cassowary::{strength::STRONG, AddConstraintError, RemoveConstraintError, Solver, Variable};
use crossterm::cursor::{self, MoveTo, MoveToColumn, MoveToNextLine};

use crate::{layout::VarPoint, Coords, Equality};

macro_rules! queue {
    ($writer:expr $(, $command:expr)* $(,)?) => {
        unsafe { crossterm::queue!($writer $(, $command)*).unwrap_unchecked() }
    }
}

#[derive(Debug)]
pub struct Lines {
    pub bytes: Vec<u8>,
    cutoffs: Vec<usize>,
    coords: Coords,
    real_cursor: Option<bool>,
}

impl Lines {
    pub fn show_real_cursor(&mut self) {
        self.real_cursor = Some(true);
    }

    pub fn hide_real_cursor(&mut self) {
        self.real_cursor = Some(false);
    }

    fn on(&self, y: usize) -> Option<(&[u8], usize, usize)> {
        let (tl, br) = (self.coords.tl(), self.coords.br());

        if (tl.y..br.y).contains(&y) {
            let y = y - tl.y;
            let start = self.cutoffs[y];

            let (start_x, end_x) = (self.coords.tl().x, self.coords.br().x);
            let bytes = match self.cutoffs.get(y + 1) {
                Some(end) => &self.bytes[start..*end],
                None => &self.bytes[start..],
            };

            Some((bytes, start_x, end_x))
        } else {
            None
        }
    }
}

impl Write for Lines {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.bytes.extend_from_slice(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.cutoffs.push(self.bytes.len());
        Ok(())
    }
}

struct Receiver {
    lines: Arc<Mutex<Option<Lines>>>,
    coords: Coords,
}

impl Receiver {
    fn take(&self) -> Option<Lines> {
        self.lines.lock().unwrap().take()
    }
}

#[derive(Debug)]
pub struct Sender {
    lines: Arc<Mutex<Option<Lines>>>,
    coords: Coords,
}

impl Sender {
    pub fn lines(&self) -> Lines {
        let area = self.coords.width() * self.coords.height();
        let mut cutoffs = Vec::with_capacity(self.coords.height());
        cutoffs.push(0);

        Lines {
            bytes: Vec::with_capacity(area * 2),
            cutoffs,
            coords: self.coords,
            real_cursor: None,
        }
    }

    pub fn send(&self, lines: Lines) {
        *self.lines.lock().unwrap() = Some(lines);
    }
}

pub struct Printer {
    solver: Solver,
    map: HashMap<Variable, (Arc<AtomicUsize>, Arc<AtomicBool>)>,

    recvs: Vec<Receiver>,
    is_offline: bool,
    max: VarPoint,
}

impl Printer {
    pub fn new() -> Self {
        let (map, solver, max) = {
            let (width, height) = crossterm::terminal::size().unwrap();
            let (width, height) = (width as f64, height as f64);

            let mut map = HashMap::new();
            let mut solver = Solver::new();
            let max = VarPoint::new_from_hash_map(&mut map);
            let strong = STRONG * 5.0;
            solver.add_edit_variable(max.x_var(), strong).unwrap();
            solver.suggest_value(max.x_var(), width).unwrap();

            solver.add_edit_variable(max.y_var(), strong).unwrap();
            solver.suggest_value(max.y_var(), height).unwrap();

            (map, solver, max)
        };

        Self {
            solver,
            map,

            recvs: Vec::new(),
            is_offline: false,
            max,
        }
    }

    /// Updates the value of all [`VarPoint`]s that have changed,
    /// returning true if any of them have.
    pub fn update(&mut self, change_max: bool) -> bool {
        if change_max {
            let (width, height) = crossterm::terminal::size().unwrap();
            let (width, height) = (width as f64, height as f64);

            let max = &self.max;
            self.solver.suggest_value(max.x_var(), width).unwrap();
            self.solver.suggest_value(max.y_var(), height).unwrap();
        }

        self.recvs.clear();

        let mut any_has_changed = false;
        for (var, new) in self.solver.fetch_changes() {
            let (value, has_changed) = &self.map[var];

            let new = new.round() as usize;
            let old = value.swap(new, Ordering::Release);
            any_has_changed |= old != new;
            has_changed.store(old != new, Ordering::Release);
        }

        any_has_changed
    }

    pub fn sender(&mut self, coords: Coords) -> Sender {
        let receiver = Receiver {
            lines: Arc::new(Mutex::new(None)),
            coords,
        };

        let sender = Sender {
            lines: receiver.lines.clone(),
            coords,
        };

        match self
            .recvs
            .binary_search_by(|other| other.coords.cmp(&coords))
        {
            Ok(_) => unreachable!("One receiver per area."),
            Err(pos) => self.recvs.insert(pos, receiver),
        }

        sender
    }

    pub fn shutdown(&mut self) {
        self.is_offline = true;
    }

    pub fn is_offline(&self) -> bool {
        self.is_offline
    }

    pub fn print(&self) {
        static CURSOR_IS_REAL: AtomicBool = AtomicBool::new(false);
        let list: Vec<_> = self.recvs.iter().flat_map(Receiver::take).collect();

        if list.is_empty() {
            return;
        }

        let mut stdout = stdout().lock();
        queue!(stdout, cursor::Hide, MoveTo(0, 0));

        for y in 0..self.max.coord().y {
            let mut x = 0;

            let iter = list.iter().flat_map(|lines| lines.on(y));

            for (bytes, start, end) in iter {
                if x != start {
                    queue!(stdout, MoveToColumn(start as u16));
                }

                stdout.write_all(bytes).unwrap();

                x = end;
            }

            queue!(stdout, MoveToNextLine(1));
        }

        let was_real = if let Some(was_real) = list
            .iter()
            .filter_map(|lines| lines.real_cursor)
            .reduce(|prev, was_real| prev || was_real)
        {
            CURSOR_IS_REAL.store(was_real, Ordering::Relaxed);
            was_real
        } else {
            CURSOR_IS_REAL.load(Ordering::Relaxed)
        };

        if was_real {
            queue!(stdout, cursor::RestorePosition, cursor::Show);
        }

        stdout.flush().unwrap();
    }

    pub fn vars_mut(&mut self) -> &mut HashMap<Variable, (Arc<AtomicUsize>, Arc<AtomicBool>)> {
        &mut self.map
    }

    pub fn add_equality(&mut self, eq: Equality) -> Result<(), AddConstraintError> {
        self.solver.add_constraint(eq)
    }

    pub fn add_equalities<'a>(
        &mut self,
        eqs: impl IntoIterator<Item = &'a Equality>,
    ) -> Result<(), AddConstraintError> {
        self.solver.add_constraints(eqs)
    }

    pub fn remove_equality(&mut self, eq: &Equality) -> Result<(), RemoveConstraintError> {
        self.solver.remove_constraint(eq)
    }

    pub fn max(&self) -> &VarPoint {
        &self.max
    }
}

impl Default for Printer {
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl Send for Printer {}
unsafe impl Sync for Printer {}