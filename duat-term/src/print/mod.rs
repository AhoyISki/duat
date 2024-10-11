use std::{
    fmt::Alignment,
    io::{stdout, Write},
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc, LazyLock, Mutex,
    },
};

use cassowary::{strength::STRONG, AddConstraintError, Solver, Variable};
use crossterm::{
    cursor::{self, MoveTo, MoveToColumn, MoveToNextLine},
    execute,
    style::{Print, ResetColor, SetStyle},
    terminal,
};
use duat_core::{
    forms::{self, FormId, DEFAULT_FORM_ID},
    ui::Axis,
};

use self::frame::Edge;
use crate::{area::Coord, Coords, Equality};

mod frame;
mod line;
mod var;

pub use self::{
    frame::{Brush, Frame},
    var::{VarPoint, VarValue},
};

pub struct Printer {
    eqs_to_add: Vec<Equality>,
    eqs_to_remove: Vec<Equality>,
    solver: Solver,
    vars: Vec<(Variable, SavedVar)>,
    edges: Vec<Edge>,

    recvs: Vec<Receiver>,
    is_offline: bool,
    is_disabled: bool,
    max: VarPoint,
}

impl Printer {
    pub fn new() -> Self {
        let (vars, solver, max) = {
            let (width, height) = crossterm::terminal::size().unwrap();
            let (width, height) = (width as f64, height as f64);

            let mut vars = Vec::new();
            let mut solver = Solver::new();
            let max = VarPoint::new_from_map(&mut vars);
            let strong = STRONG * 3.0;
            solver.add_edit_variable(max.x_var(), strong).unwrap();
            solver.suggest_value(max.x_var(), width).unwrap();

            solver.add_edit_variable(max.y_var(), strong).unwrap();
            solver.suggest_value(max.y_var(), height).unwrap();

            (vars, solver, max)
        };

        Self {
            eqs_to_add: Vec::new(),
            eqs_to_remove: Vec::new(),
            solver,
            vars,
            edges: Vec::new(),

            recvs: Vec::new(),
            is_offline: false,
            is_disabled: false,
            max,
        }
    }

    pub fn edge(&mut self, lhs: &VarPoint, rhs: &VarPoint, axis: Axis, fr: Frame) -> VarValue {
        let width = VarValue::new();
        let var = &lhs.on_axis(axis).var;
        let i = self.vars.binary_search_by_key(var, key).unwrap();
        let saved = self.vars.get_mut(i).unwrap();
        saved.1 = SavedVar::frame(lhs.on_axis(axis), rhs.on_axis(axis), &width);

        let edge = Edge::new(&width.value, lhs, rhs, axis.perp(), fr);
        self.edges.push(edge);

        width
    }

    pub fn set_copy(&mut self, vv: &VarValue, copy: &VarValue) {
        let copy = {
            let i = self.vars.binary_search_by_key(&copy.var, key).unwrap();
            match self.vars.get(i).unwrap() {
                (_, SavedVar::Val { .. } | SavedVar::Edge { .. }) => copy.value.clone(),
                (_, SavedVar::Copy { copy, .. }) => copy.clone(),
            }
        };

        let i = self.vars.binary_search_by_key(&vv.var, key).unwrap();
        self.vars.get_mut(i).unwrap().1 = SavedVar::copy(vv, copy);
    }

    pub fn var_point(&mut self) -> VarPoint {
        let x = VarValue::new();
        let y = VarValue::new();
        self.vars.push((x.var, SavedVar::val(&x)));
        self.vars.push((y.var, SavedVar::val(&y)));

        VarPoint::new(x, y)
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

        let mut any_has_changed = false;
        let (framed, mut frames, copies) = {
            let mut framed = Vec::new();
            let mut frames = Vec::new();
            let mut copies: Vec<(&Arc<AtomicUsize>, _, _)> = Vec::new();

            for (var, new) in self.solver.fetch_changes() {
                let i = self.vars.binary_search_by_key(var, key).ok();
                match i.and_then(|i| self.vars.get(i)) {
                    Some((_, SavedVar::Val { value, has_changed })) => {
                        let new = new.round() as usize;
                        let old = value.swap(new, Ordering::Release);
                        any_has_changed |= old != new;
                        has_changed.store(old != new, Ordering::Release);
                    }
                    Some((_, SavedVar::Edge { width: frame, rhs, value, has_changed })) => {
                        framed.push((frame, rhs, value, has_changed));
                    }
                    Some((_, SavedVar::Copy { copy, value, has_changed })) => {
                        copies.push((copy, value, has_changed))
                    }
                    // In this case, the variable is a frame.
                    None => frames.push((var, new)),
                }
            }

            (framed, frames, copies)
        };

        for (frame, rhs, value, has_changed) in framed {
            if let Some((_, new)) = frames.extract_if(|(var, _)| **var == frame.var).next() {
                let new = new.round() as usize;
                let old = frame.value.swap(new, Ordering::Release);
                any_has_changed |= old != new;
                has_changed.store(old != new, Ordering::Release);
            }

            let new = rhs.load(Ordering::Acquire) - frame.value();
            let old = value.swap(new, Ordering::Release);
            any_has_changed |= old != new;
            has_changed.fetch_or(old != new, Ordering::Acquire);
        }

        for (copy, value, has_changed) in copies {
            let new = copy.load(Ordering::Acquire);
            let old = value.swap(new, Ordering::Release);
            any_has_changed = old != new;
            has_changed.store(old != new, Ordering::Release);
        }

        print_edges(&self.edges);

        any_has_changed
    }

    pub fn sender(&mut self, tl: &VarPoint, br: &VarPoint) -> Sender {
        let recv = Receiver {
            lines: Arc::new(Mutex::new(None)),
            tl: tl.clone(),
            br: br.clone(),
        };

        let sender = Sender {
            lines: recv.lines.clone(),
            tl: tl.clone(),
            br: br.clone(),
        };

        let (Ok(i) | Err(i)) = self
            .recvs
            .binary_search_by(|other| other.coords().cmp(&recv.coords()));
        self.recvs.insert(i, recv);

        sender
    }

    pub fn remove_sender(&mut self, sender: Sender) {
        self.recvs
            .retain(|recv| !Arc::ptr_eq(&recv.lines, &sender.lines));
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
        execute!(stdout, terminal::BeginSynchronizedUpdate).unwrap();
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

        let cursor_was_real = if let Some(was_real) = list
            .iter()
            .filter_map(|lines| lines.real_cursor)
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

    pub fn add_equality(&mut self, eq: Equality) {
        self.eqs_to_add.push(eq);
    }

    pub fn add_equalities<'a>(&mut self, eqs: impl IntoIterator<Item = &'a Equality>) {
        self.eqs_to_add.extend(eqs.into_iter().cloned())
    }

    pub fn remove_equality(&mut self, eq: Equality) {
        // If there is no SavedVar, then the first term is a frame.
        let var = eq.expr().terms[0].variable;
        if let Ok(i) = self.vars.binary_search_by_key(&var, key)
            && let Some((_, saved)) = self.vars.get_mut(i)
        {
            match saved {
                SavedVar::Val { .. } => {}
                SavedVar::Edge { value, has_changed, width, .. } => {
                    self.edges.retain(|e| !Arc::ptr_eq(&e.width, &width.value));

                    let (value, has_changed) = (value.clone(), has_changed.clone());
                    *saved = SavedVar::Val { value, has_changed };
                }
                SavedVar::Copy { value, has_changed, .. } => {
                    let (value, has_changed) = (value.clone(), has_changed.clone());
                    *saved = SavedVar::Val { value, has_changed };
                }
            }
        }
        if self.eqs_to_add.extract_if(|e| *e == eq).count() == 0 {
            self.eqs_to_remove.push(eq);
        }
    }

    pub fn disable(&mut self) {
        self.is_disabled = true;
    }

    pub fn enable(&mut self) {
        self.is_disabled = false;
    }

    pub fn max(&self) -> &VarPoint {
        &self.max
    }

    pub fn flush_equalities(&mut self) -> Result<(), AddConstraintError> {
        for eq in self.eqs_to_remove.drain(..) {
            self.solver.remove_constraint(&eq).unwrap();
        }
        self.solver.add_constraints(&self.eqs_to_add)?;
        self.update(false);
        self.eqs_to_add.clear();
        Ok(())
    }
}

impl Default for Printer {
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl Send for Printer {}
unsafe impl Sync for Printer {}

#[derive(Debug)]
struct Receiver {
    lines: Arc<Mutex<Option<Lines>>>,
    tl: VarPoint,
    br: VarPoint,
}

impl Receiver {
    fn take(&self) -> Option<Lines> {
        self.lines.lock().unwrap().take()
    }

    fn coords(&self) -> Coords {
        Coords::new(self.tl.coord(), self.br.coord())
    }
}

#[derive(Debug)]
pub struct Sender {
    lines: Arc<Mutex<Option<Lines>>>,
    tl: VarPoint,
    br: VarPoint,
}

impl Sender {
    pub fn lines(&self, shift: usize, cap: usize) -> Lines {
        let area = self.coords().width() * self.coords().height();
        let mut cutoffs = Vec::with_capacity(self.coords().height());
        cutoffs.push(0);

        Lines {
            bytes: Vec::with_capacity(area * 2),
            cutoffs,
            coords: self.coords(),
            real_cursor: None,

            line: Vec::new(),
            len: 0,
            positions: Vec::new(),
            align: Alignment::Left,
            shift,
            cap,
        }
    }

    pub fn send(&self, lines: Lines) {
        *self.lines.lock().unwrap() = Some(lines);
    }

    pub fn coords(&self) -> Coords {
        Coords::new(self.tl.coord(), self.br.coord())
    }
}

#[derive(Debug)]
pub struct Lines {
    bytes: Vec<u8>,
    cutoffs: Vec<usize>,
    coords: Coords,
    real_cursor: Option<bool>,

    line: Vec<u8>,
    len: usize,
    positions: Vec<(usize, usize)>,
    align: Alignment,
    shift: usize,
    cap: usize,
}

impl Lines {
    pub fn push_char(&mut self, char: char, len: usize) {
        self.len += len;
        let mut bytes = [0; 4];
        char.encode_utf8(&mut bytes);
        self.positions.push((self.line.len(), len));
        self.line.extend(&bytes[..char.len_utf8()]);
    }

    pub fn realign(&mut self, alignment: Alignment) {
        self.align = alignment;
    }

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

    pub(crate) fn is_empty(&self) -> bool {
        self.line.is_empty()
    }
}

impl Write for Lines {
    /// For writing *ONLY* crossterm commands
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.line.extend(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        const BLANK: [u8; 1000] = [b' '; 1000];
        let default_form = duat_core::forms::from_id(DEFAULT_FORM_ID);

        let align_start = match self.align {
            Alignment::Left => 0,
            Alignment::Right => self.cap - self.len,
            Alignment::Center => (self.cap - self.len) / 2,
        };

        let (start_i, start_d) = {
            let mut dist = align_start;
            let Some(&(start, len)) = self.positions.iter().find(|(_, len)| {
                dist += len;
                dist > self.shift
            }) else {
                queue!(self.bytes, ResetColor, SetStyle(default_form.style));
                self.bytes.extend_from_slice(&BLANK[..self.coords.width()]);
                self.cutoffs.push(self.bytes.len());

                self.line.clear();
                self.positions.clear();
                self.len = 0;
                return Ok(());
            };

            // If tre character is cut by the start, don't print it.
            if dist - len < self.shift {
                let str = unsafe { std::str::from_utf8_unchecked(&self.line[start..]) };
                let char = str.chars().next().unwrap();
                (start + char.len_utf8(), dist - self.shift)
            } else {
                (start, dist - len - self.shift)
            }
        };

        let (end_i, end_d) = {
            let mut dist = align_start + self.len;

            let Some(&(end, len)) = self.positions.iter().rev().find(|(_, len)| {
                dist -= len;
                dist < self.shift + self.coords.width()
            }) else {
                queue!(self.bytes, ResetColor, SetStyle(default_form.style));
                self.bytes.extend_from_slice(&BLANK[..self.coords.width()]);
                self.cutoffs.push(self.bytes.len());

                self.line.clear();
                self.positions.clear();
                self.len = 0;
                return Ok(());
            };

            // If tre character is cut by the end, don't print it.
            if dist + len > self.shift + self.coords.width() {
                (end, dist - self.shift)
            } else {
                let str = unsafe { std::str::from_utf8_unchecked(&self.line[end..]) };
                let char = str.chars().next().unwrap();
                (end + char.len_utf8(), dist + len - self.shift)
            }
        };

        queue!(self.bytes, ResetColor, SetStyle(default_form.style));
        self.bytes.extend_from_slice(&BLANK[..start_d]);

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

        self.bytes.extend_from_slice(&self.line[start_i..end_i]);
        queue!(self.bytes, ResetColor, SetStyle(default_form.style));
        self.bytes
            .extend_from_slice(&BLANK[..(self.coords.width() - end_d)]);
        self.cutoffs.push(self.bytes.len());

        self.line.clear();
        self.positions.clear();
        self.len = 0;
        Ok(())
    }
}

pub enum SavedVar {
    Val {
        value: Arc<AtomicUsize>,
        has_changed: Arc<AtomicBool>,
    },
    Edge {
        width: VarValue,
        rhs: Arc<AtomicUsize>,
        value: Arc<AtomicUsize>,
        has_changed: Arc<AtomicBool>,
    },
    Copy {
        copy: Arc<AtomicUsize>,
        value: Arc<AtomicUsize>,
        has_changed: Arc<AtomicBool>,
    },
}

impl SavedVar {
    fn val(vv: &VarValue) -> Self {
        Self::Val {
            value: vv.value.clone(),
            has_changed: vv.has_changed.clone(),
        }
    }

    fn frame(lhs: &VarValue, rhs: &VarValue, frame: &VarValue) -> Self {
        Self::Edge {
            width: frame.clone(),
            rhs: rhs.value.clone(),
            value: lhs.value.clone(),
            has_changed: lhs.has_changed.clone(),
        }
    }

    fn copy(vv: &VarValue, copy: Arc<AtomicUsize>) -> Self {
        Self::Copy {
            copy,
            value: vv.value.clone(),
            has_changed: vv.has_changed.clone(),
        }
    }
}

fn print_edges(edges: &[Edge]) {
    static FRAME_FORM: LazyLock<FormId> = LazyLock::new(|| forms::set_weak_ref("Frame", "Default"));
    let frame_form = forms::from_id(*FRAME_FORM);

    let mut stdout = std::io::stdout().lock();

    let edges: Vec<_> = edges.iter().filter_map(|edge| edge.edge_coords()).collect();

    let mut crossings = Vec::<(Coord, [Option<Brush>; 4])>::new();

    for (i, &coords) in edges.iter().enumerate() {
        if let Axis::Horizontal = coords.axis {
            let char = match coords.line {
                Some(line) => line::horizontal(line, line),
                None => unreachable!(),
            };
            let line = char.to_string().repeat(coords.br.x - coords.tl.x + 1);
            queue!(
                stdout,
                cursor::MoveTo(coords.tl.x as u16, coords.tl.y as u16),
                ResetColor,
                SetStyle(frame_form.style),
                Print(line)
            )
        } else {
            let char = match coords.line {
                Some(line) => line::vertical(line, line),
                None => unreachable!(),
            };

            for y in (coords.tl.y)..=coords.br.y {
                queue!(
                    stdout,
                    cursor::MoveTo(coords.tl.x as u16, y as u16),
                    ResetColor,
                    SetStyle(frame_form.style),
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
            SetStyle(frame_form.style),
            Print(line::crossing(right, up, left, down, true))
        )
    }
}

macro queue($writer:expr $(, $command:expr)* $(,)?) {
    unsafe { crossterm::queue!($writer $(, $command)*).unwrap_unchecked() }
}

fn key(entry: &(Variable, SavedVar)) -> Variable {
    entry.0
}
