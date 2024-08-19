use std::{
    collections::HashMap,
    fmt::Alignment,
    io::{stdout, Write},
    sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Arc, Mutex,
    },
};

use cassowary::{strength::STRONG, AddConstraintError, Solver, Variable};
use crossterm::{
    cursor::{self, MoveTo, MoveToColumn, MoveToNextLine},
    execute,
    style::{ResetColor, SetStyle},
    terminal,
};
use duat_core::{palette::DEFAULT_FORM_ID, ui::Axis};

use crate::{area::Coord, Coords, Equality};

macro_rules! queue {
    ($writer:expr $(, $command:expr)* $(,)?) => {
        unsafe { crossterm::queue!($writer $(, $command)*).unwrap_unchecked() }
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
        let default_form = duat_core::palette::form_from_id(DEFAULT_FORM_ID);

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

pub struct Printer {
    eqs_to_add: Vec<Equality>,
    eqs_to_remove: Vec<Equality>,
    solver: Solver,
    map: HashMap<Variable, (Arc<AtomicUsize>, Arc<AtomicBool>)>,

    recvs: Vec<Receiver>,
    is_offline: bool,
    is_disabled: bool,
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
            eqs_to_add: Vec::new(),
            eqs_to_remove: Vec::new(),
            solver,
            map,

            recvs: Vec::new(),
            is_offline: false,
            is_disabled: false,
            max,
        }
    }

    pub fn var_value(&mut self) -> VarValue {
        let v = VarValue::new();
        self.map
            .insert(v.var, (v.value.clone(), v.has_changed.clone()));

        v
    }

    pub fn var_point(&mut self) -> VarPoint {
        let y = VarValue::new();
        let x = VarValue::new();
        self.map
            .insert(x.var, (x.value.clone(), x.has_changed.clone()));
        self.map
            .insert(y.var, (y.value.clone(), y.has_changed.clone()));

        VarPoint { y, x }
    }

    /// Updates the value of all [`VarPoint`]s that have changed,
    /// returning true if any of them have.
    pub fn update(&mut self, change_max: bool) -> bool {
        if change_max {
            let (width, height) = crossterm::terminal::size().unwrap();
            let (width, height) = (width as f64, height as f64);

            let max = &self.max;
            let _ = self.solver.suggest_value(max.x_var(), width);
            let _ = self.solver.suggest_value(max.y_var(), height);
        }

        let mut any_has_changed = false;
        for (var, new) in self.solver.fetch_changes() {
            let (val, has_changed) = &self.map[var];
            let new = new.round() as usize;
            let old = val.swap(new, Ordering::Release);
            any_has_changed |= old != new;
            has_changed.store(old != new, Ordering::Release);
        }

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

    pub fn vars_mut(&mut self) -> &mut HashMap<Variable, (Arc<AtomicUsize>, Arc<AtomicBool>)> {
        &mut self.map
    }

    pub fn add_equality(&mut self, eq: Equality) {
        self.eqs_to_add.push(eq);
    }

    pub fn add_equalities<'a>(&mut self, eqs: impl IntoIterator<Item = &'a Equality>) {
        self.eqs_to_add.extend(eqs.into_iter().cloned())
    }

    pub fn remove_equality(&mut self, eq: Equality) {
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

/// A [`Variable`], attached to its value, which is automatically kept
/// up to date.
#[derive(Clone)]
pub struct VarValue {
    var: Variable,
    value: Arc<AtomicUsize>,
    has_changed: Arc<AtomicBool>,
}

impl VarValue {
    pub fn var(&self) -> Variable {
        self.var
    }

    pub fn value(&self) -> usize {
        self.value.load(Ordering::Acquire)
    }

    /// Returns a new instance of [`VarValue`]
    fn new() -> Self {
        Self {
            var: Variable::new(),
            value: Arc::new(AtomicUsize::new(0)),
            has_changed: Arc::new(AtomicBool::new(false)),
        }
    }

    pub fn has_changed(&self) -> bool {
        self.has_changed.swap(false, Ordering::Release)
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

impl std::ops::BitOr<cassowary::WeightedRelation> for &VarValue {
    type Output = cassowary::PartialConstraint;

    fn bitor(self, rhs: cassowary::WeightedRelation) -> cassowary::PartialConstraint {
        self.var | rhs
    }
}

impl std::ops::BitOr<&VarValue> for cassowary::PartialConstraint {
    type Output = cassowary::Constraint;

    fn bitor(self, rhs: &VarValue) -> cassowary::Constraint {
        self | rhs.var
    }
}

impl std::ops::Add<f64> for &VarValue {
    type Output = cassowary::Expression;

    fn add(self, rhs: f64) -> cassowary::Expression {
        self.var + rhs
    }
}

impl std::ops::Add<&VarValue> for f64 {
    type Output = cassowary::Expression;

    fn add(self, rhs: &VarValue) -> cassowary::Expression {
        self + rhs.var
    }
}

impl std::ops::Add<&VarValue> for &VarValue {
    type Output = cassowary::Expression;

    fn add(self, rhs: &VarValue) -> cassowary::Expression {
        self.var + rhs.var
    }
}

impl std::ops::Add<cassowary::Term> for &VarValue {
    type Output = cassowary::Expression;

    fn add(self, rhs: cassowary::Term) -> cassowary::Expression {
        self.var + rhs
    }
}

impl std::ops::Add<&VarValue> for cassowary::Term {
    type Output = cassowary::Expression;

    fn add(self, rhs: &VarValue) -> cassowary::Expression {
        self + rhs.var
    }
}

impl std::ops::Add<cassowary::Expression> for &VarValue {
    type Output = cassowary::Expression;

    fn add(self, rhs: cassowary::Expression) -> cassowary::Expression {
        self.var + rhs
    }
}

impl std::ops::Add<&VarValue> for cassowary::Expression {
    type Output = cassowary::Expression;

    fn add(self, rhs: &VarValue) -> cassowary::Expression {
        self + rhs.var
    }
}

impl std::ops::Neg for &VarValue {
    type Output = cassowary::Term;

    fn neg(self) -> cassowary::Term {
        -self.var
    }
}

impl std::ops::Sub<f64> for &VarValue {
    type Output = cassowary::Expression;

    fn sub(self, rhs: f64) -> cassowary::Expression {
        self.var - rhs
    }
}

impl std::ops::Sub<&VarValue> for f64 {
    type Output = cassowary::Expression;

    fn sub(self, rhs: &VarValue) -> cassowary::Expression {
        self + rhs.var
    }
}

impl std::ops::Sub<&VarValue> for &VarValue {
    type Output = cassowary::Expression;

    fn sub(self, rhs: &VarValue) -> cassowary::Expression {
        self.var - rhs.var
    }
}

impl std::ops::Sub<cassowary::Term> for &VarValue {
    type Output = cassowary::Expression;

    fn sub(self, rhs: cassowary::Term) -> cassowary::Expression {
        self.var - rhs
    }
}

impl std::ops::Sub<&VarValue> for cassowary::Term {
    type Output = cassowary::Expression;

    fn sub(self, rhs: &VarValue) -> cassowary::Expression {
        self - rhs.var
    }
}

impl std::ops::Sub<cassowary::Expression> for &VarValue {
    type Output = cassowary::Expression;

    fn sub(self, rhs: cassowary::Expression) -> cassowary::Expression {
        self.var - rhs
    }
}

impl std::ops::Sub<&VarValue> for cassowary::Expression {
    type Output = cassowary::Expression;

    fn sub(self, rhs: &VarValue) -> cassowary::Expression {
        self - rhs.var
    }
}

impl std::ops::Mul<f64> for &VarValue {
    type Output = cassowary::Term;

    fn mul(self, rhs: f64) -> cassowary::Term {
        self.var * rhs
    }
}

impl std::ops::Mul<VarValue> for f64 {
    type Output = cassowary::Term;

    fn mul(self, rhs: VarValue) -> cassowary::Term {
        self * rhs.var
    }
}

impl std::ops::Div<f64> for VarValue {
    type Output = cassowary::Term;

    fn div(self, rhs: f64) -> cassowary::Term {
        self.var / rhs
    }
}

/// A point on the screen, which can be calculated by [`cassowary`]
/// and interpreted by `duat_term`.
#[derive(Clone, PartialEq, PartialOrd)]
pub struct VarPoint {
    y: VarValue,
    x: VarValue,
}

impl VarPoint {
    /// Returns a new instance of [`VarPoint`]
    pub fn new_from_hash_map(
        map: &mut HashMap<Variable, (Arc<AtomicUsize>, Arc<AtomicBool>)>,
    ) -> Self {
        let vp = VarPoint { x: VarValue::new(), y: VarValue::new() };

        map.insert(vp.x.var, (vp.x.value.clone(), vp.x.has_changed.clone()));
        map.insert(vp.y.var, (vp.y.value.clone(), vp.y.has_changed.clone()));

        vp
    }

    pub fn coord(&self) -> Coord {
        let x = self.x.value.load(Ordering::Relaxed);
        let y = self.y.value.load(Ordering::Relaxed);

        Coord::new(x, y)
    }

    pub fn x_var(&self) -> Variable {
        self.x.var
    }

    pub fn y_var(&self) -> Variable {
        self.y.var
    }

    pub fn y(&self) -> &VarValue {
        &self.y
    }

    pub fn x(&self) -> &VarValue {
        &self.x
    }

    pub fn on_axis(&self, axis: Axis) -> &VarValue {
        match axis {
            Axis::Horizontal => &self.x,
            Axis::Vertical => &self.y,
        }
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
