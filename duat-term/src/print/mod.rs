use std::{
    fmt::Alignment,
    io::{Write, stdout},
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU32, AtomicUsize, Ordering},
    },
};

use cassowary::Variable;
use crossterm::{
    cursor::{self, MoveTo, MoveToColumn, MoveToNextLine},
    execute,
    style::Attribute,
    terminal,
};
use duat_core::{Mutex, session, ui::Axis};
use sync_solver::SyncSolver;
use variables::Variables;

use crate::{Coords, Equality, queue, style};

mod frame;
mod line;
mod var;

pub use self::{
    frame::{Brush, Frame},
    var::{VarPoint, VarValue},
};

pub struct Printer {
    sync_solver: Mutex<SyncSolver>,
    vars: Mutex<Variables>,
    recvs: Mutex<Vec<Receiver>>,
    max: VarPoint,
    has_to_print_edges: AtomicBool,
    updates: AtomicUsize,
}

impl Printer {
    pub fn new() -> Self {
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
            recvs: Mutex::new(Vec::new()),
            max,
            has_to_print_edges: AtomicBool::new(false),
            updates: AtomicUsize::new(0),
        }
    }

    pub fn var_point(&self) -> VarPoint {
        self.vars.lock().new_point()
    }

    pub fn set_edge(&self, lhs: &VarPoint, rhs: &VarPoint, axis: Axis, fr: Frame) -> VarValue {
        self.vars.lock().set_edge([lhs, rhs], axis, fr)
    }

    pub fn set_copy(&self, vv: &VarValue, above: &VarValue) {
        self.vars.lock().set_copy(vv, above);
    }

    /// Updates the value of all [`VarPoint`]s that have changed,
    /// returning true if any of them have.
    pub fn update(&self, change_max: bool) {
        let mut sync_solver = self.sync_solver.lock();
        sync_solver.update(change_max, &self.max).unwrap();

        let vars = self.vars.lock();
        let mut has_changeds = vars.update_variables(sync_solver.fetch_changes());
        drop(sync_solver);

        if !has_changeds.is_empty() {
            let mut to_remove = Vec::new();
            for (_, above) in has_changeds.iter() {
                // Parents should never show that they've been changed, since that
                // property will never actually be checked.
                // If that were to be set, printing would be paused forever.
                if let Some(above) = above
                    && let Some(i) = has_changeds
                        .iter()
                        .position(|(hc, _)| Arc::ptr_eq(hc, &above.has_changed))
                {
                    if let Err(j) = to_remove.binary_search(&i) {
                        to_remove.insert(j, i)
                    }
                }
            }

            for i in to_remove.into_iter().rev() {
                has_changeds.remove(i);
            }

            let mut new_up = 0;
            for (has_changed, _) in has_changeds {
                let prev = has_changed.swap(true, Ordering::Release);

                // Only add to the updates if those have been checked.
                if !prev
                    && !Arc::ptr_eq(has_changed, &self.max.y().has_changed)
                    && !Arc::ptr_eq(has_changed, &self.max.x().has_changed)
                {
                    new_up += 1;
                }
            }
            let prev_up = self.updates.fetch_add(new_up, Ordering::Release);
            duat_core::log_file!("updates at {}", prev_up + new_up);
            if prev_up + new_up > 0 {
                let _ = session::pause_printing();
                self.has_to_print_edges.store(true, Ordering::Relaxed);
            }
        }
    }

    pub fn sender(&self, tl: &VarPoint, br: &VarPoint) -> Sender {
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

        let mut recvs = self.recvs.lock();
        let (Ok(i) | Err(i)) = recvs.binary_search_by_key(&recv.coords(), Receiver::coords);
        recvs.insert(i, recv);

        sender
    }

    pub fn remove_sender(&self, sender: Sender) {
        self.recvs
            .lock()
            .retain(|recv| !Arc::ptr_eq(&recv.lines, &sender.lines));
    }

    pub fn add_eqs<'a>(&self, eqs: impl IntoIterator<Item = &'a Equality>) {
        self.sync_solver.lock().add_eqs(eqs.into_iter().cloned());
    }

    pub fn remove_eq(&self, eq: Equality) {
        // If there is no SavedVar, then the first term is a frame.
        self.sync_solver.lock().remove_eqs([eq.clone()]);
        self.vars.lock().remove_eq(eq);
    }

    pub fn remove_edge(&self, edge: VarValue) {
        self.vars.lock().remove_edge(&edge);
    }

    pub fn take_rect_parts(
        &self,
        rect: &crate::layout::Rect,
    ) -> (Vec<(Variable, SavedVar)>, Option<Receiver>) {
        let mut vars = self.vars.lock();
        let (tl, br) = rect.var_points();
        if let Some(edge) = rect.edge() {
            vars.remove_edge(edge);
        }
        let vars = vars.take_from(rect);

        let receiver = self
            .recvs
            .lock()
            .extract_if(.., |recv| recv.tl == *tl && recv.br == *br)
            .next();

        (vars, receiver)
    }

    pub fn insert_rect_parts(&self, saved: Vec<(Variable, SavedVar)>, recv: Option<Receiver>) {
        let mut vars = self.vars.lock();
        for (var, saved) in saved {
            vars.insert((var, saved));
        }

        if let Some(recv) = recv {
            let mut recvs = self.recvs.lock();
            let (Ok(i) | Err(i)) = recvs.binary_search_by_key(&recv.coords(), Receiver::coords);
            recvs.insert(i, recv);
        }
    }

    pub fn max(&self) -> &VarPoint {
        &self.max
    }

    pub fn print(&self) {
        static CURSOR_IS_REAL: AtomicBool = AtomicBool::new(false);
        let list: Vec<Lines> = self.recvs.lock().iter().flat_map(Receiver::take).collect();

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

        if self.has_to_print_edges.swap(false, Ordering::Relaxed) {
            self.vars.lock().print_edges();
        }

        execute!(stdout, terminal::EndSynchronizedUpdate).unwrap();
    }

    pub unsafe fn notice_updates(&self, n: usize) -> usize {
        let prev = self.updates.fetch_sub(n, Ordering::Relaxed);
        duat_core::log_file!("updates at {}", prev - n);
        prev - n
    }
}

impl Default for Printer {
    fn default() -> Self {
        Self::new()
    }
}

unsafe impl Send for Printer {}
unsafe impl Sync for Printer {}

mod variables {
    use std::sync::{
        Arc, LazyLock,
        atomic::{AtomicBool, Ordering},
    };

    use cassowary::Variable;
    use crossterm::{
        cursor,
        style::{Print, ResetColor, SetStyle},
    };
    use duat_core::{
        form::{self, FormId},
        ui::Axis,
    };

    use super::{Frame, SavedVar, VarPoint, VarValue, frame::Edge};
    use crate::{Brush, area::Coord, layout::Rect, queue};

    pub struct Variables {
        saved: Vec<(Variable, SavedVar)>,
        edges: Vec<Edge>,
        var_value_fn: fn() -> VarValue,
    }

    impl Variables {
        pub fn new() -> Self {
            Self {
                saved: Vec::new(),
                edges: Vec::new(),
                var_value_fn: VarValue::new,
            }
        }

        pub fn new_var(&mut self) -> VarValue {
            let vv = (self.var_value_fn)();
            self.saved.push((vv.var, SavedVar::val(&vv)));
            vv
        }

        pub fn new_point(&mut self) -> VarPoint {
            VarPoint::new(self.new_var(), self.new_var())
        }

        pub fn set_edge(&mut self, [lhs, rhs]: [&VarPoint; 2], axis: Axis, fr: Frame) -> VarValue {
            let width = (self.var_value_fn)();
            let var = &lhs.on_axis(axis).var;

            let i = self.saved.binary_search_by_key(var, key_fn).unwrap();
            let saved = self.saved.get_mut(i).unwrap();
            saved.1 = SavedVar::frame(lhs.on_axis(axis), rhs.on_axis(axis), &width);

            let edge = Edge::new(&width.value, lhs, rhs, axis.perp(), fr);
            self.edges.push(edge);

            width
        }

        pub fn set_copy(&mut self, vv: &VarValue, above: &VarValue) {
            let copy = {
                let i = self.saved.binary_search_by_key(&above.var, key_fn).unwrap();
                match self.saved.get(i).unwrap() {
                    (_, SavedVar::Val { .. } | SavedVar::Edge { .. }) => above.clone(),
                    (_, SavedVar::Copy { copy, .. }) => copy.clone(),
                }
            };

            let i = self.saved.binary_search_by_key(&vv.var, key_fn).unwrap();
            self.saved.get_mut(i).unwrap().1 = SavedVar::copy(vv, copy, above.clone());
        }

        pub fn insert(&mut self, (var, saved): (Variable, SavedVar)) {
            let (Ok(i) | Err(i)) = self.saved.binary_search_by_key(&var, key_fn);
            self.saved.insert(i, (var, saved));
        }

        pub(crate) fn take_from(&mut self, rect: &Rect) -> Vec<(Variable, SavedVar)> {
            let (tl, br) = rect.var_points();
            self.saved
                .extract_if(.., |(var, _)| {
                    [tl.x_var(), tl.y_var(), br.x_var(), br.y_var()].contains(var)
                })
                .collect()
        }

        pub fn remove_eq(&mut self, eq: cassowary::Constraint) {
            let var = eq.expr().terms[0].variable;
            if let Ok(i) = self.saved.binary_search_by_key(&var, key_fn)
                && let Some((_, saved)) = self.saved.get_mut(i)
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
        }

        pub fn remove_edge(&mut self, edge: &VarValue) {
            self.edges.retain(|e| !Arc::ptr_eq(&e.width, &edge.value));
        }

        pub fn update_variables(
            &self,
            changes: &[(Variable, f64)],
        ) -> Vec<(&Arc<AtomicBool>, Option<&VarValue>)> {
            let mut has_changeds = Vec::new();
            let mut copies = Vec::new();

            for (var, new) in changes {
                let i = self.saved.binary_search_by_key(var, key_fn).ok();
                match i.and_then(|i| self.saved.get(i)) {
                    Some((_, SavedVar::Val { value, has_changed })) => {
                        let new = new.round() as u32;
                        let old = value.swap(new, Ordering::Release);
                        if old != new {
                            has_changeds.push((has_changed, None));
                        }
                    }
                    Some((_, SavedVar::Edge { width, rhs, value, .. })) => {
                        let new = rhs.load(Ordering::Acquire) - width.value();
                        value.store(new, Ordering::Release);
                    }
                    Some((_, SavedVar::Copy { copy, above, value, has_changed })) => {
                        // Here, I'm passing the above value, so that it (a
                        // parent value) won't be said to have been changed.
                        copies.push((&copy.value, value, has_changed, Some(above)))
                    }
                    // In this case, the variable has been removed.
                    None => {}
                }
            }

            // Copies are updated after, so that the real values are updated
            // first.
            for (copy, value, has_changed, above) in copies {
                let new = copy.load(Ordering::Acquire);
                let old = value.swap(new, Ordering::Release);
                if old != new {
                    has_changeds.push((has_changed, above));
                }
            }

            has_changeds
        }

        pub fn print_edges(&self) {
            static FRAME_FORM: LazyLock<FormId> =
                LazyLock::new(|| form::set_weak("Frame", "Default"));
            let frame_form = form::from_id(*FRAME_FORM);

            let mut stdout = std::io::stdout().lock();

            let edges: Vec<_> = self
                .edges
                .iter()
                .filter_map(|edge| edge.edge_coords())
                .collect();

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
                        SetStyle(frame_form.style),
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
                    Print(super::line::crossing(right, up, left, down, true))
                )
            }
        }
    }

    fn key_fn(entry: &(Variable, SavedVar)) -> Variable {
        entry.0
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
            solver.add_edit_variable(max.x_var(), strong).unwrap();
            solver.suggest_value(max.x_var(), width).unwrap();

            solver.add_edit_variable(max.y_var(), strong).unwrap();
            solver.suggest_value(max.y_var(), height).unwrap();

            SyncSolver {
                solver,
                eqs_to_add: Vec::new(),
                eqs_to_remove: Vec::new(),
            }
        }

        pub fn update(
            &mut self,
            change_max: bool,
            max: &VarPoint,
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

                self.solver.suggest_value(max.x_var(), width).unwrap();
                self.solver.suggest_value(max.y_var(), height).unwrap();
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

#[derive(Debug)]
pub struct Receiver {
    lines: Arc<Mutex<Option<Lines>>>,
    tl: VarPoint,
    br: VarPoint,
}

impl Receiver {
    fn take(&self) -> Option<Lines> {
        self.lines.lock().take()
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
    pub fn lines(&self, shift: u32, cap: u32) -> Lines {
        let area = (self.coords().width() * self.coords().height()) as usize;
        let mut cutoffs = Vec::with_capacity(self.coords().height() as usize);
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
        *self.lines.lock() = Some(lines);
    }

    pub fn coords(&self) -> Coords {
        Coords::new(self.tl.coord(), self.br.coord())
    }
}

pub struct Lines {
    bytes: Vec<u8>,
    cutoffs: Vec<usize>,
    coords: Coords,
    real_cursor: Option<bool>,

    line: Vec<u8>,
    len: u32,
    shift: u32,
    cap: u32,
    positions: Vec<(usize, u32)>,
    align: Alignment,
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
            .field("align", &self.align)
            .finish()
    }
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
        self.align = alignment;
    }

    pub fn show_real_cursor(&mut self) {
        self.real_cursor = Some(true);
    }

    pub fn hide_real_cursor(&mut self) {
        self.real_cursor = Some(false);
    }

    fn on(&self, y: u32) -> Option<(&[u8], u32, u32)> {
        let (tl, br) = (self.coords.tl(), self.coords.br());

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
            let mut dist = align_start + self.len;

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
        if self.coords.width() > end_d {
            style!(self.bytes, default_form.style);
            self.bytes
                .extend_from_slice(&BLANK[..(self.coords.width() - end_d) as usize]);
        }
        self.cutoffs.push(self.bytes.len());

        self.line.clear();
        self.positions.clear();
        self.len = 0;
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
        Ok(())
    }
}

pub enum SavedVar {
    Val {
        value: Arc<AtomicU32>,
        has_changed: Arc<AtomicBool>,
    },
    Edge {
        width: VarValue,
        rhs: Arc<AtomicU32>,
        value: Arc<AtomicU32>,
        has_changed: Arc<AtomicBool>,
    },
    Copy {
        copy: VarValue,
        above: VarValue,
        value: Arc<AtomicU32>,
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

    fn copy(vv: &VarValue, copy: VarValue, above: VarValue) -> Self {
        Self::Copy {
            copy,
            above,
            value: vv.value.clone(),
            has_changed: vv.has_changed.clone(),
        }
    }
}
