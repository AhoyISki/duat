use std::{
    io::Write,
    sync::atomic::{AtomicBool, Ordering},
};

use cassowary::{Expression, Variable};
use crossterm::{
    cursor::{self, MoveTo, MoveToColumn},
    style::ResetColor,
};
use duat_core::{form, text::SpawnId, ui::Axis};

use self::{sync_solver::SyncSolver, variables::Variables};
use crate::{AreaId, Coords, Equality, Mutex, area::Coord, layout::Rect, queue};

mod edges;
mod lines;
mod sync_solver;
mod variables;

pub use self::{
    edges::{Brush, Frame},
    lines::{Gaps, LinesBuilder},
};

pub struct Printer {
    sync_solver: Mutex<SyncSolver>,
    vars: Mutex<Variables>,
    old_lines: Mutex<Vec<Lines>>,
    new_lines: Mutex<Vec<Lines>>,
    spawned_lines: Mutex<Vec<(AreaId, Lines)>>,
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
            old_lines: Mutex::new(Vec::new()),
            new_lines: Mutex::new(Vec::new()),
            spawned_lines: Mutex::new(Vec::new()),
            max,
            has_to_print_edges: AtomicBool::new(false),
        }
    }

    ////////// Area setup functions

    /// Adds a new [`VarPoint`] to the list of [`Variable`]s and
    /// returns it
    pub fn new_point(&self) -> VarPoint {
        self.vars.lock().unwrap().new_point()
    }

    /// Returns a new dynamically updated [`Variable`], which centers
    /// a [`Rect`] as well as another which represents the length of
    /// said [`Rect`]
    pub fn new_widget_spawn(
        &self,
        id: SpawnId,
        deps: [VarPoint; 2],
        len: Option<f32>,
        axis: Axis,
        prefers_before: bool,
    ) -> [Variable; 2] {
        self.sync_solver.lock().unwrap().new_widget_spawn(
            id,
            &mut self.vars.lock().unwrap(),
            deps,
            len,
            axis,
            prefers_before,
        )
    }

    /// Returns a new dynamically updated [`Variable`], which centers
    /// a [`Rect`] and another which represents the length of
    /// said [`Rect`].
    ///
    /// Also returns a [`VarPoint`], which is meant to represent to
    /// top left corner of a cell in the terminal.
    pub fn new_text_spawn(
        &self,
        id: SpawnId,
        len: Option<f32>,
        axis: Axis,
        prefers_before: bool,
    ) -> ([Variable; 2], VarPoint) {
        self.sync_solver.lock().unwrap().new_text_spawn(
            id,
            &mut self.vars.lock().unwrap(),
            len,
            axis,
            prefers_before,
        )
    }

    /// Returns the spawned info associated with a [`SpawnId`]
    ///
    /// This info consists of the following:
    ///
    /// - The `center` and `len` variables
    /// - The top left corner of the spawn target
    /// - The bottom right corner of the spawn target
    pub fn get_spawn_info(
        &self,
        id: SpawnId,
    ) -> Option<([Variable; 2], [Expression; 2], [Expression; 2])> {
        self.sync_solver.lock().unwrap().get_spawn_info(id)
    }

    /// Sets the new desired length for a [`SpawnId`]
    pub fn set_spawn_len(&self, id: SpawnId, len: Option<f64>) {
        self.sync_solver.lock().unwrap().set_spawn_len(id, len);
    }

    /// Creates a new edge from the two [`VarPoint`]s
    ///
    /// This function will return the [`Variable`] representing the
    /// `width` of that edge. It can only have a value of `1` or `0`.
    pub fn set_edge(&self, lhs: VarPoint, rhs: VarPoint, axis: Axis, fr: Frame) -> Variable {
        self.vars.lock().unwrap().set_edge([lhs, rhs], axis, fr)
    }

    /// Adds [`Equality`]s to the solver
    pub fn add_eqs(&self, eqs: impl IntoIterator<Item = Equality>) {
        self.sync_solver.lock().unwrap().add_eqs(eqs);
    }

    ////////// Layout modification functions

    /// Removes [`Equality`]s from the solver
    pub fn remove_eqs(&self, eqs: impl IntoIterator<Item = Equality>) {
        // If there is no SavedVar, then the first term is a frame.
        self.sync_solver.lock().unwrap().remove_eqs(eqs);
    }

    /// Removes an edge from the list of edges
    pub fn remove_edge(&self, edge: Variable) {
        self.vars.lock().unwrap().remove_edge(edge);
    }

    /// Takes the [`Variables`] of a [`Rect`]
    ///
    /// This is done when swapping two [`Rect`]s from different
    /// windows.
    pub fn remove_rect(&self, rect: &mut Rect) -> [Variable; 4] {
        self.sync_solver
            .lock()
            .unwrap()
            .remove_eqs(rect.drain_eqs());

        let mut vars = self.vars.lock().unwrap();
        let [tl, br] = rect.var_points();
        if let Some(edge) = rect.edge() {
            vars.remove_edge(edge);
        }
        for var in [tl.x(), tl.y(), br.x(), br.y()] {
            vars.remove(var);
        }
        drop(vars);

        self.spawned_lines
            .lock()
            .unwrap()
            .retain(|(id, _)| *id != rect.id());

        [tl.x(), tl.y(), br.x(), br.y()]
    }

    /// Removes the information regarding a [`SpawnId`]
    ///
    /// This will remove, more specifically, the `center` and `len`
    /// variables, as well as future calculations for the center of
    /// the spawned widget.
    pub fn remove_spawn_info(&self, id: SpawnId) {
        let Some(edit_vars) = self.sync_solver.lock().unwrap().remove_spawn_info(id) else {
            return;
        };

        let mut vars = self.vars.lock().unwrap();
        match edit_vars {
            sync_solver::ReturnedEditVars::WidgetSpawned([center, len]) => {
                vars.remove(center);
                vars.remove(len);
            }
            sync_solver::ReturnedEditVars::TextSpawned([center, len, tl_x, tl_y]) => {
                vars.remove(center);
                vars.remove(len);
                vars.remove(tl_x);
                vars.remove(tl_y);
            }
        }
        drop(vars);
    }

    /// Inserts the [`Variables`] taken from a [`Rect`]
    pub fn insert_rect_vars(&self, new_vars: [Variable; 4]) {
        let mut vars = self.vars.lock().unwrap();
        for var in new_vars {
            vars.insert(var);
        }
    }

    ////////// Updating functions

    /// Updates the value of all [`VarPoint`]s that have changed
    pub fn update(&self, change_max: bool, assign_floating: bool) {
        let changes = {
            let mut ss = self.sync_solver.lock().unwrap();
            ss.update(change_max, self.max, assign_floating).unwrap()
        };

        if change_max {
            self.old_lines.lock().unwrap().clear();
        }

        let mut vars = self.vars.lock().unwrap();
        vars.update_variables(changes);
        self.has_to_print_edges.store(true, Ordering::Relaxed);
    }

    /// Clears a spawned Widget from screen, not actually deleting it
    pub fn clear_spawn(&self, target: AreaId) {
        self.spawned_lines
            .lock()
            .unwrap()
            .retain(|(id, _)| *id != target);
    }

    /// Replace a set of [`Equality`]s with another
    pub fn replace(
        &self,
        old_eqs: impl IntoIterator<Item = Equality>,
        new_eqs: impl IntoIterator<Item = Equality>,
    ) {
        let mut ss = self.sync_solver.lock().unwrap();
        ss.remove_eqs(old_eqs);
        ss.add_eqs(new_eqs);
    }

    /// Moves a [`SpawnId`] to another [`Coord`]
    pub fn move_spawn_to(&self, id: SpawnId, coord: Coord, char_width: u32) {
        self.sync_solver
            .lock()
            .unwrap()
            .move_spawn_to(id, coord, char_width);
    }

    /// Main printing function, responsible for keeping things
    /// consistent
    pub fn print(&self) {
        static CURSOR_IS_REAL: AtomicBool = AtomicBool::new(false);
        const SPACES: &[u8] = &[b' '; 3000];

        let stdout = if self.has_to_print_edges.swap(false, Ordering::Relaxed) {
            let mut stdout = stdout::get();
            let edge_form = form::from_id(form::id_of!("terminal.frame"));
            self.vars
                .lock()
                .unwrap()
                .print_edges(&mut stdout, edge_form);
            Some(stdout)
        } else {
            None
        };

        let new_lines = std::mem::take(&mut *self.new_lines.lock().unwrap());
        let spawned_lines = self.spawned_lines.lock().unwrap();

        let mut old_lines = self.old_lines.lock().unwrap();

        let mut stdout = stdout.unwrap_or_else(stdout::get);
        let max = self.max_value();

        queue!(stdout, cursor::Hide, ResetColor);
        write!(stdout, "\x1b[?2026h").unwrap();

        for y in 0..max.y {
            write!(stdout, "\x1b[{}H", y + 1).unwrap();

            let mut x = 0;

            let mut old_iter = old_lines.iter().filter_map(|lines| lines.on(y));
            let mut new_iter = new_lines.iter().filter_map(|lines| lines.on(y)).peekable();

            while let Some((line, [start, end])) = new_iter
                .next_if(|(_, [start, _])| spawned_lines.is_empty() || *start == x)
                .or_else(|| {
                    old_iter.find(|(_, [start, _])| !spawned_lines.is_empty() && *start >= x)
                })
            {
                if x != start {
                    queue!(stdout, MoveToColumn(start as u16));
                }

                stdout.write_all(line.bytes).unwrap();

                if end == max.x {
                    stdout.write_all(b"\x1b[K").unwrap();
                } else {
                    stdout.write_all(&SPACES[..line.end_spaces]).unwrap();
                }

                x = end;
            }
        }

        let cursor_was_real = if let Some(was_real) = new_lines
            .iter()
            .filter_map(|lines| lines.real_cursor)
            .reduce(|prev, was_real| prev || was_real)
        {
            CURSOR_IS_REAL.store(was_real, Ordering::Relaxed);
            was_real
        } else {
            CURSOR_IS_REAL.load(Ordering::Relaxed)
        };

        for (_, lines) in spawned_lines.iter() {
            for y in lines.coords.tl.y..lines.coords.br.y {
                queue!(stdout, MoveTo(lines.coords.tl.x as u16, y as u16));
                let (info, ..) = lines.on(y).unwrap();
                stdout.write_all(info.bytes).unwrap();
                stdout.write_all(&SPACES[..info.end_spaces]).unwrap();
            }
        }

        if cursor_was_real {
            queue!(stdout, cursor::RestorePosition, cursor::Show);
        }

        write!(stdout, "\x1b[?2026l").unwrap();
        stdout.flush().unwrap();

        for info in new_lines {
            old_lines.retain(|l| !l.coords.intersects(info.coords));

            let Err(i) = old_lines.binary_search_by_key(&info.coords, |lines| lines.coords) else {
                unreachable!("Colliding Lines should have been removed already");
            };

            old_lines.insert(i, info);
        }
    }

    ////////// Lines functions

    /// Sends the finished [`Lines`], off to be printed
    pub fn send_lines(&self, builder: LinesBuilder) {
        let mut new_lines = self.new_lines.lock().unwrap();
        // Areas that intersect with this one came from a previous
        // organization of areas or are a previous version of itself, so they
        // should also be removed.
        new_lines.retain(|l| !l.coords.intersects(builder.coords()));

        let Err(i) = new_lines.binary_search_by_key(&builder.coords(), |lines| lines.coords) else {
            unreachable!("Colliding Lines should have been removed already");
        };

        new_lines.insert(i, builder.build());
    }

    /// Sends the finished [`Lines`] of a floating `Widget` to be
    /// printed
    pub fn send_spawned_lines(&self, id: AreaId, builder: LinesBuilder) {
        let mut spawned_lines = self.spawned_lines.lock().unwrap();

        // This is done in order to preserve the order in which the floating
        // Widgets were sent.
        if let Some((_, old_lines)) = spawned_lines.iter_mut().find(|(other, _)| *other == id) {
            *old_lines = builder.build();
        } else {
            spawned_lines.push((id, builder.build()));
        }
    }

    ////////// Querying functions

    /// The maximum [`VarPoint`], i.e. the bottom right of the screen
    pub fn max(&self) -> &VarPoint {
        &self.max
    }

    /// The current value of the [`max`] [`VarPoint`]
    ///
    /// [`max`]: Self::max
    pub fn max_value(&self) -> Coord {
        let mut vars = self.vars.lock().unwrap();
        let (max, _) = vars.coord(self.max, false);
        max
    }

    /// Gets [`Coords`] from two [`VarPoint`]s
    pub fn coords(&self, var_points: [VarPoint; 2], is_printing: bool) -> (Coords, bool) {
        let mut vars = self.vars.lock().unwrap();
        let (tl, tl_has_changed) = vars.coord(var_points[0], is_printing);
        let (br, br_has_changed) = vars.coord(var_points[1], is_printing);
        (Coords::new(tl, br), tl_has_changed || br_has_changed)
    }

    /// Wether a [`Variable`] has changed
    pub fn coords_have_changed(&self, [tl, br]: [VarPoint; 2]) -> bool {
        let vars = self.vars.lock().unwrap();
        [tl.x(), tl.y(), br.x(), br.y()]
            .into_iter()
            .any(|var| vars.has_changed(var))
    }
}

unsafe impl Send for Printer {}
unsafe impl Sync for Printer {}

/// A list of lines to print, belonging to some `Widget`
struct Lines {
    bytes: Vec<u8>,
    line_infos: Vec<InnerLineInfo>,
    coords: Coords,
    real_cursor: Option<bool>,
}

impl Lines {
    /// A line on a given `y` position
    ///
    /// Returns [`None`] if these [`Lines`] don't intersect with the
    /// given `y`.
    pub fn on(&self, y: u32) -> Option<(LineInfo<'_>, [u32; 2])> {
        let (tl, br) = (self.coords.tl, self.coords.br);
        let y = y.checked_sub(tl.y)? as usize;

        self.line_infos.get(y).map(|info| {
            let end = self
                .line_infos
                .get(y + 1)
                .map(|i| i.offset)
                .unwrap_or(self.bytes.len());
            let info = LineInfo {
                bytes: &self.bytes[info.offset..end],
                end_spaces: info.end_spaces,
            };
            (info, [tl.x, br.x])
        })
    }
}

struct InnerLineInfo {
    offset: usize,
    end_spaces: usize,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct LineInfo<'a> {
    bytes: &'a [u8],
    end_spaces: usize,
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

    pub fn on(&self, axis: Axis) -> Variable {
        match axis {
            Axis::Horizontal => self.x,
            Axis::Vertical => self.y,
        }
    }
}

mod stdout {
    use std::{
        fs::File,
        io::BufWriter,
        sync::{LazyLock, Mutex, MutexGuard},
    };

    pub type Stdout = MutexGuard<'static, BufWriter<File>>;

    /// Gets a BufWriter wrapper around the stdout
    pub fn get() -> Stdout {
        #[cfg(not(windows))]
        use unix::get_stdout;
        #[cfg(windows)]
        use windows::get_stdout;

        const CAP: usize = usize::pow(2, 14);
        static STDOUT: LazyLock<Mutex<BufWriter<File>>> =
            LazyLock::new(|| Mutex::new(BufWriter::with_capacity(CAP, get_stdout())));

        STDOUT.lock().unwrap()
    }

    #[cfg(not(windows))]
    mod unix {
        use std::{
            fs::File,
            io::stdout,
            os::fd::{AsRawFd, FromRawFd},
        };

        /// The stdout [`File`] on non Windows systems
        pub fn get_stdout() -> File {
            unsafe { File::from_raw_fd(stdout().as_raw_fd()) }
        }
    }

    #[cfg(windows)]
    mod windows {
        use std::{
            fs::File,
            io::stdout,
            os::windows::io::{AsRawHandle, FromRawHandle},
        };

        /// The stdout [`File`] on Windows systems
        pub fn get_stdout() -> File {
            unsafe { File::from_raw_handle(stdout().as_raw_handle()) }
        }
    }
}
