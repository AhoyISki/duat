use std::{
    io::Write,
    sync::atomic::{AtomicBool, Ordering},
};

use cassowary::Variable;
use crossterm::{
    cursor::{self, MoveTo, MoveToColumn, MoveToNextLine},
    terminal,
};
use duat_core::{cfg::PrintCfg, form, ui::Axis};

use self::{lines::LinesBuilder, sync_solver::SyncSolver, variables::Variables};
use crate::{AreaId, Coords, Equality, Mutex, area::Coord, layout::Rect, queue};

mod edges;
mod lines;
mod sync_solver;
mod variables;

pub use self::{
    edges::{Brush, Frame},
    lines::Gaps,
};

pub struct Printer {
    sync_solver: Mutex<SyncSolver>,
    vars: Mutex<Variables>,
    old_lines: Mutex<Vec<(AreaId, Lines)>>,
    new_lines: Mutex<NewLinesList>,
    floating_lines: Mutex<Vec<(AreaId, Lines)>>,
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
            new_lines: Mutex::new(NewLinesList::default()),
            floating_lines: Mutex::new(Vec::new()),
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
    pub fn new_floating_center(
        &self,
        deps: [Variable; 2],
        len: Option<f32>,
        axis: Axis,
        prefers_before: bool,
    ) -> [Variable; 2] {
        self.sync_solver.lock().unwrap().new_floating_center(
            &mut self.vars.lock().unwrap(),
            deps,
            len,
            axis,
            prefers_before,
        )
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

        [tl.x(), tl.y(), br.x(), br.y()]
    }

    /// Inserts the [`Variables`] taken from a [`Rect`]
    pub fn insert_rect_vars(&self, new_vars: [Variable; 4]) {
        let mut vars = self.vars.lock().unwrap();
        for var in new_vars {
            vars.insert(var);
        }
    }

    ////////// Updating functions

    /// Updates the value of all [`VarPoint`]s that have changed,
    /// returning true if any of them have.
    pub fn update(&self, change_max: bool, assign_floating: bool) {
        let changes = {
            let mut ss = self.sync_solver.lock().unwrap();
            ss.update(change_max, self.max, assign_floating).unwrap()
        };

        let mut vars = self.vars.lock().unwrap();
        vars.update_variables(changes);
        self.has_to_print_edges.store(true, Ordering::Relaxed);
    }

    pub fn replace(
        &self,
        old_eqs: impl IntoIterator<Item = Equality>,
        new_eqs: impl IntoIterator<Item = Equality>,
    ) {
        let mut ss = self.sync_solver.lock().unwrap();
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
            let mut ss = self.sync_solver.lock().unwrap();
            ss.remove_eqs(old_eqs);
            ss.add_eqs(new_eqs);
            ss.update(change_max, self.max, false).unwrap()
        };

        let mut vars = self.vars.lock().unwrap();
        vars.update_variables(changes);
        self.has_to_print_edges.store(true, Ordering::Relaxed);
        // Drop here, so self.updates and self.vars update "at the same time"
        drop(vars)
    }

    pub fn print(&self) {
        static CURSOR_IS_REAL: AtomicBool = AtomicBool::new(false);

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
        if new_lines.list.is_empty() {
            return;
        }
        let mut old_lines = self.old_lines.lock().unwrap();

        let mut stdout = stdout.unwrap_or_else(stdout::get);
        let max = self.max_value();

        queue!(stdout, terminal::BeginSynchronizedUpdate);
        queue!(stdout, cursor::Hide, MoveTo(0, 0));

        for y in 0..max.y {
            let mut x = 0;

            let iter = new_lines
                .list
                .iter()
                .filter_map(|(id, lines)| {
                    lines.on(y).filter(|line_info| {
                        if let Some(old_y) = y.checked_add_signed(new_lines.scroll)
                            && let Some(old_line_info) =
                                old_lines.iter().find_map(|(old_id, lines)| {
                                    lines.on(old_y).filter(|_| old_id == id)
                                })
                        {
                            old_line_info != *line_info
                        } else {
                            true
                        }
                    })
                })
                .chain(
                    (new_lines.scroll != 0)
                        .then(|| old_lines.iter().filter_map(|(_, lines)| lines.on(y)))
                        .into_iter()
                        .flatten(),
                );

            for (bytes, start, end) in iter {
                if x != start {
                    queue!(stdout, MoveToColumn(start as u16));
                }

                stdout.write_all(bytes).unwrap();

                x = end;
            }

            queue!(stdout, MoveToNextLine(1));
        }

        let cursor_was_real = if let Some(was_real) = new_lines
            .list
            .iter()
            .filter_map(|(_, lines)| lines.real_cursor)
            .reduce(|prev, was_real| prev || was_real)
        {
            CURSOR_IS_REAL.store(was_real, Ordering::Relaxed);
            was_real
        } else {
            CURSOR_IS_REAL.load(Ordering::Relaxed)
        };

        let list = self.floating_lines.lock().unwrap();
        if !list.is_empty() {
            for (_, lines) in list.iter() {
                for y in lines.coords.tl.y..lines.coords.br.y {
                    queue!(stdout, MoveTo(lines.coords.tl.x as u16, y as u16));
                    let (bytes, ..) = lines.on(y).unwrap();
                    stdout.write_all(bytes).unwrap();
                }
            }
        }

        if cursor_was_real {
            queue!(stdout, cursor::RestorePosition, cursor::Show);
        }

        queue!(stdout, terminal::EndSynchronizedUpdate);
        stdout.flush().unwrap();

        for (id, lines) in new_lines.list {
            old_lines.retain(|(i, l)| *i != id && !l.coords.intersects(lines.coords));

            let Err(i) = old_lines.binary_search_by_key(&lines.coords, |(_, lines)| lines.coords)
            else {
                unreachable!("Colliding Lines should have been removed already");
            };

            old_lines.insert(i, (id, lines));
        }
    }

    ////////// Lines functions

    /// Returns a new [`Lines`], a struct used to print to the screen
    pub fn lines(&self, coords: Coords, max: Coord, shift: u32, cfg: PrintCfg) -> LinesBuilder {
        LinesBuilder::new(coords, max, shift, cfg)
    }

    /// Sends the finished [`Lines`], off to be printed
    pub fn send_lines(&self, id: AreaId, builder: LinesBuilder) {
        let mut new_lines = self.new_lines.lock().unwrap();
        // This area may have been sent without being printed, in that case,
        // we just remove it.
        // Also, areas that intersect with this one came from a previous
        // organization of areas, so they should also be removed.
        new_lines
            .list
            .retain(|(i, l)| *i != id && !l.coords.intersects(builder.coords()));

        let Err(i) = new_lines
            .list
            .binary_search_by_key(&builder.coords(), |(_, lines)| lines.coords)
        else {
            unreachable!("Colliding Lines should have been removed already");
        };

        new_lines.list.insert(i, (id, builder.build()));
    }

    /// Sends the finished [`Lines`] of a floating `Widget` to be
    /// printed
    pub fn send_floating_lines(&self, id: AreaId, builder: LinesBuilder) {
        let mut list = self.floating_lines.lock().unwrap();

        // This is done in order to preserve the order in which the floating
        // Widgets were sent.
        if let Some((_, old_lines)) = list.iter_mut().find(|(other, _)| *other == id) {
            *old_lines = builder.build();
        } else {
            list.push((id, builder.build()));
        }
    }

    /// Decides how much to scroll before printing
    pub fn set_lines_scroll(&self, scroll: i32) {
        self.new_lines.lock().unwrap().scroll = scroll;
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

#[derive(PartialEq, Eq)]
struct Lines {
    list: Vec<Vec<u8>>,
    coords: Coords,
    real_cursor: Option<bool>,
}

impl Lines {
    pub fn on(&self, y: u32) -> Option<(&[u8], u32, u32)> {
        let (tl, br) = (self.coords.tl, self.coords.br);

        self.list
            .get((y - tl.y) as usize)
            .map(|bytes| (bytes.as_slice(), tl.x, br.x))
    }
}

#[derive(Default)]
struct NewLinesList {
    scroll: i32,
    list: Vec<(AreaId, Lines)>,
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

        static STDOUT: LazyLock<Mutex<BufWriter<File>>> =
            LazyLock::new(|| Mutex::new(BufWriter::new(get_stdout())));

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
