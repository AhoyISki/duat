use std::{
    fmt::Alignment,
    io::Write,
    sync::atomic::{AtomicBool, Ordering},
};

use cassowary::Variable;
use crossterm::{
    cursor::{self, MoveTo, MoveToColumn, MoveToNextLine},
    style::{Attribute, ContentStyle},
    terminal,
};
use duat_core::{
    cfg::PrintCfg,
    form::{self, Painter},
    ui::Axis,
};
use sync_solver::SyncSolver;
use variables::Variables;

use crate::{
    AreaId, CStyle, Coords, Equality, Mutex, area::Coord, layout::Rect, print_style, queue,
};

mod edges;
mod variables;
mod sync_solver;

pub use self::edges::{Brush, Frame};

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

        let list = std::mem::take(&mut *self.old_lines.lock().unwrap());
        if list.is_empty() {
            return;
        }

        let mut stdout = stdout.unwrap_or_else(stdout::get);
        let max = list.last().unwrap().1.coords.br;

        queue!(stdout, terminal::BeginSynchronizedUpdate);
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

        let list = self.floating_lines.lock().unwrap();
        if !list.is_empty() {
            for (_, lines) in list.iter() {
                let coords = lines.coords();
                for y in coords.tl.y..coords.br.y {
                    queue!(stdout, MoveTo(coords.tl.x as u16, y as u16));
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
    }

    /// Returns a new [`Lines`], a struct used to print to the screen
    pub fn lines(&self, coords: Coords, max: Coord, shift: u32, cfg: PrintCfg) -> LinesBuilder {
        let cap = cfg.wrap_width(coords.width());
        let mut cutoffs = Vec::with_capacity(coords.height() as usize);
        cutoffs.push(0);

        LinesBuilder {
            Lines {
            list: (0..coords.height())
                .map(|_| Vec::with_capacity(coords.width() as usize))
                .collect(),
            coords,
            real_cursor: None,
            }

			cur_line: 0,
            line: Vec::with_capacity(coords.width() as usize * 4),
            len: 0,
            positions: Vec::new(),
            gaps: Gaps::OnRight,
            default_gaps: Gaps::OnRight,
            rightmost: coords.br.x == max.x,

            shift,
            cap,
        }
    }

    /// Sends the finished [`Lines`], off to be printed
    pub fn send(&self, id: AreaId, lines: LinesBuilder) {
        let mut list = self.old_lines.lock().unwrap();
        // This area may have been sent without being printed, in that case,
        // we just remove it.
        // Also, areas that intersect with this one came from a previous
        // organization of areas, so they should also be removed.
        list.retain(|(i, l)| *i != id && !l.coords().intersects(lines.coords()));

        let Err(i) = list.binary_search_by_key(&lines.coords(), |(_, lines)| lines.coords()) else {
            unreachable!("Colliding Lines should have been removed already");
        };

        list.insert(i, (id, lines));
    }

    /// Sends the finished [`Lines`] of a floating `Widget` to be
    /// printed
    pub fn send_floating(&self, id: AreaId, lines: LinesBuilder) {
        let mut list = self.floating_lines.lock().unwrap();

        // This is done in order to preserve the order in which the floating
        // Widgets were sent.
        if let Some((_, old_lines)) = list.iter_mut().find(|(other, _)| *other == id) {
            *old_lines = lines;
        } else {
            list.push((id, lines));
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

struct Lines {
    list: Vec<Vec<u8>>,
    coords: Coords,
    real_cursor: Option<bool>,
}

impl Lines {
    fn on(&self, y: u32) -> Option<(&[u8], u32, u32)> {
        let (tl, br) = (self.coords.tl, self.coords.br);

        if (tl.y..br.y).contains(&y) {
            let y = y - tl.y;
            let start = self.cutoffs[y as usize];

            let bytes = match self.cutoffs.get(y as usize + 1) {
                Some(end) => &self.list[start..*end],
                None => &self.list[start..],
            };

            Some((bytes, tl.x, br.x))
        } else {
            None
        }
    }
}

pub struct LinesBuilder {
    lines: Lines,
    
    // Temporary things
    cur_line: usize,
    line: Vec<u8>,
    len: u32,
    positions: Vec<(usize, u32)>,
    gaps: Gaps,
    default_gaps: Gaps,

    // Outside information
    shift: u32,
    cap: u32,
    rightmost: bool,
}

impl LinesBuilder {
    pub fn push_char(&mut self, char: char, len: u32) {
        self.len += len;
        let mut bytes = [0; 4];
        char.encode_utf8(&mut bytes);
        if self.coords.width() < self.cap {
            self.positions.push((self.line.len(), len));
        }
        self.line.extend(&bytes[..char.len_utf8()]);
    }

    pub fn realign(&mut self, alignment: Alignment) {
        self.default_gaps = match alignment {
            Alignment::Left => Gaps::OnRight,
            Alignment::Right => Gaps::OnLeft,
            Alignment::Center => Gaps::OnSides,
        };
        if let Gaps::OnRight | Gaps::OnLeft | Gaps::OnSides = self.gaps {
            self.gaps = self.default_gaps.clone();
        }
    }

    pub fn add_spacer(&mut self) {
        self.gaps.add_spacer(self.line.len());
    }

    pub fn show_real_cursor(&mut self) {
        self.real_cursor = Some(true);
    }

    pub fn hide_real_cursor(&mut self) {
        self.real_cursor = Some(false);
    }

    pub fn end_line(
        &mut self,
        ansi_codes: &mut micromap::Map<CStyle, String, 16>,
        painter: &Painter,
    ) {
        fn starting_spaces(bytes: &mut Vec<u8>, len: usize) {
            if len > 0 {
                bytes.extend_from_slice(&BLANK[..len]);
            }
        }

        fn ending_spaces(
            bytes: &mut Vec<u8>,
            len: usize,
            style: ContentStyle,
            ansi_codes: &mut micromap::Map<CStyle, String, 16>,
            is_rightmost: bool,
        ) {
            print_style(bytes, style, ansi_codes);
            if is_rightmost {
                bytes.extend_from_slice(b"\x1b[0K");
            } else {
                bytes.extend_from_slice(&BLANK[..len])
            }
        }

        const BLANK: [u8; 1000] = [b' '; 1000];
        let default = {
            let mut default_form = painter.get_default();
            default_form.style.attributes.set(Attribute::Reset);
            default_form.style
        };

        let spaces = self.gaps.get_spaces(self.cap - self.len);
        // Shortcut
        if self.coords.width() >= self.cap {
            print_style(&mut self.list, default, ansi_codes);

            let start_d = match &self.gaps {
                Gaps::OnRight => 0,
                Gaps::OnLeft => self.cap - self.len,
                Gaps::OnSides => (self.cap - self.len) / 2,
                Gaps::Spacers(bytes) => {
                    let spacers = bytes.iter().zip(spaces);
                    let mut start = 0;

                    for (&end, len) in spacers {
                        self.list.extend_from_slice(&self.line[start..end]);
                        self.list.extend(&BLANK[..len as usize]);
                        start = end
                    }
                    self.list.extend(&self.line[start..self.line.len()]);

                    self.go_to_next_line();
                    return;
                }
            };

            starting_spaces(&mut self.list, start_d as usize);
            self.list.extend_from_slice(&self.line);
            let end_d = start_d + self.len;
            if self.coords.width() > end_d {
                let len = (self.coords.width() - end_d) as usize;
                ending_spaces(&mut self.list, len, default, ansi_codes, self.rightmost);
            }
            self.go_to_next_line();
            return;
        }

        let (start_i, start_d) = {
            let mut dist = match &self.gaps {
                Gaps::OnRight | Gaps::Spacers(_) => 0,
                Gaps::OnLeft => self.cap - self.len,
                Gaps::OnSides => (self.cap - self.len) / 2,
            };

            // Using a different loop when there are no spacers in order to
            // improve efficiency.
            let found_start = if let Gaps::Spacers(bytes) = &self.gaps {
                let mut sb = spaces.iter().zip(bytes).peekable();
                self.positions.iter().find(|(b, len)| {
                    dist += len;
                    if let Some((space, _)) = sb.next_if(|(_, lhs)| *lhs <= b) {
                        dist += space;
                    }
                    dist > self.shift
                })
            } else {
                self.positions.iter().find(|(_, len)| {
                    dist += len;
                    dist > self.shift
                })
            };

            // Situation where the line is empty, from having no characters or
            // from being shifted out of sight.
            let Some(&(start, len)) = found_start else {
                let len = self.coords.width() as usize;
                ending_spaces(&mut self.list, len, default, ansi_codes, self.rightmost);

                self.go_to_next_line();
                return;
            };

            // If the character is cut by the start, don't print it.
            if dist - len < self.shift && len <= 2 {
                let str = unsafe { std::str::from_utf8_unchecked(&self.line[start..]) };
                let char = str.chars().next().unwrap();
                (start + char.len_utf8(), dist - self.shift)
            } else {
                (start, dist - len - self.shift)
            }
        };

        let (end_i, end_d) = {
            let mut dist = match &self.gaps {
                Gaps::OnRight => self.len,
                Gaps::OnLeft | Gaps::Spacers(_) => self.cap,
                Gaps::OnSides => self.len + (self.cap - self.len) / 2,
            };
            let found_end = if let Gaps::Spacers(bytes) = &self.gaps {
                let mut sb = spaces.iter().zip(bytes).rev().peekable();
                self.positions.iter().rev().find(|(b, len)| {
                    dist -= len;
                    if let Some((space, _)) = sb.next_if(|(_, lhs)| *lhs > b) {
                        dist -= space;
                    }
                    dist < self.shift + self.coords.width()
                })
            } else {
                self.positions.iter().rev().find(|(_, len)| {
                    dist -= len;
                    dist < self.shift + self.coords.width()
                })
            };

            // Due to spacers in the middle of the line, end_i might actually be
            // smaller than start_i.
            let Some(&(end, len)) = found_end.filter(|(end_i, _)| *end_i >= start_i) else {
                print_style(&mut self.list, default, ansi_codes);
                self.list
                    .extend_from_slice(&BLANK[..self.coords.width() as usize]);

                self.go_to_next_line();
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

        print_style(&mut self.list, default, ansi_codes);
        starting_spaces(&mut self.list, start_d as usize);

        self.add_ansi(start_i);

        if let Gaps::Spacers(bytes) = &mut self.gaps {
            let spacers = bytes
                .iter()
                .zip(spaces)
                .skip_while(|(b, _)| **b <= start_i)
                .take_while(|(b, _)| **b < end_i);

            let mut start = start_i;

            for (&end, len) in spacers {
                self.list.extend_from_slice(&self.line[start..end]);
                self.list.extend_from_slice(&BLANK[..len as usize]);
                start = end;
            }

            self.list.extend_from_slice(&self.line[start..end_i]);
        } else {
            self.list.extend_from_slice(&self.line[start_i..end_i]);
        }

        if self.coords.width() > end_d {
            let len = (self.coords.width() - end_d) as usize;
            ending_spaces(&mut self.list, len, default, ansi_codes, self.rightmost);
        }

        self.go_to_next_line();
    }

    pub fn coords(&self) -> Coords {
        self.coords
    }

    pub fn cap(&self) -> u32 {
        self.cap
    }

    fn go_to_next_line(&mut self) {
        self.cutoffs.push(self.list.len());
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
                self.list.push(0x1b)
            } else if b == b'm' && adding_ansi {
                adding_ansi = false;
                self.list.push(b'm')
            } else if adding_ansi {
                self.list.push(b)
            }
        }
    }
}

#[derive(Default)]
struct NewLinesList {
    scroll: Option<usize>,
    list: Vec<(AreaId, Lines)>,
}

impl Write for LinesBuilder {
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
pub enum Gaps {
    OnRight,
    OnLeft,
    OnSides,
    Spacers(Vec<usize>),
}

impl Gaps {
    /// Adds a [`Spacer`] to this [`Gaps`]
    ///
    /// [`Spacer`]: duat_core::text::Tag::Spacer
    pub fn add_spacer(&mut self, byte: usize) {
        match self {
            Gaps::OnRight | Gaps::OnLeft | Gaps::OnSides => *self = Gaps::Spacers(vec![byte]),
            Gaps::Spacers(items) => items.push(byte),
        };
    }

    pub fn get_spaces(&self, space: u32) -> Vec<u32> {
        let Self::Spacers(bytes) = self else {
            return Vec::new();
        };
        let mut enough_space = space;
        while !enough_space.is_multiple_of(bytes.len() as u32) {
            enough_space += 1;
        }
        let diff = enough_space - space;
        (0..bytes.len() as u32)
            .map(|i| (enough_space / bytes.len() as u32) - (i < diff) as u32)
            .collect()
    }
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
