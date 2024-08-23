#![feature(
    iter_collect_into,
    let_chains,
    control_flow_enum,
    if_let_guard,
    extract_if,
    is_none_or,
    decl_macro,
    map_many_mut
)]

use std::{
    fmt::Debug,
    io,
    sync::{atomic::Ordering, OnceLock},
    time::Duration,
};

pub use area::{Area, Coords};
use crossterm::{
    cursor, event, execute,
    terminal::{self, ClearType},
};
use duat_core::{
    data::{Context, RwData},
    text::err,
    ui, DuatError,
};

use self::{layout::Layout, print::Printer};
pub use self::{
    print::{Brush, Frame},
    rules::{VertRule, VertRuleCfg},
};

mod area;
mod layout;
mod print;
mod rules;

static FUNCTIONS: OnceLock<StaticFns> = OnceLock::new();

pub struct Ui {
    windows: Vec<Area>,
    printer: RwData<Printer>,
    fr: Frame,
}

impl ui::Ui for Ui {
    type Area = Area;
    type ConstraintChangeErr = ConstraintErr;
    type StaticFns = StaticFns;

    fn new(statics: Self::StaticFns) -> Self {
        FUNCTIONS.get_or_init(|| statics);

        std::panic::set_hook(Box::new(|info| {
            let trace = std::backtrace::Backtrace::capture();
            execute!(
                io::stdout(),
                terminal::Clear(ClearType::All),
                terminal::LeaveAlternateScreen,
                terminal::EnableLineWrap,
                cursor::Show
            )
            .unwrap();
            terminal::disable_raw_mode().unwrap();
            if let std::backtrace::BacktraceStatus::Captured = trace.status() {
                println!("{trace}");
            }
            println!("{info}")
        }));

        Ui {
            windows: Vec::new(),
            printer: RwData::new(Printer::new()),
            fr: Frame::default(),
        }
    }

    fn start(&mut self, sender: ui::Sender, context: Context<Self>) {
        let functions = FUNCTIONS.get().unwrap();
        let printer = self.printer.clone();
        duat_core::thread::spawn(move || {
            loop {
                if let Ok(true) = (functions.poll)() {
                    let res = match (functions.read)().unwrap() {
                        event::Event::Key(key) => sender.send_key(key),
                        event::Event::Resize(..) => {
                            printer.write().update(true);
                            sender.send_resize()
                        }
                        event::Event::FocusGained
                        | event::Event::FocusLost
                        | event::Event::Mouse(_)
                        | event::Event::Paste(_) => Ok(()),
                    };

                    if res.is_err() {
                        break;
                    }
                }

                printer.read().print();

                if context.has_ended() {
                    break;
                }
            }
        });
    }

    fn new_root(&mut self) -> Self::Area {
        self.printer.write().flush_equalities().unwrap();

        let layout = Layout::new(self.fr, self.printer.clone());
        let root = Area::new(layout.main_index(), RwData::new(layout));
        let area = root.clone();

        self.windows.push(root);

        area
    }

    fn open(&mut self) {
        execute!(
            io::stdout(),
            terminal::EnterAlternateScreen,
            terminal::DisableLineWrap
        )
        .unwrap();
        terminal::enable_raw_mode().unwrap();
    }

    fn end(&mut self) {}

    fn close(&mut self) {
        execute!(
            io::stdout(),
            terminal::Clear(ClearType::All),
            terminal::LeaveAlternateScreen,
            terminal::EnableLineWrap,
            cursor::Show
        )
        .unwrap();
        terminal::disable_raw_mode().unwrap();
    }

    fn stop_printing(&mut self) {
        self.printer.write().disable()
    }

    fn resume_printing(&mut self) {
        self.printer.write().enable()
    }

    fn flush_layout(&mut self) {
        self.printer.write().flush_equalities().unwrap();
    }
}

#[derive(Clone, Copy)]
pub struct StaticFns {
    poll: fn() -> Result<bool, io::Error>,
    read: fn() -> Result<event::Event, io::Error>,
}

impl Default for StaticFns {
    fn default() -> Self {
        fn poll() -> Result<bool, io::Error> {
            crossterm::event::poll(Duration::from_millis(10))
        }

        Self { poll, read: crossterm::event::read }
    }
}

pub enum ConstraintErr {
    NoParent,
    Impossible,
}

impl std::fmt::Display for ConstraintErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

impl std::fmt::Debug for ConstraintErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // NOTE: Might not be true in the future.
            ConstraintErr::NoParent => {
                write!(f, "No parents, so its constraint can't be changed.")
            }
            ConstraintErr::Impossible => {
                write!(f, "The constraint change is impossible.")
            }
        }
    }
}

impl DuatError for ConstraintErr {
    fn into_text(self) -> duat_core::text::Text {
        match self {
            ConstraintErr::NoParent => {
                err!("Since it is the master node, its constraints " [*a] "cannot" [] "change")
            }
            ConstraintErr::Impossible => {
                err!("The requested constraint change is impossible")
            }
        }
    }
}

#[derive(Debug)]
pub enum Anchor {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

impl std::error::Error for ConstraintErr {}

#[derive(Clone, Copy, PartialEq)]
pub struct AreaId(usize);

impl AreaId {
    /// Generates a unique index for [`Rect`]s.
    fn new() -> Self {
        use std::sync::atomic::AtomicUsize;
        static INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

        AreaId(INDEX_COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

impl std::fmt::Debug for AreaId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

type Equality = cassowary::Constraint;
