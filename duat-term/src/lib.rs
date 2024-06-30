#![allow(incomplete_features)]
#![feature(iter_collect_into, let_chains, generic_const_exprs)]

use std::{
    backtrace,
    fmt::Debug,
    io,
    sync::{
        atomic::{AtomicBool, Ordering},
        OnceLock,
    },
    time::Duration,
};

pub use area::{Area, Coords};
use crossterm::{
    cursor, event, execute,
    terminal::{self, ClearType},
};
use duat_core::{data::RwData, ui, Context};
use layout::Layout;
pub use layout::{Brush, Frame};
use print::Printer;
pub use rules::{VertRule, VertRuleCfg};

mod area;
mod layout;
mod print;
mod rules;

static FUNCTIONS: OnceLock<StaticFns> = OnceLock::new();
static RESIZED: AtomicBool = AtomicBool::new(false);

pub struct Ui {
    windows: Vec<Area>,
    printer: RwData<Printer>,
    frame: Frame,
}

impl ui::Ui for Ui {
    type Area = Area;
    type ConstraintChangeErr = ConstraintChangeErr;
    type StaticFns = StaticFns;

    fn new(statics: Self::StaticFns) -> Self {
        FUNCTIONS.get_or_init(|| statics);

        std::panic::set_hook(Box::new(|hook| {
            let backtrace = std::backtrace::Backtrace::force_capture().to_string();
            execute!(
                io::stdout(),
                terminal::Clear(ClearType::All),
                terminal::LeaveAlternateScreen,
                terminal::EnableLineWrap,
                cursor::Show
            )
            .unwrap();
            terminal::disable_raw_mode().unwrap();
            println!("{hook}\n\nBACKTRACE:\n\n{backtrace}")
        }));

        Ui {
            windows: Vec::new(),
            printer: RwData::new(Printer::new()),
            frame: Frame::default(),
        }
    }

    fn start(&mut self, sender: ui::Sender, context: Context<Self>) {
        let functions = FUNCTIONS.get().unwrap();
        let printer = self.printer.clone();
        context.spawn(move || {
            loop {
                if let Ok(true) = (functions.poll)() {
                    let res = match (functions.read)().unwrap() {
                        event::Event::Key(key) => sender.send_key(key),
                        event::Event::Resize(..) => {
                            RESIZED.store(true, Ordering::Release);
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
        let layout = Layout::new(self.frame, self.printer.clone());
        let root = Area::new(layout.main_index(), RwData::new(layout));

        let area = Area::new(root.layout.read().main_index(), root.layout.clone());

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

    fn finish_printing(&self) {}
}

#[derive(Clone, Copy)]
pub struct StaticFns {
    poll: fn() -> Result<bool, io::Error>,
    read: fn() -> Result<event::Event, io::Error>,
}

impl Default for StaticFns {
    fn default() -> Self {
        fn poll() -> Result<bool, io::Error> {
            crossterm::event::poll(Duration::from_millis(50))
        }

        Self {
            poll,
            read: crossterm::event::read,
        }
    }
}

pub enum ConstraintChangeErr {
    NoParent,
    Impossible,
}

impl std::fmt::Debug for ConstraintChangeErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // NOTE: Might not be true in the future.
            ConstraintChangeErr::NoParent => {
                write!(f, "No parents, so its constraint can't be changed.")
            }
            ConstraintChangeErr::Impossible => {
                write!(f, "The constraint change is impossible.")
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
