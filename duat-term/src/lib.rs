#![allow(incomplete_features)]
#![feature(
    lazy_cell,
    result_option_inspect,
    iter_collect_into,
    let_chains,
    generic_const_exprs
)]

use std::{
    fmt::Debug,
    io,
    sync::atomic::{AtomicBool, Ordering},
    time::Duration,
};

pub use area::{Area, Coords};
use crossterm::{
    cursor, event, execute,
    terminal::{self, ClearType},
};
use duat_core::{data::RwData, ui, Globals};
use layout::Layout;
pub use layout::{Brush, Frame};
use print::Printer;
pub use rules::{VertRule, VertRuleCfg};

mod area;
mod layout;
mod print;
mod rules;

static RESIZED: AtomicBool = AtomicBool::new(false);

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

pub struct Ui {
    windows: Vec<Area>,
    printer: RwData<Printer>,
    frame: Frame,
}

impl Default for Ui {
    fn default() -> Self {
        Self::new(Frame::default())
    }
}

impl Ui {
    pub fn new(frame: Frame) -> Self {
        let printer = Printer::new();

        Self {
            windows: Vec::new(),
            printer: RwData::new(printer),
            frame,
        }
    }
}

impl ui::Ui for Ui {
    type Area = Area;
    type ConstraintChangeErr = ConstraintChangeErr;

    fn start(&mut self, sender: ui::Sender, globals: Globals<Self>) {
        let printer = self.printer.clone();
        globals.spawn(move || {
            loop {
                if let Ok(true) = event::poll(Duration::from_millis(10)) {
                    let res = match event::read().unwrap() {
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

                if globals.has_ended() {
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
        execute!(io::stdout(), terminal::EnterAlternateScreen).unwrap();
        terminal::enable_raw_mode().unwrap();
    }

    fn end(&mut self) {}

    fn close(&mut self) {
        execute!(
            io::stdout(),
            terminal::Clear(ClearType::All),
            terminal::LeaveAlternateScreen,
            cursor::Show
        )
        .unwrap();
        terminal::disable_raw_mode().unwrap();
    }

    fn finish_printing(&self) {}
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
                write!(
                    f,
                    "The area does not have a parent, so its constraint cannot be changed."
                )
            }
            ConstraintChangeErr::Impossible => {
                write!(f, "The constraint change is impossible.")
            }
        }
    }
}

type Equality = cassowary::Constraint;
