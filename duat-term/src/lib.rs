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
use duat_core::{data::RwData, ui, log_info};
use layout::Layout;
pub use layout::{Brush, Frame};
pub use rules::{VertRule, VertRuleCfg};

mod area;
mod layout;
mod print;
mod rules;

static DUAT_ENDED: AtomicBool = AtomicBool::new(false);
static PRINT_EDGES: AtomicBool = AtomicBool::new(false);

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

#[derive(Default)]
pub struct Ui {
    windows: Vec<Area>,
    frame: Frame,
}

impl Ui {
    pub fn new(frame: Frame) -> Self {
        Self {
            windows: Vec::new(),
            frame,
        }
    }
}

impl ui::Ui for Ui {
    type Area = Area;
    type ConstraintChangeErr = ConstraintChangeErr;

    fn set_sender(&mut self, sender: ui::Sender) {
        std::thread::spawn(move || {
            loop {
                if let Ok(true) = event::poll(Duration::from_millis(10)) {
                    let res = match event::read().unwrap() {
                        event::Event::Key(key) => sender.send_key(key),
                        event::Event::Resize(..) => {
                            PRINT_EDGES.store(true, Ordering::Release);
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

                if DUAT_ENDED.fetch_and(false, Ordering::Relaxed) {
                    break;
                }
            }
        });
    }

    fn new_root(&mut self) -> Self::Area {
        let layout = Layout::new(self.frame);
        let root = Area::new(layout.main_index(), RwData::new(layout));

        let area = Area::new(root.layout.read().main_index(), root.layout.clone());

        self.windows.push(root);

        area
    }

    fn startup(&mut self) {
        execute!(io::stdout(), terminal::EnterAlternateScreen).unwrap();
        terminal::enable_raw_mode().unwrap();
    }

    fn unload(&mut self) {
        DUAT_ENDED.store(true, Ordering::Relaxed);
    }

    fn shutdown(&mut self) {
        DUAT_ENDED.store(true, Ordering::Relaxed);
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
