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
};

pub use area::{Area, Coords};
use crossterm::{
    cursor, execute,
    terminal::{self, ClearType},
};
use duat_core::{data::RwData, ui};
use layout::Layout;
pub use layout::{Brush, Frame};
pub use rules::{VertRule, VertRuleCfg};

mod area;
mod layout;
mod print;
mod rules;

static STOP_PRINTING: AtomicBool = AtomicBool::new(false);

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
        STOP_PRINTING.store(true, Ordering::Relaxed);
    }

    fn shutdown(&mut self) {
        STOP_PRINTING.store(true, Ordering::Relaxed);
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
