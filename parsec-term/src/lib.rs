#![feature(result_option_inspect, iter_collect_into, let_chains)]
#![allow(clippy::type_complexity)]

use std::{fmt::Debug, io};

use area::PrintInfo;
use crossterm::{
    cursor, execute,
    terminal::{self, ClearType}
};
use layout::{Frame, Layout};
use parsec_core::{
    data::{ReadableData, RwData},
    ui::{self, PushSpecs}
};

mod area;
mod layout;
mod rules;

#[derive(Debug)]
pub enum Anchor {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight
}

#[derive(Debug, Clone)]
pub struct Window {
    layout: RwData<Layout>
}

impl ui::Window for Window {
    type Area = Area;

    fn bisect(&mut self, area: &Area, specs: PushSpecs, is_glued: bool) -> (Area, Option<Area>) {
        let (child, parent) =
            self.layout.mutate(|layout| layout.bisect(area.index, specs, is_glued));

        (
            Area::new(self.layout.clone(), child),
            parent.map(|parent| Area::new(self.layout.clone(), parent))
        )
    }
}

unsafe impl Send for Window {}
unsafe impl Sync for Window {}

#[derive(Clone, Copy, PartialEq)]
pub struct AreaIndex(usize);

impl std::fmt::Debug for AreaIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

#[derive(Default)]
pub struct Ui {
    windows: Vec<Window>,
    frame: Frame
}

impl Ui {
    pub fn new(frame: Frame) -> Self {
        Self { windows: Vec::new(), frame }
    }
}

impl ui::Ui for Ui {
    type ConstraintChangeErr = ConstraintChangeErr;
    type PrintInfo = PrintInfo;
    type Area = Area;
    type Window = Window;

    fn new_window(&mut self) -> (Self::Window, Self::Area) {
        let window = Window { layout: RwData::new(Layout::new(self.frame)) };

        let area = Area {
            layout: window.layout.clone(),
            index: window.layout.read().main_index()
        };

        self.windows.push(window.clone());

        (window, area)
    }

    fn startup(&mut self) {
        // This makes it so that if the application panics, the panic message
        // is printed nicely and the terminal is left in a usable
        // state.
        use std::panic;
        let orig_hook = panic::take_hook();
        panic::set_hook(Box::new(move |panic_info| {
            let _ = execute!(
                io::stdout(),
                terminal::Clear(ClearType::All),
                terminal::LeaveAlternateScreen,
                cursor::Show
            );

            terminal::disable_raw_mode().unwrap();

            orig_hook(panic_info);
            std::process::exit(1)
        }));

        let _ = execute!(io::stdout(), terminal::EnterAlternateScreen);
        terminal::enable_raw_mode().unwrap();
    }

    fn shutdown(&mut self) {
        let _ = execute!(
            io::stdout(),
            terminal::Clear(ClearType::All),
            terminal::LeaveAlternateScreen,
            cursor::Show
        );
        terminal::disable_raw_mode().unwrap();
    }
}

pub enum ConstraintChangeErr {
    NoParent,
    Impossible
}

impl std::fmt::Debug for ConstraintChangeErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // NOTE: Might not be true in the future.
            ConstraintChangeErr::NoParent => {
                write!(f, "The area does not have a parent, so its constraint cannot be changed.")
            }
            ConstraintChangeErr::Impossible => {
                write!(f, "The constraint change is impossible.")
            }
        }
    }
}

pub use area::{Area, Coords};
pub use rules::{SepChar, SepForm, VertRule, VertRuleCfg};
