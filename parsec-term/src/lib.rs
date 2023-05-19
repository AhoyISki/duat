#![feature(result_option_inspect, iter_collect_into)]

use std::{fmt::Debug, io, sync::atomic::Ordering};

use crossterm::{
    cursor, execute,
    terminal::{self, ClearType}
};

use area::PrintInfo;
use layout::Layout;
use parsec_core::{
    data::RwData,
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

    fn get_area(&self, index: usize) -> Option<Self::Area> {
        let layout = self.layout.clone();
        if layout.inspect(|layout| {
            layout
                .fetch_index(index)
                .filter(|rect| rect.read().children.is_none())
                .is_some()
        }) {
            Some(Area { index, layout })
        } else {
            None
        }
    }

    fn layout_has_changed(&self) -> bool {
        self.layout.inspect(|layout| {
            let ret = layout.vars_changed.load(Ordering::Acquire);
            layout.vars_changed.store(false, Ordering::Release);
            ret
        })
    }

    fn bisect(&mut self, index: usize, specs: PushSpecs, is_glued: bool) -> (usize, Option<usize>) {
        let mut layout = self.layout.write();
        let ret = layout.bisect(index, specs, is_glued);
        ret
    }

    fn request_width_to_fit(&self, _text: &str) -> Result<(), ()> {
        todo!()
    }
}

unsafe impl Send for Window {}
unsafe impl Sync for Window {}

pub struct Ui {
    windows: Vec<Window>
}

impl Default for Ui {
    fn default() -> Self {
        Ui {
            windows: Vec::new()
        }
    }
}

impl ui::Ui for Ui {
    type Area = Area;
    type PrintInfo = PrintInfo;
    type Window = Window;

    fn new_window(&mut self) -> (Self::Window, Self::Area) {
        let window = Window {
            layout: RwData::new(Layout::new())
        };

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

pub use area::{Area, Coords};
pub use rules::{SepChar, SepForm, VertRule, VertRuleCfg};
