use crossterm::event::KeyEvent;

use super::{Cursors, InputMethod};
use crate::{data::RwData, history::History, ui::Area, widgets::File};

#[derive(Clone)]
pub struct Editor {
    cursors: Cursors,
    _history: History,
}

impl Editor {
    pub fn new() -> Self {
        Self {
            cursors: Cursors::new(),
            _history: History::new(),
        }
    }
}

impl Default for Editor {
    fn default() -> Self {
        Self::new()
    }
}

impl InputMethod for Editor {
    type Widget = File;

    fn send_key(&mut self, _key: KeyEvent, _widget: &RwData<Self::Widget>, _area: &impl Area) {
        todo!()
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}
