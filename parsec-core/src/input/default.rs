use crossterm::event::KeyEvent;

use super::{Cursors, InputMethod};
use crate::{history::History, ui::{Ui, Area}, widgets::File, data::RwData};

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

    fn send_key(&mut self, key: KeyEvent, widget: &RwData<Self::Widget>, area: &impl Area) {
        todo!()
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}
