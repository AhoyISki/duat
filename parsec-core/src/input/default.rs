use crossterm::event::KeyEvent;

use super::{Cursors, InputMethod};
use crate::{data::RwData, history::History, ui::Ui, widgets::File, Globals};

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

impl<U> InputMethod<U> for Editor
where
    U: Ui,
{
    type Widget = File<U>;

    fn send_key(
        &mut self,
        _key: KeyEvent,
        _widget: &RwData<Self::Widget>,
        _area: &U::Area,
        _globals: Globals<U>,
    ) {
        todo!()
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}
