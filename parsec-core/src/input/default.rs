use super::{Cursors, InputMethod};
use crate::{history::History, ui::Ui, widgets::FileWidget};

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

//impl crate::data::AsAny for Editor {
//    fn as_any(&self) -> &dyn std::any::Any {
//        self
//    }
//}

impl InputMethod for Editor {
    type Widget = FileWidget;

    fn send_key<U>(
        &mut self,
        _key: crossterm::event::KeyEvent,
        _widget: &crate::data::RwData<Self::Widget>,
        _area: &U::Area,
        _controler: &crate::Controler<U>,
    ) where
        U: Ui,
    {
        todo!()
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}
