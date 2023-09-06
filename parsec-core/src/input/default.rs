use super::{Cursors, InputMethod};
use crate::{history::History, ui::Ui, widgets::FileWidget};

#[derive(Clone)]
pub struct Editor {
    cursors: Cursors,
    history: History,
}

impl Editor {
    pub fn new() -> Self {
        Self {
            cursors: Cursors::new(),
            history: History::new(),
        }
    }
}

impl crate::data::AsAny for Editor {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl InputMethod for Editor {
    type Widget = FileWidget;

    fn send_key<U>(
        &mut self,
        key: crossterm::event::KeyEvent,
        widget: &crate::data::RwData<Self::Widget>,
        area: &U::Area,
        controler: &crate::Controler<U>,
    ) -> bool
    where
        U: Ui,
    {
        todo!()
    }

    fn cursors(&self) -> Option<&Cursors> {
        todo!()
    }
}
