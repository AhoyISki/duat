use super::{Cursors, InputMethod};
use crate::{position::Cursor, ui::Ui, widgets::CommandLine};

#[derive(Clone)]
pub struct Commander {
    cursors: Cursors,
}

impl Commander {
    pub fn new() -> Self {
        let mut cursors = Cursors::new();
        cursors.insert(Cursor::default());
        Self { cursors }
    }
}

impl Default for Commander {
    fn default() -> Self {
        Self::new()
    }
}

//impl crate::data::AsAny for Commander {
//    fn as_any(&self) -> &dyn std::any::Any {
//        self
//    }
//}

impl InputMethod for Commander {
    type Widget = CommandLine;

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
