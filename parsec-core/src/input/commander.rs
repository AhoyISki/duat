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

impl crate::data::AsAny for Commander {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl InputMethod for Commander {
    type Widget = CommandLine;

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
        Some(&self.cursors)
    }
}
