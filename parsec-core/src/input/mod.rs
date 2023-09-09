mod commander;
mod default;
mod multi_cursor;
mod remapper;

use crossterm::event::KeyEvent;

pub use self::{
    commander::Commander,
    default::Editor,
    multi_cursor::{Cursors, MultiCursorEditor, NoHistory, WithHistory},
    remapper::Remapper,
};
use crate::{data::RwData, ui::Ui, widgets::ActiveWidget, Controler};

pub trait InputMethod: crate::data::AsAny + Send + Sync + 'static {
    type Widget: ActiveWidget
    where
        Self: Sized;

    fn send_key<U>(
        &mut self,
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        controler: &Controler<U>,
    ) where
        U: Ui,
        Self: Sized;

    fn remapper(self) -> Remapper<Self>
    where
        Self: Sized + Clone,
    {
        Remapper::new(self)
    }

    fn cursors(&self) -> Option<&Cursors> {
        None
    }
}
