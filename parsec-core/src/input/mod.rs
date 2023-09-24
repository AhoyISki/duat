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
use crate::{data::RwData, ui::Area, widgets::ActiveWidget};

pub trait InputMethod: Send + Sync + 'static {
    type Widget: ActiveWidget
    where
        Self: Sized;

    fn send_key(&mut self, key: KeyEvent, widget: &RwData<Self::Widget>, area: &impl Area)
    where
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
