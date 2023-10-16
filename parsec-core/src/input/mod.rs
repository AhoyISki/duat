mod commander;
mod default;
mod multi_cursor;
mod remapper;

use crossterm::event::{KeyEvent, KeyModifiers};

pub use self::{
    commander::Commander,
    default::Editor,
    multi_cursor::{Cursors, MultiCursorEditor},
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

    fn on_focus(&mut self, _area: &impl Area)
    where
        Self: Sized,
    {
    }

    fn on_unfocus(&mut self, _area: &impl Area)
    where
        Self: Sized,
    {
    }
}

pub macro key {
    ($code:pat) => {
        KeyEvent { code: $code, modifiers: KeyModifiers::NONE, .. }
    },

    ($code:pat, $modifiers:pat) => {
        crossterm::event::KeyEvent { code: $code, modifiers: $modifiers, .. }
    }
}
