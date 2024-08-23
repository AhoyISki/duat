mod commander;
mod default;
mod helper;

pub mod key {
    pub use crossterm::event::{KeyCode as Code, KeyEvent as Event, KeyModifiers as Mod};

    pub macro key {
        ($code:pat) => {
            Event { code: $code, modifiers: Mod::NONE, .. }
        },

        ($code:pat, $modifiers:pat) => {
            Event { code: $code, modifiers: $modifiers, .. }
        }
    }
}

use key::Event;

pub use self::{
    commander::Commander,
    default::KeyMap,
    helper::{Cursor, Cursors, EditHelper},
};
use crate::{
    data::{Context, RwData},
    ui::Ui,
    widgets::{ActiveWidget, File},
};

pub trait InputMethod<U>: Send + Sync + 'static
where
    U: Ui,
{
    type Widget: ActiveWidget<U>
    where
        Self: Sized;

    fn send_key(
        &mut self,
        key: Event,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        globals: Context<U>,
    ) where
        Self: Sized;

    fn cursors(&self) -> Option<&Cursors> {
        None
    }

    fn on_focus(&mut self, _area: &U::Area)
    where
        Self: Sized,
    {
    }

    fn on_unfocus(&mut self, _area: &U::Area)
    where
        Self: Sized,
    {
    }
}

pub trait InputForFiles<U>: Sized + InputMethod<U, Widget = File>
where
    U: Ui,
{
    fn set_cursors(&mut self, cursors: Cursors);
}
