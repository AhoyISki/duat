mod default;
mod multi_cursor;

pub use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

pub use self::{
    default::KeyMap,
    multi_cursor::{Cursors, MultiCursorEditor},
};
use crate::{
    data::{Context, RwData},
    ui::Ui,
    widgets::ActiveWidget,
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
        key: KeyEvent,
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

pub macro key {
    ($code:pat) => {
        KeyEvent { code: $code, modifiers: KeyModifiers::NONE, .. }
    },

    ($code:pat, $modifiers:pat) => {
        KeyEvent { code: $code, modifiers: $modifiers, .. }
    }
}
