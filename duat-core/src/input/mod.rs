mod commander;
mod default;
mod helper;

pub use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

pub use self::{commander::Commander, default::KeyMap, helper::EditHelper};
use crate::{
    data::{Context, RwData},
    position::Cursor,
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

#[derive(Clone, Debug)]
pub struct Cursors {
    list: Vec<Cursor>,
    main: usize,
}

impl Cursors {
    pub fn new() -> Self {
        Self {
            list: vec![Cursor::default()],
            main: 0,
        }
    }

    pub fn remove_extras(&mut self) {
        let cursor = self.list[self.main].clone();
        self.list = vec![cursor];
        self.main = 0;
    }

    pub fn insert(&mut self, cursor: Cursor) {
        self.list.push(cursor)
    }

    pub fn insert_and_switch(&mut self, _cursor: Cursor) {}

    pub fn main(&self) -> &Cursor {
        &self.list[self.main]
    }

    pub fn nth(&self, index: usize) -> Option<Cursor> {
        self.list.get(index).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Cursor, bool)> {
        self.list
            .iter()
            .enumerate()
            .map(move |(index, cursor)| (cursor, index == self.main))
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn reset(&mut self) {
        self.list = vec![Cursor::default()]
    }

    pub(crate) fn clear(&mut self) {
        self.list.clear()
    }
}

impl Default for Cursors {
    fn default() -> Self {
        Self::new()
    }
}
