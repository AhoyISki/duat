use duat_core::{
    data::Pass,
    file::File,
    mode::{self, KeyCode, KeyEvent, KeyMod, key},
    prelude::Handle,
    ui::Ui,
};

use super::{IncSearch, RunCommands, SearchFwd, SearchRev};

#[derive(Clone)]
pub struct Regular;

impl<U: Ui> mode::Mode<U> for Regular {
    type Widget = File<U>;

    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, mut handle: Handle<Self::Widget, U>) {
        match key {
            // Characters
            key!(KeyCode::Char(char)) => handle.edit_all(pa, |mut e| {
                e.insert(char);
                e.move_hor(1);
            }),
            key!(KeyCode::Enter) => handle.edit_all(pa, |mut e| {
                e.insert('\n');
                e.move_hor(1);
            }),

            // Text Removal
            key!(KeyCode::Backspace) => handle.edit_all(pa, |mut e| {
                if e.anchor().is_some() {
                    e.replace("");
                    e.unset_anchor();
                } else {
                    e.move_hor(-1);
                    e.replace("");
                }
            }),
            key!(KeyCode::Delete) => handle.edit_all(pa, |mut e| {
                e.replace("");
                e.unset_anchor();
            }),

            // Movement
            key!(KeyCode::Left, KeyMod::SHIFT) => handle.edit_all(pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_hor(-1);
            }),
            key!(KeyCode::Right, KeyMod::SHIFT) => handle.edit_all(pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_hor(1);
            }),
            key!(KeyCode::Up, KeyMod::SHIFT) => handle.edit_all(pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_ver(-1);
            }),
            key!(KeyCode::Down, KeyMod::SHIFT) => handle.edit_all(pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_ver(1);
            }),
            key!(KeyCode::Left, KeyMod::NONE) => handle.edit_all(pa, |mut e| {
                e.unset_anchor();
                e.move_hor(-1);
            }),
            key!(KeyCode::Right, KeyMod::NONE) => handle.edit_all(pa, |mut e| {
                e.unset_anchor();
                e.move_hor(1);
            }),
            key!(KeyCode::Up, KeyMod::NONE) => handle.edit_all(pa, |mut e| {
                e.unset_anchor();
                e.move_ver(-1);
            }),
            key!(KeyCode::Down, KeyMod::NONE) => handle.edit_all(pa, |mut e| {
                e.unset_anchor();
                e.move_ver(1);
            }),

            // Control
            key!(KeyCode::Char('p'), KeyMod::CONTROL) => mode::set::<U>(RunCommands::new()),
            key!(KeyCode::Char('f'), KeyMod::CONTROL) => mode::set::<U>(IncSearch::new(SearchFwd)),
            key!(KeyCode::Char('F'), KeyMod::CONTROL) => mode::set::<U>(IncSearch::new(SearchRev)),

            _ => {}
        }
    }
}
