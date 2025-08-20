use duat_core::{
    data::Pass,
    file::File,
    mode::{self, KeyCode::*, KeyEvent, KeyMod as Mod, key},
    prelude::Handle,
    ui::Ui,
};

use super::{IncSearch, RunCommands, SearchFwd, SearchRev};

/// The regular, bogstandard mode, a.k.a., supposed to be like VSCode
#[derive(Clone)]
pub struct Regular;

impl<U: Ui> mode::Mode<U> for Regular {
    type Widget = File<U>;

    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<Self::Widget, U>) {
        match key {
            // Characters
            key!(Char(char)) => handle.edit_all(pa, |mut e| {
                e.insert(char);
                e.move_hor(1);
            }),
            key!(Enter) => handle.edit_all(pa, |mut e| {
                e.insert('\n');
                e.move_hor(1);
            }),

            // Text Removal
            key!(Backspace) => handle.edit_all(pa, |mut e| {
                if e.anchor().is_some() {
                    e.replace("");
                    e.unset_anchor();
                } else {
                    e.move_hor(-1);
                    e.set_anchor();
                    e.replace("");
                    e.unset_anchor();
                }
            }),
            key!(Delete) => handle.edit_all(pa, |mut e| {
                e.set_anchor_if_needed();
                e.replace("");
                e.unset_anchor();
            }),

            // Movement
            key!(Left) => handle.edit_all(pa, |mut e| {
                e.unset_anchor();
                e.move_hor(-1);
            }),
            key!(Right) => handle.edit_all(pa, |mut e| {
                e.unset_anchor();
                e.move_hor(1);
            }),
            key!(Up) => handle.edit_all(pa, |mut e| {
                e.unset_anchor();
                e.move_ver(-1);
            }),
            key!(Down) => handle.edit_all(pa, |mut e| {
                e.unset_anchor();
                e.move_ver(1);
            }),
            key!(Left, Mod::SHIFT) => handle.edit_all(pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_hor(-1);
            }),
            key!(Right, Mod::SHIFT) => handle.edit_all(pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_hor(1);
            }),
            key!(Up, Mod::SHIFT) => handle.edit_all(pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_ver(-1);
            }),
            key!(Down, Mod::SHIFT) => handle.edit_all(pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_ver(1);
            }),

            // Copying and pasting
            key!(Char('c'), Mod::CONTROL) => handle.edit_main(pa, |e| {
                duat_core::clipboard::set_text(e.selection());
            }),
            key!(Char('v'), Mod::CONTROL) => handle.edit_main(pa, |e| {
                duat_core::clipboard::set_text(e.selection());
            }),

            // Control
            key!(Char('p'), Mod::CONTROL) => mode::set::<U>(RunCommands::new()),
            key!(Char('f'), Mod::CONTROL) => mode::set::<U>(IncSearch::new(SearchFwd)),
            key!(Char('F'), Mod::CONTROL) => mode::set::<U>(IncSearch::new(SearchRev)),

            _ => {}
        }
    }
}
