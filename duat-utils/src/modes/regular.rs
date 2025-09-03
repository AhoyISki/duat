use duat_core::{
    cmd,
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
                } else if e.move_hor(-1) == -1 {
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
            key!(Char('v'), Mod::CONTROL) => handle.edit_all(pa, |mut e| {
                if let Some(text) = duat_core::clipboard::get_text() {
                    e.replace(text);
                }
            }),

            // Searching
            key!(Char('f'), Mod::CONTROL) => mode::set::<U>(IncSearch::new(SearchFwd)),
            key!(Char('F'), Mod::CONTROL) => mode::set::<U>(IncSearch::new(SearchRev)),

            // Control
            key!(Char('P'), Mod::CONTROL) | key!(F(1)) => mode::set::<U>(RunCommands::new()),
            key!(Char('p'), Mod::CONTROL) => {
                mode::set::<U>(RunCommands::new());
                mode::send_keys("edit ");
            }
            key!(Char('N'), Mod::CONTROL) => {
                mode::set::<U>(RunCommands::new());
                mode::send_keys("open ");
            }
            key!(Char('W'), Mod::CONTROL) => cmd::quit(),
            key!(Char(','), Mod::CONTROL) => cmd::queue_notify("open --cfg"),

            _ => {}
        }
    }
}
