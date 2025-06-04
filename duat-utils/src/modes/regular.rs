use duat_core::{
    data::{Pass, RwData},
    file::File,
    mode::{self, EditHelper, KeyCode, KeyEvent, KeyMod, key},
    ui::Ui,
};

use super::{IncSearch, RunCommands, SearchFwd, SearchRev};

#[derive(Clone)]
pub struct Regular;

impl<U: Ui> mode::Mode<U> for Regular {
    type Widget = File<U>;

    fn send_key(
        &mut self,
        mut pa: Pass,
        key: KeyEvent,
        widget: RwData<Self::Widget>,
        area: U::Area,
    ) {
        let mut helper = EditHelper::new(&mut pa, widget, area);

        match key {
            // Characters
            key!(KeyCode::Char(char)) => helper.edit_all(&mut pa, |mut e| {
                e.insert(char);
                e.move_hor(1);
            }),
            key!(KeyCode::Enter) => helper.edit_all(&mut pa, |mut e| {
                e.insert('\n');
                e.move_hor(1);
            }),

            // Text Removal
            key!(KeyCode::Backspace) => helper.edit_all(&mut pa, |mut e| {
                if e.anchor().is_some() {
                    e.replace("");
                    e.unset_anchor();
                } else {
                    e.move_hor(-1);
                    e.replace("");
                }
            }),
            key!(KeyCode::Delete) => helper.edit_all(&mut pa, |mut e| {
                e.replace("");
                e.unset_anchor();
            }),

            // Movement
            key!(KeyCode::Left, KeyMod::SHIFT) => helper.edit_all(&mut pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_hor(-1);
            }),
            key!(KeyCode::Right, KeyMod::SHIFT) => helper.edit_all(&mut pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_hor(1);
            }),
            key!(KeyCode::Up, KeyMod::SHIFT) => helper.edit_all(&mut pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_ver(-1);
            }),
            key!(KeyCode::Down, KeyMod::SHIFT) => helper.edit_all(&mut pa, |mut e| {
                e.set_anchor_if_needed();
                e.move_ver(1);
            }),
            key!(KeyCode::Left, KeyMod::NONE) => helper.edit_all(&mut pa, |mut e| {
                e.unset_anchor();
                e.move_hor(-1);
            }),
            key!(KeyCode::Right, KeyMod::NONE) => helper.edit_all(&mut pa, |mut e| {
                e.unset_anchor();
                e.move_hor(1);
            }),
            key!(KeyCode::Up, KeyMod::NONE) => helper.edit_all(&mut pa, |mut e| {
                e.unset_anchor();
                e.move_ver(-1);
            }),
            key!(KeyCode::Down, KeyMod::NONE) => helper.edit_all(&mut pa, |mut e| {
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
