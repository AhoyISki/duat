use lender::Lender;

use super::{
    EditHelper, IncSearch, KeyCode, KeyEvent, KeyMod, RunCommands, SearchFwd, SearchRev, key,
};
use crate::{
    ui::{Area, Ui},
    widgets::File,
};

#[derive(Clone)]
pub struct Regular;

impl<U: Ui> super::Mode<U> for Regular {
    type Widget = File;

    fn send_key(&mut self, key: KeyEvent, widget: &mut Self::Widget, area: &U::Area) {
        let mut helper = EditHelper::new(widget, area);

        match key {
            // Characters
            key!(KeyCode::Char(char)) => helper.edit_iter().for_each(|mut e| {
                e.insert(char);
                e.move_hor(1);
            }),
            key!(KeyCode::Enter) => helper.edit_iter().for_each(|mut e| {
                e.insert('\n');
                e.move_hor(1);
            }),

            // Text Removal
            key!(KeyCode::Backspace) => helper.edit_iter().for_each(|mut e| {
                if e.anchor().is_some() {
                    e.replace("");
                    e.unset_anchor();
                } else {
                    e.move_hor(-1);
                    e.replace("");
                }
            }),
            key!(KeyCode::Delete) => helper.edit_iter().for_each(|mut e| {
                e.replace("");
                e.unset_anchor();
            }),

            // Movement
            key!(KeyCode::Left, KeyMod::SHIFT) => move_each_and_select(helper, Side::Left, 1),
            key!(KeyCode::Right, KeyMod::SHIFT) => move_each_and_select(helper, Side::Right, 1),
            key!(KeyCode::Up, KeyMod::SHIFT) => move_each_and_select(helper, Side::Top, 1),
            key!(KeyCode::Down, KeyMod::SHIFT) => move_each_and_select(helper, Side::Bottom, 1),
            key!(KeyCode::Left, KeyMod::NONE) => move_each(helper, Side::Left, 1),
            key!(KeyCode::Right, KeyMod::NONE) => move_each(helper, Side::Right, 1),
            key!(KeyCode::Up, KeyMod::NONE) => move_each(helper, Side::Top, 1),
            key!(KeyCode::Down, KeyMod::NONE) => move_each(helper, Side::Bottom, 1),

            // Control
            key!(KeyCode::Char('p'), KeyMod::CONTROL) => super::set::<U>(RunCommands::new()),
            key!(KeyCode::Char('f'), KeyMod::CONTROL) => super::set::<U>(IncSearch::new(SearchFwd)),
            key!(KeyCode::Char('F'), KeyMod::CONTROL) => super::set::<U>(IncSearch::new(SearchRev)),

            _ => {}
        }
    }
}

fn move_each<S>(mut helper: EditHelper<File, impl Area, S>, direction: Side, amount: u32) {
    helper.edit_iter().for_each(|mut e| {
        e.unset_anchor();
        match direction {
            Side::Top => e.move_ver(-(amount as i32)),
            Side::Bottom => e.move_ver(amount as i32),
            Side::Left => e.move_hor(-(amount as i32)),
            Side::Right => e.move_hor(amount as i32),
        };
    });
}

fn move_each_and_select<S>(
    mut helper: EditHelper<File, impl Area, S>,
    direction: Side,
    amount: u32,
) {
    helper.edit_iter().for_each(|mut e| {
        if e.anchor().is_none() {
            e.set_anchor();
        }
        match direction {
            Side::Top => e.move_ver(-(amount as i32)),
            Side::Bottom => e.move_ver(amount as i32),
            Side::Left => e.move_hor(-(amount as i32)),
            Side::Right => e.move_hor(amount as i32),
        };
    });
}

enum Side {
    Left,
    Right,
    Top,
    Bottom,
}
