use super::{Cursors, EditHelper, Fwd, IncSearcher, KeyCode, KeyEvent, KeyMod, key};
use crate::{
    commands,
    data::RwData,
    ui::{Area, Ui},
    widgets::{File, IncSearch, RunCommands},
};

#[derive(Clone)]
pub struct Regular;

impl<U: Ui> super::Mode<U> for Regular {
    type Widget = File;

    fn send_key(
        &mut self,
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        cursors: Option<Cursors>,
    ) -> Option<Cursors> {
        let mut cursors = cursors.unwrap_or_else(Cursors::new_exclusive);

        let mut helper = EditHelper::new(widget, area, &mut cursors);
        match key {
            // Characters
            key!(KeyCode::Char(char), KeyMod::SHIFT | KeyMod::NONE) => {
                helper.edit_on_each(|e| e.insert(char));
                helper.move_each(|m| m.move_hor(1));
            }
            key!(KeyCode::Enter) => {
                helper.edit_on_each(|e| e.insert('\n'));
                helper.move_each(|m| m.move_hor(1));
            }

            // Text Removal
            key!(KeyCode::Backspace) => {
                let mut anchors = Vec::with_capacity(helper.cursors_len());
                helper.move_each(|m| {
                    let caret = m.caret();
                    anchors.push(m.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                    m.set_anchor();
                    m.move_hor(-1);
                });
                let mut anchors = anchors.into_iter().cycle();
                helper.edit_on_each(|e| e.replace(""));
                helper.move_each(|m| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        m.set_anchor();
                        m.move_to(anchor);
                        m.swap_ends()
                    } else {
                        m.unset_anchor();
                    }
                });
            }
            key!(KeyCode::Delete) => {
                let mut anchors = Vec::with_capacity(helper.cursors_len());
                helper.move_each(|m| {
                    let caret = m.caret();
                    anchors.push(m.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                    m.set_anchor();
                    m.move_hor(1);
                });
                let mut anchors = anchors.into_iter().cycle();
                helper.edit_on_each(|e| e.replace(""));
                helper.move_each(|m| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        m.set_anchor();
                        m.move_to(anchor);
                        m.swap_ends();
                    } else {
                        m.unset_anchor();
                    }
                });
            }

            // Movement
            key!(KeyCode::Left, KeyMod::SHIFT) => move_each_and_select(helper, Side::Left, 1),
            key!(KeyCode::Right, KeyMod::SHIFT) => move_each_and_select(helper, Side::Right, 1),
            key!(KeyCode::Up, KeyMod::SHIFT) => move_each_and_select(helper, Side::Top, 1),
            key!(KeyCode::Down, KeyMod::SHIFT) => move_each_and_select(helper, Side::Bottom, 1),
            key!(KeyCode::Left) => move_each(helper, Side::Left, 1),
            key!(KeyCode::Right) => move_each(helper, Side::Right, 1),
            key!(KeyCode::Up) => move_each(helper, Side::Top, 1),
            key!(KeyCode::Down) => move_each(helper, Side::Bottom, 1),

            // Control
            key!(KeyCode::Char('p'), KeyMod::CONTROL) => {
                commands::set_cmd_mode::<U>(RunCommands::new())
            }
            key!(KeyCode::Char('f'), KeyMod::CONTROL) => {
                commands::set_cmd_mode::<U>(IncSearch::new(Fwd::new))
            }

            _ => {}
        }

        Some(cursors)
    }
}

fn move_each<I>(mut helper: EditHelper<File, impl Area, I>, direction: Side, amount: usize) {
    helper.move_each(|m| {
        m.unset_anchor();
        match direction {
            Side::Top => m.move_ver(-(amount as isize)),
            Side::Bottom => m.move_ver(amount as isize),
            Side::Left => m.move_hor(-(amount as isize)),
            Side::Right => m.move_hor(amount as isize),
        }
    });
}

fn move_each_and_select<I>(
    mut helper: EditHelper<File, impl Area, I>,
    direction: Side,
    amount: usize,
) {
    helper.move_each(|m| {
        if m.anchor().is_none() {
            m.set_anchor();
        }
        match direction {
            Side::Top => m.move_ver(-(amount as isize)),
            Side::Bottom => m.move_ver(amount as isize),
            Side::Left => m.move_hor(-(amount as isize)),
            Side::Right => m.move_hor(amount as isize),
        }
    });
}

enum Side {
    Left,
    Right,
    Top,
    Bottom,
}
