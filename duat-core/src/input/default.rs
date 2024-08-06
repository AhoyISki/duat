use super::{
    key::{key, Code, Event, Mod},
    Cursors, EditHelper,
};
use crate::{
    data::{Context, RwData},
    ui::Ui,
    widgets::File,
};

#[derive(Clone)]
pub struct KeyMap {
    cursors: Cursors,
}

impl KeyMap {
    pub fn new() -> Self {
        Self {
            cursors: Cursors::new(),
        }
    }
}

impl Default for KeyMap {
    fn default() -> Self {
        Self::new()
    }
}

impl<U> super::InputMethod<U> for KeyMap
where
    U: Ui,
{
    type Widget = File;

    fn send_key(
        &mut self,
        key: Event,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        context: Context<U>,
    ) {
        let mut editor = EditHelper::<File, U>::new(widget, area, &mut self.cursors);
        match key {
            // Characters
            key!(Code::Char(ch)) => {
                editor.edit_on_each_cursor(|editor| {
                    editor.insert(ch);
                });
                editor.move_each_cursor(|mover| {
                    mover.move_hor(1);
                });
            }
            key!(Code::Char(ch), Mod::SHIFT) => {
                editor.edit_on_each_cursor(|editor| {
                    editor.insert(ch);
                });
                editor.move_each_cursor(|mover| {
                    mover.move_hor(1);
                });
            }
            key!(Code::Enter) => {
                editor.edit_on_each_cursor(|editor| {
                    editor.insert('\n');
                });
                editor.move_each_cursor(|mover| {
                    mover.move_hor(1);
                });
            }

            // Text Removal
            key!(Code::Backspace) => {
                let mut anchors = Vec::with_capacity(editor.len_cursors());
                editor.move_each_cursor(|mover| {
                    let caret = mover.caret();
                    anchors.push(mover.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(-1);
                });
                let mut anchors = anchors.into_iter().cycle();
                editor.edit_on_each_cursor(|editor| {
                    editor.replace("");
                });
                editor.move_each_cursor(|mover| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        mover.set_anchor();
                        mover.move_to(anchor);
                        mover.swap_ends()
                    } else {
                        mover.unset_anchor();
                    }
                });
            }
            key!(Code::Delete) => {
                let mut anchors = Vec::with_capacity(editor.len_cursors());
                editor.move_each_cursor(|mover| {
                    let caret = mover.caret();
                    anchors.push(mover.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(1);
                });
                let mut anchors = anchors.into_iter().cycle();
                editor.edit_on_each_cursor(|editor| {
                    editor.replace("");
                });
                editor.move_each_cursor(|mover| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        mover.set_anchor();
                        mover.move_to(anchor);
                        mover.swap_ends()
                    } else {
                        mover.unset_anchor();
                    }
                });
            }

            // Movement
            key!(Code::Left, Mod::SHIFT) => {
                move_each_and_select(editor, Side::Left, 1);
            }
            key!(Code::Right, Mod::SHIFT) => {
                move_each_and_select(editor, Side::Right, 1);
            }
            key!(Code::Up, Mod::SHIFT) => {
                move_each_and_select(editor, Side::Top, 1);
            }
            key!(Code::Down, Mod::SHIFT) => {
                move_each_and_select(editor, Side::Bottom, 1);
            }
            key!(Code::Left) => move_each(editor, Side::Left, 1),
            key!(Code::Right) => move_each(editor, Side::Right, 1),
            key!(Code::Up) => move_each(editor, Side::Top, 1),
            key!(Code::Down) => move_each(editor, Side::Bottom, 1),

            // Control
            key!(Code::Char('p'), Mod::CONTROL) => {
                let _ = context.commands.run("switch-to CommandLine");
            }

            _ => {}
        }
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}

fn move_each<U: Ui>(mut editor: EditHelper<File, U>, direction: Side, amount: usize) {
    editor.move_each_cursor(|mover| {
        mover.unset_anchor();
        match direction {
            Side::Top => mover.move_ver(-(amount as isize)),
            Side::Bottom => mover.move_ver(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize),
        }
    });
}

fn move_each_and_select<U: Ui>(mut editor: EditHelper<File, U>, direction: Side, amount: usize) {
    editor.move_each_cursor(|mover| {
        if !mover.anchor_is_set() {
            mover.set_anchor();
        }
        match direction {
            Side::Top => mover.move_ver(-(amount as isize)),
            Side::Bottom => mover.move_ver(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize),
        }
    });
}

enum Side {
    Left,
    Right,
    Top,
    Bottom,
}
