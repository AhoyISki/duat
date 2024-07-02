use super::{key, Cursors, KeyCode, KeyEvent, KeyModifiers, MultiCursorEditor};
use crate::{data::RwData, ui::Ui, widgets::File, Context};

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
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        globals: Context<U>,
    ) {
        let mut editor = MultiCursorEditor::<File, U>::new(widget, area, &mut self.cursors);
        match key {
            // Characters
            key!(KeyCode::Char(ch)) => {
                editor.edit_on_each_cursor(|editor| {
                    editor.insert(ch);
                });
                editor.move_each_cursor(|mover| {
                    mover.move_hor(1);
                });
            }
            key!(KeyCode::Char(ch), KeyModifiers::SHIFT) => {
                editor.edit_on_each_cursor(|editor| {
                    editor.insert(ch);
                });
                editor.move_each_cursor(|mover| {
                    mover.move_hor(1);
                });
            }
            key!(KeyCode::Enter) => {
                editor.edit_on_each_cursor(|editor| {
                    editor.insert('\n');
                });
                editor.move_each_cursor(|mover| {
                    mover.move_hor(1);
                });
            }

            // Text Removal
            key!(KeyCode::Backspace) => {
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
            key!(KeyCode::Delete) => {
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
            key!(KeyCode::Left, KeyModifiers::SHIFT) => {
                move_each_and_select(editor, Side::Left, 1);
            }
            key!(KeyCode::Right, KeyModifiers::SHIFT) => {
                move_each_and_select(editor, Side::Right, 1);
            }
            key!(KeyCode::Up, KeyModifiers::SHIFT) => {
                move_each_and_select(editor, Side::Top, 1);
            }
            key!(KeyCode::Down, KeyModifiers::SHIFT) => {
                move_each_and_select(editor, Side::Bottom, 1);
            }
            key!(KeyCode::Left) => move_each(editor, Side::Left, 1),
            key!(KeyCode::Right) => move_each(editor, Side::Right, 1),
            key!(KeyCode::Up) => move_each(editor, Side::Top, 1),
            key!(KeyCode::Down) => move_each(editor, Side::Bottom, 1),

            // Control
            key!(KeyCode::Char('p'), KeyModifiers::CONTROL) => {
                let _ = globals.commands.run("switch-to CommandLine");
            }

            _ => {}
        }
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}

fn move_each<U: Ui>(mut editor: MultiCursorEditor<File, U>, direction: Side, amount: usize) {
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

fn move_each_and_select<U: Ui>(
    mut editor: MultiCursorEditor<File, U>,
    direction: Side,
    amount: usize,
) {
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
