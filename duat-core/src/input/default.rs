use super::{key, Cursors, EditHelper, InputForFiles, KeyCode, KeyEvent, KeyMod};
use crate::{
    data::{Context, RwData},
    ui::{Area, Ui},
    widgets::File,
};

#[derive(Clone)]
pub struct KeyMap {
    cursors: Cursors,
}

impl KeyMap {
    pub fn new() -> Self {
        Self { cursors: Cursors::new_exclusive() }
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
        context: Context<U>,
    ) {
        let mut editor = EditHelper::new(widget, area, &mut self.cursors);
        match key {
            // Characters
            key!(KeyCode::Char(char), KeyMod::SHIFT | KeyMod::NONE) => {
                editor.edit_on_each(|editor| {
                    editor.insert(char);
                });
                editor.move_each(|mover| {
                    mover.move_hor(1);
                });
            }
            key!(KeyCode::Enter) => {
                editor.edit_on_each(|editor| {
                    editor.insert('\n');
                });
                editor.move_each(|mover| {
                    mover.move_hor(1);
                });
            }

            // Text Removal
            key!(KeyCode::Backspace) => {
                let mut anchors = Vec::with_capacity(editor.cursors_len());
                editor.move_each(|mover| {
                    let caret = mover.caret();
                    anchors.push(mover.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(-1);
                });
                let mut anchors = anchors.into_iter().cycle();
                editor.edit_on_each(|editor| {
                    editor.replace("");
                });
                editor.move_each(|mover| {
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
                let mut anchors = Vec::with_capacity(editor.cursors_len());
                editor.move_each(|mover| {
                    let caret = mover.caret();
                    anchors.push(mover.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(1);
                });
                let mut anchors = anchors.into_iter().cycle();
                editor.edit_on_each(|editor| {
                    editor.replace("");
                });
                editor.move_each(|mover| {
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
            key!(KeyCode::Left, KeyMod::SHIFT) => {
                move_each_and_select(editor, Side::Left, 1);
            }
            key!(KeyCode::Right, KeyMod::SHIFT) => {
                move_each_and_select(editor, Side::Right, 1);
            }
            key!(KeyCode::Up, KeyMod::SHIFT) => {
                move_each_and_select(editor, Side::Top, 1);
            }
            key!(KeyCode::Down, KeyMod::SHIFT) => {
                move_each_and_select(editor, Side::Bottom, 1);
            }
            key!(KeyCode::Left) => move_each(editor, Side::Left, 1),
            key!(KeyCode::Right) => move_each(editor, Side::Right, 1),
            key!(KeyCode::Up) => move_each(editor, Side::Top, 1),
            key!(KeyCode::Down) => move_each(editor, Side::Bottom, 1),

            // Control
            key!(KeyCode::Char('p'), KeyMod::CONTROL) => {
                let _ = context.run_cmd("switch-to CommandLine<Ui>");
            }

            _ => {}
        }
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}

impl<U> InputForFiles<U> for KeyMap
where
    U: Ui,
{
    fn set_cursors(&mut self, cursors: Cursors) {
        self.cursors = cursors;
        self.cursors.set_exclusive();
    }
}

fn move_each<I>(mut editor: EditHelper<File, impl Area, I>, direction: Side, amount: usize) {
    editor.move_each(|mover| {
        mover.unset_anchor();
        match direction {
            Side::Top => mover.move_ver(-(amount as isize)),
            Side::Bottom => mover.move_ver(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize),
        }
    });
}

fn move_each_and_select<I>(
    mut editor: EditHelper<File, impl Area, I>,
    direction: Side,
    amount: usize,
) {
    editor.move_each(|mover| {
        if mover.anchor().is_none() {
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
