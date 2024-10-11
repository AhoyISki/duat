use super::{key, Cursors, EditHelper, InputForFiles, KeyCode, KeyEvent, KeyMod};
use crate::{
    data::{Context, RwData},
    ui::{Area, Ui},
    widgets::{CommandLine, File},
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
        _context: Context<U>,
    ) {
        let mut helper = EditHelper::new(widget, area, &mut self.cursors);
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
            key!(KeyCode::Char('p'), KeyMod::CONTROL) => crate::switch_to::<CommandLine<U>>(),

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
