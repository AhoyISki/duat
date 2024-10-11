use super::{key, Cursors, EditHelper, InputMethod, KeyCode, KeyEvent, KeyMod};
use crate::{
    data::{Context, RwData},
    ui::Ui,
    widgets::CommandLine,
};

#[derive(Clone)]
pub struct Commander {
    cursors: Cursors,
}

impl Commander {
    pub fn new() -> Self {
        Self { cursors: Cursors::new_exclusive() }
    }
}

impl Default for Commander {
    fn default() -> Self {
        Self::new()
    }
}

impl<U> InputMethod<U> for Commander
where
    U: Ui,
{
    type Widget = CommandLine<U>;

    fn send_key(
        &mut self,
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        _context: Context<U>,
    ) {
        let mut helper = EditHelper::new(widget, area, &mut self.cursors);

        match key {
            key!(KeyCode::Backspace) => {
                helper.move_main(|m| {
                    m.set_anchor();
                    m.move_hor(-1);
                });
                helper.edit_on_main(|e| e.replace(""));
                helper.move_main(|m| {
                    m.unset_anchor();
                });
            }
            key!(KeyCode::Delete) => {
                helper.move_main(|m| {
                    m.set_anchor();
                    m.move_hor(1);
                });
                helper.edit_on_main(|e| {
                    e.replace("");
                });
                helper.move_main(|m| {
                    m.unset_anchor();
                });
            }

            key!(KeyCode::Char(char)) => {
                helper.edit_on_main(|e| e.insert(char));
                helper.move_main(|m| m.move_hor(1));
            }
            key!(KeyCode::Char(char), KeyMod::SHIFT) => {
                helper.edit_on_main(|e| e.insert(char));
                helper.move_main(|m| m.move_hor(1));
            }

            key!(KeyCode::Left) => {
                helper.move_main(|m| {
                    m.unset_anchor();
                    m.move_hor(-1)
                });
            }
            key!(KeyCode::Right) => {
                helper.move_main(|m| {
                    m.unset_anchor();
                    m.move_hor(1)
                });
            }

            key!(KeyCode::Esc) => {
                helper.move_main(|m| {
                    m.move_hor(isize::MIN);
                    m.set_anchor();
                    m.move_hor(isize::MAX);
                });
                helper.edit_on_main(|e| e.replace(""));
                self.cursors = Cursors::new_exclusive();
                crate::return_to_file();
            }
            key!(KeyCode::Enter) => {
                self.cursors = Cursors::new_exclusive();
                crate::return_to_file();
            }
            _ => {}
        }
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}
