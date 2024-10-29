use super::{Cursors, EditHelper, KeyCode, KeyEvent, KeyMod, Mode, key};
use crate::{commands, data::RwData, text::Point, ui::Ui, widgets::CommandLine};

#[derive(Clone)]
pub struct Command;

impl<U: Ui> Mode<U> for Command {
    type Widget = CommandLine<U>;

    fn send_key(
        &mut self,
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        cursors: &mut Cursors,
    ) {
        cursors.make_excl();
        let mut helper = EditHelper::new(widget, area, cursors);

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
                    m.move_to(Point::default());
                    m.set_anchor();
                    m.move_to(m.len_point());
                });
                helper.edit_on_main(|e| e.replace(""));
                cursors.clear();
                commands::reset_mode();
            }
            key!(KeyCode::Enter) => {
                cursors.clear();
                commands::reset_mode();
            }
            _ => {}
        }
    }
}
