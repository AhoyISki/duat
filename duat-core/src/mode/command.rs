use super::{EditHelper, KeyCode, KeyEvent, Mode, key};
use crate::{text::Point, ui::Ui, widgets::CmdLine};

#[derive(Clone)]
pub struct Command;

impl<U: Ui> Mode<U> for Command {
    type Widget = CmdLine<U>;

    fn send_key(&mut self, key: KeyEvent, widget: &mut Self::Widget, area: &U::Area) {
        let mut helper = EditHelper::new(widget, area);
        helper.cursors_mut().make_excl();

        match key {
            key!(KeyCode::Backspace) => {
                helper.move_main(|mut m| {
                    m.set_anchor();
                    m.move_hor(-1);
                });
                helper.edit_main(|e| e.replace(""));
                helper.move_main(|mut m| {
                    m.unset_anchor();
                });
            }
            key!(KeyCode::Delete) => {
                helper.move_main(|mut m| {
                    m.set_anchor();
                    m.move_hor(1);
                });
                helper.edit_main(|e| {
                    e.replace("");
                });
                helper.move_main(|mut m| {
                    m.unset_anchor();
                });
            }

            key!(KeyCode::Char(char)) => {
                helper.edit_main(|e| e.insert(char));
                helper.move_main(|mut m| m.move_hor(1));
            }
            key!(KeyCode::Left) => {
                helper.move_main(|mut m| {
                    m.unset_anchor();
                    m.move_hor(-1)
                });
            }
            key!(KeyCode::Right) => {
                helper.move_main(|mut m| {
                    m.unset_anchor();
                    m.move_hor(1)
                });
            }

            key!(KeyCode::Esc) => {
                let p = helper.text().len();
                helper.move_main(|mut m| {
                    m.move_to(Point::default());
                    m.set_anchor();
                    m.move_to(p);
                });
                helper.edit_main(|e| e.replace(""));
                helper.cursors_mut().clear();
                super::reset();
            }
            key!(KeyCode::Enter) => {
                helper.cursors_mut().clear();
                super::reset();
            }
            _ => {}
        }
    }
}
