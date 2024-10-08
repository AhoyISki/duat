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
        context: Context<U>,
    ) {
        let mut helper = EditHelper::new(widget, area, &mut self.cursors);

        match key {
            key!(KeyCode::Backspace) => {
                helper.move_main(|mover| {
                    mover.set_anchor();
                    mover.move_hor(-1);
                });
                helper.edit_on_main(|editor| {
                    editor.replace("");
                });
                helper.move_main(|mover| {
                    mover.unset_anchor();
                });
            }
            key!(KeyCode::Delete) => {
                helper.move_main(|mover| {
                    mover.set_anchor();
                    mover.move_hor(1);
                });
                helper.edit_on_main(|editor| {
                    editor.replace("");
                });
                helper.move_main(|mover| {
                    mover.unset_anchor();
                });
            }

            key!(KeyCode::Char(char)) => {
                helper.edit_on_main(|editor| editor.insert(char));
                helper.move_main(|mover| mover.move_hor(1));
            }
            key!(KeyCode::Char(char), KeyMod::SHIFT) => {
                helper.edit_on_main(|editor| editor.insert(char));
                helper.move_main(|mover| mover.move_hor(1));
            }

            key!(KeyCode::Left) => {
                helper.move_main(|mover| {
                    mover.unset_anchor();
                    mover.move_hor(-1)
                });
            }
            key!(KeyCode::Right) => {
                helper.move_main(|mover| {
                    mover.unset_anchor();
                    mover.move_hor(1)
                });
            }

            key!(KeyCode::Esc) => {
                helper.move_main(|mover| {
                    mover.move_hor(isize::MIN);
                    mover.set_anchor();
                    mover.move_hor(isize::MAX);
                });
                helper.edit_on_main(|editor| editor.replace(""));
                drop(helper);
                self.cursors = Cursors::new_exclusive();
                context.run_cmd("return-to-file").unwrap();
            }
            key!(KeyCode::Enter) => {
                drop(helper);
                self.cursors = Cursors::new_exclusive();
                context.run_cmd("return-to-file").unwrap();
            }
            _ => {}
        }
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}
