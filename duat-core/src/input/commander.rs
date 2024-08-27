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
        let mut editor = EditHelper::new(widget, area, &mut self.cursors);

        match key {
            key!(KeyCode::Backspace) => {
                editor.move_main(|mover| {
                    mover.set_anchor();
                    mover.move_hor(-1);
                });
                editor.edit_on_main(|editor| {
                    editor.replace("");
                });
                editor.move_main(|mover| {
                    mover.unset_anchor();
                });
            }
            key!(KeyCode::Delete) => {
                editor.move_main(|mover| {
                    mover.set_anchor();
                    mover.move_hor(1);
                });
                editor.edit_on_main(|editor| {
                    editor.replace("");
                });
                editor.move_main(|mover| {
                    mover.unset_anchor();
                });
            }

            key!(KeyCode::Char(char)) => {
                editor.edit_on_main(|editor| editor.insert(char));
                editor.move_main(|mover| mover.move_hor(1));
            }
            key!(KeyCode::Char(char), KeyMod::SHIFT) => {
                editor.edit_on_main(|editor| editor.insert(char));
                editor.move_main(|mover| mover.move_hor(1));
            }

            key!(KeyCode::Left) => {
                editor.move_main(|mover| {
                    mover.unset_anchor();
                    mover.move_hor(-1)
                });
            }
            key!(KeyCode::Right) => {
                editor.move_main(|mover| {
                    mover.unset_anchor();
                    mover.move_hor(1)
                });
            }

            key!(KeyCode::Esc) => {
                editor.move_main(|mover| {
                    mover.move_hor(isize::MIN);
                    mover.set_anchor();
                    mover.move_hor(isize::MAX);
                });
                editor.edit_on_main(|editor| editor.replace(""));
                self.cursors = Cursors::new_exclusive();
                context.commands.return_to_file().unwrap();
            }
            key!(KeyCode::Enter) => {
                self.cursors = Cursors::new_exclusive();
                context.commands.return_to_file().unwrap();
            }
            _ => {}
        }
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}
