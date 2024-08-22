use super::{
    key::{key, Code, Event, Mod},
    Cursors, EditHelper, InputMethod,
};
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
        key: Event,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        context: Context<U>,
    ) {
        let mut editor = EditHelper::new(widget, area, &mut self.cursors);

        match key {
            key!(Code::Backspace) => {
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
            key!(Code::Delete) => {
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

            key!(Code::Char(char)) => {
                editor.edit_on_main(|editor| editor.insert(char));
                editor.move_main(|mover| mover.move_hor(1));
            }
            key!(Code::Char(char), Mod::SHIFT) => {
                editor.edit_on_main(|editor| editor.insert(char));
                editor.move_main(|mover| mover.move_hor(1));
            }

            key!(Code::Left) => {
                editor.move_main(|mover| {
                    mover.unset_anchor();
                    mover.move_hor(-1)
                });
            }
            key!(Code::Right) => {
                editor.move_main(|mover| {
                    mover.unset_anchor();
                    mover.move_hor(1)
                });
            }

            key!(Code::Esc) => {
                editor.move_main(|mover| {
                    mover.move_hor(isize::MIN);
                    mover.set_anchor();
                    mover.move_hor(isize::MAX);
                });
                editor.edit_on_main(|editor| editor.replace(""));
                self.cursors = Cursors::new_exclusive();
                context.commands.return_to_file().unwrap();
            }
            key!(Code::Enter) => {
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
