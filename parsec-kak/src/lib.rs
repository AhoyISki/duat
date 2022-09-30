use crossterm::event::{KeyCode, KeyEvent};
use parsec_core::{input::EditingScheme, layout::FileWidget};

#[derive(Clone, PartialEq)]
pub enum Mode {
    Insert,
    Normal,
    Goto,
    View,
}

pub struct Editor {
    cur_mode: Mode,
}

impl Editor {
    pub fn new() -> Self {
        Editor { cur_mode: Mode::Insert }
    }
}

impl EditingScheme for Editor {
    type Mode = Mode;

    fn cur_mode(&self) -> Self::Mode {
        self.cur_mode.clone()
    }

    fn process_key<U>(&mut self, key: &KeyEvent, file: &mut parsec_core::layout::FileWidget<U>)
    where
        U: parsec_core::ui::Ui,
    {

        if let KeyCode::Char(ch) = key.code {
            match self.cur_mode {
                Mode::Insert => {
                    let mut cursor_list = file.cursor_list();
                    let mut cursors = cursor_list.cursors.write();
                    cursors.iter_mut().for_each(|mut c| {
                        file.splice(&mut c, ch);
                        c.move_hor(1, file);
                    });
                }
                _ => {}
            }
        }
    }

    fn send_remapped_keys(&self) -> bool {
        matches!(self.cur_mode, Mode::Insert)
    }
}
