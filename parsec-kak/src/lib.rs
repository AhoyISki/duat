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
        let mut cursor_list = file.cursor_list();
        let mut cursors = cursor_list.cursors.write();

        match self.cur_mode {
            Mode::Insert => match key.code {
                KeyCode::Char(ch) => {
                    cursors.iter_mut().for_each(|mut c| {
                        file.splice(&mut c, ch);
                        c.move_hor(1, file);
                    });
                }
                KeyCode::Enter => {
                    cursors.iter_mut().for_each(|mut c| {
                        file.splice(&mut c, '\n');
                        c.move_hor(1, file);
                    });
                }
                KeyCode::Backspace => {
                    cursors.iter_mut().for_each(|mut c| {
                        c.set_anchor();
                        c.move_hor(-1, file);
                        file.splice(&mut c, "");
                        c.unset_anchor();
                    });
                }
                KeyCode::Delete => {
                    cursors.iter_mut().for_each(|mut c| {
                        c.set_anchor();
                        c.move_hor(1, file);
                        file.splice(&mut c, "");
                        c.unset_anchor();
                    });
                }
                KeyCode::Left => {
                    cursors.iter_mut().for_each(|c| {
                        c.move_hor(-1, file);
                    });
                }
                KeyCode::Right => {
                    cursors.iter_mut().for_each(|c| {
                        c.move_hor(1, file);
                    });
                }
                KeyCode::Up => {
                    cursors.iter_mut().for_each(|c| {
                        c.move_ver(-1, file);
                    });
                }
                KeyCode::Down => {
                    cursors.iter_mut().for_each(|c| {
                        c.move_ver(1, file);
                    });
                }
                _ => {}
            },
            _ => {}
        }
    }

    fn send_remapped_keys(&self) -> bool {
        matches!(self.cur_mode, Mode::Insert)
    }
}
