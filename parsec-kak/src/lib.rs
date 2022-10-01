use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use parsec_core::{
    action::Splice, cursor::TextCursor, input::EditingScheme, layout::FileWidget, ui::Ui,
};

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
        let mut splices = Vec::new();

        match self.cur_mode {
            Mode::Insert => match key {
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if ch == &'z' =>
                {
                    drop(cursors);
                    file.undo();
                }
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if ch == &'y' =>
                {
                    drop(cursors);
                    file.redo();
                }
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if ch == &'s' =>
                {
                    let mut cursor = cursors.last().unwrap().clone();
                    cursor.move_ver(1, file);
                    cursors.push(cursor);
                }
                KeyEvent { code: KeyCode::Char(ch), .. } => {
                    cursors.iter_mut().for_each(|c| {
                        correct_cursor(c, &splices);
                        splices.push(file.splice(c.clone(), ch));
                        c.move_hor(1, file);
                    });
                }
                KeyEvent { code: KeyCode::Enter, .. } => {
                    cursors.iter_mut().for_each(|c| {
                        c.move_hor(1, file);
                    });
                }
                KeyEvent { code: KeyCode::Backspace, .. } => {
                    cursors.iter_mut().for_each(|c| {
                        correct_cursor(c, &splices);
                        c.set_anchor();
                        c.move_hor(-1, file);
                        splices.push(file.splice(c.clone(), ""));
                        c.unset_anchor();
                    });
                }
                KeyEvent { code: KeyCode::Delete, .. } => {
                    cursors.iter_mut().for_each(|c| {
                        correct_cursor(c, &splices);
                        c.set_anchor();
                        c.move_hor(1, file);
                        splices.push(file.splice(c.clone(), ""));
                        c.unset_anchor();
                    });
                }
                KeyEvent { code: KeyCode::Left, .. } => {
                    cursors.iter_mut().for_each(|c| {
                        c.move_hor(-1, file);
                    });
                }
                KeyEvent { code: KeyCode::Right, .. } => {
                    cursors.iter_mut().for_each(|c| {
                        c.move_hor(1, file);
                    });
                }
                KeyEvent { code: KeyCode::Up, .. } => {
                    cursors.iter_mut().for_each(|c| {
                        c.move_ver(-1, file);
                    });
                }
                KeyEvent { code: KeyCode::Down, .. } => {
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

fn correct_cursor(cursor: &mut TextCursor, splices: &Vec<Splice>) {
    for splice in splices.iter() {
        cursor.calibrate(*splice);
    }
}
