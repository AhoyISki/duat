use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use parsec_core::input::EditingScheme;

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
        let mut file_editor = file.cursor_list();

        match self.cur_mode {
            Mode::Insert => match key {
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if ch == &'z' =>
                {
                    file.undo();
                }
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if ch == &'y' =>
                {
                    file.redo();
                }
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if ch == &'s' =>
                {
                    file_editor.on_last(|c| c.move_ver(1, &file));
                }
                KeyEvent { code: KeyCode::Char(ch), .. } => {
                    file_editor.edit_on_each_cursor(|mut c| {
                        file.edit(&mut c, ch);
                    });
                }
                KeyEvent { code: KeyCode::Enter, .. } => {
                    file_editor.edit_on_each_cursor(|mut c| {
                        file.edit(&mut c, '\n');
                    });
                }
                //KeyEvent { code: KeyCode::Backspace, .. } => {
                //    cursor_list.on_each(|c, l| {
                //        c.set_anchor();
                //        c.move_hor(-1, file);
                //        file.edit(&c, "", l);
                //        c.unset_anchor();
                //    });
                //}
                //KeyEvent { code: KeyCode::Delete, .. } => {
                //    cursor_list.on_each(|c, l| {
                //        c.set_anchor();
                //        c.move_hor(1, file);
                //        file.edit(&c, "", l);
                //        c.unset_anchor();
                //    });
                //}
                //KeyEvent { code: KeyCode::Left, .. } => {
                //    cursor_list.on_each(|c, _| {
                //        c.move_hor(-1, file);
                //    });
                //}
                //KeyEvent { code: KeyCode::Right, .. } => {
                //    cursor_list.on_each(|c, _| {
                //        c.move_hor(1, file);
                //    });
                //}
                //KeyEvent { code: KeyCode::Up, .. } => {
                //    cursor_list.on_each(|c, _| {
                //        c.move_ver(-1, file);
                //    });
                //}
                //KeyEvent { code: KeyCode::Down, .. } => {
                //    cursor_list.on_each(|c, _| {
                //        c.move_ver(1, file);
                //    });
                //}
                _ => {}
            },
            _ => {}
        }
    }

    fn send_remapped_keys(&self) -> bool {
        matches!(self.cur_mode, Mode::Insert)
    }
}
