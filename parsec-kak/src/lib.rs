use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use parsec_core::{input::EditingScheme, layout::file_widget::FileWidget, FOR_TEST, log_info};

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

    fn process_key<U>(&mut self, key: &KeyEvent, file: &mut FileWidget<U>)
    where
        U: parsec_core::ui::Ui,
    {
        let mut file_editor = file.file_editor();

        match self.cur_mode {
            Mode::Insert => match key {
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if *ch == 'd' =>
                unsafe { FOR_TEST = !FOR_TEST },
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if *ch == 'n' =>
                {
                    file_editor.rotate_main_forward();
                }
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if *ch == 'z' =>
                {
                    file.undo();
                }
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if *ch == 'y' =>
                {
                    file.redo();
                }
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if *ch == 's' =>
                {
                    file_editor.clone_last();
                    file_editor.move_last(|c| c.move_ver(1, &file));
                }
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if *ch == 'a' =>
                {
                    file_editor.clone_last();
                    file_editor.move_last(|c| c.move_hor(1, &file));
                }
                KeyEvent { code: KeyCode::Char(ch), modifiers: KeyModifiers::CONTROL, .. }
                    if *ch == 't' =>
                {
                    file_editor.move_each_cursor(|mut c| {
                        c.set_anchor();
                        c.move_hor(3, &file);
                    });
                    file_editor.edit_on_each_cursor(|mut c| {
                        file.edit(&mut c, "xd");
                    });
                    file_editor.move_each_cursor(|mut c| {
                        c.unset_anchor();
                    });
                }
                KeyEvent { code: KeyCode::Char(ch), .. } => {
                    let mut index = 0;
                    let main_cursor = file_editor.main_cursor();
                    file_editor.edit_on_each_cursor(|mut c| {
                        if index == main_cursor {
                            file.edit(&mut c, ch.to_uppercase());
                        } else {
                            file.edit(&mut c, ch);
                        }
                        index += 1;
                    });
                    file_editor.move_each_cursor(|mut c| {
                        c.move_hor(1, &file);
                    });
                }
                KeyEvent { code: KeyCode::Enter, .. } => {
                    file_editor.edit_on_each_cursor(|mut c| {
                        file.edit(&mut c, '\n');
                    });
                }
                KeyEvent { code: KeyCode::Backspace, .. } => {
                    file_editor.move_each_cursor(|mut c| {
                        if !c.anchor_is_set() {
                            c.set_anchor();
                            c.move_hor(-1, &file);
                        }
                    });
                    file_editor.edit_on_each_cursor(|mut c| {
                        file.edit(&mut c, "");
                    });
                    file_editor.move_each_cursor(|mut c| {
                        c.unset_anchor();
                    });
                }
                KeyEvent { code: KeyCode::Delete, .. } => {
                    file_editor.move_each_cursor(|mut c| {
                        c.set_anchor();
                        c.move_hor(1, &file);
                    });
                    file_editor.edit_on_each_cursor(|mut c| {
                        file.edit(&mut c, "");
                    });
                    file_editor.move_each_cursor(|mut c| {
                        c.unset_anchor();
                    });
                }
                KeyEvent { code: KeyCode::Left, modifiers: KeyModifiers::SHIFT, .. } => {
                    file_editor.move_main(|c| c.move_hor(-1, &file));
                }
                KeyEvent { code: KeyCode::Right, modifiers: KeyModifiers::SHIFT, .. } => {
                    file_editor.move_main(|c| c.move_hor(1, &file));
                }
                KeyEvent { code: KeyCode::Up, modifiers: KeyModifiers::SHIFT, .. } => {
                    file_editor.move_main(|c| c.move_ver(-1, &file));
                }
                KeyEvent { code: KeyCode::Down, modifiers: KeyModifiers::SHIFT, .. } => {
                    file_editor.move_main(|c| c.move_ver(1, &file));
                }
                KeyEvent { code: KeyCode::Left, .. } => {
                    file_editor.move_each_cursor(|mut c| c.move_hor(-1, &file));
                }
                KeyEvent { code: KeyCode::Right, .. } => {
                    file_editor.move_each_cursor(|mut c| c.move_hor(1, &file));
                }
                KeyEvent { code: KeyCode::Up, .. } => {
                    file_editor.move_each_cursor(|mut c| c.move_ver(-1, &file));
                }
                KeyEvent { code: KeyCode::Down, .. } => {
                    file_editor.move_each_cursor(|mut c| c.move_ver(1, &file));
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
