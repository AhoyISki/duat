use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use parsec_core::{
    input::InputScheme,
    layout::file_widget::{FileEditor, FileWidget},
    ui::{Direction, Ui},
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

    fn match_insert<U>(
        &mut self, key: &KeyEvent, mut file_editor: parsec_core::layout::file_widget::FileEditor,
        file: &mut FileWidget<U>,
    ) where
        U: Ui,
    {
        match key {
            KeyEvent { code: KeyCode::Char(ch), .. } => {
                file_editor.edit_on_each_cursor(|mut c| {
                    file.insert(&mut c, ch);
                });
                file_editor.move_each_cursor(|mut c| {
                    c.move_hor(1, &file);
                });
            }
            KeyEvent { code: KeyCode::Enter, .. } => {
                file_editor.edit_on_each_cursor(|mut c| {
                    file.insert(&mut c, '\n');
                });
                file_editor.move_each_cursor(|mut c| {
                    c.move_hor(1, &file);
                });
            }
            KeyEvent { code: KeyCode::Backspace, .. } => {
                let mut anchors = Vec::with_capacity(file_editor.cursors_len());
                file_editor.move_each_cursor(|mut cursor| {
                    let caret = cursor.caret();
                    anchors.push(cursor.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                    cursor.set_anchor();
                    cursor.move_hor(-1, &file);
                });
                let mut anchors = anchors.into_iter().cycle();
                file_editor.edit_on_each_cursor(|mut cursor| {
                    file.replace(&mut cursor, "");
                    if let Some(Some((anchor, true))) = anchors.next() {
                        cursor.calibrate_pos(anchor);
                    }
                });
                file_editor.move_each_cursor(|mut cursor| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        cursor.set_anchor();
                        cursor.move_to(anchor, &file);
                        cursor.switch_ends()
                    } else {
                        cursor.unset_anchor();
                    }
                });
            }
            KeyEvent { code: KeyCode::Delete, .. } => {
                let mut anchors = Vec::with_capacity(file_editor.cursors_len());
                file_editor.move_each_cursor(|mut cursor| {
                    let caret = cursor.caret();
                    anchors.push(cursor.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                    cursor.set_anchor();
                    cursor.move_hor(1, &file);
                });
                let mut anchors = anchors.into_iter().cycle();
                file_editor.edit_on_each_cursor(|mut cursor| {
                    file.replace(&mut cursor, "");
                    if let Some(Some((anchor, true))) = anchors.next() {
                        cursor.calibrate_pos(anchor);
                    }
                });
                file_editor.move_each_cursor(|mut cursor| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        cursor.set_anchor();
                        cursor.move_to(anchor, &file);
                        cursor.switch_ends()
                    } else {
                        cursor.unset_anchor();
                    }
                });
            }
            KeyEvent { code: KeyCode::Left, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut file_editor, Direction::Left, 1, file);
            }
            KeyEvent { code: KeyCode::Right, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut file_editor, Direction::Right, 1, file);
            }
            KeyEvent { code: KeyCode::Up, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut file_editor, Direction::Top, 1, file);
            }
            KeyEvent { code: KeyCode::Down, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut file_editor, Direction::Bottom, 1, file);
            }
            KeyEvent { code: KeyCode::Left, .. } => {
                move_each(&mut file_editor, Direction::Left, 1, file);
            }
            KeyEvent { code: KeyCode::Right, .. } => {
                move_each(&mut file_editor, Direction::Right, 1, file);
            }
            KeyEvent { code: KeyCode::Up, .. } => {
                move_each(&mut file_editor, Direction::Top, 1, file);
            }
            KeyEvent { code: KeyCode::Down, .. } => {
                move_each(&mut file_editor, Direction::Bottom, 1, file);
            }
            KeyEvent { code: KeyCode::Tab, .. } => {
                file.new_moment();
                self.cur_mode = Mode::Normal;
            }
            _ => {}
        }
    }

    fn match_normal<U>(
        &mut self, key: &KeyEvent, mut file_editor: parsec_core::layout::file_widget::FileEditor,
        file: &mut FileWidget<U>,
    ) where
        U: Ui,
    {
        match key {
            ////////// Movement keys that get rid of selections.
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'h' => {
                move_each(&mut file_editor, Direction::Left, 1, file);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'j' => {
                move_each(&mut file_editor, Direction::Bottom, 1, file);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'k' => {
                move_each(&mut file_editor, Direction::Top, 1, file);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'l' => {
                move_each(&mut file_editor, Direction::Right, 1, file);
            }

            ////////// Movement keys that retain or create selections.
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'H' => {
                move_each_and_select(&mut file_editor, Direction::Left, 1, file);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'J' => {
                move_each_and_select(&mut file_editor, Direction::Bottom, 1, file);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'K' => {
                move_each_and_select(&mut file_editor, Direction::Top, 1, file);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'L' => {
                move_each_and_select(&mut file_editor, Direction::Right, 1, file);
            }

            ////////// Insertion keys.
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'i' => {
                file_editor.move_each_cursor(|mut cursor| cursor.set_caret_on_start());
                self.cur_mode = Mode::Insert;
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'a' => {
                file_editor.move_each_cursor(|mut cursor| cursor.set_caret_on_end());
                self.cur_mode = Mode::Insert;
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'c' => {
                file_editor.edit_on_each_cursor(|mut cursor| file.replace(&mut cursor, ""));
                file_editor.move_each_cursor(|mut cursor| cursor.unset_anchor());
                self.cur_mode = Mode::Insert;
            }

            ////////// History manipulation.
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'u' => file.undo(),
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'U' => file.redo(),
            _ => {}
        }
    }
}

impl InputScheme for Editor {
    fn process_key<U>(&mut self, key: &KeyEvent, file: &mut FileWidget<U>)
    where
        U: parsec_core::ui::Ui,
    {
        let mut file_editor = file.file_editor();

        match self.cur_mode {
            Mode::Insert => self.match_insert(key, file_editor, file),
            Mode::Normal => self.match_normal(key, file_editor, file),
            _ => {}
        }
    }

    fn send_remapped_keys(&self) -> bool {
        matches!(self.cur_mode, Mode::Insert)
    }
}

fn move_each<U>(
    file_editor: &mut FileEditor, direction: Direction, amount: usize, file: &FileWidget<U>,
) where
    U: Ui,
{
    file_editor.move_each_cursor(|mut c| {
        c.unset_anchor();
        match direction {
            Direction::Top => c.move_ver(-(amount as i32), file),
            Direction::Bottom => c.move_ver(amount as i32, file),
            Direction::Left => c.move_hor(-(amount as i32), file),
            Direction::Right => c.move_hor(amount as i32, file),
        }
    });
}

fn move_each_and_select<U>(
    file_editor: &mut FileEditor, direction: Direction, amount: usize, file: &FileWidget<U>,
) where
    U: Ui,
{
    file_editor.move_each_cursor(|mut c| {
        if !c.anchor_is_set() {
            c.set_anchor();
        }
        match direction {
            Direction::Top => c.move_ver(-(amount as i32), file),
            Direction::Bottom => c.move_ver(amount as i32, file),
            Direction::Left => c.move_hor(-(amount as i32), file),
            Direction::Right => c.move_hor(amount as i32, file),
        }
    });
}
