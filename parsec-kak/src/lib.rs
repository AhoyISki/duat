use std::fmt::Display;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use parsec_core::{
    config::{RoData, RwData},
    input::InputScheme,
    ui::{Direction, Ui},
    widgets::file_widget::{FileEditor, FileWidget},
};

#[derive(Clone, Copy, PartialEq)]
pub enum Mode {
    Insert,
    Normal,
    Goto,
    View,
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Insert => f.write_fmt(format_args!("insert")),
            Mode::Normal => f.write_fmt(format_args!("normal")),
            Mode::Goto => f.write_fmt(format_args!("goto")),
            Mode::View => f.write_fmt(format_args!("view")),
        }
    }
}

pub struct Editor {
    cur_mode: RwData<Mode>,
}

impl Editor {
    pub fn new() -> Self {
        Editor { cur_mode: RwData::new(Mode::Normal) }
    }

    fn match_insert<U>(&mut self, key: &KeyEvent, mut file_editor: FileEditor<U>)
    where
        U: Ui,
    {
        match key {
            KeyEvent { code: KeyCode::Char(ch), .. } => {
                file_editor.edit_on_each_cursor(|mut editor| {
                    editor.insert(ch);
                });
                file_editor.move_each_cursor(|mut mover| {
                    mover.move_hor(1);
                });
            }
            KeyEvent { code: KeyCode::Enter, .. } => {
                file_editor.edit_on_each_cursor(|mut editor| {
                    editor.insert('\n');
                });
                file_editor.move_each_cursor(|mut mover| {
                    mover.move_hor(1);
                });
            }
            KeyEvent { code: KeyCode::Backspace, .. } => {
                let mut anchors = Vec::with_capacity(file_editor.cursors_len());
                file_editor.move_each_cursor(|mut mover| {
                    let caret = mover.caret();
                    anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(-1);
                });
                let mut anchors = anchors.into_iter().cycle();
                file_editor.edit_on_each_cursor(|mut editor| {
                    editor.replace("");
                    if let Some(Some((anchor, true))) = anchors.next() {
                        editor.calibrate_pos(anchor);
                    }
                });
                file_editor.move_each_cursor(|mut mover| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        mover.set_anchor();
                        mover.move_to(anchor);
                        mover.switch_ends()
                    } else {
                        mover.unset_anchor();
                    }
                });
            }
            KeyEvent { code: KeyCode::Delete, .. } => {
                let mut anchors = Vec::with_capacity(file_editor.cursors_len());
                file_editor.move_each_cursor(|mut mover| {
                    let caret = mover.caret();
                    anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(1);
                });
                let mut anchors = anchors.into_iter().cycle();
                file_editor.edit_on_each_cursor(|mut editor| {
                    editor.replace("");
                    if let Some(Some((anchor, true))) = anchors.next() {
                        editor.calibrate_pos(anchor);
                    }
                });
                file_editor.move_each_cursor(|mut mover| {
                    if let Some(Some((anchor, _))) = anchors.next() {
                        mover.set_anchor();
                        mover.move_to(anchor);
                        mover.switch_ends()
                    } else {
                        mover.unset_anchor();
                    }
                });
            }
            KeyEvent { code: KeyCode::Left, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut file_editor, Direction::Left, 1);
            }
            KeyEvent { code: KeyCode::Right, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut file_editor, Direction::Right, 1);
            }
            KeyEvent { code: KeyCode::Up, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut file_editor, Direction::Top, 1);
            }
            KeyEvent { code: KeyCode::Down, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut file_editor, Direction::Bottom, 1);
            }
            KeyEvent { code: KeyCode::Left, .. } => {
                move_each(&mut file_editor, Direction::Left, 1);
            }
            KeyEvent { code: KeyCode::Right, .. } => {
                move_each(&mut file_editor, Direction::Right, 1);
            }
            KeyEvent { code: KeyCode::Up, .. } => {
                move_each(&mut file_editor, Direction::Top, 1);
            }
            KeyEvent { code: KeyCode::Down, .. } => {
                move_each(&mut file_editor, Direction::Bottom, 1);
            }
            KeyEvent { code: KeyCode::Tab, .. } => {
                file_editor.new_moment();
                *self.cur_mode.write() = Mode::Normal;
            }
            _ => {}
        }
    }

    fn match_normal<U>(
        &mut self, key: &KeyEvent, mut file_editor: FileEditor<U>,
    ) where
        U: Ui,
    {
        match key {
            ////////// Movement keys that get rid of selections.
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'h' => {
                move_each(&mut file_editor, Direction::Left, 1);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'j' => {
                move_each(&mut file_editor, Direction::Bottom, 1);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'k' => {
                move_each(&mut file_editor, Direction::Top, 1);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'l' => {
                move_each(&mut file_editor, Direction::Right, 1);
            }

            ////////// Movement keys that retain or create selections.
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'H' => {
                move_each_and_select(&mut file_editor, Direction::Left, 1);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'J' => {
                move_each_and_select(&mut file_editor, Direction::Bottom, 1);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'K' => {
                move_each_and_select(&mut file_editor, Direction::Top, 1);
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'L' => {
                move_each_and_select(&mut file_editor, Direction::Right, 1);
            }

            ////////// Insertion keys.
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'i' => {
                file_editor.move_each_cursor(|mut mover| mover.set_caret_on_start());
                *self.cur_mode.write() = Mode::Insert;
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'a' => {
                file_editor.move_each_cursor(|mut mover| mover.set_caret_on_end());
                *self.cur_mode.write() = Mode::Insert;
            }
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'c' => {
                file_editor.edit_on_each_cursor(|mut editor| editor.replace(""));
                file_editor.move_each_cursor(|mut mover| mover.unset_anchor());
                *self.cur_mode.write() = Mode::Insert;
            }

            ////////// History manipulation.
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'u' => file_editor.undo(),
            KeyEvent { code: KeyCode::Char(ch), .. } if *ch == 'U' => file_editor.redo(),
            _ => {}
        }
    }

    pub fn cur_mode(&self) -> RoData<Mode> {
        RoData::from(&self.cur_mode)
    }
}

impl InputScheme for Editor {
    fn process_key<U>(&mut self, key: &KeyEvent, file: &mut FileWidget<U>)
    where
        U: parsec_core::ui::Ui,
    {
        let file_editor = FileEditor::from(file);

        let cur_mode = *self.cur_mode.read();
        match cur_mode {
            Mode::Insert => self.match_insert(key, file_editor),
            Mode::Normal => self.match_normal(key, file_editor),
            _ => {}
        }
    }

    fn send_remapped_keys(&self) -> bool {
        matches!(*self.cur_mode.read(), Mode::Insert)
    }
}

fn move_each<U>(file_editor: &mut FileEditor<U>, direction: Direction, amount: usize)
where
    U: Ui,
{
    file_editor.move_each_cursor(|mut mover| {
        mover.unset_anchor();
        match direction {
            Direction::Top => mover.move_ver(-(amount as i32)),
            Direction::Bottom => mover.move_ver(amount as i32),
            Direction::Left => mover.move_hor(-(amount as i32)),
            Direction::Right => mover.move_hor(amount as i32),
        }
    });
}

fn move_each_and_select<U>(file_editor: &mut FileEditor<U>, direction: Direction, amount: usize)
where
    U: Ui,
{
    file_editor.move_each_cursor(|mut mover| {
        if !mover.anchor_is_set() {
            mover.set_anchor();
        }
        match direction {
            Direction::Top => mover.move_ver(-(amount as i32)),
            Direction::Bottom => mover.move_ver(amount as i32),
            Direction::Left => mover.move_hor(-(amount as i32)),
            Direction::Right => mover.move_hor(amount as i32),
        }
    });
}
