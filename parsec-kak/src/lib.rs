use std::fmt::Display;

use crossterm::event::{KeyCode::*, KeyEvent, KeyModifiers};
use parsec_core::{
    data::RwData,
    history::History,
    input::{Cursors, InputMethod, MultiCursorEditor, WithHistory},
    ui::Ui,
    widgets::{CommandLine, File},
    Controler, ACTIVE_FILE,
};

#[derive(Default, Clone, Copy, PartialEq)]
pub enum Mode {
    Insert,
    #[default]
    Normal,
    GoTo,
    View,
    Command,
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Insert => f.write_str("insert"),
            Mode::Normal => f.write_str("normal"),
            Mode::GoTo => f.write_str("goto"),
            Mode::View => f.write_str("view"),
            Mode::Command => f.write_str("command"),
        }
    }
}

enum Side {
    Left,
    Right,
    Top,
    Bottom,
}

#[derive(Default, Clone)]
pub struct Editor {
    cursors: Cursors,
    history: History,
    mode: Mode,
    last_file: String,
}

impl Editor {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn mode(input: &Self) -> String {
        input.mode.to_string()
    }
}

impl InputMethod for Editor {
    type Widget = File;

    fn send_key<U>(
        &mut self,
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        controler: &Controler<U>,
    ) where
        U: Ui,
        Self: Sized,
    {
        let cursors = &mut self.cursors;
        let history = &mut self.history;
        let editor = MultiCursorEditor::with_history(widget, cursors, area, history);

        match self.mode {
            Mode::Insert => match_insert(editor, key, &mut self.mode),
            Mode::Normal => match_normal(editor, key, &mut self.mode, controler),
            Mode::Command => match_command(editor, key, &mut self.mode, controler),
            Mode::GoTo => {
                match_goto(editor, key, &mut self.last_file, controler);
                self.mode = Mode::Normal;
            }
            Mode::View => todo!(),
        }
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}

/// Commands that are available in `Mode::Insert`.
fn match_insert<U: Ui>(
    mut editor: MultiCursorEditor<WithHistory, U, File>,
    key: KeyEvent,
    mode: &mut Mode,
) {
    match key {
        KeyEvent { code: Char(ch), .. } => {
            editor.edit_on_each_cursor(|editor| {
                editor.insert(ch);
            });
            editor.move_each_cursor(|mover| {
                mover.move_hor(1);
            });
        }
        KeyEvent { code: Enter, .. } => {
            editor.edit_on_each_cursor(|editor| {
                editor.insert('\n');
            });
            editor.move_each_cursor(|mover| {
                mover.move_hor(1);
            });
        }
        KeyEvent {
            code: Backspace, ..
        } => {
            let mut anchors = Vec::with_capacity(editor.len_cursors());
            editor.move_each_cursor(|mover| {
                let caret = mover.caret();
                anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                mover.set_anchor();
                mover.move_hor(-1);
            });
            let mut anchors = anchors.into_iter().cycle();
            editor.edit_on_each_cursor(|editor| {
                editor.replace("");
            });
            editor.move_each_cursor(|mover| {
                if let Some(Some((anchor, _))) = anchors.next() {
                    mover.set_anchor();
                    mover.move_to(anchor);
                    mover.switch_ends()
                } else {
                    mover.unset_anchor();
                }
            });
        }
        KeyEvent { code: Delete, .. } => {
            let mut anchors = Vec::with_capacity(editor.len_cursors());
            editor.move_each_cursor(|mover| {
                let caret = mover.caret();
                anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                mover.set_anchor();
                mover.move_hor(1);
            });
            let mut anchors = anchors.into_iter().cycle();
            editor.edit_on_each_cursor(|editor| {
                editor.replace("");
            });
            editor.move_each_cursor(|mover| {
                if let Some(Some((anchor, _))) = anchors.next() {
                    mover.set_anchor();
                    mover.move_to(anchor);
                    mover.switch_ends()
                } else {
                    mover.unset_anchor();
                }
            });
        }
        KeyEvent {
            code: Left,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Left, 1);
        }
        KeyEvent {
            code: Right,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Right, 1);
        }
        KeyEvent {
            code: Up,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Top, 1);
        }
        KeyEvent {
            code: Down,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Bottom, 1);
        }
        KeyEvent { code: Left, .. } => move_each(editor, Side::Left, 1),
        KeyEvent { code: Right, .. } => move_each(editor, Side::Right, 1),
        KeyEvent { code: Up, .. } => move_each(editor, Side::Top, 1),
        KeyEvent { code: Down, .. } => move_each(editor, Side::Bottom, 1),
        KeyEvent { code: Esc, .. } => {
            editor.new_moment();
            *mode = Mode::Normal;
        }
        _ => {}
    }
}

/// Commands that are available in `Mode::Normal`.
fn match_normal<U: Ui>(
    mut editor: MultiCursorEditor<WithHistory, U, File>,
    key: KeyEvent,
    mode: &mut Mode,
    controler: &Controler<U>,
) {
    match key {
        ////////// SessionControl commands.
        KeyEvent {
            code: Char('c'),
            modifiers: KeyModifiers::CONTROL,
            ..
        } => {
            controler.quit();
        }

        ////////// Movement keys that retain or create selections.
        KeyEvent {
            code: Char('H') | Left,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Left, 1);
        }
        KeyEvent {
            code: Char('J') | Down,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Bottom, 1);
        }
        KeyEvent {
            code: Char('K') | Up,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Top, 1);
        }
        KeyEvent {
            code: Char('L') | Right,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Right, 1);
        }

        ////////// Movement keys that get rid of selections.
        KeyEvent {
            code: Char('h') | Left,
            ..
        } => {
            move_each(editor, Side::Left, 1);
        }
        KeyEvent {
            code: Char('j') | Down,
            ..
        } => {
            move_each(editor, Side::Bottom, 1);
        }
        KeyEvent {
            code: Char('k') | Up,
            ..
        } => {
            move_each(editor, Side::Top, 1);
        }
        KeyEvent {
            code: Char('l') | Right,
            ..
        } => {
            move_each(editor, Side::Right, 1);
        }

        ////////// Insertion keys.
        KeyEvent {
            code: Char('i'), ..
        } => {
            editor.move_each_cursor(|mover| mover.switch_ends());
            *mode = Mode::Insert;
        }
        KeyEvent {
            code: Char('a'), ..
        } => {
            editor.move_each_cursor(|mover| mover.set_caret_on_end());
            *mode = Mode::Insert;
        }
        KeyEvent {
            code: Char('c'), ..
        } => {
            editor.edit_on_each_cursor(|editor| editor.replace(""));
            editor.move_each_cursor(|mover| mover.unset_anchor());
            *mode = Mode::Insert;
        }

        ////////// Other mode changing keys.
        KeyEvent {
            code: Char(':'), ..
        } => {
            if controler.switch_to::<CommandLine>().is_ok() {
                *mode = Mode::Command;
            }
        }
        KeyEvent {
            code: Char('g'), ..
        } => *mode = Mode::GoTo,

        ////////// History manipulation.
        KeyEvent {
            code: Char('u'), ..
        } => editor.undo(),
        KeyEvent {
            code: Char('U'), ..
        } => editor.redo(),
        _ => {}
    }
}

/// Commands that are available in `Mode::Command`.
fn match_command<U: Ui>(
    mut editor: MultiCursorEditor<WithHistory, U, File>,
    key: KeyEvent,
    mode: &mut Mode,
    controler: &Controler<U>,
) {
    match key {
        KeyEvent { code: Enter, .. } => {
            if controler.return_to_file().is_ok() {
                *mode = Mode::Normal;
            }
        }
        KeyEvent {
            code: Backspace, ..
        } => {
            let mut anchors = Vec::with_capacity(editor.len_cursors());
            editor.move_each_cursor(|mover| {
                let caret = mover.caret();
                anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                mover.set_anchor();
                mover.move_hor(-1);
            });
            let mut anchors = anchors.into_iter().cycle();
            editor.edit_on_each_cursor(|editor| {
                editor.replace("");
            });
            editor.move_each_cursor(|mover| {
                if let Some(Some((anchor, _))) = anchors.next() {
                    mover.set_anchor();
                    mover.move_to(anchor);
                    mover.switch_ends()
                } else {
                    mover.unset_anchor();
                }
            });
        }
        KeyEvent { code: Delete, .. } => {
            let mut anchors = Vec::with_capacity(editor.len_cursors());
            editor.move_each_cursor(|mover| {
                let caret = mover.caret();
                anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                mover.set_anchor();
                mover.move_hor(1);
            });
            let mut anchors = anchors.into_iter().cycle();
            editor.edit_on_each_cursor(|editor| {
                editor.replace("");
            });
            editor.move_each_cursor(|mover| {
                if let Some(Some((anchor, _))) = anchors.next() {
                    mover.set_anchor();
                    mover.move_to(anchor);
                    mover.switch_ends()
                } else {
                    mover.unset_anchor();
                }
            });
        }
        KeyEvent { code: Char(ch), .. } => {
            editor.edit_on_main(|editor| editor.insert(ch));
            editor.move_main(|mover| mover.move_hor(1));
        }

        KeyEvent {
            code: Left,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Left, 1);
        }
        KeyEvent {
            code: Right,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Right, 1);
        }
        KeyEvent {
            code: Up,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Top, 1);
        }
        KeyEvent {
            code: Down,
            modifiers: KeyModifiers::SHIFT,
            ..
        } => {
            move_each_and_select(editor, Side::Bottom, 1);
        }
        KeyEvent { code: Left, .. } => {
            move_each(editor, Side::Left, 1);
        }
        KeyEvent { code: Right, .. } => {
            move_each(editor, Side::Right, 1);
        }
        KeyEvent { code: Up, .. } => {
            move_each(editor, Side::Top, 1);
        }
        KeyEvent { code: Down, .. } => {
            move_each(editor, Side::Bottom, 1);
        }

        KeyEvent { code: Esc, .. } => {
            editor.move_main(|mover| {
                mover.move_hor(isize::MIN);
                mover.set_anchor();
                mover.move_hor(isize::MAX);
            });

            editor.edit_on_main(|editor| editor.replace(""));

            if controler.return_to_file().is_ok() {
                *mode = Mode::Normal
            }
        }
        _ => {}
    }
}

/// Commands that are available in `Mode::GoTo`.
fn match_goto<U: Ui>(
    mut editor: MultiCursorEditor<WithHistory, U, File>,
    key: KeyEvent,
    last_file: &mut String,
    controler: &Controler<U>,
) {
    match key {
        KeyEvent {
            code: Char('a'), ..
        } => {
            if controler.switch_to_file(last_file.clone()).is_ok() {
                *last_file = ACTIVE_FILE.name().unwrap_or(String::from("*scratch file*"));
            }
        }
        KeyEvent {
            code: Char('j'), ..
        } => {
            editor.move_main(|mover| mover.move_ver(isize::MAX));
        }
        KeyEvent {
            code: Char('k'), ..
        } => {
            editor.move_main(|mover| mover.move_to_coords(0, 0));
        }
        KeyEvent {
            code: Char('n'), ..
        } => {
            if controler.next_file().is_ok() {
                *last_file = ACTIVE_FILE.name().unwrap_or(String::from("*scratch file*"));
            }
        }
        KeyEvent {
            code: Char('N'), ..
        } => {
            if controler.prev_file().is_ok() {
                *last_file = ACTIVE_FILE.name().unwrap_or(String::from("*scratch file*"));
            }
        }
        _ => {}
    }
}

fn move_each<U: Ui>(
    mut editor: MultiCursorEditor<WithHistory, U, File>,
    direction: Side,
    amount: usize,
) {
    editor.move_each_cursor(|mover| {
        mover.unset_anchor();
        match direction {
            Side::Top => mover.move_ver(-(amount as isize)),
            Side::Bottom => mover.move_ver(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize),
        }
    });
}

fn move_each_and_select<U: Ui>(
    mut editor: MultiCursorEditor<WithHistory, U, File>,
    direction: Side,
    amount: usize,
) {
    editor.move_each_cursor(|mover| {
        if !mover.anchor_is_set() {
            mover.set_anchor();
        }
        match direction {
            Side::Top => mover.move_ver(-(amount as isize)),
            Side::Bottom => mover.move_ver(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize),
        }
    });
}
