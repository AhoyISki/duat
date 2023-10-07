use std::fmt::Display;

use crossterm::event::{
    KeyCode::{self, *},
    KeyEvent, KeyModifiers,
};
use parsec_core::{
    data::RwData,
    history::History,
    input::{key, Cursors, InputMethod, MultiCursorEditor, WithHistory},
    ui::Area,
    widgets::{CommandLine, File},
    CURRENT_FILE,
    commands
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

    fn send_key(&mut self, key: KeyEvent, widget: &RwData<Self::Widget>, area: &impl Area) {
        let cursors = &mut self.cursors;
        let history = &mut self.history;
        let editor = MultiCursorEditor::with_history(widget, cursors, area, history);

        match self.mode {
            Mode::Insert => match_insert(editor, key, &mut self.mode),
            Mode::Normal => match_normal(editor, key, &mut self.mode),
            Mode::GoTo => {
                match_goto(editor, key, &mut self.last_file);
                self.mode = Mode::Normal;
            }
            Mode::View => todo!(),
            Mode::Command => {
                unreachable!("The editor is not supposed to be focused if this is the current mode")
            }
        }
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }

    fn on_focus(&mut self, _area: &impl Area)
    where
        Self: Sized,
    {
        self.mode = Mode::Normal
    }
}

/// Commands that are available in `Mode::Insert`.
fn match_insert(
    mut editor: MultiCursorEditor<WithHistory, File, impl Area>,
    key: KeyEvent,
    mode: &mut Mode,
) {
    match key {
        key!(KeyCode::Char(ch)) => {
            editor.edit_on_each_cursor(|editor| {
                editor.insert(ch);
            });
            editor.move_each_cursor(|mover| {
                mover.move_hor(1);
            });
        }
        key!(KeyCode::Char(ch), KeyModifiers::SHIFT) => {
            editor.edit_on_each_cursor(|editor| {
                editor.insert(ch);
            });
            editor.move_each_cursor(|mover| {
                mover.move_hor(1);
            });
        }
        key!(KeyCode::Enter) => {
            editor.edit_on_each_cursor(|editor| {
                editor.insert('\n');
            });
            editor.move_each_cursor(|mover| {
                mover.move_hor(1);
            });
        }
        key!(KeyCode::Backspace) => {
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
        key!(KeyCode::Delete) => {
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
        key!(KeyCode::Left, KeyModifiers::SHIFT) => {
            move_each_and_select(editor, Side::Left, 1);
        }
        key!(KeyCode::Right, KeyModifiers::SHIFT) => {
            move_each_and_select(editor, Side::Right, 1);
        }
        key!(KeyCode::Up, KeyModifiers::SHIFT) => {
            move_each_and_select(editor, Side::Top, 1);
        }
        key!(KeyCode::Down, KeyModifiers::SHIFT) => {
            move_each_and_select(editor, Side::Bottom, 1);
        }
        key!(KeyCode::Left) => move_each(editor, Side::Left, 1),
        key!(KeyCode::Right) => move_each(editor, Side::Right, 1),
        key!(KeyCode::Up) => move_each(editor, Side::Top, 1),
        key!(KeyCode::Down) => move_each(editor, Side::Bottom, 1),
        key!(KeyCode::Esc) => {
            editor.new_moment();
            *mode = Mode::Normal;
        }
        _ => {}
    }
}

/// Commands that are available in `Mode::Normal`.
fn match_normal(
    mut editor: MultiCursorEditor<WithHistory, File, impl Area>,
    key: KeyEvent,
    mode: &mut Mode,
) {
    match key {
        ////////// SessionControl commands.
        key!(KeyCode::Char('c'), KeyModifiers::CONTROL) => {
            commands::quit();
        }

        ////////// Movement keys that retain or create selections.
        key!(KeyCode::Char('H') | Left, KeyModifiers::SHIFT) => {
            move_each_and_select(editor, Side::Left, 1);
        }
        key!(KeyCode::Char('J') | Down, KeyModifiers::SHIFT) => {
            move_each_and_select(editor, Side::Bottom, 1);
        }
        key!(KeyCode::Char('K') | Up, KeyModifiers::SHIFT) => {
            move_each_and_select(editor, Side::Top, 1);
        }
        key!(KeyCode::Char('L') | Right, KeyModifiers::SHIFT) => {
            move_each_and_select(editor, Side::Right, 1);
        }

        ////////// Movement keys that get rid of selections.
        key!(KeyCode::Char('h') | Left) => {
            move_each(editor, Side::Left, 1);
        }
        key!(KeyCode::Char('j') | Down) => {
            move_each(editor, Side::Bottom, 1);
        }
        key!(KeyCode::Char('k') | Up) => {
            move_each(editor, Side::Top, 1);
        }
        key!(KeyCode::Char('l') | Right) => {
            move_each(editor, Side::Right, 1);
        }

        ////////// Insertion keys.
        key!(KeyCode::Char('i')) => {
            editor.move_each_cursor(|mover| mover.switch_ends());
            *mode = Mode::Insert;
        }
        key!(KeyCode::Char('a')) => {
            editor.move_each_cursor(|mover| mover.set_caret_on_end());
            *mode = Mode::Insert;
        }
        key!(KeyCode::Char('c')) => {
            editor.edit_on_each_cursor(|editor| editor.replace(""));
            editor.move_each_cursor(|mover| mover.unset_anchor());
            *mode = Mode::Insert;
        }

        ////////// Other mode changing keys.
        key!(KeyCode::Char(':')) => {
            if commands::switch_to::<CommandLine>().is_ok() {
                *mode = Mode::Command;
            }
        }
        key!(KeyCode::Char('g')) => *mode = Mode::GoTo,

        ////////// History manipulation.
        key!(KeyCode::Char('u')) => editor.undo(),
        key!(KeyCode::Char('U')) => editor.redo(),
        _ => {}
    }
}

/// Commands that are available in `Mode::GoTo`.
fn match_goto(
    mut editor: MultiCursorEditor<WithHistory, File, impl Area>,
    key: KeyEvent,
    last_file: &mut String,
) {
    match key {
        key!(KeyCode::Char('a')) => {
            if commands::buffer(last_file.clone()).is_ok() {
                *last_file = CURRENT_FILE
                    .name()
                    .unwrap_or(String::from("*scratch file*"));
            }
        }
        key!(KeyCode::Char('j')) => {
            editor.move_main(|mover| mover.move_ver(isize::MAX));
        }
        key!(KeyCode::Char('k')) => {
            editor.move_main(|mover| mover.move_to_coords(0, 0));
        }
        key!(KeyCode::Char('n')) => {
            if commands::next_file().is_ok() {
                *last_file = CURRENT_FILE
                    .name()
                    .unwrap_or(String::from("*scratch file*"));
            }
        }
        key!(KeyCode::Char('N')) => {
            if commands::prev_file().is_ok() {
                *last_file = CURRENT_FILE
                    .name()
                    .unwrap_or(String::from("*scratch file*"));
            }
        }
        _ => {}
    }
}

fn move_each(
    mut editor: MultiCursorEditor<WithHistory, File, impl Area>,
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

fn move_each_and_select(
    mut editor: MultiCursorEditor<WithHistory, File, impl Area>,
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
