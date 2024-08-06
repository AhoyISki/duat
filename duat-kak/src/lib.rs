use std::fmt::Display;

use crossterm::event::{
    KeyCode::{*},
    KeyEvent, KeyModifiers as Mod,
};
use duat_core::{
    data::{Context, RwData},
    hooks::{self, Hookable},
    input::{key, Cursors, EditHelper, InputMethod},
    palette::{self, Form},
    text::{text, Text},
    ui::Ui,
    widgets::File,
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

/// Triggers whenever the mode changes
///
/// Arguments:
/// * The previous [`Mode`]
/// * The current [`Mode`]
pub struct OnModeChange {}

impl Hookable for OnModeChange {
    type Args<'args> = (Mode, Mode);
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
pub struct KeyMap {
    cursors: Cursors,
    mode: Mode,
    last_file: String,
}

impl KeyMap {
    pub fn new() -> Self {
        palette::set_weak_form("Mode", Form::new().green());
        Self::default()
    }

    pub fn mode(&self) -> String {
        self.mode.to_string()
    }

    pub fn mode_fmt(&self) -> Text {
        text!([Mode] { self.mode.to_string() })
    }
}

impl<U> InputMethod<U> for KeyMap
where
    U: Ui,
{
    type Widget = File;

    fn send_key(
        &mut self,
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        context: Context<U>,
    ) {
        let cursors = &mut self.cursors;
        let editor = EditHelper::new(widget, area, cursors);

        match self.mode {
            Mode::Insert => match_insert(editor, key, &mut self.mode),
            Mode::Normal => match_normal(editor, key, &mut self.mode, context),
            Mode::GoTo => {
                match_goto(editor, key, &mut self.last_file, context);
                self.mode = Mode::Normal;
                hooks::trigger::<OnModeChange>((Mode::GoTo, Mode::Normal));
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

    fn on_focus(&mut self, _area: &U::Area)
    where
        Self: Sized,
    {
        self.mode = Mode::Normal;
        hooks::trigger::<OnModeChange>((Mode::GoTo, Mode::Normal));
    }
}

/// Commands that are available in `Mode::Insert`.
fn match_insert<U: Ui>(mut editor: EditHelper<File, U>, key: KeyEvent, mode: &mut Mode) {
    match key {
        key!(Char(char)) => {
            editor.edit_on_each_cursor(|editor| editor.insert(char));
            editor.move_each_cursor(|mover| mover.move_hor(1));
        }
        key!(Char(char), Mod::SHIFT) => {
            editor.edit_on_each_cursor(|editor| editor.insert(char));
            editor.move_each_cursor(|mover| mover.move_hor(1));
        }
        key!(Enter) => {
            editor.edit_on_each_cursor(|editor| editor.insert('\n'));
            editor.move_each_cursor(|mover| mover.move_hor(1));
        }
        key!(Backspace) => {
            let mut anchors = Vec::with_capacity(editor.len_cursors());
            editor.move_each_cursor(|mover| {
                let caret = mover.caret();
                anchors.push(mover.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
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
                    mover.swap_ends()
                } else {
                    mover.unset_anchor();
                }
            });
        }
        key!(Delete) => {
            let mut anchors = Vec::with_capacity(editor.len_cursors());
            editor.move_each_cursor(|mover| {
                let caret = mover.caret();
                anchors.push(mover.unset_anchor().map(|anchor| (anchor, anchor >= caret)));
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
                    mover.swap_ends()
                } else {
                    mover.unset_anchor();
                }
            });
        }
        key!(Left, Mod::SHIFT) => select_and_move_each_wrapped(editor, Side::Left, 1),
        key!(Right, Mod::SHIFT) => select_and_move_each_wrapped(editor, Side::Right, 1),
        key!(Up, Mod::SHIFT) => select_and_move_each_wrapped(editor, Side::Top, 1),
        key!(Down, Mod::SHIFT) => select_and_move_each_wrapped(editor, Side::Bottom, 1),

        key!(Left) => move_each_wrapped(editor, Side::Left, 1),
        key!(Right) => move_each_wrapped(editor, Side::Right, 1),
        key!(Up) => move_each_wrapped(editor, Side::Top, 1),
        key!(Down) => move_each_wrapped(editor, Side::Bottom, 1),

        key!(Esc) => {
            editor.new_moment();
            *mode = Mode::Normal;
            hooks::trigger::<OnModeChange>((Mode::Insert, Mode::Normal))
        }
        _ => {}
    }
}

/// Commands that are available in `Mode::Normal`.
fn match_normal<U: Ui>(
    mut editor: EditHelper<File, U>,
    key: KeyEvent,
    mode: &mut Mode,
    context: Context<U>,
) {
    match key {
        ////////// Movement keys that retain or create selections.
        key!(Char('H'), Mod::SHIFT) => select_and_move_each(editor, Side::Left, 1),
        key!(Char('J'), Mod::SHIFT) => select_and_move_each(editor, Side::Bottom, 1),
        key!(Char('K'), Mod::SHIFT) => select_and_move_each(editor, Side::Top, 1),
        key!(Char('L'), Mod::SHIFT) => select_and_move_each(editor, Side::Right, 1),

        ////////// Movement keys that get rid of selections.
        key!(Char('h')) => move_each(editor, Side::Left, 1),
        key!(Char('j')) => move_each(editor, Side::Bottom, 1),
        key!(Char('k')) => move_each(editor, Side::Top, 1),
        key!(Char('l')) => move_each(editor, Side::Right, 1),

        ////////// Movement keys that retain or create selections.
        key!(Left, Mod::SHIFT) => select_and_move_each_wrapped(editor, Side::Left, 1),
        key!(Down, Mod::SHIFT) => select_and_move_each_wrapped(editor, Side::Bottom, 1),
        key!(Up, Mod::SHIFT) => select_and_move_each_wrapped(editor, Side::Top, 1),
        key!(Right, Mod::SHIFT) => select_and_move_each_wrapped(editor, Side::Right, 1),

        ////////// Movement keys that get rid of selections.
        key!(Left) => move_each_wrapped(editor, Side::Left, 1),
        key!(Down) => move_each_wrapped(editor, Side::Bottom, 1),
        key!(Up) => move_each_wrapped(editor, Side::Top, 1),
        key!(Right) => move_each_wrapped(editor, Side::Right, 1),

        ////////// Insertion keys.
        key!(Char('i')) => {
            editor.move_each_cursor(|mover| mover.swap_ends());
            *mode = Mode::Insert;
            hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
        }
        key!(Char('a')) => {
            editor.move_each_cursor(|mover| mover.set_caret_on_end());
            *mode = Mode::Insert;
            hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
        }
        key!(Char('c')) => {
            editor.edit_on_each_cursor(|editor| editor.replace(""));
            editor.move_each_cursor(|mover| mover.unset_anchor());
            *mode = Mode::Insert;
            hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Insert));
        }

        ////////// Other mode changing keys.
        key!(Char(':')) => {
            if context.commands.run("switch-to CommandLine<Ui>").is_ok() {
                context
                    .commands
                    .run("set-cmd-mode RunCommands<Ui>")
                    .unwrap();
                *mode = Mode::Command;
                hooks::trigger::<OnModeChange>((Mode::Normal, Mode::Command));
            }
        }
        key!(Char('g')) => {
            *mode = Mode::GoTo;
            hooks::trigger::<OnModeChange>((Mode::Normal, Mode::GoTo));
        }

        ////////// For now
        key!(Char('q')) => panic!("Quit on purpose"),

        ////////// History manipulation.
        key!(Char('u')) => editor.undo(),
        key!(Char('U'), Mod::SHIFT) => editor.redo(),
        _ => {}
    }
}

/// Commands that are available in `Mode::GoTo`.
fn match_goto<U: Ui>(
    mut editor: EditHelper<File, U>,
    key: KeyEvent,
    last_file: &mut String,
    context: Context<U>,
) {
    match key {
        key!(Char('a')) if context.commands.buffer(last_file.clone()).is_ok() => {
            *last_file = context.cur_file().unwrap().name();
        }
        key!(Char('a')) => {}
        key!(Char('j')) => editor.move_main(|mover| mover.move_ver(isize::MAX)),
        key!(Char('k')) => editor.move_main(|mover| mover.move_to_coords(0, 0)),
        key!(Char('n')) if context.commands.next_file().is_ok() => {
            *last_file = context.cur_file().unwrap().name()
        }
        key!(Char('n')) => {}
        key!(Char('N')) if context.commands.prev_file().is_ok() => {
            *last_file = context.cur_file().unwrap().name()
        }
        _ => {}
    }
}

fn move_each<U: Ui>(mut editor: EditHelper<File, U>, direction: Side, amount: usize) {
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

fn select_and_move_each<U: Ui>(mut editor: EditHelper<File, U>, direction: Side, amount: usize) {
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

fn move_each_wrapped<U: Ui>(mut editor: EditHelper<File, U>, direction: Side, amount: usize) {
    editor.move_each_cursor(|mover| {
        mover.unset_anchor();
        match direction {
            Side::Top => mover.move_ver_wrapped(-(amount as isize)),
            Side::Bottom => mover.move_ver_wrapped(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize),
        }
    });
}

fn select_and_move_each_wrapped<U: Ui>(
    mut editor: EditHelper<File, U>,
    direction: Side,
    amount: usize,
) {
    editor.move_each_cursor(|mover| {
        if !mover.anchor_is_set() {
            mover.set_anchor();
        }
        match direction {
            Side::Top => mover.move_ver_wrapped(-(amount as isize)),
            Side::Bottom => mover.move_ver_wrapped(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize),
        }
    });
}
