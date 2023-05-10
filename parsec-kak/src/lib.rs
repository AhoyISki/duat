use std::fmt::Display;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use parsec_core::{
    data::{RoData, RwData},
    input::InputScheme,
    ui::{Side, Ui},
    widgets::{ActionableWidget, WidgetActor, CommandLine},
    Controls,
};

#[derive(Default, Clone, Copy, PartialEq)]
pub enum Mode {
    Insert,
    #[default]
    Normal,
    GoTo,
    View,
    Command
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Insert => f.write_fmt(format_args!("insert")),
            Mode::Normal => f.write_fmt(format_args!("normal")),
            Mode::GoTo => f.write_fmt(format_args!("goto")),
            Mode::View => f.write_fmt(format_args!("view")),
            Mode::Command => f.write_fmt(format_args!("command"))
        }
    }
}

#[derive(Default)]
pub struct Editor {
    cur_mode: RwData<Mode>,
    last_file: String
}

impl Editor {
    /// Commands that are available in `Mode::Insert`.
    fn match_insert<U, AW>(&mut self, key: &KeyEvent, mut actor: WidgetActor<U, AW>)
    where
        U: Ui,
        AW: ActionableWidget<U> + ?Sized
    {
        match key {
            KeyEvent {
                code: KeyCode::Char(ch),
                ..
            } => {
                actor.edit_on_each_cursor(|mut editor| {
                    editor.insert(ch);
                });
                actor.move_each_cursor(|mut mover| {
                    mover.move_hor(1);
                });
            }
            KeyEvent {
                code: KeyCode::Enter,
                ..
            } => {
                actor.edit_on_each_cursor(|mut editor| {
                    editor.insert('\n');
                });
                actor.move_each_cursor(|mut mover| {
                    mover.move_hor(1);
                });
            }
            KeyEvent {
                code: KeyCode::Backspace,
                ..
            } => {
                let mut anchors = Vec::with_capacity(actor.cursors_len());
                actor.move_each_cursor(|mut mover| {
                    let caret = mover.caret();
                    anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(-1);
                });
                let mut anchors = anchors.into_iter().cycle();
                actor.edit_on_each_cursor(|mut editor| {
                    editor.replace("");
                });
                actor.move_each_cursor(|mut mover| {
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
                code: KeyCode::Delete,
                ..
            } => {
                let mut anchors = Vec::with_capacity(actor.cursors_len());
                actor.move_each_cursor(|mut mover| {
                    let caret = mover.caret();
                    anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(1);
                });
                let mut anchors = anchors.into_iter().cycle();
                actor.edit_on_each_cursor(|mut editor| {
                    editor.replace("");
                });
                actor.move_each_cursor(|mut mover| {
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
                code: KeyCode::Left,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Left, 1);
            }
            KeyEvent {
                code: KeyCode::Right,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Right, 1);
            }
            KeyEvent {
                code: KeyCode::Up,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Top, 1);
            }
            KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Bottom, 1);
            }
            KeyEvent {
                code: KeyCode::Left,
                ..
            } => {
                move_each(&mut actor, Side::Left, 1);
            }
            KeyEvent {
                code: KeyCode::Right,
                ..
            } => {
                move_each(&mut actor, Side::Right, 1);
            }
            KeyEvent {
                code: KeyCode::Up, ..
            } => {
                move_each(&mut actor, Side::Top, 1);
            }
            KeyEvent {
                code: KeyCode::Down,
                ..
            } => {
                move_each(&mut actor, Side::Bottom, 1);
            }
            KeyEvent {
                code: KeyCode::Esc, ..
            } => {
                actor.new_moment();
                *self.cur_mode.write() = Mode::Normal;
            }
            _ => {}
        }
    }

    /// Commands that are available in `Mode::Normal`.
    fn match_normal<U, AW>(
        &mut self, key: &KeyEvent, mut actor: WidgetActor<U, AW>, controls: &mut Controls<U>
    ) where
        U: Ui,
        AW: ActionableWidget<U> + ?Sized
    {
        match key {
            ////////// SessionControl commands.
            KeyEvent {
                code: KeyCode::Char('c'),
                modifiers: KeyModifiers::CONTROL,
                ..
            } => {
                controls.quit();
            }

            ////////// Movement keys that retain or create selections.
            KeyEvent {
                code: KeyCode::Char('H') | KeyCode::Left,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Left, 1);
            }
            KeyEvent {
                code: KeyCode::Char('J') | KeyCode::Down,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Bottom, 1);
            }
            KeyEvent {
                code: KeyCode::Char('K') | KeyCode::Up,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Top, 1);
            }
            KeyEvent {
                code: KeyCode::Char('L') | KeyCode::Right,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Right, 1);
            }

            ////////// Movement keys that get rid of selections.
            KeyEvent {
                code: KeyCode::Char('h') | KeyCode::Left,
                ..
            } => {
                move_each(&mut actor, Side::Left, 1);
            }
            KeyEvent {
                code: KeyCode::Char('j') | KeyCode::Down,
                ..
            } => {
                move_each(&mut actor, Side::Bottom, 1);
            }
            KeyEvent {
                code: KeyCode::Char('k') | KeyCode::Up,
                ..
            } => {
                move_each(&mut actor, Side::Top, 1);
            }
            KeyEvent {
                code: KeyCode::Char('l') | KeyCode::Right,
                ..
            } => {
                move_each(&mut actor, Side::Right, 1);
            }

            ////////// Insertion keys.
            KeyEvent {
                code: KeyCode::Char('i'),
                ..
            } => {
                actor.move_each_cursor(|mut mover| mover.switch_ends());
                *self.cur_mode.write() = Mode::Insert;
            }
            KeyEvent {
                code: KeyCode::Char('a'),
                ..
            } => {
                actor.move_each_cursor(|mut mover| mover.set_caret_on_end());
                *self.cur_mode.write() = Mode::Insert;
            }
            KeyEvent {
                code: KeyCode::Char('c'),
                ..
            } => {
                actor.edit_on_each_cursor(|mut editor| editor.replace(""));
                actor.move_each_cursor(|mut mover| mover.unset_anchor());
                *self.cur_mode.write() = Mode::Insert;
            }

            ////////// Other mode changing keys.
            KeyEvent {
                code: KeyCode::Char(':'),
                ..
            } => {
                controls.run_cmd("set-prompt :").unwrap();
                if let Ok(_) = controls.switch_to_widget::<CommandLine<U>>() {
                    *self.cur_mode.write() = Mode::Command;
                }
            }
            KeyEvent {
                code: KeyCode::Char('g'),
                ..
            } => *self.cur_mode.write() = Mode::GoTo,

            ////////// History manipulation.
            KeyEvent {
                code: KeyCode::Char('u'),
                ..
            } => actor.undo(),
            KeyEvent {
                code: KeyCode::Char('U'),
                ..
            } => actor.redo(),
            _ => {}
        }
    }

    /// Commands that are available in `Mode::Command`.
    fn match_command<U, AW>(
        &mut self, key: &KeyEvent, mut actor: WidgetActor<U, AW>, controls: &mut Controls<U>
    ) where
        U: Ui,
        AW: ActionableWidget<U> + ?Sized
    {
        match key {
            KeyEvent {
                code: KeyCode::Enter,
                ..
            } => {
                actor.edit_on_main(|mut editor| editor.insert('\n'));
                if let Ok(_) = controls.return_to_file() {
                    *self.cur_mode.write() = Mode::Normal;
                }
            }
            KeyEvent {
                code: KeyCode::Backspace,
                ..
            } => {
                let mut anchors = Vec::with_capacity(actor.cursors_len());
                actor.move_each_cursor(|mut mover| {
                    let caret = mover.caret();
                    anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(-1);
                });
                let mut anchors = anchors.into_iter().cycle();
                actor.edit_on_each_cursor(|mut editor| {
                    editor.replace("");
                });
                actor.move_each_cursor(|mut mover| {
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
                code: KeyCode::Delete,
                ..
            } => {
                let mut anchors = Vec::with_capacity(actor.cursors_len());
                actor.move_each_cursor(|mut mover| {
                    let caret = mover.caret();
                    anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(1);
                });
                let mut anchors = anchors.into_iter().cycle();
                actor.edit_on_each_cursor(|mut editor| {
                    editor.replace("");
                });
                actor.move_each_cursor(|mut mover| {
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
                code: KeyCode::Char(ch),
                ..
            } => {
                actor.edit_on_main(|mut editor| editor.insert(ch));
                actor.move_main(|mut mover| mover.move_hor(1));
            }

            KeyEvent {
                code: KeyCode::Left,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Left, 1);
            }
            KeyEvent {
                code: KeyCode::Right,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Right, 1);
            }
            KeyEvent {
                code: KeyCode::Up,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Top, 1);
            }
            KeyEvent {
                code: KeyCode::Down,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Side::Bottom, 1);
            }
            KeyEvent {
                code: KeyCode::Left,
                ..
            } => {
                move_each(&mut actor, Side::Left, 1);
            }
            KeyEvent {
                code: KeyCode::Right,
                ..
            } => {
                move_each(&mut actor, Side::Right, 1);
            }
            KeyEvent {
                code: KeyCode::Up, ..
            } => {
                move_each(&mut actor, Side::Top, 1);
            }
            KeyEvent {
                code: KeyCode::Down,
                ..
            } => {
                move_each(&mut actor, Side::Bottom, 1);
            }

            KeyEvent {
                code: KeyCode::Esc, ..
            } => {
                if let Ok(_) = controls.return_to_file() {
                    *self.cur_mode.write() = Mode::Normal;
                }
            }
            _ => {}
        }
    }

    /// Commands that are available in `Mode::GoTo`.
    fn match_goto<U, E>(
        &mut self, key: &KeyEvent, mut _actor: WidgetActor<U, E>, controls: &mut Controls<U>
    ) where
        U: Ui,
        E: ActionableWidget<U> + ?Sized
    {
        match key {
            KeyEvent {
                code: KeyCode::Char('a'),
                ..
            } => {
                if let Ok(_) = controls.switch_to_file(&self.last_file) {
                    self.last_file = controls.active_file().to_string();
                }
            }
            KeyEvent {
                code: KeyCode::Char('n'),
                ..
            } => {
                if let Ok(_) = controls.next_file() {
                    self.last_file = controls.active_file().to_string();
                }
            }
            KeyEvent {
                code: KeyCode::Char('N'),
                ..
            } => {
                if let Ok(_) = controls.prev_file() {
                    self.last_file = controls.active_file().to_string();
                }
            }
            _ => {}
        }
        *self.cur_mode.write() = Mode::Normal;
    }

    /// A readable state of which mode is currently active.
    pub fn cur_mode(&self) -> RoData<Mode> {
        RoData::from(&self.cur_mode)
    }

    pub fn mode_fn(&self) -> impl Fn() -> String + Clone {
        let mode = RoData::from(&self.cur_mode);
        move || mode.to_string()
    }
}

impl InputScheme for Editor {
    fn process_key<U, AW>(
        &mut self, key: &KeyEvent, actor: WidgetActor<U, AW>, controls: &mut Controls<U>
    ) where
        U: Ui,
        AW: ActionableWidget<U> + ?Sized
    {
        let cur_mode = *self.cur_mode.read();
        match cur_mode {
            Mode::Insert => self.match_insert(key, actor),
            Mode::Normal => self.match_normal(key, actor, controls),
            Mode::Command => self.match_command(key, actor, controls),
            Mode::GoTo => self.match_goto(key, actor, controls),
            Mode::View => todo!()
        }
    }

    fn send_remapped_keys(&self) -> bool {
        matches!(*self.cur_mode.try_read().unwrap(), Mode::Insert)
    }
}

fn move_each<U, E>(file_editor: &mut WidgetActor<U, E>, direction: Side, amount: usize)
where
    U: Ui,
    E: ActionableWidget<U> + ?Sized
{
    file_editor.move_each_cursor(|mut mover| {
        mover.unset_anchor();
        match direction {
            Side::Top => mover.move_ver(-(amount as isize)),
            Side::Bottom => mover.move_ver(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize)
        }
    });
}

fn move_each_and_select<U, E>(file_editor: &mut WidgetActor<U, E>, direction: Side, amount: usize)
where
    U: Ui,
    E: ActionableWidget<U> + ?Sized
{
    file_editor.move_each_cursor(|mut mover| {
        if !mover.anchor_is_set() {
            mover.set_anchor();
        }
        match direction {
            Side::Top => mover.move_ver(-(amount as isize)),
            Side::Bottom => mover.move_ver(amount as isize),
            Side::Left => mover.move_hor(-(amount as isize)),
            Side::Right => mover.move_hor(amount as isize)
        }
    });
}
