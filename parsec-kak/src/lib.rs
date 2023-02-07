use std::fmt::Display;

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use parsec_core::{
    config::{RoData, RwData},
    input::InputScheme,
    ui::{Direction, Ui},
    widgets::{
        file_widget::FileWidget,
        WidgetActor,
        EditableWidget,
    },
};

#[derive(Clone, Copy, PartialEq)]
pub enum Mode {
    Insert,
    Normal,
    Goto,
    View,
    Command,
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mode::Insert => f.write_fmt(format_args!("insert")),
            Mode::Normal => f.write_fmt(format_args!("normal")),
            Mode::Goto => f.write_fmt(format_args!("goto")),
            Mode::View => f.write_fmt(format_args!("view")),
            Mode::Command => f.write_fmt(format_args!("command")),
        }
    }
}

pub struct Editor {
    cur_mode: RwData<Mode>,
}

impl Editor {
    /// Returns a new instance of `parsec-kak::Editor`.
    pub fn new() -> Self {
        Editor { cur_mode: RwData::new(Mode::Normal) }
    }

    /// The default mappings for the insert mode.
    fn match_insert<U, E>(&mut self, key: &KeyEvent, mut editable_actor: WidgetActor<U, E>)
    where
        U: Ui,
        E: EditableWidget<U>,
    {
        match key {
            KeyEvent { code: KeyCode::Char(ch), .. } => {
                editable_actor.edit_on_each_cursor(|mut editor| {
                    editor.insert(ch);
                });
                editable_actor.move_each_cursor(|mut mover| {
                    mover.move_hor(1);
                });
            }
            KeyEvent { code: KeyCode::Enter, .. } => {
                editable_actor.edit_on_each_cursor(|mut editor| {
                    editor.insert('\n');
                });
                editable_actor.move_each_cursor(|mut mover| {
                    mover.move_hor(1);
                });
            }
            KeyEvent { code: KeyCode::Backspace, .. } => {
                let mut anchors = Vec::with_capacity(editable_actor.cursors_len());
                editable_actor.move_each_cursor(|mut mover| {
                    let caret = mover.caret();
                    anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(-1);
                });
                let mut anchors = anchors.into_iter().cycle();
                editable_actor.edit_on_each_cursor(|mut editor| {
                    editor.replace("");
                    if let Some(Some((anchor, true))) = anchors.next() {
                        editor.calibrate_pos(anchor);
                    }
                });
                editable_actor.move_each_cursor(|mut mover| {
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
                let mut anchors = Vec::with_capacity(editable_actor.cursors_len());
                editable_actor.move_each_cursor(|mut mover| {
                    let caret = mover.caret();
                    anchors.push(mover.take_anchor().map(|anchor| (anchor, anchor >= caret)));
                    mover.set_anchor();
                    mover.move_hor(1);
                });
                let mut anchors = anchors.into_iter().cycle();
                editable_actor.edit_on_each_cursor(|mut editor| {
                    editor.replace("");
                    if let Some(Some((anchor, true))) = anchors.next() {
                        editor.calibrate_pos(anchor);
                    }
                });
                editable_actor.move_each_cursor(|mut mover| {
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
                move_each_and_select(&mut editable_actor, Direction::Left, 1);
            }
            KeyEvent { code: KeyCode::Right, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut editable_actor, Direction::Right, 1);
            }
            KeyEvent { code: KeyCode::Up, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut editable_actor, Direction::Top, 1);
            }
            KeyEvent { code: KeyCode::Down, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut editable_actor, Direction::Bottom, 1);
            }
            KeyEvent { code: KeyCode::Left, .. } => {
                move_each(&mut editable_actor, Direction::Left, 1);
            }
            KeyEvent { code: KeyCode::Right, .. } => {
                move_each(&mut editable_actor, Direction::Right, 1);
            }
            KeyEvent { code: KeyCode::Up, .. } => {
                move_each(&mut editable_actor, Direction::Top, 1);
            }
            KeyEvent { code: KeyCode::Down, .. } => {
                move_each(&mut editable_actor, Direction::Bottom, 1);
            }
            KeyEvent { code: KeyCode::Tab, .. } => {
                editable_actor.new_moment();
                *self.cur_mode.write() = Mode::Normal;
            }
            _ => {}
        }
    }

    /// The default mappings for the normal mode.
    fn match_normal<U, E>(&mut self, key: &KeyEvent, mut editable_actor: WidgetActor<U, E>)
    where
        U: Ui,
        E: EditableWidget<U>,
    {
        match key {
            ////////// Movement keys that retain or create selections.
            KeyEvent {
                code: KeyCode::Char('H') | KeyCode::Left,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut editable_actor, Direction::Left, 1);
            }
            KeyEvent {
                code: KeyCode::Char('J') | KeyCode::Down,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut editable_actor, Direction::Bottom, 1);
            }
            KeyEvent {
                code: KeyCode::Char('K') | KeyCode::Up,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut editable_actor, Direction::Top, 1);
            }
            KeyEvent {
                code: KeyCode::Char('L') | KeyCode::Right,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut editable_actor, Direction::Right, 1);
            }

            ////////// Movement keys that get rid of selections.
            KeyEvent { code: KeyCode::Char('h') | KeyCode::Left, .. } => {
                move_each(&mut editable_actor, Direction::Left, 1);
            }
            KeyEvent { code: KeyCode::Char('j') | KeyCode::Down, .. } => {
                move_each(&mut editable_actor, Direction::Bottom, 1);
            }
            KeyEvent { code: KeyCode::Char('k') | KeyCode::Up, .. } => {
                move_each(&mut editable_actor, Direction::Top, 1);
            }
            KeyEvent { code: KeyCode::Char('l') | KeyCode::Right, .. } => {
                move_each(&mut editable_actor, Direction::Right, 1);
            }

            ////////// Insertion keys.
            KeyEvent { code: KeyCode::Char('i'), .. } => {
                editable_actor.move_each_cursor(|mut mover| mover.set_caret_on_start());
                *self.cur_mode.write() = Mode::Insert;
            }
            KeyEvent { code: KeyCode::Char('a'), .. } => {
                editable_actor.move_each_cursor(|mut mover| mover.set_caret_on_end());
                *self.cur_mode.write() = Mode::Insert;
            }
            KeyEvent { code: KeyCode::Char('c'), .. } => {
                editable_actor.edit_on_each_cursor(|mut editor| editor.replace(""));
                editable_actor.move_each_cursor(|mut mover| mover.unset_anchor());
                *self.cur_mode.write() = Mode::Insert;
            }

            ////////// Command line keys.
            KeyEvent { code: KeyCode::Char(':'), .. } => *self.cur_mode.write() = Mode::Command,

            ////////// History manipulation.
            KeyEvent { code: KeyCode::Char('u'), .. } => editable_actor.undo(),
            KeyEvent { code: KeyCode::Char('U'), .. } => editable_actor.redo(),
            _ => {}
        }
    }

    /// The default mappings for the command mode.
    fn match_command<U, E>(&mut self, key: &KeyEvent, mut widget_actor: WidgetActor<U, E>)
    where
        U: Ui,
        E: EditableWidget<U>,
    {
        match key {
            KeyEvent { code: KeyCode::Esc, .. } => {
                *self.cur_mode.write() = Mode::Normal;
            }
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
        let widget_actor = WidgetActor::from(file);

        let cur_mode = *self.cur_mode.read();
        match cur_mode {
            Mode::Insert => self.match_insert(key, widget_actor),
            Mode::Normal => self.match_normal(key, widget_actor),
            Mode::Command => self.match_command(key, widget_actor),
            _ => {}
        }
    }

    fn send_remapped_keys(&self) -> bool {
        matches!(*self.cur_mode.read(), Mode::Insert)
    }
}

fn move_each<U, E>(file_editor: &mut WidgetActor<U, E>, direction: Direction, amount: usize)
where
    U: Ui,
    E: EditableWidget<U>,
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

fn move_each_and_select<U, E>(
    file_editor: &mut WidgetActor<U, E>, direction: Direction, amount: usize,
) where
    U: Ui,
    E: EditableWidget<U>,
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
