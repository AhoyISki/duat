use std::{default, fmt::Display};

use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
use parsec_core::{
    config::{RoData, RwData},
    input::InputScheme,
    ui::{Direction, Ui},
    widgets::{ActionableWidget, TargetWidget, WidgetActor},
    SessionControl,
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
            Mode::Insert => f.write_fmt(format_args!("insert")),
            Mode::Normal => f.write_fmt(format_args!("normal")),
            Mode::GoTo => f.write_fmt(format_args!("goto")),
            Mode::View => f.write_fmt(format_args!("view")),
            Mode::Command => f.write_fmt(format_args!("command")),
        }
    }
}

#[derive(Default)]
pub struct Editor {
    cur_mode: RwData<Mode>,
    last_file: usize,
}

impl Editor {
	/// Commands that are available in `Mode::Insert`.
    fn match_insert<U, E>(&mut self, key: &KeyEvent, mut actor: WidgetActor<U, E>)
    where
        U: Ui,
        E: ActionableWidget<U> + ?Sized,
    {
        match key {
            KeyEvent { code: KeyCode::Char(ch), .. } => {
                actor.edit_on_each_cursor(|mut editor| {
                    editor.insert(ch);
                });
                actor.move_each_cursor(|mut mover| {
                    mover.move_hor(1);
                });
            }
            KeyEvent { code: KeyCode::Enter, .. } => {
                actor.edit_on_each_cursor(|mut editor| {
                    editor.insert('\n');
                });
                actor.move_each_cursor(|mut mover| {
                    mover.move_hor(1);
                });
            }
            KeyEvent { code: KeyCode::Backspace, .. } => {
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
                    if let Some(Some((anchor, true))) = anchors.next() {
                        editor.calibrate_pos(anchor);
                    }
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
            KeyEvent { code: KeyCode::Delete, .. } => {
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
                    if let Some(Some((anchor, true))) = anchors.next() {
                        editor.calibrate_pos(anchor);
                    }
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
            KeyEvent { code: KeyCode::Left, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut actor, Direction::Left, 1);
            }
            KeyEvent { code: KeyCode::Right, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut actor, Direction::Right, 1);
            }
            KeyEvent { code: KeyCode::Up, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut actor, Direction::Top, 1);
            }
            KeyEvent { code: KeyCode::Down, modifiers: KeyModifiers::SHIFT, .. } => {
                move_each_and_select(&mut actor, Direction::Bottom, 1);
            }
            KeyEvent { code: KeyCode::Left, .. } => {
                move_each(&mut actor, Direction::Left, 1);
            }
            KeyEvent { code: KeyCode::Right, .. } => {
                move_each(&mut actor, Direction::Right, 1);
            }
            KeyEvent { code: KeyCode::Up, .. } => {
                move_each(&mut actor, Direction::Top, 1);
            }
            KeyEvent { code: KeyCode::Down, .. } => {
                move_each(&mut actor, Direction::Bottom, 1);
            }
            KeyEvent { code: KeyCode::Tab, .. } => {
                actor.new_moment();
                *self.cur_mode.write() = Mode::Normal;
            }
            _ => {}
        }
    }

	/// Commands that are available in `Mode::Normal`.
    fn match_normal<U, E>(
        &mut self, key: &KeyEvent, mut actor: WidgetActor<U, E>, control: &mut SessionControl<U>,
    ) where
        U: Ui,
        E: ActionableWidget<U> + ?Sized,
    {
        match key {
            ////////// SessionControl commands.
            KeyEvent { code: KeyCode::Char('c'), modifiers: KeyModifiers::CONTROL, .. } => {
                control.quit();
            }

            ////////// Movement keys that retain or create selections.
            KeyEvent {
                code: KeyCode::Char('H') | KeyCode::Left,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Direction::Left, 1);
            }
            KeyEvent {
                code: KeyCode::Char('J') | KeyCode::Down,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Direction::Bottom, 1);
            }
            KeyEvent {
                code: KeyCode::Char('K') | KeyCode::Up,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Direction::Top, 1);
            }
            KeyEvent {
                code: KeyCode::Char('L') | KeyCode::Right,
                modifiers: KeyModifiers::SHIFT,
                ..
            } => {
                move_each_and_select(&mut actor, Direction::Right, 1);
            }

            ////////// Movement keys that get rid of selections.
            KeyEvent { code: KeyCode::Char('h') | KeyCode::Left, .. } => {
                move_each(&mut actor, Direction::Left, 1);
            }
            KeyEvent { code: KeyCode::Char('j') | KeyCode::Down, .. } => {
                move_each(&mut actor, Direction::Bottom, 1);
            }
            KeyEvent { code: KeyCode::Char('k') | KeyCode::Up, .. } => {
                move_each(&mut actor, Direction::Top, 1);
            }
            KeyEvent { code: KeyCode::Char('l') | KeyCode::Right, .. } => {
                move_each(&mut actor, Direction::Right, 1);
            }

            ////////// Insertion keys.
            KeyEvent { code: KeyCode::Char('i'), .. } => {
                actor.move_each_cursor(|mut mover| mover.set_caret_on_start());
                *self.cur_mode.write() = Mode::Insert;
            }
            KeyEvent { code: KeyCode::Char('a'), .. } => {
                actor.move_each_cursor(|mut mover| mover.set_caret_on_end());
                *self.cur_mode.write() = Mode::Insert;
            }
            KeyEvent { code: KeyCode::Char('c'), .. } => {
                actor.edit_on_each_cursor(|mut editor| editor.replace(""));
                actor.move_each_cursor(|mut mover| mover.unset_anchor());
                *self.cur_mode.write() = Mode::Insert;
            }

            ////////// Other mode changing keys.
            KeyEvent { code: KeyCode::Char(':'), .. } => {
                control.switch_widget(TargetWidget::First(String::from("command")));
                *self.cur_mode.write() = Mode::Command;
            }
            KeyEvent { code: KeyCode::Char('g'), .. } => *self.cur_mode.write() = Mode::GoTo,

            ////////// History manipulation.
            KeyEvent { code: KeyCode::Char('u'), .. } => actor.undo(),
            KeyEvent { code: KeyCode::Char('U'), .. } => actor.redo(),
            _ => {}
        }
    }

	/// Commands that are available in `Mode::Command`.
    fn match_command<U, E>(
        &mut self, key: &KeyEvent, mut actor: WidgetActor<U, E>, control: &mut SessionControl<U>,
    ) where
        U: Ui,
        E: ActionableWidget<U> + ?Sized,
    {
        match key {
            KeyEvent { code: KeyCode::Char('\n'), .. } => {
                actor.edit_on_main(|mut editor| editor.replace('\n'));
                control.return_to_file();
                *self.cur_mode.write() = Mode::Normal;
            }
            KeyEvent { code: KeyCode::Char(ch), .. } => {
                actor.edit_on_main(|mut editor| editor.replace(ch));
            }
            KeyEvent { code: KeyCode::Esc, .. } => {
                control.return_to_file();
                *self.cur_mode.write() = Mode::Normal;
            }
            _ => {}
        }
    }

	/// Commands that are available in `Mode::GoTo`.
    fn match_goto<U, E>(
        &mut self, key: &KeyEvent, mut _actor: WidgetActor<U, E>, control: &mut SessionControl<U>,
    ) where
        U: Ui,
        E: ActionableWidget<U> + ?Sized,
    {
        match key {
            KeyEvent { code: KeyCode::Char('a'), .. } => {
                control.switch_widget(TargetWidget::Absolute(String::from("file"), self.last_file));
                self.last_file = control.active_file();
            }
            KeyEvent { code: KeyCode::Char('n'), .. } => {
                let (active, max) = (control.active_file(), control.max_file());
                let next_file = if active == max { 1 } else { active + 1 };
                control.switch_widget(TargetWidget::Absolute(String::from("file"), next_file));
                self.last_file = control.active_file();
            }
            KeyEvent { code: KeyCode::Char('N'), .. } => {
                let (active, max) = (control.active_file(), control.max_file());
                let prev_file = if active == 1 { max } else { active - 1 };
                control.switch_widget(TargetWidget::Absolute(String::from("file"), prev_file));
                self.last_file = control.active_file();
            }
            _ => {}
        }
        *self.cur_mode.write() = Mode::Normal;
    }

    /// A readable state of which mode is currently active.
    pub fn cur_mode(&self) -> RoData<Mode> {
        RoData::from(&self.cur_mode)
    }
}

impl InputScheme for Editor {
    fn process_key<U, A>(
        &mut self, key: &KeyEvent, actor: WidgetActor<U, A>, control: &mut SessionControl<U>,
    ) where
        U: Ui,
        A: ActionableWidget<U> + ?Sized,
    {
        let cur_mode = *self.cur_mode.read();
        match cur_mode {
            Mode::Insert => self.match_insert(key, actor),
            Mode::Normal => self.match_normal(key, actor, control),
            Mode::Command => self.match_command(key, actor, control),
            Mode::GoTo => self.match_goto(key, actor, control),
            Mode::View => todo!(),
        }
    }

    fn send_remapped_keys(&self) -> bool {
        matches!(*self.cur_mode.read(), Mode::Insert)
    }
}

fn move_each<U, E>(file_editor: &mut WidgetActor<U, E>, direction: Direction, amount: usize)
where
    U: Ui,
    E: ActionableWidget<U> + ?Sized,
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
    E: ActionableWidget<U> + ?Sized,
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
