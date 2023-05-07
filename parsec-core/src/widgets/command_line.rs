#[cfg(not(feature = "deadlock-detection"))]
use std::sync::RwLock;
use std::{any::Any, error::Error, fmt::Display, sync::Arc};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::RwLock;

use super::{ActionableWidget, EditAccum, NormalWidget, Widget};
use crate::{
    data::{DownCastableData, RwData},
    position::{Cursor, Editor, Mover},
    text::{PrintCfg, Text},
    ui::{PrintInfo, PushSpecs, Ui},
    Manager
};

/// A command that doesn't returns a `String` as a result.
///
/// The command takes in two vectors of `String`s, the first one is
/// the "flags" passed on to the command. The second one is a list of
/// arguments passed on to the command.
pub struct Command {
    /// A command that may mutate the `Commandable` struct.
    ///
    /// # Arguments
    ///
    /// - 1: A `Vec<String>` representing the flags that have been
    ///   passed to the function.
    /// - 2: A `Vec<String>` representing the arguments to be read by
    ///   the function.
    ///
    /// # Returns
    ///
    /// A `Result<Option<String>, String>`. `Ok(Some(String))` is an
    /// outcome that could be used to chain multiple commands.
    /// `Err(String)` is obligatory, used to tell the user what went
    /// wrong while running the command.
    function: Box<dyn FnMut(&[String], &[String]) -> Result<Option<String>, String>>,
    /// A list of `String`s that act as callers for this `Command`.
    callers: Vec<String>
}

impl Command {
    pub fn new(
        function: Box<dyn FnMut(&[String], &[String]) -> Result<Option<String>, String>>,
        callers: Vec<String>
    ) -> Self {
        Self { function, callers }
    }

    fn try_exec(
        &mut self, cmd: &String, flags: &[String], args: &[String]
    ) -> Result<Option<String>, CommandError> {
        if self.callers.contains(&cmd) {
            return (self.function)(flags, args).map_err(|err| CommandError::Failed(err));
        } else {
            return Err(CommandError::NotFound(cmd.clone()));
        }
    }

    fn callers(&self) -> &[String] {
        &self.callers
    }
}

/// A list of [`Command`]s, meant to be used in a [`CommandLine<U>`]
/// or in hooks (TODO).
#[derive(Default)]
pub struct Commands {
    commands: Vec<Command>
}

impl Commands {
    /// Returns a new instance of `CommandList`.
    pub fn new() -> Self {
        Commands {
            commands: Vec::new()
        }
    }

    /// Tries to execute a given command on any of its lists.
    pub(crate) fn try_exec(
        &mut self, cmd: String, flags: Vec<String>, args: Vec<String>
    ) -> Result<Option<String>, CommandError> {
        for command in &mut self.commands {
            let result = command.try_exec(&cmd, flags.as_slice(), args.as_slice());
            let Err(CommandError::NotFound(_)) = result else {
                return result;
            };
        }

        Err(CommandError::NotFound(cmd))
    }

    /// Tries to add a new `CommandList`. Returns an error if any of
    /// its commands already exists.
    pub(crate) fn try_add(&mut self, command: Command) -> Result<(), CommandError> {
        let mut new_callers = command.callers().iter();

        for caller in self.commands.iter().map(|cmd| cmd.callers()).flatten() {
            if new_callers.any(|new_caller| new_caller == caller) {
                return Err(CommandError::AlreadyExists(caller.clone()));
            }
        }

        self.commands.push(command);

        Ok(())
    }
}

pub struct CommandLineCfg {
    run_string: String,
    _cmd_replacements: Vec<(String, String)>
}

impl Default for CommandLineCfg {
    fn default() -> Self {
        CommandLineCfg {
            run_string: String::from(':'),
            _cmd_replacements: Vec::new()
        }
    }
}

pub struct CommandLine<U>
where
    U: Ui + ?Sized
{
    text: Text<U>,
    print_info: U::PrintInfo,
    cursor: [Cursor; 1],
    commands: RwData<Commands>,
    needs_update: bool,
    cfg: CommandLineCfg
}

impl<U> CommandLine<U>
where
    U: Ui + Default + 'static
{
    /// Returns a function that outputs a [`CommandLine<U>`], using a
    /// [`CommandLineCfg`].
    pub fn config_fn(cfg: CommandLineCfg) -> Box<dyn FnOnce(&Manager, PushSpecs) -> Widget<U>> {
        Box::new(move |manager, _| {
            let command_line = CommandLine {
                text: Text::default_string(),
                print_info: U::PrintInfo::default(),
                cursor: [Cursor::default()],
                commands: manager.commands(),
                needs_update: false,
                cfg
            };

            Widget::actionable(Arc::new(RwLock::new(command_line)), Box::new(|| true))
        })
    }

    /// Returns a function that outputs a [`CommandLine<U>`], using
    /// the default [`CommandLineCfg`].
    pub fn default_fn() -> Box<dyn FnOnce(&Manager, PushSpecs) -> Widget<U>> {
        Box::new(move |session_manager, _| {
            let command_line = CommandLine {
                text: Text::default_string(),
                print_info: U::PrintInfo::default(),
                cursor: [Cursor::default()],
                commands: session_manager.commands(),
                needs_update: false,
                cfg: CommandLineCfg::default()
            };

            Widget::actionable(Arc::new(RwLock::new(command_line)), Box::new(|| true))
        })
    }
}

impl<U> NormalWidget<U> for CommandLine<U>
where
    U: Ui + 'static
{
    fn update(&mut self, label: &U::Label) {
        let print_cfg = PrintCfg::default();
        self.print_info
            .scroll_to_gap(&self.text, self.cursor[0].caret(), label, &print_cfg);

        // self.match_scroll();
        let lines: String = self.text.inner().chars_at(0).collect();
        let mut whole_command = lines.split_whitespace().map(|word| String::from(word));

        let Some(command) = whole_command.next() else {
			return;
		};

        let mut command_list = self.commands.write();
        let _ = command_list.try_exec(command, Vec::new(), whole_command.collect());

        self.needs_update = false;
    }

    fn needs_update(&self) -> bool {
        self.needs_update
    }

    fn text(&self) -> &Text<U> {
        &self.text
    }
}

impl<U> ActionableWidget<U> for CommandLine<U>
where
    U: Ui + 'static
{
    fn editor<'a>(&'a mut self, _: usize, edit_accum: &'a mut EditAccum) -> Editor<U> {
        self.needs_update = true;
        Editor::new(&mut self.cursor[0], &mut self.text, edit_accum, None, None)
    }

    fn mover<'a>(&'a mut self, _: usize, label: &'a U::Label) -> Mover<U> {
        self.needs_update = true;
        Mover::new(&mut self.cursor[0], &self.text, label, PrintCfg::default())
    }

    fn members_for_cursor_tags(&mut self) -> (&mut Text<U>, &[Cursor], usize) {
        (&mut self.text, self.cursor.as_slice(), 0)
    }

    fn cursors(&self) -> &[Cursor] {
        self.cursor.as_slice()
    }

    fn mut_cursors(&mut self) -> Option<&mut Vec<Cursor>> {
        None
    }

    fn main_cursor_index(&self) -> usize {
        0
    }

    fn mut_main_cursor_index(&mut self) -> Option<&mut usize> {
        None
    }

    fn on_focus(&mut self, label: &U::Label) {
        self.needs_update = true;
        self.text = Text::new_string(&self.cfg.run_string);
        let chars = self.cfg.run_string.chars().count() as isize;
        self.cursor[0].move_hor::<U>(chars, &self.text, label, &PrintCfg::default());
        self.text.add_cursor_tags(self.cursor.as_slice(), 0);
    }

    fn on_unfocus(&mut self, _label: &U::Label) {
        self.needs_update = true;
        self.text = Text::default_string();
        self.cursor[0] = Cursor::default();
        self.text.remove_cursor_tags(self.cursor.as_slice());
    }

    fn still_valid(&self) -> bool {
        !self.text.is_empty()
    }
}

impl<U> DownCastableData for CommandLine<U>
where
    U: Ui + 'static
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub enum CommandError {
    AlreadyExists(String),
    NotFound(String),
    Failed(String)
}

impl Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandError::AlreadyExists(cmd) => {
                f.write_fmt(format_args!("The command \"{}\" already exists!", cmd))
            }
            CommandError::NotFound(cmd) => {
                f.write_fmt(format_args!("The command \"{}\" was not found!", cmd))
            }
            CommandError::Failed(failure) => f.write_fmt(format_args!("{}", failure))
        }
    }
}

impl Error for CommandError {}
