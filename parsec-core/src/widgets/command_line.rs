#[cfg(not(feature = "deadlock-detection"))]
use std::sync::Mutex;
use std::{any::Any, error::Error, fmt::Display, sync::Arc};

#[cfg(feature = "deadlock-detection")]
use no_deadlocks::Mutex;

use super::{ActionableWidget, NormalWidget, Widget};
use crate::{
    config::{DownCastableData, RwData},
    position::{Cursor, Editor, Mover, SpliceAdder},
    text::{PrintInfo, Text},
    ui::{EndNode, Ui},
    Session,
};

/// The sole purpose of this module is to prevent any external implementations of `Commander`.
mod private {
    pub trait Commander {}
}

pub trait Commander: private::Commander {
    fn try_exec(
        &mut self, cmd: &String, flags: &[String], args: &[String],
    ) -> Result<Option<String>, CommandError>;

    fn callers(&self) -> &[String];
}

/// A command that doesn't returns a `String` as a result.
///
/// The command takes in two vectors of `String`s, the first one is the "flags" passed on to the
/// command. The second one is a list of arguments passed on to the command.
pub struct Command<C>
where
    C: ?Sized + 'static,
{
    /// A command that may mutate the `Commandable` struct.
    ///
    /// # Arguments
    ///
    /// - 1: A `Vec<String>` representing the flags that have been passed to the function.
    /// - 2: A `Vec<String>` representing the arguments to be read by the function.
    ///
    /// # Returns
    ///
    /// A `Result<Option<String>, String>`. `Ok(Some(String))` is an outcome that could be used to
    /// chain multiple commands. `Err(String)` is obligatory, used to tell the user what went wrong
    /// while running the command.
    function: Box<dyn FnMut(&mut C, &[String], &[String]) -> Result<Option<String>, String>>,
    /// A list of `String`s that act as callers for this `Command`.
    callers: Vec<String>,
    /// The object that will be affected by `self.function`.
    commandable: RwData<C>,
}

impl<C> Command<C>
where
    C: ?Sized,
{
    pub fn new(
        function: Box<dyn FnMut(&mut C, &[String], &[String]) -> Result<Option<String>, String>>,
        callers: Vec<String>, commandable: RwData<C>,
    ) -> Self {
        Self { function, callers, commandable }
    }
}

impl<C> private::Commander for Command<C> where C: ?Sized {}

impl<C> Commander for Command<C>
where
    C: ?Sized,
{
    fn try_exec(
        &mut self, cmd: &String, flags: &[String], args: &[String],
    ) -> Result<Option<String>, CommandError> {
        if self.callers.contains(&cmd) {
            return (self.function)(&mut self.commandable.write(), flags, args)
                .map_err(|err| CommandError::Failed(err));
        } else {
            return Err(CommandError::NotFound(cmd.clone()));
        }
    }

    fn callers(&self) -> &[String] {
        &self.callers
    }
}

/// A list of `CommandList`s, that can execute commands on any of them.
#[derive(Default)]
pub struct CommandList {
    commands: Vec<Box<dyn Commander>>,
}

impl CommandList {
    /// Returns a new instance of `CommandListList`.
    pub fn new<C>(command: Box<C>) -> Self
    where
        C: Commander + 'static,
    {
        CommandList { commands: vec![command] }
    }

    /// Tries to execute a given command on any of its lists.
    pub(crate) fn try_exec(
        &mut self, cmd: String, flags: Vec<String>, args: Vec<String>,
    ) -> Result<Option<String>, CommandError> {
        for command in &mut self.commands {
            let result = command.try_exec(&cmd, flags.as_slice(), args.as_slice());
            let Err(CommandError::NotFound(_)) = result else {
                return result;
            };
        }

        Err(CommandError::NotFound(cmd))
    }

    /// Tries to add a new `CommandList`. Returns an error if any of its commands already exists.
    pub(crate) fn try_add(&mut self, command: Box<dyn Commander>) -> Result<(), CommandError> {
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

pub struct CommandLineConfig {
    run_string: String,
    _cmd_replacements: Vec<(String, String)>,
}

impl Default for CommandLineConfig {
    fn default() -> Self {
        CommandLineConfig { run_string: String::from(':'), _cmd_replacements: Vec::new() }
    }
}

pub struct CommandLine<U>
where
    U: Ui,
{
    text: Text<U>,
    print_info: PrintInfo,
    cursor: [Cursor; 1],
    command_list: RwData<CommandList>,
    needs_update: bool,
    config: CommandLineConfig,
}

impl<U> CommandLine<U>
where
    U: Ui + 'static,
{
    pub fn default(session: &Session<U>) -> Widget<U> {
        let command_line = CommandLine {
            text: Text::default(),
            print_info: PrintInfo::default(),
            cursor: [Cursor::default()],
            command_list: session.global_commands(),
            needs_update: false,
            config: CommandLineConfig::default(),
        };

        Widget::Actionable(RwData::new_unsized(Arc::new(Mutex::new(command_line))))
    }
}

impl<U> DownCastableData for CommandLine<U>
where
    U: Ui + 'static,
{
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl<U> NormalWidget<U> for CommandLine<U>
where
    U: Ui + 'static,
{
    fn identifier(&self) -> &str {
        "parsec-command-line"
    }

    fn update(&mut self, end_node: &mut EndNode<U>) {
        self.print_info.update(self.cursor[0].caret(), &self.text, end_node);

        //self.match_scroll();
        if self.text.lines().len() > 1 {
            let lines: String = self.text.lines().iter().map(|line| line.text().as_str()).collect();
            let mut whole_command = lines.split_whitespace().map(|word| String::from(word));

            let Some(command) = whole_command.next() else {
    			return;
			};

            let mut command_list = self.command_list.write();
            let _ = command_list.try_exec(command, Vec::new(), whole_command.collect());
        }

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
    U: Ui + 'static,
{
    fn editor<'a>(
        &'a mut self, _: usize, splice_adder: &'a mut SpliceAdder, end_node: &'a EndNode<U>,
    ) -> Editor<U> {
        self.needs_update = true;
        Editor::new(&mut self.cursor[0], splice_adder, &mut self.text, end_node, None, None)
    }

    fn mover<'a>(&'a mut self, _: usize, end_node: &'a EndNode<U>) -> Mover<U> {
        self.needs_update = true;
        Mover::new(&mut self.cursor[0], &self.text, end_node, None)
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

    fn on_focus(&mut self, end_node: &mut EndNode<U>) {
        self.needs_update = true;
        self.text = Text::from(&self.config.run_string);
        let chars = self.config.run_string.chars().count() as i32;
        self.cursor[0].move_hor(chars, &self.text.lines(), end_node);
        self.text.add_cursor_tags(self.cursor.as_slice(), 0);
    }

    fn on_unfocus(&mut self, _end_node: &mut EndNode<U>) {
        self.needs_update = true;
        self.text = Text::default();
        self.cursor[0] = Cursor::default();
        self.text.remove_cursor_tags(self.cursor.as_slice(), 0);
    }

    fn still_valid(&self) -> bool {
        !self.text.is_empty()
    }
}

unsafe impl<U> Send for CommandLine<U> where U: Ui {}

#[derive(Debug)]
pub enum CommandError {
    AlreadyExists(String),
    NotFound(String),
    Failed(String),
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
            CommandError::Failed(failure) => f.write_fmt(format_args!("{}", failure)),
        }
    }
}

impl Error for CommandError {}
