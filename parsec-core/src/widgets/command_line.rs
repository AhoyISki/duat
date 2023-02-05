use std::{error::Error, fmt::Display};

use crate::{
    config::RwData,
    text::Text,
    ui::{EndNode, Ui},
};

use super::{Widget, file_widget::PrintInfo};

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
    C: ?Sized,
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

pub struct CommandLine<U>
where
    U: Ui,
{
    end_node: RwData<EndNode<U>>,
    text: Text,
    command_list: RwData<CommandList>
}

impl<U> Widget<U> for CommandLine<U>
where
    U: Ui,
{
    fn end_node(&self) -> &RwData<EndNode<U>> {
        &self.end_node
    }

    fn end_node_mut(&mut self) -> &mut RwData<EndNode<U>> {
        &mut self.end_node
    }

    fn update(&mut self) {
        todo!()
    }

    fn needs_update(&self) -> bool {
        todo!()
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn print(&mut self) {
        self.text.print(&mut self.end_node.write(), PrintInfo::default());
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
