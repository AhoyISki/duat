use std::{any::Any, error::Error, fmt::Display};

use crate::config::RwData;

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
}

impl<C> Command<C>
where
    C: ?Sized,
{
    pub fn new(
        function: Box<dyn FnMut(&mut C, &[String], &[String]) -> Result<Option<String>, String>>,
        callers: Vec<String>,
    ) -> Self {
        Self { function, callers }
    }
}

pub struct CommandList<C>
where
    C: ?Sized,
{
    commands: Vec<Command<C>>,
    commandable: RwData<C>,
}

impl<C> CommandList<C>
where
    C: ?Sized,
{
    /// Returns a new instance of `CommandList`.
    pub fn new(commands: Vec<Command<C>>, commandable: RwData<C>) -> Self {
        CommandList { commands, commandable }
    }

    /// Tries to execute a given command.
    pub(crate) fn try_exec(
        &mut self, cmd: &String, flags: &[String], args: &[String],
    ) -> Result<Option<String>, CommandError> {
        for command in &mut self.commands {
            if command.callers.contains(&cmd) {
                return (command.function)(&mut self.commandable.write(), flags, args)
                    .map_err(|err| CommandError::Failed(cmd.clone()));
            }
        }

        Err(CommandError::NotFound(cmd.clone()))
    }
}

/// A list of `CommandList`s, that can execute commands on any of them.
#[derive(Default)]
pub struct CommandListList {
    lists: Vec<Box<CommandList<dyn Any>>>,
}

impl CommandListList {
    /// Returns a new instance of `CommandListList`.
    pub fn new(command_list: CommandList<dyn Any>) -> Self {
        CommandListList { lists: vec![Box::new(command_list)] }
    }

    /// Tries to execute a given command on any of its lists.
    pub(crate) fn try_exec(
        &mut self, cmd: String, flags: Vec<String>, args: Vec<String>,
    ) -> Result<Option<String>, CommandError> {
        for command_list in &mut self.lists {
            let result = command_list.try_exec(&cmd, flags.as_slice(), args.as_slice());
            let Err(CommandError::NotFound(_)) = result else {
                return result;
            };
        }

        Err(CommandError::NotFound(cmd))
    }

    /// Tries to add a new `CommandList`. Returns an error if any of its commands already exists.
    pub(crate) fn try_add(
        &mut self, command_list: CommandList<dyn Any>,
    ) -> Result<(), CommandError> {
        let mut new_callers = command_list.commands.iter().map(|cmd| &cmd.callers).flatten();

        for caller in
            self.lists.iter().map(|list| &list.commands).flatten().map(|cmd| &cmd.callers).flatten()
        {
            if new_callers.any(|new_caller| new_caller == caller) {
                return Err(CommandError::AlreadyExists(caller.clone()));
            }
        }

        self.lists.push(Box::new(command_list));

        Ok(())
    }
}

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
