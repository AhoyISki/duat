/// A function to be used on hooks or in the [`CommandLine<U>`].
///
/// The [`Command`] takes in two vectors of [`String`]s, the first
/// represents the `flags` passed on to the [`Command`], while the
/// second is a list of `arguments`.
///
/// # Examples
///
/// When creating a [`Command`], one should prioritize the reduction
/// of "data" that the [`Command`] uses when triggering.
///
/// As an example, let's say you're creating a [`NormalWidget<U>`],
/// represented by the struct below.
///
/// ```rust
/// # use parsec_core::{text::Text, ui::Ui};
/// # use std::sync::{atomic::AtomicBool, Arc};
/// struct MyWidget<U>
/// where
///     U: Ui
/// {
///     text: Text<U>,
///     other_field: String,
///     relevant_field: Arc<AtomicBool>
/// }
/// ```
///
/// In this case, assuming that you will add the [`Command`]s through
/// a function that takes, as parameters, one `RwData<MyWidget<U>` and
/// one [`&mut Commands`][Commands], the function should look like
/// this:
///
/// ```rust
/// # use parsec_core::{
/// #     data::RwData,
/// #     text::Text,
/// #     ui::Ui,
/// #     commands::{Command, CommandErr, Commands}
/// # };
/// # use std::sync::{
/// #     atomic::{AtomicBool, Ordering},
/// #     Arc
/// # };
/// # struct MyWidget<U>
/// # where
/// #     U: Ui
/// # {
/// #     text: Text<U>,
/// #     other_field: String,
/// #     relevant_field: Arc<AtomicBool>
/// # }
/// #
/// # fn command_function(
/// #     relevant_field: bool, flags: &[String], args: &[String]
/// # ) -> Result<Option<String>, String> {
/// #     todo!();
/// # }
/// #
/// fn add_commands<U>(
///     widget: RwData<MyWidget<U>>, commands: &mut Commands
/// ) -> Result<(), CommandErr>
/// where
///     U: Ui
/// {
///     let relevant_field = widget.read().relevant_field.clone();
///     let command = Command::new(
///         move |flags, args| {
///             command_function(relevant_field.load(Ordering::Relaxed), flags, args)
///         },
///         vec![String::from("my-function-caller")]
///     );
///     commands.try_add(command)
/// }
/// ```
///
/// Instead of this:
///
/// ```rust
/// # use parsec_core::{
/// #     data::RwData,
/// #     text::Text,
/// #     ui::Ui,
/// #     commands::{Command, CommandErr, Commands}
/// # };
/// # use std::sync::{
/// #     atomic::{AtomicBool, Ordering},
/// #     Arc
/// # };
/// # struct MyWidget<U>
/// # where
/// #     U: Ui
/// # {
/// #     text: Text<U>,
/// #     other_field: String,
/// #     relevant_field: Arc<AtomicBool>
/// # }
/// #
/// # fn command_function(
/// #     relevant_field: bool, flags: &[String], args: &[String]
/// # ) -> Result<Option<String>, String> {
/// #     todo!();
/// # }
/// #
/// fn add_commands<U>(
///     widget: RwData<MyWidget<U>>, commands: &mut Commands
/// ) -> Result<(), CommandErr>
/// where
///     U: Ui
/// {
///     let command = Command::new(
///         move |flags, args| {
///             let relevant_field = widget
///                 .read()
///                 .relevant_field
///                 .load(Ordering::Relaxed);
///             command_function(relevant_field, flags, args)
///         },
///         vec![String::from("my-function-caller")]
///     );
///     commands.try_add(command)
/// }
/// ```
///
/// In the second version, the whole `widget` variable gets moved into
/// the closure. The problem with this is that this specific
/// [`RwData`] will get used very often, and by running the
/// [`Command`], you may cause a deadlock, which is really annoying to
/// diagnose.
///
/// As an example, there is the [`CommandLine<U>`] widget. If its
/// [`Command`]s moved an entire [`RwData<CommandLine<U>>`] to the
/// closures, every single one of them, when triggered through the
/// widget, would result in a deadlock, since they're writing to an
/// [`RwData<T>`] that was already being written to.
pub struct Command {
    /// A closure to trigger when any of the `callers` are called.
    ///
    /// # Arguments
    ///
    /// - 1: A [`&[String]`][String] representing the flags that have
    ///   been passed to the function.
    /// - 2: A [`&[String]`][String] representing the arguments to be
    ///   read by the function.
    ///
    /// # Returns
    ///
    /// A [`Result<Option<String>, String>`]. [`Ok(Some(String))`] is
    /// an outcome that could be used to chain multiple commands.
    /// The [`String`] in [`Err(String)`] is used to tell the user
    /// what went wrong while running the command, and possibly to
    /// show a message somewhere on Parsec.
    function: Box<dyn FnMut(&[String], &[String]) -> Result<Option<String>, String>>,
    /// A list of [`String`]s that act as callers for this `Command`.
    callers: Vec<String>
}

impl Command {
    /// Returns a new instance of [`Command`].
    ///
    /// The first parameter is the function that will be triggered
    /// through any of the keywords in the second parameter.
    pub fn new(
        function: impl FnMut(&[String], &[String]) -> Result<Option<String>, String> + 'static,
        callers: Vec<String>
    ) -> Self {
        if let Some(wrong_caller) =
            callers.iter().find(|caller| caller.split_whitespace().count() != 1)
        {
            panic!("Command caller \"{wrong_caller}\" is not a singular word.");
        }
        Self {
            function: Box::new(function),
            callers
        }
    }

    /// Executes the inner function if the `caller` matches any of the
    /// callers in [`self`].
    fn try_exec(
        &mut self, caller: &String, flags: &[String], args: &[String]
    ) -> Result<Option<String>, CommandErr> {
        if self.callers.contains(&caller) {
            return (self.function)(flags, args).map_err(|err| CommandErr::Failed(err));
        } else {
            return Err(CommandErr::NotFound(caller.clone()));
        }
    }

    /// The list of callers that will trigger this command.
    fn callers(&self) -> &[String] {
        &self.callers
    }
}

/// A list of [`Command`]s, meant to be used in a [`CommandLine<U>`]
/// or in hooks (TODO).
#[derive(Default)]
pub struct Commands(Vec<Command>);

impl Commands {
    /// Returns a new instance of [`Commands`].
    pub fn new() -> Self {
        Commands(Vec::new())
    }

    /// Parses a [`String`] and tries to execute a [`Command`].
    ///
    /// The [`ToString`] will be parsed by separating the first word
    /// as the caller, while the rest of the words are treated as
    /// args.
    pub(crate) fn try_exec(
        &mut self, command: impl ToString
    ) -> Result<Option<String>, CommandErr> {
        let command = command.to_string();
        let mut command = command.split_whitespace().map(|word| String::from(word));

        let caller = command.next().ok_or(CommandErr::Empty)?;
        let args = command.collect::<Vec<String>>();

        for command in &mut self.0 {
            let result = command.try_exec(&caller, &[], args.as_slice());
            let Err(CommandErr::NotFound(_)) = result else {
                return result;
            };
        }

        Err(CommandErr::NotFound(caller))
    }

    /// Tries to add a new [`Command`].
    ///
    /// Returns an [`Err`] if any of the callers of the [`Command`]
    /// are already callers for other commands.
    pub fn try_add(&mut self, command: Command) -> Result<(), CommandErr> {
        let mut new_callers = command.callers().iter();

        for caller in self.0.iter().map(|cmd| cmd.callers()).flatten() {
            if new_callers.any(|new_caller| new_caller == caller) {
                return Err(CommandErr::AlreadyExists(caller.clone()));
            }
        }

        self.0.push(command);

        Ok(())
    }
}

/// An error representing a failure in executing a [`Command`].
#[derive(Debug)]
pub enum CommandErr {
    AlreadyExists(String),
    NotFound(String),
    Failed(String),
    Empty
}

impl std::fmt::Display for CommandErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandErr::AlreadyExists(caller) => {
                f.write_fmt(format_args!("The caller \"{}\" already exists.", caller))
            }
            CommandErr::NotFound(caller) => {
                f.write_fmt(format_args!("The caller \"{}\" was not found.", caller))
            }
            CommandErr::Failed(failure) => f.write_str(failure),
            CommandErr::Empty => f.write_str("No caller supplied.")
        }
    }
}

impl std::error::Error for CommandErr {}
