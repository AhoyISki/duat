//! An [`ActionableWidget<U>`] capable of running [`Command`]s.
//!
//! This widget is capable of running [`Command`]s that are defined
//! and stored in the [`Commands`] struct. It does so by treating the
//! first word as the `caller` for the [`Command`], while the other
//! words are treated as `arguments` for said [`Command`].
//!
//! There are plans on creating a simple interface for `flags` in the
//! near future.
//!
//! There are also plans to permit overriding [`CommandLine<U>`]'s
//! behaviour, such as making it search for text in a [`Text<U>`], in
//! real time.
//!
//! Currently, you can also change the prompt of a [`CommandLine<U>`],
//! by running the `set-prompt` [`Command`].
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
/// For example, if you're creating a [`Command`] that acts on a
/// [`NormalWidget<U>`], you should avoid moving the entire widget to
/// a closure:
///
/// ```rust
/// struct MyWidget<U>
/// where
/// 	U: Ui
/// {
/// 	text: Text<U>,
/// 	other_field: String,
/// 	relevant_field: Arc<AtomicBool>>
/// }
/// ```
///
/// In this case, assuming that you will add the [`Command`]s through
/// a function that takes, as parameters, one `RwData<MyWidget<U>` and
/// one [`&mut Commands`][Commands], the function should look like
/// this:
///
/// ```rust
/// # use crate::ui::Ui;
/// # struct MyWidget<U>
/// # where
/// #     U: Ui
/// # {
/// #     text: Text<U>,
/// #     other_field: String,
/// #     relevant_field: Arc<AtomicBool>>
/// # }
/// fn add_commands<U>(
///     widget: RwData<MyWidget<U>>, commands: &mut Commands
/// ) -> Result<(), CommandError>
/// where
///     U: Ui
/// {
///     let relevant_field = widget.relevant_field.clone();
///     let command = Command::new(
///         move |flags, args| {
///             if relevant_field.load(std::Sync::Ordering::Acquire) {
///                 // Function internals.
///             }
///         },
///         vec![String::from("my-function-caller")]
///     );
///     commands.try_add(command);
/// }
/// ```
///
/// Instead of this:
///
/// ```rust
/// # use crate::ui::Ui;
/// # struct MyWidget<U>
/// # where
/// #     U: Ui
/// # {
/// #     text: Text<U>,
/// #     other_field: String,
/// #     relevant_field: Arc<AtomicBool>>
/// # }
/// fn add_commands<U>(
///     widget: RwData<MyWidget<U>>, commands: &mut Commands
/// ) -> Result<(), CommandError>
/// where
///     U: Ui
/// {
///     let command = Command::new(
///         move |flags, args| {
///             if widget
///                 .read()
///                 .relevant_field
///                 .load(std::Sync::Ordering::Acquire)
///             {
///                 // Function internals.
///             }
///         },
///         vec![String::from("my-function-caller")]
///     );
///     commands.try_add(command);
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
    ) -> Result<Option<String>, CommandError> {
        if self.callers.contains(&caller) {
            return (self.function)(flags, args).map_err(|err| CommandError::Failed(err));
        } else {
            return Err(CommandError::NotFound(caller.clone()));
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
    pub(crate) fn try_parse(
        &mut self, command: impl ToString
    ) -> Result<Option<String>, CommandError> {
        let command = command.to_string();
        let mut command = command.split_whitespace().map(|word| String::from(word));

        let caller = command.next().ok_or(CommandError::Empty)?;
        let args = command.collect::<Vec<String>>();

        for command in &mut self.0 {
            let result = command.try_exec(&caller, &[], args.as_slice());
            let Err(CommandError::NotFound(_)) = result else {
                return result;
            };
        }

        Err(CommandError::NotFound(caller))
    }

    /// Tries to add a new [`Command`].
    ///
    /// Returns an [`Err`] if any of the callers of the [`Command`]
    /// are already callers for other commands.
    pub(crate) fn try_add(&mut self, command: Command) -> Result<(), CommandError> {
        let mut new_callers = command.callers().iter();

        for caller in self.0.iter().map(|cmd| cmd.callers()).flatten() {
            if new_callers.any(|new_caller| new_caller == caller) {
                return Err(CommandError::AlreadyExists(caller.clone()));
            }
        }

        self.0.push(command);

        Ok(())
    }
}

/// An [`ActionableWidget<U>`] whose primary purpose is to execute
/// [`Command`]s.
///
/// While this is the primary purpose of the [`CommandLine<U>`], in
/// the future, it will be able to change its functionality to, for
/// example, search for pieces of text on a
/// [`FileWidget<U>`][crate::widgets::FileWidget] in real time.
pub struct CommandLine<U>
where
    U: Ui
{
    text: Text<U>,
    print_info: U::PrintInfo,
    cursor: [Cursor; 1],
    commands: RwData<Commands>,
    prompt: RwData<String>
}

impl<U> CommandLine<U>
where
    U: Ui + Default + 'static
{
    /// Returns a function that outputs a [`CommandLine<U>`] with a
    /// custom prompt.
    pub fn prompt_fn(prompt: impl ToString) -> Box<dyn FnOnce(&Manager<U>, PushSpecs) -> Widget<U>> {
        let prompt = prompt.to_string();
        Box::new(move |manager, _| {
            let command_line = CommandLine::<U> {
                text: Text::default_string(),
                print_info: U::PrintInfo::default(),
                cursor: [Cursor::default()],
                commands: manager.commands(),
                prompt: RwData::new(prompt)
            };

            add_commands(manager, &command_line);

            Widget::actionable(Arc::new(RwLock::new(command_line)), Box::new(|| false))
        })
    }

    /// Returns a function that outputs a [`CommandLine<U>`] with
    /// `":"` as a prompt.
    pub fn default_fn() -> Box<dyn FnOnce(&Manager<U>, PushSpecs) -> Widget<U>> {
        Box::new(move |manager, _| {
            let command_line = CommandLine::<U> {
                text: Text::default_string(),
                print_info: U::PrintInfo::default(),
                cursor: [Cursor::default()],
                commands: manager.commands(),
                prompt: RwData::new(String::from(":"))
            };

            add_commands(manager, &command_line);

            Widget::actionable(Arc::new(RwLock::new(command_line)), Box::new(|| false))
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
        Editor::new(&mut self.cursor[0], &mut self.text, edit_accum, None, None)
    }

    fn mover<'a>(&'a mut self, _: usize, label: &'a U::Label) -> Mover<U> {
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
        self.text = Text::new_string(&self.prompt);
        let chars = self.prompt.read().chars().count() as isize;
        self.cursor[0].move_hor::<U>(chars, &self.text, label, &PrintCfg::default());
        self.text.add_cursor_tags(self.cursor.as_slice(), 0);
    }

    fn on_unfocus(&mut self, _label: &U::Label) {
        let text = std::mem::replace(&mut self.text, Text::default_string());
        self.cursor[0] = Cursor::default();
        self.text.remove_cursor_tags(self.cursor.as_slice());

        // A '\n' indicates that the command has been "entered".
        if let Some('\n') = text.iter_chars_at(0).last() {
            let cmd = text.iter_chars_at(self.prompt.read().chars().count()).collect::<String>();
            let _ = self.commands.write().try_parse(cmd);
        };
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

/// An error representing a failure in executing a [`Command`].
#[derive(Debug)]
pub enum CommandError {
    AlreadyExists(String),
    NotFound(String),
    Failed(String),
    Empty
}

impl Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandError::AlreadyExists(caller) => {
                f.write_fmt(format_args!("The caller \"{}\" already exists.", caller))
            }
            CommandError::NotFound(caller) => {
                f.write_fmt(format_args!("The caller \"{}\" was not found.", caller))
            }
            CommandError::Failed(failure) => f.write_str(failure),
            CommandError::Empty => f.write_str("No caller supplied.")
        }
    }
}

impl Error for CommandError {}

/// Adds the commands of the [`CommandLine<U>`] to the [`Manager`]'s
/// [`Commands`].
fn add_commands<U>(manager: &Manager<U>, command_line: &CommandLine<U>)
where
    U: Ui
{
    let prompt = command_line.prompt.clone();
    let set_prompt = Command::new(
        move |_, new_prompt| {
            *prompt.write() = new_prompt.first().cloned().unwrap_or(String::from(""));
            Ok(None)
        },
        vec![String::from("set-prompt")]
    );

    manager.commands.write().try_add(set_prompt).unwrap();
}
