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
    /// A closure to trigger when any of the `callers` are called.
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
    /// Returns a new instance of [`Command`].
    ///
    /// The first parameter is the function that will be triggered
    /// through any of the keywords in the second parameter.
    pub fn new(
        function: Box<dyn FnMut(&[String], &[String]) -> Result<Option<String>, String>>,
        callers: Vec<String>
    ) -> Self {
        if let Some(wrong_caller) =
            callers.iter().find(|caller| caller.split_whitespace().count() != 1)
        {
            panic!("Command caller \"{wrong_caller}\" is not a single word.");
        }
        Self { function, callers }
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

    /// Tries to execute a given `caller`, by checking if any of its
    /// [`Command`]s's callers match it.
    ///
    /// Will return an [`Err`] if the `caller` was not found.
    pub(crate) fn try_exec(
        &mut self, caller: String, flags: Vec<String>, args: Vec<String>
    ) -> Result<Option<String>, CommandError> {
        for command in &mut self.0 {
            let result = command.try_exec(&caller, flags.as_slice(), args.as_slice());
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
    U: Ui + ?Sized
{
    text: Text<U>,
    print_info: U::PrintInfo,
    cursor: [Cursor; 1],
    commands: RwData<Commands>,
    prompt: String
}

impl<U> CommandLine<U>
where
    U: Ui + Default + 'static
{
    /// Returns a function that outputs a [`CommandLine<U>`] with a
    /// custom prompt.
    pub fn prompt_fn(prompt: impl ToString) -> Box<dyn FnOnce(&Manager, PushSpecs) -> Widget<U>> {
        let prompt = prompt.to_string();
        Box::new(move |manager, _| {
            let command_line = Arc::new(RwLock::new(CommandLine::<U> {
                text: Text::default_string(),
                print_info: U::PrintInfo::default(),
                cursor: [Cursor::default()],
                commands: manager.commands(),
                prompt
            }));

            let widget: Arc<RwLock<dyn ActionableWidget<U>>> = command_line.clone();
            add_commands(manager, command_line);

            Widget::actionable(widget, Box::new(|| true))
        })
    }

    /// Returns a function that outputs a [`CommandLine<U>`] with
    /// `":"` as a prompt.
    pub fn default_fn() -> Box<dyn FnOnce(&Manager, PushSpecs) -> Widget<U>> {
        Box::new(move |manager, _| {
            let command_line = Arc::new(RwLock::new(CommandLine::<U> {
                text: Text::default_string(),
                print_info: U::PrintInfo::default(),
                cursor: [Cursor::default()],
                commands: manager.commands(),
                prompt: String::from(":")
            }));

            let widget: Arc<RwLock<dyn ActionableWidget<U>>> = command_line.clone();
            add_commands(manager, command_line);

            Widget::actionable(widget, Box::new(|| true))
        })
    }
}

impl<U> NormalWidget<U> for CommandLine<U>
where
    U: Ui + 'static
{
    fn update(&mut self, _label: &U::Label) {}

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
        let chars = self.prompt.chars().count() as isize;
        self.cursor[0].move_hor::<U>(chars, &self.text, label, &PrintCfg::default());
        self.text.add_cursor_tags(self.cursor.as_slice(), 0);
    }

    fn on_unfocus(&mut self, label: &U::Label) {
        let text = std::mem::replace(&mut self.text, Text::default_string());
        self.cursor[0] = Cursor::default();
        self.text.remove_cursor_tags(self.cursor.as_slice());

        let print_cfg = PrintCfg::default();
        self.print_info.scroll_to_gap(&text, self.cursor[0].caret(), label, &print_cfg);

        // A '\n' indicates that the command has been "entered".
        if let Some('\n') = text.iter_chars_at(0).last() {
            let line = text.iter_chars_at(self.prompt.chars().count()).collect::<String>();
            let mut whole_command = line.split_whitespace().map(|word| String::from(word));

            let Some(command) = whole_command.next() else {
    			return;
    		};

            let mut command_list = self.commands.write();
            let _ = command_list.try_exec(command.clone(), Vec::new(), whole_command.collect());
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
    Failed(String)
}

impl Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandError::AlreadyExists(caller) => {
                f.write_fmt(format_args!("The caller \"{}\" already exists!", caller))
            }
            CommandError::NotFound(caller) => {
                f.write_fmt(format_args!("The caller \"{}\" was not found!", caller))
            }
            CommandError::Failed(failure) => f.write_fmt(format_args!("{}", failure))
        }
    }
}

impl Error for CommandError {}

/// Adds the commands of the [`CommandLine<U>`] to the [`Manager`]'s
/// [`Commands`].
fn add_commands<U>(manager: &Manager, command_line: Arc<RwLock<CommandLine<U>>>)
where
    U: Ui + Default + 'static
{
    let set_prompt = Command::new(
        Box::new(move |prompt, _| {
            let mut command_line = command_line.write().unwrap();
            command_line.prompt = prompt.first().cloned().unwrap_or(String::from(""));
            Ok(None)
        }),
        vec![String::from("set-prompt")]
    );

    manager.commands.write().try_add(set_prompt).unwrap();
}
