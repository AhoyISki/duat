use std::{
    collections::HashMap,
    mem::MaybeUninit,
    sync::{Arc, LazyLock, RwLock}, fmt::Display,
};

use super::{Args, CmdResult, Error, Flags, Result};
use crate::{
    data::{Data, RwData},
    text::{text, Text},
    ui::{Area, Ui, Window},
    widgets::PassiveWidget,
    CURRENT_FILE, CURRENT_WIDGET,
};

/// A struct representing flags passed down to [`Command`]s when
/// running them.
pub struct InnerFlags<'a> {
    short: String,
    long: Vec<&'a str>,
}

impl<'a> InnerFlags<'a> {
    /// Checks if all of the [`char`]s in the `short` passed.
    pub fn short(&self, short: impl AsRef<str>) -> bool {
        let mut all_chars = true;
        for char in short.as_ref().chars() {
            all_chars &= self.short.contains(char);
        }
        all_chars
    }

    /// Returns `true` if the `long` flag was passed.
    pub fn long(&self, flag: impl AsRef<str>) -> bool {
        self.long.contains(&flag.as_ref())
    }

    /// Returns `true` if no flags have been passed.
    pub fn is_empty(&self) -> bool {
        self.short.is_empty() && self.long.is_empty()
    }
}

/// A function that can be called by name.
#[derive(Clone)]
struct Command {
    f: RwData<dyn FnMut(Flags, Args) -> CmdResult>,
    callers: Arc<[String]>,
}

impl Command {
    /// Returns a new instance of [`Command`].
    fn new<F>(callers: impl IntoIterator<Item = impl ToString>, f: F) -> Self
    where
        F: FnMut(Flags, Args) -> CmdResult + 'static,
    {
        let callers: Arc<[String]> = callers
            .into_iter()
            .map(|caller| caller.to_string())
            .collect();

        if let Some(caller) = callers
            .iter()
            .find(|caller| caller.split_whitespace().count() != 1)
        {
            panic!("Command caller \"{caller}\" contains more than one word.");
        }
        Self {
            f: RwData::new_unsized::<F>(Arc::new(RwLock::new(f))),
            callers,
        }
    }

    /// Executes the inner function if the `caller` matches any of
    /// the callers in [`self`].
    fn try_exec(&self, flags: Flags, args: Args<'_>) -> Result<Option<Text>> {
        (self.f.write())(flags, args).map_err(Error::CommandFailed)
    }

    /// The list of callers that will trigger this command.
    fn callers(&self) -> &[String] {
        &self.callers
    }
}

unsafe impl Send for Command {}
unsafe impl Sync for Command {}

/// A list of [`Command`]s.
pub struct Commands {
    inner: LazyLock<RwData<InnerCommands>>,
    widget_getter: RwLock<MaybeUninit<RwData<dyn WidgetGetter>>>,
}

impl Commands {
    /// Returns a new instance of [`Commands`].
    pub const fn new() -> Self {
        Self {
            inner: LazyLock::new(|| {
                let inner = RwData::new(InnerCommands {
                    list: Vec::new(),
                    aliases: HashMap::new(),
                });

                let alias = {
                    let inner = inner.clone();
                    Command::new(["alias"], move |flags, mut args| {
                        if !flags.is_empty() {
                            Err(text!(
                                "An alias cannot take any flags, try moving them after the \
                                 command, like \"alias my-alias my-caller --foo --bar\", instead \
                                 of \"alias --foo --bar my-alias my-caller\""
                            ))
                        } else {
                            let alias = args.next()?.to_string();
                            let args: String = args.collect();

                            inner
                                .write()
                                .try_alias(alias, args)
                                .map_err(Error::into_text)
                        }
                    })
                };

                let quit = {
                    Command::new(["quit", "q"], move |_, _| {
                        super::quit();
                        Ok(None)
                    })
                };

                inner.write().try_add(quit).unwrap();
                inner.write().try_add(alias).unwrap();

                inner
            }),
            widget_getter: RwLock::new(MaybeUninit::uninit()),
        }
    }

    /// Runs the given command.
    pub fn run(&self, call: impl Display) -> Result<Option<Text>> {
        let call = call.to_string();
        let mut args = call.split_whitespace();
        let caller = args.next().ok_or(Error::Empty)?.to_string();

        let (command, call) = self.inner.inspect(|inner| {
            if let Some(command) = inner.aliases.get(&caller) {
                let (command, call) = command;
                let mut call = call.clone() + " ";
                call.extend(args);

                Ok((command.clone(), call))
            } else {
                let command = inner
                    .list
                    .iter()
                    .find(|cmd| cmd.callers().contains(&caller))
                    .ok_or(Error::CallerNotFound(caller))?;

                Ok((command.clone(), call.clone()))
            }
        })?;

        let (flags, args) = split_flags_and_args(&call);

        Ok(command.try_exec(Flags(&flags), args).unwrap())
    }

    /// Add a new command to the [`self`].
    pub fn add(
        &self,
        callers: impl IntoIterator<Item = impl ToString>,
        f: impl FnMut(Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        let command = Command::new(callers, f);
        self.inner.write().try_add(command)
    }

    /// Adds a command that will try to affect the currently
    /// active widget or file.
    pub fn add_for_current<T: 'static>(
        &self,
        callers: impl IntoIterator<Item = impl ToString>,
        mut f: impl FnMut(&mut T, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        let command = Command::new(callers, move |flags, args| {
            let result = CURRENT_FILE.mutate_related::<T, CmdResult>(|t| f(t, flags, args.clone()));

            result
                .or_else(|| CURRENT_WIDGET.mutate_as::<T, CmdResult>(|t| f(t, flags, args.clone())))
                .transpose()?
                .ok_or_else(|| {
                    text!(
                        "The current file has no related structs of type {}"
                        { std::any::type_name::<T>() }
                    )
                })
        });

        self.inner.write().try_add(command)
    }

    /// Adds a command that will look for a given widget, and then
    /// mutate it and its area.
    pub fn add_for_widget<W: PassiveWidget>(
        &self,
        callers: impl IntoIterator<Item = impl ToString>,
        mut f: impl FnMut(&mut W, &dyn Area, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        let widget_getter = unsafe { self.widget_getter.read().unwrap().assume_init_ref().clone() };

        let command = Command::new(callers, move |flags, args| {
            CURRENT_FILE
                .mutate_related_widget::<W, CmdResult>(|widget, area| {
                    f(widget, area, flags, args.clone())
                })
                .unwrap_or_else(|| {
                    let widget_getter = widget_getter.read();
                    CURRENT_WIDGET.inspect_data(|widget, _| {
                        let widget = widget.clone().to_passive();
                        if let Some((w, a)) = widget_getter.get_from_name(W::type_name(), &widget) {
                            w.mutate_as::<W, CmdResult>(|w| f(w, a, flags, args))
                                .unwrap()
                        } else {
                            let name = W::type_name();
                            Err(text!("No widget of type " [AccentErr] name [] " found"))
                        }
                    })
                })
        });

        self.inner.write().try_add(command)
    }

    /// Adds a [`WidgetGetter`] to [`self`].
    pub fn add_widget_getter<U: Ui>(&self, getter: RwData<Vec<Window<U>>>) {
        let inner_arc = getter.inner_arc().clone() as Arc<RwLock<dyn WidgetGetter>>;
        let getter = RwData::new_unsized::<Window<U>>(inner_arc);
        let mut lock = self.widget_getter.write().unwrap();
        *lock = MaybeUninit::new(getter)
    }

    pub fn try_alias(&self, alias: impl ToString, command: impl ToString) -> Result<Option<Text>> {
        self.inner.write().try_alias(alias, command)
    }
}

struct InnerCommands {
    list: Vec<Command>,
    aliases: HashMap<String, (Command, String)>,
}

impl InnerCommands {
    /// Tries to add the given [`Command`] to the list.
    fn try_add(&mut self, command: Command) -> Result<()> {
        let mut new_callers = command.callers().iter();

        let commands = self.list.iter();
        for caller in commands.flat_map(|cmd| cmd.callers().iter()) {
            if new_callers.any(|new_caller| new_caller == caller) {
                return Err(Error::CallerAlreadyExists(caller.clone()));
            }
        }

        self.list.push(command);

        Ok(())
    }

    /// Tries to alias a full command (caller, flags, and
    /// arguments) to an alias.
    fn try_alias(&mut self, alias: impl ToString, call: impl ToString) -> Result<Option<Text>> {
        let alias = alias.to_string();
        if alias.split_whitespace().count() != 1 {
            return Err(Error::AliasNotSingleWord(alias));
        }

        let call = call.to_string();
        let caller = call
            .split_whitespace()
            .next()
            .ok_or(Error::Empty)?
            .to_string();

        let mut cmds = self.list.iter();

        if let Some(command) = cmds.find(|cmd| cmd.callers().contains(&caller)) {
            let entry = (command.clone(), call.clone());
            match self.aliases.insert(alias.clone(), entry) {
                Some((_, prev_call)) => Ok(Some(text!(
                    "Aliased " [AccentOk] alias []
                    " from " [AccentOk] prev_call []
                    " to " [AccentOk] call [] "."
                ))),
                None => Ok(Some(text!(
                     "Aliased " [AccentOk] alias []
                     " to " [AccentOk] call [] "."
                ))),
            }
        } else {
            Err(Error::CallerNotFound(caller))
        }
    }
}

trait WidgetGetter: Send + Sync {
    fn get_from_name(
        &self,
        type_id: &'static str,
        arc: &dyn Data<dyn PassiveWidget>,
    ) -> Option<(&RwData<dyn PassiveWidget>, &dyn Area)>;
}

impl<U> WidgetGetter for Vec<Window<U>>
where
    U: Ui,
{
    fn get_from_name(
        &self,
        type_name: &'static str,
        widget: &dyn Data<dyn PassiveWidget>,
    ) -> Option<(&RwData<dyn PassiveWidget>, &dyn Area)> {
        let window = self
            .iter()
            .position(|w| w.widgets().any(|(cmp, _)| cmp.ptr_eq(widget)))
            .unwrap();

        let on_window = self[window].widgets();
        let previous = self.iter().take(window).flat_map(|w| w.widgets());
        let following = self.iter().skip(window + 1).flat_map(|w| w.widgets());

        on_window
            .chain(following)
            .chain(previous)
            .find(|(w, _)| w.type_name() == type_name)
            .map(|(w, a)| (w.as_passive(), a as &dyn Area))
    }
}

/// Takes the [`Flags`] from an [`Iterator`] of `args`.
pub fn split_flags_and_args(command: &str) -> (InnerFlags<'_>, Args<'_>) {
    let mut short = String::new();
    let mut long = Vec::new();

    let mut args = command.split_whitespace().peekable();

    args.next();

    while let Some(arg) = args.peek() {
        if let Some(long_arg) = arg.strip_prefix("--") {
            if !long_arg.is_empty() {
                args.next();
                if !long.contains(&long_arg) {
                    long.push(long_arg)
                }
            } else {
                args.next();
                break;
            }
        } else if let Some(short_arg) = arg.strip_prefix('-') {
            args.next();
            for char in short_arg.chars() {
                if !short.contains(char) {
                    short.push(char)
                }
            }
        } else {
            break;
        }
    }

    (InnerFlags { short, long }, Args {
        count: 0,
        expected: None,
        args,
    })
}
