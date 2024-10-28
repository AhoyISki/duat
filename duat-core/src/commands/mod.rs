//! Interaction with Duat through commands
//!
//! TO BE DONE
use std::{
    any::type_name,
    collections::HashMap,
    fmt::Display,
    sync::{Arc, LazyLock},
};

pub use self::{
    control::*,
    global::*,
    parameters::{Args, Flags, split_flags_and_args},
};
use crate::{
    Error, context,
    data::{RwData, RwLock},
    input::Cursors,
    text::{Text, err, ok},
    ui::{Ui, Window},
    widgets::{File, Node, Widget},
};

mod control;
mod parameters;

mod global {
    use super::{Args, CmdResult, Commands, Flags, Result};
    use crate::{input::Cursors, text::Text, ui::Ui, widgets::Widget};

    static COMMANDS: Commands = Commands::new();

    /// Canonical way to quit Duat.
    ///
    /// By calling the quit command, all threads will finish their
    /// tasks, and then Duat will execute a program closing
    /// function, as defined by the [`Ui`].
    pub fn quit() {
        COMMANDS.run("quit").unwrap();
    }

    /// Switches to/opens a [`File`] with the given name.
    ///
    /// If you wish to specifically switch to files that are already
    /// open, use [`buffer`].
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`File`]: crate::widgets::File
    pub fn edit(file: impl std::fmt::Display) -> Result<Option<Text>> {
        COMMANDS.run(format!("edit {file}"))
    }

    /// Switches to a [`File`] with the given name.
    ///
    /// If there is no file open with that name, does nothing. Use
    /// [`edit`] if you wish to open files.
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`File`]: crate::widgets::File
    pub fn buffer(file: impl std::fmt::Display) -> Result<Option<Text>> {
        COMMANDS.run(format!("buffer {file}"))
    }

    /// Switches to the next [`File`].
    ///
    /// This function will only look at files that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`next_global_file`].
    ///
    /// [`File`]: crate::widgets::File
    pub fn next_file() -> Result<Option<Text>> {
        COMMANDS.run("next-file")
    }

    /// Switches to the previous [`File`].
    ///
    /// This function will only look at files that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`prev_global_file`].
    ///
    /// [`File`]: crate::widgets::File
    pub fn prev_file() -> Result<Option<Text>> {
        COMMANDS.run("prev-file")
    }

    /// Switches to the next [`File`].
    ///
    /// This function will look for files in all windows. If you want
    /// to limit the search to just the current window, use
    /// [`next_file`].
    ///
    /// [`File`]: crate::widgets::File
    pub fn next_global_file() -> Result<Option<Text>> {
        COMMANDS.run("next-file --global")
    }

    /// Switches to the previous [`File`].
    ///
    /// This function will look for files in all windows. If you want
    /// to limit the search to just the current window, use
    /// [`prev_file`].
    ///
    /// [`File`]: crate::widgets::File
    pub fn prev_global_file() -> Result<Option<Text>> {
        COMMANDS.run("prev-file --global")
    }

    /// Tries to alias a `caller` to an existing `command`.
    ///
    /// Returns an [`Err`] if the `caller` is already a caller for
    /// another command, or if `command` is not a real caller to an
    /// existing command.
    pub fn alias(alias: impl ToString, command: impl ToString) -> Result<Option<Text>> {
        COMMANDS.alias(alias, command)
    }

    /// Runs a full command, with a caller, [`Flags`], and [`Args`].
    ///
    /// When running the command, the ordering of flags does not
    /// matter, as long as they are placed before the arguments to the
    /// command.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::commands;
    /// commands::run("set-prompt new-prompt");
    /// ```
    ///
    /// In this case we're running a command that will affect the most
    /// relevant [`CommandLine`]. See [`add_for_widget`] for
    /// more information.
    ///
    /// [`CommandLine`]: crate::widgets::CommandLine
    pub fn run(call: impl std::fmt::Display) -> Result<Option<Text>> {
        COMMANDS.run(call)
    }

    /// Like [`run`], but notifies the result, not returning it
    pub fn run_notify(call: impl std::fmt::Display) -> Result<Option<Text>> {
        COMMANDS.run_notify(call)
    }

    /// Adds a command to the global list of commands.
    ///
    /// This command cannot take any arguments beyond the [`Flags`]
    /// and [`Args`], so any mutation of state must be done
    /// through captured variables, usually in the form of
    /// [`RwData`]s.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::{
    ///     commands, data::RwData, hooks::{self, OnWindowOpen}, ui::Ui, widgets::status
    /// };
    /// # fn test<U: Ui>() {
    /// let var = RwData::new(35);
    ///
    /// commands::add(["set-var"], {
    ///     let var = var.clone();
    ///     move |_flags, mut args| {
    ///         let value: usize = args.next_as()?;
    ///         *var.write() = value;
    ///
    ///         Ok(None)
    ///     }
    /// });
    ///
    /// hooks::add::<OnWindowOpen<U>>(move |builder| {
    ///     let var = var.clone();
    ///     builder.push(status!("The value is currently " var));
    /// });
    /// # }
    /// ```
    ///
    /// Since `var` is an [`RwData`], it will be updated
    /// automatically in the [`StatusLine`]
    ///
    /// [`StatusLine`]: https://docs.rs/duat/latest/duat/widgets/struct.StatusLine.html
    /// [`RoData`]: crate::data::RoData
    pub fn add(
        callers: impl IntoIterator<Item = impl ToString>,
        f: impl FnMut(Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        COMMANDS.add(callers, f)
    }

    /// Adds a command that can mutate a widget of the given type,
    /// along with its associated [`dyn Area`].
    ///
    /// This command will look for the [`Widget`] in the
    /// following order:
    ///
    /// 1. Any widget directly attached to the current file.
    /// 2. One other instance in the active window.
    /// 3. Instances in other windows.
    ///
    /// Keep in mind that this command will always execute on the
    /// first widget found.
    ///
    /// This search algorithm allows a more versatile configuration of
    /// widgets, for example, one may have a [`CommandLine`] per
    /// [`File`], or one singular [`CommandLine`] that acts upon
    /// all files in the window, and both would respond correctly
    /// to the `"set-prompt"` command.
    ///
    /// # Examples
    ///
    /// In this example, we'll create a simple `Timer` widget:
    ///
    /// ```rust
    /// // Required feature for widgets.
    /// # use std::{
    /// #     sync::{atomic::{AtomicBool, Ordering}, Arc}, marker::PhantomData,
    /// #     time::{Duration, Instant}
    /// # };
    /// # use duat_core::{
    /// #     commands, forms::{self, Form}, text::{text, Text, AlignCenter},
    /// #     ui::{Area, PushSpecs, Ui}, widgets::{Widget, WidgetCfg},
    /// # };
    /// struct TimerCfg<U>(PhantomData<U>);
    ///
    /// impl<U: Ui> WidgetCfg<U> for TimerCfg<U> {
    ///     type Widget = Timer;
    ///
    ///     fn build(self, _is_file: bool) -> (Timer, impl Fn() -> bool, PushSpecs) {
    ///         let widget = Timer {
    ///             text: text!(AlignCenter [Counter] 0 [] "ms"),
    ///             instant: Instant::now(),
    ///             // No need to use an `RwData`, since
    ///             // `RwData::has_changed` is never called.
    ///             running: Arc::new(AtomicBool::new(false)),
    ///         };
    ///
    ///         let checker = {
    ///             let p = duat_core::periodic_checker(Duration::from_millis(100));
    ///             let running = widget.running.clone();
    ///             move || p() && running.load(Ordering::Relaxed)
    ///         };
    ///
    ///         let specs = PushSpecs::below().with_ver_len(1.0);
    ///
    ///         (widget, checker, specs)
    ///     }
    /// }
    ///
    /// struct Timer {
    ///     text: Text,
    ///     instant: Instant,
    ///     running: Arc<AtomicBool>,
    /// }
    ///
    /// impl<U: Ui> Widget<U> for Timer {
    ///     type Cfg = TimerCfg<U>;
    ///
    ///     fn cfg() -> Self::Cfg {
    ///         TimerCfg(PhantomData)
    ///     }
    ///     
    ///     fn update(&mut self, _area: &U::Area) {
    ///         if self.running.load(Ordering::Relaxed) {
    ///             let duration = self.instant.elapsed();
    ///             let time = format!("{:.3?}", duration);
    ///             self.text = text!(AlignCenter [Counter] time [] "ms");
    ///         }
    ///     }
    ///     // ...
    /// #    fn text(&self) -> &Text {
    /// #        &self.text
    /// #    }
    /// #    fn text_mut(&mut self) -> &mut Text {
    /// #        &mut self.text
    /// #    }
    /// #    fn once() {}
    /// }
    /// ```
    ///
    /// Next, we'll add three commands for this widget, "`play`",
    /// "`pause`" and "`reset`". The best place to add them is in the
    /// [`once`] function of [`Widget`]s
    ///
    /// ```rust
    /// // Required feature for widgets.
    /// # use std::{
    /// #     sync::{atomic::{AtomicBool, Ordering}, Arc}, marker::PhantomData,
    /// #     time::{Duration, Instant}
    /// # };
    /// # use duat_core::{
    /// #     commands, forms::{self, Form}, text::{text, Text, AlignCenter},
    /// #     ui::{Area, PushSpecs, Ui}, widgets::{Widget, WidgetCfg},
    /// # };
    /// # struct TimerCfg<U>(PhantomData<U>);
    /// # impl<U: Ui> WidgetCfg<U> for TimerCfg<U> {
    /// #     type Widget = Timer;
    /// #     fn build(self, _is_file: bool) -> (Timer, impl Fn() -> bool, PushSpecs) {
    /// #         let widget = Timer {
    /// #             text: text!(AlignCenter [Counter] 0 [] "ms"),
    /// #             instant: Instant::now(),
    /// #             running: Arc::new(AtomicBool::new(false)),
    /// #         };
    /// #         (widget, || false, PushSpecs::below().with_ver_len(1.0))
    /// #     }
    /// # }
    /// # struct Timer {
    /// #     text: Text,
    /// #     instant: Instant,
    /// #     running: Arc<AtomicBool>,
    /// # }
    /// impl<U: Ui> Widget<U> for Timer {
    /// #    type Cfg = TimerCfg<U>;
    /// #    fn cfg() -> Self::Cfg {
    /// #        TimerCfg(PhantomData)
    /// #    }
    /// #    fn update(&mut self, _area: &U::Area) {
    /// #    }
    /// #    fn text(&self) -> &Text {
    /// #        &self.text
    /// #    }
    /// #    fn text_mut(&mut self) -> &mut Text {
    /// #        &mut self.text
    /// #    }
    ///     // ...
    ///     fn once() {
    ///         forms::set_weak("Counter", Form::green());
    ///
    ///         commands::add_for_widget::<Timer, U>(
    ///             ["play"],
    ///             |timer, _area, _cursors, _flags, _args| {
    ///                 timer.running.store(true, Ordering::Relaxed);
    ///
    ///                 Ok(None)
    ///             })
    ///             .unwrap();
    ///
    ///         commands::add_for_widget::<Timer, U>(
    ///             ["pause"],
    ///             |timer, _, _, _, _| {
    ///                 timer.running.store(false, Ordering::Relaxed);
    ///
    ///                 Ok(None)
    ///             })
    ///             .unwrap();
    ///
    ///         commands::add_for_widget::<Timer, U>(
    ///             ["reset"],
    ///             |timer, _, _, _, _| {
    ///                 timer.instant = Instant::now();
    ///
    ///                 Ok(None)
    ///             })
    ///             .unwrap();
    ///     }
    /// }
    /// ```
    ///
    /// I also added a [`Form`] in the [`once`] function. You should
    /// use [`forms::set_weak`] instead of [`forms::set`], as to not
    /// interfere with the user configuration.
    ///
    /// [`dyn Area`]: crate::ui::Area
    /// [`File`]: crate::widgets::File
    /// [`Session`]: crate::session::Session
    /// [`CommandLine`]: crate::widgets::CommandLine
    /// [`once`]: Widget::once
    /// [`Form`]: crate::forms::Form
    /// [`forms::set`]: crate::forms::set
    /// [`forms::set_weak`]: crate::forms::set_weak
    pub fn add_for_widget<W: Widget<U>, U: Ui>(
        callers: impl IntoIterator<Item = impl ToString>,
        f: impl FnMut(&mut W, &U::Area, &mut Option<Cursors>, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        COMMANDS.add_for_widget(callers, f)
    }

    pub(crate) fn caller_exists(caller: &str) -> bool {
        COMMANDS.caller_exists(caller)
    }
}

/// A list of commands.
///
/// This list contains all of the commands that have been
/// added to Duat, as well as info on the current [`File`],
/// [widget] and all of the [windows].
///
/// [`File`]: crate::widgets::File
/// [widget]: crate::widgets::ActiveWidget
/// [windows]: crate::ui::Window
struct Commands(LazyLock<RwData<InnerCommands>>);

impl Commands {
    /// Returns a new instance of [`Commands`].
    #[doc(hidden)]
    const fn new() -> Self {
        Self(LazyLock::new(|| {
            let inner = RwData::new(InnerCommands {
                list: Vec::new(),
                aliases: HashMap::new(),
            });

            let alias = {
                let inner = inner.clone();
                Command::new(["alias"], move |flags, mut args| {
                    if !flags.is_empty() {
                        Err(err!(
                            "An alias cannot take any flags, try moving them after the command, \
                             like \"alias my-alias my-caller --foo --bar\", instead of \"alias \
                             --foo --bar my-alias my-caller\""
                        ))
                    } else {
                        let alias = args.next()?.to_string();
                        let args: String = args.collect();

                        Ok(inner.write().try_alias(alias, args)?)
                    }
                })
            };

            inner.write().try_add(alias).unwrap();
            inner
        }))
    }

    /// Aliases a command to a specific word
    fn alias(&self, alias: impl ToString, command: impl ToString) -> Result<Option<Text>> {
        self.0.write().try_alias(alias, command)
    }

    /// Runs a command from a call
    fn run(&self, call: impl Display) -> Result<Option<Text>> {
        let call = call.to_string();
        let mut args = call.split_whitespace();
        let caller = args.next().ok_or(Error::Empty)?.to_string();

        let (command, call) = self.0.inspect(|inner| {
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

        command.try_exec(Flags::new(&flags), args)
    }

    /// Runs a command and notifies its result
    fn run_notify(&self, call: impl Display) -> Result<Option<Text>> {
        let ret = self.run(call);
        match ret.as_ref() {
            Ok(Some(ok)) => context::notify(ok.clone()),
            Err(err) => context::notify(err.clone().into()),
            _ => {}
        }
        ret
    }

    /// Adds a command to the list of commands
    fn add(
        &self,
        callers: impl IntoIterator<Item = impl ToString>,
        f: impl FnMut(Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        let command = Command::new(callers, f);
        self.0.write().try_add(command)
    }

    /// Adds a command for a widget of type `W`
    fn add_for_widget<W: Widget<U>, U: Ui>(
        &'static self,
        callers: impl IntoIterator<Item = impl ToString>,
        mut f: impl FnMut(&mut W, &U::Area, &mut Option<Cursors>, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        let cur_file = context::inner_cur_file::<U>();
        let windows = context::windows::<U>();
        let w = context::cur_window();

        let command = Command::new(callers, move |flags, args| {
            cur_file
                .mutate_related_widget::<W, CmdResult>(|widget, area, cursors| {
                    f(widget, area, cursors, flags, args.clone())
                })
                .unwrap_or_else(|| {
                    let windows = windows.read();

                    if windows.is_empty() {
                        return Err(err!(
                            "Widget command executed before the " [*a] "Ui" []
                            " was initiated, try executing after " [*a] "OnUiStart" []
                        ));
                    }

                    let (_, node) = widget_entry::<W, U>(&windows, w)?;
                    let (w, a, c) = node.as_active();
                    let mut c = c.write();

                    w.mutate_as(|w| f(w, a, &mut c, flags, args)).unwrap()
                })
        });

        self.0.write().try_add(command)
    }

    /// Checks if a caller/alias exists or not
    fn caller_exists(&self, caller: &str) -> bool {
        let inner = self.0.read();
        inner.aliases.contains_key(caller)
            || inner
                .list
                .iter()
                .flat_map(|cmd| cmd.callers.iter())
                .any(|c| c == caller)
    }
}

/// The standard error that should be returned when [`run`]ning
/// commands.
///
/// This error _must_ include an error message in case of failure. It
/// may also include a success message, but that is not required.
pub type CmdResult = std::result::Result<Option<Text>, Text>;

/// A function that can be called by name.
#[derive(Clone)]
struct Command {
    f: RwData<dyn FnMut(Flags, Args) -> CmdResult>,
    callers: Arc<[String]>,
}

impl Command {
    /// Returns a new instance of command.
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
struct InnerCommands {
    list: Vec<Command>,
    aliases: HashMap<String, (Command, String)>,
}

impl InnerCommands {
    /// Tries to add the given command to the list.
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
                Some((_, prev_call)) => ok!(
                    "Aliased " [*a] alias []
                    " from " [*a] prev_call []
                    " to " [*a] call [] "."
                ),
                None => ok!("Aliased " [*a] alias [] " to " [*a] call [] "."),
            }
        } else {
            Err(Error::CallerNotFound(caller))
        }
    }
}

pub type Result<T> = crate::Result<T, ()>;

fn file_entry<'a, U: Ui>(
    windows: &'a [Window<U>],
    name: &str,
) -> std::result::Result<(usize, &'a Node<U>), Text> {
    windows
        .iter()
        .enumerate()
        .flat_map(window_index_widget)
        .find(|(_, node)| {
            matches!(
                node.inspect_as::<File, bool>(|file| file.name() == name),
                Some(true)
            )
        })
        .ok_or_else(|| err!("File with name " [*a] name [] " not found."))
}

fn widget_entry<W: Widget<U>, U: Ui>(
    windows: &[Window<U>],
    w: usize,
) -> std::result::Result<(usize, &Node<U>), Text> {
    let cur_file = context::cur_file::<U>().unwrap();

    if let Some(node) = cur_file.get_related_widget::<W>() {
        windows
            .iter()
            .enumerate()
            .flat_map(window_index_widget)
            .find(|(_, n)| n.ptr_eq(node.widget()))
    } else {
        iter_around(windows, w, 0).find(|(_, node)| node.data_is::<W>())
    }
    .ok_or(err!("No widget of type " [*a] { type_name::<W>() } [] " found."))
}

fn window_index_widget<U: Ui>(
    (index, window): (usize, &Window<U>),
) -> impl DoubleEndedIterator<Item = (usize, &Node<U>)> {
    window.nodes().map(move |entry| (index, entry))
}

fn iter_around<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, &Node<U>)> + '_ {
    let prev_len: usize = windows.iter().take(window).map(Window::len_widgets).sum();

    windows
        .iter()
        .enumerate()
        .skip(window)
        .flat_map(window_index_widget)
        .skip(widget + 1)
        .chain(
            windows
                .iter()
                .enumerate()
                .take(window + 1)
                .flat_map(window_index_widget)
                .take(prev_len + widget),
        )
}

fn iter_around_rev<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, &Node<U>)> {
    let next_len: usize = windows.iter().skip(window).map(Window::len_widgets).sum();

    windows
        .iter()
        .enumerate()
        .rev()
        .skip(windows.len() - window)
        .flat_map(move |(i, win)| {
            window_index_widget((i, win))
                .rev()
                .skip(win.len_widgets() - widget)
        })
        .chain(
            windows
                .iter()
                .enumerate()
                .rev()
                .take(windows.len() - window)
                .flat_map(move |(i, win)| window_index_widget((i, win)).rev())
                .take(next_len - (widget + 1)),
        )
}
