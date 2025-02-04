//! Interaction with Duat through commands
//!
//! TO BE REDONE
use std::{
    collections::HashMap,
    fmt::Display,
    ops::Range,
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
    mode::Cursors,
    text::{Text, err, ok},
    ui::Ui,
    widget_entry,
    widgets::Widget,
};

mod control {
    use std::{
        path::PathBuf,
        sync::{
            atomic::{AtomicBool, Ordering},
            mpsc,
        },
    };

    use crate::{
        cmd::{self, Remainder},
        context, iter_around, iter_around_rev, mode,
        text::{Text, err, ok},
        ui::{Event, Ui, Window},
        widgets::File,
    };

    static HAS_ENDED: AtomicBool = AtomicBool::new(false);

    /// Returns `true` if Duat must quit/reload
    ///
    /// You should use this function in order to check if loops inside
    /// of threads should break.
    pub fn has_ended() -> bool {
        HAS_ENDED.load(Ordering::Relaxed)
    }

    /// Ends duat, either for reloading the config, or quitting
    pub(crate) fn end_session() {
        HAS_ENDED.store(true, Ordering::Relaxed);
        while crate::thread::still_running() {
            std::thread::sleep(std::time::Duration::from_micros(500));
        }
    }

    pub(crate) fn add_session_commands<U: Ui>(tx: mpsc::Sender<Event>) -> crate::Result<(), ()> {
        cmd::add!("alias", |flags, alias: &str, command: Remainder| {
            if !flags.is_empty() {
                Err(err!("An alias cannot take any flags"))
            } else {
                Ok(crate::cmd::alias(alias, command)?)
            }
        })?;

        let tx_clone = tx.clone();
        cmd::add!(["quit", "q"], move |_| {
            tx_clone.send(Event::Quit).unwrap();
            Ok(None)
        })?;

        cmd::add!(["write", "w"], move |_flags, paths: Vec<PathBuf>| {
            let file = context::cur_file::<U>()?;

            if paths.is_empty() {
                file.inspect(|file, _, _| {
                    if let Some(name) = file.path_set() {
                        let bytes = file.write()?;
                        ok!("Wrote " [*a] bytes [] " bytes to " [*a] name)
                    } else {
                        Err(err!("Give the file a name, to write it with"))
                    }
                })
            } else {
                file.inspect(|file, _, _| {
                    let mut bytes = 0;
                    for path in &paths {
                        bytes = file.write_to(path)?;
                    }

                    let files_text = {
                        let mut builder = Text::builder();
                        ok!(builder, [*a] { &paths[0] });

                        for path in paths.iter().skip(1).take(paths.len() - 1) {
                            ok!(builder, [] ", " [*a] path)
                        }

                        if paths.len() > 1 {
                            ok!(builder, [] " and " [*a] { paths.last().unwrap() })
                        }

                        builder.finish()
                    };

                    ok!("Wrote " [*a] bytes [] " bytes to " files_text)
                })
            }
        })?;

        let windows = context::windows::<U>();
        cmd::add!(["edit", "e"], move |_, path: PathBuf| {
            let windows = windows.read();

            let name = path
                .file_name()
                .ok_or(err!("No file in path"))?
                .to_string_lossy()
                .to_string();

            if !windows.iter().flat_map(Window::nodes).any(|node| {
                matches!(
                    node.inspect_as::<File, bool>(|f| f.name() == name),
                    Some(true)
                )
            }) {
                tx.send(Event::OpenFile(path.clone())).unwrap();
                return ok!("Opened " [*a] path);
            }

            mode::reset_switch_to::<U>(&name);
            ok!("Switched to " [*a] name)
        })?;

        cmd::add!(["buffer", "b"], move |_, path: PathBuf| {
            let name = path
                .file_name()
                .ok_or(err!("No file in path"))?
                .to_string_lossy()
                .to_string();

            mode::reset_switch_to::<U>(&name);
            ok!("Switched to " [*a] name)
        })?;

        let windows = context::windows();
        cmd::add!("next-file", move |flags| {
            let file = context::cur_file()?;
            let read_windows = windows.read();
            let w = context::cur_window();

            let widget_index = read_windows[w]
                .nodes()
                .position(|node| file.file_ptr_eq(node))
                .unwrap();

            let name = if flags.word("global") {
                iter_around::<U>(&read_windows, w, widget_index)
                    .find_map(|(_, node)| node.inspect_as::<File, String>(|file| file.name()))
                    .ok_or_else(|| err!("There are no other open files"))?
            } else {
                let slice = &read_windows[w..=w];
                iter_around(slice, 0, widget_index)
                    .find_map(|(_, node)| node.inspect_as::<File, String>(|file| file.name()))
                    .ok_or_else(|| err!("There are no other files open in this window"))?
            };

            mode::reset_switch_to::<U>(&name);
            ok!("Switched to " [*a] name)
        })?;

        let windows = context::windows();
        cmd::add!("prev-file", move |flags| {
            let file = context::cur_file()?;
            let windows = windows.read();
            let w = context::cur_window();

            let widget_i = windows[w]
                .nodes()
                .position(|node| file.file_ptr_eq(node))
                .unwrap();

            let name = if flags.word("global") {
                iter_around_rev::<U>(&windows, w, widget_i)
                    .find_map(|(_, node)| node.inspect_as::<File, String>(|file| file.name()))
                    .ok_or_else(|| err!("There are no other open files"))?
            } else {
                let slice = &windows[w..=w];
                iter_around_rev(slice, 0, widget_i)
                    .find_map(|(_, node)| node.inspect_as::<File, String>(|file| file.name()))
                    .ok_or_else(|| err!("There are no other files open in this window"))?
            };

            mode::reset_switch_to::<U>(&name);

            ok!("Switched to " [*a] name)
        })?;

        cmd::add!("colorscheme", |_, scheme: &str| {
            crate::form::set_colorscheme(scheme);
            ok!("Set colorscheme to " [*a] scheme [])
        })?;

        Ok(())
    }
}

mod parameters;

mod global {
    use std::ops::Range;

    use super::{Args, CmdResult, Commands, Flags, Parameter, Result};
    use crate::{mode::Cursors, text::Text, ui::Ui, widgets::Widget};

    static COMMANDS: Commands = Commands::new();

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
    /// # use duat_core::{cmd, data::RwData, hooks::{self, OnWindowOpen}, ui::Ui, widgets::status};
    /// # fn test<U: Ui>() {
    /// let var = RwData::new(35);
    ///
    /// cmd::add!(["set-var"], {
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
    /// widgets, for example, one may have a [`CmdLine`] per
    /// [`File`], or one singular [`CmdLine`] that acts upon
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
    /// #     cmd, form::{self, Form}, text::{text, Text, AlignCenter},
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
    /// #     cmd, form::{self, Form}, text::{text, Text, AlignCenter},
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
    ///         form::set_weak("Counter", Form::green());
    ///
    ///         cmd::add_for::<Timer, U>(
    ///             ["play"],
    ///             |timer, _area, _cursors, _flags, _args| {
    ///                 timer.running.store(true, Ordering::Relaxed);
    ///
    ///                 Ok(None)
    ///             })
    ///             .unwrap();
    ///
    ///         cmd::add_for::<Timer, U>(
    ///             ["pause"],
    ///             |timer, _, _, _, _| {
    ///                 timer.running.store(false, Ordering::Relaxed);
    ///
    ///                 Ok(None)
    ///             })
    ///             .unwrap();
    ///
    ///         cmd::add_for::<Timer, U>(
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
    /// use [`form::set_weak`] instead of [`form::set`], as to not
    /// interfere with the user configuration.
    ///
    /// [`StatusLine`]: https://docs.rs/duat/latest/duat/widgets/struct.StatusLine.html
    /// [`RoData`]: crate::data::RoData
    /// [`RwData`]: crate::data::RwData
    /// [`dyn Area`]: crate::ui::Area
    /// [`File`]: crate::widgets::File
    /// [`Session`]: crate::session::Session
    /// [`CmdLine`]: crate::widgets::CmdLine
    /// [`once`]: Widget::once
    /// [`Form`]: crate::form::Form
    /// [`form::set`]: crate::form::set
    /// [`form::set_weak`]: crate::form::set_weak
    pub macro add(
        $callers:expr, $($mv:ident)? |$flags:pat_param $(, $arg:ident: $t:ty)*| $f:block
    ) {{
			#[allow(unused_variables, unused_mut)]
            let cmd = $($mv)? |$flags: Flags, mut args: Args| {
                $(
                    let $arg: <$t as Parameter>::Returns = <$t as Parameter>::new(&mut args)?;
                )*

                $f
            };

			#[allow(unused_variables, unused_mut)]
            fn param_checker(mut args: Args) -> (Vec<Range<u32>>, Option<Range<u32>>) {
                let mut ok_ranges = Vec::new();

                $(
                    match args.next_as_with_range::<$t>() {
                        Ok((_, range)) => if !range.is_empty() {
                            ok_ranges.push(range);
                        }
                        Err((_, range)) => return (ok_ranges, Some(range))
                    }
                )*

                (ok_ranges, None)
            }

            add_inner($callers, cmd, param_checker)
    }}

    pub macro add_for($ui:ty, $callers:expr, $($mv:ident)? |
        $widget:ident: $w_ty:ty, $area:pat_param, $cursors:pat_param,
        $flags:pat_param $(, $arg:ident: $t:ty)*
    | $f:block) {{
        use crate::mode::Cursors;

		#[allow(unused_variables, unused_mut)]
        let cmd = $($mv)? |
            $widget: &mut $w_ty,
            $area: &<$ui as Ui>::Area,
            $cursors: &mut Cursors,
            $flags: Flags,
            mut args: Args
        | {
            $(
                let $arg: <$t as Parameter>::Returns = <$t as Parameter>::new(&mut args)?;
            )*

            $f
        };

		#[allow(unused_variables, unused_mut)]
        fn param_checker(mut args: Args) -> (Vec<Range<u32>>, Option<Range<u32>>) {
            let mut ok_ranges = Vec::new();

            $(
                match args.next_as_with_range::<$t>() {
                    Ok((_, range)) => if !range.is_empty() {
                        ok_ranges.push(range);
                    }
                    Err((_, range)) => return (ok_ranges, Some(range))
                }
            )*

            (ok_ranges, None)
        }

        add_for_inner::<$w_ty, $ui>($callers, cmd, param_checker)
    }}

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
    /// # use duat_core::cmd;
    /// cmd::run("set-prompt new-prompt");
    /// ```
    ///
    /// In this case we're running a command that will affect the most
    /// relevant [`CmdLine`]. See [`add_for`] for
    /// more information.
    ///
    /// [`CmdLine`]: crate::widgets::CmdLine
    pub fn run(call: impl std::fmt::Display) -> Result<Option<Text>> {
        COMMANDS.run(call)
    }

    /// Like [`run`], but notifies the result, not returning it
    pub fn run_notify(call: impl std::fmt::Display) -> Result<Option<Text>> {
        COMMANDS.run_notify(call)
    }

    pub(crate) fn add_inner<'a>(
        callers: impl super::Caller<'a>,
        cmd: impl FnMut(Flags, Args) -> CmdResult + 'static,
        param_checker: fn(Args) -> (Vec<Range<u32>>, Option<Range<u32>>),
    ) -> Result<()> {
        COMMANDS.add(callers.into_callers(), cmd, param_checker)
    }

    pub(crate) fn add_for_inner<'a, W: Widget<U>, U: Ui>(
        callers: impl super::Caller<'a>,
        cmd: impl FnMut(&mut W, &U::Area, &mut Cursors, Flags, Args) -> CmdResult + 'static,
        param_checker: fn(Args) -> (Vec<Range<u32>>, Option<Range<u32>>),
    ) -> Result<()> {
        COMMANDS.add_for(callers.into_callers(), cmd, param_checker)
    }

    pub(crate) fn check_params(caller: &str) -> Option<(Vec<Range<u32>>, Option<Range<u32>>)> {
        COMMANDS.check_params(caller)
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
            RwData::new(InnerCommands {
                list: Vec::new(),
                aliases: HashMap::new(),
            })
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
        cmd: impl FnMut(Flags, Args) -> CmdResult + 'static,
        param_checker: fn(Args) -> (Vec<Range<u32>>, Option<Range<u32>>),
    ) -> Result<()> {
        let command = Command::new(callers, cmd, param_checker);
        self.0.write().try_add(command)
    }

    /// Adds a command for a widget of type `W`
    fn add_for<W: Widget<U>, U: Ui>(
        &'static self,
        callers: impl IntoIterator<Item = impl ToString>,
        mut cmd: impl FnMut(&mut W, &U::Area, &mut Cursors, Flags, Args) -> CmdResult + 'static,
        param_checker: fn(Args) -> (Vec<Range<u32>>, Option<Range<u32>>),
    ) -> Result<()> {
        let cur_file = context::inner_cur_file::<U>();
        let windows = context::windows::<U>();
        let w = context::cur_window();

        let cmd = move |flags: Flags, args: Args| {
            cur_file
                .mutate_related_widget::<W, CmdResult>(|widget, area, cursors| {
                    cmd(widget, area, cursors, flags, args.clone())
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

                    w.mutate_as(|w| cmd(w, a, &mut c, flags, args)).unwrap()
                })
        };
        let command = Command::new(callers, cmd, param_checker);

        self.0.write().try_add(command)
    }

    /// Gets the parameter checker for a command, if it exists
    fn check_params(&self, call: &str) -> Option<(Vec<Range<u32>>, Option<Range<u32>>)> {
        let mut args = call.split_whitespace();
        let caller = args.next()?.to_string();

        let (_, args) = split_flags_and_args(call);

        self.0.inspect(|inner| {
            if let Some((command, _)) = inner.aliases.get(&caller) {
                Some((command.param_checker)(args))
            } else {
                let command = inner
                    .list
                    .iter()
                    .find(|cmd| cmd.callers().contains(&caller))?;

                Some((command.param_checker)(args))
            }
        })
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
    callers: Arc<[String]>,
    cmd: RwData<dyn FnMut(Flags, Args) -> CmdResult>,
    param_checker: fn(Args) -> (Vec<Range<u32>>, Option<Range<u32>>),
}

impl Command {
    /// Returns a new instance of command.
    fn new<Cmd: FnMut(Flags, Args) -> CmdResult + 'static>(
        callers: impl IntoIterator<Item = impl ToString>,
        cmd: Cmd,
        param_checker: fn(Args) -> (Vec<Range<u32>>, Option<Range<u32>>),
    ) -> Self {
        let callers: Arc<[String]> = callers
            .into_iter()
            .map(|caller| caller.to_string())
            .collect();

        if let Some(caller) = callers
            .iter()
            .find(|caller| caller.split_whitespace().count() != 1)
        {
            panic!("Command caller \"{caller}\" contains more than one word");
        }
        Self {
            cmd: RwData::new_unsized::<Cmd>(Arc::new(RwLock::new(cmd))),
            param_checker,
            callers,
        }
    }

    /// Executes the inner function if the `caller` matches any of
    /// the callers in [`self`].
    fn try_exec(&self, flags: Flags, args: Args<'_>) -> Result<Option<Text>> {
        (self.cmd.write())(flags, args).map_err(Error::CommandFailed)
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
                    " to " [*a] call
                ),
                None => ok!("Aliased " [*a] alias [] " to " [*a] call),
            }
        } else {
            Err(Error::CallerNotFound(caller))
        }
    }
}

pub type Result<T> = crate::Result<T, ()>;

pub trait Caller<'a>: Sized {
    fn into_callers(self) -> impl Iterator<Item = &'a str>;
}

impl<'a> Caller<'a> for &'a str {
    fn into_callers(self) -> impl Iterator<Item = &'a str> {
        [self].into_iter()
    }
}

impl<'a> Caller<'a> for &'a [&'a str] {
    fn into_callers(self) -> impl Iterator<Item = &'a str> {
        self.iter().cloned()
    }
}

impl<'a, const N: usize> Caller<'a> for [&'a str; N] {
    fn into_callers(self) -> impl Iterator<Item = &'a str> {
        self.into_iter()
    }
}

pub trait Parameter<'a>: Sized {
    type Returns;
    /// Tries to consume arguments until forming a parameter
    fn new(args: &mut Args<'a>) -> std::result::Result<Self::Returns, Text>;
}

impl<'a, P: Parameter<'a>> Parameter<'a> for Option<P> {
    type Returns = Option<P::Returns>;

    /// Will match either the argument given, or nothing
    ///
    /// This, like [`Vec`] _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match something, match
    /// nothing, or fail matching in order to give accurate feedback.
    fn new(args: &mut Args<'a>) -> std::result::Result<Self::Returns, Text> {
        match args.next_as::<P>() {
            Ok(arg) => Ok(Some(arg)),
            // This probably means an argument was trying to be matched, but failed.
            Err(err) if args.is_recording_range() => Err(err),
            Err(_) => Ok(None),
        }
    }
}

impl<'a, P: Parameter<'a>> Parameter<'a> for Vec<P> {
    type Returns = Vec<P::Returns>;

    /// Will match either the argument given, or nothing
    ///
    /// This, like [`Option`] _has_ to be the final argument in the
    /// [`Parameter`] list, as it will either match correcly, finish
    /// matching, or match incorrectly in order to give accurate
    /// feedback.
    fn new(args: &mut Args<'a>) -> std::result::Result<Self::Returns, Text> {
        let mut returns = Vec::new();

        loop {
            match args.next_as::<P>() {
                Ok(ret) => returns.push(ret),
                // This probably means an argument was trying to be matched, but failed.
                Err(err) if args.is_recording_range() => break Err(err),
                Err(_) => break Ok(returns),
            }
        }
    }
}

impl<'a> Parameter<'a> for &'a str {
    type Returns = &'a str;

    fn new(args: &mut Args<'a>) -> std::result::Result<Self::Returns, Text> {
        args.next()
    }
}

impl Parameter<'_> for String {
    type Returns = String;

    fn new(args: &mut Args) -> std::result::Result<Self::Returns, Text> {
        Ok(args.next()?.to_string())
    }
}

/// Returns the remaining arguments, separated by a space
///
/// May return an empty [`String`]
struct Remainder;

impl Parameter<'_> for Remainder {
    type Returns = String;

    fn new(args: &mut Args) -> std::result::Result<Self::Returns, Text> {
        let args = std::iter::from_fn(|| args.next().ok());
        Ok(args.intersperse(" ").collect())
    }
}

parse_impl!(bool);
parse_impl!(u8);
parse_impl!(u16);
parse_impl!(u32);
parse_impl!(u64);
parse_impl!(u128);
parse_impl!(usize);
parse_impl!(i8);
parse_impl!(i16);
parse_impl!(i32);
parse_impl!(i64);
parse_impl!(i128);
parse_impl!(isize);
parse_impl!(f32);
parse_impl!(f64);
parse_impl!(std::path::PathBuf);

macro parse_impl($t:ty) {
    impl Parameter<'_> for $t {
        type Returns = Self;

        fn new(
            args: &mut Args,
        ) -> std::result::Result<Self::Returns, Text> {
            let arg = args.next()?;
            arg.parse().map_err(|_| err!(
                [*a] arg [] "couldn't be parsed as "
                [*a] { stringify!($t) } []
            ))
        }
    }
}
