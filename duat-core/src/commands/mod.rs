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
    text::{Text, err, ok, text},
    ui::{Ui, Window},
    widgets::{File, Node, Widget},
};

mod parameters;

mod global {
    use super::{Args, CmdResult, Commands, Flags, Result};
    use crate::{data::RwData, text::Text, ui::Ui, widgets::Widget};

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

    /// Adds a command to an object "related" to the current [`File`]
    ///
    /// This object can be one of three things, a [`Widget`],
    /// an [`Mode`], or the [`File`] itself. When the
    /// command is executed, Duat will look at the currently active
    /// file for any instance of an [`RwData<Thing>`] it can find.
    /// Those will either be the file itself, or will be added in
    /// the [`Session`]'s "file_fn".
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use duat_core::{
    /// #     commands::{self, Result},
    /// #     data::RwData,
    /// #     input::Mode,
    /// #     text::{Text, text},
    /// #     ui::Ui,
    /// #     widgets::File
    /// # };
    /// # fn test<U: Ui>() {
    /// #[derive(Debug)]
    /// enum Mode {
    ///     Normal,
    ///     Insert,
    ///     Prompt,
    ///     Visual,
    /// }
    ///
    /// struct ModalEditor {
    ///     mode: Mode,
    /// }
    ///
    /// impl<U: Ui> Mode<U> for ModalEditor {
    ///     /* Implementation details. */
    /// # type Widget = File;
    /// # fn send_key(
    /// #     &mut self,
    /// #     key: crossterm::event::KeyEvent,
    /// #     widget: &RwData<Self::Widget>,
    /// #     area: &U::Area,
    /// # )
    /// # {
    /// #     todo!();
    /// # }
    /// }
    ///
    /// commands::add_for_current::<ModalEditor, U>(
    ///     ["set-mode"],
    ///     |modal, flags, mut args| {
    ///         let mut modal = modal.write();
    ///         let mode = args.next_else(text!("No mode given"))?;
    ///
    ///         match mode {
    ///             "normal" | "Normal" => modal.mode = Mode::Normal,
    ///             "insert" | "Insert" => modal.mode = Mode::Insert,
    ///             "prompt" | "Prompt" => modal.mode = Mode::Prompt,
    ///             "visual" | "Visual" => modal.mode = Mode::Visual,
    ///             mode => {
    ///                 return Err(text!(
    ///                     "Mode" [AccentErr] mode []
    ///                     "is not a valid mode"
    ///                 ));
    ///             }
    ///         }
    ///
    ///         let mode = format!("{:?}", modal.mode);
    ///         Ok(Some(text!("Mode was set to " [AccentOk] mode [] ".")))
    ///     }
    /// )
    /// .unwrap();
    /// # }
    /// ```
    ///
    /// [`File`]: crate::widgets::File
    /// [`Mode`]: crate::input::Mode
    /// [`Session`]: crate::session::Session
    pub fn add_for_current<T: 'static, U: Ui>(
        callers: impl IntoIterator<Item = impl ToString>,
        f: impl FnMut(&RwData<T>, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        COMMANDS.add_for_current::<T, U>(callers, f)
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
    /// In this example, we create a simple `Timer` widget, along with
    /// some control commands.
    ///
    /// ```rust
    /// // Required feature for widgets.
    /// # use std::{
    /// #     sync::{atomic::{AtomicBool, Ordering}, Arc}, marker::PhantomData, time::Instant
    /// # };
    /// # use duat_core::{
    /// #     commands, forms::{self, Form}, text::{text, Text, AlignCenter},
    /// #     ui::{Area, PushSpecs, Ui}, widgets::{Widget, Widget, WidgetCfg},
    /// # };
    /// struct Timer {
    ///     text: Text,
    ///     instant: Instant,
    ///     running: Arc<AtomicBool>,
    /// }
    ///
    /// struct TimerCfg<U>(PhantomData<U>);
    ///
    /// impl<U: Ui> WidgetCfg<U> for TimerCfg<U> {
    ///     type Widget = Timer;
    ///
    ///     fn build(self, _is_file: bool) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
    ///         let timer = Timer {
    ///             text: text!(AlignCenter [Counter] 0 [] "ms"),
    ///             instant: Instant::now(),
    ///             // No need to use an `RwData`, since
    ///             // `RwData::has_changed` is never called.
    ///             running: Arc::new(AtomicBool::new(false)),
    ///         };
    ///
    ///         // The checker should tell the `Timer` to update only
    ///         // if `running` is `true`.
    ///         let checker = {
    ///             // Clone any variables before moving them to
    ///             // the `checker`.
    ///             let running = timer.running.clone();
    ///             move || running.load(Ordering::Relaxed)
    ///         };
    ///
    ///         let specs = PushSpecs::below().with_ver_len(1.0);
    ///
    ///         (Widget::passive(timer), checker, specs)
    ///     }
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
    ///             let duration = format!("{:.3?}", duration);
    ///             self.text = text!(
    ///                 AlignCenter [Counter] duration [] "ms"
    ///             );
    ///         }
    ///     }
    ///
    ///     fn text(&self) -> &Text {
    ///         &self.text
    ///     }
    ///
    ///     // The `once` function of a `Widget` is only called
    ///     // when that widget is first created.
    ///     // It is generally useful to add commands and set forms
    ///     // in the `palette`.
    ///     fn once() {
    ///         // `palette::set_weak_form` will only set that form if
    ///         // it doesn't already exist.
    ///         // That means that a user of the widget will be able to
    ///         // control that form by changing it before or after this
    ///         // widget is pushed.
    ///         forms::set_weak("Counter", Form::green());
    ///
    ///         commands::add_for_widget::<Timer, U>(
    ///             ["play"],
    ///             |timer, _area, _flags, _args| {
    ///                 timer.read().running.store(true, Ordering::Relaxed);
    ///
    ///                 Ok(None)
    ///             })
    ///             .unwrap();
    ///
    ///         commands::add_for_widget::<Timer, U>(
    ///             ["pause"],
    ///             |timer, _area, _flags, _args| {
    ///                 timer.read().running.store(false, Ordering::Relaxed);
    ///
    ///                 Ok(None)
    ///             })
    ///             .unwrap();
    ///
    ///         commands::add_for_widget::<Timer, U>(
    ///             ["pause"],
    ///             |timer, _area, _flags, _args| {
    ///                 timer.write().instant = Instant::now();
    ///
    ///                 Ok(None)
    ///             })
    ///             .unwrap();
    ///     }
    /// }
    /// ```
    ///
    /// [`dyn Area`]: crate::ui::Area
    /// [`File`]: crate::widgets::File
    /// [`Session`]: crate::session::Session
    /// [`CommandLine`]: crate::widgets::CommandLine
    pub fn add_for_widget<W: Widget<U>, U: Ui>(
        callers: impl IntoIterator<Item = impl ToString>,
        f: impl FnMut(&RwData<W>, &U::Area, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        COMMANDS.add_for_widget(callers, f)
    }

    pub(crate) fn caller_exists(caller: &str) -> bool {
        COMMANDS.caller_exists(caller)
    }
}

mod control {
    use std::{
        any::TypeId,
        path::PathBuf,
        sync::{
            atomic::{AtomicBool, Ordering}, mpsc, Arc, LazyLock
        }, time::Duration,
    };

    use crossterm::event::KeyEvent;
    use parking_lot::Mutex;

    use super::{file_entry, widget_entry};
    use crate::{
        commands::{self, iter_around, iter_around_rev},
        context,
        input::Mode,
        text::{Text, err, ok},
        ui::{Event, Ui, Window},
        widgets::{CmdLineMode, CommandLine, File, Node},
    };

    static SEND_KEY: LazyLock<Mutex<Box<dyn FnMut(KeyEvent) + Send + Sync>>> =
        LazyLock::new(|| Mutex::new(Box::new(|_| {})));
    static RESET_MODE: LazyLock<Mutex<Arc<dyn Fn() + Send + Sync>>> =
        LazyLock::new(|| Mutex::new(Arc::new(|| {})));
    static SET_MODE: Mutex<Option<Arc<dyn Fn() + Send + Sync>>> = Mutex::new(None);
    static HAS_ENDED: AtomicBool = AtomicBool::new(false);

    /// Returns `true` if Duat must quit/reload
    ///
    /// You should use this function in order to check if loops inside
    /// of threads should break.
    pub fn has_ended() -> bool {
        HAS_ENDED.load(Ordering::Relaxed)
    }

    pub fn was_mode_set() -> Option<Arc<dyn Fn() + Send + Sync>> {
        SET_MODE.lock().take()
    }

    pub fn set_default_mode<M: Mode<U>, U: Ui>(mode: M) {
        *RESET_MODE.lock() = Arc::new(move || set_mode_fn::<M, U>(mode.clone()));
        *SET_MODE.lock() = Some(RESET_MODE.lock().clone());
    }

    pub fn reset_mode() {
        *SET_MODE.lock() = Some(RESET_MODE.lock().clone())
    }

    pub fn set_mode<U: Ui>(mode: impl Mode<U>) {
        *SET_MODE.lock() = Some(Arc::new(move || set_mode_fn(mode.clone())));
    }

    pub fn set_cmd_mode<U: Ui>(mode: impl CmdLineMode<U>) {
        let Ok(cur_file) = context::cur_file::<U>() else {
            return;
        };

        if let Some(node) = cur_file.get_related_widget::<CommandLine<U>>() {
            node.try_downcast::<CommandLine<U>>()
                .unwrap()
                .write()
                .set_mode(mode);
        } else {
            let windows = context::windows::<U>().read();
            let w = context::cur_window();
            let cur_window = &windows[w];

            let mut widgets = {
                let previous = windows[..w].iter().flat_map(Window::nodes);
                let following = windows[(w + 1)..].iter().flat_map(Window::nodes);
                cur_window.nodes().chain(previous).chain(following)
            };

            if let Some(cmd_line) = widgets.find_map(|node| {
                node.data_is::<CommandLine<U>>()
                    .then(|| node.try_downcast::<CommandLine<U>>().unwrap())
            }) {
                cmd_line.write().set_mode(mode)
            }
        }
    }

    /// Switches to the file with the given name
    pub fn switch_to_file<U: Ui>(name: impl std::fmt::Display) {
        let windows = context::windows::<U>().read();
        let name = name.to_string();
        match file_entry(&windows, &name) {
            Ok((_, node)) => switch_widget(node.clone()),
            Err(err) => context::notify(err),
        }

        *SET_MODE.lock() = Some(RESET_MODE.lock().clone());
    }

    pub(crate) fn send_key(key: KeyEvent) {
        SEND_KEY.lock()(key);
    }

    /// Ends duat, either for reloading the config, or quitting
    pub(crate) fn end_session() {
        HAS_ENDED.store(true, Ordering::Relaxed);
        while crate::thread::still_running() {
            std::thread::sleep(Duration::from_micros(500));
        }
    }

    pub(crate) fn switch_widget<U: Ui>(node: Node<U>) {
        if let Ok(widget) = context::cur_widget::<U>() {
            widget.node().on_unfocus();
        }

        context::set_cur(node.as_file(), node.clone());

        node.on_focus();
    }

    pub(crate) fn add_session_commands<U: Ui>(tx: mpsc::Sender<Event>) -> crate::Result<(), ()> {
        commands::add(["quit", "q"], {
            let tx = tx.clone();

            move |_flags, _args| {
                tx.send(Event::Quit).unwrap();
                Ok(None)
            }
        })?;

        commands::add(["write", "w"], move |_flags, mut args| {
            let file = context::cur_file::<U>()?;

            let paths = {
                let mut paths = Vec::new();

                while let Ok(arg) = args.next() {
                    paths.push(arg.to_string());
                }

                paths
            };

            if paths.is_empty() {
                file.inspect(|file, _, _| {
                    if let Some(name) = file.set_name() {
                        let bytes = file.write()?;
                        ok!("Wrote " [*a] bytes [] " bytes to " [*a] name [] ".")
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

                    ok!("Wrote " [*a] bytes [] " bytes to " files_text [] ".")
                })
            }
        })?;

        commands::add(["edit", "e"], {
            let windows = context::windows::<U>();

            move |_, mut args| {
                let windows = windows.read();
                let file = args.next_else(err!("No path supplied."))?;

                let path = PathBuf::from(file);
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
                    tx.send(Event::OpenFile(path)).unwrap();
                    return ok!("Opened " [*a] file [] ".");
                }

                commands::switch_to_file::<U>(&name);
                ok!("Switched to " [*a] name [] ".")
            }
        })?;

        commands::add(["buffer", "b"], move |_, mut args| {
            let path: PathBuf = args.next_as()?;
            let name = path
                .file_name()
                .ok_or(err!("No file in path"))?
                .to_string_lossy()
                .to_string();

            commands::switch_to_file::<U>(&name);
            ok!("Switched to " [*a] name [] ".")
        })?;

        commands::add(["next-file"], {
            let windows = context::windows();

            move |flags, _| {
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
                        .ok_or_else(|| err!("There are no other open files."))?
                } else {
                    let slice = &read_windows[w..=w];
                    iter_around(slice, 0, widget_index)
                        .find_map(|(_, node)| node.inspect_as::<File, String>(|file| file.name()))
                        .ok_or_else(|| err!("There are no other files open in this window."))?
                };

                commands::switch_to_file::<U>(&name);
                ok!("Switched to " [*a] name [] ".")
            }
        })?;

        commands::add(["prev-file"], {
            let windows = context::windows();

            move |flags, _| {
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
                        .ok_or_else(|| err!("There are no other open files."))?
                } else {
                    let slice = &windows[w..=w];
                    iter_around_rev(slice, 0, widget_i)
                        .find_map(|(_, node)| node.inspect_as::<File, String>(|file| file.name()))
                        .ok_or_else(|| err!("There are no other files open in this window."))?
                };

                commands::switch_to_file::<U>(&name);

                ok!("Switched to " [*a] name [] ".")
            }
        })?;

        Ok(())
    }

    fn send_key_fn<U: Ui>(mode: &mut impl Mode<U>, key: KeyEvent) {
        let Ok(widget) = context::cur_widget::<U>() else {
            return;
        };

        widget.mutate_data_as(|widget, area, cursors| {
            let mut c = cursors.write();
            *c = mode.send_key(key, widget, area, c.take())
        });
    }

    fn set_mode_fn<M: Mode<U>, U: Ui>(mut mode: M) {
        // If we are on the correct widget, no switch is needed.
        if context::cur_widget::<U>().unwrap().type_id() != TypeId::of::<M::Widget>() {
            let windows = context::windows().read();
            let w = context::cur_window();
            let entry = if TypeId::of::<M::Widget>() == TypeId::of::<File>() {
                let name = context::cur_file::<U>().unwrap().name();
                file_entry(&windows, &name)
            } else {
                widget_entry::<M::Widget, U>(&windows, w)
            };

            match entry {
                Ok((_, node)) => switch_widget(node.clone()),
                Err(err) => {
                    context::notify(err);
                    return;
                }
            };
        }

        *SEND_KEY.lock() = Box::new(move |key| send_key_fn::<U>(&mut mode, key));
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

    /// Adds a command for a current struct of type `T`
    fn add_for_current<T: 'static, U: Ui>(
        &'static self,
        callers: impl IntoIterator<Item = impl ToString>,
        mut f: impl FnMut(&RwData<T>, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        let cur_file = context::inner_cur_file::<U>();
        let cur_widget = context::inner_cur_widget::<U>();

        let command = Command::new(callers, move |flags, args| {
            let result = cur_file.mutate_related::<T, CmdResult>(|t| f(t, flags, args.clone()));

            result
                .or_else(|| cur_widget.mutate_as::<T, CmdResult>(|t| f(t, flags, args.clone())))
                .transpose()?
                .ok_or_else(|| {
                    text!(
                        "The current file has no related structs of type {}"
                        { std::any::type_name::<T>() }
                    )
                })
        });

        self.0.write().try_add(command)
    }

    /// Adds a command for a widget of type `W`
    fn add_for_widget<W: Widget<U>, U: Ui>(
        &'static self,
        callers: impl IntoIterator<Item = impl ToString>,
        mut f: impl FnMut(&RwData<W>, &U::Area, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        let cur_file = context::inner_cur_file::<U>();
        let windows = context::windows::<U>();
        let w = context::cur_window();

        let command = Command::new(callers, move |flags, args| {
            cur_file
                .mutate_related_widget::<W, CmdResult>(|widget, area| {
                    f(widget, area, flags, args.clone())
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
                    let w = node.widget();
                    f(&w.try_downcast().unwrap(), node.area(), flags, args)
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
