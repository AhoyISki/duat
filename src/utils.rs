use std::sync::RwLock;

use hotpatch::patchable;
use parsec_core::{
    commands::Commands,
    data::{CurrentFile, CurrentWidget},
    session::SessionCfg,
    text::PrintCfg,
    Globals,
};
use parsec_term::VertRule;

use crate::{
    prelude::CommandLine,
    widgets::{LineNumbers, StatusLine},
    Ui,
};

pub static CURRENT_FILE: CurrentFile<Ui> = CurrentFile::new();
pub static CURRENT_WIDGET: CurrentWidget<Ui> = CurrentWidget::new();
pub static COMMANDS: Commands<Ui> = Commands::new(&CURRENT_FILE, &CURRENT_WIDGET);
pub static GLOBALS: Globals<Ui> = Globals::new(&CURRENT_FILE, &CURRENT_WIDGET, &COMMANDS);

pub static UI_FN: UiFn = RwLock::new(None);
pub static CFG_FN: CfgFn = RwLock::new(None);
pub static PRINT_CFG: RwLock<Option<PrintCfg>> = RwLock::new(None);

#[patchable]
pub fn parsec() -> SessionStarter {
    finish()
}

pub struct SessionStarter {
    pub(crate) globals: Globals<Ui>,
    pub(crate) ui_fn: &'static UiFn,
    pub(crate) cfg_fn: &'static CfgFn,
}

pub fn finish() -> SessionStarter {
    SessionStarter {
        globals: GLOBALS,
        ui_fn: &UI_FN,
        cfg_fn: &CFG_FN,
    }
}

pub mod config {
    use parsec_core::{input::InputMethod, session::SessionCfg, widgets::File};

    use super::{default_cfg_fn, CFG_FN, UI_FN};
    use crate::Ui;

    pub fn set_ui(f: impl FnMut() -> Ui + Send + Sync + 'static) {
        *UI_FN.write().unwrap() = Some(Box::new(f));
    }

    pub fn set_fn(f: impl FnMut(&mut SessionCfg<Ui>) + Send + Sync + 'static) {
        *CFG_FN.write().unwrap() = Some(Box::new(f));
    }

    pub fn set_input<I: InputMethod<Ui, Widget = File<Ui>> + Clone>(
        mut f: impl FnMut() -> I + Send + Sync + 'static,
    ) {
        let mut cfg_fn = CFG_FN.write().unwrap();
        let prev = cfg_fn.take();

        *cfg_fn = Some(match prev {
            Some(mut prev_fn) => Box::new(move |cfg| {
                prev_fn(cfg);
                cfg.set_input(f())
            }),
            None => Box::new(move |cfg| {
                default_cfg_fn(cfg);
                cfg.set_input(f())
            }),
        })
    }
}

pub mod hooks {
    use parsec_core::ui::FileBuilder;

    use super::{default_cfg_fn, CFG_FN};
    use crate::Ui;

    pub fn on_file_open(f: impl FnMut(&FileBuilder<Ui>) + Send + Sync + 'static) {
        let mut cfg_fn = CFG_FN.write().unwrap();
        let prev = cfg_fn.take();

        *cfg_fn = Some(match prev {
            Some(mut prev_fn) => Box::new(move |cfg| {
                prev_fn(cfg);
                cfg.suffix_file_fn(f);
            }),
            None => Box::new(move |cfg| {
                default_cfg_fn(cfg);
                cfg.suffix_file_fn(f);
            }),
        })
    }

    pub fn reset_file_fn() {
        let mut cfg_fn = CFG_FN.write().unwrap();
        let prev = cfg_fn.take();

        *cfg_fn = Some(match prev {
            Some(mut prev_fn) => Box::new(move |cfg| {
                prev_fn(cfg);
                cfg.set_file_fn(|_| {})
            }),
            None => Box::new(move |cfg| {
                default_cfg_fn(cfg);
                cfg.set_file_fn(|_| {})
            }),
        })
    }
}

pub mod print {
    use std::ops::RangeInclusive;

    use parsec_core::text::PrintCfg;

    use super::PRINT_CFG;

    pub mod forms {
        pub use parsec_core::palette::{
            set_extra_cursor as extra_cursor, set_form as set, set_main_cursor as main_cursor,
            set_source as source,
        };
    }

    pub mod get {
        pub use parsec_core::palette::{extra_cursor, form_of_id, id_of_name, main_cursor};
    }

    pub fn wrap_on_width() {
        let mut print_cfg = PRINT_CFG.write().unwrap();
        let prev = print_cfg.take();

        *print_cfg = Some(match prev {
            Some(prev) => prev.width_wrapped(),
            None => PrintCfg::default().width_wrapped(),
        })
    }

    pub fn wrap_on_words() {
        let mut print_cfg = PRINT_CFG.write().unwrap();
        let prev = print_cfg.take();

        *print_cfg = Some(match prev {
            Some(prev) => prev.word_wrapped(),
            None => PrintCfg::default().word_wrapped(),
        })
    }

    pub fn wrap_on_cap(cap: usize) {
        let mut print_cfg = PRINT_CFG.write().unwrap();
        let prev = print_cfg.take();

        *print_cfg = Some(match prev {
            Some(prev) => prev.wrapped_on_cap(cap),
            None => PrintCfg::default().wrapped_on_cap(cap),
        })
    }

    pub fn indent_on_wrap() {
        let mut print_cfg = PRINT_CFG.write().unwrap();
        let prev = print_cfg.take();

        *print_cfg = Some(match prev {
            Some(prev) => prev.indenting_wrap(),
            None => PrintCfg::default().indenting_wrap(),
        })
    }

    pub fn tab_size(tab_size: usize) {
        let mut print_cfg = PRINT_CFG.write().unwrap();
        let prev = print_cfg.take();

        *print_cfg = Some(match prev {
            Some(prev) => prev.with_tabs_size(tab_size),
            None => PrintCfg::default().with_tabs_size(tab_size),
        })
    }

    pub fn new_line(char: char) {
        let mut print_cfg = PRINT_CFG.write().unwrap();
        let prev = print_cfg.take();

        *print_cfg = Some(match prev {
            Some(prev) => prev.with_new_line_as(char),
            None => PrintCfg::default().with_new_line_as(char),
        })
    }

    pub fn trailing_new_line(char: char) {
        let mut print_cfg = PRINT_CFG.write().unwrap();
        let prev = print_cfg.take();

        *print_cfg = Some(match prev {
            Some(prev) => prev.with_trailing_new_line_as(char),
            None => PrintCfg::default().with_trailing_new_line_as(char),
        })
    }

    pub fn scrolloff(x: usize, y: usize) {
        let mut print_cfg = PRINT_CFG.write().unwrap();
        let prev = print_cfg.take();

        *print_cfg = Some(match prev {
            Some(prev) => prev.with_scrolloff(x, y),
            None => PrintCfg::default().with_scrolloff(x, y),
        })
    }

    pub fn word_chars(word_chars: impl Iterator<Item = RangeInclusive<char>>) {
        let mut print_cfg = PRINT_CFG.write().unwrap();
        let prev = print_cfg.take();

        *print_cfg = Some(match prev {
            Some(prev) => prev.with_word_chars(word_chars),
            None => PrintCfg::default().with_word_chars(word_chars),
        })
    }
}

pub mod control {
    use std::fmt::Display;

    use parsec_core::{
        commands::{Args, CmdResult, Flags, Result},
        text::Text,
        ui,
        widgets::{ActiveWidget, PassiveWidget},
    };

    use super::COMMANDS;
    use crate::Ui;

    /// Canonical way to quit Parsec.
    ///
    /// By calling the quit command, all threads will finish their
    /// tasks, and then Parsec will execute a program closing
    /// function, as defined by the [`Ui`].
    pub fn quit() {
        COMMANDS.quit();
    }

    /// Switches to the given [`ActiveWidget`].
    ///
    /// The widget will be chosen in the following order:
    ///
    /// 1. Any instance that is "related" to the currently active
    ///    [`File`], that is, any widgets that were added during the
    ///    [`Session`]'s "`file_fn`".
    /// 2. Other widgets in the currently active window, related or
    ///    not to any given [`File`].
    /// 3. Any instance of the [`PassiveWidget`] that is found in
    ///    other windows, looking first at windows ahead.
    ///
    /// [`File`]: crate::widgets::File
    /// [`Session`]: crate::session::Session
    pub fn switch_to<W: ActiveWidget<Ui>>() -> Result<Option<Text>> {
        COMMANDS.run(format!("switch-to {}", W::name()))
    }

    /// Switches to/opens a [`File`] with the given name.
    ///
    /// If you wish to specifically switch to files that are already
    /// open, use [`commands::buffer`].
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::buffer`]: crate::commands::buffer
    pub fn edit(file: impl Display) -> Result<Option<Text>> {
        COMMANDS.run(format!("edit {}", file))
    }

    /// Switches to a [`File`] with the given name.
    ///
    /// If there is no file open with that name, does nothing. Use
    /// [`commands::edit`] if you wish to open files.
    ///
    /// If there are more arguments, they will be ignored.
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::edit`]: crate::commands::edit
    pub fn buffer(file: impl Display) -> Result<Option<Text>> {
        COMMANDS.run(format!("buffer {}", file))
    }

    /// Switches to the next [`File`].
    ///
    /// This function will only look at files that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`commands::next_global_file`].
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::next_global_file`]: crate::commands::next_global_file
    pub fn next_file() -> Result<Option<Text>> {
        COMMANDS.run("next-file")
    }

    /// Switches to the previous [`File`].
    ///
    /// This function will only look at files that are opened in the
    /// current window. If you want to include other windows in the
    /// search, use [`commands::prev_global_file`].
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::prev_global_file`]: crate::commands::prev_global_file
    pub fn prev_file() -> Result<Option<Text>> {
        COMMANDS.run("prev-file")
    }

    /// Switches to the next [`File`].
    ///
    /// This function will look for files in all windows. If you want
    /// to limit the search to just the current window, use
    /// [`commands::next_file`].
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::next_file`]: crate::commands::next_file
    pub fn next_global_file() -> Result<Option<Text>> {
        COMMANDS.run("next-file --global")
    }

    /// Switches to the previous [`File`].
    ///
    /// This function will look for files in all windows. If you want
    /// to limit the search to just the current window, use
    /// [`commands::prev_file`].
    ///
    /// [`File`]: crate::widgets::File
    /// [`commands::prev_file`]: crate::commands::prev_file
    pub fn prev_global_file() -> Result<Option<Text>> {
        COMMANDS.run("prev-file --global")
    }

    /// If not in a [`File`], switches to the last active [`File`].
    ///
    /// This is useful if the currently active widget is not a file
    /// (e.g. [`CommandLine`], a file tree, etc), and you want to
    /// return to the file seamlessly.
    ///
    /// [`File`]: crate::widgets::File
    /// [`CommandLine`]: crate::widgets::CommandLine
    pub fn return_to_file() -> Result<Option<Text>> {
        COMMANDS.run("return-to-file")
    }

    /// Tries to alias a `caller` to an existing `command`.
    ///
    /// Returns an [`Err`] if the `caller` is already a caller for
    /// another command, or if `command` is not a real caller to an
    /// exisiting [`Command`].
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
    /// # use parsec_core::{
    /// #     commands::{self, Result},
    /// #     text::Text,
    /// # };
    /// # fn test() -> Result<Option<Text>> {
    /// commands::run("set-prompt new-prompt")
    /// # }
    /// ```
    ///
    /// In this case we're running a command that will affect the most
    /// relevant [`CommandLine`]. See [`commands::add_for_widget`] for
    /// more information.
    ///
    /// [`CommandLine`]: crate::widgets::CommandLine
    /// [`commands::add_for_widget`]: crate::commands::add_for_widget
    pub fn run(call: impl Display) -> Result<Option<Text>> {
        COMMANDS.run(call)
    }

    /// Adds a command to the global list of commands.
    ///
    /// This command cannot take any arguments beyond the [`Flags`]
    /// and [`Args`], so any mutation of state must be done
    /// through captured variables, usually in the form of
    /// [`RwData<T>`]s.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::{
    /// #     commands,
    /// #     data::RwData,
    /// #     text::{text, Text},
    /// #     widgets::status
    /// # };
    /// // Shared state, which will be displayed in a `StatusLine`.
    /// let var = RwData::new(35);
    ///
    /// commands::add(["set-var"], {
    ///     // A clone is necessary, in order to have one copy of `var`
    ///     // in the closure, while the other is in the `StatusLine`.
    ///     let var = var.clone();
    ///     move |_flags, mut args| {
    ///         // You can easily parse arguments, and an appropriate
    ///         // error will be returned if the parsing fails.
    ///         let value: usize = args.next_as()?;
    ///         *var.write() = value;
    ///
    ///         Ok(None)
    ///     }
    /// });
    ///
    /// // A `StatusLineCfg` that can be used to create a `StatusLine`.
    /// let status_cfg = status!("The value is currently " var);
    /// ```
    ///
    /// In the above example, we created a variable that can be
    /// modified by the command `"set-var"`, and then sent it to a
    /// [`StatusLineCfg`], so that it could be displayed in a
    /// [`StatusLine`]. Note that the use of an [`RwData<usize>`]/
    /// [`RoData<usize>`] means that the [`StatusLine`] will
    /// be updated automatically, whenever the command is ran.
    ///
    /// [`StatusLineCfg`]: crate::widgets::StatusLineCfg
    /// [`StatusLine`]: crate::widgets::StatusLine
    /// [`RoData<usize>`]: crate::data::RoData
    pub fn add(
        callers: impl IntoIterator<Item = impl ToString>,
        f: impl FnMut(Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        COMMANDS.add(callers, f)
    }

    /// Adds a command to an object "related" to the current [`File`]
    ///
    /// This object can be one of three things, a [`PassiveWidget`],
    /// an [`InputMethod`], or the [`File`] itCOMMANDS. When the
    /// command is ran, Parsec will look at the currently active
    /// file for any instance of an [`RwData<Thing>`] it can find.
    /// Those will either be the file itself, or will be added in
    /// the [`Session`]'s "file_fn".
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use parsec_core::{
    /// #     commands,
    /// #     data::RwData,
    /// #     input::{InputMethod, MultiCursorEditor},
    /// #     text::text,
    /// #     widgets::File,
    /// # };
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
    /// impl InputMethod for ModalEditor {
    ///     // Implementation details.
    /// # type Widget = File
    /// # where
    /// #     Self: Sized;
    ///
    /// # fn send_key(
    /// #     &mut self,
    /// #     key: crossterm::event::KeyEvent,
    /// #     widget: &RwData<Self::Widget>,
    /// #     area: &impl parsec_core::ui::Area,
    /// # ) where
    /// #     Self: Sized,
    /// # {
    /// #     todo!()
    /// # }
    /// }
    ///
    /// commands::add_for_current::<ModalEditor>(
    ///     ["set-mode"],
    ///     |modal, flags, mut args| {
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
    /// ```
    ///
    /// [`File`]: crate::widgets::File
    /// [`InputMethod`]: crate::input::InputMethod
    /// [`Session`]: crate::session::Session
    pub fn add_for_current<T: 'static>(
        callers: impl IntoIterator<Item = impl ToString>,
        f: impl FnMut(&mut T, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        COMMANDS.add_for_current(callers, f)
    }

    /// Adds a command that can mutate a widget of the given type,
    /// along with its associated [`dyn Area`].
    ///
    /// This command will look for the [`PassiveWidget`] in the
    /// following order:
    ///
    /// 1. Any instance that is "related" to the currently active
    ///    [`File`], that is, any widgets that were added during the
    ///    [`Session`]'s "`file_fn`".
    /// 2. Other widgets in the currently active window, related or
    ///    not to any given [`File`].
    /// 3. Any instance of the [`PassiveWidget`] that is found in
    ///    other windows, looking first at windows ahead.
    ///
    /// Keep in mind that the search is deterministic, that is, if
    /// there are multiple instances of the widget that fit the
    /// same category, only one of them will ever be used.
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
    /// #![feature(return_position_impl_trait_in_trait)]
    /// # use std::{
    /// #    sync::{
    /// #        atomic::{AtomicBool, Ordering},
    /// #        Arc,
    /// #    },
    /// #    time::Instant,
    /// # };
    /// # use parsec_core::{
    /// #    commands,
    /// #    palette::{self, Form},
    /// #    text::{text, Text, AlignCenter},
    /// #    ui::{Area, PushSpecs, Ui},
    /// #    widgets::{PassiveWidget, Widget},
    /// # };
    /// # pub struct Timer {
    /// #    text: Text,
    /// #    instant: Instant,
    /// #    running: Arc<AtomicBool>,
    /// # }
    /// impl PassiveWidget for Timer {
    ///     fn build<U: Ui>() -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
    ///         let timer = Self {
    ///             text: text!(AlignCenter [Counter] "0ms"),
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
    ///         let specs = PushSpecs::below().with_lenght(1.0);
    ///
    ///         (Widget::passive(timer), checker, specs)
    ///     }
    ///
    ///     fn update(&mut self, _area: &impl Area) {
    ///         if COMMANDS.running.load(Ordering::Relaxed) {
    ///             let duration = COMMANDS.instant.elapsed();
    ///             let duration = format!("{:.3?}", duration);
    ///             COMMANDS.text = text!(
    ///                 AlignCenter [Counter] duration [] "elapsed"
    ///             );
    ///         }
    ///     }
    ///     
    ///
    ///     fn text() -> &Text {
    ///         &COMMANDS.text
    ///     }
    ///
    ///     // The `once` function of a `PassiveWidget` is only called
    ///     // when that widget is first created.
    ///     // It is generally useful to add commands and set forms
    ///     // in the `palette`.
    ///     fn once() {
    ///         // `palette::set_weak_form` will only set that form if
    ///         // it doesn't already exist.
    ///         // That means that a user of the widget will be able to
    ///         // control that form by changing it before or after this
    ///         // widget is pushed.
    ///         palette::set_weak_form("Counter", Form::new().green());
    ///
    ///         commands::add_for_widget::<Timer>(
    ///             ["play"],
    ///             |timer, _area, _flags, _args| {
    ///                 timer.running.store(true, Ordering::Relaxed);
    ///
    ///                 Ok(None)
    ///             })
    ///             .unwrap();
    ///
    ///         commands::add_for_widget::<Timer>(
    ///             ["pause"],
    ///             |timer, _area, _flags, _args| {
    ///                 timer.running.store(false, Ordering::Relaxed);
    ///
    ///                 Ok(None)
    ///             })
    ///             .unwrap();
    ///
    ///         commands::add_for_widget::<Timer>(
    ///             ["pause"],
    ///             |timer, _area, _flags, _args| {
    ///                 timer.instant = Instant::now();
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
    pub fn add_for_widget<W: PassiveWidget<Ui>>(
        callers: impl IntoIterator<Item = impl ToString>,
        f: impl FnMut(&mut W, &<Ui as ui::Ui>::Area, Flags, Args) -> CmdResult + 'static,
    ) -> Result<()> {
        COMMANDS.add_for_widget::<W>(callers, f)
    }
}

pub type UiFn = RwLock<Option<Box<dyn FnOnce() -> Ui + Send + Sync>>>;
pub type CfgFn = RwLock<Option<Box<dyn FnOnce(&mut SessionCfg<Ui>) + Send + Sync>>>;

pub fn default_cfg_fn(cfg: &mut SessionCfg<Ui>) {
    cfg.set_print_cfg(
        PRINT_CFG
            .write()
            .unwrap()
            .take()
            .unwrap_or_default()
            .width_wrapped(),
    );

    cfg.set_file_fn(|builder| {
        builder.push::<VertRule>();
        builder.push::<LineNumbers>();
        let (child, _) = builder.push::<StatusLine>();
        builder.push_cfg_to(CommandLine::cfg().left_with_percent(50), child);
    });

    cfg.set_input(parsec_kak::Editor::new());
}
