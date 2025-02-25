//! # Duat
//!
//! Duat is a text editor meant to have as much modularity as
//! possible, while keeping a sensible default configuration. It is
//! written _and configured_ in Rust, through the use of a
//! configuration Rust crate, placed in `~/.config/duat/` (or wherever
//! `$XDG_CONFIG_HOME` is set to).
//!
//! When installing Duat, this crate will be automatically placed in
//! that spot, and it will have a default example configuration.
//!
//! When you first run Duat, and whenever you update the
//! configuration, it will be compiled and reloaded automatically, so
//! you can see the changes in _almost_ real time.
//!
//! Note that this is an alpha project, so there may be some quirks
//! and bugs. So if you have a problem, or something seems confusing,
//! feel free to ask questions or raise an issue, that would be very
//! welcome 🥰.
//!
//! ## Getting started
//!
//! To install Duat, do the following:
//!
//! ```text
//! cargo install duat
//! ```
//!
//! Although, since this is an alpha, I would recommend the git
//! version, since that is kept much  more up to date:
//!
//! ```text
//! git clone https://github.com/AhoyISki/duat
//! cargo install --path duat
//! ```
//!
//! ## Configuration
//!
//! In the configuration file, there should be a `setup_duat!` macro,
//! which takes in a function with no parameters.
//!
//! This function is the setup for duat, and it can be empty, which is
//! the equivalent of the default configuration for Duat.
//!
//! Here's an example configuration file, which makes use of the
//! `duat-kak` crate, which is a plugin for Duat. This plugin, like
//! all others, is included without the `duat_` prefix, so in the
//! config it is just `kak`.
//!
//! ```rust
//! # mod kak {
//! #     use duat::{prelude::{mode::*, data::RwData}, Area, Ui, widgets::File};
//! #     #[derive(Clone)]
//! #     pub struct Normal;
//! #     impl Mode<Ui> for Normal {
//! #         type Widget = File;
//! #         fn send_key(&mut self, _: KeyEvent, _: &RwData<File>, _: &Area) {
//! #             todo!();
//! #         }
//! #     }
//! #     #[derive(Clone)]
//! #     pub struct Insert;
//! #     impl Mode<Ui> for Insert {
//! #         type Widget = File;
//! #         fn send_key(&mut self, _: KeyEvent, _: &RwData<File>, _: &Area) {
//! #             todo!();
//! #         }
//! #     }
//! #     pub struct Kak;
//! #     impl duat_core::Plugin<Ui> for Kak {
//! #         fn new() -> Self {
//! #             Self
//! #         }
//! #         fn plug(self) {}
//! #     }
//! # }
//! setup_duat!(setup);
//! use duat::prelude::*;
//! use kak::{Insert, Normal};
//!
//! fn setup() {
//!     plug!(kak::Kak);
//!     map::<Insert>("jk", "<Esc>");
//!
//!     print::wrap_on_width();
//!
//!     hooks::remove("FileWidgets");
//!     hooks::add::<OnFileOpen>(|builder| {
//!         builder.push(VertRule::cfg());
//!         builder.push(LineNumbers::cfg());
//!     });
//!
//!     hooks::remove("WindowWidgets");
//!     hooks::add::<OnWindowOpen>(|builder| {
//!         let status = status!(
//!             [File] { File::name } " "
//!             mode " " selections_fmt " " main_fmt
//!         );
//!
//!         let (child, _) = builder.push(status);
//!         builder.push_to(CmdLine::cfg().left_ratioed(3, 7), child);
//!     });
//!
//!     hooks::add::<ModeSwitched>(|&(_, new)| match new {
//!         "Insert" => cursor::set_main(CursorShape::SteadyBar),
//!         _ => cursor::set_main(CursorShape::SteadyBlock)
//!     });
//!
//!     form::set("Mode", Form::dark_magenta());
//! }
//! ```
//!
//! This configuration does the following things:
//!
//! - [plugs] the `Kak` plugin, which changes the [default mode];
//! - [Maps] jk to esc in the `Insert` mode;
//! - [Changes] the wrapping;
//! - [Removes] the hook [group] "FileWidgets";
//! - [Pushes] a [vertical rule] and [line numbers] to every file;
//! - Removes the hook group "WindowWidgets";
//! - Pushes a [custom status line] and [command line] to the bottom
//!   of the screen;
//! - [Adds] hooks for [mode changes] in Duat;
//! - [Changes](form::set) the [style] of the mode printed on the
//!   status line;
//!
//! These are some of the ways you can configure Duat. You might
//! notice some things that can be done with these simple options:
//!
//! ```rust
//! # use duat::prelude::*;
//! hooks::add::<OnFileOpen>(|builder| {
//!     builder.push(LineNumbers::cfg());
//!     builder.push(LineNumbers::cfg());
//!     builder.push(LineNumbers::cfg().on_the_right());
//!     builder.push(LineNumbers::cfg().on_the_right());
//! });
//! ```
//!
//! Now, every file will open with two lines of numbers, one on each
//! side. Would you ever want to do this? ...No, not really, but it
//! shows how configurable Duat can be.
//!
//! Duat also comes with a fully fledged text styling system, which
//! significantly eases the creation of widgets:
//!
//! ```rust
//! # use duat::prelude::*;
//! let text = text!([MyForm] "Waow it's my form! " [] "not anymore 😢");
//! ```
//!
//! In this example, I'm using the "MyForm" form in order to style the
//! text, while `[]` reverts back to the "Default" form. The
//! [`status!`] macro works similarly.
//!
//! Duat also has a simple command system, that lets you add commands
//! with arguments supported by Rust's type system:
//!
//! ```rust
//! # use duat::prelude::*;
//! # fn test() -> Result<(), Error<()>> {
//! let callers = ["collapse-cmd-line", "ccmd"];
//! cmd::add_for!(callers, |_: CmdLine, area: Area| {
//!     area.constrain_ver(Constraint::Length(0.0))?;
//!     Ok(None)
//! })
//! # }
//! ```
//!
//! The 2 arguments
//!
//! ## Features
//!
//! Duat provides a lot of features, trying to be as configurable as
//! possible, here are some of the things that Duat is capable of:
//!
//! - Completely custom modes, with full Vim style remapping
//! - Completely custom widgets, with user created modes
//! - Arbitrary concealment of text, and arbitrary ghost text
//! - Custom hooks, whose activation is up to the creator
//! - Custom commands, with customizable parameters supported by
//!   Rust's robust type system
//! - Multi UI adaptability, although for now, only a terminal UI has
//!   been made
//! - And many others still being planned
//!
//! Additionally, by choosing Rust as its configuration language, Duat
//! also gains the following features:
//!
//! - Complete type safety
//! - A very functional programming language, with lots of native
//!   features
//! - Cargo is the plugin manager
//!
//! ## Roadmap
//!
//! These are the goals that have been accomplished or are on their
//! way:
//!
//! - [x] Implement basic visual functionality (printing, scrolling,
//!   etc);
//! - [x] Implement wrapping;
//! - [x] Implement editing;
//! - [x] Create a kak mode;
//! - [x] Implement the use of multiple cursors;
//! - [x] Implement a history system;
//! - [x] Implement colors;
//! - [x] Implement widgets and designated areas;
//! - [x] Make all of these things easy to use on a public interface;
//! - [x] Create a number line and a separator line;
//! - [x] Create a status line;
//! - [x] File switching;
//! - [x] Create a command creation interface and a command line;
//! - [x] Add the ability to frame areas;
//! - [x] Implement concealment;
//! - [x] Implement hot reloading of configuration;
//! - [x] Create a "normal editing" mode;
//! - [x] Add the ability to create hooks;
//! - [x] Create a more generalized plugin system;
//! - [x] Implement incremental Regex searching;
//! - [x] Implement tree-sitter;
//! - [ ] Add floating widgets, not tied to the session layout;
//! - [ ] Implement autocompletion lists;
//! - [ ] Create an LSP plugin;
//! - [ ] Create a vim mode;
//!
//! ︙
//!
//! - [ ] Create an Iced frontend;
//!
//! __NOTE:__ These are not set in stone, and may be done out of
//! order.
//!
//! ## Why should I use this?
//!
//! I don't know what your personal reasoning would be, but in my
//! case, I really like Kakoune's editing model, but was frustrated
//! with the lack of some features, like folding, multiple file
//! editing, the general barebonesness of the configuration, etc.
//!
//! I know that Neovim has all of these features, and Helix supposedly
//! tries to solve some of these issues. But I don't really like
//! either of their editing styles to be honest.
//!
//! And so I thought, why not make my own text editor?
//!
//! I thought, why not make a text editor that is as modular as
//! possible, while still having a sensible default configuration?
//! That I could modify however I wanted, and with a language that I
//! love?
//!
//! That's why I decided to create Duat.
//!
//! ## Why the name
//!
//! idk, cool sounding word that I got from Spelunky 2.
//!
//! Also, just wanted to say that no AI was used in this project, cuz
//! I don't like it.
//!
//! [plugs]: plug
//! [default mode]: mode::set_default
//! [Maps]: prelude::map
//! [Changes]: prelude::print::wrap_on_width
//! [Removes]: hooks::remove
//! [group]: hooks::add_grouped
//! [Pushes]: duat_core::ui::FileBuilder
//! [vertical rule]: prelude::VertRule
//! [line numbers]: prelude::LineNumbers
//! [custom status line]: prelude::status
//! [command line]: prelude::CmdLine
//! [Adds]: hooks::add
//! [mode changes]: hooks::ModeSwitched
//! [style]: form::Form
//! [`status!`]: prelude::status
//! [tags]: duat_core::text::Tag
#![feature(decl_macro, let_chains)]

use std::sync::RwLock;

pub use duat_core::{Error, thread};
use duat_core::{Plugin, session::SessionCfg};
pub use setup::{Messengers, MetaStatics, pre_setup, run_duat};

pub mod print;
mod setup;

pub mod mode {
    //! Commands for the manipulation of [`Mode`]s
    pub use duat_core::mode::*;
    use duat_core::{mode, widgets::CmdLineMode};

    use crate::Ui;

    /// Sets the new default mode
    ///
    /// This is the mode that will be set when [`mode::reset`] is
    /// called.
    ///
    /// [`mode::reset`]: reset
    pub fn set_default(mode: impl Mode<Ui>) {
        mode::set_default(mode);
    }

    /// Sets the [`Mode`], switching to the appropriate [`Widget`]
    ///
    /// [`Widget`]: Mode::Widget
    pub fn set(mode: impl Mode<Ui>) {
        mode::set(mode);
    }

    /// Resets the mode to the [default]
    ///
    /// [default]: set_default
    pub fn reset() {
        mode::reset();
    }

    /// Sets the [`CmdLineMode`]
    pub fn set_cmd(mode: impl CmdLineMode<Ui>) {
        mode::set_cmd(mode);
    }

    /// Maps a sequence of keys to another
    ///
    /// The keys follow the same rules as Vim, so regular, standalone
    /// characters are mapped verbatim, while "`<{mod}-{key}>`" and
    /// "`<{special}>`" sequences are mapped like in Vim.
    ///
    /// Here are the available special keys:
    ///
    /// - `<Enter> => Enter`,
    /// - `<Tab> => Tab`,
    /// - `<Bspc> => Backspace`,
    /// - `<Del> => Delete`,
    /// - `<Esc> => Esc`,
    /// - `<Up> => Up`,
    /// - `<Down> => Down`,
    /// - `<Left> => Left`,
    /// - `<Right> => Right`,
    /// - `<PageU> => PageUp`,
    /// - `<PageD> => PageDown`,
    /// - `<Home> => Home`,
    /// - `<End> => End`,
    /// - `<Ins> => Insert`,
    /// - `<F{1-12}> => F({1-12})`,
    ///
    /// And the following modifiers are available:
    ///
    /// - `C => Control`,
    /// - `A => Alt`,
    /// - `S => Shift`,
    /// - `M => Meta`,
    ///
    /// If another sequence already exists on the same mode, which
    /// would intersect with this one, the new sequence will not be
    /// added.
    pub fn map<M: Mode<Ui>>(take: &str, give: impl AsGives<Ui>) {
        mode::map::<M, Ui>(take, give);
    }

    /// Aliases a sequence of keys to another
    ///
    /// The difference between aliasing and mapping is that an alias
    /// will be displayed on the text as a [ghost text], making it
    /// seem like you are typing normally. This text will be printed
    /// with the `Alias` [form].
    ///
    /// If another sequence already exists on the same mode, which
    /// would intersect with this one, the new sequence will not be
    /// added.
    ///
    /// # Note
    ///
    /// This sequence is not like Vim's `alias`, in that if you make a
    /// mistake while typing the sequence, the alias is undone, and
    /// you will be just typing normally.
    ///
    /// The alias command also works on any [`Mode`], not just
    /// "insert like" modes. You can also use any key in the input or
    /// output of this `alias`
    ///
    /// [ghost text]: duat_core::text::Tag::GhostText
    /// [form]: crate::form::Form
    pub fn alias<M: Mode<Ui>>(take: &str, give: impl AsGives<Ui>) {
        mode::alias::<M, Ui>(take, give);
    }
}

pub mod cursor {
    //! Functions to alter the [`Cursors`] of Duat
    //!
    //! [`Cursors`]: duat_core::mode::Cursors
    pub use duat_core::form::{
        extra_cursor as get_extra, main_cursor as get_main, set_extra_cursor as set_extra,
        set_main_cursor as set_main, unset_extra_cursor as unset_extra,
        unset_main_cursor as unset_main,
    };
}

pub mod form {
    //! Functions to alter the [`Form`]s of Duat
    pub use duat_core::form::{
        Color, ColorScheme, CursorShape, Form, add_colorscheme, from_id, id_of, set,
        set_colorscheme,
    };
}

pub mod hooks {
    //! Hook utilities
    pub use duat_core::hooks::{
        ColorSchemeSet, ConfigLoaded, ConfigUnloaded, ExitedDuat, FormSet, Hookable, ModeSwitched,
        add, add_grouped, group_exists, remove,
    };

    use crate::Ui;
    /// [`Hookable`]: Triggers when a [`File`] is created
    ///
    /// # Arguments
    ///
    /// - The file [builder], which can be used to push widgets to the
    ///   file, and to eachother.
    ///
    /// [`File`]: duat_core::widgets::File
    /// [builder]: duat_core::ui::FileBuilder
    pub type OnFileOpen = duat_core::hooks::OnFileOpen<Ui>;

    /// [`Hookable`]: Triggers when a new window is opened
    ///
    /// # Arguments
    ///
    /// - The window [builder], which can be used to push widgets to
    ///   the edges of the window, surrounding the inner file region.
    ///
    /// [builder]: duat_core::ui::WindowBuilder
    pub type OnWindowOpen = duat_core::hooks::OnWindowOpen<Ui>;

    /// [`Hookable`]: Triggers when the [`Widget`] is focused
    ///
    /// # Arguments
    ///
    /// - The widget itself.
    ///
    /// [`Widget`]: duat_core::widgets::Widget
    pub type FocusedOn<W> = duat_core::hooks::FocusedOn<W, Ui>;

    /// [`Hookable`]: Triggers when the [`Widget`] is unfocused
    ///
    /// # Arguments
    ///
    /// - The widget itself.
    ///
    /// [`Widget`]: duat_core::widgets::Widget
    pub type UnfocusedFrom<W> = duat_core::hooks::UnfocusedFrom<W, Ui>;

    /// [`Hookable`]: Triggers whenever a [key] is sent
    ///
    /// # Arguments
    ///
    /// - The [key] sent.
    /// - An [`RwData<dyn Widget<U>>`] for the widget
    ///
    /// [key]: crate::mode::KeyEvent
    /// [`RwData<dyn Widget<U>>`]: crate::prelude::RwData
    pub type KeySent = duat_core::hooks::KeySent<Ui>;

    /// [`Hookable`]: Triggers whenever a [key] is sent to the [`Widget`]
    ///
    /// # Arguments
    ///
    /// - The [key] sent.
    /// - An [`RwData<W>`] for the widget.
    ///
    /// [key]: crate::mode::KeyEvent
    /// [`Widget`]: crate::widgets::Widget
    /// [`RwData<W>`]: crate::prelude::RwData
    pub type KeySentTo<W> = duat_core::hooks::KeySentTo<W, Ui>;
}

/// Plugs a list of plugins
///
/// These plugins should use the builder construction pattern, i.e.,
/// they should look like this:
///
/// ```rust
/// pub struct MyPlugin {
///     // ..options
/// }
///
/// impl MyPlugin {
///     pub fn new() -> Self {
///         // ...
///         # todo!();
///     }
///
///     pub fn modify1(self, parameter: bool) -> Self {
///         // ...
///         # todo!();
///     }
///
///     pub fn modify2(self, parameter: i32) -> Self {
///         // ...
///         # todo!();
///     }
///
///     pub fn plug(self) {
///         // Finally applies the plugin, after all alterations
///     }
/// }
/// ```
///
/// As you can see above, they should also have a `plug` method, which
/// consumes the plugin.
pub macro plug($($plugin:expr),+) {{
    $(
        plug_inner($plugin);
    )+
}}

#[doc(hidden)]
pub fn plug_inner(plugin: impl Plugin<Ui>) {
    plugin.plug()
}

pub mod widgets {
    //! Duat's builtin widgets
    pub use duat_core::{
        ui::Constraint,
        widgets::{File, LineNum, Widget, status},
    };

    use crate::Ui;

    pub type CmdLine = duat_core::widgets::CmdLine<Ui>;
    pub type CmdLineCfg = duat_core::widgets::CmdLineCfg<Ui>;
    pub type StatusLine = duat_core::widgets::StatusLine<Ui>;
    pub type StatusLineCfg = duat_core::widgets::StatusLineCfg<Ui>;
    pub type LineNumbers = duat_core::widgets::LineNumbers<Ui>;
    pub type LineNumbersCfg = duat_core::widgets::LineNumbersCfg<Ui>;
}

pub mod state {
    //! Common [`StatusLine`] fields
    //!
    //! [`StatusLine`]: crate::widgets::StatusLine
    pub use duat_core::widgets::common::*;
}

pub mod control {
    //! Prebuilt general controls for Duat
    //!
    //! This module contains the expected commands of a
    //! text editor that don't involve particular widgets
    //! or other more specific concepts
    //!
    //! All of the functions listed in here also have a [command]
    //! equivalent, that you can call from the [`CmdLine`].
    //!
    //! [command]: duat_core::cmd
    //! [`CmdLine`]: crate::prelude::CmdLine
    pub use duat_core::cmd::{
        alias, buffer, edit, next_file, next_global_file, prev_file, prev_global_file, quit,
    };
}

pub mod prelude {
    //! The prelude of Duat
    #[doc(hidden)]
    pub use duat_core;
    pub use duat_core::{
        DuatError, Error, Plugin, clipboard, cmd,
        data::{self, RwData},
        text::{self, Builder, Text, err, hint, ok, text},
        ui::Area as AreaTrait,
        widgets::Widget,
    };
    #[cfg(feature = "term-ui")]
    pub use duat_term::{self as ui, VertRule};

    pub use crate::{
        Area, Ui, control, cursor,
        form::{self, CursorShape, Form},
        hooks::{
            self, ColorSchemeSet, ConfigLoaded, ConfigUnloaded, ExitedDuat, FormSet, ModeSwitched,
            OnFileOpen, OnWindowOpen,
        },
        mode::{self, Cursors, Mode, alias, map},
        plug, print, setup_duat,
        state::*,
        widgets::*,
    };
}

/// Pre and post setup for Duat
///
/// This macro *MUST* be used in order for the program to run,
/// it will generate the function that actually runs Duat.
pub macro setup_duat($setup:expr) {
    use std::sync::mpsc;

    use crate::prelude::{
        Text,
        duat_core::{session::FileRet, ui, widgets::File},
    };

    #[unsafe(no_mangle)]
    fn run(
        ms: MetaStatics,
        prev_files: Vec<FileRet>,
        messengers: Messengers,
    ) -> (Vec<FileRet>, mpsc::Receiver<ui::DuatEvent>) {
        pre_setup();
        $setup();
        run_duat(ms, prev_files, messengers)
    }
}

// This will eventually be a NOT AND to check if any Uis have been
// chosen at all.
// Later, I'll also have an XOR checker to make sure only one Ui was
// chosen.
#[cfg(not(feature = "term-ui"))]
compile_error!("No ui has been chosen to compile Duat with.");

/// The [`Ui`](duat_core::ui::Ui) that Duat is using
///
/// In this case, that would be [`duat_term`]'s [`Ui`]
#[cfg(feature = "term-ui")]
pub type Ui = duat_term::Ui;
#[cfg(feature = "term-ui")]
pub type Area = <duat_term::Ui as duat_core::ui::Ui>::Area;

/// A function that sets the [`SessionCfg`].
type CfgFn = RwLock<Option<Box<dyn FnOnce(&mut SessionCfg<Ui>) + Send + Sync>>>;
