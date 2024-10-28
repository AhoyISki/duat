//! # Duat
//!
//! Duat is a text editor with Rust as its configuration language. It
//! makes use of a configuration crate in the user's `~/.config`
//! directory. This configuration crate works just like a regular Rust
//! crate, but it is dynamically loaded by Duat and executed on
//! startup.
//!
//! When installed, Duat will be able to automatically detect changes
//! in the user's configuration, adapting to them automatically, with
//! a very short delay.
//!
//! ## Features
//!
//! Duat provides a lot of features, trying to be as configurable as
//! possible, here are some of the things that Duat is capable of:
//!
//! - Completely custom input methods, with full Vim style remapping
//! - Completely custom widgets, with user created input methods
//! - Arbitrary concealment of text, and arbitrary ghost text
//! - Custom hooks, whose activation is up to the creator
//! - Multi UI adaptability, although for now, only a terminal UI has
//!   been made
//! - And many others still being planned
//!
//! Additionaly, by choosing Rust as its configuration language, Duat
//! also gains the following features:
//!
//! - Complete type safety
//! - A very functional programming language, with lots of native
//!   features
//! - Cargo is the plugin manager
//!
//! ## How to use
//!
//! In order to use it, you must have `cargo` installed. If you do,
//! run
//!
//! `cargo install duat`
//!
//! This will install the default version of Duat, which uses a
//! terminal user interface. It will also create a configuration
//! directory in `$XDG_CONFIG_HOME/duat/` or `~/.config/duat/`. This
//! config will have some default changes, but you can modify it as
//! you wish. It also has some documentation explaining the basics of
//! Duat.
//!
//! For now, it has a barebones configuration, which is based on
//! Kakoune, so if you are familiar with that text editor, many of the
//! commands are going to be the same.
//!
//! ## Configuration
//!
//! In the configuration file, there should be a `setup_duat!` macro.
//! This macro takes in a function pointer and executes it as setup
//! for Duat.
//!
//! Here's an example configuration file, which makes use of
//! `duat-kak`
//!
//! ```rust
//! # mod duat_kak {
//! #     use duat::{prelude::{mode::*, data::RwData}, Area, Ui, widgets::File};
//! #     #[derive(Clone)]
//! #     pub struct Normal;
//! #     impl Mode<Ui> for Normal {
//! #         type Widget = File;
//! #         fn send_key(
//! #             &mut self,
//! #             key: KeyEvent,
//! #             widget: &RwData<File>,
//! #             area: &Area,
//! #             cursors: Option<Cursors>,
//! #         ) -> Option<Cursors> {
//! #             todo!();
//! #         }
//! #     }
//! #     #[derive(Clone)]
//! #     pub struct Insert;
//! #     impl Mode<Ui> for Insert {
//! #         type Widget = File;
//! #         fn send_key(
//! #             &mut self,
//! #             key: KeyEvent,
//! #             widget: &RwData<File>,
//! #             area: &Area,
//! #             cursors: Option<Cursors>,
//! #         ) -> Option<Cursors> {
//! #             todo!();
//! #         }
//! #     }
//! # }
//! setup_duat!(setup);
//! use duat::prelude::*;
//! use duat_kak::{Insert, Normal};
//!
//! fn setup() {
//!     mode::set_default(Normal);
//!     map::<Insert>(keys!("jk"), keys!(Esc));
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
//!         builder.push_to(CommandLine::cfg().left_ratioed(3, 7), child);
//!     });
//!
//!     hooks::add::<ModeSwitched>(|&(_, new)| match new {
//!         "Insert" => cursor::set_main(CursorShape::SteadyBar),
//!         _ => cursor::set_main(CursorShape::SteadyBlock)
//!     });
//!
//!     forms::set("Mode", Form::dark_magenta());
//! }
//! ```
//!
//! This configuration does the following things:
//!
//! - Changes the [default mode] to a Kakoune inspired `Normal`;
//! - [Maps] jk to [esc];
//! - [Changes] the wrapping;
//! - [Removes] the hook [group] "FileWidgets";
//! - [Pushes] a [vertical rule] and [line numbers] to every file;
//! - Removes the hook group "WindowWidgets";
//! - Pushes a [custom status line] and [command line] to the bottom
//!   of the screen;
//! - [Adds] hooks for [mode changes] in Duat;
//! - [Changes](forms::set) the [style] of the mode printed on the
//!   status line;
//!
//! These are some of the ways you can configure Duat. You might
//! notice some things that can be done with these simle options:
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
//! let callers = ["collapse-command-line", "collapse-cmd"];
//! commands::add_for_widget::<CommandLine>(
//!     callers,
//!     |_command_line, area, _cursors, _flags, _args| {
//!         area.constrain_ver(Constraint::Length(0.0))?;
//!
//!         Ok(None)
//!     },
//! )
//! # }
//! ```
//!
//! The 2 arguments
//!
//! ## Roadmap
//!
//! These are the goals that have been acomplished or are on their
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
//! - [ ] Add floating widgets, not tied to the session layout;
//! - [ ] Implement autocompletion lists;
//! - [ ] Implement tree-sitter;
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
//! with the lack of some
//! features, like folding, multiple file editing, the general
//! barebonesness of the configuration, etc.
//!
//! I know that Neovim has all of these features, and Helix supposedly
//! tries to
//! solve some of these issues. But I don't really like either of
//! their editing
//! styles to be honest.
//!
//! And so I thought, why not make my own text editor?
//!
//! I thought, why not make a text editor that is as modular as
//! possible, while
//! still having a sensible default configuration? That I could modify
//! however I
//! wanted, and with a language that I love?
//!
//! That is why I decided to embark on this journey.
//!
//! ## Why the name
//!
//! idk, cool sounding word that I got from Spelunky 2.
//!
//! Also, just wanted to say that no AI was used in this project, cuz
//! I don't like it.
//!
//! [default mode]: mode::set_default
//! [Maps]: prelude::map
//! [esc]: prelude::keys
//! [Changes]: prelude::print::wrap_on_width
//! [Removes]: hooks::remove
//! [group]: hooks::add_grouped
//! [Pushes]: duat_core::ui::FileBuilder
//! [vertical rule]: prelude::VertRule
//! [line numbers]: prelude::LineNumbers
//! [custom status line]: prelude::status
//! [command line]: prelude::CommandLine
//! [Adds]: hooks::add
//! [mode changes]: hooks::ModeSwitched
//! [style]: forms::Form
//! [`status!`]: prelude::status
//! [tags]: duat_core::text::Tag
#![feature(decl_macro)]

use std::sync::RwLock;

use duat_core::session::SessionCfg;
pub use duat_core::thread;
pub use setup::{pre_setup, run_duat};

pub mod commands;
pub mod mode;
pub mod print;
mod remapper;
mod setup;

pub mod cursor {
    //! Functions to alter the [`Cursors`] of Duat
    //!
    //! [`Cursors`]: duat_core::input::Cursors
    pub use duat_core::forms::{
        extra_cursor as get_extra, main_cursor as get_main, set_extra_cursor as set_extra,
        set_main_cursor as set_main, unset_extra_cursor as unset_extra,
        unset_main_cursor as unset_main,
    };
}

pub mod forms {
    //! Functions to alter the [`Form`]s of Duat
    pub use duat_core::forms::{CursorShape, Form, from_id, id_of, set};
}

pub mod hooks {
    //! Hook utilites
    pub use duat_core::hooks::{ModeSwitched, add, add_grouped, group_exists, remove};

    use crate::Ui;
    /// Triggers whenever a [`File`] is created
    ///
    /// # Arguments
    ///
    /// - The file [builder], which can be used to push widgets to the
    ///   file, and to eachother.
    ///
    /// [`File`]: duat_core::widgets::File
    /// [builder]: duat_core::ui::FileBuilder
    pub type OnFileOpen = duat_core::hooks::OnFileOpen<Ui>;

    /// Triggers whenever a new window is opened
    ///
    /// # Arguments
    ///
    /// - The window [builder], which can be used to push widgets to
    ///   the edges of the window, surrounding the inner file region.
    ///
    /// [builder]: duat_core::ui::WindowBuilder
    pub type OnWindowOpen = duat_core::hooks::OnWindowOpen<Ui>;

    /// Triggers whenever the given [`Widget`] is focused
    ///
    /// # Arguments
    ///
    /// - The widget itself.
    ///
    /// [`widget`]: duat_core::widgets::Widget
    pub type FocusedOn<W> = duat_core::hooks::FocusedOn<W, Ui>;

    /// Triggers whenever the given [`Widget`] is unfocused
    ///
    /// # Arguments
    ///
    /// - The widget itself.
    ///
    /// [`widget`]: duat_core::widgets::Widget
    pub type UnfocusedFrom<W> = duat_core::hooks::UnfocusedFrom<W, Ui>;
}

pub mod plugin {
    //! Functions to load [`Plugin`]s
    pub use duat_core::Plugin;

    use crate::{Ui, setup::PLUGIN_FN};

    /// Loads the [`Plugin`]
    pub fn load<P: Plugin<Ui>>() {
        let mut old = PLUGIN_FN.write().unwrap();
        let old_f = std::mem::replace(&mut *old, Box::new(|_| {}));
        *old = Box::new(|cfg| {
            old_f(cfg);
            cfg.load_plugin::<P>();
        });
    }

    /// Loads the [`Plugin`], then mutates it
    pub fn load_and<P: Plugin<Ui>>(f: impl FnOnce(&mut P) + Send + Sync + 'static) {
        let mut old = PLUGIN_FN.write().unwrap();
        let old_f = std::mem::replace(&mut *old, Box::new(|_| {}));
        *old = Box::new(|cfg| {
            old_f(cfg);
            cfg.load_plugin_and(f);
        });
    }
}

pub mod widgets {
    //! Duat's builtin widgets
    pub use duat_core::{
        ui::Constraint,
        widgets::{File, status},
    };

    use crate::Ui;

    pub type CommandLine = duat_core::widgets::CommandLine<Ui>;
    pub type CommandLineCfg = duat_core::widgets::CommandLineCfg<Ui>;
    pub type StatusLine = duat_core::widgets::StatusLine<Ui>;
    pub type StatusLineCfg = duat_core::widgets::StatusLineCfg<Ui>;
    pub type LineNumbers = duat_core::widgets::LineNumbers<Ui>;
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
    //! equivalent, that you can call from the [`CommandLine`].
    //!
    //! [command]: duat_core::commands
    //! [`CommandLine`]: crate::prelude::CommandLine
    pub use duat_core::commands::{
        alias, buffer, edit, next_file, next_global_file, prev_file, prev_global_file, quit,
    };
}

pub mod prelude {
    //! The prelude of Duat
    pub use duat_core::{
        self, DuatError, Error, data,
        text::{Builder, Text, err, hint, ok, text},
        ui::Area,
        widgets::Widget,
    };
    #[cfg(feature = "term-ui")]
    pub use duat_term::{self as ui, VertRule};

    pub use crate::{
        Ui, commands, control, cursor,
        forms::{self, CursorShape, Form},
        hooks::{self, ModeSwitched, OnFileOpen, OnWindowOpen},
        mode::{self, keys, map},
        print, setup_duat,
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

    use crate::prelude::duat_core::{data::RwData, ui, widgets::File};

    #[no_mangle]
    fn run(
        prev_files: Vec<(RwData<File>, bool)>,
        tx: mpsc::Sender<ui::Event>,
        rx: mpsc::Receiver<ui::Event>,
        statics: <Ui as ui::Ui>::StaticFns,
    ) -> Vec<(RwData<File>, bool)> {
        pre_setup();

        $setup();

        run_duat(prev_files, tx, rx, statics)
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
