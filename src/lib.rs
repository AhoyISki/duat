//! Duat is a text editor built with extensibility and performance in
//! mind, while still having very sensible defaults. It is configured
//! in Rust, by use of a crate. The choice of Rust for configuration
//! grants several benefits:
//!
//! * Safe code by default;
//! * Top quality code checking through rust-analyzer;
//! * Cargo is the plugin manager;
//! * The vast expressiveness of the Rust type system;
//! * Rust is just a fun language to code in;
//!
//! The use of Rust, to some, may seem like a mistake, since it is not
//! a "beginner friendly language", but Duat is developed with ease of
//! configuration in mind, so an average configuration file won't be
//! as verbose as a lot of Rust code, while maintaining good levels of
//! readability:
//!
//! ```rust,ignore
//! use duat::prelude::*;
//! use duat_kak::{KeyMap, Mode, OnModeChange};
//!
//! run! {
//!     print::wrap_on_width();
//!
//!     hooks::remove_group("FileWidgets");
//!     hooks::add::<OnFileOpen>(|builder| {
//!         builder.push::<VertRule>();
//!         builder.push::<LineNumbers>();
//!     });
//!
//!     hooks::remove_group("WindowWidgets");
//!     hooks::add::<OnWindowOpen>(|builder| {
//!         let status = status!(
//!             [File] { File::name } " "
//!             { KeyMap::mode_fmt } " "
//!             selections_fmt " " main_fmt
//!         );
//!
//!         let (child, _) = builder.push_cfg(status);
//!         builder.push_cfg_to(CommandLine::cfg().left_with_percent(30), child);
//!     });
//!
//!     input::set(KeyMap::new());
//!
//!     hooks::add::<OnModeChange>(|(_, new)| match new {
//!         Mode::Insert => cursor::set_main(CursorShape::SteadyBar),
//!         _ => cursor::set_main(CursorShape::SteadyBlock)
//!     });
//!
//!     forms::set("Mode", Form::new().dark_magenta());
//! }
//! ```
//!
//! This configuration does the following things:
//!
//! - Changes the wrapping;
//! - Removes the hook group "FileWidgets";
//! - Pushes a vertical rule and line numbers to every file;
//! - Removes the hook group "WindowWidgets";
//! - Pushes a status line and command line to the bottom of the
//!   screen;
//! - Changes the input method to a Kakoune inspired duat-kak;
//! - Adds hooks for mode changes in said input method;
//! - Changes the style of the mode printed on the status line;
//!
//! These are some of the configurations available for usage in Duat,
//! and one might perceive how these can be tinkered with to change a
//! lot of things within Duat. For example, the usage of hooks to push
//! widgets to files means that you could do this:
//!
//! ```rust
//! # use crate::prelude::*;
//! hooks::add::<OnFileOpen>(|builder| {
//!     builder.push::<VertRule>();
//!     builder.push::<LineNumbers>();
//!     builder.push_cfg(LineNumbers::cfg().on_the_right());
//!     builder.push_cfg(LineNumbers::cfg().on_the_right());
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
//! # use crate::prelude::*
//! let styled text = text!([MyForm] "Waow it's my text" [] "not anymore 😢");
//! ```
//!
//! In this example, I'm using the "MyForm" form in order to style the
//! text, while `[]` reverts back to the "Default" form.
//!
//! With the [tags] provided by Duat, you can also change the
//! alignment, conceal text, add ghost text that can't be interacted
//! with, and also add buttons that take mouse input (in the future).
//! These other tags are particularly useful when one wants to style
//! the file, where multiple plugins may insert tags that do not
//! interact with eachother.
//!
//! Duat also has a simple command system, that lets you add commands
//! with arguments supported by Rust's type system:
//! ```rust
//! # use crate::prelude;
//! let callers = ["collapse-command-line", "collapse-cmd"];
//! commands::add_for_widget::<CommandLine>(callers, |command_line, area| {
//!     area.constrain_ver(Constraint::Length(0))?;
//!
//!     Ok(None)
//! })
//! ```
//!
//! [tags]: duat_core::text::Tag
#![feature(decl_macro)]

use std::sync::RwLock;

use duat_core::session::SessionCfg;
pub use duat_core::thread;
pub use setup::{pre_setup, run_duat};

pub mod commands;
pub mod control;
pub mod input;
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
    pub use duat_core::forms::{CursorShape, Form, from_id, set, id_of};
}

pub mod hooks {
    //! Hook utilites
    pub use duat_core::hooks::{add, add_grouped, group_exists, remove_group};

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

    /// Triggers whenever the given [`widget`] is focused
    ///
    /// # Arguments
    ///
    /// - The widget itself.
    ///
    /// [`widget`]: duat_core::widgets::ActiveWidget
    pub type FocusedOn<W> = duat_core::hooks::FocusedOn<W, Ui>;

    /// Triggers whenever the given [`widget`] is unfocused
    ///
    /// # Arguments
    ///
    /// - The widget itself.
    ///
    /// [`widget`]: duat_core::widgets::ActiveWidget
    pub type UnfocusedFrom<W> = duat_core::hooks::UnfocusedFrom<W, Ui>;
}

pub mod plugin {
    //! Functions to load [`Plugin`]s
    pub use duat_core::Plugin;

    use crate::{
        Ui,
        setup::{CONTEXT, PLUGIN_FN},
    };

    /// Loads the [`Plugin`]
    pub fn load<P>()
    where
        P: Plugin<Ui>,
    {
        let mut old = PLUGIN_FN.write().unwrap();
        let old_f = std::mem::replace(&mut *old, Box::new(|_| {}));
        *old = Box::new(|cfg| {
            old_f(cfg);
            cfg.load_plugin::<P>(CONTEXT);
        });
    }

    /// Loads the [`Plugin`], then mutates it
    pub fn load_then<P>(f: impl FnOnce(&mut P) + Send + Sync + 'static)
    where
        P: Plugin<Ui>,
    {
        let mut old = PLUGIN_FN.write().unwrap();
        let old_f = std::mem::replace(&mut *old, Box::new(|_| {}));
        *old = Box::new(|cfg| {
            old_f(cfg);
            cfg.load_plugin_then::<P>(CONTEXT, f);
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
    pub type CommandLineCfg<I> = duat_core::widgets::CommandLineCfg<I, Ui>;
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

pub mod prelude {
    //! The prelude of Duat
    pub use duat_core::{
        self, DuatError, Error, data,
        text::{Builder, Text, err, hint, ok, text},
        ui::Area,
        widgets::PassiveWidget,
    };
    #[cfg(feature = "term-ui")]
    pub use duat_term::{self as ui, VertRule};

    pub use crate::{
        Ui, commands, control, cursor,
        forms::{self, CursorShape, Form},
        hooks::{self, OnFileOpen, OnWindowOpen},
        input, print, setup_duat,
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

/// A function that sets the [`SessionCfg`].
type CfgFn = RwLock<Option<Box<dyn FnOnce(&mut SessionCfg<Ui>) + Send + Sync>>>;
