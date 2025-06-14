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
//! you can see the changes in *almost* real time. Initially, building
//! Duat and its configuration crate might take a few minutes. And the
//! first reload might also take a similar amount of time. But
//! whenever you make new changes, the next reloads should take only
//! about a second (for debug profile) and ~3 seconds (for release
//! profile).
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
//! cargo install --path duat --features git-deps
//! ```
//!
//! And if you want to nuke your config in order to get the newest
//! default config crate, you can do the following:
//!
//! ```text
//! rm -rf ~/.config/duat
//! git clone https://github.com/AhoyISki/duat
//! cargo install --path duat --features git-deps
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
//! # mod treesitter {
//! #     pub struct TreeSitter;
//! #     impl duat_core::Plugin<duat::Ui> for TreeSitter {
//! #         fn new() -> Self {
//! #             Self
//! #         }
//! #         fn plug(self) {}
//! #     }
//! # }
//! # mod kak {
//! #     use duat::{prelude::{*, mode::KeyEvent}};
//! #     #[derive(Clone)]
//! #     pub struct Normal;
//! #     impl Mode<Ui> for Normal {
//! #         type Widget = File;
//! #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<File>) {
//! #             todo!();
//! #         }
//! #     }
//! #     #[derive(Clone)]
//! #     pub struct Insert;
//! #     impl Mode<Ui> for Insert {
//! #         type Widget = File;
//! #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<File>) {
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
//!     plug!(treesitter::TreeSitter::new(), kak::Kak::new());
//!     map::<Insert>("jk", "<Esc>");
//!
//!     print::wrap_on_edge();
//!
//!     hook::remove("FileWidgets");
//!     hook::add::<OnFileOpen>(|pa, builder| {
//!         builder.push(pa, VertRule::cfg());
//!         builder.push(pa, LineNumbers::cfg());
//!     });
//!
//!     hook::remove("WindowWidgets");
//!     hook::add::<OnWindowOpen>(|pa, builder| {
//!         let upper_mode = mode_name().map(|m| match m.split_once('<') {
//!             Some((no_generics, _)) => no_generics.to_uppercase(),
//!             None => m.to_uppercase(),
//!         });
//!         let status_line = status!("[Mode]{upper_mode}{Spacer}{file_fmt} {sels_fmt} {main_fmt}");
//!
//!         builder.push(pa, status_line);
//!         let (child, _) = builder.push(pa, PromptLine::cfg());
//!         builder.push_to(pa, child, Notifications::cfg());
//!     });
//!
//!     hook::add::<ModeSwitched>(|_, (_, new)| match new {
//!         "Insert" => cursor::set_main(CursorShape::SteadyBar),
//!         _ => cursor::set_main(CursorShape::SteadyBlock),
//!     });
//!
//!     form::set("Mode", Form::dark_magenta());
//! }
//! ```
//!
//! This configuration does the following things:
//!
//! - [plugs] the `Kak` plugin, which changes the [default mode], and
//!   the `TreeSitter` plugin, which adds syntax highlighting and is
//!   also used by the `Kak` plugin;
//! - [Maps] jk to esc in the `Insert` mode;
//! - [Changes] the wrapping;
//! - [Removes] the hook [group] "FileWidgets";
//! - [Pushes] a [vertical rule] and [line numbers] to every file;
//! - [Removes] the hook group "WindowWidgets";
//! - Pushes a [custom status line] (with a [Spacer] for 2 separate
//!   sides, and a reformatted [`mode_name`]), a [command line], and a
//!   [notifications widget] to the bottom of the screen;
//! - [Adds] hooks for [mode changes] in Duat;
//! - [Changes](form::set) the [style] of the mode printed on the
//!   status line;
//!
//! These are some of the ways you can configure Duat. You might
//! notice some things that can be done with these simple options:
//!
//! ```rust
//! use duat::prelude::*;
//! # fn test() {
//! hook::add::<OnFileOpen>(|pa, builder| {
//!     builder.push(pa, VertRule::cfg());
//!     builder.push(pa, LineNumbers::cfg());
//!     builder.push(pa, VertRule::cfg().on_the_right());
//!     builder.push(pa, LineNumbers::cfg().on_the_right());
//! });
//! # }
//! ```
//!
//! Now, every file will open with two lines of numbers, one on each
//! side. Would you ever want to do this? ...No, not really, but it
//! shows how configurable Duat can be.
//!
//! Duat also comes with a fully fledged [text creation system], which
//! significantly eases the creation of widgets:
//!
//! ```rust
//! # use duat::prelude::*;
//! let text = txt!("[my_form]Waow it's my form![]not anymore 😢");
//! ```
//!
//! In this example, I'm using the "my_form" form in order to style
//! the text, while `[]` reverts back to the "default" form. Double
//! `[[` and `]]` escape the `[` and `]`, just like in [`println!`].
//! The [`status!`] macro works similarly.
//!
//! Duat also has a simple command system, that lets you add commands
//! with arguments supported by Rust's type system. As an example,
//! this command will change the [numbering] of a [`LineNumbers`]
//! widget, switching between absolute and relative numbering.
//!
//! # Default plugins
//!
//! When you install duat, the default config crate will come with
//! some preinstalled plugins:
//!
//! - [`duat-kak`] is a plugin that changes the default mode of Duat
//!   to one inspired by [Kakoune]'s "Normal", also bringing with it
//!   various other modes from Kakoune.
//! - [`duat-catppuccin`] is a just a simple colorscheme plugin, it
//!   adds the four flavors from the [catppuccin] colorscheme. You can
//!   pick between the four of them, you can apply its colors to other
//!   [`Form`]s and you can allow or disallow the colorscheme to set
//!   the background color.
//! - [`duat-treesitter`] brings [tree-sitter] to Duat in the form of
//!   syntax highlighting and indentation calculation, which can be
//!   used by Modes (such as those from `duat-kak`) in order to give
//!   better feedback when editing files.
//!
//! You can, of course, unplug these by not calling [`plug!`], or you
//! could remove them entirely by taking them out of the
//! `Cargo.toml`'s [dependencies section].
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
//! [plugs]: prelude::plug
//! [default mode]: prelude::mode::set_default
//! [Maps]: prelude::map
//! [Changes]: prelude::print::wrap_on_edge
//! [Removes]: prelude::hook::remove
//! [group]: prelude::hook::add_grouped
//! [Pushes]: prelude::ui::FileBuilder::push
//! [vertical rule]: prelude::VertRule
//! [line numbers]: prelude::LineNumbers
//! [custom status line]: prelude::status
//! [Spacer]: prelude::Spacer
//! [`mode_name`]: prelude::mode_name
//! [command line]: prelude::PromptLine
//! [notifications widget]: prelude::Notifications
//! [Adds]: prelude::hook::add
//! [mode changes]: prelude::hook::ModeSwitched
//! [style]: prelude::form::Form
//! [text creation system]: prelude::text::txt
//! [`status!`]: prelude::status
//! [numbering]: prelude::LineNum
//! [`LineNumbers`]: prelude::LineNumbers
//! [tags]: duat_core::text::Tag
//! [`duat-kak`]: https://github.com/AhoyISki/duat-kak
//! [Kakoune]: https://github.com/mawww/kakoune
//! [`duat-catppuccin`]: https://github.com/AhoyISki/duat-catppuccin
//! [catppuccin]: https://catppuccin.com
//! [`Form`]: prelude::Form
//! [`duat-treesitter`]: https://github.com/AhoyISki/duat-treesitter
//! [tree-sitter]: https://tree-sitter.github.io/tree-sitter
//! [`plug!`]: prelude::plug
//! [dependencies section]: https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html
#![feature(decl_macro, let_chains)]

use std::sync::RwLock;

use duat_core::session::SessionCfg;
pub use duat_core::{crate_dir, duat_name, src_crate};

pub use self::setup::{Messengers, MetaStatics, pre_setup, run_duat};

pub mod print;
mod setup;

/// Commands for the manipulation of [`Mode`]s
///
/// [`Mode`]: crate::mode::Mode
pub mod mode {
    use duat_core::mode;
    pub use duat_core::mode::*;

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
    /// [ghost text]: duat_core::text::Ghost
    /// [form]: crate::form::Form
    pub fn alias<M: Mode<Ui>>(take: &str, give: impl AsGives<Ui>) {
        mode::alias::<M, Ui>(take, give);
    }
}

/// Functions to alter the [`Cursors`] of Duat
///
/// [`Cursors`]: duat_core::mode::Cursors
pub mod cursor {
    pub use duat_core::form::{
        extra_cursor as get_extra, main_cursor as get_main, set_extra_cursor as set_extra,
        set_main_cursor as set_main, unset_extra_cursor as unset_extra,
        unset_main_cursor as unset_main,
    };
}

/// Functions to alter the [`Form`]s of Duat
///
/// [`Form`]: crate::form::Form
pub mod form {
    pub use duat_core::form::{
        Color, ColorScheme, CursorShape, Form, add_colorscheme, enable_mask, from_id, id_of, set,
        set_colorscheme, set_many,
    };
}

/// Hook utilities
pub mod hook {
    pub use duat_core::hook::{
        ColorSchemeSet, ConfigLoaded, ConfigUnloaded, ExitedDuat, FileWritten, FormSet, Hookable,
        KeysSent, KeysSentTo, ModeSwitched, add, add_grouped, group_exists, remove,
    };
    pub use duat_utils::hooks::*;

    use crate::Ui;

    /// [`Hookable`]: Triggers when a [`Widget`]'s [cfg] is created
    ///
    /// # Arguments
    ///
    /// - The [`WidgetCfg`] in question.
    /// - The [`FileHandle`] to which it was pushed, if there was one.
    ///
    /// Do note that this [hook] doesn't let you change the layout of
    /// the pushed [`Widget`]s, only their configuration. So if
    /// one of these changes changed the [direction] where the
    /// [`Widget`] was pushed, for example, the layout could get
    /// messed up.
    ///
    /// If you want to change the layout of the [`Widget`]s in a
    /// controled fashion, look at [`OnFileOpen`] and
    /// [`OnWindowOpen`].
    ///
    /// [`Widget`]: crate::prelude::Widget
    /// [`FileHandle`]: crate::prelude::context::FileHandle
    /// [cfg]: crate::prelude::Widget::Cfg
    /// [`WidgetCfg`]: crate::prelude::WidgetCfg
    /// [hook]: self
    /// [direction]: crate::prelude::ui::PushSpecs
    pub type WidgetCreated<W> = duat_core::hook::WidgetCreated<W, Ui>;

    /// [`Hookable`]: Triggers when a [`File`] is opened
    ///
    /// # Arguments
    ///
    /// - The file [builder], which can be used to push widgets to the
    ///   file, and to eachother.
    ///
    /// This is a rather "advanced" [hook], since it lets you change
    /// the layout of [`Widget`]s around the screen. If you don't
    /// need all that power, you can check out [`WidgetCreated`],
    /// which is a more straightforward form of changing
    /// [`Widget`]s, and doesn't interfere with the default hooks
    /// of `"FileWidgets"` and `"WindowWidgets"`, preset by Duat.
    ///
    /// [`File`]: crate::prelude::File
    /// [`Widget`]: crate::prelude::Widget
    /// [builder]: crate::prelude::ui::FileBuilder
    /// [hook]: self
    pub type OnFileOpen = duat_core::hook::OnFileOpen<Ui>;

    /// [`Hookable`]: Triggers when a new window is opened
    ///
    /// # Arguments
    ///
    /// - The window [builder], which can be used to push widgets to
    ///   the edges of the window, surrounding the inner file region.
    ///
    ///
    /// This is a rather "advanced" [hook], since it lets you change
    /// the layout of [`Widget`]s around the screen. If you don't
    /// need all that power, you can check out [`WidgetCreated`],
    /// which is a more straightforward form of changing
    /// [`Widget`]s, and doesn't interfere with the default hooks
    /// of `"FileWidgets"` and `"WindowWidgets"`, preset by Duat.
    ///
    /// [`File`]: crate::prelude::File
    /// [`Widget`]: crate::prelude::Widget
    /// [builder]: crate::prelude::ui::WindowBuilder
    /// [hook]: self
    pub type OnWindowOpen = duat_core::hook::OnWindowOpen<Ui>;

    /// [`Hookable`]: Triggers when the [`Widget`] is focused
    ///
    /// # Arguments
    ///
    /// - The [`Handle`] for the [`Widget`]
    ///
    /// [`Handle`]: crate::prelude::Handle
    /// [`Widget`]: crate::prelude::Widget
    pub type FocusedOn<W> = duat_core::hook::FocusedOn<W, Ui>;

    /// [`Hookable`]: Triggers when the [`Widget`] is unfocused
    ///
    /// # Arguments
    ///
    /// - The [`Handle`] for the [`Widget`]
    ///
    /// [`Handle`]: crate::prelude::Handle
    /// [`Widget`]: crate::prelude::Widget
    pub type UnfocusedFrom<W> = duat_core::hook::UnfocusedFrom<W, Ui>;

    /// [`Hookable`]: Triggers whenever a [key] is sent to the [`Widget`]
    ///
    /// # Arguments
    ///
    /// - The [key] sent.
    /// - The [`Handle`] for its [`Widget`].
    ///
    /// [key]: crate::mode::KeyEvent
    /// [`Handle`]: crate::prelude::Handle
    /// [`Widget`]: crate::prelude::Widget
    pub type KeySentTo<W> = duat_core::hook::KeysSentTo<W, Ui>;

    /// [`Hookable`]: Lets you modify a [`Mode`] as it is set
    ///
    /// # Arguments
    ///
    /// - The new mode.
    /// - The [`Handle`] for its [`Widget`].
    ///
    /// This hook is very useful if you want to, for example, set
    /// different options upon switching to modes, depending on things
    /// like the language of a [`File`].
    ///
    /// [`Mode`]: crate::mode::Mode
    /// [`File`]: crate::widgets::File
    /// [`Handle`]: crate::prelude::Handle
    /// [`Widget`]: crate::prelude::Widget
    pub type ModeSetTo<M> = duat_core::hook::ModeSetTo<M, Ui>;
}

/// Duat's builtin widgets
pub mod widgets {
    pub use duat_core::ui::{Widget, WidgetCfg};
    pub use duat_utils::widgets::*;

    /// The widget that is used to print and edit files
    pub type File = duat_core::file::File<super::Ui>;
}

/// Common [`StatusLine`] fields
///
/// [`StatusLine`]: duat_utils::widgets::StatusLine
pub mod state {
    pub use duat_utils::state::*;
}

#[allow(unused_imports)]
use duat_core::{session::FileRet, ui::DuatEvent};

/// Pre and post setup for Duat
///
/// This macro *MUST* be used in order for the program to run,
/// it will generate the function that actually runs Duat.
pub macro setup_duat($setup:expr) {
    use std::sync::mpsc;

    use $crate::prelude::{File, Text, context::Logs};

    #[unsafe(no_mangle)]
    fn run(
        logs: Logs,
        ms: MetaStatics,
        prev_files: Vec<Vec<FileRet>>,
        (duat_tx, duat_rx): Messengers,
    ) -> (
        Vec<Vec<FileRet>>,
        mpsc::Receiver<DuatEvent>,
        Option<std::time::Instant>,
    ) {
        pre_setup(Some(logs), duat_tx);
        $setup();
        run_duat(ms, prev_files, duat_rx)
    }
}

/// The prelude of Duat
pub mod prelude {
    use std::process::Output;

    pub use duat_core::{
        Plugin, clipboard, cmd, context,
        data::{self, Pass},
        text::{
            self, AlignCenter, AlignLeft, AlignRight, Builder, Conceal, Ghost, Spacer, Text, txt,
        },
        ui::{self, RawArea, Widget, WidgetCfg},
    };
    #[cfg(feature = "term-ui")]
    pub use duat_term::{self as term, VertRule};

    pub use crate::{
        Area, Ui, cursor,
        form::{self, CursorShape, Form},
        hook::{
            self, ColorSchemeSet, ConfigLoaded, ConfigUnloaded, ExitedDuat, FileWritten, FocusedOn,
            FormSet, ModeSetTo, ModeSwitched, OnFileOpen, OnWindowOpen, SearchPerformed,
            SearchUpdated, UnfocusedFrom,
        },
        mode::{self, Mode, alias, map},
        print, setup_duat,
        state::*,
        widgets::*,
    };

    /// Plugs a list of plugins
    ///
    /// These plugins should use the builder construction pattern,
    /// i.e., they should look like this:
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
    /// As you can see above, they should also have a `plug` method,
    /// which consumes the plugin.
    pub macro plug($($plugin:expr),+ $(,)?) {{
        $(
            plug_inner($plugin);
        )+
    }}

    /// Executes a shell command, returning its [`Output`] if
    /// successful
    pub fn exec(command: impl ToString) -> Option<Output> {
        let command = command.to_string();
        let mut chars = command.char_indices();
        let mut start = None;
        let mut end = None;
        let mut is_quoting = false;
        let mut last_char = 'a';

        let mut args = std::iter::from_fn(|| {
            for (b, char) in chars.by_ref() {
                let lc = last_char;
                last_char = char;
                if start.is_some() && char.is_whitespace() && !is_quoting {
                    end = Some(b);
                    break;
                } else if char == '"' && lc != '\\' {
                    is_quoting = !is_quoting;
                    if !is_quoting {
                        end = Some(b + 1);
                        break;
                    } else {
                        start = Some(b);
                    }
                } else if !char.is_whitespace() && start.is_none() {
                    start = Some(b);
                }
            }

            start.take().map(|s| unsafe {
                let e = end.take().unwrap_or(command.len());
                core::str::from_utf8_unchecked(&command.as_bytes()[s..e])
            })
        });

        let mut cmd = std::process::Command::new(args.next()?);
        cmd.args(args);

        cmd.output().ok()
    }

    #[doc(hidden)]
    pub fn plug_inner(plugin: impl Plugin<Ui>) {
        plugin.plug()
    }

    /// A handle to a [`Widget`] in Duat
    ///
    /// The [`Handle`] lets you do all sorts of edits on a [`Widget`].
    /// You can, for example, make use of the [`Selection`]s in
    /// its [`Text`] in order to edit the [`Text`] in a very
    /// declarative way.
    ///
    /// ```rust
    /// use duat::prelude::*;
    ///
    /// fn set_hook() {
    ///     hook::add::<UnfocusedFrom<File>>(|pa, handle| {
    ///         handle.edit_all(pa, |mut cursor| {
    ///             cursor.set_caret_on_end();
    ///             cursor.unset_anchor();
    ///         });
    ///     });
    /// }
    /// ```
    ///
    /// In this case, the hook above will unset every `anchor` of each
    /// [`Selection`] when you unfocus from a [`File`]. You could call
    /// this from the [setup] function, in order to enable said [hook]
    ///
    /// [`Selection`]: crate::mode::Selection
    /// [setup]: crate::prelude::setup_duat
    pub type Handle<W> = duat_core::context::Handle<W, Ui>;
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
/// The [`RawArea`] of the [`Ui`]
///
/// [`RawArea`]: duat_core::ui::RawArea
pub type Area = <duat_term::Ui as duat_core::ui::Ui>::Area;

/// A function that sets the [`SessionCfg`].
type CfgFn = RwLock<Option<Box<dyn FnOnce(&mut SessionCfg<Ui>) + Send + Sync>>>;
