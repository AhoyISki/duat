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
//! Duat and its configuration crate might take a while. But while
//! reloading Duat, in my (somewhat old) computer, it takes ~0.7 (for
//! small changes) to ~2.3 seconds (for bigger changes) to reload
//! the config.
//!
//! Note that this is an alpha project, so there may be some quirks
//! and bugs. So if you have a problem, or something seems confusing,
//! feel free to ask questions or raise an issue, that would be very
//! welcome 🥰.
//!
//! ## Getting started
//!
//! > [!NOTE]
//! >
//! > On this section, I will be referring to duat's configuration by
//! > `~/.config/duat/`, but you should replace that with that of your
//! > operating system.
//!
//! To install Duat, I am assuming that you have `cargo` installed on
//! your system, if you don't, [install it].
//!
//! After installing `cargo`, you will also need to install the
//! `nightly` toolchain:
//!
//! ### On unix-like operating systems
//!
//! ```bash
//! rustup install nightly
//! ```
//!
//! ### On Windows
//!
//! ```bash
//! rustup install nightly-x86_64-pc-windows-gnu
//! ```
//!
//! To install Duat, another dependency that you will need is `gcc`.
//! On unix-like operating systems, that should already be installed.
//! But on Windows (of course) you need to (very) manually install it.
//! You can follow the instructions on [this guide] in order to do
//! that. You can skip the prerequesits section, it's vscode specific.
//!
//! Next, in order to run duat, you should add `~/.cargo/bin/` to your
//! `$PATH` variable. Alternatively, you can just add
//! `~/.cargo/bin/duat`, if you want to add just `duat` to the
//! `$PATH`. Now, you can install duat:
//!
//! ```bash
//! cargo install duat
//! ```
//!
//! Although, since this is an alpha, I would recommend the git
//! version, since that is kept much  more up to date:
//!
//! ```bash
//! cargo install --git https://github.com/AhoyISki/duat --force --features git-deps
//! ```
//!
//! And if you want to nuke your config in order to get the newest
//! default config crate, you can do the following:
//!
//! ```bash
//! rm -rf ~/.config/duat
//! cargo install --git https://github.com/AhoyISki/duat --force --features git-deps
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
//! #     impl Kak {
//! #         pub fn new() -> Self { Self }
//! #     }
//! #     impl duat_core::Plugin<Ui> for Kak {
//! #         fn plug(self, _: &duat_core::Plugins<Ui>) {}
//! #     }
//! # }
//! setup_duat!(setup);
//! use duat::prelude::*;
//!
//! fn setup() {
//!     plug(kak::Kak::new());
//!     map::<kak::Insert>("jk", "<Esc>");
//!
//!     print::wrap_on_edge();
//!
//!     hook::add::<LineNumbers<Ui>>(|pa, (cfg, _)| cfg.align_right());
//!
//!     hook::add::<StatusLine<Ui>>(|pa, (cfg, _)| {
//!         let upper_mode = mode_name(pa).map(pa, |m| match m.split_once('<') {
//!             Some((no_generics, _)) => no_generics.to_uppercase(),
//!             None => m.to_uppercase(),
//!         });
//!         cfg.fmt(status!(
//!             "[Mode]{upper_mode}{Spacer}{name_txt} {sels_txt} {main_txt}"
//!         ))
//!     });
//!
//!     hook::add::<ModeSwitched>(|_, (_, new)| match new {
//!         "Insert" => cursor::set_main(CursorShape::SteadyBar),
//!         _ => cursor::unset(),
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
//! - Changes the alignment of the [`LineNumbers`] [`Widget`];
//! - [Removes] the hook group "FooterWidgets";
//! - Changes the [status line] (with a [Spacer] for 2 separate sides,
//!   and a reformatted [`mode_name`]);
//! - [Adds] hooks for [mode changes] in Duat, which change the shape
//!   of the cursor;
//! - [Changes](form::set) the [style] of the mode printed on the
//!   status line;
//!
//! These are only some of the options available to configure Duat,
//! you can also add [custom commands], place widgets around other
//! [`Widget`](crate::hook::WidgetCreated)s and [windows], create
//! [`Parser`]s that can track every change on a [`File`], and many
//! other things.
//!
//! Duat also comes with a fully fledged [text creation system], which
//! significantly eases the creation of widgets:
//!
//! ```rust
//! # use duat::prelude::*;
//! # fn test() {
//! let text = txt!("[my_form]Waow it's my form![]not anymore 😢").build();
//! # }
//! ```
//!
//! In this example, I'm using the "my_form" form in order to style
//! the text, while `[]` reverts back to the "default" form. Double
//! `[[` and `]]` escape the `[` and `]`, just like `{{` and `}}` in
//! [`println!`]. The [`status!`] macro works similarly.
//!
//! # Troubleshooting
//!
//! These issues asume that you are working with the `--git-deps`
//! version of `duat`
//!
//! ## The configuration fails to compile/recompile
//!
//! Try running this in `~/.config/duat`:
//!
//! ```bash
//! cargo clean && cargo update && cargo build --release
//! ```
//!
//! This could solve inconsistencies in the API, given that it could
//! change without the plugins being aware of those changes.
//!
//! ## It still fails to compile!
//!
//! In this case, you should open an issue with the error message that
//! `cargo build --release` sent you.
//!
//! ## It's still segfaulting as I reopen!
//!
//! This is an indication that your installed version of duat became
//! incompatible with that of your config. Rerun the installation
//! process, no need to remove `~/.config/duat`.
//!
//! ## It's still segfaulting!
//!
//! In that case open an issue
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
//! - Completely custom modes, with full Vim style remapping;
//! - Completely custom widgets, with user created modes;
//! - Arbitrary concealment of text, and arbitrary ghost text;
//! - Custom hooks, whose activation is up to the creator;
//! - Custom commands, with customizable parameters supported by;
//!   Rust's robust type system;
//! - Multi UI adaptability, although for now, only a terminal UI has
//!   been made;
//! - And many others still being planned;
//!
//! Additionally, by choosing Rust as its configuration language, Duat
//! also gains the following features:
//!
//! - Complete type safety;
//! - A very functional programming language, with lots of native
//!   features;
//! - Cargo is the plugin manager;
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
//! [install it]: https://www.rust-lang.org/tools/install
//! [plugs]: prelude::plug
//! [default mode]: prelude::mode::set_default
//! [Maps]: prelude::map
//! [Changes]: prelude::print::wrap_on_edge
//! [Removes]: prelude::hook::remove
//! [group]: prelude::hook::add_grouped
//! [vertical rule]: prelude::VertRule
//! [line numbers]: prelude::LineNumbers
//! [status line]: prelude::status
//! [Spacer]: prelude::Spacer
//! [`mode_name`]: prelude::mode_name
//! [command line]: prelude::PromptLine
//! [notifications widget]: prelude::Notifications
//! [widget combo]: prelude::FooterWidgets
//! [Adds]: prelude::hook::add
//! [mode changes]: prelude::hook::ModeSwitched
//! [style]: prelude::form::Form
//! [text creation system]: prelude::text::txt
//! [`status!`]: prelude::status
//! [numbering]: duat_utils::widgets::LineNumbersCfg::rel_abs
//! [`LineNumbers`]: prelude::LineNumbers
//! [`Widget`]: prelude::Widget
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
//! [custom commands]: crate::prelude::cmd
//! [windows]: crate::hook::WindowCreated
//! [`Parser`]: duat_core::file::Parser
//! [`File`]: crate::prelude::File
//! [this guide]: https://code.visualstudio.com/docs/cpp/config-mingw
#![feature(decl_macro, thread_spawn_hook)]

use std::sync::RwLock;

use duat_core::session::SessionCfg;
pub use duat_core::utils::{crate_dir, duat_name, src_crate};

pub use self::setup::{Channels, Initials, MetaStatics, pre_setup, run_duat};

pub mod print;
mod setup;

pub mod cmd {
    //! Command creation and calling utilities
    pub use duat_core::cmd::*;

    /// Command [`Parameter`]: [`Handle`]s for a given type of
    /// [`Widget`]
    ///
    /// This [`Parameter`] lets you act upon [`Handle`]s of a type of
    /// [`Widget`] with the following methods:
    ///
    /// - [`on_current`]: Acts on the current, most relevant instance.
    /// - [`on_each`]: Acts on every instance, on every window.
    /// - [`on_window`]: Acts on all instances on the current window.
    /// - [`on_flags`]: Acts based on [`Flags`] passed, `"global"` for
    ///   [`on_each`], `"window"` for [`on_window`].
    ///
    /// [`on_current`]: Handles::on_current
    /// [`on_each`]: Handles::on_each
    /// [`on_window`]: Handles::on_window
    /// [`on_flags`]: Handles::on_flags
    /// [`Widget`]: crate::prelude::Widget
    /// [`Handle`]: crate::prelude::Handle
    pub type Handles<'a, W> = duat_core::cmd::Handles<'a, W, crate::Ui>;
}

pub mod cursor {
    //! Functions to alter the [`Cursors`] of Duat
    //!
    //! [`Cursors`]: duat_core::mode::Cursors
    pub use duat_core::form::{
        extra_cursor as get_extra, main_cursor as get_main, set_extra_cursor as set_extra,
        set_main_cursor as set_main, unset_cursors as unset, unset_extra_cursor as unset_extra,
        unset_main_cursor as unset_main,
    };
}

pub mod form {
    //! Functions to alter the [`Form`]s of Duat
    //!
    //! [`Form`]: crate::form::Form
    pub use duat_core::form::{
        Color, ColorScheme, CursorShape, Form, Palette, add_colorscheme, enable_mask, from_id,
        id_of, set, set_colorscheme, set_many,
    };
}

pub mod hook {
    //! Utilities for hooks in Duat
    //!
    //! In Duat, hooks are handled through the [`Hookable`] trait.
    //! This trait contains the [`Hookable::Input`] associated
    //! type, which is what should be passed to hooks on the
    //! specific [`Hookable`]. By implementing this trait, you
    //! allow an end user to hook executions whenever said
    //! [`Hookable`] is triggered:
    //!
    //! ```rust
    //! setup_duat!(setup);
    //! use duat::prelude::*;
    //!
    //! fn setup() {
    //!     hook::add::<LineNumbers<Ui>>(|pa, (line_nums, _)| {
    //!         line_nums.align_right().align_main_left().rel_abs()
    //!     });
    //! }
    //! ```
    //!
    //! The [hook above] makes it so that, whenever [`LineNumbers`]
    //! are created, they main number is absolute and shifts left,
    //! while other numbers are relative and shift right. This
    //! hook gives you the power to configure any [`Widget`] that
    //! gets created, but what if you want to add more widgets?
    //!
    //! ```rust
    //! setup_duat!(setup);
    //! use duat::prelude::*;
    //!
    //! fn setup() {
    //!     hook::add::<File>(|_, (cfg, builder)| {
    //!         builder.push(status!("{name_txt} {main_txt}").above());
    //!         cfg
    //!     });
    //! }
    //! ```
    //!
    //! [That hook] lets you push more [`Widget`]s to a [`File`],
    //! whenever one is opened. In this case, I'm pushing a
    //! [`StatusLine`] on top of the [`File`], which displays the
    //! [file's name], as well as its [main `Selection`].
    //!
    //! Do note that this won't be the only [`Widget`] that will be
    //! pushed around the [`File`], since there are some predefined
    //! [hook groups] in Duat.
    //!
    //! From the `cfg` argument, you can also change settings on that
    //! [`File`], in a similar vein to the [`print`](crate::print)
    //! module:
    //!
    //! ```rust
    //! setup_duat!(setup);
    //! use duat::prelude::*;
    //!
    //! fn setup() {
    //!     hook::add::<File>(|_, (mut cfg, _)| {
    //!         if let Some("yaml" | "json") = cfg.filetype() {
    //!             cfg.tabstop(2)
    //!         } else {
    //!             cfg
    //!         }
    //!     });
    //! }
    //! ```
    //!
    //! The hook above will change the [tabstop] value to `2` on
    //! `"yaml"` and `"json"` files.
    //!
    //! # Default hook groups
    //!
    //! Hook groups are essentially "removable hooks", that are
    //! predefined in order to give a more complete, yet customizable
    //! default experience for Duat:
    //!
    //! ```rust
    //! setup_duat!(setup);
    //! use std::sync::atomic::{AtomicUsize, Ordering};
    //!
    //! use duat::prelude::*;
    //!
    //! fn setup() {
    //!     static KEY_COUNT: AtomicUsize = AtomicUsize::new(0);
    //!
    //!     hook::add_grouped::<KeysSent>("CountKeys", |pa, keys| {
    //!         KEY_COUNT.fetch_add(keys.len(), Ordering::Relaxed);
    //!     });
    //! }
    //! ```
    //!
    //! These are the default [hook groups]:
    //!
    //! - `"FileWidgets"`: Pushes a [`VertRule`] and [`LineNumbers`]
    //!   to new [`File`]s, via [`WidgetCreated`], (using [`File`] as
    //!   an alias for [`WidgetCreated<File>`]).
    //! - `"FooterWidgets"`: Pushes a  [`StatusLine`], [`PromptLine`]
    //!   and [`Notifications`] to new windows, via [`WindowCreated`].
    //! - `"HidePromptLine"`: Is responsible for [hiding] the
    //!   [`PromptLine`] when it is not in use, giving way to the
    //!   [`Notifications`], via [`FocusedOn`] and [`UnfocusedFrom`].
    //! - `"ReloadOnWrite"`: Reloads the `config` crate whenever any
    //!   file in it is written to, via [`FileWritten`].
    //!
    //! # Available hooks
    //!
    //! Currently, these are the existing hooks in `duat-core` and
    //! `duat-utils`:
    //!
    //! - [`ConfigLoaded`] triggers after loading the config crate.
    //! - [`ConfigUnloaded`] triggers after unloading the config
    //!   crate.
    //! - [`ExitedDuat`] triggers after Duat has exited.
    //! - [`WidgetCreated`] triggers when a [`Widget`]'s [cfg] is
    //!   created, letting you change it, the [`Widget`] can be used
    //!   as its [alias]
    //! - [`WindowCreated`], which lets you push widgets around the
    //!   window.
    //! - [`FocusedOn`] lets you act on a [`Widget`] when focused.
    //! - [`UnfocusedFrom`] lets you act on a [`Widget`] when
    //!   unfocused.
    //! - [`KeysSent`] lets you act on a [`dyn Widget`], given a[key].
    //! - [`KeysSentTo`] lets you act on a given [`Widget`], given a
    //!   [key].
    //! - [`FormSet`] triggers whenever a [`Form`] is added/altered.
    //! - [`ModeSwitched`] triggers when you change [`Mode`].
    //! - [`ModeCreated`] lets you act on a [`Mode`] after switching.
    //! - [`FileWritten`] triggers after the [`File`] is written.
    //! - [`SearchPerformed`] (from duat-utils) triggers after a
    //!   search is performed.
    //! - [`SearchUpdated`] (from duat-utils) triggers after a search
    //!   updates.
    //!
    //! [alias]: duat_core::hook::HookAlias
    //! [hook above]: WidgetCreated
    //! [That hook]: crate::prelude::WidgetCreated
    //! [`StatusLine`]: crate::prelude::StatusLine
    //! [file's name]: crate::prelude::name_txt
    //! [main `Selection`]: crate::prelude::main_txt
    //! [hook groups]: crate::hook::add_grouped
    //! [`VertRule`]: crate::prelude::VertRule
    //! [`PromptLine`]: crate::prelude::PromptLine
    //! [`Notifications`]: crate::prelude::Notifications
    //! [`WindowCreated`]: crate::prelude::WindowCreated
    //! [hiding]: duat_core::ui::Area::constrain_ver
    //! [cfg]: crate::prelude::Widget::Cfg
    //! [`File`]: crate::prelude::File
    //! [`LineNumbers`]: crate::prelude::LineNumbers
    //! [`dyn Widget`]: crate::prelude::Widget
    //! [`Widget`]: crate::prelude::Widget
    //! [`Form`]: crate::prelude::Form
    //! [key]: crate::mode::KeyEvent
    //! [deadlocks]: https://en.wikipedia.org/wiki/Deadlock_(computer_science)
    //! [`Mode`]: crate::mode::Mode
    //! [`&mut Widget`]: crate::prelude::Widget
    //! [`Output`]: Hookable::Output
    //! [tabstop]: duat_core::cfg::PrintCfg::set_tabstop
    use duat_core::data::Pass;
    pub use duat_core::hook::*;
    pub use duat_utils::hooks::*;

    use crate::Ui;

    /// Adds a [hook]
    ///
    /// This hook is ungrouped, that is, it cannot be removed. If you
    /// want a hook that is removable, see [`hook::add_grouped`].
    ///
    /// ```rust
    /// # mod kak {
    /// #     use duat::{prelude::{*, mode::KeyEvent}};
    /// #     #[derive(Clone)]
    /// #     pub struct Normal;
    /// #     impl Mode<Ui> for Normal {
    /// #         type Widget = File;
    /// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<File>) {
    /// #             todo!();
    /// #         }
    /// #     }
    /// #     impl Normal {
    /// #         pub fn f_and_t_set_search(self) -> Self { todo!() }
    /// #     }
    /// #     pub struct Kak;
    /// #     impl Kak {
    /// #         pub fn new() -> Self { Self }
    /// #     }
    /// #     impl duat_core::Plugin<Ui> for Kak {
    /// #         fn plug(self, _: &duat_core::Plugins<Ui>) {}
    /// #     }
    /// # }
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    /// use kak::*;
    ///
    /// pub fn setup() {
    ///     hook::add::<Normal>(|pa, (normal_mode, handle)| normal_mode.f_and_t_set_search());
    /// }
    /// ```
    ///
    /// [hook]: Hookable
    /// [`hook::add_grouped`]: add_grouped
    pub fn add<H: HookAlias<Ui, impl HookDummy>>(
        f: impl FnMut(&mut Pass, H::Input<'_>) -> H::Output + Send + 'static,
    ) {
        duat_core::hook::add::<H, Ui>(f);
    }

    /// Adds a grouped [hook]
    ///
    /// A grouped hook is one that, along with others on the same
    /// group, can be removed by [`hook::remove`]. If you do
    /// not need/want this feature, take a look at [`hook::add`].
    ///
    /// [hook]: Hookable
    /// [`hook::remove`]: remove
    /// [`hook::add`]: add
    pub fn add_grouped<H: HookAlias<Ui, impl HookDummy>>(
        group: &'static str,
        f: impl FnMut(&mut Pass, H::Input<'_>) -> H::Output + Send + 'static,
    ) {
        duat_core::hook::add_grouped::<H, Ui>(group, f);
    }

    /// [`Hookable`]: Triggers when a [`Widget`]'s [cfg] is created
    ///
    /// # Arguments
    ///
    /// - The [`WidgetCfg`] in question.
    /// - A [`UiBuilder`], which lets you push [`Widget`]s around this
    ///   one.
    ///
    /// # Aliases
    ///
    /// Since every [`Widget`] implements the [`HookAlias`] trait,
    /// instead of writing this in the config crate:
    ///
    /// ```rust
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     hook::add::<WidgetCreated<LineNumbers<Ui>>>(|pa, (ln, _)| ln.rel_abs());
    /// }
    /// ```
    ///
    /// You can just write this:
    ///
    /// ```rust
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     hook::add::<LineNumbers<Ui>>(|_, (ln, _)| ln.rel_abs());
    /// }
    /// ```
    ///
    /// # Changing the layout
    ///
    /// Assuming you are using `duat-term`, you could make it so every
    /// [`LineNumbers`] comes with a [`VertRule`] on the right, like
    /// this:
    ///
    /// ```rust
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     hook::add::<LineNumbers<Ui>>(|_, (ln, builder)| {
    ///         builder.push(VertRule::cfg().on_the_right());
    ///         ln
    ///     });
    /// }
    /// ```
    ///
    /// Now, every time a [`LineNumbers`]s [`Widget`] is inserted in
    /// Duat, a [`VertRule`] will be pushed on the right of it.
    /// You could even further add a [hook] on [`VertRule`], that
    /// would push further [`Widget`]s if you wanted to.
    ///
    /// By default, this is [hook]
    ///
    /// [cfg]: crate::widgets::Widget::Cfg
    /// [`WidgetCfg`]: crate::widgets::WidgetCfg
    /// [hook]: self
    /// [direction]: duat_core::ui::PushSpecs
    /// [`LineNumbers`]: duat_utils::widgets::LineNumbers
    /// [`VertRule`]: duat_term::VertRule
    /// [`Widget`]: duat_core::ui::Widget
    /// [`UiBuilder`]: duat_core::ui::UiBuilder
    pub type WidgetCreated<W> = duat_core::hook::WidgetCreated<W, Ui>;

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
    /// of `"FileWidgets"` and `"FooterWidgets"`, preset by Duat.
    ///
    /// [`File`]: crate::prelude::File
    /// [`Widget`]: crate::prelude::Widget
    /// [builder]: crate::prelude::ui::UiBuilder
    /// [hook]: self
    pub type WindowCreated = duat_core::hook::WindowCreated<Ui>;

    /// [`Hookable`]: Triggers before closing a [`File`]
    ///
    /// # Arguments
    ///
    /// - The [`File`]'s [`Handle`].
    /// - A [`Cache`]. This can be used in order to decide wether or
    ///   not some things will be reloaded on the next opening of
    ///   Duat.
    ///
    /// This will not trigger upon reloading Duat. For that, see
    /// [`FileClosed`].
    ///
    /// [`File`]: crate::prelude::File
    /// [`Handle`]: crate::prelude::Handle
    /// [`Cache`]: duat_core::context::Cache
    pub type FileClosed = duat_core::hook::FileClosed<Ui>;

    /// [`Hookable`]: Triggers before reloading a [`File`]
    ///
    /// # Arguments
    ///
    /// - The [`File`]'s [`Handle`].
    /// - A [`Cache`]. This can be used in order to decide wether or
    ///   not some things will be reloaded on the next opening of
    ///   Duat.
    ///
    /// This will not trigger upon closing Duat. For that, see
    /// [`FileClosed`].
    ///
    /// [`File`]: crate::prelude::File
    /// [`Handle`]: crate::prelude::Handle
    /// [`Cache`]: duat_core::context::Cache
    pub type FileReloaded = duat_core::hook::FileReloaded<Ui>;

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
    pub type ModeCreated<M> = duat_core::hook::ModeCreated<M, Ui>;
}

/// Commands for the manipulation of [`Mode`]s
///
/// [`Mode`]: crate::mode::Mode
pub mod mode {
    pub use duat_core::mode::*;
    use duat_core::{mode, ui::Widget};
    pub use duat_utils::modes::*;

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

    /// Resets the mode to the [default] of a given [`Widget`]
    ///
    /// Does nothing if no default was set for the given [`Widget`].
    ///
    /// [default]: set_default
    pub fn reset<W: Widget<Ui>>() {
        mode::reset::<W, Ui>();
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

/// Common [`StatusLine`] fields
///
/// [`StatusLine`]: duat_utils::widgets::StatusLine
pub mod state {
    pub use duat_utils::state::*;
}

/// Duat's builtin widgets
pub mod widgets {
    pub use duat_core::ui::{Widget, WidgetCfg};
    pub use duat_utils::widgets::*;

    /// The widget that is used to print and edit files
    pub type File = duat_core::file::File<super::Ui>;
}

#[allow(unused_imports)]
use duat_core::{session::FileParts, ui::DuatEvent};

/// Pre and post setup for Duat
///
/// This macro *MUST* be used in order for the program to run,
/// it will generate the function that actually runs Duat.
pub macro setup_duat($setup:expr) {
    use std::sync::{Mutex, mpsc};

    use $crate::prelude::{File, Text, context::Logs, form::Palette};

    #[unsafe(no_mangle)]
    fn run(
        initials: Initials,
        ms: MetaStatics,
        files: Vec<Vec<FileParts>>,
        (duat_tx, duat_rx, reload_tx): Channels,
    ) -> (Vec<Vec<FileParts>>, mpsc::Receiver<DuatEvent>) {
        pre_setup(Some(initials), Some(duat_tx));
        $setup();
        run_duat(ms, files, duat_rx, Some(reload_tx))
    }
}

/// The prelude of Duat
pub mod prelude {
    use std::{any::TypeId, process::Output};

    pub use duat_core::{clipboard, context,
        data::{self, Pass},
        file,
        prelude::Lender,
        text::{
            self, AlignCenter, AlignLeft, AlignRight, Builder, Conceal, Ghost, Spacer, Tagger,
            Text, txt,
        },
        ui::{self, Area as AreaTrait, Widget},
    };
    use duat_core::{Plugin, Plugins};
    pub use duat_filetype::*;
    #[cfg(feature = "term-ui")]
    pub use duat_term::{self as term, VertRule};

    use crate::setup::ALREADY_PLUGGED;
    pub use crate::{
        Area, Ui, cmd, cursor,
        form::{self, CursorShape, Form},
        hook::{
            self, ColorSchemeSet, ConfigLoaded, ConfigUnloaded, ExitedDuat, FileWritten, FocusedOn,
            FocusedOnDuat, FormSet, KeysSent, KeysSentTo, ModeCreated, ModeSwitched,
            SearchPerformed, SearchUpdated, UnfocusedFrom, UnfocusedFromDuat, WidgetCreated,
            WindowCreated,
        },
        mode::{self, Mode, Pager, Prompt, User, alias, map},
        print, setup_duat,
        state::*,
        widgets::*,
    };

    /// Adds a plugin to Duat
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
    /// }
    /// ```
    ///
    /// As you can see above, they should also have a `plug` method,
    /// which consumes the plugin.
    pub fn plug<P: Plugin<Ui>>(plugin: P) {
        let mut already_plugged = ALREADY_PLUGGED.lock().unwrap();
        if already_plugged.contains(&TypeId::of::<P>()) {
            context::warn!(
                "Plugin {} was added multiple times",
                duat_core::utils::duat_name::<P>()
            );
        } else {
            already_plugged.push(TypeId::of::<P>());
            plugin.plug(&Plugins::_new());
        }
    }

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

    /// A handle to a [`Widget`] in Duat
    ///
    /// The [`Handle`] lets you do all sorts of edits on a [`Widget`].
    /// You can, for example, make use of the [`Selection`]s in
    /// its [`Text`] in order to edit the [`Text`] in a very
    /// declarative way.
    ///
    /// ```rust
    /// setup_duat!(setup);
    /// use duat::prelude::*;
    ///
    /// fn setup() {
    ///     hook::add::<UnfocusedFrom<File>>(|pa, (unfocused_handle, _)| {
    ///         unfocused_handle.edit_all(pa, |mut cursor| {
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
/// The [`ui::Area`] of the [`Ui`]
///
/// [`ui::Area`]: duat_core::ui::Area
pub type Area = <duat_term::Ui as duat_core::ui::Ui>::Area;

/// A function that sets the [`SessionCfg`].
type CfgFn = RwLock<Option<Box<dyn FnOnce(&mut SessionCfg<Ui>) + Send + Sync>>>;

/// For testing mdBook examples.
#[cfg(doctest)]
#[doc(hidden)]
#[path = "../book/book_examples.rs"]
mod book;

#[cfg(doctest)]
#[doc(hidden)]
/// ```rust
/// # mod catppuccin {
/// #     use duat::prelude::*;
/// #     pub struct Catppuccin;
/// #     impl Catppuccin {
/// #         pub fn new() -> Self { Self }
/// #     }
/// #     impl duat_core::Plugin<duat::Ui> for Catppuccin {
/// #         fn plug(self, _: &duat_core::Plugins<duat::Ui>) {}
/// #     }
/// # }
/// # mod kak {
/// #     use duat::{prelude::{*, mode::KeyEvent}};
/// #     #[derive(Clone)]
/// #     pub struct Normal;
/// #     impl Mode<Ui> for Normal {
/// #         type Widget = File;
/// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<File>) {
/// #             todo!();
/// #         }
/// #     }
/// #     #[derive(Clone)]
/// #     pub struct Insert;
/// #     impl Mode<Ui> for Insert {
/// #         type Widget = File;
/// #         fn send_key(&mut self, _: &mut Pass, _: KeyEvent, _: Handle<File>) {
/// #             todo!();
/// #         }
/// #     }
/// #     pub struct Kak;
/// #     impl Kak {
/// #         pub fn new() -> Self { Self }
/// #     }
/// #     impl duat_core::Plugin<Ui> for Kak {
/// #         fn plug(self, _: &duat_core::Plugins<Ui>) {}
/// #     }
/// # }
#[doc = include_str!("../templates/config/lib.rs")]
/// ```
mod config {}
