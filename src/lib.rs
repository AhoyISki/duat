//! Duat is a text editor with sane defaults, while still having an
//! incredible amount of modularity, to the point where you can
//! replace pretty much anything.
//!
//! It is written _and configured_ in Rust, through the use of a
//! config crate. The configuration can then be reloaded without
//! closing Duat, by being recompiled as requested.
//!
//! I know that Rust isn't _really_ a scripting language, but I've
//! worked really hard to make this API intuitive to use, whilst still
//! maintaining all the safety and expressiveness that Rust is known
//! for.
//!
//! Rust is also known for long compile times, but for Duat, I've
//! managed to reduce the vast majority of reloads to under ~1.3
//! seconds, with a large chunk taking less than 800 ms (on my
//! relatively old mid range laptop).
//!
//! Do keep in mind that this is a work in progress, so there might be
//! bugs. Any feedback on features, bugs or requests is highly
//! appreciated 🥰.
//!
//! ## Installation
//!
//! To install Duat, I am assuming that you have `cargo` installed on
//! your system, if you don't, [install it]. If you are installing it
//! on Windows, you should additionlly follow the instructions that
//! they give you for installing C/C++ libraries through Visual
//! Studio.
//!
//! # Note
//!
//! On this section, I will be referring to duat's configuration by
//! `~/.config/duat/`, but you should replace it with your operating
//! system's config path. The same also applies to `~/.local/duat/`.
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
//! That is the recommended version, however, if you wish to install
//! the _bleeding edge_ version, you can call this instead:
//!
//! ```bash
//! cargo install --git https://github.com/AhoyISki/duat --features git-deps
//! ```
//!
//! ## Configuration
//!
//! When you first run `duat`, you will be prompted for the creation
//! of a new configuration crate in `~/.config/duat/` (unless it
//! already exists).
//!
//! In the configuration's `src/lib.rs`, there should be a
//! `setup_duat!` macro, which takes in a function with no parameters.
//!
//! This function is the setup for duat, and it can be empty, which is
//! the equivalent of the default configuration for Duat.
//!
//! Here's an example configuration buffer, which makes use of the
//! `duat-kak` crate, which is a plugin for Duat. This plugin, like
//! all others, is included without the `duat_` prefix, so in the
//! config it is just `kak`.
//!
//! ```rust
//! setup_duat!(setup);
//! use duat::prelude::*;
//!
//! fn setup() {
//!     map::<Insert>("jk", "<Esc>");
//!
//!     opts::set(|opts| {
//!         opts.wrap_lines = true;
//!         opts.scrolloff.y = 5;
//!     });
//!
//!     opts::set_lines(|opts| {
//!         opts.align = std::fmt::Alignment::Right;
//!     });
//!
//!     opts::set_status(|pa| {
//!         let upper_mode = mode_name().map(|m| m.to_uppercase());
//!
//!         status!("[mode]{upper_mode}{Spacer}{name_txt} {sels_txt} {main_txt}")
//!     });
//!
//!     hook::add::<ModeSwitched>(|_, (_, new)| {
//!         match new {
//!             "Insert" => cursor::set_main(CursorShape::SteadyBar),
//!             _ => cursor::unset(),
//!         }
//!         Ok(())
//!     });
//!
//!     form::set("mode", Form::dark_magenta());
//! }
//! ```
//!
//! This configuration does the following things:
//!
//! - [Maps] jk to esc in the `Insert` mode;
//! - Sets [options] for the `Buffer`, `LineNumbers` and `StatusLine`
//! - [Adds] hooks for [mode changes] in Duat, which change the shape
//!   of the cursor;
//! - [Changes](form::set) the [style] of the mode printed on the
//!   status line;
//!
//! These are only some of the options available to configure Duat,
//! you can also add [custom commands], place widgets around other
//! [`Widget`]s and [windows], create [`Parser`]s that can track every
//! change on a [`Buffer`], and many other things.
//!
//! Duat also comes with a fully fledged [text creation system], which
//! significantly eases the creation of highly formatted text:
//!
//! ```rust
//! # use duat::prelude::*;
//! # fn test() {
//! let infix = "text";
//!
//! let text = txt!("This {infix} is [form1]colored and {Spacer} distant");
//! # }
//! ```
//!
//! In the example above, `[form1]` will change the style of the text
//! to the `"form1"` [`Form`], while `{Spacer}` will place a [spacer]
//! in between the two parts of the text (See the status line in the
//! GIF, it uses spacers).
//!
//! This macro works very similarly to the [`format!`] family of
//! macros, so you also have inlining, as you can see with the
//! `{infix}` part. All of this is, of course, checked at compile
//! time.
//!
//! # Troubleshooting
//!
//! These issues asume that you are working with the `--git-deps`
//! version of `duat`
//!
//! ## The configuration fails to compile/recompile
//!
//! Try running the following:
//!
//! ```bash
//! duat --reload --clean
//! ```
//!
//! This will update all dependencies of the config, potentially
//! solving compatibility issues. The problem may also be with some
//! plugin you installed.
//!
//! ## It's segfaulting as I reopen!
//!
//! This is an indication that your installed version of duat became
//! incompatible with that of your config. Rerun the installation
//! process and call `duat --reload`.
//!
//! # Default plugins
//!
//! When you install duat, the default config crate will come with
//! the following plugins:
//!
//! - [`duat-catppuccin`] is a just a simple colorscheme plugin, it
//!   adds the four flavors from the [catppuccin] colorscheme. You can
//!   pick between the four of them, you can apply its colors to other
//!   [`Form`]s and you can allow or disallow the colorscheme to set
//!   the background color.
//!
//! It also comes with the following built-in plugins, which I will
//! later on add the ability to disable:
//!
//! - [`duatmode`] is the default mode for editing in Duat. It is
//!   heavily inspired by the Kakoune text editor in its design, with
//!   some light differences.
//! - [`duat-treesitter`] brings [tree-sitter] to Duat in the form of
//!   syntax highlighting and indentation calculation, which can be
//!   used by Modes (such as those from `duat-kak`) in order to give
//!   better feedback when editing buffers.
//! - [`duat-match-pairs`] adds matched parentheses highlighting to
//!   duat. Has some ntegration with `duat-treesitter`.
//! - [`duat-base`] adds all of the default plugins that you see, like
//!   the line numbers, status line, prompt line, etc.
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
//! - [x] Buffer switching;
//! - [x] Create a command creation interface and a command line;
//! - [x] Add the ability to frame areas;
//! - [x] Implement concealment;
//! - [x] Implement hot reloading of configuration;
//! - [x] Create a "normal editing" mode;
//! - [x] Add the ability to create hooks;
//! - [x] Create a more generalized plugin system;
//! - [x] Implement incremental Regex searching;
//! - [x] Implement tree-sitter;
//! - [x] Add floating widgets, not tied to the session layout;
//! - [x] Implement autocompletion lists;
//! - [ ] Create an LSP plugin;
//! - [ ] Create a vim mode;
//!
//! ︙
//!
//! - [ ] Create an gui frontend;
//!
//! An internal (and more detailed) TODO list, which might hard to
//! understand, can be found in [TODO](./TODO). This list will is
//! _not_ a comprehensive roadmap, as I will ocasionally remove
//! entries from it, particularly those in the `FOR NEXT UPDATE`
//! section, when said update comes out.
//!
//! __NOTE:__ These are not set in stone, and may be done out of
//! order.
//!
//! ## Why should I use this?
//!
//! I don't know what your personal reasoning would be, but in my
//! case, I really like Kakoune's editing model, but was frustrated
//! with the lack of some features, like folding, multiple buffer
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
//! [install it]: https://www.rust-lang.org/tools/install
//! [plugs]: prelude::plug
//! [Maps]: mode::map
//! [Removes]: hook::remove
//! [line numbers]: widgets::LineNumbers
//! [status line]: widgets::status
//! [Spacer]: text::Spacer
//! [`mode_name`]: state::mode_name
//! [command line]: widgets::PromptLine
//! [notifications widget]: widgets::Notifications
//! [widget combo]: widgets::FooterWidgets
//! [Adds]: hook::add
//! [options]: opts
//! [mode changes]: hook::ModeSwitched
//! [style]: form::Form
//! [text creation system]: text::txt
//! [`status!`]: widgets::status
//! [`LineNumbers`]: widgets::LineNumbers
//! [`Widget`]: widgets::Widget
//! [tags]: duat_core::text::Tag
//! [`duat-kak`]: https://github.com/AhoyISki/duat-kak
//! [Kakoune]: https://github.com/mawww/kakoune
//! [`duat-catppuccin`]: https://github.com/AhoyISki/duat-catppuccin
//! [catppuccin]: https://catppuccin.com
//! [`Form`]: prelude::Form
//! [`duat-treesitter`]: duat_treesitter
//! [`duat-match-pairs`]: duat_match_pairs
//! [`duat-base`]: duat_base
//! [tree-sitter]: https://tree-sitter.github.io/tree-sitter
//! [`plug!`]: prelude::plug
//! [dependencies section]: https://doc.rust-lang.org/cargo/reference/specifying-dependencies.html
//! [custom commands]: crate::prelude::cmd
//! [windows]: crate::hook::WindowCreated
//! [`Parser`]: duat_core::buffer::Parser
//! [`Buffer`]: crate::prelude::Buffer
//! [this guide]: https://code.visualstudio.com/docs/cpp/config-mingw

pub use duat_core::{
    Plugin, Plugins, buffer, clipboard, cmd, context, data,
    lender::{self, DoubleEndedLender, ExactSizeLender, Lender},
    text, ui, utils,
};

pub mod opts;
mod regular;
mod setup;

pub mod cursor {
    //! Functions to alter the [`Cursors`] of Duat
    //!
    //! [`Cursors`]: duat_core::mode::Cursors
    pub use duat_core::form::{
        extra_cursor as get_extra, id_of, main_cursor as get_main, set_extra_cursor as set_extra,
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
        id_of, set, set_colorscheme, set_many, set_many_weak, set_weak,
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
    //! use std::fmt::Alignment;
    //!
    //! use duat::prelude::*;
    //!
    //! fn setup() {
    //!     hook::add::<LineNumbers>(|pa, handle| {
    //!         if let Some("markdown") = handle.buffer()?.filetype(pa) {
    //!             let ln = handle.write(pa);
    //!             ln.align = Alignment::Left;
    //!             ln.relative = false;
    //!         }
    //!         Ok(())
    //!     });
    //! }
    //! ```
    //!
    //! The hook above changes the [`LineNumbers`] only when the
    //! buffer is of type `markdown`, so the numbering becomes
    //! absolute and the line numbers are printed on the left.
    //!
    //! ```rust
    //! setup_duat!(setup);
    //! use duat::prelude::*;
    //!
    //! fn setup() {
    //!     hook::add::<Buffer>(|pa, handle| {
    //!         status!("{name_txt}{Spacer}{main_txt}")
    //!             .above()
    //!             .push_on(pa, handle);
    //!         Ok(())
    //!     });
    //! }
    //! ```
    //!
    //! On the snippet above, I'm pushing a [`StatusLine`] above each
    //! [`Buffer`] as they are opened. This `StatusLine` will print
    //! the name of the `Buffer` on the left, and the main cursor on
    //! the right.
    //!
    //! Do note that this won't be the only [`Widget`] that will be
    //! pushed around the [`Buffer`], since by default, a
    //! [`LineNumbers`] and [`VertRule`] are pushed to the left.
    //!
    //! # Available hooks
    //!
    //! Currently, these are the existing hooks in `duat-core` and
    //! `duat-base`:
    //!
    //! - [`ConfigLoaded`] triggers after loading the config.
    //! - [`ConfigUnloaded`] triggers after unloading the config.
    //! - [`ExitedDuat`] triggers after Duat has exited.
    //! - [`FocusedOnDuat`] triggers when Duat gains focus.
    //! - [`UnfocusedFromDuat`] triggers when Duat loses focus.
    //! - [`WidgetCreated`] triggers when a [`Widget`] is created.
    //! - [`WindowCreated`], triggers when a [`Window`] is created.
    //! - [`BufferSaved`] triggers after the [`Buffer`] is written.
    //! - [`BufferClosed`] triggers when closing `Buffer`s.
    //! - [`BufferReloaded`] triggers when reloading Duat.
    //! - [`FocusedOn`] triggers when a widget is focused.
    //! - [`UnfocusedFrom`] triggers when a widget is unfocused.
    //! - [`FocusChanged`] is a generic version of [`FocusedOn`].
    //! - [`KeyTyped`] triggers on every _typed_ key.
    //! - [`KeySent`] triggers when keys are sent.
    //! - [`KeySentTo`] same, but on a specific `Widget`.
    //! - [`KeyTyped`] triggers when a key is _typed_, not _sent_.
    //! - [`FormSet`] triggers whenever a [`Form`] is added/altered.
    //! - [`ModeSwitched`] triggers when you change `Mode`.
    //! - [`ModeSet`] triggers when switching `Mode`s.
    //! - [`SearchPerformed`] triggers after a search is performed.
    //! - [`SearchUpdated`] triggers after a search updates.
    //!
    //! These are just the built-in ones, you can [create your own] as
    //! well.
    //!
    //! # Hook groups
    //!
    //! Hook groups are essentially "removable hooks", that are
    //! predefined in order to give a more complete, yet customizable
    //! default experience for Duat:
    //!
    //! ```rust
    //! setup_duat!(setup);
    //! use std::sync::atomic::{AtomicUsize, Ordering};
    //!
    //! use duat::{data::RwData, prelude::*};
    //!
    //! fn setup() {
    //!     let key_count = RwData::new(0);
    //!
    //!     hook::add::<KeySent>({
    //!         let key_count = key_count.clone();
    //!         move |pa, _| Ok(*key_count.write(pa) += 1)
    //!     })
    //!     .grouped("CountKeys");
    //!
    //!     // Shows the key count on the StatusLine
    //!     opts::set_status(move |pa| {
    //!         let mode_txt = mode_txt();
    //!         let key_count = key_count.clone();
    //!         status!("{mode_txt}{Spacer}{name_txt} {sels_txt} {main_txt} keys={key_count}")
    //!     });
    //!
    //!     // Stops counting keys 🙁.
    //!     hook::remove("CountKeys");
    //! }
    //! ```
    //!
    //! These are the default hook groups:
    //!
    //! - `"BufferWidgets"`: Pushes a [`VertRule`] and [`LineNumbers`]
    //!   to new [`Buffer`]s, via [`WidgetCreated`], (using [`Buffer`]
    //!   as an alias for [`WidgetCreated<Buffer>`]).
    //! - `"LogBook"`: Pushes a [`LogBook`] to the bottom.
    //! - `"FooterWidgets"`: Pushes a  [`StatusLine`], [`PromptLine`]
    //!   and [`Notifications`] to new windows, via [`WindowCreated`].
    //! - `"ReloadOnWrite"`: Reloads the `config` crate whenever any
    //!   buffer in it is written to, via [`BufferSaved`].
    //!
    //! [alias]: duat_core::hook::HookAlias
    //! [hook above]: WidgetCreated
    //! [That hook]: crate::prelude::WidgetCreated
    //! [`StatusLine`]: crate::prelude::StatusLine
    //! [buffer's name]: crate::prelude::name_txt
    //! [main `Selection`]: crate::prelude::main_txt
    //! [`VertRule`]: crate::prelude::VertRule
    //! [`PromptLine`]: crate::prelude::PromptLine
    //! [`Notifications`]: crate::prelude::Notifications
    //! [`WindowCreated`]: crate::prelude::WindowCreated
    //! [`LogBook`]: crate::prelude::LogBook
    //! [`Buffer`]: crate::prelude::Buffer
    //! [`LineNumbers`]: crate::prelude::LineNumbers
    //! [`dyn Widget`]: crate::prelude::Widget
    //! [`Widget`]: crate::prelude::Widget
    //! [`Form`]: crate::prelude::Form
    //! [key press]: crate::mode::KeyEvent
    //! [deadlocks]: https://en.wikipedia.org/wiki/Deadlock_(computer_science)
    //! [`Mode`]: crate::mode::Mode
    //! [`&mut Widget`]: crate::prelude::Widget
    //! [create your own]: Hookable
    //! [`Window`]: crate::ui::Window
    pub use duat_base::hooks::*;
    pub use duat_core::hook::*;
}

/// Commands for the manipulation of [`Mode`]s
///
/// [`Mode`]: crate::mode::Mode
pub mod mode {
    pub use duat_base::modes::*;
    pub use duat_core::mode::*;
    pub use duatmode::{Insert, Normal, add_to_param, take_param};

    pub use crate::regular::Regular;
}

pub mod state {
    //! Common items in a [`StatusLine`]
    //!
    //! These functions are meant to be simple to use, you can just
    //! put them inside a [`status!`] macro, and they will be
    //! understood with no other meddling.
    //!
    //! Examples of functions in here are [`main_txt`], which will
    //! show a formatted version of the main [`Cursor`], and
    //! [`mode_txt`] which will show a formatted version of the
    //! current [`Mode`] of Duat.
    //!
    //! [`StatusLine`]: crate::widgets::StatusLine
    //! [`status!`]: crate::widgets::status
    //! [`Cursor`]: crate::mode::Cursor
    //! [`Mode`]: crate::mode::Mode
    pub use duat_base::state::*;
    pub use duatmode::{duat_param, duat_param_txt};
}

/// Duat's builtin widgets
pub mod widgets {
    pub use duat_base::widgets::*;
    pub use duat_core::{buffer::Buffer, ui::Widget};
}

#[doc(hidden)]
pub mod private_exports {
    //! Exports from duat, not meant for direct use.
    pub use duat_core::{context::DuatReceiver, session::ReloadedBuffer, utils::catch_panic};

    pub use crate::setup::{Channels, Initials, MetaStatics, pre_setup, run_duat};
}

/// Pre and post setup for Duat
///
/// This macro *MUST* be used in order for the program to run,
/// it will generate the function that actually runs Duat.
#[macro_export]
macro_rules! setup_duat {
    ($setup:expr) => {
        use std::sync::Mutex;

        use $crate::{
            prelude::{Buffer, Text, context::Logs, form::Palette},
            private_exports::*,
        };

        #[unsafe(no_mangle)]
        fn run(
            initials: Initials,
            ms: MetaStatics,
            buffers: Vec<Vec<ReloadedBuffer>>,
            (duat_tx, duat_rx, reload_tx): Channels,
        ) -> (Vec<Vec<ReloadedBuffer>>, DuatReceiver) {
            pre_setup(ms.0, Some(initials), Some(duat_tx));
            catch_panic($setup);
            run_duat(ms, buffers, duat_rx, Some(reload_tx))
        }
    };
}

/// The prelude of Duat
pub mod prelude {
    pub use std::ops::Range;
    use std::{any::TypeId, process::Output};

    pub use duat_filetype::*;
    #[cfg(feature = "term-ui")]
    pub use duat_term::{self as term, VertRule};

    use crate::setup::ALREADY_PLUGGED;
    pub use crate::{
        Lender, Plugin, Plugins,
        buffer::{Buffer, BufferTracker},
        clipboard, cmd,
        context::{self, Handle},
        cursor,
        data::{self, Pass},
        form::{self, CursorShape, Form},
        hook::{
            self, BufferReloaded, BufferSaved, BufferUpdated, ColorSchemeSet, ConfigLoaded,
            ConfigUnloaded, ExitedDuat, FocusChanged, FocusedOnDuat, FormSet, Hookable, KeySent,
            KeySentTo, KeyTyped, ModeSwitched, SearchPerformed, SearchUpdated, UnfocusedFrom,
            UnfocusedFromDuat, WidgetCreated, WindowCreated,
        },
        mode::{
            self, Insert, KeyCode, KeyEvent, Mode, Normal, Pager, Prompt, Selection, Selections,
            User, alias, alt, ctrl, event, map, shift,
        },
        opts::{self, ScrollOff},
        setup_duat,
        state::*,
        text::{
            self, AlignCenter, AlignLeft, AlignRight, Builder, Conceal, Ghost, Point,
            RegexHaystack, Spacer, SpawnTag, Tagger, Text, txt,
        },
        ui::{self, Area, Widget},
        widgets::*,
    };

    /// Adds a plugin to Duat
    ///
    /// These plugins should use the builder construction pattern,
    /// i.e., they should look like this:
    ///
    /// ```rust
    /// #[derive(Default)]
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
    pub fn plug<P: Plugin>(plugin: P) {
        let mut already_plugged = ALREADY_PLUGGED.lock().unwrap();
        if already_plugged.contains(&TypeId::of::<P>()) {
            context::warn!(
                "Plugin {} was added multiple times",
                duat_core::utils::duat_name::<P>()
            );
        } else {
            already_plugged.push(TypeId::of::<P>());
            plugin.plug(Plugins::_new());
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
}

// This will eventually be a NOT AND to check if any Uis have been
// chosen at all.
// Later, I'll also have an XOR checker to make sure only one Ui was
// chosen.
#[cfg(not(feature = "term-ui"))]
compile_error!("No ui has been chosen to compile Duat with.");

/// For testing mdBook examples.
#[cfg(doctest)]
#[doc(hidden)]
#[path = "../book/book_examples.rs"]
mod book;

#[cfg(doctest)]
#[doc(hidden)]
/// ```rust
/// # mod duat_catppuccin {
/// #     use duat::prelude::*;
/// #     #[derive(Default)]
/// #     pub struct Catppuccin;
/// #     impl Catppuccin {
/// #         pub fn new() -> Self { Self }
/// #     }
/// #     impl Plugin for Catppuccin {
/// #         fn plug(self, _: &duat_core::Plugins) {}
/// #     }
/// # }
#[doc = include_str!("../templates/config/lib.rs")]
/// ```
mod config {}

#[cfg(doctest)]
/// ```rust
#[doc = include_str!("../templates/plugin/lib.rs")]
/// ```
mod plugin {}
