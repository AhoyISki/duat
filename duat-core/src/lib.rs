//! The core of Duat, this crate is meant to be used only for the
//! creation of plugins for Duat.
//!
//! # Quick Start
//!
//! The capabilities of `duat-core` are largely the same as the those
//! of Duat, however, the main difference is the multi [`Ui`] APIs of
//! this crate. In it, the public functions and types are defined in
//! terms of `U: Ui`,  which means that they can work on various
//! different interfaces:
//!
//! <details>
//! <summary>A `vim-sneak` inspired plugin</summary>
//! ```rust
//! // I recommend pulling the prelude in plugins.
//! use std::sync::LazyLock;
//!
//! use duat_core::{prelude::*, text::Point};
//!
//! static TAGGER: LazyLock<Tagger> = Tagger::new_static();
//! static CUR_TAGGER: LazyLock<Tagger> = Tagger::new_static();
//!
//! #[derive(Default, Clone)]
//! pub struct Sneak {
//!     step: Step,
//! }
//!
//! impl<U: Ui> Mode<U> for Sneak {
//!     type Widget = File<U>;
//!
//!     fn send_key(&mut self, pa: &mut Pass, key: mode::KeyEvent, handle: Handle<File<U>, U>) {
//!         use mode::{KeyCode::*, KeyMod as Mod};
//!
//!         let cur_id = form::id_of!("sneak.current");
//!
//!         match &mut self.step {
//!             Step::Start => {
//!                 // Make sure that the typed key is a character.
//!                 let mode::key!(Char(c0)) = key else {
//!                     mode::reset::<File<U>, U>();
//!                     return;
//!                 };
//!
//!                 let pat = format!("{c0}[A-Za-z0-9]");
//!                 if highlight_matches(pa, &pat, &handle).1.is_none() {
//!                     handle.write_text(pa, |text| text.remove_tags(*TAGGER, ..));
//!                     context::error!("No matches found for [a]{pat}");
//!                     mode::reset::<File<U>, U>();
//!                     return;
//!                 }
//!
//!                 self.step = Step::Filter(c0);
//!             }
//!             Step::Filter(c0) => {
//!                 handle.write_text(pa, |text| text.remove_tags(*TAGGER, ..));
//!
//!                 let mode::key!(Char(c1)) = key else {
//!                     mode::reset::<File<U>, U>();
//!                     return;
//!                 };
//!
//!                 let pat = format!("{c0}{c1}");
//!                 let (matches, cur) = highlight_matches(pa, &pat, &handle);
//!
//!                 let Some(cur) = cur else {
//!                     handle.write_text(pa, |text| text.remove_tags(*TAGGER, ..));
//!                     context::error!("No matches found for [a]{pat}");
//!                     mode::reset::<File<U>, U>();
//!                     return;
//!                 };
//!                 let [p0, p1] = matches[cur];
//!                 handle.write_text(pa, |text| {
//!                     text.insert_tag(*CUR_TAGGER, p0..p1, cur_id.to_tag(51))
//!                 });
//!
//!                 self.step = Step::Matched(matches, cur);
//!             }
//!             Step::Matched(matches, cur) => match (key, mode::alt_is_reverse()) {
//!                 (mode::key!(Char('n')), _) => {
//!                     let prev = *cur;
//!                     let last = matches.len() - 1;
//!                     *cur = if *cur == last { 0 } else { *cur + 1 };
//!
//!                     handle.write_text(pa, |text| {
//!                         let [p0, _] = matches[prev];
//!                         text.remove_tags(*CUR_TAGGER, p0);
//!
//!                         let [p0, p1] = matches[*cur];
//!                         text.insert_tag(*CUR_TAGGER, p0..p1, cur_id.to_tag(51));
//!                     });
//!                 }
//!                 (mode::key!(Char('N')), false) | (mode::key!(Char('n'), Mod::ALT), true) => {
//!                     let prev = *cur;
//!                     let last = matches.len() - 1;
//!                     *cur = if *cur == 0 { last } else { *cur - 1 };
//!
//!                     handle.write_text(pa, |text| {
//!                         let [p0, _] = matches[prev];
//!                         text.remove_tags(*CUR_TAGGER, p0);
//!
//!                         let [p0, p1] = matches[*cur];
//!                         text.insert_tag(*CUR_TAGGER, p0..p1, cur_id.to_tag(51));
//!                     });
//!                 }
//!                 _ => {
//!                     let [p0, p1] = matches[*cur];
//!
//!                     handle.edit_main(pa, |mut e| e.move_to(p0..p1));
//!
//!                     handle.write_text(pa, |text| text.remove_tags([*TAGGER, *CUR_TAGGER], ..));
//!                     mode::reset::<File<U>, U>();
//!                 }
//!             },
//!         }
//!     }
//! }
//!
//! fn highlight_matches<U: Ui>(
//!     pa: &mut Pass,
//!     pat: &str,
//!     handle: &Handle<File<U>, U>,
//! ) -> (Vec<[Point; 2]>, Option<usize>) {
//!     handle.write(pa, |file, area| {
//!         let (start, _) = area.start_points(file.text(), file.print_cfg());
//!         let (end, _) = area.end_points(file.text(), file.print_cfg());
//!         let caret = file.selections().get_main().unwrap().caret();
//!
//!         let (bytes, mut tags) = file.text_mut().bytes_and_tags();
//!
//!         let matches: Vec<_> = bytes.search_fwd(pat, start..end).unwrap().collect();
//!
//!         let id = form::id_of!("sneak.match");
//!
//!         let tagger = *TAGGER;
//!         let mut next = None;
//!         for (i, &[p0, p1]) in matches.iter().enumerate() {
//!             if p0 > caret && next.is_none() {
//!                 next = Some(i);
//!             }
//!             tags.insert(tagger, p0..p1, id.to_tag(50));
//!         }
//!
//!         let last = matches.len().checked_sub(1);
//!         (matches, next.or(last))
//!     })
//! }
//!
//! #[derive(Default, Clone)]
//! enum Step {
//!     #[default]
//!     Start,
//!     Filter(char),
//!     Matched(Vec<[Point; 2]>, usize),
//! }
//! ```
//! </details>
//!
//! In this example, I have created a [`Mode`] for [`File`]s. This
//! mode is based on [`vim-sneak`], which is popular (I think) within
//! Vim circles. It's like the `f` key in Vim, but it lets you look
//! for a sequence of 2 characters, instead of just one, also letting
//! you pick between matches ahead and behind on the screen.
//!
//! What's great about it is that it will work no matter what editing
//! model the user is using. It could be Vim inspired, Kakoune
//! inspired, Emacs inspired, doesn't matter. All the user has to do
//! to use this mode is this:
//!
//! ```rust
//! # struct Normal;
//! # #[derive(Default, Clone)]
//! # struct Sneak;
//! # fn map<M>(take: &str, give: impl std::any::Any) {}
//! # // I fake it here because this function is from duat, not duat-core
//! map::<Normal>("<C-s>", Sneak::default());
//! ```
//!
//! And now, whenever the usert types `Control S` in `Normal` mode,
//! the mode will switch to `Sneak`. You could replace `Normal` with
//! any other mode, from any other editing model, and this would still
//! work.
//!
//! Of course, this is most useful for plugins, for your own
//! configuration, you should probably just rely on [`map`] to
//! accomplish the same thing.
//!
//! Okay, but that was a relatively simple example, here's a more
//! advanced example, which makes use of more of Duat's features.
//!
//! This is a copy of [EasyMotion], a plugin for
//! Vim/Neovim/Kakoune/Emacs that lets you skip around the screen with
//! at most 2 keypresses.
//!
//! In order to emulate it, we use [ghost text] and [concealment]:
//!
//! ```rust
//! use duat_core::{prelude::*, text::Point};
//! #[derive(Clone)]
//! pub struct EasyMotion {
//!     is_line: bool,
//!     key: Tagger,
//!     points: Vec<[Point; 2]>,
//!     seq: String,
//! }
//!
//! impl EasyMotion {
//!     pub fn word() -> Self {
//!         Self {
//!             is_line: false,
//!             key: Tagger::new(),
//!             points: Vec::new(),
//!             seq: String::new(),
//!         }
//!     }
//!
//!     pub fn line() -> Self {
//!         Self {
//!             is_line: true,
//!             key: Tagger::new(),
//!             points: Vec::new(),
//!             seq: String::new(),
//!         }
//!     }
//! }
//!
//! impl<U: Ui> Mode<U> for EasyMotion {
//!     type Widget = File<U>;
//!
//!     fn on_switch(&mut self, pa: &mut Pass, handle: Handle<File<U>, U>) {
//!         handle.write(pa, |file, _| {
//!             let cfg = file.print_cfg();
//!             let text = file.text_mut();
//!
//!             let regex = if self.is_line {
//!                 "[^\n\\s][^\n]+"
//!             } else {
//!                 "[^\n\\s]+"
//!             };
//!
//!             let (start, _) = handle.area().start_points(text, cfg);
//!             let (end, _) = handle.area().end_points(text, cfg);
//!             self.points = text.search_fwd(regex, start..end).unwrap().collect();
//!
//!             let seqs = key_seqs(self.points.len());
//!
//!             for (seq, [p0, _]) in seqs.iter().zip(&self.points) {
//!                 let ghost = Ghost(txt!("[easy_motion.word]{seq}"));
//!                 text.insert_tag(self.key, *p0, ghost);
//!
//!                 let seq_end = p0.byte() + seq.chars().count();
//!                 text.insert_tag(self.key, p0.byte()..seq_end, Conceal);
//!             }
//!         });
//!     }
//!
//!     fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<File<U>, U>) {
//!         let char = match key {
//!             key!(KeyCode::Char(c)) => c,
//!             // Return a char that will never match.
//!             _ => '❌',
//!         };
//!         self.seq.push(char);
//!
//!         handle.write_selections(pa, |c| c.remove_extras());
//!
//!         let seqs = key_seqs(self.points.len());
//!         for (seq, &[p0, p1]) in seqs.iter().zip(&self.points) {
//!             if *seq == self.seq {
//!                 handle.edit_main(pa, |mut e| {
//!                     e.move_to(p0);
//!                     e.set_anchor();
//!                     e.move_to(p1);
//!                 });
//!                 mode::reset::<File<U>, U>();
//!             } else if seq.starts_with(&self.seq) {
//!                 continue;
//!             }
//!
//!             // Removing one end of the conceal range will remove both ends.
//!             handle.write_text(pa, |text| text.remove_tags(self.key, p1.byte()));
//!         }
//!
//!         if self.seq.chars().count() == 2 || !LETTERS.contains(char) {
//!             mode::reset::<File<U>, U>();
//!         }
//!     }
//! }
//!
//! fn key_seqs(len: usize) -> Vec<String> {
//!     let double = len / LETTERS.len();
//!
//!     let mut seqs = Vec::new();
//!     seqs.extend(LETTERS.chars().skip(double).map(char::into));
//!
//!     let chars = LETTERS.chars().take(double);
//!     seqs.extend(chars.flat_map(|c1| LETTERS.chars().map(move |c2| format!("{c1}{c2}"))));
//!
//!     seqs
//! }
//!
//! static LETTERS: &str = "abcdefghijklmnopqrstuvwxyz";
//! ```
//! All that this plugin is doing is:
//!
//! - Search on the screen for words/lines;
//! - In the beginning of said words/lines, add a [`Ghost`];
//! - Also add a [`Conceal`];
//! - Then, just match the typed keys and [remove] tags accordingly;
//! - [Move] to the matched sequence, if it exists;
//!
//! Now, in order to use this mode, it's the exact same thing as
//! `Sneak`:
//!
//! ```rust
//! # struct Normal;
//! # #[derive(Clone)]
//! # pub struct EasyMotion;
//! # impl EasyMotion {
//! #     pub fn word() -> Self {
//! #         Self
//! #     }
//! #     pub fn line() -> Self {
//! #         Self
//! #     }
//! # }
//! # fn map<M>(take: &str, give: impl std::any::Any) {}
//! # // I fake it here because this function is from duat, not duat-core
//! map::<Normal>("<CA-w>", EasyMotion::word());
//! map::<Normal>("<CA-l>", EasyMotion::line());
//! ```
//!
//! [`Mode`]: crate::mode::Mode
//! [`File`]: crate::file::File
//! [`map`]: https://docs.rs/duat/0.2.0/duat/prelude/fn.map.html
//! [EasyMotion]: https://github.com/easymotion/vim-easymotion
//! [ghost text]: crate::text::Ghost
//! [concealment]: crate::text::Conceal
//! [`Ghost`]: crate::text::Ghost
//! [`Conceal`]: crate::text::Conceal
//! [remove]: crate::text::Text::remove_tags
//! [Move]: crate::mode::Cursor::move_to
//! [`vim-sneak`]: https://github.com/justinmk/vim-sneak
#![feature(
    decl_macro,
    step_trait,
    type_alias_impl_trait,
    trait_alias,
    debug_closure_helpers,
    box_as_ptr,
    unboxed_closures,
    associated_type_defaults,
    dropck_eyepatch,
    fn_traits
)]
#![allow(clippy::single_range_in_vec_init)]

use std::{
    any::{TypeId, type_name},
    collections::HashMap,
    ops::Range,
    path::{Path, PathBuf},
    sync::{LazyLock, RwLock},
};

#[allow(unused_imports)]
use dirs_next::cache_dir;

use self::{
    data::Pass,
    file::File,
    text::Text,
    ui::{Node, Ui, Widget, Window},
};
use crate::text::txt;

pub mod cfg;
pub mod cmd;
pub mod context;
pub mod data;
pub mod file;
pub mod form;
pub mod hook;
pub mod mode;
#[doc(hidden)]
pub mod session;
pub mod text;
pub mod ui;

/// A plugin for Duat
///
/// Plugins must follow the builder pattern, and can be specific to
/// certain [`Ui`]s. Generally, plugins should do all the setup
/// necessary for their function when [`Plugin::plug`] is called.
///
/// [`Plugin`] will usually be [plugged] by a `macro` in the Duat
/// config crate. This macro requires that the [`Plugin`] be
/// compatible with the [`Ui`]. And this can cause some inconvenience
/// for the end user. For example, say we have a plugin like this:
///
/// ```rust
/// # use duat_core::{Plugin, ui::Ui};
/// struct MyPlugin;
///
/// impl<U: Ui> Plugin<U> for MyPlugin {
///     fn new() -> Self {
///         MyPlugin
///     }
///
///     fn plug(self) {
///         //..
///     }
/// }
///
/// impl MyPlugin {
///     pub fn modify(self) -> Self {
///         //..
/// #       self
///     }
/// }
/// ```
///
/// In the config crate, the user would have to add the plugin in a
/// really awkward way:
///
/// ```rust
/// # use duat_core::Plugin;
/// # macro_rules! plug {
/// #     ($($plug:expr),+) => {};
/// # }
/// # struct MyPlugin;
/// # impl<U: duat_core::ui::Ui> duat_core::Plugin<U> for MyPlugin {
/// #     fn new() -> Self {
/// #         MyPlugin
/// #     }
/// #     fn plug(self) {}
/// # }
/// # fn test<Ui: duat_core::ui::Ui>() {
/// plug!(<MyPlugin as Plugin<Ui>>::new().modify());
/// # }
/// ```
///
/// To prevent that, just add a [`Ui`] [`PhantomData`] parameter:
///
/// ```rust
/// # use std::marker::PhantomData;
/// # use duat_core::{Plugin, ui::Ui};
/// struct MyPlugin<U>(PhantomData<U>);
///
/// impl<U: Ui> Plugin<U> for MyPlugin<U> {
///     fn new() -> Self {
///         MyPlugin(PhantomData)
///     }
///
///     fn plug(self) {
///         //..
///     }
/// }
///
/// impl<U> MyPlugin<U> {
///     pub fn modify(self) -> Self {
///         //..
/// #       self
///     }
/// }
/// ```
/// And now the plugin can be plugged much more normally:
///
///
/// ```rust
/// # use std::marker::PhantomData;
/// # use duat_core::Plugin;
/// # macro_rules! plug {
/// #     ($($plug:expr),+) => {};
/// # }
/// # struct MyPlugin<U>(PhantomData<U>);
/// # impl<U: duat_core::ui::Ui> duat_core::Plugin<U> for MyPlugin<U> {
/// #     fn new() -> Self {
/// #         MyPlugin(PhantomData)
/// #     }
/// #     fn plug(self) {}
/// # }
/// # impl<U> MyPlugin<U> {
/// #     pub fn modify(self) -> Self {
/// #         self
/// #     }
/// # }
/// # fn test<Ui: duat_core::ui::Ui>() {
/// plug!(MyPlugin::new().modify());
/// # }
/// ```
/// [plugged]: Plugin::plug
/// [`PhantomData`]: std::marker::PhantomData
pub trait Plugin<U: Ui>: Sized {
    /// Returns a builder pattern instance of this [`Plugin`]
    fn new() -> Self;

    /// Sets up the [`Plugin`]
    fn plug(self);
}

pub mod prelude {
    //! The prelude of Duat
    //!
    //! I recommend adding `use duat_core::prelude::*` to the top of
    //! modules, in order to bring everything that you could possibly
    //! need into scope. This is especially the case if you're not
    //! working with an LSP which can just bring those symbols into
    //! scope.
    pub use lender::Lender;

    pub use crate::{
        Plugin,
        cfg::PrintCfg,
        cmd,
        context::{self, FileHandle, Handle},
        data::Pass,
        file::File,
        form::{self, Form},
        hook,
        mode::{self, KeyCode, KeyEvent, KeyMod, Mode, key},
        text::{AlignCenter, AlignLeft, AlignRight, Conceal, Ghost, Spacer, Tagger, Text, txt},
        ui::{PushSpecs, RawArea, Ui, Widget, WidgetCfg},
    };
}
pub use main_thread_only::MainThreadOnly;

mod main_thread_only {
    /// A container meant for access in only the main thread
    ///
    /// Use this if you want a static value that is not
    /// [`Send`]/[`Sync`].
    #[derive(Default)]
    #[doc(hidden)]
    pub struct MainThreadOnly<T>(T);

    impl<T> MainThreadOnly<T> {
        /// Returns a new [`MainThreadOnly`]
        pub const fn new(value: T) -> Self {
            Self(value)
        }

        /// Acquires the inner value.
        ///
        /// # Safety
        ///
        /// You must ensure that this operation is taking place in the
        /// main thread of execution, although this function might
        /// take a [`Pass`] parameter later on, in order to
        /// lift that requirement.
        pub unsafe fn get(&self) -> &T {
            &self.0
        }
    }

    unsafe impl<T> Send for MainThreadOnly<T> {}
    unsafe impl<T> Sync for MainThreadOnly<T> {}
}

pub mod clipboard {
    //! Clipboard interaction for Duat
    //!
    //! Just a regular clipboard, no image functionality.
    use std::sync::{Mutex, OnceLock};

    /// A clipboard for Duat, can be platform based, or local
    #[doc(hidden)]
    pub enum Clipboard {
        Platform(arboard::Clipboard),
        Local(String),
    }

    impl Default for Clipboard {
        fn default() -> Self {
            match arboard::Clipboard::new() {
                Ok(clipb) => Self::Platform(clipb),
                Err(_) => Self::Local(String::new()),
            }
        }
    }

    static CLIPB: OnceLock<&'static Mutex<Clipboard>> = OnceLock::new();

    /// Gets a [`String`] from the clipboard
    ///
    /// This can fail if the clipboard does not contain UTF-8 encoded
    /// text.
    ///
    /// Or if there is no clipboard i guess
    pub fn get_text() -> Option<String> {
        let mut clipb = CLIPB.get().unwrap().lock().unwrap();
        match &mut *clipb {
            Clipboard::Platform(clipb) => clipb.get_text().ok(),
            Clipboard::Local(clipb) => Some(clipb.clone()).filter(String::is_empty),
        }
    }

    /// Sets a [`String`] to the clipboard
    pub fn set_text(text: impl std::fmt::Display) {
        let mut clipb = CLIPB.get().unwrap().lock().unwrap();
        match &mut *clipb {
            Clipboard::Platform(clipb) => clipb.set_text(text.to_string()).unwrap(),
            Clipboard::Local(clipb) => *clipb = text.to_string(),
        }
    }

    pub(crate) fn set_clipboard(clipb: &'static Mutex<Clipboard>) {
        CLIPB.set(clipb).map_err(|_| {}).expect("Setup ran twice");
    }
}

////////// Text Builder macros (for pub/private bending)
mod private_exports {
    pub use format_like::format_like;

    pub macro log($target:expr, $lvl:expr, $($arg:tt)*) {{
        let text = $crate::text::txt!($($arg)*).build();

		$crate::context::logs().push_record($crate::context::Record::new(
    		text,
    		$lvl,
    		$target,
    		Some(module_path!()),
    		Some(file!()),
    		Some(line!())
		));
    }}

    pub macro parse_str($builder:expr, $str:literal) {{
        let builder = $builder;
        builder.push_str($str);
        builder
    }}

    pub macro parse_arg {
        ($builder:expr, "", $arg:expr) => {{
            let builder = $builder;
            builder.push($arg);
            builder
        }},
        ($builder:expr, $modif:literal, $arg:expr) => {{
            let builder = $builder;
            builder.push(format!(concat!("{:", $modif, "}"), &$arg));
            builder
        }},
    }

    pub macro parse_form {
        ($builder:expr, "",) => {{
            let builder = $builder;
            builder.push($crate::form::DEFAULT_ID);
            builder
        }},
        ($builder:expr, "", a) => {{
            let builder = $builder;
            builder.push($crate::form::ACCENT_ID);
            builder
        }},
        ($builder:expr, "", $($form:tt)*) => {{
            let builder = $builder;
            builder.push($crate::form::id_of!(concat!($(stringify!($form)),*)));
            builder
        }},
        ($builder:expr, $modif:literal, $($form:tt)*) => {{
            compile_error!(concat!("at the moment, Forms don't support modifiers like ", $modif))
        }}
    }
}

////////// General utility functions

/// Takes a type and generates an appropriate name for it
///
/// Use this function if you need a name of a type to be
/// referrable by string, such as by commands or by the
/// user.
///
/// # NOTE
///
/// Any `<Ui>` or `Ui, ` type arguments will be removed from the final
/// result, since Duat is supposed to have only one [`Ui`] in use.
pub fn duat_name<T: ?Sized + 'static>() -> &'static str {
    fn duat_name_inner(type_id: TypeId, type_name: &str) -> &'static str {
        static NAMES: LazyLock<RwLock<HashMap<TypeId, &'static str>>> =
            LazyLock::new(RwLock::default);
        let mut names = NAMES.write().unwrap();

        if let Some(name) = names.get(&type_id) {
            name
        } else {
            let mut name = String::new();

            for path in type_name.split_inclusive(['<', '>', ',', ' ']) {
                for segment in path.split("::") {
                    let is_type = segment.chars().any(|c| c.is_uppercase());
                    let is_punct = segment.chars().all(|c| !c.is_alphanumeric());
                    let is_dyn = segment.starts_with("dyn");
                    if is_type || is_punct || is_dyn {
                        name.push_str(segment);
                    }
                }
            }

            while let Some((i, len)) = None
                .or_else(|| name.find("<Ui>").map(|i| (i, "<Ui>".len())))
                .or_else(|| name.find("Ui, ").map(|i| (i, "Ui, ".len())))
                .or_else(|| name.find("::<Ui>").map(|i| (i, "::<Ui>".len())))
            {
                unsafe {
                    name.as_mut_vec().splice(i..(i + len), []);
                }
            }

            names.insert(type_id, name.leak());
            names.get(&type_id).unwrap()
        }
    }

    duat_name_inner(TypeId::of::<T>(), std::any::type_name::<T>())
}

/// Returns the source crate of a given type
pub fn src_crate<T: ?Sized + 'static>() -> &'static str {
    fn src_crate_inner(type_id: TypeId, type_name: &'static str) -> &'static str {
        static CRATES: LazyLock<RwLock<HashMap<TypeId, &'static str>>> =
            LazyLock::new(|| RwLock::new(HashMap::new()));
        let mut crates = CRATES.write().unwrap();

        if let Some(src_crate) = crates.get(&type_id) {
            src_crate
        } else {
            let src_crate = type_name.split([' ', ':']).find(|w| *w != "dyn").unwrap();

            crates.insert(type_id, src_crate);
            crates.get(&type_id).unwrap()
        }
    }

    src_crate_inner(TypeId::of::<T>(), std::any::type_name::<T>())
}

/// The path for the config crate of Duat
pub fn crate_dir() -> Option<&'static Path> {
    static CRATE_DIR: LazyLock<Option<&Path>> = LazyLock::new(|| {
        dirs_next::config_dir().map(|config_dir| {
            let path: &'static str = config_dir.join("duat").to_string_lossy().to_string().leak();

            std::fs::create_dir_all(path).unwrap();
            Path::new(path)
        })
    });
    *CRATE_DIR
}

/// The path for a plugin's auxiliary files
///
/// If you want to store something in a more permanent basis, and also
/// possibly allow for the user to modify some files (e.g. a TOML file
/// with definitions for various LSPs), you should place it in here.
///
/// This function will also create said directory, if it doesn't
/// already exist, only returning [`Some`], if it managed to verify
/// its existance.
pub fn plugin_dir(plugin: &str) -> Option<PathBuf> {
    assert_ne!(plugin, "", "Can't have an empty plugin name");

    static PLUGIN_DIR: LazyLock<Option<&Path>> = LazyLock::new(|| {
        dirs_next::data_local_dir().map(|local_dir| {
            let path: &'static str = local_dir
                .join("duat/plugins")
                .to_string_lossy()
                .to_string()
                .leak();

            Path::new(path)
        })
    });

    let plugin_dir = (*PLUGIN_DIR)?.join(plugin);
    std::fs::create_dir_all(&plugin_dir).ok()?;

    Some(plugin_dir)
}

/// Convenience function for the bounds of a range
#[track_caller]
fn get_ends(range: impl std::ops::RangeBounds<usize>, max: usize) -> (usize, usize) {
    let start = match range.start_bound() {
        std::ops::Bound::Included(start) => *start,
        std::ops::Bound::Excluded(start) => *start + 1,
        std::ops::Bound::Unbounded => 0,
    };
    let end = match range.end_bound() {
        std::ops::Bound::Included(end) => *end + 1,
        std::ops::Bound::Excluded(end) => *end,
        std::ops::Bound::Unbounded => max,
    };
    assert!(
        start <= max,
        "index out of bounds: the len is {max}, but the index is {start}",
    );
    assert!(
        end <= max,
        "index out of bounds: the len is {max}, but the index is {end}",
    );

    (start, end)
}

/// Adds two shifts together
pub fn add_shifts(lhs: [i32; 3], rhs: [i32; 3]) -> [i32; 3] {
    let b = lhs[0] + rhs[0];
    let c = lhs[1] + rhs[1];
    let l = lhs[2] + rhs[2];
    [b, c, l]
}

/// Allows binary searching with an initial guess and displaced
/// entries
///
/// This function essentially looks at a list of entries and with a
/// starting shift position, shifts them by an amount, before
/// comparing inside of the binary search.
///
/// By using this function, it is very possible to
/// It is currently used in 2 places, in the `History` of [`Text`]s,
/// and in the `Cursors` list.
fn merging_range_by_guess_and_lazy_shift<T, U: Copy + Ord + std::fmt::Debug, V: Copy>(
    (container, len): (&impl std::ops::Index<usize, Output = T>, usize),
    (guess_i, [start, end]): (usize, [U; 2]),
    (sh_from, shift, zero_shift, shift_fn): (usize, V, V, fn(U, V) -> U),
    (start_fn, end_fn): (fn(&T) -> U, fn(&T) -> U),
) -> Range<usize> {
    fn binary_search_by_key_and_index<T, K>(
        container: &(impl std::ops::Index<usize, Output = T> + ?Sized),
        len: usize,
        key: K,
        f: impl Fn(usize, &T) -> K,
    ) -> std::result::Result<usize, usize>
    where
        K: PartialEq + Eq + PartialOrd + Ord,
    {
        let mut size = len;
        let mut left = 0;
        let mut right = size;

        while left < right {
            let mid = left + size / 2;

            let k = f(mid, &container[mid]);

            match k.cmp(&key) {
                std::cmp::Ordering::Less => left = mid + 1,
                std::cmp::Ordering::Equal => return Ok(mid),
                std::cmp::Ordering::Greater => right = mid,
            }

            size = right - left;
        }

        Err(left)
    }

    let sh = |n: usize| if n >= sh_from { shift } else { zero_shift };
    let start_of = |i: usize| shift_fn(start_fn(&container[i]), sh(i));
    let end_of = |i: usize| shift_fn(end_fn(&container[i]), sh(i));
    let search = |n: usize, t: &T| shift_fn(start_fn(t), sh(n));

    let mut c_range = if let Some(prev_i) = guess_i.checked_sub(1)
        && (prev_i < len && start_of(prev_i) <= start && start <= end_of(prev_i))
    {
        prev_i..guess_i
    } else {
        match binary_search_by_key_and_index(container, len, start, search) {
            Ok(i) => i..i + 1,
            Err(i) => {
                if let Some(prev_i) = i.checked_sub(1)
                    && start <= end_of(prev_i)
                {
                    prev_i..i
                } else {
                    i..i
                }
            }
        }
    };

    // On Cursors, the Cursors can intersect, so we need to check
    while c_range.start > 0 && start <= end_of(c_range.start - 1) {
        c_range.start -= 1;
    }

    // This block determines how far ahead this cursor will merge
    if c_range.end < len && end >= start_of(c_range.end) {
        c_range.end = match binary_search_by_key_and_index(container, len, end, search) {
            Ok(i) => i + 1,
            Err(i) => i,
        }
    }

    while c_range.end + 1 < len && end >= start_of(c_range.end + 1) {
        c_range.end += 1;
    }

    c_range
}

/// An entry for a file with the given name
#[allow(clippy::result_large_err)]
fn file_entry<'a, U: Ui>(
    pa: &Pass,
    windows: &'a [Window<U>],
    name: &str,
) -> Result<(usize, usize, &'a Node<U>), Text> {
    windows
        .iter()
        .enumerate()
        .flat_map(window_index_widget)
        .find(|(.., node)| node.read_as(pa, |f: &File<U>| f.name() == name) == Some(true))
        .ok_or_else(|| txt!("File with name [a]{name}[] not found").build())
}

/// An entry for a widget of a specific type
#[allow(clippy::result_large_err)]
fn widget_entry<'a, W: Widget<U>, U: Ui>(
    pa: &Pass,
    windows: &'a [Window<U>],
    w: usize,
) -> Result<(usize, usize, &'a Node<U>), Text> {
    let handle = context::fixed_file::<U>(pa).unwrap();

    if let Some(handle) = handle.get_related_widget::<W>(pa) {
        windows
            .iter()
            .enumerate()
            .flat_map(window_index_widget)
            .find(|(.., n)| n.ptr_eq(handle.widget()))
    } else {
        iter_around(windows, w, 0).find(|(.., node)| node.data_is::<W>())
    }
    .ok_or(txt!("No widget of type [a]{}[] found", type_name::<W>()).build())
}

/// Iterator over a group of windows, that returns the window's index
fn window_index_widget<U: Ui>(
    (index, window): (usize, &Window<U>),
) -> impl ExactSizeIterator<Item = (usize, usize, &Node<U>)> + DoubleEndedIterator {
    window
        .nodes()
        .enumerate()
        .map(move |(i, entry)| (index, i, entry))
}

/// Iterates around a specific widget, going forwards
fn iter_around<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, usize, &Node<U>)> + '_ {
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

/// Iterates around a specific widget, going backwards
fn iter_around_rev<U: Ui>(
    windows: &[Window<U>],
    window: usize,
    widget: usize,
) -> impl Iterator<Item = (usize, usize, &Node<U>)> {
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

// Debugging objects.
#[doc(hidden)]
pub static DEBUG_TIME_START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();
