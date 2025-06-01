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
//! ```rust
//! # use duat_core::{
//! #     mode::{self, Cursors, EditHelper, KeyCode, KeyEvent, Mode, key},
//! #     Lender, ui::Ui, widgets::File,
//! # };
//! #[derive(Default, Clone)]
//! struct FindSeq(Option<char>);
//!
//! impl<U: Ui> Mode<U> for FindSeq {
//!     type Widget = File;
//!
//!     fn send_key(&mut self, key: KeyEvent, file: &mut File, area: &U::Area) {
//!         use KeyCode::*;
//!         let mut helper = EditHelper::new(file, area);
//!
//!         // Make sure that the typed key is a character.
//!         let key!(Char(c)) = key else {
//!             mode::reset();
//!             return;
//!         };
//!         // Checking if a character was already sent.
//!         let Some(first) = self.0 else {
//!             self.0 = Some(c);
//!             return;
//!         };
//!
//!         helper.edit_iter().for_each(|mut e| {
//!             let pat: String = [first, c].iter().collect();
//!             let matched = e.search_fwd(pat, None).next();
//!             if let Some([p0, p1]) = matched {
//!                 e.move_to(p0);
//!                 e.set_anchor();
//!                 e.move_to(p1);
//!                 e.move_hor(-1);
//!             }
//!         });
//!
//!         mode::reset();
//!     }
//! }
//! ```
//!
//! In this example, I have created a [`Mode`] for [`File`]s. This
//! mode is (I think) popular within Vim circles. It's like the `f`
//! key in Vim, but it lets you look for a sequence of 2 characters,
//! instead of just one.
//!
//! What's great about it is that it will work no matter what editing
//! model the user is using. It could be Vim inspired, Kakoune
//! inspired, Emacs inspired, doesn't matter. All the user has to do
//! to use this mode is this:
//!
//! ```rust
//! # struct Normal;
//! # #[derive(Default, Clone)]
//! # struct FindSeq;
//! # fn map<M>(take: &str, give: &impl std::any::Any) {}
//! # // I fake it here because this function is from duat, not duat-core
//! map::<Normal>("<C-s>", &FindSeq::default());
//! ```
//!
//! And now, whenever the usert types `Control S` in `Normal` mode,
//! the mode will switch to `FindSeq`. You could replace `Normal` with
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
//! # use duat_core::{
//! #     mode::{self, Cursors, EditHelper, KeyCode, KeyEvent, Mode, key},
//! #     Lender, text::{Key, Point, Tag, text}, ui::{Area, Ui}, widgets::File,
//! # };
//! #[derive(Clone)]
//! pub struct EasyMotion {
//!     is_line: bool,
//!     key: Key,
//!     points: Vec<[Point; 2]>,
//!     seq: String,
//! }
//!
//! impl EasyMotion {
//!     pub fn word() -> Self {
//!         Self {
//!             is_line: false,
//!             key: Key::new(),
//!             points: Vec::new(),
//!             seq: String::new(),
//!         }
//!     }
//!
//!     pub fn line() -> Self {
//!         Self {
//!             is_line: true,
//!             key: Key::new(),
//!             points: Vec::new(),
//!             seq: String::new(),
//!         }
//!     }
//! }
//!
//! impl<U: Ui> Mode<U> for EasyMotion {
//!     type Widget = File;
//!
//!     fn on_switch(&mut self, file: &mut File, area: &<U as Ui>::Area) {
//!         let cfg = file.print_cfg();
//!         let text = file.text_mut();
//!
//!         let regex = match self.is_line {
//!             true => "[^\n\\s][^\n]+",
//!             false => "[^\n\\s]+",
//!         };
//!         let (start, _) = area.first_points(text, cfg);
//!         let (end, _) = area.last_points(text, cfg);
//!         self.points = text.search_fwd(regex, (start, end)).unwrap().collect();
//!
//!         let seqs = key_seqs(self.points.len());
//!
//!         for (seq, [p0, _]) in seqs.iter().zip(&self.points) {
//!             let ghost = text!([EasyMotionWord] seq);
//!
//!             text.insert_tag(self.key, Tag::Ghost(p0.byte(), ghost));
//!             let seq_end = p0.byte() + seq.chars().count() ;
//!             text.insert_tag(self.key, Tag::Conceal(p0.byte()..seq_end));
//!         }
//!     }
//!
//!     fn send_key(&mut self, key: KeyEvent, file: &mut File, area: &U::Area) {
//!         let char = match key {
//!             key!(KeyCode::Char(c)) => c,
//!             // Return a char that will never match.
//!             _ => '❌'
//!         };
//!         self.seq.push(char);
//!
//!         let mut helper = EditHelper::new(file, area);
//!         helper.cursors_mut().remove_extras();
//!
//!         let seqs = key_seqs(self.points.len());
//!         for (seq, &[p0, p1]) in seqs.iter().zip(&self.points) {
//!             if *seq == self.seq {
//!                 let mut e = helper.edit_main();
//!                 e.move_to(p0);
//!                 e.set_anchor();
//!                 e.move_to(p1);
//!                 mode::reset();
//!             } else if seq.starts_with(&self.seq) {
//!                 continue;
//!             }
//!
//!             helper.text_mut().remove_tags(p1.byte(), self.key);
//!             helper.text_mut().remove_tags(p1.byte() + seq.len(), self.key);
//!         }
//!
//!         if self.seq.chars().count() == 2 || !LETTERS.contains(char) {
//!             mode::reset();
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
//! - In the beginning of said words/lines, add a [`Tag::Ghost`];
//! - Also add a [`Tag::Conceal`];
//! - Then, just match the typed keys and [remove] tags accordingly;
//! - [Move] to the matched sequence, if it exists;
//!
//! Now, in order to use this mode, it's the exact same thing as
//! `FindSeq`:
//!
//! ```rust
//! # struct Normal;
//! #[derive(Clone)]
//! # pub struct EasyMotion;
//! # impl EasyMotion {
//! #     pub fn word() -> Self {
//! #         Self
//! #     }
//! #     pub fn line() -> Self {
//! #         Self
//! #     }
//! # }
//! # fn map<M>(take: &str, give: &impl std::any::Any) {}
//! # // I fake it here because this function is from duat, not duat-core
//! map::<Normal>("<CA-w>", &EasyMotion::word());
//! map::<Normal>("<CA-l>", &EasyMotion::line());
//! ```
//!
//! [`Mode`]: crate::mode::Mode
//! [`File`]: crate::widgets::File
//! [`map`]: https://docs.rs/duat/0.2.0/duat/prelude/fn.map.html
//! [EasyMotion]: https://github.com/easymotion/vim-easymotion
//! [ghost text]: crate::text::Tag::Ghost
//! [concealment]: crate::text::Tag::Conceal
//! [`Tag::Ghost`]: crate::text::Tag::Ghost
//! [`Tag::Conceal`]: crate::text::Tag::Conceal
//! [remove]: crate::text::Text::remove_tags
//! [Move]: crate::mode::Editor::move_to
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
    sync::{LazyLock, Once},
};

use data::Pass;
#[allow(unused_imports)]
use dirs_next::cache_dir;
pub use lender::Lender;
pub use parking_lot::{Mutex, MutexGuard, RwLock, RwLockReadGuard, RwLockWriteGuard};
use text::Text;
use ui::Window;
use widgets::{File, Node, Widget};

use self::{text::err, ui::Ui};

pub mod cache;
pub mod cfg;
pub mod cmd;
pub mod context;
pub mod data;
pub mod form;
pub mod hooks;
pub mod mode;
pub mod session;
pub mod text;
pub mod ui;
pub mod widgets;

pub mod prelude {
    //! The prelude of Duat
    pub use crate::{
        cmd, data, form,
        text::{Builder, Text, err, hint, ok, text},
        ui, widgets,
    };
}

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

pub mod clipboard {
    //! Clipboard interaction for Duat
    //!
    //! Just a regular clipboard, no image functionality.
    use std::sync::OnceLock;

    pub use arboard::Clipboard;
    use parking_lot::Mutex;

    static CLIPB: OnceLock<&'static Mutex<Clipboard>> = OnceLock::new();

    /// Gets a [`String`] from the clipboard
    ///
    /// This can fail if the clipboard does not contain UTF-8 encoded
    /// text.
    ///
    /// Or if there is no clipboard i guess
    pub fn get_text() -> Option<String> {
        CLIPB.get().unwrap().lock().get_text().ok()
    }

    /// Sets a [`String`] to the clipboard
    pub fn set_text(text: impl std::fmt::Display) {
        let clipb = CLIPB.get().unwrap();
        clipb.lock().set_text(text.to_string()).unwrap();
    }

    pub(crate) fn set_clipboard(clipb: &'static Mutex<Clipboard>) {
        CLIPB.set(clipb).map_err(|_| {}).expect("Setup ran twice");
    }
}

////////// Text Builder macros (for pub/private bending)
mod private_exports {
    pub use format_like::format_like;

    pub macro inner_text($builder:expr, $default_id:expr, $accent_id:expr, $($parts:tt)*) {{
        #[allow(unused_imports)]
        use $crate::private_exports::{format_like, parse_arg, parse_form, parse_str};

        format_like!(
            parse_str,
            [('{', parse_arg, false), ('[', parse_form, true)],
            ($builder, $default_id, $accent_id),
            $($parts)*
        );
    }}

    pub macro parse_str($builder_and_forms:expr, $str:literal) {{
        let (builder, default_id, accent_id) = $builder_and_forms;
        builder.push_str($str);
        (builder, default_id, accent_id)
    }}

    pub macro parse_arg {
        ($builder_and_forms:expr, "", $arg:expr) => {{
            let (builder, default_id, accent_id) = $builder_and_forms;
            builder.push($arg);
            (builder, default_id, accent_id)
        }},
        ($builder_and_forms:expr, $modif:literal, $arg:expr) => {{
            let (builder, default_id, accent_id) = $builder_and_forms;
            builder.push(format!(concat!("{:", $modif, "}"), $arg));
            (builder, default_id, accent_id)
        }},
    }

    pub macro parse_form {
        ($builder_and_forms:expr, "",) => {{
            let (builder, default_id, accent_id) = $builder_and_forms;
            builder.push(default_id);
            (builder, default_id, accent_id)
        }},
        ($builder_and_forms:expr, "", a) => {{
            let (builder, default_id, accent_id) = $builder_and_forms;
            builder.push(accent_id);
            (builder, default_id, accent_id)
        }},
        ($builder_and_forms:expr, "", $($form:tt)*) => {{
            let (builder, default_id, accent_id) = $builder_and_forms;
            builder.push($crate::form::id_of!(concat!($(stringify!($form)),*)));
            (builder, default_id, accent_id)
        }},
        ($builder_and_forms:expr, $modif:literal, $($form:tt)*) => {{
            compile_error!(concat!("at the moment, Forms don't support modifiers like ", $modif))
        }}
    }
}

////////// General utility functions

// /// A checker that returns `true` every `duration`
// ///
// /// This is primarily used within [`WidgetCfg::build`], where a
// /// `checker` must be returned in order to update the widget.
// ///
// /// [`WidgetCfg::build`]: crate::widgets::WidgetCfg::build
//  pub fn periodic_checker(duration: Duration) -> impl Fn() -> bool {
//     let check = Arc::new(AtomicBool::new(false));
//     crate::thread::spawn({
//         let check = check.clone();
//         move || {
//             while !crate::context::will_reload_or_quit() {
//                 std::thread::sleep(duration);
//                 check.store(true, Ordering::Release);
//             }
//         }
//     });
//
//     move || check.fetch_and(false, Ordering::Acquire)
// }

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
        let mut names = NAMES.write();

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
///
/// This is primarily used on the [`cache`] module.
pub fn src_crate<T: ?Sized + 'static>() -> &'static str {
    fn src_crate_inner(type_id: TypeId, type_name: &'static str) -> &'static str {
        static CRATES: LazyLock<RwLock<HashMap<TypeId, &'static str>>> =
            LazyLock::new(|| RwLock::new(HashMap::new()));
        let mut crates = CRATES.write();

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
    static CONFIG_DIR: LazyLock<Option<&Path>> = LazyLock::new(|| {
        dirs_next::config_dir().map(|config_dir| {
            let path: &'static str = config_dir.join("duat").to_string_lossy().to_string().leak();

            std::fs::create_dir_all(path).unwrap();
            Path::new(path)
        })
    });
    *CONFIG_DIR
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
        "index out of bounds: the len is {max}, but the index is {start}, coming from {}",
        std::panic::Location::caller()
    );
    assert!(
        end <= max,
        "index out of bounds: the len is {max}, but the index is {end}, coming from {}",
        std::panic::Location::caller()
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
        .find(|(.., node)| node.read_as(pa, |f: &File| f.name() == name) == Some(true))
        .ok_or_else(|| err!("File with name [a]{name}[] not found").build())
}

/// An entry for a widget of a specific type
#[allow(clippy::result_large_err)]
fn widget_entry<'a, W: Widget<U>, U: Ui>(
    pa: &Pass,
    windows: &'a [Window<U>],
    w: usize,
) -> Result<(usize, usize, &'a Node<U>), Text> {
    let handle = context::fixed_file::<U>(pa).unwrap();

    if let Some((widget, _)) = handle.get_related_widget::<W>(pa) {
        windows
            .iter()
            .enumerate()
            .flat_map(window_index_widget)
            .find(|(.., n)| n.ptr_eq(&widget))
    } else {
        iter_around(windows, w, 0).find(|(.., node)| node.data_is::<W>())
    }
    .ok_or(err!("No widget of type [a]{}[] found", type_name::<W>()).build())
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
#[doc(hidden)]
pub static HOOK: Once = Once::new();
#[doc(hidden)]
pub static LOG: LazyLock<Mutex<String>> = LazyLock::new(|| Mutex::new(String::new()));

/// Log information to a log file
#[doc(hidden)]
pub macro log($($text:tt)*) {{
    if let Some(cache) = cache_dir()
        && let Ok(file) = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(cache.join("duat/log"))
    {
        use std::{io::Write, time::Instant};

        let mut file = std::io::BufWriter::new(file);
        let mut text = format!($($text)*);

        if let Some(start) = $crate::DEBUG_TIME_START.get()
            && text != "" {
            if text.lines().count() > 1 {
                let chars = text
                    .char_indices()
                    .filter_map(|(pos, char)| (char == '\n').then_some(pos));
                let nl_indices: Vec<usize> = chars.collect();
                for index in nl_indices.iter().rev() {
                    text.insert_str(index + 1, "  ");
                }

                let duration = Instant::now().duration_since(*start);
                write!(file, "\nat {:.4?}:\n  {text}", duration).unwrap();
            } else {
                let duration = Instant::now().duration_since(*start);
                write!(file, "\nat {:.4?}: {text}", duration).unwrap();
            }
        } else {
            write!(file, "\n{text}").unwrap();
        }
    }
}}
