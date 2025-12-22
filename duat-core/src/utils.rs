//! General utility functions for Duat
//!
//! This module contains a bunch of functions which are mostly useful
//! within Duat, but I have found use for them in some of my plugins,
//! so I'm making them public for other to use as well.
//!
//! Internally (and externally), these functions work with structs
//! that have a "shift" component. Specifically, these structs are an
//! ordered list with a shifting component. The shifting component
//! indicates at which point in the list there is a shift. Elements
//! before that point are considered to be correctly shifted, whilst
//! elements after that point are considered to be incorrectly shifted
//! by the exact size of the shift.
//!
//! This essentially lets me have a sorted list (for binary search and
//! fast insertion via [`GapBuffer`]s) while still letting me shift
//! elements (usually by a [`Change`] in the [`Text`]) without
//! updating every element after the [`Change`]. I just have to shift
//! elements between where the [`Change`] was inserted and where the
//! shift starts.
//!
//! This mirrors how text editing works in the real world. A user
//! doesn't just type randomly on the text buffer, the changes are
//! usually localized. Given that, this model of keeping a shifting
//! point results in a very low number of updates that need to be
//! made.
//!
//! One other (more common) way to prevent a bazillion changes to the
//! text is to divide said text in lines. So a change in some place
//! will only affect other elements on the same line. However, the
//! biggest problem with this approach is that you are purposefully
//! dividing your text in lines, which makes a lot of other things
//! more complicated than simply having a buffer of bytes. For
//! example, a change spanning multiple lines is much more complex to
//! implement in this kind of system. And this complexity trickles
//! down through your whole text editor, even down to printing text,
//! which I also find really simple with my system.
//!
//! [`GapBuffer`]: gapbuf::GapBuffer
//! [`Change`]: crate::text::Change
use std::{
    any::TypeId,
    collections::HashMap,
    ops::Range,
    path::{Path, PathBuf},
    sync::{LazyLock, OnceLock, RwLock},
};

use crate::text::{Text, txt};

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
///
/// [`Ui`]: crate::ui::Ui
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

/// The path for the crate that was loaded
static CRATE_DIR: OnceLock<Option<&Path>> = OnceLock::new();
/// The profile that was loaded
static PROFILE: OnceLock<&str> = OnceLock::new();

/// Sets the crate directory
///
/// **FOR USE BY THE DUAT EXECUTABLE ONLY**
#[doc(hidden)]
pub fn set_crate_dir_and_profile(dir: Option<&'static Path>, profile: &'static str) {
    CRATE_DIR
        .set(dir)
        .expect("Crate directory set multiple times.");
    PROFILE.set(profile).expect("Profile set multiple times.");
}

/// The path for the config crate of Duat
pub fn crate_dir() -> Result<&'static Path, Text> {
    CRATE_DIR
        .get()
        .expect("Config not set yet")
        .ok_or_else(|| txt!("Config directory is [a]undefined"))
}

/// The Profile that is currently loaded
///
/// This is only valid inside of the loaded configuration's `run`
/// function, not in the metastatic Ui.
pub fn profile() -> &'static str {
    PROFILE.get().expect("Profile not set yet")
}

/// The path for a plugin's auxiliary buffers
///
/// If you want to store something in a more permanent basis, and also
/// possibly allow for the user to modify some buffers (e.g. a TOML
/// buffer with definitions for various LSPs), you should place it in
/// here.
///
/// This function will also create said directory, if it doesn't
/// already exist, only returning [`Some`], if it managed to verify
/// its existance.
pub fn plugin_dir(plugin: &str) -> Result<PathBuf, Text> {
    assert_ne!(plugin, "", "Can't have an empty plugin name");

    static PLUGIN_DIR: LazyLock<Option<&Path>> = LazyLock::new(|| {
        dirs_next::data_local_dir().map(|local_dir| {
            let path: &'static str = local_dir
                .join("duat")
                .join("plugins")
                .to_string_lossy()
                .to_string()
                .leak();

            Path::new(path)
        })
    });

    let plugin_dir = (*PLUGIN_DIR)
        .ok_or_else(|| txt!("Local directory is [a]undefined"))?
        .join(plugin);
    std::fs::create_dir_all(&plugin_dir)?;

    Ok(plugin_dir)
}

/// Convenience function for the bounds of a range
#[track_caller]
pub fn get_range(range: impl std::ops::RangeBounds<usize>, max: usize) -> Range<usize> {
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
        "start out of bounds: the len is {max}, but the index is {start}",
    );
    assert!(
        end <= max,
        "end out of bounds: the len is {max}, but the index is {end}",
    );

    start..end
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
pub fn merging_range_by_guess_and_lazy_shift<T, U: Copy + Ord + std::fmt::Debug, V: Copy>(
    (container, len): (&impl std::ops::Index<usize, Output = T>, usize),
    (guess_i, [start, end]): (usize, [U; 2]),
    (shift_from, shift, zero_shift, shift_fn): (usize, V, V, fn(U, V) -> U),
    (start_fn, end_fn): (fn(&T) -> U, fn(&T) -> U),
) -> Range<usize> {
    let sh = |n: usize| if n >= shift_from { shift } else { zero_shift };
    let start_of = |i: usize| shift_fn(start_fn(&container[i]), sh(i));
    let end_of = |i: usize| shift_fn(end_fn(&container[i]), sh(i));
    let search = |n: usize, t: &T| shift_fn(start_fn(t), sh(n));

    let mut m_range = if let Some(prev_i) = guess_i.checked_sub(1)
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
    while m_range.start > 0 && start <= end_of(m_range.start - 1) {
        m_range.start -= 1;
    }

    // This block determines how far ahead this cursor will merge
    if m_range.end < len && end >= start_of(m_range.end) {
        m_range.end = match binary_search_by_key_and_index(container, len, end, search) {
            Ok(i) => i + 1,
            Err(i) => i,
        }
    }

    while m_range.end + 1 < len && end >= start_of(m_range.end + 1) {
        m_range.end += 1;
    }

    m_range
}

/// A binary search that takes into account the index of the element
/// in order to extract the key
///
/// In Duat, this is used for searching in ordered lists where the
/// elements after a certain index are shifted by some amount, while
/// those behind that point aren't shifted at all.
pub fn binary_search_by_key_and_index<T, K>(
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

/// A function to catch panics
///
/// Used in duat-core in order to prevent sudden panics from just
/// crashing the program, which would be bad for the end user I think.
///
/// You shouldn't use this function unless you are doing a trait based
/// API, where the implementation of traits by users might cause
/// panics.
pub fn catch_panic<R>(f: impl FnOnce() -> R) -> Option<R> {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)).ok()
}

/// Macro used internally for doc tests in duat-core
#[doc(hidden)]
#[rustfmt::skip]
#[macro_export]
macro_rules! doc_duat {
    ($duat:ident) => {
        #[allow(unused, missing_docs)]
        mod $duat {
            pub mod hook {
                pub use $crate::hook::*;
            }
            
            pub mod text {
                pub use $crate::text::*;
            }
            
            pub mod cursor {
                pub use $crate::form::{
                    extra_cursor as get_extra, id_of, main_cursor as get_main,
                    set_extra_cursor as set_extra, set_main_cursor as set_main,
                    unset_cursors as unset, unset_extra_cursor as unset_extra,
                    unset_main_cursor as unset_main,
                };
            }

            pub mod opts {
                use super::prelude::*;
                pub use $crate::opts::{self, PrintOpts};
                pub fn set(set: impl FnOnce(PrintOpts) -> PrintOpts) {}
                pub fn set_lines<T>(set: T) {}
                pub fn set_status<T>(set: impl FnMut(&mut Pass) -> T) {}
                pub fn set_notifs<T>(set: T) {}
                pub fn set_logs<T>(set: T) {}
                pub fn one_line_footer() {}
            }

            pub mod data {
                pub use $crate::data::*;
            }

            pub mod state {
                use super::prelude::*;
                pub fn name_txt(buffer: &Buffer) -> Text { Text::default() }
                pub fn path_txt(buffer: &Buffer) -> Text { Text::default() }
                pub fn mode_name() -> data::DataMap<&'static str, &'static str> { todo!() }
                pub fn mode_txt() -> data::DataMap<&'static str, Text> { todo!() }
                pub fn main_byte(buffer: &Buffer) -> usize { 0 }
                pub fn main_char(buffer: &Buffer) -> usize { 0 }
                pub fn main_line(buffer: &Buffer) -> usize { 0 }
                pub fn main_col(buffer: &Buffer, area: &ui::Area) -> usize { 0 }
                pub fn main_txt(buffer: &Buffer, area: &ui::Area) -> Text { Text::default() }
                pub fn selections(buffer: &Buffer) -> usize { 0 }
                pub fn sels_txt(buffer: &Buffer) -> Text { Text::default() }
                pub fn cur_map_txt() -> data::DataMap<(Vec<KeyEvent>, bool), Text> { todo!() }
                pub fn last_key() -> data::RwData<String> { todo!() }
            }

            pub mod mode {
                pub use $crate::mode::*;
                pub use super::modes::*;
            }
                
            pub mod prelude {
                pub use std::ops::Range;
                pub use $crate::try_or_log_err;
                
                pub use $crate::{
                    Plugin, Plugins,
                    buffer::{Buffer, BufferTracker},
                    clipboard, cmd,
                    context::{self, Handle},
                    data::{self, Pass},
                    form::{self, CursorShape, Form},
                    hook::{
                        self, BufferSaved, BufferUpdated, ColorSchemeSet, ConfigLoaded, ConfigUnloaded,
                        ExitedDuat, FocusChanged, FocusedOnDuat, FormSet, Hookable, KeySent,
                        KeySentTo, ModeSwitched, UnfocusedFrom, UnfocusedFromDuat,
                        WidgetCreated, WindowCreated,
                    },
                    lender::{Lender, DoubleEndedLender, ExactSizeLender},
                    text::{
                        self, AlignCenter, AlignLeft, AlignRight, Builder, Conceal, Ghost, Spacer,
                        SpawnTag, Tagger, Text, txt, Point, Searcher
                    },
                    ui::{self, Area, Widget},
                };
                
                pub use super::{
                    cursor::*,
                    mode::{
                        self, KeyCode, KeyEvent, Mode, Prompt, Pager, User, alias, alt, ctrl, event,
                        map, shift,
                    },
                    state::*, widgets::*, PassFileType, FileType, opts, plug
                };

                #[macro_export]
                macro_rules! setup_duat{ ($setup:ident) => {} }
            }

            pub mod widgets {
                use std::fmt::Alignment;
                
                pub struct LineNumbers {
                    buffer: Handle,
                    text: Text,
                    pub relative: bool,
                    pub align: Alignment,
                    pub main_align: Alignment,
                    pub show_wraps: bool,
                }
                impl LineNumbers {
                    pub fn builder() -> LineNumbersOpts { LineNumbersOpts {
                        relative: false,
                        align: Alignment::Right,
                        main_align: Alignment::Right,
                        show_wraps: false,
                        on_the_right: false
                    }}
                }
                impl Widget for LineNumbers {
                    fn update(pa: &mut Pass, handle: &Handle<Self>) {}
                    fn needs_update(&self, pa: &Pass) -> bool { false }
                    fn text(&self) -> &Text { &self.text }
                    fn text_mut(&mut self) -> &mut Text { &mut self.text }
                }
                #[derive(Clone, Copy, Debug)]
                pub struct LineNumbersOpts {
                    pub relative: bool,
                    pub align: Alignment,
                    pub main_align: Alignment,
                    pub show_wraps: bool,
                    pub on_the_right: bool,
                }
                impl LineNumbersOpts {
                    pub fn push_on(self, _: &mut Pass, _: &Handle) {}
                }
                
                #[macro_export]
                macro_rules! status{ ($str: literal) => { $duat::widgets::StatusLine } }
                pub struct StatusLine;
                impl StatusLine {
                    pub fn above(self) -> Self { Self }
                    pub fn push_on(self, _: &mut Pass, _: &impl $crate::ui::PushTarget) {}
                }

                pub struct LogBook;
                impl LogBook {
                    pub fn builder() -> LogBookOpts { LogBookOpts::default() }
                    pub fn push_on(self, _: &mut Pass, _: &impl $crate::ui::PushTarget) {}
                }
                #[derive(Default, Clone, Copy, Debug)]
                pub struct LogBookOpts {
                    pub close_on_unfocus: bool,
                    pub hidden: bool,
                    pub side: $crate::ui::Side,
                    pub height: f32,
                    pub width: f32,
                }
                impl LogBookOpts {
                    pub fn push_on(self, _: &mut Pass, _: &impl $crate::ui::PushTarget) {}
                }
                
                use super::prelude::*;
                pub struct VertRule;
                impl VertRule {
                    pub fn builder() -> VertRuleBuilder { VertRuleBuilder }
                }
                pub struct VertRuleBuilder;
                impl VertRuleBuilder {
                    pub fn push_on(self, _: &mut Pass, _: &impl $crate::ui::PushTarget) {}
                    pub fn on_the_right(self) -> Self { self }
                }
            }

            pub mod modes {
                use super::prelude::*;
                #[derive(Clone)]
                pub struct Pager;
                impl $crate::mode::Mode for Pager {
                    type Widget = $crate::buffer::Buffer;
                    fn send_key(
                        &mut self,
                        _: &mut Pass,
                        _: $crate::mode::KeyEvent,
                        _: Handle<Self::Widget>,
                    ) {
                    }
                }
                
                #[derive(Clone)]
                pub struct Prompt;
                impl $crate::mode::Mode for Prompt {
                    type Widget = $crate::buffer::Buffer;
                    fn send_key(
                        &mut self,
                        _: &mut Pass,
                        _: $crate::mode::KeyEvent,
                        _: Handle<Self::Widget>,
                    ) {
                    }
                }

                #[derive(Clone)]
                pub struct RunCommands;
                impl RunCommands {
                    pub fn new() -> Prompt {
                        Prompt
                    }
                    pub fn new_with(initial: &str) -> Prompt {
                        Prompt
                    }
                }
            }

            pub trait FileType {
                fn filetype(&self) -> Option<&'static str> { None }
            }

            impl FileType for prelude::Buffer {}
            impl FileType for String {}
            impl FileType for &'_ str {}
            impl FileType for std::path::PathBuf {}
            impl FileType for &'_ std::path::Path {}

            pub trait PassFileType {
                fn filetype(&self, _: &prelude::Pass) -> Option<&'static str> { None }
            }
            impl PassFileType for prelude::data::RwData<prelude::Buffer> {}
            impl PassFileType for prelude::Handle {}

            pub fn plug<P: $crate::Plugin>(plugin: P) {}
        }
    }
}
