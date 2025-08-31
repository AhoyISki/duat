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
    sync::{LazyLock, OnceLock},
};

use parking_lot::RwLock;

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
        .ok_or_else(|| txt!("Config directory is [a]undefined").build())
}

/// The Profile that is currently loaded
///
/// This is only valid inside of the loaded configuration's `run`
/// function, not in the metastatic Ui.
pub fn profile() -> &'static str {
    PROFILE.get().expect("Profile not set yet")
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
pub fn get_ends(range: impl std::ops::RangeBounds<usize>, max: usize) -> (usize, usize) {
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
