//! Ways to organize opened [`File`]s
//!
//! By default, when calling `:e some_file<Enter>`, Duat will follow
//! [`MasterOnLeft`], a type of [`Layout`] for opening [`File`]s. That
//! is, the first opened [`File`] will be on the left of the screeng,
//! and all subsequent [`File`]s will be stacked vertically on the
//! right of the screen.
//!
//! You can create your own [`Layout`] fairly trivially, for example,
//! here's a spiraled layout:
//!
//! ```rust
//! use duat_core::{
//!     data::RwData,
//!     prelude::*,
//!     ui::{PushSpecs, layout::*},
//! };
//! pub struct Spiraled;
//!
//! impl<U: Ui> Layout<U> for Spiraled {
//!     fn new_file(
//!         &mut self,
//!         file: &File<U>,
//!         prev: Vec<Handle<File<U>, U>>,
//!     ) -> Result<(Handle<File<U>, U>, PushSpecs), Text> {
//!         // One File is always open.
//!         let last = prev.last().unwrap().clone();
//!         match prev.len() % 4 {
//!             1 => Ok((last, PushSpecs::right())),
//!             2 => Ok((last, PushSpecs::below())),
//!             3 => Ok((last, PushSpecs::left())),
//!             4 => Ok((last, PushSpecs::above())),
//!             _ => unreachable!("That's not how math works, man!"),
//!         }
//!     }
//! }
//! ```
//!
//! Also notice that this function can fail, which means you can set a
//! limit to how many [`File`]s should can open in a single window.
use super::{PushSpecs, Ui};
use crate::{
    data::Pass,
    file::File,
    prelude::Handle,
    ui::{Side, Window},
};

/// A form of organizing opened [`File`]s
///
/// Determines how the n'th [`File`] should be opened, given the
/// previously opened [`File`]s on the same window.
pub trait Layout<U: Ui>: Send + Sync {
    /// Opens a new [`File`]
    ///
    /// The returned [`Ok(Handle<File>, PushSpecs)`] value represents
    /// the [`PushSpecs`] to use when pushing this new [`File`],
    /// and the [`Handle<File>`] representing which [`File`] to push
    /// this [`File`] to.
    ///
    /// There will _always_ be at least one [`File`] open, since the
    /// first opened [`File`] doesn't follow layouts.
    ///
    /// [`Ok(Handle<File>, PushSpecs)`]: Handle
    fn new_file(
        &mut self,
        pa: &Pass,
        cur_win: usize,
        file: &File<U>,
        windows: &[Window<U>],
    ) -> (Handle<File<U>, U>, PushSpecs);
}

/// [`Layout`]: One [`File`] on the left, others on the right
///
/// One [`File`] will occupy the whole left side of the screen, and
/// future files will be vertically stacked on the right
#[derive(Clone)]
pub struct MasterOnLeft;

impl<U: Ui> Layout<U> for MasterOnLeft {
    fn new_file(
        &mut self,
        pa: &Pass,
        cur_win: usize,
        _file: &File<U>,
        windows: &[Window<U>],
    ) -> (Handle<File<U>, U>, PushSpecs) {
        let last = windows[cur_win].file_handles(pa).last().unwrap().clone();
        if windows[cur_win].file_handles(pa).len() == 1 {
            (last, PushSpecs { side: Side::Right, .. })
        } else {
            (last, PushSpecs { side: Side::Below, .. })
        }
    }
}
