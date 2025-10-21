//! Ways to organize opened [`Buffer`]s
//!
//! By default, when calling `:e some_file<Enter>`, Duat will follow
//! [`MasterOnLeft`], a type of [`Layout`] for opening [`Buffer`]s.
//! That is, the first opened [`Buffer`] will be on the left of the
//! screeng, and all subsequent [`Buffer`]s will be stacked vertically
//! on the right of the screen.
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
//!         buffer: &Buffer<U>,
//!         prev: Vec<Handle<Buffer<U>, U>>,
//!     ) -> Result<(Handle<Buffer<U>, U>, PushSpecs), Text> {
//!         // One Buffer is always open.
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
//! limit to how many [`Buffer`]s should can open in a single window.
use super::PushSpecs;
use crate::{
    buffer::Buffer,
    context::Handle,
    data::Pass,
    ui::{Side, Window},
};

/// A form of organizing opened [`Buffer`]s
///
/// Determines how the n'th [`Buffer`] should be opened, given the
/// previously opened [`Buffer`]s on the same window.
pub trait Layout: Send + Sync {
    /// Opens a new [`Buffer`]
    ///
    /// The returned [`Ok(Handle<Buffer>, PushSpecs)`] value
    /// represents the [`PushSpecs`] to use when pushing this new
    /// [`Buffer`], and the [`Handle<Buffer>`] representing which
    /// [`Buffer`] to push this [`Buffer`] to.
    ///
    /// There will _always_ be at least one [`Buffer`] open, since the
    /// first opened [`Buffer`] doesn't follow layouts.
    ///
    /// [`Ok(Handle<Buffer>, PushSpecs)`]: Handle
    fn new_buffer(
        &mut self,
        pa: &Pass,
        cur_win: usize,
        buffer: &Buffer,
        windows: &[Window],
    ) -> (Handle, PushSpecs);
}

/// [`Layout`]: One [`Buffer`] on the left, others on the right
///
/// One [`Buffer`] will occupy the whole left side of the screen, and
/// future buffers will be vertically stacked on the right
#[derive(Clone)]
pub struct MasterOnLeft;

impl Layout for MasterOnLeft {
    fn new_buffer(
        &mut self,
        pa: &Pass,
        cur_win: usize,
        _file: &Buffer,
        windows: &[Window],
    ) -> (Handle, PushSpecs) {
        let last = windows[cur_win].buffers(pa).last().unwrap().clone();
        if windows[cur_win].buffers(pa).len() == 1 {
            (last, PushSpecs { side: Side::Right, .. })
        } else {
            (last, PushSpecs { side: Side::Below, .. })
        }
    }
}
