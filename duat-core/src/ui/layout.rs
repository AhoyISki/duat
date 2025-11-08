//! Ways to organize opened [`Buffer`]s
//!
//! By default, when calling `:e some_buffer<Enter>`, Duat will follow
//! [`MasterOnLeft`], a type of [`Layout`] for opening `Buffer`s.
//! That is, the first opened `Buffer` will be on the left of the
//! screeng, and all subsequent `Buffer`s will be stacked vertically
//! on the right of the screen.
//!
//! You can create your own [`Layout`] fairly trivially, for example,
//! here's a spiraled layout:
//!
//! ```rust
//! #![feature(default_field_values)]
//! # duat_core::doc_duat!(duat);
//! use duat::prelude::*;
//! use ui::{PushSpecs, Side, Window, layout::Layout};
//!
//! pub struct Spiraled;
//!
//! impl Layout for Spiraled {
//!     fn new_buffer(&mut self, pa: &Pass, windows: &[Window]) -> (Handle, PushSpecs) {
//!         let cur_win = context::current_win_index(pa);
//!         let buffers = windows[cur_win].buffers(pa);
//!         let last = buffers.iter().last().unwrap().clone();
//!
//!         match buffers.len() % 4 {
//!             0 => (last, PushSpecs { side: Side::Right, .. }),
//!             1 => (last, PushSpecs { side: Side::Below, .. }),
//!             2 => (last, PushSpecs { side: Side::Left, .. }),
//!             3 => (last, PushSpecs { side: Side::Above, .. }),
//!             _ => unreachable!("That's not how math works, man!"),
//!         }
//!     }
//! }
//! ```
//!
//! Also notice that this function can fail, which means you can set a
//! limit to how many [`Buffer`]s should can open in a single window.
//!
//! [`Buffer`]: crate::buffer::Buffer
use super::PushSpecs;
use crate::{
    context::{self, Handle},
    data::Pass,
    ui::{Side, Window},
};

/// A form of organizing opened [`Buffer`]s
///
/// Determines how the n'th `Buffer` should be opened, given the
/// previously opened `Buffer`s on the same window.
///
/// [`Buffer`]: crate::buffer::Buffer
pub trait Layout: Send + Sync {
    /// Opens a new [`Buffer`]
    ///
    /// The returned `(Handle<Buffer>, PushSpecs)` value
    /// represents the [`PushSpecs`] to use when pushing this new
    /// [`Buffer`], and the [`Handle<Buffer>`] representing which
    /// `Buffer` to push this `Buffer` to.
    ///
    /// There will _always_ be at least one `Buffer` open, since the
    /// first opened `Buffer` doesn't follow layouts.
    ///
    /// [`Ok(Handle<Buffer>, PushSpecs)`]: Handle
    ///
    /// [`Buffer`]: crate::buffer::Buffer
    fn new_buffer(&mut self, pa: &Pass, windows: &[Window]) -> (Handle, PushSpecs);
}

/// [`Layout`]: One [`Buffer`] on the left, others on the right
///
/// One `Buffer` will occupy the whole left side of the screen, and
/// future buffers will be vertically stacked on the right
///
/// [`Buffer`]: crate::buffer::Buffer
#[derive(Clone)]
pub struct MasterOnLeft;

impl Layout for MasterOnLeft {
    fn new_buffer(&mut self, pa: &Pass, windows: &[Window]) -> (Handle, PushSpecs) {
        let cur_win = context::current_win_index(pa);
        let last = windows[cur_win].buffers(pa).last().unwrap().clone();
        if windows[cur_win].buffers(pa).len() == 1 {
            (last, PushSpecs { side: Side::Right, ..Default::default() })
        } else {
            (last, PushSpecs { side: Side::Below, ..Default::default() })
        }
    }
}
