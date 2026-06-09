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
//! # duat_core::doc_duat!(duat);
//! use duat::prelude::*;
//! use ui::{PushSpecs, Side, Window, layout::Layout};
//!
//! pub struct Spiraled;
//!
//! impl Layout for Spiraled {
//!     fn new_buffer(
//!         &mut self,
//!         pa: &Pass,
//!         windows: &[Window],
//!         win: usize,
//!     ) -> Option<(Handle<Buffer>, PushSpecs)> {
//!         let buffers = windows[win].buffers(pa);
//!         let last = buffers.iter().last().unwrap().clone();
//!
//!         Some(match buffers.len() % 4 {
//!             0 => (last, PushSpecs { side: Side::Right, ..Default::default() }),
//!             1 => (last, PushSpecs { side: Side::Below, ..Default::default() }),
//!             2 => (last, PushSpecs { side: Side::Left, ..Default::default() }),
//!             3 => (last, PushSpecs { side: Side::Above, ..Default::default() }),
//!             _ => unreachable!("That's not how math works, man!"),
//!         })
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
    buffer::Buffer,
    context::Handle,
    data::Pass,
    ui::{Side, Window},
};

/// A form of organizing opened [`Buffer`]s
///
/// Determines how the n'th `Buffer` should be opened, given the
/// previously opened `Buffer`s on the same window.
///
/// [`Buffer`]: crate::buffer::Buffer
pub trait Layout: Send {
    /// Opens a new [`Buffer`].
    ///
    /// The returned `(Handle<Buffer>, PushSpecs)` value
    /// represents the [`PushSpecs`] to use when pushing this new
    /// [`Buffer`], and the [`Handle<Buffer>`] representing which
    /// `Buffer` to push this `Buffer` to.
    ///
    /// If this returned value is [`None`], then the [`Buffer`] will
    /// be placed on a new window instead.
    ///
    /// There will _always_ be at least one `Buffer` open, since the
    /// first opened `Buffer` doesn't follow layouts.
    fn new_buffer(
        &mut self,
        pa: &Pass,
        windows: &[Window],
        win: usize,
    ) -> Option<(Handle<Buffer>, PushSpecs)>;
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
    fn new_buffer(
        &mut self,
        pa: &Pass,
        windows: &[Window],
        win: usize,
    ) -> Option<(Handle<Buffer>, PushSpecs)> {
        let last = windows[win].buffers(pa).last().unwrap().clone();
        Some(if windows[win].buffers(pa).len() == 1 {
            (last, PushSpecs { side: Side::Right, ..Default::default() })
        } else {
            (last, PushSpecs { side: Side::Below, ..Default::default() })
        })
    }
}
