//! Utilities for incremental search in Duat
//!
//! This specific feature of Duat is kind of split across this crate
//! and [`duat-core`], since some of the low level features (like
//! spawning a bazillion [`Cursor`]s) were only possible with access
//! to private things.
//!
//! [`duat-core`]: duat_core
//! [`Cursor`]: duat_core::mode::Cursor
use duat_core::{
    buffer::Buffer,
    context::Handle,
    data::Pass,
    text::{Searcher, Text, txt},
};

/// An abstraction trait used to handle incremental search
///
/// This trait can be used for various ways of interpreting what
/// incremental search should do, right now, these are the
/// implementations of [`IncSearcher`]:
///
/// - [`SearchFwd`]: In each cursor, searches forward for the match
/// - [`SearchRev`]: In each cursor, searches backwards for the match
/// - [`ExtendFwd`]: In each cursor, extends forward for the match
/// - [`ExtendRev`]: In each cursor, extends backwards for the match
///
/// Here is how you can implement this trait yourself:
///
/// ```rust
/// use duat_core::{prelude::*, text::Searcher};
/// use duat_utils::modes::IncSearcher;
///
/// #[derive(Clone, Copy)]
/// struct SearchAround;
///
/// impl IncSearcher for SearchAround {
///     fn search(&mut self, pa: &mut Pass, handle: Handle<Buffer, U, Searcher>) {
///         handle.edit_all(pa, |mut c| {
///             c.set_caret_on_end();
///             let Some([_, p1]) = c.search_inc_fwd(None).next() else {
///                 return;
///             };
///
///             c.set_caret_on_start();
///             let Some([p0, _]) = c.search_inc_rev(None).next() else {
///                 return;
///             };
///
///             c.move_to(p0);
///             c.set_anchor();
///             c.move_to(p1);
///         });
///     }
///
///     fn prompt(&self) -> Text {
///         txt!("[prompt]search around")
///     }
/// }
/// ```
///
/// There are more advanced implementations in the [`duat-kak`] crate
///
/// [`duat-kak`]: https://docs.rs/duat-kak
pub trait IncSearcher: Clone + Send + 'static {
    /// Performs the incremental search
    fn search(&mut self, pa: &mut Pass, handle: Handle<Buffer, Searcher>);

    /// What prompt to show in the [`PromptLine`]
    ///
    /// [`PromptLine`]: crate::widgets::PromptLine
    fn prompt(&self) -> Text;
}

/// Searches forward on each [`Cursor`]
///
/// [`Cursor`]: duat_core::mode::Cursor
#[derive(Clone, Copy)]
pub struct SearchFwd;

impl IncSearcher for SearchFwd {
    fn search(&mut self, pa: &mut Pass, handle: Handle<Buffer, Searcher>) {
        handle.edit_all(pa, |mut c| {
            let caret = c.caret();
            if let Some(range) = { c.search_inc_fwd(None).find(|r| r.start != caret.byte()) } {
                c.move_to(range)
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]search")
    }
}

/// Searches backwards on each [`Cursor`]
///
/// [`Cursor`]: duat_core::mode::Cursor
#[derive(Clone, Copy)]
pub struct SearchRev;

impl IncSearcher for SearchRev {
    fn search(&mut self, pa: &mut Pass, handle: Handle<Buffer, Searcher>) {
        handle.edit_all(pa, |mut c| {
            let caret = c.caret();
            if let Some(range) = { c.search_inc_rev(None).find(|r| r.end != caret.byte()) } {
                c.move_to(range)
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]rev search")
    }
}

/// Extends forward on each [`Cursor`]
///
/// [`Cursor`]: duat_core::mode::Cursor
#[derive(Clone, Copy)]
pub struct ExtendFwd;

impl IncSearcher for ExtendFwd {
    fn search(&mut self, pa: &mut Pass, handle: Handle<Buffer, Searcher>) {
        handle.edit_all(pa, |mut c| {
            let caret = c.caret();
            if let Some(range) = { c.search_inc_fwd(None).find(|r| r.start != caret.byte()) } {
                c.set_anchor_if_needed();
                c.move_to(range.end);
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]search (extend)")
    }
}

/// Extends backwards on each [`Cursor`]
///
/// [`Cursor`]: duat_core::mode::Cursor
#[derive(Clone, Copy)]
pub struct ExtendRev;

impl IncSearcher for ExtendRev {
    fn search(&mut self, pa: &mut Pass, handle: Handle<Buffer, Searcher>) {
        handle.edit_all(pa, |mut c| {
            let caret = c.caret();
            if let Some(range) = { c.search_inc_rev(None).find(|r| r.end != caret.byte()) } {
                c.set_anchor_if_needed();
                c.move_to(range.start);
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]rev search (extend)")
    }
}
