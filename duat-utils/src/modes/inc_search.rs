//! Utilities for incremental search in Duat
//!
//! This specific feature of Duat is kind of split across this crate
//! and [`duat-core`], since some of the low level features (like
//! spawning a bazillion [`Cursor`]s) were only possible with access
//! to private things.
//!
//! [`duat-core`]: duat_core
//! [`Cursor`]: duat_core::mode::Cursor
use duat_core::{prelude::*, text::Searcher};

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
/// impl<U: Ui> IncSearcher<U> for SearchAround {
///     fn search(&mut self, pa: &mut Pass, mut handle: Handle<File<U>, U, Searcher>) {
///        handle.edit_all(pa, |mut e| {
///            e.set_caret_on_end();
///            let Some([_, p1]) = e.search_inc_fwd(None).next() else {
///                return;
///            };
///
///            e.set_caret_on_start();
///            let Some([p0, _]) = e.search_inc_rev(None).next() else {
///                return;
///            };
///
///            e.move_to(p0);
///            e.set_anchor();
///            e.move_to(p1);
///        });
///    }
///
///    fn prompt(&self) -> Text {
///        txt!("[Prompt]search around[Prompt.colon]:").build()
///    }
/// }
/// ```
///
/// There are more advanced implementations in the [`duat-kak`] crate
///
/// [`duat-kak`]: https://docs.rs/duat-kak
pub trait IncSearcher<U: Ui>: Clone + 'static {
    /// Performs the incremental search
    fn search(&mut self, pa: &mut Pass, handle: Handle<File<U>, U, Searcher>);

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

impl<U: Ui> IncSearcher<U> for SearchFwd {
    fn search(&mut self, pa: &mut Pass, mut handle: Handle<File<U>, U, Searcher>) {
        handle.edit_all(pa, |mut e| {
            let caret = e.caret();
            let next = e.search_inc_fwd(None).find(|[p, _]| *p != caret);
            if let Some([p0, p1]) = next {
                e.move_to(p0);
                if p1 > p0 {
                    e.set_anchor();
                    e.move_to(p1);
                    e.move_hor(-1);
                }
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[Prompt]search[Prompt.colon]:").build()
    }
}

/// Searches backwards on each [`Cursor`]
///
/// [`Cursor`]: duat_core::mode::Cursor
#[derive(Clone, Copy)]
pub struct SearchRev;

impl<U: Ui> IncSearcher<U> for SearchRev {
    fn search(&mut self, pa: &mut Pass, mut handle: Handle<File<U>, U, Searcher>) {
        handle.edit_all(pa, |mut e| {
            let caret = e.caret();
            let next = e.search_inc_rev(None).find(|[_, p]| *p != caret);
            if let Some([p0, p1]) = next {
                e.move_to(p0);
                if p1 > p0 {
                    e.set_anchor();
                    e.move_to(p1);
                    e.move_hor(-1);
                }
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[Prompt]rev search[Prompt.colon]:").build()
    }
}

/// Extends forward on each [`Cursor`]
///
/// [`Cursor`]: duat_core::mode::Cursor
#[derive(Clone, Copy)]
pub struct ExtendFwd;

impl<U: Ui> IncSearcher<U> for ExtendFwd {
    fn search(&mut self, pa: &mut Pass, mut handle: Handle<File<U>, U, Searcher>) {
        handle.edit_all(pa, |mut e| {
            let caret = e.caret();
            let next = e.search_inc_fwd(None).find(|[p, _]| *p != caret);
            if let Some([_, p1]) = next {
                if e.anchor().is_none() {
                    e.set_anchor();
                }
                e.move_to(p1);
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[Prompt]search (extend)[Prompt.colon]:").build()
    }
}

/// Extends backwards on each [`Cursor`]
///
/// [`Cursor`]: duat_core::mode::Cursor
#[derive(Clone, Copy)]
pub struct ExtendRev;

impl<U: Ui> IncSearcher<U> for ExtendRev {
    fn search(&mut self, pa: &mut Pass, mut handle: Handle<File<U>, U, Searcher>) {
        handle.edit_all(pa, |mut e| {
            let caret = e.caret();
            let next = e.search_inc_rev(None).find(|[_, p]| *p != caret);
            if let Some([p0, _]) = next {
                if e.anchor().is_none() {
                    e.set_anchor();
                }
                e.move_to(p0);
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[Prompt]rev search (extend)[Prompt.colon]:").build()
    }
}
