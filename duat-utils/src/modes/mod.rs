//! Additional [`Mode`]s for Duat
//!
//! This module adds some very important [`Mode`]s, not only for base
//! Duat configuration, but also in extending it with a lot of
//! functionality.
//!
//! Chief among the extensible modes in here is the [`Prompt`] and
//! [`IncSearch`] [`Mode`]s, which can leverage traits to do a wide
//! variety of things:
//!
//! ```rust
//! use duat_core::{prelude::*, text::Searcher};
//! use duat_utils::modes::IncSearcher;
//!
//! #[derive(Clone, Copy)]
//! struct KeepMatching;
//!
//! impl<U: Ui> IncSearcher<U> for KeepMatching {
//!     fn search(&mut self, pa: &mut Pass, handle: Handle<File<U>, U, Searcher>) {
//!         handle.edit_all(pa, |mut c| {
//!             c.set_caret_on_start();
//!             let [_, end] = c.range();
//!             if c.search_inc_fwd(Some(end)).next().is_none() {
//!                 c.destroy();
//!             }
//!         });
//!     }
//!
//!     fn prompt(&self) -> Text {
//!         txt!("[Prompt]keep matching[Prompt.colon]:").build()
//!     }
//! }
//! ```
//!
//! The above [`IncSearcher`] will do the incremental search, and keep
//! only the [`Cursor`]s that matched at some point inside of their
//! selections.
//!
//! [`Mode`]: duat_core::mode::Mode
//! [`Cursor`]: duat_core::mode::Cursor
use std::sync::Mutex;

use duat_core::{data::Pass, file::File, prelude::Handle, ui::Ui};

pub use self::{
    inc_search::{ExtendFwd, ExtendRev, IncSearcher, SearchFwd, SearchRev},
    pager::{Pager, PagerSearch},
    prompt::{IncSearch, PipeSelections, Prompt, PromptMode, RunCommands},
};

mod inc_search;
mod pager;
mod prompt;

static CLIPBOARD: Mutex<Vec<String>> = Mutex::new(Vec::new());
/// Copy the text of the [`Selection`]s in the [`Handle`]
///
/// This function does _not_ make use of the same clipboard as
/// [`duat_core::clipboard`]. That one acts in the same way as the
/// system's clipboard.
///
/// This clipboard is able to store any number of selections, which
/// can later be copied via [`paste_strings`]. If only one non empty
/// selection is copied, it will also be copied to the system's
/// clipboard.
///
/// [`Selection`]: duat_core::mode::Selection
pub fn copy_selections<U: Ui>(pa: &mut Pass, handle: &Handle<File<U>, U, ()>) {
    let mut copies: Vec<String> = Vec::new();
    handle.edit_all(pa, |c| copies.push(c.selection().collect()));
    if copies.len() == 1 && !copies.first().unwrap().is_empty() {
        duat_core::clipboard::set_text(copies.first().unwrap());
    }
    *CLIPBOARD.lock().unwrap() = copies
}

/// Pastes the strings copied with [`copy_selections`]
///
/// If the system clipboard was updated since after the last call to
/// [`copy_selections`], this function will return a one element
/// [`Vec<String>`] with the new contents of the system clipboard,
/// instead of the selections taken via [`copy_selections`].
pub fn paste_strings() -> Vec<String> {
    static SYSTEM_CLIPB: Mutex<Option<String>> = Mutex::new(None);

    let paste = duat_core::clipboard::get_text();

    let mut sys_clipb = SYSTEM_CLIPB.lock().unwrap();

    // If there was no previous clipboard, or it has changed, copy the new
    // pasted text
    if let Some(paste) = paste
        && sys_clipb.as_ref().is_none_or(|sc| *sc != paste)
    {
        *CLIPBOARD.lock().unwrap() = vec![paste.clone()];
        *sys_clipb = Some(paste.clone());
        vec![paste]
    } else {
        CLIPBOARD.lock().unwrap().clone()
    }
}
