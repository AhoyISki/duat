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
//!     fn search(
//!         &mut self,
//!         pa: &mut Pass,
//!         handle: Handle<File<U>, U, Searcher>,
//!     ) {
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
pub use self::{
    inc_search::{ExtendFwd, ExtendRev, IncSearcher, SearchFwd, SearchRev},
    prompt::{IncSearch, PipeSelections, Prompt, PromptMode, RunCommands},
    regular::Regular,
};

mod inc_search;
mod prompt;
mod regular;
