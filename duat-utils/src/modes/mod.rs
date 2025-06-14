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
//! use duat_core::prelude::*;
//! use duat_utils::IncSearcher;
//!
//! #[derive(Clone, Copy)]
//! struct KeepMatching;
//!
//! impl<U: Ui> IncSearcher for KeepMatching {
//!     fn search(
//!         &mut self,
//!         pa: &mut Pass,
//!         handle: FileHandle<U>,
//!         se: Searcher,
//!     ) {
//!         let mut helper = EditHelper::inc_from_handle(pa, handle, se);
//!
//!         helper.edit_all(pa, |mut e| {
//!             e.set_caret_on_start();
//!             let [_, end] = e.range();
//!             if e.search_inc_fwd(Some(end)).next().is_none() {
//!                 e.destroy();
//!             }
//!         });
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
