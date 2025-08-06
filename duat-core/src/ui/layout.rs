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
//!         prev: Vec<(Handle<File<U>, U>, FileId<U>)>,
//!     ) -> Result<(FileId<U>, PushSpecs), Text> {
//!         // One File is always open.
//!         let (_, last) = prev.last().unwrap();
//!         match prev.len() % 4 {
//!             1 => Ok((last.clone(), PushSpecs::right())),
//!             2 => Ok((last.clone(), PushSpecs::below())),
//!             3 => Ok((last.clone(), PushSpecs::left())),
//!             4 => Ok((last.clone(), PushSpecs::above())),
//!             _ => unreachable!("That's not how math works, man!"),
//!         }
//!     }
//! }
//! ```
//!
//! Also notice that this function can fail, which means you can set a
//! limit to how many [`File`]s should can open in a single window.
use super::{Node, PushSpecs, Ui};
use crate::{data::Pass, file::File, prelude::Handle, text::Text};

/// A form of organizing opened [`File`]s
///
/// Determines how the n'th [`File`] should be opened, given the
/// previously opened [`File`]s on the same window.
pub trait Layout<U>: Send + Sync
where
    U: Ui,
{
    /// Opens a new [`File`]
    ///
    /// The returned [`Ok(FileId, PushSpecs)`] value represents
    /// the [`PushSpecs`] to use when pushing this new [`File`],
    /// and the [`FileId`] representing which [`File`] to push
    /// this [`File`] to.
    ///
    /// There will _always_ be at least one [`File`] open, since the
    /// first opened [`File`] doesn't follow layouts.
    ///
    /// [`Ok(FileId, PushSpecs)`]: FileId
    fn new_file(
        &mut self,
        file: &File<U>,
        prev: Vec<Handle<File<U>, U>>,
    ) -> Result<(Handle<File<U>, U>, PushSpecs), Text>;
}

/// [`Layout`]: One [`File`] on the left, others on the right
///
/// One [`File`] will occupy the whole left side of the screen, and
/// future files will be vertically stacked on the right
#[derive(Clone)]
pub struct MasterOnLeft;

impl<U> Layout<U> for MasterOnLeft
where
    U: Ui,
{
    fn new_file(
        &mut self,
        _file: &File<U>,
        prev: Vec<Handle<File<U>, U>>,
    ) -> Result<(Handle<File<U>, U>, PushSpecs), Text> {
        let last = prev.last().unwrap().clone();
        Ok(if prev.len() == 1 {
            (last, PushSpecs::right())
        } else {
            (last, PushSpecs::below())
        })
    }
}

/// The list of all [`File`]s in a window
pub(super) fn window_files<U: Ui>(pa: &Pass, nodes: &[Node<U>]) -> Vec<Handle<File<U>, U>> {
    let mut files: Vec<Handle<File<U>, U>> = nodes
        .iter()
        .filter_map(|node| node.try_downcast())
        .collect();

    files.sort_unstable_by_key(|file| file.read(pa).layout_order);

    files
}
