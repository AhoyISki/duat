pub use internals::{FileId, Layout, window_files};

use super::{PushSpecs, Ui};
use crate::{data::RwData, file::File, text::Text};

mod internals {
    use crate::{
        data::{Pass, RwData},
        file::File,
        text::Text,
        ui::{Node, PushSpecs, RawArea, Ui},
    };

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
        /// [`Ok(FileId, PushSpecs)`]: FileId
        fn new_file(
            &mut self,
            file: &File<U>,
            prev: Vec<(RwData<File<U>>, FileId<U>)>,
        ) -> Result<(FileId<U>, PushSpecs), Text>;
    }

    /// A unique identifier for a [`File`]
    pub struct FileId<U: Ui>(pub(crate) U::Area);

    /// The list of all [`File`]s in a window
    pub fn window_files<U: Ui>(pa: &Pass, nodes: &[Node<U>]) -> Vec<(RwData<File<U>>, FileId<U>)> {
        let mut files: Vec<_> = nodes
            .iter()
            .filter(|&node| node.widget().data_is::<File<U>>())
            .map(|node| {
                let area = node
                    .area()
                    .get_cluster_master()
                    .unwrap_or_else(|| node.area().clone());
                let (file, ..) = node.as_file().unwrap();
                (file, FileId(area))
            })
            .collect();

        files.sort_unstable_by_key(|(file, _)| file.read(pa, |f| f.layout_order));

        files
    }
}

/// [`Layout`]: One [`File`] on the left, others on the right
///
/// One [`File`] will occupy the whole left side of the screen, and
/// future files will be vertically stacked on the right
pub struct MasterOnLeft;

impl<U> Layout<U> for MasterOnLeft
where
    U: Ui,
{
    fn new_file(
        &mut self,
        _file: &File<U>,
        mut prev: Vec<(RwData<File<U>>, FileId<U>)>,
    ) -> Result<(FileId<U>, PushSpecs), Text> {
        let (_, last) = prev.pop().unwrap();
        Ok(if prev.is_empty() {
            (last, PushSpecs::right())
        } else {
            (last, PushSpecs::below())
        })
    }
}
