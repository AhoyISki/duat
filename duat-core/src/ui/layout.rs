pub use internals::{FileId, Layout, WindowFiles, window_files};

use super::{PushSpecs, Ui};
use crate::{text::Text, widgets::File};

mod internals {
    use crate::{
        text::Text,
        ui::{Area, Node, PushSpecs, Ui},
        widgets::File,
    };

    pub trait Layout<U>: Send + Sync
    where
        U: Ui,
    {
        fn new_file(
            &mut self,
            file: &File,
            prev: WindowFiles<'_, U>,
        ) -> Result<(FileId<U>, PushSpecs), Text>;
    }

    pub struct FileId<U: Ui>(pub(in crate::ui) U::Area);

    pub fn window_files<U: Ui>(nodes: &[Node<U>]) -> WindowFiles<'_, U> {
        let mut files: Vec<(&Node<U>, FileId<U>)> = nodes
            .iter()
            .filter(|&node| node.widget().data_is::<File>())
            .map(|node| {
                let area = node
                    .area()
                    .get_cluster_master()
                    .unwrap_or_else(|| node.area().clone());
                (node, FileId::<U>(area))
            })
            .collect();

        files.sort_unstable_by_key(|(node, _)| node.read_as::<File>().unwrap().layout_ordering);

        files
    }

    pub type WindowFiles<'a, U> = Vec<(&'a Node<U>, FileId<U>)>;
}

pub struct MasterOnLeft;

impl<U> Layout<U> for MasterOnLeft
where
    U: Ui,
{
    fn new_file(
        &mut self,
        _file: &File,
        mut prev: WindowFiles<'_, U>,
    ) -> Result<(FileId<U>, PushSpecs), Text> {
        let (_, last) = prev.pop().unwrap();
        Ok(if prev.is_empty() {
            (last, PushSpecs::right())
        } else {
            (last, PushSpecs::below())
        })
    }
}
