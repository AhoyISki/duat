pub use internals::{FileId, Layout};
pub(super) use internals::{LayoutFiles, iter_files_for_layout};

use super::{PushSpecs, Ui};
use crate::{Result, widgets::File};

mod internals {
    use crate::{
        Result,
        data::RwData,
        ui::{Area, Node, PushSpecs, Ui},
        widgets::{File, Widget},
    };

    pub trait Layout<U>: Send + Sync
    where
        U: Ui,
    {
        fn new_file(
            &mut self,
            file: &File,
            prev: LayoutFiles<'_, U>,
        ) -> Result<(FileId<U>, PushSpecs), ()>;
    }

    pub struct FileId<U>(pub(in crate::ui) U::Area)
    where
        U: Ui;

    pub fn iter_files_for_layout<U>(nodes: &[Node<U>]) -> LayoutFiles<'_, U>
    where
        U: Ui,
    {
        nodes
            .iter()
            .filter(|&node| node.widget().data_is::<File>())
            .map(|node| {
                let area = node
                    .area()
                    .get_cluster_master()
                    .unwrap_or_else(|| node.area().clone());
                (node.widget(), FileId::<U>(area))
            })
    }

    pub type LayoutFiles<'a, U: Ui> =
        impl Iterator<Item = (&'a RwData<dyn Widget<U>>, FileId<U>)> + 'a + Clone;
}

pub struct MasterOnLeft;

impl<U> Layout<U> for MasterOnLeft
where
    U: Ui,
{
    fn new_file(
        &mut self,
        _file: &File,
        prev: LayoutFiles<'_, U>,
    ) -> Result<(FileId<U>, PushSpecs), ()> {
        let (i, (_, last)) = prev.enumerate().last().unwrap();
        Ok(match i == 0 {
            true => (last, PushSpecs::right()),
            false => (last, PushSpecs::below()),
        })
    }
}
