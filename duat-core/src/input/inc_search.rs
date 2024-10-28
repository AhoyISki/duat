use super::{Cursors, EditHelper};
use crate::{
    data::RwData,
    text::Searcher,
    ui::{Area, Ui},
    widgets::File,
};

pub trait IncSearcher<U: Ui>: Sized + Send + Sync + 'static {
    fn new(
        file: &RwData<File>,
        area: &U::Area,
        cursors: Option<Cursors>,
    ) -> (Self, Option<Cursors>);

    fn search(
        &mut self,
        file: &RwData<File>,
        area: &U::Area,
        searcher: Searcher,
        cursors: Option<Cursors>,
    ) -> Option<Cursors>;

    #[allow(unused)]
    fn finish(
        &mut self,
        file: &RwData<File>,
        area: &U::Area,
        cursors: Option<Cursors>,
    ) -> Option<Cursors> {
        cursors
    }
}

pub struct Fwd<U: Ui> {
    cursors: Cursors,
    info: <U::Area as Area>::PrintInfo,
}

impl<U: Ui> IncSearcher<U> for Fwd<U> {
    fn new(
        _file: &RwData<File>,
        area: &U::Area,
        cursors: Option<Cursors>,
    ) -> (Self, Option<Cursors>) {
        (
            Self {
                cursors: cursors.clone().unwrap_or_default(),
                info: area.get_print_info(),
            },
            cursors,
        )
    }

    fn search(
        &mut self,
        file: &RwData<File>,
        area: &U::Area,
        searcher: Searcher,
        _cursors: Option<Cursors>,
    ) -> Option<Cursors> {
        if searcher.is_empty() {
            area.set_print_info(self.info.clone());
            return Some(self.cursors.clone());
        }

        let mut cursors = self.cursors.clone();

        let mut helper = EditHelper::new_inc(file, area, &mut cursors, searcher);

        helper.move_each(|m| {
            let next = m.search_inc(None).next();
            if let Some((p0, p1)) = next {
                m.move_to(p0);
                m.set_anchor();
                m.move_to(p1);
                m.move_hor(-1);
            } else if m.is_main() {
                area.set_print_info(self.info.clone());
            }
        });

        drop(helper);

        Some(cursors)
    }
}
