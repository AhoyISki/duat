use super::{Cursors, EditHelper};
use crate::{
    data::RwData,
    text::Searcher,
    ui::{Area, Ui},
    widgets::File,
};

pub trait IncSearcher<U: Ui>: Sized + Send + Sync + 'static {
    fn new(file: &RwData<File>, area: &U::Area, cursors: &mut Cursors) -> Self;

    fn search(
        &mut self,
        file: &RwData<File>,
        area: &U::Area,
        searcher: Searcher,
        cursors: &mut Cursors,
    );

    #[allow(unused)]
    fn finish(&mut self, file: &RwData<File>, area: &U::Area, cursors: &mut Cursors) {}
}

pub struct Fwd<U: Ui> {
    cursors: Cursors,
    info: <U::Area as Area>::PrintInfo,
}

impl<U: Ui> IncSearcher<U> for Fwd<U> {
    fn new(_file: &RwData<File>, area: &U::Area, cursors: &mut Cursors) -> Self {
        Self {
            cursors: cursors.clone(),
            info: area.get_print_info(),
        }
    }

    fn search(
        &mut self,
        file: &RwData<File>,
        area: &U::Area,
        searcher: Searcher,
        cursors: &mut Cursors,
    ) {
        if searcher.is_empty() {
            area.set_print_info(self.info.clone());
            return;
        }

        *cursors = self.cursors.clone();

        let mut helper = EditHelper::new_inc(file, area, cursors, searcher);

        helper.move_each(|mut m| {
            let next = m.search_inc(None).next();
            if let Some((p0, p1)) = next {
                m.move_to(p0);
                m.set_anchor();
                m.move_to(p1);
                if m.is_incl() {
                    m.move_hor(-1);
                }
            } else if m.is_main() {
                area.set_print_info(self.info.clone());
            }
        });
    }
}

pub struct Rev<U: Ui> {
    cursors: Cursors,
    info: <U::Area as Area>::PrintInfo,
}

impl<U: Ui> IncSearcher<U> for Rev<U> {
    fn new(_file: &RwData<File>, area: &U::Area, cursors: &mut Cursors) -> Self {
        Self {
            cursors: cursors.clone(),
            info: area.get_print_info(),
        }
    }

    fn search(
        &mut self,
        file: &RwData<File>,
        area: &U::Area,
        searcher: Searcher,
        cursors: &mut Cursors,
    ) {
        if searcher.is_empty() {
            area.set_print_info(self.info.clone());
            return;
        }

        *cursors = self.cursors.clone();

        let mut helper = EditHelper::new_inc(file, area, cursors, searcher);

        helper.move_each(|mut m| {
            let next = m.search_inc_rev(None).next();
            if let Some((p0, p1)) = next {
                m.move_to(p0);
                m.set_anchor();
                m.move_to(p1);
                if m.is_incl() {
                    m.move_hor(-1);
                }
            } else if m.is_main() {
                area.set_print_info(self.info.clone());
            }
        });
    }
}
