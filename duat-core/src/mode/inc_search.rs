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
        cursors: &mut Cursors,
        searcher: Searcher,
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
            info: area.print_info(),
        }
    }

    fn search(
        &mut self,
        file: &RwData<File>,
        area: &U::Area,
        cursors: &mut Cursors,
        searcher: Searcher,
    ) {
        *cursors = self.cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(self.info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, cursors, searcher);

        helper.move_many(.., |mut m| {
            let caret = m.caret();
            let next = m.search_inc_fwd(None).find(|(p, _)| *p != caret);
            if let Some((p0, p1)) = next {
                m.move_to(p0);
                if p1 > p0 {
                    m.set_anchor();
                    m.move_to(p1);
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
            info: area.print_info(),
        }
    }

    fn search(
        &mut self,
        file: &RwData<File>,
        area: &U::Area,
        cursors: &mut Cursors,
        searcher: Searcher,
    ) {
        *cursors = self.cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(self.info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, cursors, searcher);

        helper.move_many(.., |mut m| {
            let caret = m.caret();
            let next = m.search_inc_rev(None).find(|(_, p)| *p != caret);
            if let Some((p0, p1)) = next {
                m.move_to(p0);
                if p1 > p0 {
                    m.set_anchor();
                    m.move_to(p1);
                    m.move_hor(-1);
                }
            } else if m.is_main() {
                area.set_print_info(self.info.clone());
            }
        });
    }
}

pub struct ExtendFwd<U: Ui> {
    cursors: Cursors,
    info: <U::Area as Area>::PrintInfo,
}

impl<U: Ui> IncSearcher<U> for ExtendFwd<U> {
    fn new(_file: &RwData<File>, area: &U::Area, cursors: &mut Cursors) -> Self {
        Self {
            cursors: cursors.clone(),
            info: area.print_info(),
        }
    }

    fn search(
        &mut self,
        file: &RwData<File>,
        area: &U::Area,
        cursors: &mut Cursors,
        searcher: Searcher,
    ) {
        *cursors = self.cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(self.info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, cursors, searcher);

        helper.move_many(.., |mut m| {
            let caret = m.caret();
            let next = m.search_inc_fwd(None).find(|(p, _)| *p != caret);
            if let Some((_, p1)) = next {
                if m.anchor().is_none() {
                    m.set_anchor();
                }
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

pub struct ExtendRev<U: Ui> {
    cursors: Cursors,
    info: <U::Area as Area>::PrintInfo,
}

impl<U: Ui> IncSearcher<U> for ExtendRev<U> {
    fn new(_file: &RwData<File>, area: &U::Area, cursors: &mut Cursors) -> Self {
        Self {
            cursors: cursors.clone(),
            info: area.print_info(),
        }
    }

    fn search(
        &mut self,
        file: &RwData<File>,
        area: &U::Area,
        cursors: &mut Cursors,
        searcher: Searcher,
    ) {
        *cursors = self.cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(self.info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, cursors, searcher);

        helper.move_many(.., |mut m| {
            let caret = m.caret();
            let next = m.search_inc_rev(None).find(|(_, p)| *p != caret);
            if let Some((p0, _)) = next {
                if m.anchor().is_none() {
                    m.set_anchor();
                }
                m.move_to(p0);
            } else if m.is_main() {
                area.set_print_info(self.info.clone());
            }
        });
    }
}
