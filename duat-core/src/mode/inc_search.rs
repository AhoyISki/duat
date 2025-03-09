use super::{Cursors, EditHelper};
use crate::{
    text::{Searcher, Text, text},
    ui::{Area, Ui},
    widgets::File,
};

pub trait IncSearcher<U: Ui>: Clone + Send + Sync + 'static {
    fn search(&mut self, orig: &Orig<U>, file: &mut File, area: &U::Area, searcher: Searcher);

    #[allow(unused)]
    fn finish(&mut self, orig: &Orig<U>, file: &mut File, area: &U::Area) {}

    fn prompt(&self) -> Text;
}

#[derive(Clone, Copy)]
pub struct SearchFwd;

impl<U: Ui> IncSearcher<U> for SearchFwd {
    fn search(&mut self, orig: &Orig<U>, file: &mut File, area: &U::Area, searcher: Searcher) {
        let (cursors, info) = orig;
        *file.cursors_mut().unwrap() = cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, searcher);

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
                area.set_print_info(info.clone());
            }
        });
    }

    fn prompt(&self) -> Text {
        text!([Prompt] "search" [Prompt.colon] ":")
    }
}

#[derive(Clone, Copy)]
pub struct SearchRev;

impl<U: Ui> IncSearcher<U> for SearchRev {
    fn search(&mut self, orig: &Orig<U>, file: &mut File, area: &U::Area, searcher: Searcher) {
        let (cursors, info) = orig;
        *file.cursors_mut().unwrap() = cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, searcher);

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
                area.set_print_info(info.clone());
            }
        });
    }

    fn prompt(&self) -> Text {
        text!([Prompt] "rev search" [Prompt.colon] ":")
    }
}

#[derive(Clone, Copy)]
pub struct ExtendFwd;

impl<U: Ui> IncSearcher<U> for ExtendFwd {
    fn search(&mut self, orig: &Orig<U>, file: &mut File, area: &U::Area, searcher: Searcher) {
        let (cursors, info) = orig;
        *file.cursors_mut().unwrap() = cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, searcher);

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
                area.set_print_info(info.clone());
            }
        });
    }

    fn prompt(&self) -> Text {
        text!([Prompt] "search (extend)" [Prompt.colon] ":")
    }
}

#[derive(Clone, Copy)]
pub struct ExtendRev;

impl<U: Ui> IncSearcher<U> for ExtendRev {
    fn search(&mut self, orig: &Orig<U>, file: &mut File, area: &U::Area, searcher: Searcher) {
        let (cursors, info) = orig;
        *file.cursors_mut().unwrap() = cursors.clone();
        if searcher.is_empty() {
            area.set_print_info(info.clone());
            return;
        }

        let mut helper = EditHelper::new_inc(file, area, searcher);

        helper.move_many(.., |mut m| {
            let caret = m.caret();
            let next = m.search_inc_rev(None).find(|(_, p)| *p != caret);
            if let Some((p0, _)) = next {
                if m.anchor().is_none() {
                    m.set_anchor();
                }
                m.move_to(p0);
            } else if m.is_main() {
                area.set_print_info(info.clone());
            }
        });
    }

    fn prompt(&self) -> Text {
        text!([Prompt] "rev search (extend)" [Prompt.colon] ":")
    }
}

#[allow(type_alias_bounds)]
pub type Orig<U: Ui> = (Cursors, <U::Area as Area>::PrintInfo);
