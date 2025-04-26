use duat_core::{
    Lender,
    mode::{Cursors, EditHelper},
    text::{Searcher, Text, text},
    ui::{RawArea, Ui},
    widgets::File,
};

pub trait IncSearcher<U: Ui>: Clone + Send + Sync + 'static {
    fn search(&mut self, orig: &Orig<U>, file: &mut File, area: &U::Area, searcher: Searcher);

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
        helper.edit_iter().for_each(|mut e| {
            let caret = e.caret();
            let next = e.search_inc_fwd(None).find(|[p, _]| *p != caret);
            if let Some([p0, p1]) = next {
                e.move_to(p0);
                if p1 > p0 {
                    e.set_anchor();
                    e.move_to(p1);
                    e.move_hor(-1);
                }
            } else if e.is_main() {
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
        helper.edit_iter().for_each(|mut e| {
            let caret = e.caret();
            let next = e.search_inc_rev(None).find(|[_, p]| *p != caret);
            if let Some([p0, p1]) = next {
                e.move_to(p0);
                if p1 > p0 {
                    e.set_anchor();
                    e.move_to(p1);
                    e.move_hor(-1);
                }
            } else if e.is_main() {
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
        helper.edit_iter().for_each(|mut e| {
            let caret = e.caret();
            let next = e.search_inc_fwd(None).find(|[p, _]| *p != caret);
            if let Some([_, p1]) = next {
                if e.anchor().is_none() {
                    e.set_anchor();
                }
                e.move_to(p1);
            } else if e.is_main() {
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
        helper.edit_iter().for_each(|mut e| {
            let caret = e.caret();
            let next = e.search_inc_rev(None).find(|[_, p]| *p != caret);
            if let Some([p0, _]) = next {
                if e.anchor().is_none() {
                    e.set_anchor();
                }
                e.move_to(p0);
            } else if e.is_main() {
                area.set_print_info(info.clone());
            }
        });
    }

    fn prompt(&self) -> Text {
        text!([Prompt] "rev search (extend)" [Prompt.colon] ":")
    }
}

#[allow(type_alias_bounds)]
pub type Orig<U: Ui> = (Cursors, <U::Area as RawArea>::PrintInfo);
