use duat_core::{
    context::FileHandle,
    data::Pass,
    mode::EditHelper,
    text::{Searcher, Text, txt},
    ui::Ui,
};

pub trait IncSearcher<U: Ui>: Clone + 'static {
    fn search(&mut self, pa: &mut Pass, handle: FileHandle<U>, se: Searcher);

    fn prompt(&self) -> Text;
}

#[derive(Clone, Copy)]
pub struct SearchFwd;

impl<U: Ui> IncSearcher<U> for SearchFwd {
    fn search(&mut self, pa: &mut Pass, handle: FileHandle<U>, se: Searcher) {
        let mut helper = EditHelper::inc_from_handle(pa, handle, se);
        helper.edit_all(pa, |mut e| {
            let caret = e.caret();
            let next = e.search_inc_fwd(None).find(|[p, _]| *p != caret);
            if let Some([p0, p1]) = next {
                e.move_to(p0);
                if p1 > p0 {
                    e.set_anchor();
                    e.move_to(p1);
                    e.move_hor(-1);
                }
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[Prompt]search[Prompt.colon]:").build()
    }
}

#[derive(Clone, Copy)]
pub struct SearchRev;

impl<U: Ui> IncSearcher<U> for SearchRev {
    fn search(&mut self, pa: &mut Pass, handle: FileHandle<U>, se: Searcher) {
        let mut helper = EditHelper::inc_from_handle(pa, handle, se);
        helper.edit_all(pa, |mut e| {
            let caret = e.caret();
            let next = e.search_inc_rev(None).find(|[_, p]| *p != caret);
            if let Some([p0, p1]) = next {
                e.move_to(p0);
                if p1 > p0 {
                    e.set_anchor();
                    e.move_to(p1);
                    e.move_hor(-1);
                }
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[Prompt]rev search[Prompt.colon]:").build()
    }
}

#[derive(Clone, Copy)]
pub struct ExtendFwd;

impl<U: Ui> IncSearcher<U> for ExtendFwd {
    fn search(&mut self, pa: &mut Pass, handle: FileHandle<U>, se: Searcher) {
        let mut helper = EditHelper::inc_from_handle(pa, handle, se);
        helper.edit_all(pa, |mut e| {
            let caret = e.caret();
            let next = e.search_inc_fwd(None).find(|[p, _]| *p != caret);
            if let Some([_, p1]) = next {
                if e.anchor().is_none() {
                    e.set_anchor();
                }
                e.move_to(p1);
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[Prompt]search (extend)[Prompt.colon]:").build()
    }
}

#[derive(Clone, Copy)]
pub struct ExtendRev;

impl<U: Ui> IncSearcher<U> for ExtendRev {
    fn search(&mut self, pa: &mut Pass, handle: FileHandle<U>, se: Searcher) {
        let mut helper = EditHelper::inc_from_handle(pa, handle, se);
        helper.edit_all(pa, |mut e| {
            let caret = e.caret();
            let next = e.search_inc_rev(None).find(|[_, p]| *p != caret);
            if let Some([p0, _]) = next {
                if e.anchor().is_none() {
                    e.set_anchor();
                }
                e.move_to(p0);
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[Prompt]rev search (extend)[Prompt.colon]:").build()
    }
}
