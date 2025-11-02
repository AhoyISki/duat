use duat::{prelude::*, text::Searcher};
use duat::mode::IncSearcher;

#[derive(Clone, Copy)]
pub(crate) struct Select;

impl IncSearcher for Select {
    fn search(&mut self, pa: &mut Pass, handle: Handle<Buffer, Searcher>) {
        handle.edit_all(pa, |mut c| {
            c.set_caret_on_start();
            if let Some(anchor) = c.anchor() {
                let ranges: Vec<[text::Point; 2]> = c.search_inc_fwd(Some(anchor)).collect();

                for (i, &[p0, p1]) in ranges.iter().enumerate() {
                    c.move_to(p0);
                    if p1.char() > p0.char() + 1 {
                        c.set_anchor();
                        c.move_to(p1);
                        c.move_hor(-1);
                    } else {
                        c.unset_anchor();
                    }
                    if i < ranges.len() - 1 {
                        c.copy();
                    }
                }
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]select")
    }
}

#[derive(Clone, Copy)]
pub(crate) struct Split;

impl IncSearcher for Split {
    fn search(&mut self, pa: &mut Pass, handle: Handle<Buffer, Searcher>) {
        handle.edit_all(pa, |mut c| {
            c.set_caret_on_start();
            if let Some(anchor) = c.anchor() {
                let ranges: Vec<text::Point> = c.search_inc_fwd(Some(anchor)).flatten().collect();
                let cursors_to_add = ranges.len() / 2 + 1;
                let iter = [c.caret()]
                    .into_iter()
                    .chain(ranges)
                    .chain([anchor])
                    .array_chunks();

                for (i, [p0, p1]) in iter.enumerate() {
                    c.move_to(p0);
                    if p1.char() > p0.char() + 1 {
                        c.set_anchor();
                        c.move_to(p1);
                        c.move_hor(-1);
                    } else if p1 > p0 {
                        c.unset_anchor();
                    } else {
                        continue;
                    }
                    if i < cursors_to_add {
                        c.copy();
                    }
                }
            }
        })
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]split")
    }
}
