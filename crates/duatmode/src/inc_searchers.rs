use duat_base::modes::IncSearcher;
use duat_core::{
    buffer::Buffer,
    context::Handle,
    data::Pass,
    text::{Text, txt},
};

/// Selects matches from within every cursor
#[derive(Clone, Copy)]
pub(crate) struct Select;

impl IncSearcher for Select {
    fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>) {
        handle.edit_all(pa, |mut c| {
            c.set_caret_on_start();
            let Some(anchor) = c.anchor() else { return };

            let range = c.caret()..anchor;
            let ranges: Vec<_> = c.search(pat).range(range).collect();

            for (i, range) in ranges.iter().enumerate() {
                c.move_to(range.clone());
                if i < ranges.len() - 1 {
                    c.copy();
                }
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]select")
    }
}

/// Splits selections on every cursor
#[derive(Clone, Copy)]
pub(crate) struct Split;

impl IncSearcher for Split {
    fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>) {
        handle.edit_all(pa, |mut c| {
            let range = c.range();
            if range.is_empty() {
                return;
            }

            let ranges: Vec<usize> = c
                .search(pat)
                .range(range.clone())
                .flat_map(|r| [r.start, r.end])
                .collect();

            duat_core::debug!("{ranges:#?}");

            let mut iter = [range.start.byte()]
                .into_iter()
                .chain(ranges)
                .chain([range.end.byte()]);

            while let Some(p0) = iter.next()
                && let Some(p1) = iter.next()
            {
                duat_core::debug!("inserting on {p0:?}, {p1:?}");
                c.move_to(p0..p1);
                c.copy();
            }

            c.destroy();
        })
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]split")
    }
}

/// Keeps/removes only the cursors that match the predicate
#[derive(Clone, Copy)]
pub(crate) struct KeepMatching(pub bool);

impl IncSearcher for KeepMatching {
    fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>) {
        let keep = self.0;

        handle.edit_all(pa, |mut c| {
            c.set_caret_on_start();
            let range = c.range();
            if c.search(pat).range(range).next().is_some() != keep {
                c.destroy();
            }
        });
    }

    fn prompt(&self) -> Text {
        match self.0 {
            true => txt!("[prompt]keep matching"),
            false => txt!("[prompt]keep not matching"),
        }
    }
}
