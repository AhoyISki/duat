use duat_base::modes::IncSearcher;
use duat_core::{
    buffer::Buffer,
    context::Handle,
    data::Pass,
    text::{Text, txt},
};

/// Selects matches from within every selection
#[derive(Clone, Copy)]
pub(crate) struct Select;

impl IncSearcher for Select {
    fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>) {
        handle.edit_all(pa, |mut s| {
            s.set_cursor_on_start();
            let Some(anchor) = s.anchor() else { return };

            let range = s.cursor()..anchor;
            let ranges: Vec<_> = s.search(pat).range(range).collect();

            for (i, range) in ranges.iter().enumerate() {
                s.move_to(range.clone());
                if i < ranges.len() - 1 {
                    s.copy();
                }
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]select")
    }
}

/// Splits selections on every selection
#[derive(Clone, Copy)]
pub(crate) struct Split;

impl IncSearcher for Split {
    fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>) {
        handle.edit_all(pa, |mut s| {
            let range = s.range();
            if range.end.byte() - range.start.byte() <= 1 {
                return;
            }

            let ranges: Vec<usize> = s
                .search(pat)
                .range(range.clone())
                .flat_map(|r| [r.start, r.end])
                .collect();

            let mut iter = [range.start.byte()]
                .into_iter()
                .chain(ranges)
                .chain([range.end.byte()]);

            while let Some(b0) = iter.next()
                && let Some(b1) = iter.next()
                && b0 != range.end.byte()
            {
                s.move_to(b0..b1);
                s.copy();
            }

            s.destroy();
        })
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]split")
    }
}

/// Keeps/removes only the selections that match the predicate
#[derive(Clone, Copy)]
pub(crate) struct KeepMatching(pub bool);

impl IncSearcher for KeepMatching {
    fn search(&mut self, pa: &mut Pass, pat: &str, handle: Handle<Buffer>) {
        let keep = self.0;

        handle.edit_all(pa, |mut s| {
            s.set_cursor_on_start();
            let range = s.range();
            if s.search(pat).range(range).next().is_some() != keep {
                s.destroy();
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
