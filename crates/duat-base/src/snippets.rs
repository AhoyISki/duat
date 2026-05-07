use std::ops::Range;

use duat_core::{buffer::PerBuffer, context::Handle, data::Pass};

static SNIPPETS: PerBuffer<Snippets> = PerBuffer::new();

struct Snippets {
    list: Vec<Snippets>
}

struct Snippet {
    jumps: Vec<Vec<Range<usize>>>,
}

pub(crate) fn add_snippet(buffer: &Handle, pa: &mut Pass) {
}
