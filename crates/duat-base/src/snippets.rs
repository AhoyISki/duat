use std::ops::Range;

use duat_core::buffer::PerBuffer;

static SNIPPETS: PerBuffer<Snippets> = PerBuffer::new();

struct Snippets {
    list: Vec<Snippets>
}

struct Snippet {
    jumps: Vec<Vec<Range<usize>>>,
}
