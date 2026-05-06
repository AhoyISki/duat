use std::ops::Range;

struct Snippets {
    list: Vec<Snippets>
}

struct Snippet {
    jumps: Vec<Vec<Range<usize>>>,
}

enum Bullshit {
    asdfassdfasdf
}
