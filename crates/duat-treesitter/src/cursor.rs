use duat_core::text::Strs;
use tree_sitter::{Node, TreeCursor};

#[allow(unused)]
pub struct Cursor<'a> {
    strs: &'a Strs,
    cursor: TreeCursor<'a>,
}

#[allow(unused)]
impl Cursor<'_> {
    pub fn node(&self) -> Node<'_> {
        self.cursor.node()
    }

    pub fn parent(&mut self) -> Option<Node<'_>> {
        self.cursor.goto_parent().then(|| self.cursor.node())
    }

    pub fn first_child(&mut self) -> Option<Node<'_>> {
        self.cursor.goto_first_child().then(|| self.cursor.node())
    }

    pub fn last_child(&mut self) -> Option<Node<'_>> {
        self.cursor.goto_last_child().then(|| self.cursor.node())
    }

    pub fn child_with_byte(&mut self, byte: usize) -> Option<(usize, Node<'_>)> {
        self.cursor
            .goto_first_child_for_byte(byte)
            .map(|i| (i, self.cursor.node()))
    }

    pub fn prev_sibling(&mut self) -> Option<Node<'_>> {
        self.cursor
            .goto_previous_sibling()
            .then(|| self.cursor.node())
    }

    pub fn next_sibling(&mut self) -> Option<Node<'_>> {
        self.cursor.goto_next_sibling().then(|| self.cursor.node())
    }

    pub fn goto_descendant(&mut self, index: usize) {
        self.cursor.goto_descendant(index);
    }
}
