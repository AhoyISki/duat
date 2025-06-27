use duat_core::text::Bytes;
use tree_sitter::{Node, QueryCursor, TreeCursor};

use crate::TsCursor;

pub struct Cursor<'a> {
    bytes: &'a Bytes,
    cursor: TreeCursor<'a>,
}

impl Cursor<'_> {
    pub fn node(&self) -> Node {
        self.cursor.node()
    }

    pub fn parent(&mut self) -> Option<Node> {
        self.cursor.goto_parent().then(|| self.cursor.node())
    }

    pub fn first_child(&mut self) -> Option<Node> {
        self.cursor.goto_first_child().then(|| self.cursor.node())
    }

    pub fn last_child(&mut self) -> Option<Node> {
        self.cursor.goto_last_child().then(|| self.cursor.node())
    }

    pub fn child_with_byte(&mut self, byte: usize) -> Option<(usize, Node)> {
        self.cursor
            .goto_first_child_for_byte(byte)
            .map(|i| (i, self.cursor.node()))
    }

    pub fn prev_sibling(&mut self) -> Option<Node> {
        self.cursor
            .goto_previous_sibling()
            .then(|| self.cursor.node())
    }

    pub fn next_sibling(&mut self) -> Option<Node> {
        self.cursor.goto_next_sibling().then(|| self.cursor.node())
    }

    pub fn goto_descendant(&mut self, index: usize) {
        self.cursor.goto_descendant(index);
    }
}
