use std::ops::Range;

use duat_core::{
    form::FormId,
    prelude::Ranges,
    text::{Bytes, Tags},
};
use tree_sitter::{InputEdit, Node, Parser, Point as TsPoint, Range as TsRange, Tree};

use crate::{
    LangParts, forms_from_lang_parts, highlight_and_inject, parser_fn, refactor_injections,
    ts_tagger,
};

/// An injected [`Tree`], which can contain any number of [`Range`]s
pub struct InjectedTree {
    parser: Parser,
    lang_parts: LangParts<'static>,
    forms: &'static [(FormId, u8)],
    tree: Tree,
    old_tree: Option<Tree>,
    ranges: Ranges,
    injections: Vec<InjectedTree>,
    has_changes: bool,
}

impl InjectedTree {
    /// Returns a new [`InjectedTree`] with an initial [`Range`]
    pub(crate) fn new(bytes: &Bytes, lang_parts: LangParts<'static>, range: Range<usize>) -> Self {
        let (.., lang, _) = &lang_parts;
        let forms = forms_from_lang_parts(lang_parts);

        let mut parser = Parser::new();
        parser.set_language(lang).unwrap();
        parser
            .set_included_ranges(&[ts_range_from_range(bytes, range.clone())])
            .unwrap();

        let tree = parser
            .parse_with_options(&mut parser_fn(bytes), None, None)
            .unwrap();

        Self {
            parser,
            lang_parts,
            forms,
            tree,
            old_tree: None,
            ranges: Ranges::new(range),
            injections: Vec::new(),
            has_changes: true,
        }
    }

    /// Edits the [`Tree`] within
    pub(crate) fn edit(&mut self, edit: &InputEdit) {
        self.has_changes = true;
        self.ranges.shift_by(
            edit.start_byte,
            edit.new_end_byte as i32 - edit.old_end_byte as i32,
        );
        self.tree.edit(edit);

        for inj in self.injections.iter_mut() {
            inj.edit(edit);
        }
    }

    /// Updates the [`Tree`] within, should be done after applying a
    /// bunch of [`Change`]s
    ///
    /// [`Change`]: duat_core::text::Change
    pub(crate) fn update_tree(&mut self, bytes: &Bytes) {
        if self.has_changes {
            let ranges: Vec<TsRange> = self
                .ranges
                .iter()
                .map(|range| ts_range_from_range(bytes, range))
                .collect();
            self.parser.set_included_ranges(&ranges).unwrap();

            let tree = self
                .parser
                .parse_with_options(&mut parser_fn(bytes), Some(&self.tree), None)
                .unwrap();
            self.old_tree = Some(std::mem::replace(&mut self.tree, tree));

            for inj in self.injections.iter_mut() {
                inj.update_tree(bytes);
            }
        }
        self.has_changes = false;
    }

    /// Adds another [`Range`] to be parsed with this [`InjectedTree`]
    ///
    /// Returns `true` if the [`Range`] was added, `false` if it
    /// was already in the list.
    pub(crate) fn add_range(&mut self, range: Range<usize>) {
        if !self.ranges.iter().any(|r| r == range) {
            self.has_changes = true;
            self.ranges.add(range);
        }
    }

    /// Removes a [`Range`] from the list of parsed [`Range`]s
    pub(crate) fn remove_range(&mut self, range: Range<usize>) {
        self.has_changes = true;
        let _ = self.ranges.remove(range.clone());

        for inj in self.injections.iter_mut() {
            inj.remove_range(range.clone());
        }
    }

    /// Highlights and injects based on the [`LangParts`] queries
    pub(crate) fn highlight_and_inject(
        &mut self,
        bytes: &Bytes,
        tags: &mut Tags,
        range: Range<usize>,
    ) {
        let tagger = ts_tagger();
        for range in self.ranges.iter_over(range.clone()) {
            tags.remove(tagger, range);
        }

        highlight_and_inject(
            self.tree.root_node(),
            &mut self.injections,
            (self.lang_parts, self.forms),
            (bytes, tags),
            range,
        );
    }

    /// Injects on the given [`Ranges`], incrementing these ranges to
    /// include changes
    pub(crate) fn refactor_injections(&mut self, ranges: &mut Ranges, bytes: &Bytes) {
        refactor_injections(
            ranges,
            (self.lang_parts, &mut self.injections),
            (self.old_tree.as_ref(), &self.tree),
            bytes,
        );
    }

    ////////// Post change queries

    /// The changed [`Range`]s of this injected tree
    ///
    /// [`Range`]: TsRange
    pub(crate) fn changed_ranges(&self) -> impl Iterator<Item = TsRange> + '_ {
        self.old_tree.iter().flat_map(|old_tree| {
            let inj_changed_ranges: Vec<TsRange> = self
                .injections
                .iter()
                .flat_map(|inj| inj.changed_ranges())
                .collect();

            old_tree
                .changed_ranges(&self.tree)
                .chain(inj_changed_ranges)
        })
    }

    ////////// Querying functions

    /// Wether there are any [`Range`]s included in this
    /// [`InjectedTree`]
    pub(crate) fn is_empty(&self) -> bool {
        self.ranges.is_empty()
    }

    /// The ranges included in this [`InjectedTree`]
    pub(crate) fn included_ranges(&self) -> impl Iterator<Item = Range<usize>> {
        self.ranges.iter()
    }

    /// The root [`Node`] of the [`Tree`] within
    pub(crate) fn root_node(&self) -> Node<'_> {
        self.tree.root_node()
    }

    /// The [`LangParts`] used by this [`InjectedTree`]
    pub(crate) fn lang_parts(&self) -> LangParts<'static> {
        self.lang_parts
    }
}

impl std::fmt::Debug for InjectedTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TsParser")
            .field("tree", &self.tree)
            .field("old_tree", &self.old_tree)
            .field("ranges", &self.ranges)
            .finish_non_exhaustive()
    }
}

fn ts_range_from_range(bytes: &Bytes, range: Range<usize>) -> TsRange {
    TsRange {
        start_byte: range.start,
        end_byte: range.end,
        start_point: TsPoint {
            row: bytes.point_at_byte(range.start).line(),
            column: bytes
                .chars_rev(..range.start)
                .unwrap()
                .take_while(|(_, c)| *c != '\n')
                .count(),
        },
        end_point: TsPoint {
            row: bytes.point_at_byte(range.end).line(),
            column: bytes
                .chars_rev(..range.end)
                .unwrap()
                .take_while(|(_, c)| *c != '\n')
                .count(),
        },
    }
}
