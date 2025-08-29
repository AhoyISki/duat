use std::ops::Range;

use duat_core::{
    form::FormId,
    prelude::Ranges,
    text::{Bytes, Tags},
};
use tree_sitter::{InputEdit, Node, Parser, Point as TsPoint, Query, Range as TsRange, Tree};

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
    ranges_to_parse: Ranges,
}

impl InjectedTree {
    /// Returns a new [`InjectedTree`] with an initial [`Range`]
    pub(crate) fn new(bytes: &Bytes, lang_parts: LangParts<'static>, ranges: Ranges) -> Self {
        let (.., lang, _) = &lang_parts;
        let forms = forms_from_lang_parts(lang_parts);

        let included_ranges: Vec<TsRange> = ranges
            .iter()
            .map(|range| ts_range_from_range(bytes, range))
            .collect();

        let mut parser = Parser::new();
        parser.set_language(lang).unwrap();
        parser.set_included_ranges(&included_ranges).unwrap();

        let tree = parser
            .parse_with_options(&mut parser_fn(bytes), None, None)
            .unwrap();

        Self {
            parser,
            lang_parts,
            forms,
            tree,
            old_tree: None,
            ranges,
            injections: Vec::new(),
            ranges_to_parse: Ranges::empty(),
        }
    }

    /// Edits the [`Tree`] within
    pub(crate) fn edit(&mut self, edit: &InputEdit) {
        let byte_diff = edit.new_end_byte as i32 - edit.old_end_byte as i32;
        self.ranges.shift_by(edit.start_byte, byte_diff);
        self.ranges_to_parse.shift_by(edit.start_byte, byte_diff);

        for range in self.ranges.iter() {
            if range.contains(&edit.start_byte) || range.contains(&edit.new_end_byte) {
                self.ranges_to_parse.add(range);
            }
        }

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
        let ranges: Vec<TsRange> = std::mem::take(&mut self.ranges_to_parse)
            .into_iter()
            .map(|range| ts_range_from_range(bytes, range))
            .collect();

        if !ranges.is_empty() {
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
    }

    /// Adds another [`Range`] to be parsed with this [`InjectedTree`]
    ///
    /// In addition, this will also mark this [`Range`] as one to be
    /// parsed
    pub(crate) fn add_range(&mut self, range: Range<usize>) {
        self.ranges.add(range.clone());
        self.ranges_to_parse.add(range);
    }

    /// Removes a [`Range`] from the list of parsed [`Range`]s
    pub(crate) fn remove_range(&mut self, range: Range<usize>) {
        if self.ranges.iter().any(|r| r == range) {
            let _ = self.ranges.remove(range.clone());
            let _ = self.ranges_to_parse.remove(range.clone());

            for inj in self.injections.iter_mut() {
                inj.remove_range(range.clone());
            }
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
            tags.remove(tagger, range.clone());
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

    /// The [`LangParts`] used by this [`InjectedTree`]
    pub(crate) fn lang_parts(&self) -> LangParts<'static> {
        self.lang_parts
    }

    /// Gets the parts of the [`InjectedTree`] needed for indentation
    pub(crate) fn get_injection_indent_parts(
        &self,
        start_byte: usize,
    ) -> Option<(Node<'_>, &'static Query, Range<usize>)> {
        crate::context::debug!("{self.ranges:#?}, {start_byte}");
        if let Some(range) = self.ranges.iter().find(|r| r.contains(&start_byte)) {
            self.injections
                .iter()
                .find_map(|inj| inj.get_injection_indent_parts(start_byte))
                .or(Some((
                    self.tree
                        .root_node()
                        .descendant_for_byte_range(range.start, range.end)
                        .unwrap(),
                    self.lang_parts.2.injections,
                    range.clone(),
                )))
        } else {
            None
        }
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
