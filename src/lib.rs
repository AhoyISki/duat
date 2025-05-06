//! A [tree-sitter] implementation for Duat
//!
//! `duat-treesitter` currently does two things:
//!
//! * Syntax highlighting
//! * Indentation calculation
//!
//! Both of those things are done through the [`TsParser`] reader,
//! with the [`PubTsParser`] public API.
//!
//! [tree-sitter]: https://tree-sitter.github.io/tree-sitter
use std::{
    collections::HashMap,
    fs,
    marker::PhantomData,
    ops::Range,
    path::{Path, PathBuf},
    sync::LazyLock,
};

use duat_core::{
    Mutex, add_shifts,
    cfg::PrintCfg,
    form::{self, Form, FormId},
    hooks::{self, OnFileOpen},
    text::{
        Bytes, Change, Key, Matcheable, MutTags, Point, Reader, ReaderCfg, Tag, Text,
        merge_range_in,
    },
};
use duat_filetype::FileType;
use streaming_iterator::StreamingIterator;
use tree_sitter::{
    InputEdit, Language, Node, Parser, Point as TSPoint, Query, QueryCapture as QueryCap,
    QueryCursor, TextProvider, Tree,
};

mod languages;

/// The [tree-sitter] pluging for Duat
///
/// For now, it adds syntax highlighting and indentation, but more
/// features will be coming in the future.
///
/// These things are done through the [`TsParser`] [`Reader`], which
/// reads updates the inner syntax tree when the [`Text`] reports any
/// changes.
///
/// # NOTE
///
/// If you are looking to create a [`Reader`] which can do similar
/// things, you should look at the code for the implementation of
/// [`Reader`] for [`TsParser`], it's relatively short and with good
/// explanations for what is happening.
///
/// [tree-sitter]: https://tree-sitter.github.io/tree-sitter
pub struct TreeSitter<U>(PhantomData<U>);

impl<U: duat_core::ui::Ui> duat_core::Plugin<U> for TreeSitter<U> {
    fn new() -> Self {
        Self(PhantomData)
    }

    fn plug(self) {
        dlopen_rs::init();

        form::set_many!(
            ("variable", Form::white()),
            ("variable.parameter", Form::italic()),
            ("variable.builtin", Form::dark_yellow()),
            ("constant", Form::grey()),
            ("constant.builtin", Form::dark_yellow()),
            ("module", Form::blue().italic()),
            ("label", Form::green()),
            ("string", Form::green()),
            ("character", Form::dark_yellow()),
            ("boolean", Form::dark_yellow()),
            ("number", Form::dark_yellow()),
            ("type", Form::yellow().italic()),
            ("type.builtin", Form::yellow().reset()),
            ("attribute", Form::green()),
            ("property", Form::green()),
            ("function", Form::blue().reset()),
            ("constructor", Form::dark_yellow().reset()),
            ("operator", Form::cyan()),
            ("keyword", Form::magenta()),
            ("punctuation.bracket", Form::red()),
            ("punctuation.delimiter", Form::cyan()),
            ("comment", Form::grey()),
            ("comment.documentation", Form::grey().bold()),
            ("markup.strong", Form::bold()),
            ("markup.italic", Form::italic()),
            ("markup.strikethrough", Form::crossed_out()),
            ("markup.underline", Form::underlined()),
            ("markup.heading", Form::blue().bold()),
            ("markup.math", Form::yellow()),
            ("markup.quote", Form::grey().italic()),
            ("markup.link", Form::blue().underlined()),
            ("markup.raw", Form::cyan()),
            ("markup.list", Form::yellow()),
            ("markup.list.checked", Form::green()),
            ("markup.list.unchecked", Form::grey()),
        );

        hooks::add_grouped::<OnFileOpen<U>>("TreeSitter", |builder| {
            let (file, _) = builder.read();
            let filetype = file.filetype();

            if let Some(filetype) = filetype
                && let Some(ts_parser_cfg) = TsParser::cfg(filetype)
            {
                drop(file);
                let _ = builder.add_reader(ts_parser_cfg);
            }
        });
    }
}

pub struct TsParser {
    parser: Parser,
    range: Range<usize>,
    offset: TSPoint,
    lang_parts: LangParts<'static>,
    forms: &'static [(FormId, u8)],
    tree: Tree,
    old_tree: Option<Tree>,
    sub_trees: Vec<TsParser>,
    key: Key,
}

impl TsParser {
    fn init(
        bytes: &mut Bytes,
        tags: &mut MutTags,
        range: Range<usize>,
        offset: TSPoint,
        lang_parts: LangParts<'static>,
        forms: &'static [(FormId, u8)],
    ) -> TsParser {
        let (.., lang, _) = &lang_parts;

        let mut parser = Parser::new();
        parser.set_language(lang).unwrap();

        let tree = parser
            .parse_with_options(&mut buf_parse(bytes, range.clone()), None, None)
            .unwrap();

        let mut parser = TsParser {
            parser,
            range: range.clone(),
            offset,
            lang_parts,
            forms,
            tree,
            old_tree: None,
            sub_trees: Vec::new(),
            key: ts_key(),
        };

        parser.highlight_and_inject(bytes, tags, range);

        parser
    }

    fn highlight_and_inject(&mut self, bytes: &mut Bytes, tags: &mut MutTags, range: Range<usize>) {
        if range.start >= self.range.end || range.end <= self.range.start {
            return;
        }

        let (.., Queries { highlights: highlight, injections, .. }) = &self.lang_parts;
        let buf = TsBuf(bytes);

        tags.remove(range.clone(), self.key);

        // Include a little bit of overhang, in order to deal with some loose
        // ends, mostly related to comments.
        // There should be no tag duplication, since Duat does not allow that.
        let start = range.start.saturating_sub(1).max(self.range.start);
        let end = (range.end + 1).min(bytes.len().byte()).min(self.range.end);
        let mut cursor = QueryCursor::new();
        cursor.set_byte_range(start..end);
        let root = self
            .tree
            .root_node_with_offset(self.range.start, self.offset);

        let sub_trees_to_add = {
            type SubTreeToAdd = (Range<usize>, TSPoint, LangParts<'static>);
            let mut to_add: Vec<SubTreeToAdd> = Vec::new();

            let mut inj_captures = cursor.captures(injections, root, buf);
            let cn = injections.capture_names();

            let is_content = |cap: &&QueryCap| cn[cap.index as usize] == "injection.content";
            let is_language = |cap: &&QueryCap| cn[cap.index as usize] == "injection.language";

            while let Some((qm, _)) = inj_captures.next() {
                let Some(cap) = qm.captures.iter().find(is_content) else {
                    continue;
                };

                let props = injections.property_settings(qm.pattern_index);
                let lang = props
                    .iter()
                    .find_map(|p| {
                        (p.key.as_ref() == "injection.language")
                            .then_some(p.value.as_ref().unwrap().to_string())
                    })
                    .or_else(|| {
                        let cap = qm.captures.iter().find(is_language)?;
                        Some(bytes.strs(cap.node.byte_range()).to_string())
                    });

                let range = cap.node.byte_range();
                let key_fn = |(range, ..): &(Range<usize>, _, _)| range.start;
                if let Some(lang) = lang
                    && let Some(mut lang_parts) = lang_parts(&lang)
                    && let Err(i) = to_add.binary_search_by_key(&range.start, key_fn)
                {
                    let start = cap.node.start_position();

                    // You may want to set a new injections query, only for this capture.
                    if let Some(prop) = props.iter().find(|p| p.key.as_ref() == "injection.query")
                        && let Some(value) = prop.value.as_ref()
                        && let Some(injections) =
                            query_from_path(&format!("{lang}/{value}"), lang_parts.1)
                    {
                        lang_parts.2.injections = injections;
                    }

                    to_add.insert(i, (range, start, lang_parts));
                }
            }

            to_add
        };

        // If a tree was not in sub_trees_to_add, but is part of the affected
        // range, that means it was removed.
        self.sub_trees.retain_mut(|st| {
            if let Some((.., lp)) = sub_trees_to_add.iter().find(|(lhs, ..)| *lhs == st.range) {
                if lp.0 != st.lang_parts.0 {
                    if !(st.range.start >= start && st.range.end <= end) {
                        tags.remove(st.range.clone(), self.key);
                    }
                    false
                } else {
                    st.highlight_and_inject(bytes, tags, st.range.clone());
                    true
                }
            // If the sub tree was not found, but its range was
            // parsed, it was deleted
            // Unless it is a non-empty duat-text sub tree, in which
            // case this rule is ignored
            } else if st.range.start >= start && st.range.end <= end {
                false
            } else {
                st.highlight_and_inject(bytes, tags, range.clone());
                true
            }
        });

        // In the end, we add the sub trees that weren't already in there.
        // This should automatically handle all of the sub trees's sub trees.
        for (range, offset, lang_parts) in sub_trees_to_add {
            let key_fn = |st: &TsParser| st.range.start;

            let Err(i) = self.sub_trees.binary_search_by_key(&range.start, key_fn) else {
                continue;
            };

            let form_parts = forms_from_lang_parts(&lang_parts);
            let st = TsParser::init(bytes, tags, range, offset, lang_parts, form_parts);
            self.sub_trees.insert(i, st)
        }

        // We highlight at the very end, so if, for example, a sub tree gets
        // removed, tags can be readded, without leaving a blank space, in
        // case the injection was of the same language.
        let buf = TsBuf(bytes);
        cursor.set_byte_range(start..end);
        let mut hi_captures = cursor.captures(highlight, root, buf);
        while let Some((qm, _)) = hi_captures.next() {
            for cap in qm.captures.iter() {
                let range = cap.node.range();
                let (start, end) = (range.start_byte, range.end_byte);
                let (form, priority) = self.forms[cap.index as usize];
                if start != end {
                    tags.insert(self.key, Tag::form(start..end, form, priority));
                }
            }
        }
    }

    fn cfg(filetype: &str) -> Option<TsParserCfg> {
        TsParserCfg::new(filetype)
    }

    fn apply_sub_tree_change(&mut self, change: Change<&str>, bytes: &mut Bytes) {
        let start = change.start();
        let added = change.added_end();
        let taken = change.taken_end();

        // By this point, if this tree were to be clipped by the change, it
        // would have been removed already.
        if start.byte() < self.range.start && taken.byte() <= self.range.start {
            let ts_start = ts_point(start, bytes);
            let ts_taken_end = ts_point_from(taken, (ts_start.column, start), change.taken_text());
            let ts_added_end = ts_point_from(added, (ts_start.column, start), change.added_text());

            self.range.start = (self.range.start as i32 + change.shift()[0]) as usize;
            self.range.end = (self.range.end as i32 + change.shift()[0]) as usize;
            self.offset = deoffset(self.offset, ts_taken_end);
            self.offset = reoffset(self.offset, ts_added_end);
        } else if taken.byte() < self.range.end {
            let edit = input_edit(change, bytes, self.offset, self.range.start);
            self.tree.edit(&edit);
            self.range.end = (self.range.end as i32 + change.shift()[0]) as usize;
            self.old_tree = None;
            duat_core::log!("the old tree in {:?} is None", self.range);
        } else {
            // If this sub tree wasn't affected, neither will any of its children.
            return;
        }

        self.sub_trees.retain_mut(|st| {
            if change_clips(change, st.range.clone()) {
                false
            } else {
                st.apply_sub_tree_change(change, bytes);
                true
            }
        });
    }

    fn shift_tree(&mut self, shift: [i32; 3]) {
        self.range.start = (self.range.start as i32 + shift[0]) as usize;
        self.range.end = (self.range.end as i32 + shift[0]) as usize;
        self.offset.row = (self.offset.row as i32 + shift[2]) as usize;
        for st in self.sub_trees.iter_mut() {
            st.shift_tree(shift);
        }
    }

    fn reparse_sub_trees(&mut self, bytes: &mut Bytes) {
        for st in self.sub_trees.iter_mut() {
            st.reparse_sub_trees(bytes);
            if st.old_tree.is_none() {
                duat_core::log!("reparsed tree at {:?}", st.range);
                let mut parse_fn = buf_parse(bytes, st.range.clone());
                let tree = st
                    .parser
                    .parse_with_options(&mut parse_fn, Some(&st.tree), None)
                    .unwrap();

                st.old_tree = Some(std::mem::replace(&mut st.tree, tree));
            }
        }
    }
}

impl Reader for TsParser {
    type PublicReader<'a> = PubTsParser<'a>;

    // `apply_changes` is not meant to modify the `Text`, it is only meant
    // to update the internal state of the `Reader`.
    fn apply_changes(&mut self, bytes: &mut Bytes, changes: &[Change<&str>]) {
        fn changes_b_shift(changes: &[Change<&str>]) -> i32 {
            changes
                .iter()
                .fold(0, |b_sh, change| b_sh + change.shift()[0])
        }

        let mut sub_trees_to_remove = Vec::new();
        let mut sub_trees = self.sub_trees.iter_mut().enumerate().peekable();
        let mut last_changes: Vec<Change<&str>> = Vec::new();
        let (mut sh_from, mut shift) = (0, [0; 3]);

        for &change in changes.iter() {
            let start = change.start();

            let input_edit = input_edit(change, bytes, self.offset, self.range.start);
            self.tree.edit(&input_edit);

            self.range.end = (self.range.end as i32 + change.shift()[0]) as usize;

            // First, deal with all sub trees before the Change.
            while let Some((i, st)) = sub_trees.next_if(|(i, st)| {
                let end = if *i >= sh_from {
                    (st.range.end as i32 + shift[0] + changes_b_shift(&last_changes)) as usize
                } else {
                    st.range.end
                };
                end <= start.byte()
            }) {
                if i == sh_from {
                    st.shift_tree(shift);
                    for change in last_changes.iter() {
                        st.apply_sub_tree_change(*change, bytes);
                    }
                }

                sh_from = i + 1;
            }

            // Then, get rid of consecutively clipped sub trees.
            while let Some((i, _)) = sub_trees.next_if(|(i, st)| {
                let range = if *i == sh_from {
                    let shift = shift[0] + changes_b_shift(&last_changes);
                    let start = (st.range.start as i32 + shift) as usize;
                    let end = (st.range.end as i32 + shift) as usize;
                    start..end
                } else {
                    st.range.clone()
                };
                change_clips(change, range)
            }) {
                sub_trees_to_remove.push(i);
                sh_from = i + 1;
            }

            // Now, this sub tree should either contain the change or be ahead of
            // it.
            if let Some((i, st)) = sub_trees.peek_mut() {
                if *i == sh_from {
                    st.shift_tree(shift);
                    for change in last_changes.iter() {
                        st.apply_sub_tree_change(*change, bytes);
                    }
                }

                st.apply_sub_tree_change(change, bytes);
                sh_from = *i + 1;

                if let Some(last) = last_changes.last()
                    && (last.taken_end().line() == start.line()
                        || last.taken_end().byte() > st.range.start)
                {
                    last_changes.push(change);
                } else {
                    shift = last_changes
                        .drain(..)
                        .fold(shift, |sh, change| add_shifts(sh, change.shift()));
                    last_changes = vec![change];
                };
            }
        }

        for (i, st) in sub_trees {
            if i >= sh_from {
                st.shift_tree(shift);
                for change in last_changes.iter() {
                    st.apply_sub_tree_change(*change, bytes);
                }
            }
        }

        let mut parse_fn = buf_parse(bytes, self.range.clone());
        let tree = self
            .parser
            .parse_with_options(&mut parse_fn, Some(&self.tree), None)
            .unwrap();

        self.old_tree = Some(std::mem::replace(&mut self.tree, tree));
        drop(parse_fn);

        for i in sub_trees_to_remove.into_iter().rev() {
            self.sub_trees.remove(i);
        }

        self.reparse_sub_trees(bytes);
    }

    // `ranges_to_update` takes the information stored in `apply_changes`
    // and returns a `Range<usize>` of byte indexes that need to be
    // updated.
    // This is done for efficiency reasons, but you don't _have_ to
    // implement this function. If you don't, it will be assumed that the
    // whole `Text` needs updates.
    fn ranges_to_update(
        &mut self,
        bytes: &mut Bytes,
        changes: &[Change<&str>],
    ) -> Vec<Range<usize>> {
        fn merge_tree_changed_ranges(parser: &TsParser, ranges: &mut Vec<Range<usize>>) {
            if let Some(old_tree) = parser.old_tree.as_ref() {
                for range in parser.tree.changed_ranges(old_tree) {
                    let range = range.start_byte..range.end_byte;
                    merge_range_in(ranges, range);
                }

                for st in parser.sub_trees.iter() {
                    merge_tree_changed_ranges(st, ranges)
                }
            }
        }
        let mut ranges = Vec::new();
        // let mut checked_points = Vec::new();

        // This initial check might find larger, somewhat self contained nodes
        // that have changed, e.g. an identifier that is now recognized as a
        // function, things of that sort.
        merge_tree_changed_ranges(self, &mut ranges);

        // However, `changed_ranges` doesn't catch everything, so another
        // check is done. At a minimum, at least the lines where the changes
        // took place should be updated.
        for change in changes {
            let start = change.start();
            let added = change.added_end();
            let start = bytes.point_at_line(start.line());
            let end = bytes.point_at_line((added.line() + 1).min(bytes.len().line()));
            merge_range_in(&mut ranges, start.byte()..end.byte());
        }

        ranges
    }

    fn update_range(&mut self, bytes: &mut Bytes, mut tags: MutTags, range: Range<usize>) {
        self.highlight_and_inject(bytes, &mut tags, range);
    }

    fn public_reader<'a>(&'a mut self, bytes: &'a mut Bytes) -> Self::PublicReader<'a> {
        PubTsParser(self, bytes)
    }
}

impl std::fmt::Debug for TsParser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start = self.range.start;
        f.debug_struct("TsParser")
            .field("range", &self.range)
            .field("offset", &self.offset)
            .field("tree", &self.tree.root_node_with_offset(start, self.offset))
            .field("old_tree", &self.old_tree)
            .field("sub_trees", &self.sub_trees)
            .finish_non_exhaustive()
    }
}

pub struct TsParserCfg {
    lang_parts: LangParts<'static>,
    form_parts: &'static [(FormId, u8)],
}

impl TsParserCfg {
    pub fn new(filetype: &str) -> Option<Self> {
        let lang_parts = lang_parts(filetype)?;
        let form_parts = forms_from_lang_parts(&lang_parts);

        Some(TsParserCfg { lang_parts, form_parts })
    }
}

impl ReaderCfg for TsParserCfg {
    type Reader = TsParser;

    fn init(self, bytes: &mut Bytes, mut tags: MutTags) -> Result<Self::Reader, Text> {
        let offset = TSPoint::default();
        Ok(TsParser::init(
            bytes,
            &mut tags,
            0..bytes.len().byte(),
            offset,
            self.lang_parts,
            self.form_parts,
        ))
    }
}

pub struct PubTsParser<'a>(&'a mut TsParser, &'a mut Bytes);

impl<'a> PubTsParser<'a> {
    pub fn lang(&self) -> &'static str {
        self.0.lang_parts.0
    }

    /// Returns the indentation difference from the previous line
    // WARNING: long ass function
    pub fn indent_on(&mut self, p: Point, cfg: PrintCfg) -> Option<usize> {
        let (.., Queries { indents, .. }) = &self.0.lang_parts;
        if indents.pattern_count() == 0 {
            return None;
        }
        let tab = cfg.tab_stops.size() as i32;
        let [start, _] = self.1.points_of_line(p.line());

        // TODO: Get injected trees
        let root = self.0.tree.root_node();

        type Captures<'a> = HashMap<&'a str, HashMap<usize, HashMap<&'a str, Option<&'a str>>>>;
        let mut caps: Captures = HashMap::new();
        let q = {
            let mut cursor = QueryCursor::new();
            let buf = TsBuf(self.1);
            cursor.matches(indents, root, buf).for_each(|qm| {
                for cap in qm.captures.iter() {
                    let cap_end = indents.capture_names()[cap.index as usize]
                        .strip_prefix("indent.")
                        .unwrap();
                    let nodes = if let Some(nodes) = caps.get_mut(cap_end) {
                        nodes
                    } else {
                        caps.insert(cap_end, HashMap::new());
                        caps.get_mut(cap_end).unwrap()
                    };
                    let props = indents.property_settings(qm.pattern_index).iter();
                    nodes.insert(
                        cap.node.id(),
                        props
                            .map(|p| {
                                let key = p.key.strip_prefix("indent.").unwrap();
                                (key, p.value.as_deref())
                            })
                            .collect(),
                    );
                }
            });
            |caps: &Captures, node: Node, queries: &[&str]| {
                caps.get(queries[0])
                    .and_then(|nodes| nodes.get(&node.id()))
                    .is_some_and(|props| {
                        let key = queries.get(1);
                        key.is_none_or(|key| props.iter().any(|(k, _)| k == key))
                    })
            }
        };

        // The first non indent character of this line.
        let indented_start = self
            .1
            .chars_fwd(start)
            .take_while(|(p, _)| p.line() == start.line())
            .find_map(|(p, c)| (!c.is_whitespace()).then_some(p));

        let mut opt_node = if let Some(indented_start) = indented_start {
            Some(descendant_in(root, indented_start.byte()))
        // If the line is empty, look behind for another.
        } else {
            // Find last previous empty line.
            let Some((prev_l, line)) = self
                .1
                .lines((Point::default(), start))
                .rev()
                .find(|(_, line)| !(line.matches(r"^\s*$", ..).unwrap()))
            else {
                // If there is no previous non empty line, align to 0.
                return Some(0);
            };
            let trail = line.chars().rev().take_while(|c| c.is_whitespace()).count();

            let [prev_start, prev_end] = self.1.points_of_line(prev_l);
            let mut node = descendant_in(root, prev_end.byte() - (trail + 1));
            if node.kind().contains("comment") {
                // Unless the whole line is a comment, try to find the last node
                // before the comment.
                // This technically fails if there are multiple block comments.
                let first_node = descendant_in(root, prev_start.byte());
                if first_node.id() != node.id() {
                    node = descendant_in(root, node.start_byte() - 1)
                }
            }

            Some(if q(&caps, node, &["end"]) {
                descendant_in(root, start.byte())
            } else {
                node
            })
        };

        if q(&caps, opt_node.unwrap(), &["zero"]) {
            return Some(0);
        }

        let mut indent = 0;
        let mut processed_lines = Vec::new();
        while let Some(node) = opt_node {
            // If a node is not an indent and is marked as auto or ignore, act
            // accordingly.
            if !q(&caps, node, &["begin"])
                && node.start_position().row < p.line()
                && p.line() <= node.end_position().row
            {
                if !q(&caps, node, &["align"]) && q(&caps, node, &["auto"]) {
                    return None;
                } else if q(&caps, node, &["ignore"]) {
                    return Some(0);
                }
            }

            let s_line = node.range().start_point.row;
            let e_line = node.range().end_point.row;
            let should_process = !processed_lines.contains(&s_line);

            let mut is_processed = false;

            if should_process
                && ((s_line == p.line() && q(&caps, node, &["branch"]))
                    || (s_line != p.line() && q(&caps, node, &["dedent"])))
            {
                indent -= tab;
                is_processed = true;
            }

            let is_in_err = should_process && node.parent().is_some_and(|p| p.is_error());
            // Indent only if the node spans more than one line, or under other
            // special circumstances.
            if should_process
                && q(&caps, node, &["begin"])
                && (s_line != e_line || is_in_err || q(&caps, node, &["begin", "immediate"]))
                && (s_line != p.line() || q(&caps, node, &["begin", "start_at_same_line"]))
            {
                is_processed = true;
                indent += tab;
            }

            if is_in_err && !q(&caps, node, &["align"]) {
                let mut cursor = node.walk();
                for child in node.children(&mut cursor) {
                    if q(&caps, child, &["align"]) {
                        let props = caps["align"][&child.id()].clone();
                        caps.get_mut("align").unwrap().insert(node.id(), props);
                    }
                }
            }

            type FoundDelim<'a> = (Option<Node<'a>>, bool);
            fn find_delim<'a>(bytes: &mut Bytes, node: Node<'a>, delim: &str) -> FoundDelim<'a> {
                let mut c = node.walk();
                let child = node.children(&mut c).find(|child| child.kind() == delim);
                let ret = child.map(|child| {
                    let [_, end] = bytes.points_of_line(child.range().start_point.row);
                    let range = child.range().start_byte..end.byte();
                    let line = bytes.contiguous(range);
                    let is_last_in_line = line.split_whitespace().any(|w| w != delim);
                    (child, is_last_in_line)
                });
                let (child, is_last_in_line) = ret.unzip();
                (child, is_last_in_line.unwrap_or(false))
            }

            if should_process
                && q(&caps, node, &["align"])
                && (s_line != e_line || is_in_err)
                && s_line != p.line()
            {
                let props = &caps["align"][&node.id()];
                let (o_delim_node, o_is_last_in_line) = props
                    .get(&"open_delimiter")
                    .and_then(|delim| delim.map(|d| find_delim(self.1, node, d)))
                    .unwrap_or((Some(node), false));
                let (c_delim_node, c_is_last_in_line) = props
                    .get(&"close_delimiter")
                    .and_then(|delim| delim.map(|d| find_delim(self.1, node, d)))
                    .unwrap_or((Some(node), false));

                if let Some(o_delim_node) = o_delim_node {
                    let o_s_line = o_delim_node.start_position().row;
                    let o_s_col = o_delim_node.start_position().row;
                    let c_s_line = c_delim_node.map(|n| n.start_position().row);

                    // If the previous line was marked with an open_delimiter, treat it
                    // like an indent.
                    let indent_is_absolute = if o_is_last_in_line && should_process {
                        indent += tab;
                        // If the aligned node ended before the current line, its @align
                        // shouldn't affect it.
                        if c_is_last_in_line && c_s_line.is_some_and(|l| l < p.line()) {
                            indent = (indent - 1).max(0);
                        }
                        false
                    // Aligned indent
                    } else if c_is_last_in_line
                        && let Some(c_s_line) = c_s_line
                        // If the aligned node ended before the current line, its @align
                        // shouldn't affect it.
                        && (o_s_line != c_s_line && c_s_line < p.line())
                    {
                        indent = (indent - 1).max(0);
                        false
                    } else {
                        let inc = props.get("increment").cloned().flatten();
                        indent = o_s_col as i32 + inc.map(str::parse::<i32>).unwrap().unwrap();
                        true
                    };

                    // If this is the last line of the @align, then some additional
                    // indentation may be needed to avoid clashes. This is the case in
                    // some function parameters, for example.
                    let avoid_last_matching_next = c_s_line
                        .is_some_and(|c_s_line| c_s_line != o_s_line && c_s_line == p.line())
                        && props.contains_key("avoid_last_matching_next");
                    if avoid_last_matching_next {
                        indent += tab;
                    }
                    is_processed = true;
                    if indent_is_absolute {
                        return Some(indent as usize);
                    }
                }
            }

            if should_process && is_processed {
                processed_lines.push(s_line);
            }
            opt_node = node.parent();
        }

        Some(indent as usize)
    }
}

fn descendant_in(node: Node, byte: usize) -> Node {
    node.descendant_for_byte_range(byte, byte + 1).unwrap()
}

fn buf_parse<'a>(
    bytes: &'a mut Bytes,
    range: Range<usize>,
) -> impl FnMut(usize, TSPoint) -> &'a [u8] {
    let [s0, s1] = bytes.strs(range).to_array();
    |byte, _point| {
        if byte < s0.len() {
            &s0.as_bytes()[byte..]
        } else {
            &s1.as_bytes()[byte - s0.len()..]
        }
    }
}

fn ts_point(point: Point, buffer: &Bytes) -> TSPoint {
    let strs = buffer.strs(..point.byte());
    let iter = strs.into_iter().flat_map(str::chars).rev();
    let col = iter.take_while(|&b| b != '\n').count();

    TSPoint::new(point.line(), col)
}

fn ts_point_from(to: Point, (col, from): (usize, Point), str: &str) -> TSPoint {
    let col = if to.line() == from.line() {
        col + str.chars().count()
    } else {
        str.chars().rev().take_while(|&b| b != '\n').count()
    };

    TSPoint::new(to.line(), col)
}

fn forms_from_lang_parts(
    (lang, _, Queries { highlights, .. }): &LangParts<'static>,
) -> &'static [(FormId, u8)] {
    #[rustfmt::skip]
    const PRIORITIES: &[&str] = &[
        "string", "variable", "module", "label", "character", "boolean", "number", "type",
        "attribute", "property", "function", "constant", "constructor", "operator", "keyword",
        "punctuation", "comment", "markup"
    ];
    type MemoizedForms<'a> = HashMap<&'a str, &'a [(FormId, u8)]>;

    static LISTS: LazyLock<Mutex<MemoizedForms<'static>>> = LazyLock::new(Mutex::default);
    let mut lists = LISTS.lock();

    if let Some(forms) = lists.get(lang) {
        forms
    } else {
        let capture_names = highlights.capture_names();
        let priorities = capture_names.iter().map(|name| {
            PRIORITIES
                .iter()
                .take_while(|p| !name.starts_with(*p))
                .count() as u8
        });

        let ids = form::ids_of_non_static(
            capture_names
                .iter()
                .map(|name| name.to_string() + "." + lang),
        );
        let forms: Vec<(FormId, u8)> = ids.into_iter().zip(priorities).collect();

        lists.insert(lang, forms.leak());
        lists.get(lang).unwrap()
    }
}

#[derive(Clone, Copy)]
struct TsBuf<'a>(&'a Bytes);

impl<'a> TextProvider<&'a [u8]> for TsBuf<'a> {
    type I = std::array::IntoIter<&'a [u8], 2>;

    fn text(&mut self, node: tree_sitter::Node) -> Self::I {
        let range = node.range();
        let buffers = self.0.buffers(range.start_byte..range.end_byte);
        buffers.to_array().into_iter()
    }
}

fn lang_parts(lang: &str) -> Option<LangParts<'static>> {
    static MAPS: LazyLock<Mutex<HashMap<&str, LangParts<'static>>>> = LazyLock::new(Mutex::default);

    let mut maps = MAPS.lock();

    Some(if let Some(lang_parts) = maps.get(lang).copied() {
        lang_parts
    } else {
        let language: &'static Language = Box::leak(Box::new(languages::get_language(lang)?));

        let path = PathBuf::from(lang);
        let highlights = query_from_path(path.join("highlights"), language).unwrap();
        let indents = query_from_path(path.join("indents"), language).unwrap();
        let injections = query_from_path(path.join("injections"), language).unwrap();

        let queries = Queries { highlights, indents, injections };

        let lang = lang.to_string().leak();

        maps.insert(lang, (lang, language, queries.clone()));

        (lang, language, queries)
    })
}

#[allow(dead_code)]
fn log_node(node: tree_sitter::Node) {
    use std::fmt::Write;

    let mut cursor = node.walk();
    let mut node = Some(cursor.node());
    let mut log = String::new();
    while let Some(n) = node {
        let indent = " ".repeat(cursor.depth() as usize);
        if cursor.node().is_named() {
            writeln!(log, "{indent}{n:?}").unwrap();
        }
        let mut next_exists = cursor.goto_first_child() || cursor.goto_next_sibling();
        while !next_exists && cursor.goto_parent() {
            next_exists = cursor.goto_next_sibling();
        }
        node = next_exists.then_some(cursor.node());
    }

    duat_core::log!("{log}");
}

type LangParts<'a> = (&'a str, &'a Language, Queries<'a>);

#[derive(Clone, Copy)]
struct Queries<'a> {
    highlights: &'a Query,
    indents: &'a Query,
    injections: &'a Query,
}

/// The Key for tree-sitter
fn ts_key() -> Key {
    static KEY: LazyLock<Key> = LazyLock::new(Key::new);
    *KEY
}

fn deoffset(ts_point: TSPoint, offset: TSPoint) -> TSPoint {
    if ts_point.row == offset.row {
        TSPoint::new(ts_point.row - offset.row, ts_point.column - offset.column)
    } else {
        TSPoint::new(ts_point.row - offset.row, ts_point.column)
    }
}

fn reoffset(ts_point: TSPoint, offset: TSPoint) -> TSPoint {
    if ts_point.row == 0 {
        TSPoint::new(ts_point.row + offset.row, ts_point.column + offset.column)
    } else {
        TSPoint::new(ts_point.row + offset.row, ts_point.column)
    }
}

fn input_edit(
    change: Change<&str>,
    bytes: &mut Bytes,
    offset: TSPoint,
    r_start: usize,
) -> InputEdit {
    let start = change.start();
    let added = change.added_end();
    let taken = change.taken_end();

    let ts_start = ts_point(start, bytes);
    let ts_taken_end = ts_point_from(taken, (ts_start.column, start), change.taken_text());
    let ts_added_end = ts_point_from(added, (ts_start.column, start), change.added_text());

    InputEdit {
        start_byte: start.byte() - r_start,
        old_end_byte: taken.byte() - r_start,
        new_end_byte: added.byte() - r_start,
        start_position: deoffset(ts_start, offset),
        old_end_position: deoffset(ts_taken_end, offset),
        new_end_position: deoffset(ts_added_end, offset),
    }
}

fn change_clips(change: Change<&str>, range: Range<usize>) -> bool {
    let start = change.start();
    let taken = change.taken_end();

    (start.byte() <= range.start && range.start < taken.byte())
        || (start.byte() < range.end && range.end <= taken.byte())
}

fn query_from_path(path: impl AsRef<Path>, language: &Language) -> Option<&'static Query> {
    static QUERIES: LazyLock<Mutex<HashMap<PathBuf, &'static Query>>> =
        LazyLock::new(Mutex::default);

    let path = duat_core::plugin_dir("duat-treesitter")?
        .join("queries")
        .join(path.as_ref())
        .with_extension("scm");

    let mut queries = QUERIES.lock();

    Some(if let Some(query) = queries.get(&path) {
        query
    } else {
        let query = Box::leak(Box::new(
            Query::new(language, &fs::read_to_string(&path).unwrap_or_default()).ok()?,
        ));

        queries.insert(path, query);

        query
    })
}
