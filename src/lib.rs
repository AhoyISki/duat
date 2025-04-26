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
#![feature(decl_macro, let_chains, macro_metavar_expr_concat)]

use std::{collections::HashMap, marker::PhantomData, ops::Range, path::Path, sync::LazyLock};

use duat_core::{
    Mutex,
    cfg::PrintCfg,
    form::{self, Form, FormId},
    hooks::{self, OnFileOpen},
    text::{
        Bytes, Change, Key, Matcheable, MutTags, Point, Reader, ReaderCfg, Tag, Text,
        merge_range_in,
    },
};
use streaming_iterator::StreamingIterator;
use tree_sitter::{
    InputEdit, Language, Node, Parser, Point as TSPoint, Query, QueryCapture, QueryCursor,
    TextProvider, Tree,
};

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
            let name = builder.read().0.name();
            if let Some(ts_parser_cfg) = TsParser::cfg(&name) {
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

        let (.., [hi_query, _, inj_query]) = &self.lang_parts;
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
            let mut sub_trees_to_add: Vec<(Range<usize>, TSPoint, String)> = Vec::new();

            let mut inj_captures = cursor.captures(inj_query, root, buf);
            let cap_names = inj_query.capture_names();
            let is_content =
                |cap: &&QueryCapture| cap_names[cap.index as usize] == "injection.content";
            let is_language =
                |cap: &&QueryCapture| cap_names[cap.index as usize] == "injection.language";
            while let Some((qm, _)) = inj_captures.next() {
                let Some(cap) = qm.captures.iter().find(is_content) else {
                    continue;
                };

                let props = inj_query.property_settings(qm.pattern_index);
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

                if let Some(lang) = lang
                    && !sub_trees_to_add
                        .iter()
                        .any(|(lhs, ..)| *lhs == cap.node.byte_range())
                {
                    sub_trees_to_add.push((cap.node.byte_range(), cap.node.start_position(), lang));
                }
            }

            sub_trees_to_add
        };

        // If a tree was not in sub_trees_to_add, but is part of the affected
        // range, that means it was removed.
        self.sub_trees.retain_mut(|st| {
            if let Some((.., lang)) = sub_trees_to_add.iter().find(|(lhs, ..)| *lhs == st.range) {
                if lang != st.lang_parts.1[0] {
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
            } else if st.range.start >= start && st.range.end <= end {
                false
            } else {
                st.highlight_and_inject(bytes, tags, range.clone());
                true
            }
        });

        // In the end, we add the sub trees that weren't already in there.
        // This should automatically handle all of the sub trees's sub trees.
        for (range, offset, lang) in sub_trees_to_add {
            if !self.sub_trees.iter().any(|st| st.range == range) {
                let Some(lang_parts) = lang_parts_from_ident(&lang) else {
                    continue;
                };

                let form_parts = forms_from_query(&lang_parts);
                self.sub_trees.push(TsParser::init(
                    bytes, tags, range, offset, lang_parts, form_parts,
                ))
            }
        }

        // We highlight at the very end, so if, for example, a sub tree gets
        // removed, tags can be readded, without leaving a blank space, in
        // case the injection was of the same language.
        let buf = TsBuf(bytes);
        cursor.set_byte_range(start..end);
        let mut hi_captures = cursor.captures(hi_query, root, buf);
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

    fn cfg(file: &str) -> Option<TsParserCfg> {
        TsParserCfg::new(file)
    }
}

impl Reader for TsParser {
    type PublicReader<'a> = PubTsParser<'a>;

    // `apply_changes` is not meant to modify the `Text`, it is only meant
    // to update the internal state of the `Reader`.
    fn apply_changes(&mut self, bytes: &mut Bytes, changes: &[Change<&str>]) {
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

        let mut at_least_one_change = false;
        for change in changes {
            let start = change.start();
            let added = change.added_end();
            let taken = change.taken_end();

            let ts_start = ts_point(start, bytes);
            let ts_taken_end = ts_point_from(taken, (ts_start.column, start), change.taken_text());
            let ts_added_end = ts_point_from(added, (ts_start.column, start), change.added_text());

            // Only make changes to this tree if the Change takes place in it.
            // If the Change happens before the tree, we can just shift it.
            if start.byte() < self.range.start && taken.byte() <= self.range.start {
                self.range.start = (self.range.start as i32 + change.shift()[0]) as usize;
                self.range.end = (self.range.end as i32 + change.shift()[0]) as usize;
                self.offset = deoffset(self.offset, ts_taken_end);
                self.offset = reoffset(self.offset, ts_added_end);
                continue;
            // If it happens after, we can just ignore it.
            // By this point, sub trees that intersect with a change
            // should already have been removed, so we can ignore that
            // scenario.
            } else if start.byte() > self.range.end {
                continue;
            }
            at_least_one_change = true;

            self.tree.edit(&InputEdit {
                start_byte: start.byte() - self.range.start,
                old_end_byte: taken.byte() - self.range.start,
                new_end_byte: added.byte() - self.range.start,
                start_position: deoffset(ts_start, self.offset),
                old_end_position: deoffset(ts_taken_end, self.offset),
                new_end_position: deoffset(ts_added_end, self.offset),
            });

            self.range.end = (self.range.end as i32 + change.shift()[0]) as usize;

            self.sub_trees.retain_mut(|st| {
                // Remove trees that intersect with the changes
                !((start.byte() < st.range.start && st.range.start < taken.byte())
                    || (start.byte() < st.range.end && st.range.end < taken.byte()))
            });
        }

        if at_least_one_change {
            let mut parse_fn = buf_parse(bytes, self.range.clone());
            let tree = self
                .parser
                .parse_with_options(&mut parse_fn, Some(&self.tree), None)
                .unwrap();

            // I keep an old tree around, in order to compare it for tagging
            // purposes.
            self.old_tree = Some(std::mem::replace(&mut self.tree, tree));
        }

        for st in self.sub_trees.iter_mut() {
            st.apply_changes(bytes, changes);
        }
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
        let mut ranges = Vec::new();
        // let mut checked_points = Vec::new();

        // This initial check might find larger, somewhat self contained nodes
        // that have changed, e.g. an identifier that is now recognized as a
        // function, things of that sort.
        if let Some(old_tree) = self.old_tree.as_ref() {
            for range in self.tree.changed_ranges(old_tree) {
                let range = range.start_byte..range.end_byte;
                merge_range_in(&mut ranges, range);
            }
        }

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
    pub fn new(file: &str) -> Option<Self> {
        let lang_parts = lang_parts_from_file(file)?;
        let form_parts = forms_from_query(&lang_parts);

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
        self.0.lang_parts.1[0]
    }

    /// Returns the indentation difference from the previous line
    // WARNING: long ass function
    pub fn indent_on(&mut self, p: Point, cfg: PrintCfg) -> Option<usize> {
        let (.., [_, ind_query, _]) = &self.0.lang_parts;
        if ind_query.pattern_count() == 0 {
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
            cursor.matches(ind_query, root, buf).for_each(|qm| {
                for cap in qm.captures.iter() {
                    let cap_end = ind_query.capture_names()[cap.index as usize]
                        .strip_prefix("indent.")
                        .unwrap();
                    let nodes = if let Some(nodes) = caps.get_mut(cap_end) {
                        nodes
                    } else {
                        caps.insert(cap_end, HashMap::new());
                        caps.get_mut(cap_end).unwrap()
                    };
                    let props = ind_query.property_settings(qm.pattern_index).iter();
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

fn forms_from_query(
    (_, [lang, _], _, [query, ..]): &LangParts<'static>,
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
        let capture_names = query.capture_names();
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

fn lang_parts_from_file(path: impl AsRef<Path>) -> Option<LangParts<'static>> {
    static LANGUAGES: LazyLock<HashMap<&str, &InnerLangParts>> = LazyLock::new(|| {
        HashMap::from_iter(LANGS.iter().filter_map(|parts| parts.0.map(|e| (e, parts))))
    });

    let ext = path.as_ref().extension()?.to_str()?;
    LANGUAGES
        .get(ext)
        .copied()
        .map(|(ext, idents, lang_and_queries)| {
            let (lang, queries) = &**lang_and_queries;
            (*ext, *idents, lang, queries)
        })
}

fn lang_parts_from_ident(lang: &str) -> Option<LangParts<'static>> {
    static LANGUAGES: LazyLock<HashMap<&str, &InnerLangParts>> =
        LazyLock::new(|| HashMap::from_iter(LANGS.iter().map(|lp| (lp.1[0], lp))));

    LANGUAGES.get(lang).map(|(ext, idents, lang_and_queries)| {
        let (lang, queries) = &**lang_and_queries;
        (*ext, *idents, lang, queries)
    })
}

#[allow(dead_code)]
fn log_node(node: tree_sitter::Node) {
    use std::fmt::Write;

    let mut cursor = node.walk();
    let mut node = Some(cursor.node());
    let mut log = String::new();
    while let Some(no) = node {
        let indent = " ".repeat(cursor.depth() as usize);
        if cursor.node().is_named() {
            writeln!(log, "{indent}{no:?}").unwrap();
        }
        let mut next_exists = cursor.goto_first_child() || cursor.goto_next_sibling();
        while !next_exists && cursor.goto_parent() {
            next_exists = cursor.goto_next_sibling();
        }
        node = next_exists.then_some(cursor.node());
    }

    duat_core::log!("{log}");
}

type InnerLangParts<'a> = (
    Option<&'a str>,
    [&'a str; 2],
    LazyLock<&'a (Language, [Query; 3])>,
);
type LangParts<'a> = (Option<&'a str>, [&'a str; 2], &'a Language, &'a [Query; 3]);

#[rustfmt::skip]
static LANGS: [InnerLangParts; 57] = {
    use ts_md::INLINE_LANGUAGE as INLINE_MARKDOWN;
    macro l($name:ident) {
        ${concat(ts_, $name)}::LANGUAGE.into()
    }
    macro lf($name:ident) {
        ${concat(ts_, $name)}::language()
    }

    macro h($name:ident, $lang:expr) {
        Query::new(
            &$lang,
            include_str!(concat!("../queries/", stringify!($name), "/highlights.scm")),
        )
        .unwrap()
    }
    macro i($name:ident, $lang:expr) {
        Query::new(
            &$lang,
            include_str!(concat!("../queries/", stringify!($name), "/indents.scm")),
        )
        .unwrap()
    }
    macro j($name:ident, $lang:expr) {
        Query::new(
            &$lang,
            include_str!(concat!("../queries/", stringify!($name), "/injections.scm")),
        )
        .unwrap()
    }
    macro h_i($name:ident, $lang:expr) {{
        LazyLock::new(|| {
            let lang = $lang;
            let queries = [
                h!($name, lang),
                i!($name, lang),
                Query::new(&$lang, "").unwrap(),
            ];
            Box::leak(Box::new((lang, queries)))
        })
    }}
    macro h_j($name:ident, $lang:expr) {{
        LazyLock::new(|| {
            let lang = $lang;
            let queries = [
                h!($name, lang),
                Query::new(&$lang, "").unwrap(),
                j!($name, lang),
            ];
            Box::leak(Box::new((lang, queries)))
        })
    }}
    macro h_i_j($name:ident, $lang:expr) {{
        LazyLock::new(|| {
            let lang = $lang;
            let queries = [h!($name, $lang), i!($name, $lang), j!($name, $lang)];
            Box::leak(Box::new((lang, queries)))
        })
    }}

    [
        (Some("asm"), ["asm", "Assembly"], h_j!(asm, l!(asm))),
        (Some("c"), ["c", "C"], h_i_j!(c, l!(c))),
        (Some("h"), ["c", "C"], h_i_j!(c, l!(c))),
        (Some("cc"), ["cpp", "C++"], h_i_j!(cpp, l!(cpp))),
        (Some("cpp"), ["cpp", "C++"], h_i_j!(cpp, l!(cpp))),
        (Some("cxx"), ["cpp", "C++"], h_i_j!(cpp, l!(cpp))),
        (Some("hpp"), ["cpp", "C++"], h_i_j!(cpp, l!(cpp))),
        (Some("hxx"), ["cpp", "C++"], h_i_j!(cpp, l!(cpp))),
        (Some("cs"), ["csharp", "C#"], h_j!(c_sharp, l!(c_sharp))),
        (Some("css"), ["css", "CSS"], h_i_j!(css, l!(css))),
        (Some("dart"), ["dart", "Dart"], h_i_j!(dart, lf!(dart))),
        (Some("ex"), ["elixir", "Elixir"], h_i_j!(elixir, l!(elixir))),
        (Some("exs"), ["elixir", "Elixir"], h_i_j!(elixir, l!(elixir))),
        (Some("erl"), ["erlang", "Erlang"], h_j!(erlang, l!(erlang))),
        (Some("hrl"), ["erlang", "Erlang"], h_j!(erlang, l!(erlang))),
        (Some("xrl"), ["erlang", "Erlang"], h_j!(erlang, l!(erlang))),
        (Some("yrl"), ["erlang", "Erlang"], h_j!(erlang, l!(erlang))),
        (Some("for"), ["fortran", "Fortran"], h_i_j!(fortran, l!(fortran))),
        (Some("fpp"), ["fortran", "Fortran"], h_i_j!(fortran, l!(fortran))),
        (Some("gleam"), ["gleam", "Gleam"], h_i_j!(gleam, l!(gleam))),
        (Some("go"), ["go", "Go"], h_i_j!(go, l!(go))),
        (Some("groovy"), ["groovy", "Groovy"], h_i_j!(groovy, l!(groovy))),
        (Some("gvy"), ["groovy", "Groovy"], h_i_j!(groovy, l!(groovy))),
        (Some("hsc"), ["haskell", "Haskell"], h_j!(haskell, l!(haskell))),
        (Some("hs"), ["haskell", "Haskell"], h_j!(haskell, l!(haskell))),
        (Some("htm"), ["html", "HTML"], h_i_j!(html, l!(html))),
        (Some("html"), ["html", "HTML"], h_i_j!(html, l!(html))),
        (Some("java"), ["java", "Java"], h_i_j!(java, l!(java))),
        (Some("js"), ["js", "JavaScript"], h_i_j!(javascript, l!(js))),
        (Some("jsonc"), ["json", "JSON"], h_i!(json, l!(json))),
        (Some("json"), ["json", "JSON"], h_i!(json, l!(json))),
        (Some("jl"), ["julia", "Julia"], h_i_j!(julia, l!(julia))),
        (Some("lua"), ["lua", "Lua"], h_i_j!(lua, l!(lua))),
        (Some("md"), ["markdown", "Markdown"], h_i_j!(markdown, l!(md))),
        (None, ["markdown_inline", "Markdown"], h_j!(markdown_inline, INLINE_MARKDOWN.into())),
        (Some("nix"), ["nix", "Nix"], h_i_j!(nix, l!(nix))),
        (Some("m"), ["objc", "Objective-C"], h_i_j!(objc, l!(objc))),
        (Some("ml"), ["ocaml", "OCaml"], h_i_j!(ocaml, ts_ocaml::LANGUAGE_OCAML.into())),
        (Some("php"), ["php", "PHP"], h_i_j!(php, ts_php::LANGUAGE_PHP_ONLY.into())),
        (Some("pyc"), ["python", "Python"], h_i_j!(python, l!(python))),
        (Some("pyo"), ["python", "Python"], h_i_j!(python, l!(python))),
        (Some("py"), ["python", "Python"], h_i_j!(python, l!(python))),
        (Some("r"), ["r", "R"], h_i_j!(r, l!(r))),
        (Some("rb"), ["ruby", "Ruby"], h_i_j!(ruby, l!(ruby))),
        (Some("rs"), ["rust", "Rust"], h_i_j!(rust, l!(rust))),
        (Some("scala"), ["scala", "Scala"], h_j!(scala, l!(scala))),
        (Some("sc"), ["scala", "Scala"], h_j!(scala, l!(scala))),
        (Some("scss"), ["scss", "SCSS"], h_i_j!(scss, lf!(scss))),
        (Some("sh"), ["shell", "Shell"], h_j!(bash, l!(bash))),
        (Some("sql"), ["sql", "SQL"], h_i_j!(sql, l!(sequel))),
        (Some("swift"), ["swift", "Swift"], h_i_j!(swift, l!(swift))),
        (Some("ts"), ["ts", "TypeScript"], h_j!(typescript, ts_ts::LANGUAGE_TYPESCRIPT.into())),
        (Some("vim"), ["viml", "Viml"], h_j!(vim, lf!(vim))),
        (Some("xml"), ["xml", "XML"], h_i_j!(xml, ts_xml::LANGUAGE_XML.into())),
        (Some("yaml"), ["yaml", "YAML"], h_i_j!(yaml, l!(yaml))),
        (Some("yml"), ["yaml", "YAML"], h_i_j!(yaml, l!(yaml))),
        (Some("zig"), ["zig", "Zig"], h_i_j!(zig, l!(zig))),
    ]
};

/// The Key for tree-sitter
fn ts_key() -> Key {
    static KEY: LazyLock<Key> = LazyLock::new(Key::new);
    *KEY
}
