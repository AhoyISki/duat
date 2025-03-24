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
#![feature(decl_macro, let_chains)]

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
    InputEdit, Language, Node, Parser, Point as TSPoint, Query, QueryCaptures, QueryCursor,
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
    lang_parts: LangParts<'static>,
    forms: &'static [(FormId, Key, Key, usize)],
    keys: Range<Key>,
    tree: Tree,
    old_tree: Option<Tree>,
}

impl TsParser {
    fn cfg(file: &str) -> Option<TsParserCfg> {
        TsParserCfg::new(file)
    }
}

impl Reader for TsParser {
    type PublicReader<'a> = PubTsParser<'a>;

    // `apply_changes` is not meant to modify the `Text`, it is only meant
    // to update the internal state of the `Reader`.
    fn apply_changes(&mut self, bytes: &mut Bytes, changes: &[Change<&str>]) {
        for change in changes {
            let start = change.start();
            let added = change.added_end();
            let taken = change.taken_end();

            let ts_start = ts_point(start, bytes);
            let ts_taken_end = ts_point_from(taken, (ts_start.column, start), change.taken_text());
            let ts_added_end = ts_point_from(added, (ts_start.column, start), change.added_text());
            // Tree::edit will update the inner tree to reflect changes.
            self.tree.edit(&InputEdit {
                start_byte: start.byte(),
                old_end_byte: taken.byte(),
                new_end_byte: added.byte(),
                start_position: ts_start,
                old_end_position: ts_taken_end,
                new_end_position: ts_added_end,
            });
        }

        let tree = self
            .parser
            .parse_with_options(&mut buf_parse(bytes), Some(&self.tree), None)
            .unwrap();
        // I keep an old tree around, in order to compare it for tagging
        // purposes.
        self.old_tree = Some(std::mem::replace(&mut self.tree, tree));
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
        let (.., [hi_query, _]) = &self.lang_parts;
        let old_tree = self.old_tree.as_ref().unwrap();
        let mut cursor = QueryCursor::new();
        let buf = TsBuf(bytes);

        let mut ranges = Vec::new();
        let mut checked_points = Vec::new();

        for change in changes {
            let start = change.start();
            let added = change.added_end();
            let start = bytes.point_at_line(start.line()).byte();
            let end = bytes
                .point_at_line((added.line() + 1).min(bytes.len().line()))
                .byte();

            // Don't check the same region twice.
            if checked_points.contains(&(start, end)) {
                continue;
            }

            // Add one to the start and end, in order to include comments, since
            // those act a little weird in tree sitter.
            let ts_start = start.saturating_sub(1);
            let ts_end = (end + 1).min(bytes.len().byte());
            cursor.set_byte_range(ts_start..ts_end);

            let mut ml_ranges = Vec::new();

            // The next two loops concern themselves with determining what regions
            // of the file have been altered.
            // Normally, this is assumed to be just the current line, but in the
            // cases of primarily strings and ml comments, this will (probably)
            // involve the entire rest of the file below.
            let mut query_matches = cursor.captures(hi_query, old_tree.root_node(), buf);
            while let Some((query_match, _)) = query_matches.next() {
                for cap in query_match.captures.iter() {
                    let range = cap.node.range();
                    if range.start_point.row != range.end_point.row {
                        ml_ranges.push((range, cap.index));
                    }
                }
            }

            let mut query_matches = cursor.captures(hi_query, self.tree.root_node(), buf);
            'ml_range_edited: while let Some((query_match, _)) = query_matches.next() {
                for cap in query_match.captures.iter() {
                    let range = cap.node.range();
                    let entry = (range, cap.index);

                    // If a ml range existed before and after the change, then it can be
                    // assumed that it was not altered.
                    // If it exists in only one of those times, then the whole rest of the
                    // file ahead must be checked, as ml ranges like strings will always
                    // alter every subsequent line in the file.
                    if range.start_point.row != range.end_point.row
                        && ml_ranges.extract_if(.., |r| *r == entry).next().is_none()
                    {
                        ml_ranges.push(entry);
                        break 'ml_range_edited;
                    }
                }
            }

            if ml_ranges.is_empty() {
                // `merge_range_in` is a useful function for the
                // `Reader::ranges_to_update`, it just merges a range into a list of
                // sorted ranges, returning in the exact format that Duat needs
                merge_range_in(&mut ranges, start..end)
            } else {
                // Considering that all ranges are coming in order, we can just
                // disregard the updates from the other ranges, since they will belong
                // to this one.
                merge_range_in(&mut ranges, start..bytes.len().byte());
                break;
            }

            checked_points.push((start, end));
        }

        ranges
    }

    fn update_range(&mut self, bytes: &mut Bytes, mut tags: MutTags, range: Range<usize>) {
        let (.., [hi_query, _]) = &self.lang_parts;
        let buf = TsBuf(bytes);

        tags.remove(range.clone(), self.keys.clone());

        let start = range.start.saturating_sub(1);
        let end = (range.end + 1).min(bytes.len().byte());

        let mut cursor = QueryCursor::new();

        cursor.set_byte_range(start..end);
        let query_captures = cursor.captures(hi_query, self.tree.root_node(), buf);
        highlight_captures(&mut tags, query_captures, &self.forms);
    }

    fn public_reader<'a>(&'a mut self, bytes: &'a mut Bytes) -> Self::PublicReader<'a> {
        PubTsParser(self, bytes)
    }
}

pub struct TsParserCfg {
    lang_parts: LangParts<'static>,
    forms: &'static [(FormId, Key, Key, usize)],
    keys: Range<Key>,
}

impl TsParserCfg {
    pub fn new(file: &str) -> Option<Self> {
        let lang_parts = lang_parts(file)?;
        let (keys, forms) = forms_from_query(&lang_parts);

        Some(TsParserCfg { lang_parts, forms, keys })
    }
}

impl ReaderCfg for TsParserCfg {
    type Reader = TsParser;

    fn init(self, bytes: &mut Bytes, mut tags: MutTags) -> Result<Self::Reader, Text> {
        let (_, lang, [hi_query, _]) = &self.lang_parts;

        let mut parser = Parser::new();
        parser.set_language(lang).unwrap();

        let tree = parser
            .parse_with_options(&mut buf_parse(bytes), None, None)
            .unwrap();
        let mut cursor = QueryCursor::new();
        let buf = TsBuf(bytes);

        let query_captures = cursor.captures(hi_query, tree.root_node(), buf);
        highlight_captures(&mut tags, query_captures, &self.forms);

        duat_core::log_file!("{tags:#?}");

        Ok(TsParser {
            parser,
            lang_parts: self.lang_parts,
            tree,
            forms: self.forms,
            keys: self.keys,
            old_tree: None,
        })
    }
}

pub struct PubTsParser<'a>(&'a mut TsParser, &'a mut Bytes);

impl<'a> PubTsParser<'a> {
    pub fn lang(&self) -> &'static str {
        self.0.lang_parts.0[1]
    }

    /// Returns the indentation difference from the previous line
    // WARNING: long ass function
    pub fn indent_on(&mut self, p: Point, cfg: PrintCfg) -> Option<usize> {
        let (.., [_, indent_query]) = &self.0.lang_parts;
        if indent_query.pattern_count() == 0 {
            return None;
        }
        let tab = cfg.tab_stops.size() as i32;
        let [start, _] = self.1.points_of_line(p.line());
        let indented_start = self
            .1
            .chars_fwd(start)
            .take_while(|(p, _)| p.line() == start.line())
            .find_map(|(p, c)| (!c.is_whitespace()).then_some(p));
        // TODO: Get injected trees
        let root = self.0.tree.root_node();

        type Captures<'a> = HashMap<&'a str, HashMap<usize, HashMap<&'a str, Option<&'a str>>>>;
        let mut caps: Captures = HashMap::new();
        let q = {
            let mut cursor = QueryCursor::new();
            let buf = TsBuf(self.1);
            cursor.matches(indent_query, root, buf).for_each(|qm| {
                for cap in qm.captures.iter() {
                    let cap_end = indent_query.capture_names()[cap.index as usize]
                        .strip_prefix("indent.")
                        .unwrap();
                    let nodes = if let Some(nodes) = caps.get_mut(cap_end) {
                        nodes
                    } else {
                        caps.insert(cap_end, HashMap::new());
                        caps.get_mut(cap_end).unwrap()
                    };
                    let props = indent_query.property_settings(qm.pattern_index).iter();
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
                // If there is no previous line non empty, align to 0.
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

fn buf_parse<'a>(bytes: &'a mut Bytes) -> impl FnMut(usize, TSPoint) -> &'a [u8] {
    let [s0, s1] = bytes.strs(..).to_array();
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

fn forms_from_query(([lang, ..], _, [query, ..]): &LangParts<'static>) -> FormParts<'static> {
    #[rustfmt::skip]
    const PRECEDENCES: &[&str] = &[
        "variable", "constant", "module", "label", "string", "character", "boolean", "number",
        "type", "attribute", "property", "constructor", "operator", "keyword", "punctuation",
        "comment", "markup"
    ];
    static LISTS: LazyLock<Mutex<HashMap<&str, FormParts<'static>>>> =
        LazyLock::new(Mutex::default);
    let mut lists = LISTS.lock();

    if let Some((keys, forms)) = lists.get(lang) {
        (keys.clone(), forms)
    } else {
        let capture_names = query.capture_names();
        let precedences = capture_names.iter().map(|name| {
            PRECEDENCES
                .iter()
                .position(|p| name.starts_with(p))
                .unwrap_or(usize::MAX)
        });

        let keys = Key::new_many(capture_names.len() * 2);

        let mut iter_keys = keys.clone();
        let ids = form::ids_of_non_static(capture_names);
        let forms: Vec<(FormId, Key, Key, usize)> = ids
            .into_iter()
            .zip(precedences)
            .map(|(id, p)| (id, iter_keys.next().unwrap(), iter_keys.next().unwrap(), p))
            .collect();

        lists.insert(lang, (keys, forms.leak()));
        lists.get(lang).unwrap().clone()
    }
}

fn highlight_captures<'a>(
    tags: &mut MutTags,
    mut query_captures: QueryCaptures<TsBuf<'a>, &'a [u8]>,
    forms: &[(FormId, Key, Key, usize)],
) {
    let mut cur_start = 0;
    let mut cur_caps: Vec<(FormId, Key, Key, usize, usize)> = Vec::new();

    while let Some((query_matches, _)) = query_captures.next() {
        for cap in query_matches.captures.iter() {
            let range = cap.node.range();
            let (start, end) = (range.start_byte, range.end_byte);
            let (form, start_key, end_key, precedence) = forms[cap.index as usize];
            if start != cur_start {
                for (form, start_key, end_key, end, _) in cur_caps.drain(..) {
                    tags.insert(cur_start, Tag::PushForm(form), start_key);
                    tags.insert(end, Tag::PopForm(form), end_key);
                }
                cur_start = start;
            }
            if start != end {
                let i = cur_caps
                    .iter()
                    .take_while(|(.., p)| *p <= precedence)
                    .count();
                cur_caps.insert(i, (form, start_key, end_key, end, precedence));
            }
        }
    }

    for (form, start_key, end_key, end, _) in cur_caps.drain(..) {
        tags.insert(cur_start, Tag::PushForm(form), start_key);
        tags.insert(end, Tag::PopForm(form), end_key);
    }
}

impl<'a> TextProvider<&'a [u8]> for TsBuf<'a> {
    type I = std::array::IntoIter<&'a [u8], 2>;

    fn text(&mut self, node: tree_sitter::Node) -> Self::I {
        let range = node.range();
        let buffers = self.0.buffers(range.start_byte..range.end_byte);
        buffers.to_array().into_iter()
    }
}

#[derive(Clone, Copy)]
struct TsBuf<'a>(&'a Bytes);

fn lang_parts(path: impl AsRef<Path>) -> Option<LangParts<'static>> {
    type InnerLangParts<'a> = ([&'a str; 2], &'a Language, [&'a str; 2]);
    static LANGUAGES: LazyLock<HashMap<&'static str, InnerLangParts>> = LazyLock::new(|| {
        macro l($lang:ident) {{
            let lang: &'static Language = Box::leak(Box::new($lang::LANGUAGE.into()));
            lang
        }}
        macro lf($lang:ident) {
            Box::leak(Box::new($lang::language()))
        }
        macro h($lang:ident) {{
            let hi = include_str!(concat!("../queries/", stringify!($lang), "/highlights.scm"));
            [hi, ""]
        }}
        macro i($lang:ident) {
            include_str!(concat!("../queries/", stringify!($lang), "/indents.scm"))
        }
        macro h_i($lang:ident) {
            [h!($lang)[0], i!($lang)]
        }

        let lang_ocaml = Box::leak(Box::new(ts_ocaml::LANGUAGE_OCAML.into()));
        let lang_php = Box::leak(Box::new(ts_php::LANGUAGE_PHP_ONLY.into()));
        let lang_ts = Box::leak(Box::new(ts_ts::LANGUAGE_TYPESCRIPT.into()));
        let lang_xml = Box::leak(Box::new(ts_xml::LANGUAGE_XML.into()));

        let list = [
            (["asm", "Assembly"], l!(ts_asm), h!(asm)),
            (["c", "C"], l!(ts_c), h_i!(c)),
            (["cc", "C++"], l!(ts_cpp), h_i!(cpp)),
            (["cpp", "C++"], l!(ts_cpp), h_i!(cpp)),
            (["cs", "C#"], l!(ts_c_sharp), h!(c_sharp)),
            (["css", "CSS"], l!(ts_css), h_i!(css)),
            (["cxx", "C++"], l!(ts_cpp), h_i!(cpp)),
            (["dart", "Dart"], lf!(ts_dart), h_i!(dart)),
            (["erl", "Erlang"], l!(ts_erlang), h!(erlang)),
            (["ex", "Elixir"], l!(ts_elixir), h_i!(elixir)),
            (["exs", "Elixir"], l!(ts_elixir), h_i!(elixir)),
            (["for", "Fortran"], l!(ts_fortran), h_i!(fortran)),
            (["fpp", "Fortran"], l!(ts_fortran), h_i!(fortran)),
            (["gleam", "Gleam"], l!(ts_gleam), h_i!(gleam)),
            (["go", "Go"], l!(ts_go), h_i!(go)),
            (["groovy", "Groovy"], l!(ts_groovy), h_i!(groovy)),
            (["gvy", "Groovy"], l!(ts_groovy), h_i!(groovy)),
            (["h", "C"], l!(ts_c), h_i!(c)),
            (["hpp", "C++"], l!(ts_cpp), h_i!(cpp)),
            (["hrl", "Erlang"], l!(ts_erlang), h!(erlang)),
            (["hs", "Haskell"], l!(ts_haskell), h!(haskell)),
            (["hsc", "Haskell"], l!(ts_haskell), h!(haskell)),
            (["htm", "HTML"], l!(ts_html), h_i!(html)),
            (["html", "HTML"], l!(ts_html), h_i!(html)),
            (["hxx", "C++"], l!(ts_cpp), h_i!(cpp)),
            (["java", "Java"], l!(ts_java), h_i!(java)),
            (["jl", "Julia"], l!(ts_julia), h_i!(julia)),
            (["js", "JavaScript"], l!(ts_js), h_i!(js)),
            (["json", "JSON"], l!(ts_json), h_i!(json)),
            (["jsonc", "JSON"], l!(ts_json), h_i!(json)),
            (["lua", "Lua"], l!(ts_lua), h_i!(lua)),
            (["m", "Objective-C"], l!(ts_objc), h_i!(objc)),
            (["md", "Markdown"], l!(ts_md), h_i!(markdown)),
            (["ml", "OCaml"], lang_ocaml, h_i!(ocaml)),
            (["nix", "Nix"], l!(ts_nix), h_i!(nix)),
            (["php", "PHP"], lang_php, h_i!(php)),
            (["py", "Python"], l!(ts_python), h_i!(python)),
            (["pyc", "Python"], l!(ts_python), h_i!(python)),
            (["pyo", "Python"], l!(ts_python), h_i!(python)),
            (["r", "R"], l!(ts_r), h_i!(r)),
            (["rb", "Ruby"], l!(ts_ruby), h_i!(ruby)),
            (["rs", "Rust"], l!(ts_rust), h_i!(rust)),
            (["sc", "Scala"], l!(ts_scala), h!(scala)),
            (["scala", "Scala"], l!(ts_scala), h!(scala)),
            (["scss", "SCSS"], lf!(ts_scss), h_i!(scss)),
            (["sh", "Shell"], l!(ts_bash), h!(bash)),
            (["sql", "SQL"], l!(ts_sequel), h_i!(sql)),
            (["swift", "Swift"], l!(ts_swift), h_i!(swift)),
            (["ts", "TypeScript"], lang_ts, h!(ts)),
            (["vim", "Viml"], lf!(ts_vim), h!(vim)),
            (["xml", "XML"], lang_xml, h_i!(xml)),
            (["xrl", "Erlang"], l!(ts_erlang), h!(erlang)),
            (["yaml", "YAML"], l!(ts_yaml), h_i!(yaml)),
            (["yml", "YAML"], l!(ts_yaml), h_i!(yaml)),
            (["yrl", "Erlang"], l!(ts_erlang), h!(erlang)),
            (["zig", "Zig"], l!(ts_zig), h_i!(zig)),
        ];

        HashMap::from_iter(list.into_iter().map(|lp| (lp.0[0], lp)))
    });

    let ext = path.as_ref().extension()?.to_str()?;
    LANGUAGES.get(ext).copied().map(|(idents, lang, queries)| {
        let queries = queries.map(|q| Query::new(lang, q).unwrap());
        (idents, lang, queries)
    })
}

type LangParts<'a> = ([&'a str; 2], &'a Language, [Query; 2]);
type FormParts<'a> = (Range<Key>, &'a [(FormId, Key, Key, usize)]);
