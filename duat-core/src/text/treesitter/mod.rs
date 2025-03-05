//! Everything concerning the tree-sitter functionalities
//!
//! This is primarily two things: the syntax highlighing and
//! indentation, which for all languages will be mostly done by
//! tree-sitter. The syntax highlighing is done via a [`Reader`]
//! struct, allowing similar functionality between tree-sitter and
//! other readers, like a regex reader for example.
use std::{collections::HashMap, ops::Range, path::Path, sync::LazyLock};

use gapbuf::GapBuffer;
use parking_lot::Mutex;
use streaming_iterator::StreamingIterator;
use tree_sitter::{
    InputEdit, Language, Node, Parser, Point as TSPoint, Query, QueryCursor, TextProvider, Tree,
};

use super::{Change, Key, Point, Reader, Tag, Text};
use crate::{
    cfg::PrintCfg,
    form::{self, FormId},
    text::Matcheable,
};

pub struct TsParser {
    parser: Parser,
    queries: [Query; 2],
    tree: Tree,
    forms: &'static [(FormId, Key, Key)],
    name: &'static str,
    keys: Range<Key>,
    old_tree: Option<Tree>,
}

impl TsParser {
    pub fn new(text: &mut Text, path: impl AsRef<Path>) -> Option<Self> {
        let (name, lang, queries) = lang_from_path(path)?;

        let mut parser = Parser::new();
        parser.set_language(lang).unwrap();
        let queries = queries.map(|q| Query::new(lang, q).unwrap());

        let tree = parser
            .parse_with_options(&mut buf_parse(text), None, None)
            .unwrap();
        let mut cursor = QueryCursor::new();
        let buf = TsBuf(&text.buf);

        let (keys, forms) = forms_from_query(name, &queries[0]);

        let mut captures = cursor.captures(&queries[0], tree.root_node(), buf);

        while let Some((captures, _)) = captures.next() {
            for cap in captures.captures.iter() {
                let range = cap.node.range();
                let (start, end) = (range.start_byte, range.end_byte);
                let (form, start_key, end_key) = forms[cap.index as usize];
                if start != end {
                    text.tags.insert(start, Tag::PushForm(form), start_key);
                    text.tags.insert(end, Tag::PopForm(form), end_key);
                }
            }
        }

        Some(TsParser {
            parser,
            queries,
            tree,
            forms,
            name,
            keys,
            old_tree: None,
        })
    }

    pub fn lang(&self) -> &'static str {
        self.name
    }

    /// Returns the indentation difference from the previous line
    // WARNING: long ass function
    pub fn indent_on(&self, text: &mut Text, p: Point, cfg: PrintCfg) -> Option<usize> {
        let query = &self.queries[1];
        if query.pattern_count() == 0 {
            return None;
        }
        let tab = cfg.tab_stops.size() as i32;
        let (start, _) = text.points_of_line(p.line());
        let indented_start = text
            .chars_fwd(start)
            .take_while(|(p, _)| p.line() == start.line())
            .find_map(|(p, c)| (!c.is_whitespace()).then_some(p));
        // TODO: Get injected trees
        let root = self.tree.root_node();

        type Captures<'a> = HashMap<&'a str, HashMap<usize, HashMap<&'a str, Option<&'a str>>>>;
        let mut caps: Captures = HashMap::new();
        let q = {
            let mut cursor = QueryCursor::new();
            let buf = TsBuf(&text.buf);
            cursor.matches(query, root, buf).for_each(|qm| {
                for cap in qm.captures.iter() {
                    let cap_end = query.capture_names()[cap.index as usize]
                        .strip_prefix("indent.")
                        .unwrap();
                    let nodes = if let Some(nodes) = caps.get_mut(cap_end) {
                        nodes
                    } else {
                        caps.insert(cap_end, HashMap::new());
                        caps.get_mut(cap_end).unwrap()
                    };
                    let props = query.property_settings(qm.pattern_index).iter();
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
            let Some((prev_l, line)) = text
                .lines_in((Point::default(), start))
                .rev()
                .find(|(_, line)| !(line.matches(r"^\s*$", ..).unwrap()))
            else {
                // If there is no previous line non empty, align to 0.
                return Some(0);
            };
            let trail = line.chars().rev().take_while(|c| c.is_whitespace()).count();
            let (prev_start, prev_end) = text.points_of_line(prev_l);
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
            fn find_delim<'a>(text: &mut Text, node: Node<'a>, delim: &str) -> FoundDelim<'a> {
                let mut c = node.walk();
                let child = node.children(&mut c).find(|child| child.kind() == delim);
                let ret = child.map(|child| {
                    let (_, end) = text.points_of_line(child.range().start_point.row);
                    let range = child.range().start_byte..end.byte();
                    text.make_contiguous_in(range.clone());
                    let line = unsafe { text.continuous_in_unchecked(range) };
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
                    .and_then(|delim| delim.map(|d| find_delim(text, node, d)))
                    .unwrap_or((Some(node), false));
                let (c_delim_node, c_is_last_in_line) = props
                    .get(&"close_delimiter")
                    .and_then(|delim| delim.map(|d| find_delim(text, node, d)))
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

    pub fn parse_with<T: AsRef<[u8]>, F: FnMut(usize, tree_sitter::Point) -> T>(
        &mut self,
        callback: &mut F,
        old_tree: Option<&Tree>,
    ) -> Option<Tree> {
        self.parser.parse_with_options(callback, old_tree, None)
    }
}

fn descendant_in(node: Node, byte: usize) -> Node {
    node.descendant_for_byte_range(byte, byte + 1).unwrap()
}

impl Reader for TsParser {
    fn apply_changes(&mut self, text: &Text, changes: &[Change<&str>]) {
        for change in changes {
            let start = change.start();
            let added = change.added_end();
            let taken = change.taken_end();

            let ts_start = ts_point(start, text);
            let ts_taken_end = ts_point_from(taken, (ts_start.column, start), text);
            let ts_added_end = ts_point_from(added, (ts_start.column, start), text);
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
            .parse_with_options(&mut buf_parse(text), Some(&self.tree), None)
            .unwrap();
        self.old_tree = Some(std::mem::replace(&mut self.tree, tree));
    }

    fn ranges_to_update(&mut self, text: &Text, changes: &[Change<&str>]) -> Vec<Range<usize>> {
        let old_tree = self.old_tree.as_ref().unwrap();
        let mut cursor = QueryCursor::new();
        let buf = TsBuf(&text.buf);

        let mut to_update = Vec::new();

        for change in changes {
            let start = change.start();
            let added = change.added_end();
            let start = text.point_at_line(start.line()).byte();
            let end = text.point_at_line(added.line() + 1).byte();

            // Add one to the start and end, in order to include comments, since
            // those act a little weird in tree sitter.
            let ts_start = start.saturating_sub(1);
            let ts_end = (end + 1).min(text.len().byte());
            cursor.set_byte_range(ts_start..ts_end);

            let mut this_to_update = Vec::new();

            // The next two loops concern themselves with determining what regions
            // of the file have been altered.
            // Normally, this is assumed to be just the current line, but in the
            // cases of primarily strings and ml comments, this will (probably)
            // involve the entire rest of the file below.
            let mut query_matches = cursor.captures(&self.queries[0], old_tree.root_node(), buf);
            while let Some((query_match, _)) = query_matches.next() {
                for cap in query_match.captures.iter() {
                    let range = cap.node.range();
                    if range.start_point.row != range.end_point.row {
                        this_to_update.push((range, cap.index));
                    }
                }
            }

            let mut query_matches = cursor.captures(&self.queries[0], self.tree.root_node(), buf);
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
                        && this_to_update
                            .extract_if(.., |r| *r == entry)
                            .next()
                            .is_none()
                    {
                        this_to_update.push(entry);
                        break 'ml_range_edited;
                    }
                }
            }
            if this_to_update.is_empty() {
                super::merge_range_in(&mut to_update, start..end)
            } else {
                // Considering that all ranges are coming in order, we can just
                // disregard the updates from the other ranges, since they will belong
                // to this one.
                super::merge_range_in(&mut to_update, start..text.len().byte());
                break;
            }
        }

        to_update
    }

    fn update_range(&mut self, text: &mut Text, range: Range<usize>) {
        let buf = TsBuf(&text.buf);

        text.tags.remove_from(range.clone(), self.keys.clone());

        let start = range.start.saturating_sub(1);
        let end = (range.end + 1).min(text.len().byte());

        let mut cursor = QueryCursor::new();

        cursor.set_byte_range(start..end);
        let mut query_matches = cursor.captures(&self.queries[0], self.tree.root_node(), buf);
        while let Some((query_match, _)) = query_matches.next() {
            for cap in query_match.captures.iter() {
                let bytes = cap.node.byte_range();
                let (form, start_key, end_key) = self.forms[cap.index as usize];
                if bytes.start != bytes.end {
                    text.tags
                        .insert(bytes.start, Tag::PushForm(form), start_key);
                    text.tags.insert(bytes.end, Tag::PopForm(form), end_key);
                }
            }
        }
    }
}

fn buf_parse<'a>(text: &'a Text) -> impl FnMut(usize, TSPoint) -> &'a [u8] {
    let [s0, s1] = text.strs();
    |byte, _point| {
        if byte < s0.len() {
            &s0.as_bytes()[byte..]
        } else {
            &s1.as_bytes()[byte - s0.len()..]
        }
    }
}

fn ts_point(point: Point, text: &Text) -> TSPoint {
    let strs = text.strs_in(..point.byte());
    let iter = strs.into_iter().flat_map(str::chars).rev();
    let col = iter.take_while(|&b| b != '\n').count();

    TSPoint::new(point.line(), col)
}

fn ts_point_from(to: Point, from: (usize, Point), text: &Text) -> TSPoint {
    let (col, from) = from;
    let strs = text.strs_in((from, to));
    let iter = strs.into_iter().flat_map(str::chars).rev();

    let col = if to.line() == from.line() {
        col + iter.count()
    } else {
        iter.take_while(|&b| b != '\n').count()
    };

    TSPoint::new(to.line(), col)
}

fn forms_from_query(
    lang: &'static str,
    query: &Query,
) -> (Range<Key>, &'static [(FormId, Key, Key)]) {
    static LISTS: Mutex<Vec<(&str, (Range<Key>, &[(FormId, Key, Key)]))>> = Mutex::new(Vec::new());
    let mut lists = LISTS.lock();

    if let Some((_, (keys, forms))) = lists.iter().find(|(l, _)| *l == lang) {
        (keys.clone(), forms)
    } else {
        let mut forms = Vec::new();
        let capture_names = query.capture_names();
        let keys = Key::new_many(capture_names.len() * 2);

        let mut iter_keys = keys.clone();
        for name in capture_names {
            let start = iter_keys.next().unwrap();
            let end = iter_keys.next().unwrap();
            forms.push(if name.contains('.') {
                let (refed, _) = name.rsplit_once('.').unwrap();
                (form::set_weak(name, refed), start, end)
            } else {
                (form::set_weak(name, "Default"), start, end)
            });
        }

        lists.push((lang, (keys, forms.leak())));
        let (_, (keys, forms)) = lists.last().unwrap();
        (keys.clone(), forms)
    }
}

impl<'a> TextProvider<&'a [u8]> for TsBuf<'a> {
    type I = std::array::IntoIter<&'a [u8], 2>;

    fn text(&mut self, node: tree_sitter::Node) -> Self::I {
        let range = node.range();
        let (s0, s1) = self.0.range(range.start_byte..range.end_byte).as_slices();

        [s0, s1].into_iter()
    }
}

#[derive(Clone, Copy)]
struct TsBuf<'a>(&'a GapBuffer<u8>);

#[allow(unused)]
#[cfg(debug_assertions)]
fn log_node(node: tree_sitter::Node) {
    use std::fmt::Write;

    let mut cursor = node.walk();
    let mut node = Some(cursor.node());
    let mut log = String::new();
    while let Some(no) = node {
        let indent = " ".repeat(cursor.depth() as usize);
        writeln!(log, "{indent}{no:?}").unwrap();
        let mut next_exists = cursor.goto_first_child() || cursor.goto_next_sibling();
        while !next_exists && cursor.goto_parent() {
            next_exists = cursor.goto_next_sibling();
        }
        node = next_exists.then_some(cursor.node());
    }

    crate::log_file!("{log}");
}

fn lang_from_path(
    path: impl AsRef<Path>,
) -> Option<(&'static str, &'static Language, [&'static str; 2])> {
    type Lang<'a> = ((&'a str, &'a str, &'a str), &'a Language, [&'a str; 2]);
    static LANGUAGES: LazyLock<Mutex<Vec<Lang>>> = LazyLock::new(|| {
        macro l($lang:ident) {
            Box::leak(Box::new($lang::LANGUAGE.into()))
        }
        macro lf($lang:ident) {
            Box::leak(Box::new($lang::language()))
        }
        macro h($lang:ident) {{
            let hi = include_str!(concat!(
                "../../../ts-queries/",
                stringify!($lang),
                "/highlights.scm"
            ));
            [hi, ""]
        }}
        macro i($lang:ident) {
            include_str!(concat!(
                "../../../ts-queries/",
                stringify!($lang),
                "/indents.scm"
            ))
        }
        macro h_i($lang:ident) {
            [h!($lang)[0], i!($lang)]
        }

        let lang_ocaml = Box::leak(Box::new(ts_ocaml::LANGUAGE_OCAML.into()));
        let lang_php = Box::leak(Box::new(ts_php::LANGUAGE_PHP_ONLY.into()));
        let lang_ts = Box::leak(Box::new(ts_ts::LANGUAGE_TYPESCRIPT.into()));
        let lang_xml = Box::leak(Box::new(ts_xml::LANGUAGE_XML.into()));

        Mutex::new(vec![
            //        (".cl", "Common Lisp", "common-lisp"),
            //        (".clj", "Clojure", "clojure"),
            //        (".comp", "GLSL", "glsl"),
            //        (".frag", "GLSL", "glsl"),
            //        (".geom", "GLSL", "glsl"),
            //        (".glsl", "GLSL", "glsl"),
            //        (".haml", "Haml", "haml"),
            //        (".handlebars", "Handlebars", "handlebars"),
            //        (".hbs", "Handlebars", "handlebars"),
            //        (".hlsl", "HLSL", "HLSL"),
            //        (".ini", "INI", "ini"),
            //        (".jinja2", "Jinja", "jinja"),
            //        (".jinja", "Jinja", "jinja"),
            //        (".kt", "Kotlin", "kotlin"),
            //        (".less", "Less", "less"),
            //        (".pl", "Perl", "perl"),
            //        (".rkt", "Racket", "racket"),
            //        (".sass", "SASS", "sass"),
            //        (".tesc", "GLSL", "glsl"),
            //        (".tese", "GLSL", "glsl"),
            //        (".tex", "TeX", "tex"),
            //        (".vert", "GLSL", "glsl"),
            //        (".xhtml", "XHTML", "xhtml"),
            (("asm", "Assembly", "assembly"), l!(ts_asm), h!(asm)),
            (("c", "C", "c"), l!(ts_c), h_i!(c)),
            (("cc", "C++", "cpp"), l!(ts_cpp), h_i!(cpp)),
            (("cpp", "C++", "cpp"), l!(ts_cpp), h_i!(cpp)),
            (("cs", "C#", "csharp"), l!(ts_c_sharp), h!(c_sharp)),
            (("css", "CSS", "css"), l!(ts_css), h_i!(css)),
            (("cxx", "C++", "cpp"), l!(ts_cpp), h_i!(cpp)),
            (("dart", "Dart", "dart"), lf!(ts_dart), h_i!(dart)),
            (("erl", "Erlang", "erlang"), l!(ts_erlang), h!(erlang)),
            (("ex", "Elixir", "elixir"), l!(ts_elixir), h_i!(elixir)),
            (("exs", "Elixir", "elixir"), l!(ts_elixir), h_i!(elixir)),
            (("for", "Fortran", "fortran"), l!(ts_fortran), h_i!(fortran)),
            (("fpp", "Fortran", "fortran"), l!(ts_fortran), h_i!(fortran)),
            (("gleam", "Gleam", "gleam"), l!(ts_gleam), h_i!(gleam)),
            (("go", "Go", "go"), l!(ts_go), h_i!(go)),
            (("groovy", "Groovy", "groovy"), l!(ts_groovy), h_i!(groovy)),
            (("gvy", "Groovy", "groovy"), l!(ts_groovy), h_i!(groovy)),
            (("h", "C", "c"), l!(ts_c), h_i!(c)),
            (("hpp", "C++", "cpp"), l!(ts_cpp), h_i!(cpp)),
            (("hrl", "Erlang", "erlang"), l!(ts_erlang), h!(erlang)),
            (("hs", "Haskell", "haskell"), l!(ts_haskell), h!(haskell)),
            (("hsc", "Haskell", "haskell"), l!(ts_haskell), h!(haskell)),
            (("htm", "HTML", "html"), l!(ts_html), h_i!(html)),
            (("html", "HTML", "html"), l!(ts_html), h_i!(html)),
            (("hxx", "C++", "cpp"), l!(ts_cpp), h_i!(cpp)),
            (("java", "Java", "java"), l!(ts_java), h_i!(java)),
            (("jl", "Julia", "julia"), l!(ts_julia), h_i!(julia)),
            (("js", "JavaScript", "javascript"), l!(ts_js), h_i!(js)),
            (("json", "JSON", "json"), l!(ts_json), h_i!(json)),
            (("jsonc", "JSON", "jsonc"), l!(ts_json), h_i!(json)),
            (("lua", "Lua", "lua"), l!(ts_lua), h_i!(lua)),
            (("m", "Objective-C", "objc"), l!(ts_objc), h_i!(objc)),
            (("md", "Markdown", "markdown"), l!(ts_md), h_i!(markdown)),
            (("ml", "OCaml", "ocaml"), lang_ocaml, h_i!(ocaml)),
            (("nix", "Nix", "nix"), l!(ts_nix), h_i!(nix)),
            (("php", "PHP", "php"), lang_php, h_i!(php)),
            (("py", "Python", "python"), l!(ts_python), h_i!(python)),
            (("pyc", "Python", "python"), l!(ts_python), h_i!(python)),
            (("pyo", "Python", "python"), l!(ts_python), h_i!(python)),
            (("r", "R", "r"), l!(ts_r), h_i!(r)),
            (("rb", "Ruby", "ruby"), l!(ts_ruby), h_i!(ruby)),
            (("rs", "Rust", "rust"), l!(ts_rust), h_i!(rust)),
            (("sc", "Scala", "scala"), l!(ts_scala), h!(scala)),
            (("scala", "Scala", "scala"), l!(ts_scala), h!(scala)),
            (("scss", "SCSS", "scss"), lf!(ts_scss), h_i!(scss)),
            (("sh", "Shell", "shell"), l!(ts_bash), h!(bash)),
            (("sql", "SQL", "sql"), l!(ts_sequel), h_i!(sql)),
            (("swift", "Swift", "swift"), l!(ts_swift), h_i!(swift)),
            (("ts", "TypeScript", "typescript"), lang_ts, h!(ts)),
            (("vim", "Viml", "viml"), lf!(ts_vim), h!(vim)),
            (("xml", "XML", "xml"), lang_xml, h_i!(xml)),
            (("xrl", "Erlang", "erlang"), l!(ts_erlang), h!(erlang)),
            (("yaml", "YAML", "yaml"), l!(ts_yaml), h_i!(yaml)),
            (("yml", "YAML", "yaml"), l!(ts_yaml), h_i!(yaml)),
            (("yrl", "Erlang", "erlang"), l!(ts_erlang), h!(erlang)),
            (("zig", "Zig", "zig"), l!(ts_zig), h_i!(zig)),
        ])
    });

    let ext = path.as_ref().extension()?.to_str()?;
    let langs = LANGUAGES.lock();
    langs
        .binary_search_by(|((rhs, ..), ..)| rhs.cmp(&ext))
        .ok()
        .map(|i| {
            let ((_, name, _), lang, hl) = langs.get(i).unwrap();
            (*name, *lang, *hl)
        })
}
