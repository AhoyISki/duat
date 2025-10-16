//! A [tree-sitter] implementation for Duat
//!
//! `duat-treesitter` currently does two things:
//!
//! * Syntax highlighting
//! * Indentation calculation
//!
//! # Installation
//!
//! Just like other Duat plugins, this one can be installed by calling
//! `cargo add` in the config directory:
//!
//! ```bash
//! cargo add duat-treesitter@"*"
//! ```
//!
//! Or, if you are using a `--git-deps` version of duat, do this:
//!
//! ```bash
//! cargo add --git https://github.com/AhoyISki/duat-treesitter
//! ```
//!
//! But this is a default plugin, so you most likely won't have to do
//! that.
//!
//! [tree-sitter]: https://tree-sitter.github.io/tree-sitter
#![feature(closure_lifetime_binder)]
use std::{
    collections::HashMap,
    fs,
    ops::Range,
    path::PathBuf,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    file::{self, PathKind},
    form::FormId,
    mode::Cursor,
    prelude::*,
    text::{Builder, Bytes, Change, Matcheable, Point, Tags},
};
use duat_filetype::FileType;
use streaming_iterator::StreamingIterator;
use tree_sitter::{
    InputEdit, Language, Node, Parser, Point as TsPoint, Query, QueryCapture as QueryCap,
    QueryCursor, QueryMatch, TextProvider, Tree,
};

use self::{injections::InjectedTree, languages::parser_is_compiled};

mod cursor;
mod injections;
mod languages;

/// The [tree-sitter] plugin for Duat
///
/// For now, it adds syntax highlighting and indentation, but more
/// features will be coming in the future.
///
/// These things are done through the [`TsParser`] [`Parser`], which
/// reads updates the inner syntax tree when the [`Text`] reports any
/// changes.
///
/// # NOTE
///
/// If you are looking to create a [`Parser`] which can do similar
/// things, you should look at the code for the implementation of
/// [`Parser`] for [`TsParser`], it's relatively short and with good
/// explanations for what is happening.
///
/// [tree-sitter]: https://tree-sitter.github.io/tree-sitter
#[derive(Default)]
pub struct TreeSitter;

impl duat_core::Plugin for TreeSitter {
    fn plug(self, _: &Plugins) {
        const MAX_LEN_FOR_LOCAL: usize = 100_000;

        form::set_many_weak!(
            ("variable", Form::white()),
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
            ("punctuation.bracket", Form::grey()),
            ("punctuation.delimiter", Form::grey()),
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
            ("diff.plus", Form::red()),
            ("diff.delta", Form::blue()),
            ("diff.minus", Form::green()),
            ("node.field", "variable.member"),
        );

        hook::add_grouped::<File>("TreeSitter", |pa, handle| {
            let file = handle.write(pa);

            let path = file.path_kind();
            let filetype = if let PathKind::SetExists(path) | PathKind::SetAbsent(path) = &path
                && let Some(filetype) = path.filetype()
                && crate::languages::filetype_is_in_list(filetype)
            {
                filetype
            } else {
                context::debug!(
                    "No filetype set for [a]{}[], will try again once one is set",
                    path.name_txt()
                );
                return file.add_parser(|tracker| TsParser(Some(ParserState::NotSet(tracker))));
            };

            if parser_is_compiled(filetype)? && file.bytes().len().byte() <= MAX_LEN_FOR_LOCAL {
                let lang_parts = lang_parts_of(filetype)?;
                handle.add_parser(pa, |tracker| {
                    TsParser(Some(ParserState::Present(InnerTsParser::new(
                        lang_parts, tracker,
                    ))))
                })
            } else {
                handle.add_parser(pa, |tracker| {
                    TsParser(Some(ParserState::Remote(std::thread::spawn(move || {
                        let lang_parts = match lang_parts_of(filetype) {
                            Ok(lang_parts) => lang_parts,
                            Err(err) => {
                                context::error!("{err}");
                                return Err(tracker);
                            }
                        };

                        let mut parser = InnerTsParser::new(lang_parts, tracker);

                        while parser.parse() {}

                        parser.tracker.request_parse();
                        Ok(parser)
                    }))))
                })
            }
        });
    }
}

/// [`Parser`] that parses [`File`]'s as [tree-sitter] syntax trees
///
/// [tree-sitter]: https://tree-sitter.github.io/tree-sitter
pub struct TsParser(Option<ParserState>);

impl TsParser {
    /// The root [`Node`] of the syntax tree
    pub fn root(&self) -> Option<Node<'_>> {
        let Some(ParserState::Present(parser)) = &self.0 else {
            context::warn!("Called function that shouldn't be possible without present parser");
            return None;
        };

        Some(parser.tree.root_node())
    }

    /// Logs the root node with the [`context::debug`] macro
    pub fn debug_root(&self) {
        let Some(ParserState::Present(parser)) = &self.0 else {
            context::warn!("Called function that shouldn't be possible without present parser");
            return;
        };

        context::debug!("{}", format_root(parser.tree.root_node()));
    }

    /// Gets the requested indentation level on a given [`Point`]
    ///
    /// Will be [`None`] if the [`filetype`] hasn't been set yet or if
    /// there is no indentation query for this language.
    ///
    /// [`filetype`]: FileType::filetype
    pub fn indent_on(&self, p: Point, bytes: &Bytes, cfg: PrintCfg) -> Option<usize> {
        let Some(ParserState::Present(parser)) = &self.0 else {
            context::warn!("Called function that shouldn't be possible without present parser");
            return None;
        };

        parser.indent_on(p, bytes, cfg)
    }
}

impl file::Parser for TsParser {
    fn parse(&mut self) -> bool {
        // In this function, the changes will be applied and the Ranges will
        // be updated to include the following regions to be updated:
        //
        // - The ranges returned by Parser::changed_ranges,
        // - All ranges where an injection was added or removed, which will be
        //   acquired through the injections query, applied on the two
        //   previous range lists,
        let parser_state = self.0.take().unwrap();
        let (parser_state, do_update) = parser_state.parse();
        self.0 = Some(parser_state);

        do_update
    }

    fn update(&mut self, pa: &mut Pass, file: &Handle<File>, on: Vec<Range<Point>>) {
        match self.0.as_mut().unwrap() {
            ParserState::Present(parser) => {
                let mut parts = file.write(pa).text_mut().parts();

                for range in on {
                    let range = range.start.byte()..range.end.byte();
                    parser.highlight_and_inject(parts.bytes, &mut parts.tags, range);
                }
            }
            ParserState::Remote(..) => {
                context::warn!("Tried updating parser while it is still remote");
            }
            _ => (),
        }
    }

    fn before_get(&mut self) {
        self.parse();
    }

    fn before_try_get(&mut self) -> bool {
        let parser_state = self.0.take().unwrap();

        if let ParserState::Remote(join_handle) = parser_state {
            if join_handle.is_finished() {
                match join_handle.join().unwrap() {
                    Ok(parser) => {
                        self.0 = Some(ParserState::Present(parser));
                        self.parse()
                    }
                    Err(tracker) => {
                        self.0 = Some(ParserState::NotSet(tracker));
                        false
                    }
                }
            } else {
                self.0 = Some(ParserState::Remote(join_handle));
                false
            }
        } else {
            self.0 = Some(parser_state);
            true
        }
    }
}

struct InnerTsParser {
    parser: Parser,
    lang_parts: LangParts<'static>,
    forms: &'static [(FormId, u8)],
    tree: Tree,
    old_tree: Option<Tree>,
    injections: Vec<InjectedTree>,
    tracker: FileTracker,
}

impl InnerTsParser {
    /// Returns a new [`InnerTsParser`]
    fn new(lang_parts: LangParts<'static>, tracker: FileTracker) -> InnerTsParser {
        let (.., lang, _) = &lang_parts;
        let forms = forms_from_lang_parts(lang_parts);

        let mut parser = Parser::new();
        parser.set_language(lang).unwrap();

        let tree = parser
            .parse_with_options(&mut parser_fn(tracker.bytes()), None, None)
            .unwrap();

        InnerTsParser {
            parser,
            lang_parts,
            forms,
            tree,
            old_tree: None,
            injections: Vec::new(),
            tracker,
        }
    }

    /// Parse the newest changes, returns `false` if there were none
    fn parse(&mut self) -> bool {
        self.tracker.update();
        let bytes = self.tracker.bytes();
        let moment = self.tracker.moment();

        if moment.is_empty() {
            return false;
        }

        // These new ranges will be used for calculating things like
        // new injections, for example.
        let mut new_ranges = Ranges::empty();

        for change in moment.changes() {
            let input_edit = input_edit(change, bytes);
            self.tree.edit(&input_edit);

            for inj in self.injections.iter_mut() {
                inj.edit(&input_edit);
            }
            let range = change.line_points(bytes);
            new_ranges.add(range.start.byte()..range.end.byte());
        }

        let tree = self
            .parser
            .parse_with_options(&mut parser_fn(bytes), Some(&self.tree), None)
            .unwrap();

        self.old_tree = Some(std::mem::replace(&mut self.tree, tree));

        for inj in self.injections.iter_mut() {
            inj.update_tree(bytes);
        }

        // `changed_ranges` should mostly be able to catch any big additions
        // to the tree structure.
        for range in self
            .old_tree
            .as_ref()
            .unwrap()
            .changed_ranges(&self.tree)
            .chain(
                self.injections
                    .iter()
                    .flat_map(InjectedTree::changed_ranges),
            )
        {
            // The rows seem kind of unpredictable, which is why I have to do this
            // nonsense
            let start = bytes.point_at_line(bytes.point_at_byte(range.start_byte).line());
            let [_, end] = bytes.points_of_line(
                bytes
                    .point_at_byte(range.end_byte.min(bytes.len().byte()))
                    .line(),
            );

            new_ranges.add(start.byte()..end.byte())
        }

        // Finally, in order to properly catch injection changes, a final
        // comparison is done between the old tree and the new tree, in
        // regards to injection captures. This is done on every range in
        // new_ranges.
        refactor_injections(
            &mut new_ranges,
            (self.lang_parts, &mut self.injections),
            (self.old_tree.as_ref(), &self.tree),
            bytes,
        );

        self.tracker.add_ranges(new_ranges);

        true
    }

    /// Highlights and injects based on the [`LangParts`] queries
    fn highlight_and_inject(&mut self, bytes: &Bytes, tags: &mut Tags, range: Range<usize>) {
        tags.remove(ts_tagger(), range.clone());

        highlight_and_inject(
            self.tree.root_node(),
            &mut self.injections,
            (self.lang_parts, self.forms),
            (bytes, tags),
            range.start.saturating_sub(1)..(range.end + 1).min(bytes.len().byte()),
        );
    }

    ////////// Querying functions

    /// The expected level of indentation on a given [`Point`]
    fn indent_on(&self, p: Point, bytes: &Bytes, cfg: PrintCfg) -> Option<usize> {
        let start = bytes.point_at_line(p.line());

        let (root, indents, range) = self
            .injections
            .iter()
            .find_map(|inj| inj.get_injection_indent_parts(start.byte()))
            .unwrap_or((
                self.tree.root_node(),
                self.lang_parts.2.indents,
                0..bytes.len().byte(),
            ));

        let first_line = bytes.point_at_byte(range.start).line();

        // The query could be empty.
        if indents.pattern_count() == 0 {
            return None;
        }

        // TODO: Don't reparse python, apparently.

        type Captures<'a> = HashMap<&'a str, HashMap<usize, HashMap<&'a str, Option<&'a str>>>>;
        let mut caps = HashMap::new();
        let q = {
            let mut cursor = QueryCursor::new();
            let buf = TsBuf(bytes);
            cursor
                .matches(indents, root, buf)
                .for_each(|qm: &QueryMatch| {
                    for cap in qm.captures.iter() {
                        let Some(cap_end) =
                            indents.capture_names()[cap.index as usize].strip_prefix("indent.")
                        else {
                            continue;
                        };

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
        let indented_start = bytes
            .chars_fwd(start..)
            .unwrap()
            .take_while(|(p, _)| p.line() == start.line())
            .find_map(|(p, c)| (!c.is_whitespace()).then_some(p));

        let mut opt_node = if let Some(indented_start) = indented_start {
            Some(descendant_in(root, indented_start.byte()))
        // If the line is empty, look behind for another.
        } else {
            // Find last previous empty line.
            let mut lines = bytes.lines(..start).rev();
            let Some((prev_l, line)) = lines
                .find(|(_, line)| !(line.reg_matches(r"^\s*$", ..).unwrap()))
                .filter(|(l, _)| *l >= first_line)
            else {
                // If there is no previous non empty line, align to 0.
                return Some(0);
            };
            let trail = line.chars().rev().take_while(|c| c.is_whitespace()).count();

            let [prev_start, prev_end] = bytes.points_of_line(prev_l);
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

        let tab = cfg.tab_stops.size() as i32;
        let mut indent = if root.start_byte() != 0 {
            bytes.indent(bytes.point_at_byte(root.start_byte()), cfg) as i32
        } else {
            0
        };

        let mut processed_lines = Vec::new();
        while let Some(node) = opt_node {
            let s_line = node.start_position().row;
            let e_line = node.end_position().row;

            // If a node is not an indent and is marked as auto or ignore, act
            // accordingly.
            if !q(&caps, node, &["begin"]) && s_line < p.line() && p.line() <= e_line {
                if !q(&caps, node, &["align"]) && q(&caps, node, &["auto"]) {
                    return None;
                } else if q(&caps, node, &["ignore"]) {
                    return Some(0);
                }
            }

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

            let fd = for<'a, 'b> |node: Node<'a>, delim: &'b str| -> (Option<Node<'a>>, bool) {
                let mut c = node.walk();
                let child = node.children(&mut c).find(|child| child.kind() == delim);
                let ret = child.map(|child| {
                    let [_, end] = bytes.points_of_line(child.start_position().row);
                    let range = child.range().start_byte..end.byte();

                    let is_last_in_line = if let Some(line) = bytes.get_contiguous(range.clone()) {
                        line.split_whitespace().any(|w| w != delim)
                    } else {
                        let line = bytes.buffers(range).try_to_string().unwrap();
                        line.split_whitespace().any(|w| w != delim)
                    };

                    (child, is_last_in_line)
                });
                let (child, is_last_in_line) = ret.unzip();
                (child, is_last_in_line.unwrap_or(false))
            };

            if should_process
                && q(&caps, node, &["align"])
                && (s_line != e_line || is_in_err)
                && s_line != p.line()
            {
                let props = &caps["align"][&node.id()];
                let (o_delim_node, o_is_last_in_line) = props
                    .get(&"open_delimiter")
                    .and_then(|delim| delim.map(|d| fd(node, d)))
                    .unwrap_or((Some(node), false));
                let (c_delim_node, c_is_last_in_line) = props
                    .get(&"close_delimiter")
                    .and_then(|delim| delim.map(|d| fd(node, d)))
                    .unwrap_or((Some(node), false));

                if let Some(o_delim_node) = o_delim_node {
                    let o_s_line = o_delim_node.start_position().row;
                    let o_s_col = o_delim_node.start_position().column;
                    let c_s_line = c_delim_node.map(|n| n.start_position().row);

                    // If the previous line was marked with an open_delimiter, treat it
                    // like an indent.
                    let indent_is_absolute = if o_is_last_in_line && should_process {
                        indent += tab;
                        // If the aligned node ended before the current line, its @align
                        // shouldn't affect it.
                        if c_is_last_in_line && c_s_line.is_some_and(|l| l < p.line()) {
                            indent = (indent - tab).max(0);
                        }
                        false
                    // Aligned indent
                    } else if c_is_last_in_line
                        && let Some(c_s_line) = c_s_line
                        // If the aligned node ended before the current line, its @align
                        // shouldn't affect it.
                        && (o_s_line != c_s_line && c_s_line < p.line())
                    {
                        indent = (indent - tab).max(0);
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

        // indent < 0 means "keep level of indentation"
        (indent >= 0).then_some(indent as usize)
    }
}

impl std::fmt::Debug for InnerTsParser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TsParser")
            .field("tree", &self.tree)
            .field("old_tree", &self.old_tree)
            .field("injections", &self.injections)
            .finish_non_exhaustive()
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

type LangParts<'a> = (&'a str, &'a Language, Queries<'a>);

#[derive(Clone, Copy)]
struct Queries<'a> {
    highlights: &'a Query,
    indents: &'a Query,
    injections: &'a Query,
}

enum ParserState {
    Present(InnerTsParser),
    Remote(std::thread::JoinHandle<RemoteResult>),
    NotSet(FileTracker),
}

impl ParserState {
    fn parse(self) -> (Self, bool) {
        match self {
            ParserState::Present(mut parser) => {
                parser.parse();
                (ParserState::Present(parser), true)
            }
            ParserState::Remote(join_handle) => {
                if join_handle.is_finished() {
                    match join_handle.join().unwrap() {
                        Ok(mut parser) => {
                            parser.parse();
                            (ParserState::Present(parser), true)
                        }
                        Err(tracker) => (ParserState::NotSet(tracker), false),
                    }
                } else {
                    (ParserState::Remote(join_handle), false)
                }
            }
            ParserState::NotSet(tracker) => (ParserState::NotSet(tracker), false),
        }
    }
}

#[track_caller]
fn descendant_in(node: Node, byte: usize) -> Node {
    node.descendant_for_byte_range(byte, byte + 1).unwrap()
}

fn parser_fn<'a>(bytes: &'a Bytes) -> impl FnMut(usize, TsPoint) -> &'a [u8] {
    let [s0, s1] = bytes.buffers(..).to_array();
    |byte, _point| {
        if byte < s0.len() {
            &s0[byte..]
        } else {
            &s1[byte - s0.len()..]
        }
    }
}

fn ts_point(point: Point, buffer: &Bytes) -> TsPoint {
    let strs = buffer.buffers(..point.byte());
    let iter = strs.into_iter().rev();
    let col = iter.take_while(|&b| b != b'\n').count();

    TsPoint::new(point.line(), col)
}

fn ts_point_from(to: Point, (col, from): (usize, Point), str: &str) -> TsPoint {
    let col = if to.line() == from.line() {
        col + str.len()
    } else {
        str.bytes().rev().take_while(|&b| b != b'\n').count()
    };

    TsPoint::new(to.line(), col)
}

fn forms_from_lang_parts(
    (lang, _, Queries { highlights, .. }): LangParts<'static>,
) -> &'static [(FormId, u8)] {
    #[rustfmt::skip]
    const PRIORITIES: &[&str] = &[
        "markup", "operator", "comment", "string", "diff", "variable", "module", "label",
        "character", "boolean", "number", "type", "attribute", "property", "function", "constant",
        "constructor", "keyword", "punctuation",
    ];
    type MemoizedForms<'a> = HashMap<&'a str, &'a [(FormId, u8)]>;

    static LISTS: LazyLock<Mutex<MemoizedForms<'static>>> = LazyLock::new(Mutex::default);
    let mut lists = LISTS.lock().unwrap();

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

fn lang_parts_of(lang: &str) -> Result<LangParts<'static>, Text> {
    static MAPS: LazyLock<Mutex<HashMap<&str, LangParts<'static>>>> = LazyLock::new(Mutex::default);

    let mut maps = MAPS.lock().unwrap();

    Ok(if let Some(lang_parts) = maps.get(lang).copied() {
        lang_parts
    } else {
        let language: &'static Language = Box::leak(Box::new(languages::get_language(lang)?));

        let highlights = query_from_path(lang, "highlights", language)?;
        let indents = query_from_path(lang, "indents", language)?;
        let injections = query_from_path(lang, "injections", language)?;

        let queries = Queries { highlights, indents, injections };

        let lang = lang.to_string().leak();

        maps.insert(lang, (lang, language, queries));

        (lang, language, queries)
    })
}

/// The Key for tree-sitter
fn ts_tagger() -> Tagger {
    static TAGGER: LazyLock<Tagger> = Tagger::new_static();
    *TAGGER
}

fn input_edit(change: Change<&str>, bytes: &Bytes) -> InputEdit {
    let start = change.start();
    let added = change.added_end();
    let taken = change.taken_end();

    let ts_start = ts_point(start, bytes);
    let ts_taken_end = ts_point_from(taken, (ts_start.column, start), change.taken_str());
    let ts_added_end = ts_point_from(added, (ts_start.column, start), change.added_str());

    InputEdit {
        start_byte: start.byte(),
        old_end_byte: taken.byte(),
        new_end_byte: added.byte(),
        start_position: ts_start,
        old_end_position: ts_taken_end,
        new_end_position: ts_added_end,
    }
}

/// Returns a new [`Query`] for a given language and kind
///
/// If the [`Query`] in question does not exist, returns an emtpy
/// [`Query`] instead.
fn query_from_path(name: &str, kind: &str, language: &Language) -> Result<&'static Query, Text> {
    static QUERIES: LazyLock<Mutex<HashMap<PathBuf, &'static Query>>> =
        LazyLock::new(Mutex::default);

    let queries_dir = duat_core::utils::plugin_dir("duat-treesitter")?.join("queries");

    let path = queries_dir.join(name).join(kind).with_extension("scm");

    let mut queries = QUERIES.lock().unwrap();

    Ok(if let Some(query) = queries.get(&path) {
        query
    } else {
        let Ok(mut query) = fs::read_to_string(&path) else {
            let query = Box::leak(Box::new(Query::new(language, "").unwrap()));
            queries.insert(path, query);
            return Ok(query);
        };

        let Some(first_line) = query.lines().map(String::from).next() else {
            context::warn!(target: path.to_str().unwrap(), "Query is empty");
            let query = Box::leak(Box::new(Query::new(language, "").unwrap()));
            queries.insert(path, query);
            return Ok(query);
        };

        if let Some(langs) = first_line.strip_prefix("; inherits: ") {
            for name in langs.split(',') {
                let path = queries_dir.join(name).join(kind).with_extension("scm");
                match fs::read_to_string(&path) {
                    Ok(inherited_query) => {
                        if inherited_query.is_empty() {
                            let target = path.to_str().unwrap();
                            context::warn!(target: target, "Inherited query is empty");
                        }

                        query = format!("{inherited_query}\n{query}");
                    }
                    Err(err) => context::error!("{err}"),
                }
            }
        }

        let query = Box::leak(Box::new(match Query::new(language, &query) {
            Ok(query) => query,
            Err(err) => return Err(txt!("{err}").build()),
        }));

        queries.insert(path, query);

        query
    })
}

/// Convenience methods for use of tree-sitter in [`File`]s
pub trait TsFile {
    /// The level of indentation required at a certain [`Point`]
    ///
    /// This is determined by a query, currently, it is the query
    /// located in
    /// `"{plugin_dir}/duat-treesitter/queries/{lang}/indent.scm"`
    fn ts_indent_on(&self, p: Point) -> Option<usize>;
}

impl TsFile for File {
    fn ts_indent_on(&self, p: Point) -> Option<usize> {
        self.read_parser(|ts: &TsParser| ts.indent_on(p, self.text().bytes(), self.get_print_cfg()))
            .flatten()
    }
}

/// Convenience methods for use of tree-sitter in [`Cursor`]s
pub trait TsCursor {
    /// The level of indentation required at the [`Cursor`]'s `caret`
    ///
    /// This is determined by a query, currently, it is the query
    /// located in
    /// `"{plugin_dir}/duat-treesitter/queries/{lang}/indent.scm"`
    fn ts_indent(&self) -> Option<usize>;

    /// The level of indentation required at a certain [`Point`]
    ///
    /// This is determined by a query, currently, it is the query
    /// located in
    /// `"{plugin_dir}/duat-treesitter/queries/{lang}/indent.scm"`
    fn ts_indent_on(&self, p: Point) -> Option<usize>;

    /// Reindents the [`Cursor`]'s line
    ///
    /// This is determined by a query, currently, it is the query
    /// located in
    /// `"{plugin_dir}/duat-treesitter/queries/{lang}/indent.scm"`
    fn ts_reindent(&mut self);
}

impl<S> TsCursor for Cursor<'_, File, S> {
    fn ts_indent(&self) -> Option<usize> {
        self.ts_indent_on(self.caret())
    }

    fn ts_indent_on(&self, p: Point) -> Option<usize> {
        let cfg = self.cfg();

        self.read_parser(|ts: &TsParser| ts.indent_on(p, self.text().bytes(), cfg))
            .flatten()
    }

    fn ts_reindent(&mut self) {
        fn prev_non_empty_line_points<S>(c: &mut Cursor<File, S>) -> Option<[Point; 2]> {
            let byte_col = c
                .text()
                .buffers(..c.caret().byte())
                .take_while(|b| *b != b'\n')
                .count();
            let mut lines = c.lines_on(..c.caret().byte() - byte_col);
            let prev = lines.find_map(|(n, l): (usize, &str)| {
                l.chars().any(|c| !c.is_whitespace()).then_some(n)
            });
            prev.map(|n| c.text().points_of_line(n))
        }

        let old_col = self.v_caret().char_col();
        let anchor_existed = self.anchor().is_some();

        let old_indent = self.indent();
        let new_indent = if let Some(indent) = self.ts_indent() {
            indent
        } else {
            let prev_non_empty = prev_non_empty_line_points(self);
            prev_non_empty
                .map(|[p0, _]| self.indent_on(p0))
                .unwrap_or(0)
        };
        let indent_diff = new_indent as i32 - old_indent as i32;

        self.move_hor(-(old_col as i32));
        self.set_anchor();
        self.move_hor(old_indent as i32);

        if self.caret() == self.anchor().unwrap() {
            self.insert(" ".repeat(new_indent));
        } else {
            self.move_hor(-1);
            self.replace(" ".repeat(new_indent));
        }
        self.set_caret_on_start();
        self.unset_anchor();

        if anchor_existed {
            self.set_anchor();
            if old_col < old_indent {
                self.move_hor(old_col as i32);
            } else {
                self.move_hor(old_col as i32 + indent_diff);
            }
            self.swap_ends();
        }

        if old_col < old_indent {
            self.move_hor(old_col as i32);
        } else {
            self.move_hor(old_col as i32 + indent_diff);
        }
    }
}

#[allow(unused)]
fn format_root(node: Node) -> Text {
    fn format_range(node: Node, builder: &mut Builder) {
        let mut first = true;
        for point in [node.start_position(), node.end_position()] {
            builder.push(txt!(
                "[punctuation.bracket.TreeView][[[coords.TreeView]{}\
             	 [punctuation.delimiter.TreeView],[] [coords.TreeView]{}\
             	 [punctuation.bracket.TreeView]]]",
                point.row,
                point.column
            ));

            if first {
                first = false;
                builder.push(txt!("[punctuation.delimiter],[] "));
            }
        }
        builder.push("\n");
    }

    fn format_node(
        node: Node,
        depth: usize,
        pars: usize,
        builder: &mut Builder,
        name: Option<&str>,
    ) {
        builder.push("  ".repeat(depth));

        if let Some(name) = name {
            builder.push(txt!("[node.field]{name}[punctuation.delimiter.TreeView]: "));
        }

        builder.push(txt!("[punctuation.bracket.TreeView]("));
        builder.push(txt!("[node.name]{}", node.grammar_name()));

        let mut cursor = node.walk();
        let named_children = node.named_children(&mut cursor);
        let len = named_children.len();

        if len == 0 {
            builder.push(txt!(
                "[punctuation.bracket.TreeView]{}[] ",
                ")".repeat(pars)
            ));
            format_range(node, builder);
        } else {
            builder.push(" ");
            format_range(node, builder);

            let mut i = 0;

            for (i, child) in named_children.enumerate() {
                let name = node.field_name_for_named_child(i as u32);
                let pars = if i == len - 1 { pars + 1 } else { 1 };
                format_node(child, depth + 1, pars, builder, name);
            }
        }
    }

    let mut cursor = node.walk();
    let mut builder = Text::builder();

    format_node(node, 0, 1, &mut builder, None);

    builder.build()
}

fn highlight_and_inject(
    root: Node,
    injected_trees: &mut Vec<InjectedTree>,
    (lang_parts, forms): (LangParts<'static>, &'static [(FormId, u8)]),
    (bytes, tags): (&Bytes, &mut Tags),
    range: Range<usize>,
) {
    let tagger = ts_tagger();
    let (.., Queries { highlights, injections, .. }) = &lang_parts;

    let mut cursor = QueryCursor::new();
    cursor.set_byte_range(range.clone());
    let buf = TsBuf(bytes);

    let cn = injections.capture_names();
    let is_content = |cap: &&QueryCap| cn[cap.index as usize] == "injection.content";
    let is_language = |cap: &&QueryCap| cn[cap.index as usize] == "injection.language";

    let mut new_langs: Vec<(LangParts<'static>, Ranges)> = Vec::new();

    let mut inj_captures = cursor.captures(injections, root, buf);
    while let Some((qm, _)) = inj_captures.next() {
        let Some(cap) = qm.captures.iter().find(is_content) else {
            continue;
        };
        let cap_range = cap.node.byte_range();

        let props = injections.property_settings(qm.pattern_index);
        let Some(lang) = props
            .iter()
            .find_map(|p| {
                (p.key.as_ref() == "injection.language")
                    .then_some(p.value.as_ref().unwrap().to_string())
            })
            .or_else(|| {
                let cap = qm.captures.iter().find(is_language)?;
                Some(
                    bytes
                        .buffers(cap.node.byte_range())
                        .try_to_string()
                        .unwrap(),
                )
            })
        else {
            continue;
        };

        let Ok(mut lang_parts) = lang_parts_of(&lang) else {
            continue;
        };

        // You may want to set a new injections query, only for this capture.
        if let Some(prop) = props.iter().find(|p| p.key.as_ref() == "injection.query")
            && let Some(value) = prop.value.as_ref()
        {
            match query_from_path(&lang, value, lang_parts.1) {
                Ok(injections) => {
                    lang_parts.2.injections = injections;
                }
                Err(err) => context::error!("{err}"),
            }
        };

        if let Some(inj) = injected_trees
            .iter_mut()
            .find(|inj| inj.lang_parts().0 == lang_parts.0)
        {
            inj.add_range(cap_range.clone());
        } else if let Some(new) = new_langs.iter_mut().find(|(lp, _)| lp.0 == lang_parts.0) {
            new.1.add(cap_range);
        } else {
            new_langs.push((lang_parts, Ranges::new(cap_range)));
        }
    }

    for (lang_parts, ranges) in new_langs {
        injected_trees.push(InjectedTree::new(bytes, lang_parts, ranges));
    }

    injected_trees.retain_mut(|inj| {
        if inj.is_empty() {
            false
        } else {
            inj.update_tree(bytes);
            inj.highlight_and_inject(bytes, tags, range.clone());
            true
        }
    });

    let mut hi_captures = cursor.captures(highlights, root, buf);
    while let Some((qm, _)) = hi_captures.next() {
        let qm: &QueryMatch = qm;
        for cap in qm.captures.iter() {
            let ts_range = cap.node.range();

            // Assume that an empty range must take up the whole line
            // Cuz sometimes it be like that
            let (form, priority) = forms[cap.index as usize];
            let range = ts_range.start_byte..ts_range.end_byte;
            tags.insert(tagger, range, form.to_tag(priority));
        }
    }
}

/// Figures out injection changes
///
/// This function does not actually modify the injections (beyond
/// removing deinjected areas), as that will be done by the
/// highlight_and_inject function.
///
/// Its main purpose is to find the regions where changes have taken
/// place and add them to the ranges to update.
fn refactor_injections(
    ranges: &mut Ranges,
    (lang_parts, injected_trees): (LangParts, &mut Vec<InjectedTree>),
    (old, new): (Option<&Tree>, &Tree),
    bytes: &Bytes,
) {
    let buf = TsBuf(bytes);
    let (.., Queries { injections, .. }) = lang_parts;
    let mut cursor = QueryCursor::new();

    let cn = injections.capture_names();
    let is_content = |cap: &&QueryCap| cn[cap.index as usize] == "injection.content";

    let mut inj_ranges = Ranges::empty();

    for range in ranges.iter() {
        cursor.set_byte_range(range.clone());

        if let Some(old) = old {
            let mut inj_captures = cursor.captures(injections, old.root_node(), buf);
            while let Some((qm, _)) = inj_captures.next() {
                if let Some(cap) = qm.captures.iter().find(is_content) {
                    inj_ranges.add(cap.node.byte_range());
                    for inj in injected_trees.iter_mut() {
                        inj.remove_range(cap.node.byte_range());
                    }
                }
            }
        }

        let mut inj_captures = cursor.captures(injections, new.root_node(), buf);
        while let Some((qm, _)) = inj_captures.next() {
            if let Some(cap) = qm.captures.iter().find(is_content) {
                inj_ranges.add(cap.node.byte_range());
            }
        }
    }

    for inj in injected_trees.iter_mut() {
        inj.refactor_injections(ranges, bytes);
    }

    ranges.merge(inj_ranges);
}

type RemoteResult = Result<InnerTsParser, FileTracker>;
