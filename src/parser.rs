use std::{
    collections::HashMap,
    ops::{ControlFlow, Range},
    sync::{LazyLock, Mutex},
    time::{Duration, Instant},
};

use duat_core::{
    Ranges,
    buffer::{Buffer, BufferParts, BufferTracker, Change, PerBuffer},
    context::{self, Handle},
    data::Pass,
    form::{self, FormId},
    hook::{self, BufferUpdated},
    lender::Lender,
    opts::PrintOpts,
    text::{Bytes, Point, RegexHaystack, Tagger},
};
use duat_filetype::{FileType, PassFileType};
use tree_sitter::{
    InputEdit, Node, ParseOptions, ParseState, Parser as TsParser, Point as TsPoint, QueryCapture,
    QueryCursor, QueryMatch, QueryProperty, Range as TsRange, StreamingIterator, TextProvider,
};

use crate::{
    LangParts, Queries, lang_parts_of, languages::parser_is_compiled, query_from_path, tree::Trees,
};

const PARSE_TIMEOUT: Duration = Duration::from_millis(3);
static TRACKER: BufferTracker = BufferTracker::new();
static PARSERS: PerBuffer<Parser> = PerBuffer::new();

macro_rules! return_err {
    ($result:expr) => {{
        match $result {
            Ok(ok) => ok,
            Err(err) => {
                context::error!("{err}");
                return;
            }
        }
    }};
}

pub(crate) fn add_parser_hook() {
    fn async_parse(pa: &mut Pass, handle: &Handle, printed_lines: &[Range<usize>]) -> bool {
        if let Some(filetype) = handle.filetype(pa)
            && let Some((parser, buffer)) = PARSERS.write(pa, handle)
            && parser.lang_parts.0 == filetype
        {
            let visible_ranges = get_visible_ranges(printed_lines);
            let mut parts = TRACKER.parts(buffer).unwrap();

            apply_changes(&parts, parser);

            if !parser.parse(&mut parts, &visible_ranges, false) {
                let handle = handle.clone();
                let printed_lines = printed_lines.to_vec();
                context::queue(move |pa| _ = async_parse(pa, &handle, &printed_lines))
            }

            for range in parts
                .ranges_to_update
                .select_from(printed_lines.iter().cloned())
            {
                parser.highlight(range.clone(), &mut parts);
                parts.ranges_to_update.update_on([range]);
            }

            true
        } else {
            false
        }
    }

    hook::add::<BufferUpdated>(|pa, handle| {
        let printed_lines = handle.printed_line_ranges(pa);
        if async_parse(pa, handle, &printed_lines) {
            return;
        }

        let Some(filetype) = handle.read(pa).filetype() else {
            return;
        };
        let Some(parser_is_compiled) = parser_is_compiled(filetype) else {
            return;
        };

        if parser_is_compiled {
            let lang_parts = return_err!(lang_parts_of(filetype));
            let len_bytes = handle.text(pa).len().byte();

            let mut parser = TsParser::new();
            parser.set_language(lang_parts.1).unwrap();

            TRACKER.register_buffer(handle.write(pa));
            PARSERS.register(pa, handle, Parser {
                parser,
                trees: Trees::new([Ranges::new(0..len_bytes)]),
                n_trees_to_parse: 1,
                lang_parts,
                forms: forms_from_lang_parts(lang_parts),
                injections: Vec::new(),
                ranges_to_inject: Ranges::new(0..len_bytes),
            });

            async_parse(pa, handle, &printed_lines);
        } else {
            let handle = handle.clone();
            std::thread::spawn(move || {
                if lang_parts_of(filetype).is_ok() {
                    handle.request_update();
                }
            });
        }
    });
}

pub struct Parser {
    parser: TsParser,
    trees: Trees,
    n_trees_to_parse: usize,
    lang_parts: LangParts<'static>,
    forms: &'static [(FormId, u8)],
    ranges_to_inject: Ranges,
    injections: Vec<Parser>,
}

impl Parser {
    /// Returns the root [`Node`] of the tree sitter `Parser`
    pub fn root_node(&self) -> Node<'_> {
        let tree = self.trees.iter().next().unwrap();
        tree.ts_tree.as_ref().unwrap().root_node()
    }

    fn parse(
        &mut self,
        parts: &mut BufferParts,
        visible_ranges: &[Range<usize>],
        force: bool,
    ) -> bool {
        let start = Instant::now();

        if self.n_trees_to_parse > 0 {
            let mut parsed_at_least_one_region = false;

            for range in visible_ranges.iter() {
                let Some(parsed_a_tree) = self.parse_trees(start, range.clone(), parts, force)
                else {
                    return false;
                };

                parsed_at_least_one_region |= parsed_a_tree;
            }

            if parsed_at_least_one_region {
                self.ranges_to_inject.add(0..parts.bytes.len().byte());
            }
        }

        let ranges_to_inject: Vec<Range<usize>> = visible_ranges
            .iter()
            .flat_map(|range| self.ranges_to_inject.iter_over(range.clone()))
            .collect();

        for range in ranges_to_inject {
            self.inject(range, parts);
            if !force && must_yield(start) {
                return false;
            }
        }

        for injection in self.injections.iter_mut() {
            if !injection.parse(parts, visible_ranges, force) {
                return false;
            }
        }

        true
    }

    fn parse_trees(
        &mut self,
        start: Instant,
        range: Range<usize>,
        parts: &mut BufferParts,
        force: bool,
    ) -> Option<bool> {
        let mut callback = |_: &ParseState| match force || !must_yield(start) {
            true => ControlFlow::Continue(()),
            false => ControlFlow::Break(()),
        };

        let ts_range = |range: Range<usize>| TsRange {
            start_byte: range.start,
            end_byte: range.end,
            start_point: ts_point(parts.bytes.point_at_byte(range.start), parts.bytes),
            end_point: ts_point(parts.bytes.point_at_byte(range.end), parts.bytes),
        };

        let mut parsed_at_least_one_region = false;
        let mut parsing_failed = false;

        for (_, tree) in self.trees.intersecting_mut(range.clone()) {
            if !tree.needs_parse {
                continue;
            }

            // Almost all injections fall here.
            if tree.region.len() == 1 {
                let ts_range = ts_range(tree.region.iter().next().unwrap());
                self.parser.set_included_ranges(&[ts_range]).unwrap();
            } else {
                let ts_ranges: Vec<_> = tree.region.iter().map(ts_range).collect();
                self.parser.set_included_ranges(&ts_ranges).unwrap();
            }

            let Some(new_ts_tree) = self.parser.parse_with_options(
                &mut parser_fn(parts.bytes),
                tree.ts_tree.as_ref(),
                Some(ParseOptions::new().progress_callback(&mut callback)),
            ) else {
                parsing_failed = true;
                break;
            };

            if let Some(ts_tree) = tree.ts_tree.as_mut() {
                parts.ranges_to_update.add_ranges(
                    ts_tree
                        .changed_ranges(&new_ts_tree)
                        .map(|r| r.start_byte..r.end_byte),
                );

                *ts_tree = new_ts_tree;
            } else {
                parts.ranges_to_update.add_ranges(tree.region.iter());
                tree.ts_tree = Some(new_ts_tree);
            }

            tree.needs_parse = false;
            self.n_trees_to_parse -= 1;
            parsed_at_least_one_region = true;
        }

        if parsing_failed {
            return None;
        }

        Some(parsed_at_least_one_region)
    }

    fn inject(&mut self, range: Range<usize>, parts: &mut BufferParts) {
        let buf = TsBuf(parts.bytes);
        let (.., Queries { injections, .. }) = self.lang_parts;

        let cn = injections.capture_names();
        let is_content = |cap: &&QueryCapture| cn[cap.index as usize] == "injection.content";
        let language = |qm: &QueryMatch, props: &[QueryProperty]| {
            props
                .iter()
                .find_map(|p| {
                    (p.key.as_ref() == "injection.language")
                        .then_some(p.value.as_ref().unwrap().to_string())
                })
                .or_else(|| {
                    let cap = qm
                        .captures
                        .iter()
                        .find(|cap| cn[cap.index as usize] == "injection.language")?;
                    Some(parts.bytes.strs(cap.node.byte_range()).unwrap().to_string())
                })
        };

        let mut cursor = QueryCursor::new();
        let mut observed_injections = Vec::new();

        for (_, tree) in self.trees.intersecting(range.clone()) {
            let ts_tree = tree
                .ts_tree
                .as_ref()
                .unwrap_or_else(|| panic!("{:#?} intersects with {range:?}", tree.region));

            cursor.set_byte_range(range.clone());

            let mut inj_captures = cursor.captures(injections, ts_tree.root_node(), buf);

            while let Some((qm, _)) = inj_captures.next() {
                let Some(cap) = qm.captures.iter().find(is_content) else {
                    continue;
                };

                let props = injections.property_settings(qm.pattern_index);
                let Some(filetype) = language(qm, props) else {
                    continue;
                };

                let Ok(mut lang_parts) = lang_parts_of(&filetype) else {
                    continue;
                };

                // You may want to set a new injections query, only for this capture.
                if let Some(prop) = props.iter().find(|p| p.key.as_ref() == "injection.query")
                    && let Some(value) = prop.value.as_ref()
                {
                    match query_from_path(&filetype, value, lang_parts.1) {
                        Ok(injections) => {
                            lang_parts.2.injections = injections;
                        }
                        Err(err) => context::error!("{err}"),
                    }
                };

                let cap_range = cap.node.byte_range();
                if let Some(injection) = self
                    .injections
                    .iter_mut()
                    .find(|injection| injection.lang_parts.0 == lang_parts.0)
                {
                    if injection.trees.add_region(Ranges::new(cap_range.clone())) {
                        parts.ranges_to_update.add_ranges([cap_range.clone()]);
                    }
                } else {
                    let mut parser = TsParser::new();
                    parser.set_language(lang_parts.1).unwrap();
                    self.injections.push(Parser {
                        parser,
                        trees: Trees::new([Ranges::new(cap_range.clone())]),
                        n_trees_to_parse: 1,
                        lang_parts,
                        forms: forms_from_lang_parts(lang_parts),
                        ranges_to_inject: Ranges::new(0..parts.bytes.len().byte()),
                        injections: Vec::new(),
                    });

                    parts.ranges_to_update.add_ranges([cap_range.clone()]);
                };

                observed_injections.push((lang_parts.0, cap_range.clone()));
            }
        }

        for injection in self.injections.iter_mut() {
            let mut trees_to_remove = Vec::new();

            for (i, tree) in injection.trees.intersecting(range.clone()) {
                // TODO: Deal with combined injections
                let range = tree.region.iter().next().unwrap();
                if observed_injections
                    .extract_if(.., |(filetype, r)| {
                        *r == range && *filetype == injection.lang_parts.0
                    })
                    .next()
                    .is_none()
                {
                    parts.ranges_to_update.add_ranges(tree.region.iter());
                    trees_to_remove.push(i);
                }
            }

            for i in trees_to_remove.into_iter().rev() {
                injection.trees.remove(i);
            }
        }

        _ = self.ranges_to_inject.remove_on(range);
    }

    fn highlight(&mut self, range: Range<usize>, parts: &mut BufferParts) {
        let mut cursor = QueryCursor::new();

        cursor.set_byte_range(range.clone());
        let buf = TsBuf(parts.bytes);

        let tagger = ts_tagger();
        let (.., Queries { highlights, .. }) = &self.lang_parts;

        for (_, tree) in self.trees.intersecting(range.clone()) {
            // If the tree wasn't there, then its addition will readd its range to
            // parts.ranges_to_update, so we can just ignore it.
            let Some(ts_tree) = tree.ts_tree.as_ref() else {
                continue;
            };

            for range in tree.region.iter_over(range.clone()) {
                parts.tags.remove_excl(tagger, range);
            }

            let mut hi_captures = cursor.captures(highlights, ts_tree.root_node(), buf);
            while let Some((qm, _)) = hi_captures.next() {
                let qm: &QueryMatch = qm;
                for cap in qm.captures.iter() {
                    let ts_range = cap.node.range();

                    // Assume that an empty range must take up the whole line
                    // Cuz sometimes it be like that
                    let (form, priority) = self.forms[cap.index as usize];
                    let range = ts_range.start_byte..ts_range.end_byte;
                    parts.tags.insert(tagger, range, form.to_tag(priority));
                }
            }
        }

        for injection in self.injections.iter_mut() {
            injection.highlight(range.clone(), parts);
        }
    }

    /// The expected level of indentation on a given [`Point`]
    pub fn indent_on<'a>(&'a self, p: Point, bytes: &Bytes, opts: PrintOpts) -> Option<usize> {
        let (_, tree) = self.trees.intersecting(p.byte()..p.byte() + 1).next()?;
        let ts_tree = tree.ts_tree.as_ref()?;

        if let Some(indent) = self
            .injections
            .iter()
            .find_map(|injection| injection.indent_on(p, bytes, opts))
        {
            return Some(indent);
        }

        let (.., Queries { indents, .. }) = self.lang_parts;

        let root = ts_tree.root_node();
        let start = bytes.point_at_line(p.line());
        let first_line = bytes
            .point_at_byte(tree.region.iter().next().unwrap().start)
            .line();

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
                        let Some(name) =
                            indents.capture_names()[cap.index as usize].strip_prefix("indent.")
                        else {
                            continue;
                        };

                        let nodes = if let Some(nodes) = caps.get_mut(name) {
                            nodes
                        } else {
                            caps.insert(name, HashMap::new());
                            caps.get_mut(name).unwrap()
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
                .find(|(_, line)| !(line.matches_pat(r"^\s*$").unwrap()))
                .filter(|(l, _)| *l >= first_line)
            else {
                // If there is no previous non empty line, align to 0.
                return Some(0);
            };
            let trail = line.chars().rev().take_while(|c| c.is_whitespace()).count();

            let prev_range = bytes.line_range(prev_l);
            let mut node = descendant_in(root, prev_range.end.byte() - (trail + 1));
            if node.kind().contains("comment") {
                // Unless the whole line is a comment, try to find the last node
                // before the comment.
                // This technically fails if there are multiple block comments.
                let first_node = descendant_in(root, prev_range.start.byte());
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

        let tab = opts.tabstop as i32;
        let mut indent = if root.start_byte() != 0 {
            bytes.indent(bytes.point_at_byte(root.start_byte()), opts) as i32
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

            let fd = |node: Node<'a>, delim: &str| -> (Option<Node<'a>>, bool) {
                let mut c = node.walk();
                let child = node.children(&mut c).find(|child| child.kind() == delim);
                let ret = child.map(|child| {
                    let range = bytes.line_range(child.start_position().row);
                    let range = child.range().start_byte..range.end.byte();

                    let is_last_in_line = if let Some(line) = bytes.get_contiguous(range.clone()) {
                        line.split_whitespace().any(|w| w != delim)
                    } else {
                        let line = bytes.slices(range).try_to_string().unwrap();
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

    fn edit(&mut self, edit: &InputEdit) {
        self.parser.reset();
        self.trees.edit(edit);

        for (_, tree) in self
            .trees
            .intersecting_mut(edit.start_byte..edit.new_end_byte)
        {
            self.n_trees_to_parse += (!tree.needs_parse) as usize;
            tree.needs_parse = true;
        }

        for injection in self.injections.iter_mut() {
            injection.edit(edit);
        }
    }
}

/// Does a forced parsing of the handle
pub(crate) fn sync_parse<'p>(
    pa: &'p mut Pass,
    handle: &'p Handle,
) -> Option<(&'p Parser, &'p Buffer)> {
    let printed_lines = handle.printed_line_ranges(pa);
    let visible_ranges = get_visible_ranges(&printed_lines);
    let (parser, buffer) = PARSERS.write(pa, handle)?;

    let mut parts = TRACKER.parts(buffer).unwrap();

    apply_changes(&parts, parser);
    parser.parse(&mut parts, &visible_ranges, true);

    Some((parser, buffer))
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

/// Spent too long parsing, yield if necessary
#[track_caller]
fn must_yield(start: Instant) -> bool {
    if start.elapsed() >= PARSE_TIMEOUT && duat_core::context::has_unhandled_events() {
        context::debug!("yielded");
        true
    } else {
        false
    }
}

fn get_visible_ranges(printed_lines: &[Range<usize>]) -> Vec<Range<usize>> {
    let mut ranges_to_parse: Vec<Range<usize>> = Vec::new();
    for range in printed_lines {
        if let Some(last) = ranges_to_parse.last_mut()
            && last.end == range.start
        {
            last.end = range.end
        } else {
            ranges_to_parse.push(range.clone())
        }
    }
    ranges_to_parse
}

fn ts_point(point: Point, bytes: &Bytes) -> TsPoint {
    let strs = bytes.slices(..point.byte());
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

#[track_caller]
fn apply_changes(parts: &BufferParts<'_>, parser: &mut Parser) {
    for change in parts.changes.clone() {
        let edit = input_edit(change, parts.bytes);
        parser.edit(&edit);
    }
}

#[track_caller]
fn descendant_in(node: Node, byte: usize) -> Node {
    node.descendant_for_byte_range(byte, byte + 1).unwrap()
}

fn parser_fn<'a>(bytes: &'a Bytes) -> impl FnMut(usize, TsPoint) -> &'a [u8] {
    let [s0, s1] = bytes.slices(..).to_array();
    |byte, _point| {
        if byte < s0.len() {
            &s0[byte..]
        } else {
            &s1[byte - s0.len()..]
        }
    }
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

#[derive(Clone, Copy)]
struct TsBuf<'a>(&'a Bytes);

impl<'a> TextProvider<&'a [u8]> for TsBuf<'a> {
    type I = std::array::IntoIter<&'a [u8], 2>;

    fn text(&mut self, node: tree_sitter::Node) -> Self::I {
        let range = node.range();
        let buffers = self.0.slices(range.start_byte..range.end_byte);
        buffers.to_array().into_iter()
    }
}
