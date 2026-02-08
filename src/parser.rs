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
    opts::PrintOpts,
    text::{Point, Strs, Tagger},
};
use duat_filetype::{FileType, PassFileType};
use tree_sitter::{
    InputEdit, Node, ParseOptions, ParseState, Parser as TsParser, Point as TsPoint, QueryCapture,
    QueryCursor, QueryMatch, QueryProperty, Range as TsRange, StreamingIterator, TextProvider,
};

use crate::{LangParts, Queries, lang_parts_of, query_from_path, tree::Trees};

const PARSE_TIMEOUT: Duration = Duration::from_millis(3);
static TRACKER: BufferTracker = BufferTracker::new();
static PARSERS: PerBuffer<Parser> = PerBuffer::new();

pub(crate) fn add_parser_hook() {
    fn async_parse(
        pa: &mut Pass,
        handle: &Handle,
        printed_lines: Vec<Range<usize>>,
        is_queued: bool,
    ) -> bool {
        if let Some(filetype) = handle.filetype(pa)
            && let Some((parser, buf)) = PARSERS.write(pa, handle)
            && parser.lang_parts.0 == filetype
        {
            if parser.is_parsing && !is_queued {
                return true;
            }

            parser.is_parsing = true;

            let visible_ranges = get_visible_ranges(&printed_lines);
            let mut parts = TRACKER.parts(buf).unwrap();

            apply_changes(&parts, parser);

            // In this case, the previously sent printed_lines may be outdated and
            // the TsParsers have been reset, so get new ones.
            if is_queued && parts.changes.len() > 0 {
                let printed_lines = handle.printed_line_ranges(pa);
                return async_parse(pa, handle, printed_lines, is_queued);
            }

            if !parser.parse(&mut parts, &visible_ranges, Some(Instant::now()), handle) {
                let handle = handle.clone();
                let printed_lines = printed_lines.clone();
                context::queue(move |pa| _ = async_parse(pa, &handle, printed_lines, true))
            } else {
                parser.is_parsing = false;
            }

            for range in parts
                .ranges_to_update
                .select_from(printed_lines.iter().cloned())
            {
                let range = range.start..range.end + 1;
                parts.tags.remove_excl(ts_tagger(), range.clone());
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
        if async_parse(pa, handle, printed_lines.clone(), false) {
            return;
        }

        let Some(filetype) = handle.read(pa).filetype() else {
            return;
        };

        if let Some(lang_parts) = lang_parts_of(filetype, handle) {
            let len_bytes = handle.text(pa).len();

            let mut parser = TsParser::new();
            parser.set_language(lang_parts.1).unwrap();

            TRACKER.register_buffer(handle.write(pa));
            PARSERS.register(pa, handle, Parser {
                parser,
                trees: Trees::new([Ranges::new(0..len_bytes)]),
                lang_parts,
                forms: forms_from_lang_parts(lang_parts),
                injections: Vec::new(),
                ranges_to_inject: Ranges::new(0..len_bytes),
                is_parsing: false,
            });

            async_parse(pa, handle, printed_lines.clone(), false);
        }
    });
}

pub struct Parser {
    parser: TsParser,
    trees: Trees,
    lang_parts: LangParts<'static>,
    forms: &'static [(FormId, u8)],
    ranges_to_inject: Ranges,
    injections: Vec<Parser>,
    is_parsing: bool,
}

impl Parser {
    /// Returns the root [`Node`] of the tree sitter `Parser`
    #[track_caller]
    pub fn root_node(&self) -> Node<'_> {
        let tree = self.trees.iter().next().unwrap();
        tree.ts_tree.as_ref().unwrap().root_node()
    }

    fn parse(
        &mut self,
        parts: &mut BufferParts,
        visible_ranges: &[Range<usize>],
        start: Option<Instant>,
        handle: &Handle,
    ) -> bool {
        // To parse something, in case there are no visible ranges.
        let visible_ranges = if visible_ranges.is_empty() {
            #[allow(clippy::single_range_in_vec_init)]
            &[0..1]
        } else {
            visible_ranges
        };

        let mut parsed_at_least_one_region = false;

        for range in visible_ranges.iter() {
            let Some(parsed_a_tree) = self.parse_trees(range.clone(), parts, start) else {
                return false;
            };

            parsed_at_least_one_region |= parsed_a_tree;
        }

        if parsed_at_least_one_region {
            self.ranges_to_inject.add(0..parts.strs.len());
        }

        let ranges_to_inject = visible_ranges
            .iter()
            .flat_map(|range| self.ranges_to_inject.iter_over(range.clone()))
            .fold(Vec::<Range<usize>>::new(), |mut ranges, range| {
                match ranges.last_mut() {
                    Some(last) if last.end == range.start => last.end = range.end,
                    _ => ranges.push(range),
                }
                ranges
            });

        for range in ranges_to_inject {
            self.inject(range, parts, handle);
            if must_yield(start) {
                return false;
            }
        }

        for injection in self.injections.iter_mut() {
            if !injection.parse(parts, visible_ranges, start, handle) {
                return false;
            }
        }

        true
    }

    fn parse_trees(
        &mut self,
        range: Range<usize>,
        parts: &mut BufferParts,
        start: Option<Instant>,
    ) -> Option<bool> {
        let mut callback = |_: &ParseState| match must_yield(start) {
            true => ControlFlow::Break(()),
            false => ControlFlow::Continue(()),
        };

        let ts_range = |range: Range<usize>| TsRange {
            start_byte: range.start,
            end_byte: range.end,
            start_point: ts_point(parts.strs.point_at_byte(range.start), parts.strs),
            end_point: ts_point(parts.strs.point_at_byte(range.end), parts.strs),
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
                &mut parser_fn(parts.strs),
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
            parsed_at_least_one_region = true;
        }

        if parsing_failed {
            return None;
        }

        Some(parsed_at_least_one_region)
    }

    fn highlight(&self, range: Range<usize>, parts: &mut BufferParts) {
        let buf = TsBuf(parts.strs);

        let tagger = ts_tagger();
        let (.., Queries { highlights, .. }) = &self.lang_parts;

        for (_, tree) in self.trees.intersecting(range.clone()) {
            // If the tree wasn't there, then its addition will readd its range to
            // parts.ranges_to_update, so we can just ignore this.
            let Some(ts_tree) = tree.ts_tree.as_ref() else {
                continue;
            };

            let mut cursor = QueryCursor::new();
            cursor.set_byte_range(range.clone());
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

        for injection in self.injections.iter() {
            injection.highlight(range.clone(), parts);
        }
    }

    fn inject(&mut self, range: Range<usize>, parts: &mut BufferParts, handle: &Handle) {
        let range = self
            .injections
            .iter()
            .flat_map({
                let range = range.clone();
                move |inj| inj.trees.intersecting(range.clone())
            })
            .fold(range, |range, (_, tree)| {
                let inj_range = tree.region.iter().next().unwrap();
                range.start.min(inj_range.start)..range.end.max(inj_range.end)
            });

        let buf = TsBuf(parts.strs);
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
                    Some(parts.strs[cap.node.byte_range()].to_string())
                })
        };

        let mut cursor = QueryCursor::new();
        let mut observed_injections = Vec::new();
        let mut defered_ranges = Vec::new();

        for (_, tree) in self.trees.intersecting(range.clone()) {
            let ts_tree = tree.ts_tree.as_ref().unwrap();

            cursor.set_byte_range(range.clone());

            let mut inj_captures = cursor.captures(injections, ts_tree.root_node(), buf);

            while let Some((qm, _)) = inj_captures.next() {
                let Some(cap) = qm.captures.iter().find(is_content) else {
                    continue;
                };

                let cap_range = cap.node.byte_range();
                let props = injections.property_settings(qm.pattern_index);

                let Some(filetype) = language(qm, props) else {
                    continue;
                };

                let Some(mut lang_parts) = lang_parts_of(&filetype, handle) else {
                    defered_ranges.push(cap_range);
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
                        lang_parts,
                        forms: forms_from_lang_parts(lang_parts),
                        ranges_to_inject: Ranges::new(0..parts.strs.len()),
                        injections: Vec::new(),
                        is_parsing: false,
                    });

                    parts.ranges_to_update.add_ranges([cap_range.clone()]);
                };

                observed_injections.push((lang_parts.0, cap_range.clone()));
            }
        }

        for injection in self.injections.iter_mut() {
            let mut to_remove = Vec::new();

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
                    to_remove.push((i, range));
                }
            }

            for (i, range) in to_remove.into_iter().rev() {
                injection.trees.remove(i);
                injection.remove_injections_on(range);
            }
        }

        _ = self.ranges_to_inject.remove_on(range);

        for range in defered_ranges {
            self.ranges_to_inject.add(range);
        }
    }

    fn remove_injections_on(&mut self, range: Range<usize>) {
        for injection in self.injections.iter_mut() {
            let trees: Vec<usize> = injection
                .trees
                .intersecting(range.clone())
                .map(|(i, _)| i)
                .collect();

            for i in trees.into_iter().rev() {
                injection.trees.remove(i);
            }

            injection.remove_injections_on(range.clone());
        }
    }

    /// The expected level of indentation on a given [`Point`]
    pub fn indent_on<'a>(&'a self, lnum: usize, strs: &Strs, opts: PrintOpts) -> Option<usize> {
        let line_range = strs.line(lnum).range();

        let (_, tree) = self
            .trees
            .intersecting(line_range.start.byte()..line_range.end.byte())
            .next()?;
        let ts_tree = tree.ts_tree.as_ref()?;

        if let Some(indent) = self
            .injections
            .iter()
            .find_map(|injection| injection.indent_on(lnum, strs, opts))
        {
            return Some(indent);
        }

        let (.., Queries { indents, .. }) = self.lang_parts;

        let root = ts_tree.root_node();
        let first_line_point = strs.point_at_byte(tree.region.iter().next().unwrap().start);

        // The query could be empty.
        if indents.pattern_count() == 0 {
            return None;
        }

        let mut cursor = QueryCursor::new();
        let buf = TsBuf(strs);

        // TODO: Don't reparse python, apparently.

        type Captures<'a> = HashMap<&'a str, HashMap<usize, HashMap<&'a str, Option<&'a str>>>>;
        let mut caps = HashMap::new();
        let q = {
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
        let indented_start_column = strs[line_range.start..]
            .chars()
            .take_while(|char| *char != '\n')
            .position(|char| !char.is_whitespace());

        let mut opt_node = if let Some(column) = indented_start_column {
            Some(descendant_in(root, lnum, column))
        // If the line is empty, look behind for another.
        } else {
            let is_not_ws = |(_, char): &(_, char)| !char.is_ascii_whitespace();

            // Find last previous empty line.
            let mut lines = strs[..line_range.start].lines().rev();
            let Some(line) = lines
                .find(|line| !line.chars().all(|char| char.is_whitespace()))
                .filter(|line| line.range().start.line() >= first_line_point.line())
            else {
                // If there is no previous non empty line, align to 0.
                return Some(0);
            };

            let prev_lnum = line.range().start.line();
            let (last_non_whitespace_col, _) =
                line.chars().enumerate().filter(is_not_ws).last().unwrap();

            let mut node = descendant_in(root, prev_lnum, last_non_whitespace_col);
            if node.kind().contains("comment") {
                // Unless the whole line is a comment, try to find the last node
                // before the comment.
                // This technically fails if there are multiple block comments.
                let first_node = descendant_in(root, prev_lnum, 0);
                if first_node.id() != node.id() {
                    node = descendant_in(root, prev_lnum, node.start_position().column - 1)
                }
            }

            Some(if q(&caps, node, &["end"]) {
                descendant_in(root, lnum, 0)
            } else {
                node
            })
        };

        if q(&caps, opt_node.unwrap(), &["zero"]) {
            return Some(0);
        }

        let tab = opts.tabstop as i32;
        let mut indent = if root.start_byte() != 0 {
            strs.line(root.start_position().row).indent(opts) as i32
        } else {
            0
        };

        let mut processed_lines = Vec::new();
        while let Some(node) = opt_node {
            let s_line = node.start_position().row;
            let e_line = node.end_position().row;

            // If a node is not an indent and is marked as auto or ignore, act
            // accordingly.
            if !q(&caps, node, &["begin"]) && s_line < lnum && lnum <= e_line {
                if !q(&caps, node, &["align"]) && q(&caps, node, &["auto"]) {
                    return None;
                } else if q(&caps, node, &["ignore"]) {
                    return Some(0);
                }
            }

            let should_process = !processed_lines.contains(&s_line);

            let mut is_processed = false;

            if should_process
                && ((s_line == lnum && q(&caps, node, &["branch"]))
                    || (s_line != lnum && q(&caps, node, &["dedent"])))
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
                && (s_line != lnum || q(&caps, node, &["begin", "start_at_same_line"]))
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
                    let line_range = strs.line(child.start_position().row).byte_range();
                    let range = child.range().end_byte..line_range.end;

                    let is_last_in_line = strs[range].chars().all(|char| char.is_whitespace());

                    (child, is_last_in_line)
                });
                let (child, is_last_in_line) = ret.unzip();
                (child, is_last_in_line.unwrap_or(false))
            };

            if should_process
                && q(&caps, node, &["align"])
                && (s_line != e_line || is_in_err)
                && s_line != lnum
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
                        if c_is_last_in_line && c_s_line.is_some_and(|l| l < lnum) {
                            indent = (indent - tab).max(0);
                        }
                        false
                    // Aligned indent
                    } else if c_is_last_in_line
                        && let Some(c_s_line) = c_s_line
                        // If the aligned node ended before the current line, its @align
                        // shouldn't affect it.
                        && (o_s_line != c_s_line && c_s_line < lnum)
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
                        .is_some_and(|c_s_line| c_s_line != o_s_line && c_s_line == lnum)
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
    parser.parse(&mut parts, &visible_ranges, None, handle);

    Some((parser, buffer))
}

/// The Key for tree-sitter
fn ts_tagger() -> Tagger {
    static TAGGER: LazyLock<Tagger> = Tagger::new_static();
    *TAGGER
}

fn input_edit(change: Change<&str>, strs: &Strs) -> InputEdit {
    let start = change.start();
    let added = change.added_end();
    let taken = change.taken_end();

    let ts_start = ts_point(start, strs);
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
fn must_yield(start: Option<Instant>) -> bool {
    if let Some(start) = start {
        start.elapsed() >= PARSE_TIMEOUT && duat_core::context::has_unhandled_events()
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

fn ts_point(point: Point, strs: &Strs) -> TsPoint {
    let slices = strs.slices(..point.byte());
    let iter = slices.into_iter().flat_map(|s| s.iter().copied()).rev();
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
        parts
            .ranges_to_update
            .add_ranges([change.line_range(parts.strs)]);
        let edit = input_edit(change, parts.strs);
        parser.edit(&edit);
    }
}

#[track_caller]
fn descendant_in(node: Node, line: usize, column: usize) -> Node {
    let start = TsPoint::new(line, column);
    let end = TsPoint { column: start.column + 1, ..start };

    node.descendant_for_point_range(start, end).unwrap()
}

fn parser_fn<'a>(strs: &'a Strs) -> impl FnMut(usize, TsPoint) -> &'a [u8] {
    let [s0, s1] = strs.slices(..);
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
struct TsBuf<'a>(&'a Strs);

impl<'a> TextProvider<&'a [u8]> for TsBuf<'a> {
    type I = std::array::IntoIter<&'a [u8], 2>;

    fn text(&mut self, node: tree_sitter::Node) -> Self::I {
        let range = node.range();
        let slices = self.0.slices(range.start_byte..range.end_byte);
        slices.into_iter()
    }
}
