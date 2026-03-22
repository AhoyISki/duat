//! The default parser for the [`BufferOpts`]
//!
//! This parser is responsible for actually modifying the way
//! `Buffer`s are printed so they follow the options. The reason why
//! this is defined separately is that one may wish to replace the
//! default implementor of these opts, in order to make them more
//! compatible with other settings in Duat.
use std::{
    collections::HashMap,
    ops::Range,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    buffer::{BufferOpts, BufferParts, BufferTracker, PerBuffer},
    form::{self, FormId},
    hook::{self, BufferClosed, BufferOpened, BufferPrinted, BufferUpdated},
    text::{Ghost, RegexHaystack, Spacer, Strs, Tagger, Tags, txt},
    utils::Memoized,
};

struct BufferOptsParser {
    opts: BufferOpts,
}

static REPLACEMENT_TRACKER: BufferTracker = BufferTracker::new();
static INDENT_TRACKER: BufferTracker = BufferTracker::new();
static PARSERS: PerBuffer<BufferOptsParser> = PerBuffer::new();

pub fn enable_parser() {
    hook::add::<BufferOpened>(|pa, handle| {
        let opts_parser = BufferOptsParser { opts: handle.read(pa).opts };
        PARSERS.register(pa, handle, opts_parser);
        REPLACEMENT_TRACKER.register_buffer(handle.write(pa));
        INDENT_TRACKER.register_buffer(handle.write(pa));
    })
    .grouped("DefaultOptsParser");

    hook::add::<BufferClosed>(|pa, handle| _ = PARSERS.unregister(pa, handle))
        .grouped("DefaultOptsParser");

    let [nl_tagger, space_tagger] = [Tagger::new(), Tagger::new()];
    let cur_line_tagger = Tagger::new();
    let indent_tagger = Tagger::new();

    hook::add::<BufferUpdated>(move |pa, handle| {
        let printed_line_ranges = handle.printed_line_ranges(pa);
        let (parser, buffer) = PARSERS.write(pa, handle).unwrap();

        let opts_changed = buffer.opts != parser.opts;
        parser.opts = buffer.opts;

        let mut parts = REPLACEMENT_TRACKER.parts(buffer).unwrap();

        if parser.opts.highlight_current_line {
            hightlight_current_line(&mut parts, cur_line_tagger);
        }

        let taggers = [nl_tagger, space_tagger];
        replace_chars(parts, &printed_line_ranges, taggers, opts_changed);

        let parts = INDENT_TRACKER.parts(buffer).unwrap();
        show_indents(parts, &printed_line_ranges, indent_tagger, opts_changed);
    })
    .grouped("DefaultOptsParser");

    hook::add::<BufferPrinted>(move |pa, handle| {
        handle.text_mut(pa).remove_tags(cur_line_tagger, ..);
    })
    .grouped("DefaultOptsParser");
}

fn hightlight_current_line(parts: &mut BufferParts, tagger: Tagger) {
    static CUR_LINE_INLAY: LazyLock<Ghost> =
        LazyLock::new(|| Ghost::overlay(txt!("[current_line] {Spacer}")));

    let caret = parts.selections.main().caret();
    let line_range = parts.strs.line(caret.line()).byte_range();

    let cur_line_form = form::id_of!("current_line").to_tag(50);

    parts
        .tags
        .insert(tagger, line_range.start, CUR_LINE_INLAY.clone());
    parts.tags.insert(tagger, line_range, cur_line_form);
}

fn replace_chars(
    mut parts: BufferParts,
    ranges: &[Range<usize>],
    taggers: [Tagger; 2],
    opts_changed: bool,
) {
    static OVERLAYS: LazyLock<Mutex<HashMap<(char, FormId), Ghost>>> =
        LazyLock::new(Mutex::default);
    let mut overlays = OVERLAYS.lock().unwrap();

    macro_rules! overlay {
        ($char:expr, $form:literal) => {{
            let form = form::id_of!($form);
            overlays
                .entry(($char, form))
                .or_insert_with(|| Ghost::overlay(txt!("{}{}", form.to_tag(90), $char)))
                .clone()
        }};
    }

    if opts_changed {
        parts.ranges_to_update.add_ranges([..]);
    } else {
        parts.ranges_to_update.add_ranges(
            parts
                .changes
                .clone()
                .map(|change| change.line_range(parts.strs)),
        );
    }

    let lines_to_update = parts.ranges_to_update.select_from(ranges.iter().cloned());
    if lines_to_update.is_empty() {
        return;
    }

    let [nl_tagger, space_tagger] = taggers;

    let opts = parts.opts;

    let space_overlay = opts.space_char.map(|char| overlay!(char, "replace.space"));
    let space_overlay_trailing = opts
        .space_char_trailing
        .map(|char| overlay!(char, "replace.space.trailing"));

    let nl_overlay =
        (opts.new_line_char != ' ').then(|| overlay!(opts.new_line_char, "replace.new_line"));
    let nl_overlay_empty = opts
        .new_line_on_empty
        .map(|char| overlay!(char, "replace.new_line.empty"));
    let nl_overlay_trailing = opts
        .new_line_trailing
        .map(|char| overlay!(char, "replace.new_line.trailing"));

    for range in lines_to_update.iter().cloned() {
        parts.tags.remove(space_tagger, range.start..range.end);
        parts.tags.remove_excl(nl_tagger, range.start..range.end);

        let line = &parts.strs[range.clone()];
        let line_start = line.byte_range().start;

        let mut space_start = None;

        for (byte, char) in line.char_indices() {
            let byte = line_start + byte;
            match char {
                '\n' => {
                    let overlay = space_start
                        .and_then(|_| nl_overlay_trailing.clone())
                        .or_else(|| {
                            nl_overlay_empty
                                .as_ref()
                                .filter(|_| byte == line_start)
                                .cloned()
                        })
                        .or_else(|| nl_overlay.clone());

                    if let Some(overlay) = overlay {
                        parts.tags.insert(nl_tagger, byte, overlay);
                    }

                    if let Some(start) = space_start.take()
                        && start != line_start
                        && let Some(char) = opts.space_char_trailing
                        && char != ' '
                        && let Some(overlay) = &space_overlay_trailing
                    {
                        for (b, char) in parts.strs[start..=byte].char_indices() {
                            let b = start + b;
                            if char == ' ' {
                                parts.tags.insert(space_tagger, b, overlay.clone());
                            }
                        }
                    }
                }
                ' ' => _ = space_start.get_or_insert(byte),
                _ => {
                    if let Some(start) = space_start.take()
                        && start != line_start
                        && let Some(char) = opts.space_char
                        && char != ' '
                        && let Some(overlay) = &space_overlay
                    {
                        for byte in start..byte {
                            parts.tags.insert(space_tagger, byte, overlay.clone());
                        }
                    }
                }
            }
        }
    }

    parts.ranges_to_update.update_on(lines_to_update);
}

fn show_indents(
    mut parts: BufferParts,
    ranges: &[Range<usize>],
    tagger: Tagger,
    opts_changed: bool,
) {
    if opts_changed {
        parts.ranges_to_update.add_ranges([..]);
    } else {
        parts.ranges_to_update.add_ranges(
            parts
                .changes
                .clone()
                .map(|change| change.line_range(parts.strs)),
        );
    }

    let lines_to_update = parts.ranges_to_update.select_from(ranges.iter().cloned());
    if lines_to_update.is_empty() {
        return;
    }

    let popts = parts.opts.to_print_opts();
    let sequences = lines_to_update
        .iter()
        .fold(Vec::<Vec<&Strs>>::new(), |mut seqs, range| {
            if let Some(seq) = seqs.last_mut()
                && seq.last().unwrap().byte_range().end == range.start
            {
                seq.push(&parts.strs[range.clone()]);
            } else {
                seqs.push(vec![&parts.strs[range.clone()]]);
            }
            seqs
        });

    let set_capped = |state: &mut IndentState, line: &Strs, indent: usize| {
        let total = state.list.iter().copied().sum();
        if indent >= total && line.search(r"^\s*(\}|\)|\]|end)").next().is_some() {
            state.capped = true;
        }
    };

    for seq in sequences {
        let prev_unindented = {
            parts.strs[..seq[0].byte_range().start]
                .lines()
                .rev()
                .find_map(|line| {
                    (!line.chars().next().unwrap().is_ascii_whitespace())
                        .then_some(line.byte_range())
                })
                .unwrap_or(0..0)
        };

        let next_unindented = {
            parts.strs[seq.last().unwrap().byte_range().end..]
                .lines()
                .find_map(|line| {
                    (!line.chars().next().unwrap().is_ascii_whitespace())
                        .then_some(line.byte_range())
                })
                .unwrap_or(parts.strs.len()..parts.strs.len())
        };

        let mut state = IndentState::new(parts.opts.indent_str, parts.opts.tabstop);
        let mut empty_lines = Vec::new();

        for line in parts.strs[prev_unindented.end..next_unindented.end].lines() {
            if line.is_empty_line() {
                empty_lines.push(line);
            } else {
                let indent = line.indent(popts);
                state.truncate(indent);

                set_capped(&mut state, line, indent);
                for line in empty_lines.drain(..) {
                    state.indent_line(line, &mut parts.tags, tagger);
                }
                state.capped = false;

                state.increment(indent);
                state.indent_line(line, &mut parts.tags, tagger)
            }
        }

        let updated_range = prev_unindented.end..next_unindented.end;
        parts.ranges_to_update.update_on([updated_range]);
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct IndentState {
    list: Vec<usize>,
    capped: bool,
    indent_str: Option<&'static str>,
    tabstop: u8,
}

impl IndentState {
    fn new(indent_str: Option<&'static str>, tabstop: u8) -> Self {
        Self {
            list: Vec::new(),
            capped: false,
            indent_str,
            tabstop,
        }
    }

    fn truncate(&mut self, indent: usize) {
        self.list.retain({
            let mut sum = 0;
            move |len| {
                sum += *len;
                sum <= indent
            }
        });
    }

    fn increment(&mut self, indent: usize) {
        let total: usize = self.list.iter().copied().sum();
        if indent > self.list.iter().copied().sum() {
            self.list.push((indent - total).min(self.tabstop as usize))
        }
    }

    fn indent_line(&self, line: &Strs, tags: &mut Tags, tagger: Tagger) {
        static OVERLAYS: Memoized<IndentState, Ghost> = Memoized::new();

        let range = line.byte_range();
        tags.remove_excl(tagger, range.clone());

        if self.list.is_empty() && !self.capped {
            return;
        }

        let Some(indent_str) = self.indent_str.filter(|str| !str.is_empty()) else {
            return;
        };

        let indent_form = form::id_of!("replace.indent").to_tag(90);

        let overlay = OVERLAYS.get_or_insert_with(self, || {
            let ghost: String = self
                .list
                .iter()
                .copied()
                .chain(self.capped.then_some(self.tabstop as usize))
                .flat_map(|len| indent_str.chars().chain(std::iter::repeat(' ')).take(len))
                .collect();

            Ghost::overlay(txt!("{indent_form}{ghost}"))
        });

        tags.insert(tagger, range.start, overlay);
    }
}
