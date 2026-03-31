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
    Ns,
    buffer::{Buffer, BufferOpts, Moment, PerBuffer},
    form::{self, FormId},
    hook::{self, BufferClosed, BufferOpened, BufferPrinted, BufferUpdated},
    text::{Ghost, Mask, RegexHaystack, Strs, Tags, txt},
    utils::Memoized,
};

struct BufferOptsParser {
    opts: BufferOpts,
}

static PARSERS: PerBuffer<BufferOptsParser> = PerBuffer::new();

pub fn enable_parser(ns: Ns) {
    hook::add::<BufferOpened>(move |pa, handle| {
        let opts_parser = BufferOptsParser { opts: handle.read(pa).opts };
        PARSERS.register(pa, handle, opts_parser);
    })
    .grouped(ns);

    hook::add::<BufferClosed>(|pa, handle| _ = PARSERS.unregister(pa, handle)).grouped(ns);

    let [nl_ns, space_ns] = [Ns::new(), Ns::new()];
    let cur_line_ns = Ns::new();
    let indent_ns = Ns::new();
    let replacement_ns = Ns::new();

    hook::add::<BufferUpdated>(move |pa, handle| {
        let printed_line_ranges = handle.printed_line_ranges(pa);
        let (parser, buf) = PARSERS.write(pa, handle).unwrap();

        let opts_changed = buf.opts != parser.opts;
        parser.opts = buf.opts;

        let moment = buf.moment_for(replacement_ns);

        if parser.opts.highlight_current_line {
            hightlight_current_line(buf, cur_line_ns);
        }

        let nss = [nl_ns, space_ns];
        replace_chars(buf, &moment, &printed_line_ranges, nss, opts_changed);

        show_indents(buf, &moment, &printed_line_ranges, indent_ns, opts_changed);
    })
    .grouped(ns);

    hook::add::<BufferPrinted>(move |pa, handle| {
        handle.text_mut(pa).remove_tags(cur_line_ns, ..);
    })
    .grouped(ns);

    form::enable_mask("indent");
}

fn hightlight_current_line(buf: &mut Buffer, ns: Ns) {
    let mut parts = buf.text_parts();

    let caret = parts.selections.main().caret();
    let line_range = parts.strs.line(caret.line()).byte_range();

    parts.tags.insert(ns, line_range, Mask("current_line"));
}

fn replace_chars(
    buf: &mut Buffer,
    moment: &Moment,
    ranges: &[Range<usize>],
    nss: [Ns; 2],
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

    let ranges_to_update = buf.ranges_to_update_for(nss[0]);
    let opts = buf.opts;
    let mut parts = buf.text_parts();

    if opts_changed {
        ranges_to_update.add_ranges([..]);
    } else {
        ranges_to_update.add_ranges(moment.iter().map(|change| change.line_range(parts.strs)));
    }

    let lines_to_update = ranges_to_update.select_from(ranges.iter().cloned());
    if lines_to_update.is_empty() {
        return;
    }

    let [nl_ns, space_ns] = nss;

    let space_overlay = opts.space_char.map(|char| overlay!(char, "replace.space"));

    let nl_overlay = (opts.newline != ' ').then(|| overlay!(opts.newline, "replace.newline"));
    let nl_overlay_empty = opts
        .newline_on_empty
        .map(|char| overlay!(char, "replace.newline.empty"));
    let nl_overlay_trailing = opts
        .newline_trailing
        .map(|char| overlay!(char, "replace.newline.trailing"));

    for range in lines_to_update.iter().cloned() {
        parts.tags.remove(space_ns, range.start..range.end);
        parts.tags.remove_excl(nl_ns, range.start..range.end);

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
                        parts.tags.insert(nl_ns, byte, overlay);
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
                            parts.tags.insert(space_ns, byte, overlay.clone());
                        }
                    }
                }
            }
        }
    }

    ranges_to_update.update_on(lines_to_update);
}

fn show_indents(
    buf: &mut Buffer,
    moment: &Moment,
    ranges: &[Range<usize>],
    ns: Ns,
    opts_changed: bool,
) {
    let ranges_to_update = buf.ranges_to_update_for(ns);
    let opts = buf.opts;
    let mut parts = buf.text_parts();

    if opts_changed {
        ranges_to_update.add_ranges([..]);
    } else {
        ranges_to_update.add_ranges(moment.iter().map(|change| change.line_range(parts.strs)));
    }

    let lines_to_update = ranges_to_update.select_from(ranges.iter().cloned());
    if lines_to_update.is_empty() {
        return;
    }

    let popts = opts.to_print_opts();
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

        let mut state = IndentState::new(opts.indent_str, opts.tabstop);
        let mut empty_lines = Vec::new();

        for line in parts.strs[prev_unindented.end..next_unindented.end].lines() {
            if line.is_empty_line() {
                empty_lines.push(line);
            } else {
                let indent = line.indent(popts);
                state.truncate(indent);

                set_capped(&mut state, line, indent);
                for line in empty_lines.drain(..) {
                    state.indent_line(line, &mut parts.tags, ns);
                }
                state.capped = false;

                state.increment(indent);
                state.indent_line(line, &mut parts.tags, ns)
            }
        }

        let updated_range = prev_unindented.end..next_unindented.end;
        ranges_to_update.update_on([updated_range]);
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

    fn indent_line(&self, line: &Strs, tags: &mut Tags, ns: Ns) {
        static OVERLAYS: Memoized<IndentState, Ghost> = Memoized::new();

        let range = line.byte_range();
        tags.remove_excl(ns, range.clone());

        if self.list.is_empty() && !self.capped {
            return;
        }

        let Some(indent_str) = self.indent_str.filter(|str| !str.is_empty()) else {
            return;
        };

        let indent_form = form::id_of!("replace").to_tag(90);

        let overlay = OVERLAYS.get_or_insert_with(self, || {
            let ghost: String = self
                .list
                .iter()
                .copied()
                .chain(self.capped.then_some(self.tabstop as usize))
                .flat_map(|len| indent_str.chars().chain(std::iter::repeat(' ')).take(len))
                .collect();

            Ghost::overlay(txt!("{}{indent_form}{ghost}", Mask("indent")))
        });

        tags.insert(ns, range.start, overlay);
    }
}
