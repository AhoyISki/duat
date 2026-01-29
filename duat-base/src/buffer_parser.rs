//! The default parser for the [`BufferOpts`]
//!
//! This parser is responsible for actually modifying the way
//! `Buffer`s are printed so they follow the options. The reason why
//! this is defined separately is that one may wish to replace the
//! default implementor of these opts, in order to make them more
//! compatible with other settings in Duat.
use std::{ops::Range, sync::LazyLock};

use duat_core::{
    buffer::{BufferOpts, BufferParts, BufferTracker, PerBuffer},
    form,
    hook::{self, BufferClosed, BufferOpened, BufferPrinted, BufferUpdated},
    text::{FormTag, Ghost, Point, Spacer, SwapChar, Tagger, Tags, txt},
    utils::Memoized,
};

struct BufferOptsParser {
    opts: BufferOpts,
}

static TRACKER: BufferTracker = BufferTracker::new();
static PARSERS: PerBuffer<BufferOptsParser> = PerBuffer::new();

pub fn enable_parser() {
    hook::add::<BufferOpened>(|pa, handle| {
        let opts_parser = BufferOptsParser { opts: handle.read(pa).opts };
        PARSERS.register(pa, handle, opts_parser);
        TRACKER.register_buffer(handle.write(pa));
    })
    .grouped("DefaultOptsParser");

    hook::add::<BufferClosed>(|pa, handle| {
        PARSERS.unregister(pa, handle);
    })
    .grouped("DefaultOptsParser");

    let replace_taggers = [Tagger::new(), Tagger::new(), Tagger::new()];
    let cur_line_tagger = Tagger::new();

    hook::add::<BufferUpdated>(move |pa, handle| {
        let printed_line_ranges = handle.printed_line_ranges(pa);
        let (parser, buffer) = PARSERS.write(pa, handle).unwrap();

        let opts_have_changed = buffer.opts != parser.opts;
        parser.opts = buffer.opts;

        let mut parts = TRACKER.parts(buffer).unwrap();

        if opts_have_changed {
            parts.ranges_to_update.add_ranges([..]);
        }

        if parser.opts.highlight_current_line {
            hightlight_current_line(&mut parts, cur_line_tagger);
        }

        replace_chars(parts, printed_line_ranges, replace_taggers);
    })
    .grouped("DefaultOptsParser");

    hook::add::<BufferPrinted>(move |pa, handle| {
        handle.text_mut(pa).remove_tags(cur_line_tagger, ..);
    })
    .grouped("DefaultOptsParser");
}

fn replace_chars(mut parts: BufferParts, lines: Vec<Range<usize>>, taggers: [Tagger; 3]) {
    let [spc_tagger, nl_tagger, empty_spc_tagger] = taggers;
    let opts = parts.opts;

    // Early return if nothing needs to be done.
    if opts.indent_str.is_none()
        && opts.space_char.is_none()
        && opts.new_line_char == ' '
        && opts.new_line_on_empty.is_none()
        && opts.new_line_trailing.is_none()
    {
        return;
    }

    let space_form = form::id_of!("replace.space").to_tag(90);
    let space_form_trailing = form::id_of!("replace.space.trailing").to_tag(90);

    let indent_form = form::id_of!("replace.indent").to_tag(90);
    let indent_form_empty = form::id_of!("replace.indent.empty").to_tag(90);

    let nl_form = form::id_of!("replace.new_line").to_tag(90);
    let nl_form_empty = form::id_of!("replace.new_line.empty").to_tag(90);
    let nl_form_trailing = form::id_of!("replace.new_line.trailing").to_tag(90);

    parts.ranges_to_update.add_ranges(
        parts
            .changes
            .clone()
            .map(|change| change.line_range(parts.bytes)),
    );

    let replace_indents = |tags: &mut Tags, range: Range<usize>, form: FormTag| {
        let mut replacements = opts.indent_str.into_iter().flat_map(|str| {
            let len = str.chars().count();
            str.chars()
                .map(Some)
                .chain((0..opts.tabstop.saturating_sub(len as u8)).map(|_| None))
                .cycle()
        });

        let tab_str = opts.indent_tab_str.or(opts.indent_str).unwrap();

        for (byte, char) in parts.bytes.chars_fwd(range.clone()).unwrap() {
            if char == ' ' {
                let Some(rep) = replacements.next().unwrap() else {
                    continue;
                };

                tags.insert(spc_tagger, byte, SwapChar::new(rep));
            } else {
                for rep in tab_str.chars() {
                    tags.insert(spc_tagger, byte, SwapChar::new(rep));
                }
            }
        }

        tags.insert(spc_tagger, range, form);
    };

    let ranges_to_update = parts.ranges_to_update.select_from(lines.iter().cloned());

    if ranges_to_update.is_empty() {
        return;
    }

    let mut empty_lines: Vec<Range<Point>> = parts
        .opts
        .indent_str_on_empty
        .then(|| {
            parts
                .bytes
                .lines(..ranges_to_update[0].start)
                .rev()
                .map_while(|line| (line == "\n").then(|| line.range()))
        })
        .into_iter()
        .flatten()
        .collect();

    empty_lines.reverse();

    for range in parts.ranges_to_update.select_from(lines.iter().cloned()) {
        parts.tags.remove(spc_tagger, range.start..range.end);
        parts.tags.remove_excl(nl_tagger, range.start..range.end);
        parts
            .tags
            .remove_excl(empty_spc_tagger, range.start..range.end);

        let mut indent_byte = Some(range.start);
        let mut line_is_empty = true;
        let mut first_space_byte = None;

        for (byte, char) in parts.bytes.chars_fwd(range.clone()).unwrap() {
            match char {
                ' ' => _ = first_space_byte.get_or_insert(byte),
                '\t' if indent_byte.is_some() => {}
                '\n' => {
                    let (nl_char, nl_form) = opts
                        .new_line_trailing
                        .and_then(|char| first_space_byte.and(Some((char, nl_form_trailing))))
                        .or(opts
                            .new_line_on_empty
                            .and_then(|char| line_is_empty.then_some((char, nl_form_empty))))
                        .unwrap_or((opts.new_line_char, nl_form));

                    if nl_char != ' ' {
                        parts.tags.insert(nl_tagger, byte..byte + 1, nl_form);
                        parts.tags.insert(nl_tagger, byte, SwapChar::new(nl_char));
                    }

                    if let Some(indent_byte) = indent_byte.take()
                        && (opts.indent_str.is_some() || opts.indent_tab_str.is_some())
                    {
                        replace_indents(&mut parts.tags, indent_byte..byte, indent_form_empty);
                    } else if let Some(first) = first_space_byte.take()
                        && let Some(char) = opts.space_char_trailing.or(opts.space_char)
                    {
                        let range = first..byte;
                        parts.tags.insert(spc_tagger, range, space_form_trailing);
                        for byte in first..byte {
                            parts.tags.insert(spc_tagger, byte, SwapChar::new(char));
                        }
                    }

                    if line_is_empty && parts.opts.indent_str_on_empty {
                        empty_lines.push(parts.bytes.strs(range.clone()).range());
                    }

                    line_is_empty = true;
                    continue;
                }
                _ => {
                    let first_space_byte = first_space_byte.take();
                    if let Some(indent_byte) = indent_byte.take()
                        && (opts.indent_str.is_some() || opts.indent_tab_str.is_some())
                    {
                        replace_indents(&mut parts.tags, indent_byte..byte, indent_form);
                    } else if let Some(first) = first_space_byte
                        && let Some(char) = opts.space_char
                    {
                        parts.tags.insert(spc_tagger, first..byte, space_form);
                        for byte in first..byte {
                            parts.tags.insert(spc_tagger, byte, SwapChar::new(char));
                        }
                    }
                }
            }

            line_is_empty = false;
        }
    }

    if parts.opts.indent_str_on_empty {
        empty_lines.extend(
            parts
                .bytes
                .lines(ranges_to_update.last().unwrap().end..)
                .map_while(|line| (line == "\n").then(|| line.range())),
        );
    }

    parts.ranges_to_update.update_on(lines);

    indent_empty_lines(parts, empty_lines, empty_spc_tagger);
}

fn indent_empty_lines(mut parts: BufferParts, empty_lines: Vec<Range<Point>>, tagger: Tagger) {
    static INDENT_INLAYS: Memoized<(&str, usize), Ghost> = Memoized::new();

    let indent_form_empty = form::id_of!("replace.indent.empty").to_tag(90);
    let Some(first) = empty_lines.first().cloned() else {
        return;
    };

    let mut prev_indent = first
        .start
        .line()
        .checked_sub(1)
        .map(|line| parts.bytes.indent(line, parts.opts.to_print_opts()))
        .unwrap_or(0);

    let mut prev_line = 0;
    let mut eq_indent_range = 0..0;

    let indent_lines = |tags: &mut Tags, eq_indent_range: Range<usize>, common_indent: usize| {
        if common_indent > 0
            && let Some(indent_str) = parts.opts.indent_str
            && let Some(char) = indent_str.chars().next()
        {
            let inlay = INDENT_INLAYS.get_or_insert_with((indent_str, common_indent), || {
                let len = indent_str.chars().count();
                Ghost::inlay(txt!(
                    "{indent_form_empty}{}",
                    indent_str
                        .chars()
                        .chain((0..parts.opts.tabstop as usize - len).map(|_| ' '))
                        .cycle()
                        .take(common_indent)
                        .skip(1)
                        .collect::<String>(),
                ))
            });

            for idx in eq_indent_range.clone() {
                let range = empty_lines[idx].clone();

                tags.insert(tagger, range.start, SwapChar::new(char));
                tags.insert(tagger, range.start, inlay.clone());
                tags.insert(tagger, range, indent_form_empty);
            }
        }
    };

    for (idx, line_range) in empty_lines.iter().cloned().enumerate() {
        parts.tags.remove_excl(tagger, line_range.clone());

        let line = line_range.start.line();

        if line > 0 && line - 1 != prev_line {
            let indent = parts.bytes.indent(line - 1, parts.opts.to_print_opts());
            let common_indent = indent.min(prev_indent);

            indent_lines(&mut parts.tags, eq_indent_range.clone(), common_indent);

            eq_indent_range.start = idx;
            prev_indent = indent;
        }

        eq_indent_range.end = idx + 1;
        prev_line = line;
    }

    let common_indent = if prev_line + 1 < parts.bytes.len().line() {
        prev_indent.min(
            parts
                .bytes
                .indent(prev_line + 1, parts.opts.to_print_opts()),
        )
    } else {
        prev_indent
    };

    indent_lines(&mut parts.tags, eq_indent_range, common_indent);
}

fn hightlight_current_line(parts: &mut BufferParts, tagger: Tagger) {
    static CUR_LINE_INLAY: LazyLock<Ghost> =
        LazyLock::new(|| Ghost::inlay(txt!("[current_line] {Spacer}")));

    let caret = parts.selections.main().caret();
    let line_range = parts.bytes.line_range(caret.line());

    let cur_line_form = form::id_of!("current_line").to_tag(50);

    parts
        .tags
        .insert(tagger, line_range.start, CUR_LINE_INLAY.clone());
    parts.tags.insert(tagger, line_range, cur_line_form);
}
