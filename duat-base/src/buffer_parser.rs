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
    text::{FormTag, SwapChar, Tagger, Tags},
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

        replace(parts, parser.opts, printed_line_ranges);
    })
    .grouped("DefaultOptsParser");

    hook::add::<BufferPrinted>(move |pa, handle| {
        handle.text_mut(pa).remove_tags(cur_line_tagger, ..);
    })
    .grouped("DefaultOptsParser");
}

fn hightlight_current_line(_parts: &mut BufferParts, _tagger: Tagger) {
    // let caret = parts.selections.main().caret();
    // let line_range = parts.bytes.line_range(caret.line());

    // let cur_line_form = form::id_of!("current_line").to_tag(50);

    // parts
    //     .tags
    //     .insert(tagger, line_range.start, CUR_LINE_GHOST.clone());
    // parts.tags.insert(tagger, line_range, cur_line_form);
}

fn replace(mut parts: BufferParts, opts: BufferOpts, lines: Vec<Range<usize>>) {
    let (spc_tagger, nl_tagger) = {
        static SPC_TAGGER: LazyLock<Tagger> = Tagger::new_static();
        static NL_TAGGER: LazyLock<Tagger> = Tagger::new_static();
        (*SPC_TAGGER, *NL_TAGGER)
    };

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

    parts
        .ranges_to_update
        .add_ranges(parts.changes.map(|change| change.line_range(parts.bytes)));

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

    for range in parts.ranges_to_update.select_from(lines.iter().cloned()) {
        parts.tags.remove(spc_tagger, range.start..range.end);
        parts.tags.remove_excl(nl_tagger, range.start..=range.end);

        let mut indent_byte = Some(range.start);
        let mut line_is_empty = true;
        let mut first_space_byte = None;

        for (byte, char) in parts.bytes.chars_fwd(range).unwrap() {
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

                    line_is_empty = true;
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
        }
    }

    parts.ranges_to_update.update_on(lines);
}
