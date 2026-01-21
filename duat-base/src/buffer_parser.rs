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
    text::{Ghost, Point, ReplaceChar, Spacer, Tagger, txt},
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

    let nl_tagger = Tagger::new();
    let spc_tagger = Tagger::new();
    let cur_line_tagger = Tagger::new();

    hook::add::<BufferUpdated>(move |pa, handle| {
        let full_range = handle.full_printed_range(pa);
        let (parser, buffer) = PARSERS.write(pa, handle).unwrap();

        let opts_have_changed = buffer.opts != parser.opts;
        parser.opts = buffer.opts;

        let mut parts = TRACKER.parts(buffer).unwrap();

        if opts_have_changed {
            parts.tags.remove(nl_tagger, ..);
            parts.ranges_to_update.add_ranges([..]);
        }

        if parser.opts.highlight_current_line {
            hightlight_current_line(&mut parts, cur_line_tagger);
        }

        replace(parts, parser.opts, [spc_tagger, nl_tagger], full_range);
    })
    .grouped("DefaultOptsParser");

    hook::add::<BufferPrinted>(move |pa, handle| {
        handle.text_mut(pa).remove_tags(cur_line_tagger, ..);
    })
    .grouped("DefaultOptsParser");
}

fn hightlight_current_line(parts: &mut BufferParts, tagger: Tagger) {
    let caret = parts.selections.main().caret();
    let line_range = parts.bytes.line_range(caret.line());
    let line_range = line_range.start.byte()..line_range.end.byte();

    let cur_line_form = form::id_of!("current_line").to_tag(50);

    let ghost = Ghost::new(txt!("{Spacer}\n"));

    parts
        .tags
        .insert(tagger, line_range.end - 1, ReplaceChar(' '));
    parts.tags.insert(tagger, line_range.end, ghost.clone());
    parts.tags.insert(tagger, line_range, cur_line_form);
}

fn replace(mut parts: BufferParts, opts: BufferOpts, spc_tagger: [Tagger; 2], range: Range<Point>) {
    static NL_GHOST: LazyLock<Ghost> = LazyLock::new(|| Ghost::new("\n\n"));

    let [spc_tagger, nl_tagger] = spc_tagger;

    let space_form = form::id_of!("replace.space").to_tag(90);
    let space_form_trailing = form::id_of!("replace.space.trailing").to_tag(90);

    let indent_form = form::id_of!("replace.indent").to_tag(90);
    let indent_form_empty = form::id_of!("replace.indent.empty").to_tag(90);
    let indent_form_tab = form::id_of!("replace.indent.tab").to_tag(90);

    let nl_form = form::id_of!("replace.new_line").to_tag(90);
    let nl_form_empty = form::id_of!("replace.new_line.empty").to_tag(90);
    let nl_form_trailing = form::id_of!("replace.new_line.trailing").to_tag(90);

    let mut indenting = true;
    let mut line_is_empty = true;
    let mut first_space_byte = None;

    parts
        .ranges_to_update
        .add_ranges(parts.changes.map(|change| change.line_range(parts.bytes)));

    for range in parts.ranges_to_update.cutoff([range.clone()]) {
        parts.tags.remove_excl(nl_tagger, range.start..=range.end);
        parts.tags.remove(spc_tagger, range.clone());

        for (byte, char) in parts.bytes.chars_fwd(range).unwrap() {
            if char == ' ' {
                first_space_byte.get_or_insert(byte);
            } else if char == '\n' {
                indenting = true;

                let (nl_char, nl_form) = opts
                    .new_line_trailing
                    .and_then(|char| first_space_byte.and(Some((char, nl_form_trailing))))
                    .or(opts
                        .new_line_on_empty
                        .and_then(|char| line_is_empty.then_some((char, nl_form_empty))))
                    .unwrap_or((opts.new_line_char, nl_form));

                if nl_char != ' ' {
                    parts.tags.insert(nl_tagger, byte..byte + 1, nl_form);
                    parts.tags.insert(nl_tagger, byte, ReplaceChar(nl_char));
                    parts.tags.insert(nl_tagger, byte + 1, NL_GHOST.clone());
                }

                if let Some(first) = first_space_byte.take()
                    && let Some(space_char) = opts.space_char_trailing.or(opts.space_char)
                {
                    let range = first..byte;
                    parts.tags.insert(spc_tagger, range, space_form_trailing);
                    for byte in first..byte {
                        parts.tags.insert(spc_tagger, byte, ReplaceChar(space_char));
                    }
                }
            } else {
                let first_space_byte = first_space_byte.take();
                if indenting {
                } else if let Some(first) = first_space_byte
                    && let Some(space_char) = opts.space_char
                {
                    parts.tags.insert(spc_tagger, first..byte, space_form);
                    for byte in first..byte {
                        parts.tags.insert(spc_tagger, byte, ReplaceChar(space_char));
                    }
                }

                indenting = false;
            }

            line_is_empty = char == '\n';
        }
    }

    parts.ranges_to_update.update_on([range]);
}
