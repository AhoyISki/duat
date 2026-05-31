use duat_base::widgets::Info;
use duat_core::{
    context,
    data::Pass,
    text::{RegexHaystack, Text},
};
use lsp_types::{Hover, HoverContents, MarkedString, MarkupKind};

use crate::Encoding;

pub fn hover(pa: &mut Pass, encoding: Encoding, hover: Hover) {
    let mut text = match hover.contents {
        HoverContents::Scalar(marked_string) => parse_marked_string(&marked_string),
        HoverContents::Array(marked_strings) => {
            let mut text = Text::new();

            for marked_string in marked_strings {
                text.append_text(text.len(), &parse_marked_string(&marked_string));
            }

            text
        }
        HoverContents::Markup(markup_content) => {
            let mut text = Text::from(markup_content.value);
            if markup_content.kind == MarkupKind::Markdown {
                duat_treesitter::highlight_as(text.as_mut(), .., "markdown");
            }
            text
        }
    };

    let backtick_ranges = Vec::from_iter({
        let line0 = text.line(0);
        (line0 == "\n" || line0 == "\r\n")
            .then_some(line0.byte_range())
            .into_iter()
            .chain(
                text.lines()
                    .filter(|line| line.starts_with("```"))
                    .map(|line| line.byte_range()),
            )
    });

    for range in backtick_ranges.into_iter().rev() {
        if range.start == 0 {
            text.replace_range(range, "");
        } else {
            text.replace_range(range, "\n");
        }
    }

    let multi_nl_ranges = Vec::from_iter(text.search("(\n|\r\n){2,}|\\A(\n|\r\n)"));

    for range in multi_nl_ranges.into_iter().rev() {
        if range.start == 0 {
            text.replace_range(range, "");
        } else {
            text.replace_range(range, "\n\n");
        }
    }

    let title = {
        let buffer = context::current_buffer(pa);
        let text = buffer.text(pa);
        if let Some(range) = hover.range {
            let start = encoding.byte_from_pos(text, range.start);
            let end = encoding.byte_from_pos(text, range.end);

            if let (Some(start), Some(end)) = (start, end) {
                Some(Text::from(text[start..end].to_string()))
            } else {
                None
            }
        } else {
            None
        }
    };

    Info::set_corner(pa, text, title, true);
}

fn parse_marked_string(marked_string: &MarkedString) -> Text {
    match marked_string {
        MarkedString::String(string) => {
            let mut text = Text::from(string);
            duat_treesitter::highlight_as(text.as_mut(), .., "markdown");
            text
        }
        MarkedString::LanguageString(language_string) => {
            let mut text = Text::from(&language_string.value);
            duat_treesitter::highlight_as(text.as_mut(), .., &language_string.language);
            text
        }
    }
}
