use std::sync::LazyLock;

use duat_base::widgets::Sections;
use duat_core::{
    Ns,
    data::Pass,
    text::{RegexHaystack, Text},
};
use lsp_types::{Hover, HoverContents, MarkedString, MarkupKind};

pub fn hover(pa: &mut Pass, hover: Hover) {
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
                duat_treesitter::parse_as(text.as_mut(), .., "markdown");
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

    let link_ranges = Vec::from_iter(text.search(r"\[.*?\](\(.*?\))?"));

    for range in link_ranges.into_iter().rev() {
        if text[range.clone()].ends_with(")") {
            let br_range = text[range.clone()].rfind("](").unwrap();
            let br_range = br_range.start + range.start..br_range.end + range.start;

            // Separate in order to avoid removing the ling tag.
            text.replace_range(br_range.start + 1..range.end, "");
            text.replace_range(br_range.start..br_range.start + 1, "");
            text.replace_range(range.start..range.start + 1, "");
        } else {
            text.replace_range(range.end - 1..range.end, "");
            text.replace_range(range.start..range.start + 1, "");
        }
    }

    let title = Some("Hover".to_string());

    if let Some(info) = Sections::get_corner(pa) {
        Sections::set_section(pa, &info, *INFO_NS, text, title, 0);
    } else {
        let info = Sections::new(*INFO_NS, text, title, 0);
        info.spawn_on_corner(pa, true);
    }
}

fn parse_marked_string(marked_string: &MarkedString) -> Text {
    match marked_string {
        MarkedString::String(string) => {
            let mut text = Text::from(string);
            duat_treesitter::parse_as(text.as_mut(), .., "markdown");
            text
        }
        MarkedString::LanguageString(language_string) => {
            let mut text = Text::from(&language_string.value);
            duat_treesitter::parse_as(text.as_mut(), .., &language_string.language);
            text
        }
    }
}

static INFO_NS: LazyLock<Ns> = Ns::new_lazy();
