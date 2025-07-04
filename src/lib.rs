use std::{ops::Range, sync::LazyLock};

use duat_core::prelude::*;
use duat_treesitter::TsParser;

pub struct MatchParens;

impl<U: Ui> Reader<U> for MatchParens {
    fn update_range(&mut self, mut parts: FileParts<U>, _: Option<Range<Point>>) {
        const PARENS: &[&str] = &["(", "[", "{", "<", ")", "]", "}", ">"];

        parts.tags.remove(*PAREN_TAGGER, ..);

        let within = parts.suggested_max_range;

        parts.readers.try_read(|ts: &TsParser| {
            let parens_list = parts
                .selections
                .iter_within(within)
                .flat_map(|(_, sel, is_main)| {
                    let range = sel.range(&parts.bytes);
                    ts.root()
                        .descendant_for_byte_range(range.start, range.end)
                        .and_then(|n| {
                            PARENS
                                .iter()
                                .position(|p| *p == n.grammar_name())
                                .zip(Some((n, is_main)))
                        })
                });

            for (i, (node, is_main)) in parens_list {
                let Some(parent) = node.parent() else {
                    continue;
                };

                let mut cursor = parent.walk();

                let (start_range, end_range) = if i < 4
                    && cursor.goto_last_child()
                    && cursor.node().grammar_name() == PARENS[i + 4]
                {
                    (node.byte_range(), cursor.node().byte_range())
                } else if cursor.goto_first_child() && cursor.node().grammar_name() == PARENS[i - 4]
                {
                    (cursor.node().byte_range(), node.byte_range())
                } else {
                    continue;
                };

                let id = if is_main {
                    form::id_of!("matched_paren.main.start")
                } else {
                    form::id_of!("matched_paren.extra.start")
                };
                parts.tags.insert(*PAREN_TAGGER, start_range, id.to_tag(99));

                let id = if is_main {
                    form::id_of!("matched_paren.main.end")
                } else {
                    form::id_of!("matched_paren.extra.end")
                };
                parts.tags.insert(*PAREN_TAGGER, end_range, id.to_tag(99));
            }
        });
    }
}

impl<U: Ui> ReaderCfg<U> for MatchParens {
    type Reader = Self;

    fn init(self, bytes: RefBytes) -> Result<ReaderBox<U>, Text> {
        Ok(ReaderBox::new_local(bytes, self))
    }
}

static PAREN_TAGGER: LazyLock<Tagger> = Tagger::new_static();
