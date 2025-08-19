use std::{
    collections::HashMap,
    ops::Range,
    sync::{LazyLock, Mutex},
};

use duat_core::prelude::*;
use duat_filetype::FileType;
use duat_treesitter::TsParser;

/// [`Plugin`] and [`Parser`] to highlight the match of the delimiter
/// under [`Selection`]s
///
/// [`Selection`]: duat_core::mode::Selection
pub struct MatchPairs {
    ts_and_bytes: Vec<[&'static [u8]; 2]>,
    ts_only: Vec<[&'static [u8]; 2]>,
    escaped: Vec<[&'static str; 2]>,
}

impl MatchPairs {
    /// Returns a new [`MatchPairs`]
    pub fn new() -> Self {
        Self {
            ts_and_bytes: vec![[b"(", b")"], [b"{", b"}"], [b"[", b"]"]],
            // TODO: Add more filetypes
            ts_only: vec![[b"<", b">"]],
            escaped: vec![["\\(", "\\)"], ["\\{", "\\}"], ["\\[", "\\]"]],
        }
    }

    /// Match these pairs _always_
    ///
    /// The counterpart to this is [`match_ts_pairs`], which will
    /// match the pairs only if they are tree-sitter pairings.
    ///
    /// Matching always, as opposed to only tree-sitter pairs can be
    /// useful if you don't have tree-sitter available, or if you are
    /// matching inside comments or strings, where pairs wouldn't show
    /// up as nodes in tree-sitter.
    ///
    /// [`match_ts_pairs`]: Self::match_ts_pairs
    pub fn match_pairs(self, pairs: impl IntoIterator<Item = [&'static str; 2]>) -> Self {
        let ts_and_bytes: Vec<[&'static [u8]; 2]> = pairs
            .into_iter()
            .map(|arr| arr.map(str::as_bytes))
            .collect();
        let escaped = ts_and_bytes
            .iter()
            .map(|[l, r]| [escape(l), escape(r)])
            .collect();

        Self { ts_and_bytes, escaped, ..self }
    }

    /// Match these pairs _only_ when they are tree-sitter pairs
    ///
    /// This can be useful for situations where pairings can also be
    /// interpreted as other things, like `|`, which can be a
    /// delimiter for parameters in a rust closure, but is an "or"
    /// operator most of the time, or `<`/`>`, which are comparison
    /// operators most of the time, but can also delimit things like
    /// types in some languages.
    ///
    /// The counterpart to this is [`match_pairs`], which will always
    /// match, even when the pair is not a tree-sitter pair.
    ///
    /// [`match_pairs`]: Self::match_pairs
    pub fn match_ts_pairs(self, pairs: impl IntoIterator<Item = [&'static str; 2]>) -> Self {
        Self {
            ts_only: pairs
                .into_iter()
                .map(|arr| arr.map(str::as_bytes))
                .collect(),
            ..self
        }
    }
}

impl<U: Ui> Plugin<U> for MatchPairs {
    fn plug(self) {
        hook::add::<File<U>, U>(|_, (mut cfg, _)| {
            cfg.with_parser(MatchPairs::new())
        })
    }
}

impl<U: Ui> Parser<U> for MatchPairs {
    fn update_range(&mut self, mut parts: FileParts<U>, _: Option<Range<Point>>) {
        fn ends(str: &[u8]) -> impl Fn(&[&[u8]; 2]) -> bool {
            move |delims| delims.contains(&str)
        }
        parts.tags.remove(*PAREN_TAGGER, ..);

        let within = parts.suggested_max_range;

        'selections: for (_, selection, is_main) in parts.selections.iter_within(within) {
            let range = selection.range(parts.bytes);
            let str: Vec<u8> = parts.bytes.buffers(range.clone()).collect();

            // TODO: Support multi-character pairs
            let (delims, escaped) = if let Some(i) = self.ts_and_bytes.iter().position(ends(&str)) {
                (self.ts_and_bytes[i], Some(self.escaped[i]))
            } else if let Some(i) = self.ts_only.iter().position(ends(&str)) {
                (self.ts_only[i], None)
            } else {
                continue;
            };

            let (start_range, end_range) = if let Some(Some(ranges)) =
                parts.parsers.try_read(|ts: &TsParser| {
                    let node = ts
                        .root()
                        .and_then(|root| root.descendant_for_byte_range(range.start, range.end))
                        .and_then(|node| {
                            delims
                                .iter()
                                .position(|d| *d == node.grammar_name().as_bytes())
                                .zip(Some(node))
                        });
                    let ((delim_side, node), parent) =
                        node.and_then(|(ds, n)| Some((ds, n)).zip(n.parent()))?;

                    let mut c = parent.walk();

                    if delim_side == 0
                        && (c.goto_first_child() && c.node() == node && c.goto_parent())
                        && (c.goto_last_child() && c.node().grammar_name().as_bytes() == delims[1])
                    {
                        Some((node.byte_range(), c.node().byte_range()))
                    } else if (c.goto_last_child() && c.node() == node && c.goto_parent())
                        && (c.goto_first_child() && c.node().grammar_name().as_bytes() == delims[0])
                    {
                        Some((c.node().byte_range(), node.byte_range()))
                    } else {
                        None
                    }
                }) {
                ranges
            } else if let Some(escaped) = escaped {
                if str == delims[0] {
                    let mut iter = parts.bytes.search_fwd(escaped, range.start..).unwrap();
                    let mut bounds = 0;

                    loop {
                        let Some((i, points)) = iter.next() else {
                            continue 'selections;
                        };
                        bounds = (bounds + (i == 0) as usize) - (i == 1) as usize;
                        if bounds == 0 {
                            break (range, points[0].byte()..points[1].byte());
                        }
                    }
                } else {
                    let mut iter = parts.bytes.search_rev(escaped, ..range.end).unwrap();
                    let mut bounds = 0;

                    loop {
                        let Some((i, points)) = iter.next() else {
                            continue 'selections;
                        };
                        bounds = (bounds + (i == 1) as usize) - (i == 0) as usize;
                        if bounds == 0 {
                            break (points[0].byte()..points[1].byte(), range);
                        }
                    }
                }
            } else {
                continue;
            };

            let id = if is_main {
                form::id_of!("matched_pair.main.start")
            } else {
                form::id_of!("matched_pair.extra.start")
            };
            parts.tags.insert(*PAREN_TAGGER, start_range, id.to_tag(99));

            let id = if is_main {
                form::id_of!("matched_pair.main.end")
            } else {
                form::id_of!("matched_pair.extra.end")
            };
            parts.tags.insert(*PAREN_TAGGER, end_range, id.to_tag(99));
        }
    }
}

impl<U: Ui> ParserCfg<U> for MatchPairs {
    type Parser = Self;

    fn init(mut self, file: &File<U>) -> Result<ParserBox<U>, Text> {
        if let Some(path) = file.path_set()
            && let Some(path) = path.filetype()
        {
            self.ts_only = match path {
                "rust" => vec![[b"<", b">"], [b"|", b"|"]],
                _ => self.ts_only,
            }
        };

        Ok(ParserBox::new(file, self))
    }
}

impl Default for MatchPairs {
    fn default() -> Self {
        Self::new()
    }
}

static PAREN_TAGGER: LazyLock<Tagger> = Tagger::new_static();

/// Escapes regex pattern characters.
fn escape(str: &'static [u8]) -> &'static str {
    static TOKENS: &[u8] = b"(){}[]^$.+*?|";
    static ESCAPED_STRS: LazyLock<Mutex<HashMap<Vec<u8>, &str>>> = LazyLock::new(Mutex::default);

    let mut escaped_strs = ESCAPED_STRS.lock().unwrap();

    if let Some(escaped) = escaped_strs.get(str) {
        escaped
    } else {
        // SAFETY: This str would have originally come from a &str
        let mut escaped = unsafe { str::from_utf8_unchecked(str) }.to_string();
        for (i, _) in str.iter().enumerate().filter(|(_, b)| TOKENS.contains(b)) {
            escaped.insert(i, '\\');
        }

        let escaped = escaped.leak();
        escaped_strs.insert(str.to_vec(), escaped);

        escaped
    }
}
