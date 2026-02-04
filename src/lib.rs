//! A simple [`Plugin`] to match pairs of parentheses
//!
//! # Installation
//!
//! This is a default plugin, but you can add it manually in order to
//! configure it:
//!
//! ```bash
//! cargo add duat-match-pairs@"*" --rename match-pairs
//! ```
//!
//! Or, if you are using a `--git-deps` version of duat, do this:
//!
//! ```bash
//! cargo add --git https://github.com/AhoyISki/duat-match-pairs --rename match-pairs
//! ```
//!
//! # Usage
//!
//! In order to make use of it, just add the following to your `setup`
//! function:
//!
//! ```rust
//! # #[macro_use]
//! # macro_rules! setup_duat { ($duat:ident) => {} }
//! # mod duat { pub mod prelude { pub fn plug<P>(plugin: P) {} }}
//! setup_duat!(setup);
//! use duat::prelude::*;
//!
//! fn setup() {
//!     plug(duat_match_pairs::MatchPairs::new());
//! }
//! ```
//!
//! In this plugin, there are two types of "pairs", these are the
//! normal pairs and the treesitter pairs. The normal pairs match
//! based on the content of the text itself, so for example, in this
//! situation:
//!
//! ```rust
//! let my_string = "(this is my string)";
//! ```
//!
//! There is a normal pair within the string of `(`,`)`. However,
//! there is no treesitter pair in there, because a treesitter pair
//! only matches if the pairs are on the language's syntax tree.
//!
//! This distinction allows for some combination of pairings that can
//! also be used as non pairs. For example, in Rust, `<`,`>` is a pair
//! only on type arguments and things of the sort, in other cases, it
//! is just a comparison operator. That's where the treesitter pairs
//! come in, as they can identify when it is an actual pair, or just
//! the operator.
//!
//! In order to change what counts as a normal pair and what counts as
//! a treesitter pair, you can add the following to the setup
//! function:
//!
//! ```rust
//! # #[macro_use]
//! # macro_rules! setup_duat { ($duat:ident) => {} }
//! # mod duat { pub mod prelude { pub fn plug<P>(plugin: P) {} }}
//! setup_duat!(setup);
//! use duat::prelude::*;
//!
//! fn setup() {
//!     plug(
//!         duat_match_pairs::MatchPairs::new()
//!             .match_pairs([["\\(", "\\)"], ["\\{", "\\}"], ["\\[", "\\]"]])
//!             .match_ts_pairs([["<", ">"], ["|", "|"]]),
//!     );
//! }
//! ```
//!
//! Two things to note here:
//!
//! - For now, normal pairs only support one character regexes.
//! - Also for now, normal pairs use regex, while treesitter pairs use
//!   strings.
//!
//! [`Plugin`]: duat_core::Plugin
use std::{
    collections::HashMap,
    ops::Range,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    Plugin, Plugins,
    context::Handle,
    data::Pass,
    form,
    hook::{self, BufferUpdated},
    text::{Point, RegexHaystack, Tagger},
    ui::Widget,
};
use duat_filetype::FileType;
use duat_treesitter::TsHandle;

/// highlight the match of delimiters under [`Selection`]s
///
/// [`Selection`]: duat_core::mode::Selection
#[derive(Clone)]
pub struct MatchPairs {
    ts_and_reg: Vec<[&'static [u8]; 2]>,
    ts_only: Vec<[&'static [u8]; 2]>,
    escaped: Vec<[&'static str; 2]>,
}

impl MatchPairs {
    /// Returns a new [`MatchPairs`]
    pub fn new() -> Self {
        Self {
            ts_and_reg: vec![[b"(", b")"], [b"{", b"}"], [b"[", b"]"]],
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

        Self {
            ts_and_reg: ts_and_bytes,
            escaped,
            ..self
        }
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

impl Plugin for MatchPairs {
    fn plug(self, plugins: &Plugins) {
        plugins.require::<duat_treesitter::TreeSitter>();

        hook::add::<BufferUpdated>(move |pa, handle| {
            let file = handle.write(pa);

            let match_pairs_ref = MatchPairsRef {
                ts_and_reg: &self.ts_and_reg,
                ts_only: if let Some(path) = file.path_set()
                    && let Some(filetype) = path.filetype()
                {
                    match filetype {
                        "rust" => &[[b"<".as_slice(), b">"], [b"|", b"|"]],
                        _ => self.ts_only.as_slice(),
                    }
                } else {
                    &self.ts_only
                },
                escaped: &self.escaped,
            };

            let range = handle.full_printed_range(pa);
            match_pairs_ref.update(pa, handle, range);
        });
    }
}

impl Default for MatchPairs {
    fn default() -> Self {
        Self::new()
    }
}

struct MatchPairsRef<'mp> {
    ts_and_reg: &'mp [[&'static [u8]; 2]],
    ts_only: &'mp [[&'static [u8]; 2]],
    escaped: &'mp [[&'static str; 2]],
}

impl MatchPairsRef<'_> {
    fn update(self, pa: &mut Pass, handle: &Handle, range: Range<Point>) {
        fn ends(str: &[u8]) -> impl Fn(&[&[u8]; 2]) -> bool {
            move |delims| delims.contains(&str)
        }

        let buffer = handle.write(pa);

        buffer.text_mut().remove_tags(*PAREN_TAGGER, ..);

        let selections: Vec<_> = buffer
            .selections()
            .iter_within(range)
            .map(|(_, sel, is_main)| (sel.byte_range(buffer.bytes()), is_main))
            .collect();

        'selections: for (c_range, is_main) in selections {
            let str: Vec<u8> = handle.text(pa).slices(c_range.clone()).collect();

            // TODO: Support multi-character pairs
            let (delims, escaped) = if let Some(i) = self.ts_and_reg.iter().position(ends(&str)) {
                (self.ts_and_reg[i], Some(self.escaped[i]))
            } else if let Some(i) = self.ts_only.iter().position(ends(&str)) {
                (self.ts_only[i], None)
            } else {
                continue;
            };

            let get_ts_ranges = |parser: &duat_treesitter::Parser| {
                let node = parser
                    .root_node()
                    .descendant_for_byte_range(c_range.start, c_range.end)
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
            };

            let (start_range, end_range) = if let Some((parser, _)) = handle.get_ts_parser(pa)
                && let Some(ranges) = get_ts_ranges(parser)
            {
                ranges
            } else if let Some(escaped) = escaped {
                if str == delims[0] {
                    let mut iter = handle.text(pa).search(escaped).range(c_range.start..);
                    let mut bounds = 0;

                    loop {
                        let Some((i, m_range)) = iter.next() else {
                            continue 'selections;
                        };
                        bounds = (bounds + (i == 0) as usize) - (i == 1) as usize;
                        if bounds == 0 {
                            break (c_range, m_range);
                        }
                    }
                } else {
                    let mut iter = handle.text(pa).search(escaped).range(..c_range.end);
                    let mut bounds = 0;

                    loop {
                        let Some((i, m_range)) = iter.next_back() else {
                            continue 'selections;
                        };
                        bounds = (bounds + (i == 1) as usize) - (i == 0) as usize;
                        if bounds == 0 {
                            break (m_range, c_range);
                        }
                    }
                }
            } else {
                continue;
            };

            let buffer = handle.write(pa);

            let id = if is_main {
                form::id_of!("matched_pair.main.start")
            } else {
                form::id_of!("matched_pair.extra.start")
            };
            buffer
                .text_mut()
                .insert_tag(*PAREN_TAGGER, start_range, id.to_tag(99));

            let id = if is_main {
                form::id_of!("matched_pair.main.end")
            } else {
                form::id_of!("matched_pair.extra.end")
            };
            buffer
                .text_mut()
                .insert_tag(*PAREN_TAGGER, end_range, id.to_tag(99));
        }
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
