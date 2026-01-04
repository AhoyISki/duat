//! A [tree-sitter] implementation for Duat
//!
//! `duat-treesitter` currently does two things:
//!
//! * Syntax highlighting
//! * Indentation calculation
//!
//! # Installation
//!
//! Just like other Duat plugins, this one can be installed by calling
//! `cargo add` in the config directory:
//!
//! ```bash
//! cargo add duat-treesitter@"*"
//! ```
//!
//! Or, if you are using a `--git-deps` version of duat, do this:
//!
//! ```bash
//! cargo add --git https://github.com/AhoyISki/duat-treesitter
//! ```
//!
//! But this is a default plugin, so you most likely won't have to do
//! that.
//!
//! [tree-sitter]: https://tree-sitter.github.io/tree-sitter
use std::{
    collections::{HashMap, HashSet},
    fs,
    ops::RangeBounds,
    path::{Path, PathBuf},
    sync::{LazyLock, Mutex},
};

use duat_core::{
    Plugins,
    buffer::Buffer,
    context::{self, Handle},
    data::Pass,
    form::{self, Form},
    text::{Builder, Text, txt},
};
use tree_sitter::{Language, Node, Query};

use crate::languages::get_language;
pub use crate::parser::Parser;

mod cursor;
mod languages;
mod parser;
mod tree;

/// The [tree-sitter] plugin for Duat
///
/// For now, it adds syntax highlighting and indentation, but more
/// features will be coming in the future.
///
/// These things are done through the [`TsParser`] [`Parser`], which
/// reads updates the inner syntax tree when the [`Text`] reports any
/// changes.
///
/// # NOTE
///
/// If you are looking to create a [`Parser`] which can do similar
/// things, you should look at the code for the implementation of
/// [`Parser`] for [`TsParser`], it's relatively short and with good
/// explanations for what is happening.
///
/// [tree-sitter]: https://tree-sitter.github.io/tree-sitter
#[derive(Default)]
pub struct TreeSitter;

impl duat_core::Plugin for TreeSitter {
    fn plug(self, _: &Plugins) {
        fn copy_dir_all(src: &include_dir::Dir, dst: impl AsRef<Path>) -> std::io::Result<()> {
            fs::create_dir_all(&dst)?;
            for entry in src.entries() {
                if let Some(dir) = entry.as_dir() {
                    copy_dir_all(dir, dst.as_ref().join(entry.path().file_name().unwrap()))?;
                } else {
                    fs::write(
                        dst.as_ref().join(entry.path().file_name().unwrap()),
                        entry.as_file().unwrap().contents(),
                    )?
                }
            }
            Ok(())
        }

        static QUERIES: include_dir::Dir = include_dir::include_dir!("$CARGO_MANIFEST_DIR/queries");

        let Ok(plugin_dir) = duat_core::utils::plugin_dir("duat-treesitter") else {
            context::error!("No local directory, queries aren't installed");
            return;
        };

        let dest = plugin_dir.join("queries");
        match dest.try_exists() {
            Ok(false) => match copy_dir_all(&QUERIES, &dest) {
                Ok(_) => {
                    context::info!("Installed tree-sitter queries at [buffer]{dest}");
                }
                Err(err) => {
                    context::info!(
                        "Failed to install tree-sitter queries at [buffer]{dest}: {err}"
                    );
                }
            },
            Ok(true) => {}
            Err(err) => {
                context::warn!("Coudn't confirm existance of [buffer]{dest}: {err}")
            }
        }

        form::set_many_weak!(
            ("variable", Form::white()),
            ("variable.builtin", Form::dark_yellow()),
            ("constant", Form::grey()),
            ("constant.builtin", Form::dark_yellow()),
            ("module", Form::blue().italic()),
            ("label", Form::green()),
            ("string", Form::green()),
            ("character", Form::dark_yellow()),
            ("boolean", Form::dark_yellow()),
            ("number", Form::dark_yellow()),
            ("type", Form::yellow().italic()),
            ("type.builtin", Form::yellow().reset()),
            ("attribute", Form::green()),
            ("property", Form::green()),
            ("function", Form::blue().reset()),
            ("constructor", Form::dark_yellow().reset()),
            ("operator", Form::cyan()),
            ("keyword", Form::magenta()),
            ("punctuation.bracket", Form::grey()),
            ("punctuation.delimiter", Form::grey()),
            ("comment", Form::grey()),
            ("comment.documentation", Form::grey().bold()),
            ("markup.strong", Form::bold()),
            ("markup.italic", Form::italic()),
            ("markup.strikethrough", Form::crossed_out()),
            ("markup.underline", Form::underlined()),
            ("markup.heading", Form::blue().bold()),
            ("markup.math", Form::yellow()),
            ("markup.quote", Form::grey().italic()),
            ("markup.link", Form::blue().underlined()),
            ("markup.raw", Form::cyan()),
            ("markup.list", Form::yellow()),
            ("markup.list.checked", Form::green()),
            ("markup.list.unchecked", Form::grey()),
            ("diff.plus", Form::red()),
            ("diff.delta", Form::blue()),
            ("diff.minus", Form::green()),
            ("node.field", "variable.member"),
        );

        parser::add_parser_hook();
    }
}

type LangParts<'a> = (&'a str, &'a Language, Queries<'a>);

#[derive(Clone, Copy)]
struct Queries<'a> {
    highlights: &'a Query,
    indents: &'a Query,
    injections: &'a Query,
}

fn lang_parts_of(lang: &str, handle: &Handle) -> Option<LangParts<'static>> {
    static MAPS: LazyLock<Mutex<HashMap<&str, LangParts<'static>>>> = LazyLock::new(Mutex::default);
    static FAILED_PARTS: LazyLock<Mutex<HashSet<String>>> = LazyLock::new(Mutex::default);

    let mut maps = MAPS.lock().unwrap();

    if let Some(lang_parts) = maps.get(lang).copied() {
        Some(lang_parts)
    } else if FAILED_PARTS.lock().unwrap().contains(lang) {
        None
    } else {
        let language: &'static Language = Box::leak(Box::new(get_language(lang, handle)?));

        let get_queries = || {
            let highlights = query_from_path(lang, "highlights", language).ok()?;
            let indents = query_from_path(lang, "indents", language).ok()?;
            let injections = query_from_path(lang, "injections", language).ok()?;
            Some(Queries { highlights, indents, injections })
        };

        let Some(queries) = get_queries() else {
            FAILED_PARTS.lock().unwrap().insert(lang.to_string());
            return None;
        };

        let lang: &'static str = lang.to_string().leak();

        maps.insert(lang, (lang, language, queries));

        Some((lang, language, queries))
    }
}

/// Returns a new [`Query`] for a given language and kind
///
/// If the [`Query`] in question does not exist, returns an emtpy
/// [`Query`] instead.
fn query_from_path(name: &str, kind: &str, language: &Language) -> Result<&'static Query, Text> {
    static QUERIES: LazyLock<Mutex<HashMap<PathBuf, &'static Query>>> =
        LazyLock::new(Mutex::default);

    let queries_dir = duat_core::utils::plugin_dir("duat-treesitter")?.join("queries");

    let path = queries_dir.join(name).join(kind).with_extension("scm");

    let mut queries = QUERIES.lock().unwrap();

    Ok(if let Some(query) = queries.get(&path) {
        query
    } else {
        let Ok(mut query) = fs::read_to_string(&path) else {
            let query = Box::leak(Box::new(Query::new(language, "").unwrap()));
            queries.insert(path, query);
            return Ok(query);
        };

        let Some(first_line) = query.lines().map(String::from).next() else {
            context::warn!("Query is empty");
            let query = Box::leak(Box::new(Query::new(language, "").unwrap()));
            queries.insert(path, query);
            return Ok(query);
        };

        if let Some(langs) = first_line.strip_prefix("; inherits: ") {
            for name in langs.split(',') {
                let path = queries_dir.join(name).join(kind).with_extension("scm");
                match fs::read_to_string(&path) {
                    Ok(inherited_query) => {
                        if inherited_query.is_empty() {
                            context::warn!("Inherited query is empty");
                        }

                        query = format!("{inherited_query}\n{query}");
                    }
                    Err(err) => context::error!("{err}"),
                }
            }
        }

        let query = Box::leak(Box::new(match Query::new(language, &query) {
            Ok(query) => query,
            Err(err) => return Err(txt!("{err}")),
        }));

        queries.insert(path, query);

        query
    })
}

/// Convenience methods for use of tree-sitter in [`Buffer`]s
pub trait TsHandle {
    fn get_ts_parser<'p>(&'p self, pa: &'p mut Pass) -> Option<(&'p Parser, &'p Buffer)>;

    /// Gets the tree sitter indentation values for all the
    /// selections, from the `start`th selection, to the `end`th
    /// selection
    ///
    /// Returns [`None`] if tree-sitter isn't enabled for the current
    /// buffer, either because there is no queries for the [filetype]
    /// or because there is no filetype at all.
    ///
    /// [`caret`]: duat_core::mode::Selection::caret
    /// [filetype]: duat_filetype::FileType::filetype
    fn ts_get_indentations(
        &self,
        pa: &mut Pass,
        selections: impl RangeBounds<usize> + Clone,
    ) -> Option<Vec<usize>>;
}

impl TsHandle for Handle {
    fn get_ts_parser<'p>(&'p self, pa: &'p mut Pass) -> Option<(&'p Parser, &'p Buffer)> {
        parser::sync_parse(pa, self)
    }

    fn ts_get_indentations(
        &self,
        pa: &mut Pass,
        selections: impl RangeBounds<usize> + Clone,
    ) -> Option<Vec<usize>> {
        let range = duat_core::utils::get_range(selections, self.selections(pa).len());

        let carets: Vec<usize> = self
            .selections(pa)
            .iter()
            .enumerate()
            .take(range.end)
            .skip(range.start)
            .map(|(_, (sel, _))| sel.caret().byte())
            .collect();

        let (parser, buffer) = parser::sync_parse(pa, self)?;

        carets
            .into_iter()
            .map(|byte| {
                let bytes = buffer.bytes();
                parser.indent_on(bytes.point_at_byte(byte), bytes, buffer.opts)
            })
            .collect()
    }
}

#[allow(unused)]
fn format_root(node: Node) -> Text {
    fn format_range(node: Node, builder: &mut Builder) {
        let mut first = true;
        for point in [node.start_position(), node.end_position()] {
            builder.push(txt!(
                "[punctuation.bracket.TreeView][[[coords.TreeView]{}\
             	 [punctuation.delimiter.TreeView],[] [coords.TreeView]{}\
             	 [punctuation.bracket.TreeView]]]",
                point.row,
                point.column
            ));

            if first {
                first = false;
                builder.push(txt!("[punctuation.delimiter],[] "));
            }
        }
        builder.push("\n");
    }

    fn format_node(
        node: Node,
        depth: usize,
        pars: usize,
        builder: &mut Builder,
        name: Option<&str>,
    ) {
        builder.push("  ".repeat(depth));

        if let Some(name) = name {
            builder.push(txt!("[node.field]{name}[punctuation.delimiter.TreeView]: "));
        }

        builder.push(txt!("[punctuation.bracket.TreeView]("));
        builder.push(txt!("[node.name]{}", node.grammar_name()));

        let mut cursor = node.walk();
        let named_children = node.named_children(&mut cursor);
        let len = named_children.len();

        if len == 0 {
            builder.push(txt!(
                "[punctuation.bracket.TreeView]{}[] ",
                ")".repeat(pars)
            ));
            format_range(node, builder);
        } else {
            builder.push(" ");
            format_range(node, builder);

            let mut i = 0;

            for (i, child) in named_children.enumerate() {
                let name = node.field_name_for_named_child(i as u32);
                let pars = if i == len - 1 { pars + 1 } else { 1 };
                format_node(child, depth + 1, pars, builder, name);
            }
        }
    }

    let mut cursor = node.walk();
    let mut builder = Text::builder();

    format_node(node, 0, 1, &mut builder, None);

    builder.build()
}
