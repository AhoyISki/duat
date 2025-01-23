use std::{ops::Range, path::Path, sync::LazyLock};

use gapbuf::GapBuffer;
use parking_lot::Mutex;
use streaming_iterator::StreamingIterator;
use tree_sitter::{
    InputEdit, Language, Node, Parser, Point as TSPoint, Query, QueryCursor, TextProvider, Tree,
};

use super::{Key, Text};
use crate::{
    form::{self, FormId},
    text::{Change, Point, Tag},
};

/// A [`Text`] reader, modifying it whenever a [`Change`] happens
pub trait Reader: Send + Sync + 'static {
    /// What should happen when this [`Text`] is first created
    fn new(text: &mut Text) -> Self
    where
        Self: Sized;

    fn before_change(&mut self, text: &mut Text, change: Change<&str>);

    /// What should happen whenever a [`Change`] happens
    fn after_change(&mut self, text: &mut Text, change: Change<&str>);
}

pub struct TreeSitter {
    parser: Parser,
    query: Query,
    tree: Tree,
    forms: &'static [(FormId, Key, Key)],
    lang: &'static str,
}

impl TreeSitter {
    pub fn new(text: &mut Text, path: impl AsRef<Path>) -> Option<Self> {
        let (name, lang, hl) = lang_from_path(path)?;

        let mut parser = Parser::new();
        parser.set_language(lang).unwrap();
        let query = Query::new(lang, hl).unwrap();

        let tree = parser.parse_with(&mut buf_parse(text), None).unwrap();
        let mut cursor = QueryCursor::new();
        let buf = TsBuf(&text.buf);
        let mut captures = cursor.captures(&query, tree.root_node(), buf);

        let forms = forms_from_query("Rust", &query);

        while let Some((captures, _)) = captures.next() {
            for cap in captures.captures.iter() {
                let range = cap.node.range();
                let (start, end) = (range.start_byte as u32, range.end_byte as u32);
                let (form, start_key, end_key) = forms[cap.index as usize];
                if start != end {
                    text.tags.insert(start, Tag::PushForm(form), start_key);
                    text.tags.insert(end, Tag::PopForm(form), end_key);
                }
            }
        }

        Some(TreeSitter { parser, query, tree, forms, lang: name })
    }

    pub(super) fn after_change(&mut self, text: &mut Text, change: Change<&str>) {
        let start = change.start();
        let added = change.added_end();
        let taken = change.taken_end();

        let start_position = ts_point(start, text);
        let prev = (start_position.column, start);
        self.tree.edit(&InputEdit {
            start_byte: start.byte() as usize,
            old_end_byte: taken.byte() as usize,
            new_end_byte: added.byte() as usize,
            start_position,
            old_end_position: ts_point_from_prev(taken, prev, text),
            new_end_position: ts_point_from_prev(added, prev, text),
        });

        let tree = self
            .parser
            .parse_with(&mut buf_parse(text), Some(&self.tree))
            .unwrap();

        let changes = self.tree.changed_ranges(&tree);
        // If no change is reported, we will add tags on the added range.
        let added = changes.is_empty().then_some(start.byte()..added.byte());

        let mut cursor = QueryCursor::new();
        let buf = TsBuf(&text.buf);

        let ranges = changes.map(|c| c.start_byte as u32..c.end_byte as u32);
        for range in ranges.chain(added) {
            let Some((start, end)) = range_to_change(range.clone(), &self.tree, &tree) else {
                continue;
            };
            cursor.set_byte_range(start..end);

            let mut captures = cursor.captures(&self.query, self.tree.root_node(), buf);
            while let Some((captures, _)) = captures.next() {
                for cap in captures.captures.iter() {
                    let range = cap.node.range();
                    let (start, end) = (range.start_byte as u32, range.end_byte as u32);
                    let (_, start_key, end_key) = self.forms[cap.index as usize];
                    text.tags.remove_at(start, start_key);
                    text.tags.remove_at(end, end_key);
                }
            }

            let mut captures = cursor.captures(&self.query, tree.root_node(), buf);
            while let Some((captures, _)) = captures.next() {
                for cap in captures.captures.iter() {
                    let range = cap.node.range();
                    let (start, end) = (range.start_byte as u32, range.end_byte as u32);
                    let (form, start_key, end_key) = self.forms[cap.index as usize];
                    if start != end {
                        text.tags.insert(start, Tag::PushForm(form), start_key);
                        text.tags.insert(end, Tag::PopForm(form), end_key);
                    }
                }
            }
        }

        self.tree = tree;
    }

    pub fn lang(&self) -> &'static str {
        self.lang
    }
}

fn range_to_change(range: Range<u32>, old: &Tree, new: &Tree) -> Option<(usize, usize)> {
    let (start, end) = (range.start as usize, range.end as usize);
    let old = old.root_node();
    let old_desc = old.descendant_for_byte_range(start, end).unwrap();

    let new = new.root_node();
    let new_desc = new.descendant_for_byte_range(start, end).unwrap();
    let parent = new.child_with_descendant(new_desc).unwrap_or(new);

    (!nodes_are_equal(old_desc, new_desc)).then_some((parent.start_byte(), parent.end_byte()))
}

fn buf_parse<'a>(text: &'a Text) -> impl FnMut(usize, TSPoint) -> &'a [u8] {
    let [s0, s1] = text.strs();
    |byte, _point| {
        if byte < s0.len() {
            s0[byte..].as_bytes()
        } else {
            s1[byte - s0.len()..].as_bytes()
        }
    }
}

fn ts_point(point: Point, text: &Text) -> TSPoint {
    let strs = text.strs_in_range((Point::default(), point));
    let iter = strs.into_iter().flat_map(str::bytes);
    let col = iter.take_while(|&b| b != b'\n').count();

    TSPoint::new(point.line() as usize, col)
}

fn ts_point_from_prev(point: Point, prev: (usize, Point), text: &Text) -> TSPoint {
    let (col, prev) = prev;
    let strs = text.strs_in_range((prev, point));
    let iter = strs.into_iter().flat_map(str::bytes);

    let col = if point.line() == prev.line() {
        col + iter.count()
    } else {
        iter.take_while(|&b| b != b'\n').count()
    };

    TSPoint::new(point.line() as usize, col)
}

fn forms_from_query(lang: &'static str, query: &Query) -> &'static [(FormId, Key, Key)] {
    static LISTS: Mutex<Vec<(&'static str, &[(FormId, Key, Key)])>> = Mutex::new(Vec::new());
    let mut lists = LISTS.lock();

    if let Some((_, forms)) = lists.iter().find(|(l, _)| *l == lang) {
        forms
    } else {
        let mut forms = Vec::new();
        for name in query.capture_names() {
            forms.push(if name.contains('.') {
                let refed = name.split('.').next().unwrap().to_string().leak();
                (form::set_weak(name, refed), Key::new(), Key::new())
            } else {
                (form::set_weak(name, "Default"), Key::new(), Key::new())
            });
        }

        lists.push((lang, forms.leak()));
        lists.last().unwrap().1
    }
}

impl<'a> TextProvider<&'a [u8]> for TsBuf<'a> {
    type I = std::array::IntoIter<&'a [u8], 2>;

    fn text(&mut self, node: tree_sitter::Node) -> Self::I {
        let range = node.range();
        let (s0, s1) = self.0.range(range.start_byte..range.end_byte).as_slices();

        [s0, s1].into_iter()
    }
}

#[derive(Clone, Copy)]
struct TsBuf<'a>(&'a GapBuffer<u8>);

#[allow(unused)]
#[cfg(debug_assertions)]
fn log_node(node: Node) {
    use std::fmt::Write;

    let mut cursor = node.walk();
    let mut node = Some(cursor.node());
    let mut log = String::new();
    while let Some(no) = node {
        let indent = " ".repeat(cursor.depth() as usize);
        writeln!(log, "{indent}{no:?}").unwrap();
        let mut next_exists = cursor.goto_first_child() || cursor.goto_next_sibling();
        while !next_exists && cursor.goto_parent() {
            next_exists = cursor.goto_next_sibling();
        }
        node = next_exists.then_some(cursor.node());
    }

    crate::log_file!("{log}");
}

fn lang_from_path(
    path: impl AsRef<Path>,
) -> Option<(&'static str, &'static Language, &'static str)> {
    static LANGUAGES: LazyLock<Mutex<Vec<((&str, &str, &str), &Language, &str)>>> =
        LazyLock::new(|| {
            macro lang($lang:ident) {
                Box::leak(Box::new(${concat(tree_sitter_, $lang)}::LANGUAGE.into()))
            }
            macro high($lang:ident) {
                include_str!(concat!(
                    "../../../ts-queries/",
                    stringify!($lang),
                    "/highlights.scm"
                ))
            }

            Mutex::new(vec![
                //(("c", "C", "c"), lang!(cpp)),
                (("cc", "C++", "cpp"), lang!(cpp), high!(cpp)),
                (("cpp", "C++", "cpp"), lang!(cpp), high!(cpp)),
                //        (".cl", "Common Lisp", "common-lisp"),
                //        (".clj", "Clojure", "clojure"),
                //        (".comp", "GLSL", "glsl"),
                //        (".cs", "C#", "csharp"),
                //        (".css", "CSS", "css"),
                (("cxx", "C++", "cpp"), lang!(cpp), high!(cpp)),
                //        (".dart", "Dart", "dart"),
                //        (".frag", "GLSL", "glsl"),
                //        (".geom", "GLSL", "glsl"),
                //        (".glsl", "GLSL", "glsl"),
                //        (".go", "Go", "go"),
                //        (".h", "C", "c"),
                //        (".haml", "Haml", "haml"),
                //        (".handlebars", "Handlebars", "handlebars"),
                //        (".hbs", "Handlebars", "handlebars"),
                //        (".hlsl", "HLSL", "HLSL"),
                (("hpp", "C++", "cpp"), lang!(cpp), high!(cpp)),
                //        (".html", "HTML", "html"),
                (("hxx", "C++", "cpp"), lang!(cpp), high!(cpp)),
                //        (".ini", "INI", "ini"),
                //        (".java", "Java", "java"),
                //        (".jinja", "Jinja", "jinja"),
                //        (".jinja2", "Jinja", "jinja"),
                //        (".js", "JavaScript", "javascript"),
                //        (".json", "JSON", "json"),
                //        (".jsonc", "JSON with Comments", "jsonc"),
                //        (".kt", "Kotlin", "kotlin"),
                //        (".less", "Less", "less"),
                //        (".lua", "Lua", "lua"),
                //        (".md", "Markdown", "markdown"),
                //        (".pl", "Perl", "perl"),
                //        (".py", "Python", "python"),
                //        (".pyc", "Python", "python"),
                //        (".pyo", "Python", "python"),
                //        (".rb", "Ruby", "ruby"),
                //        (".rkt", "Racket", "racket"),
                (("rs", "Rust", "rust"), lang!(rust), high!(rust)),
                //        (".sass", "SASS", "sass"),
                //        (".sc", "Scala", "scala"),
                //        (".scala", "Scala", "scala"),
                //        (".scss", "SCSS", "scss"),
                //        (".sh", "Shell", "shell"),
                //        (".sql", "SQL", "sql"),
                //        (".swift", "Swift", "swift"),
                //        (".tesc", "GLSL", "glsl"),
                //        (".tese", "GLSL", "glsl"),
                //        (".tex", "TeX", "tex"),
                //        (".toml", "TOML", "toml"),
                //        (".ts", "TypeScript", "typescript"),
                //        (".vert", "GLSL", "glsl"),
                //        (".xhtml", "XHTML", "xhtml"),
                //        (".xml", "XML", "xml"),
                //        (".yaml", "YAML", "yaml"),
                //        (".yml", "YAML", "yaml"),
            ])
        });

    let ext = path.as_ref().extension()?.to_str()?;
    let langs = LANGUAGES.lock();
    langs
        .binary_search_by_key(&ext, |((ext, ..), ..)| ext)
        .ok()
        .map(|i| {
            let ((_, name, _), lang, hl) = langs.get(i).unwrap();
            (*name, *lang, *hl)
        })
}

fn nodes_are_equal(old: Node, new: Node) -> bool {
    let (mut old_c, mut new_c) = (old.walk(), new.walk());

    let mut old_node = Some(old_c.node());
    let mut new_node = Some(new_c.node());
    let mut old_depth = 0;
    let mut new_depth = 0;
    loop {
        match (old_node, new_node) {
            (Some(old), Some(new)) => {
                if old.range() != new.range() || old.kind_id() != new.kind_id() {
                    break false;
                }
            }
            (None, None) => break true,
            (Some(_), None) | (None, Some(_)) => break false,
        }

        let mut next_exists = old_c.goto_first_child();
        old_depth += next_exists as usize;
        next_exists = old_c.goto_next_sibling();
        while !next_exists && old_depth > 0 && old_c.goto_parent() {
            old_depth -= 1;
            next_exists = old_c.goto_next_sibling();
        }
        old_node = next_exists.then_some(old_c.node());

        let mut next_exists = new_c.goto_first_child();
        new_depth += next_exists as usize;
        next_exists = new_c.goto_next_sibling();
        while !next_exists && new_depth > 0 && new_c.goto_parent() {
            new_depth -= 1;
            next_exists = new_c.goto_next_sibling();
        }
        new_node = next_exists.then_some(new_c.node());
    }
}
