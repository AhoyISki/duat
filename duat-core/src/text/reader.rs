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
    fn before_change(&mut self, text: &mut Text, change: Change<&str>);

    /// What should happen whenever a [`Change`] happens
    ///
    /// Should return a list of [`Range`]s to check
    fn after_change(
        &mut self,
        text: &mut Text,
        change: Change<&str>,
        within: Range<u32>,
    ) -> Vec<Range<u32>>;

    /// Updates a given [`Range`]
    ///
    /// This should take into account all changes that have taken
    /// place before this point.
    ///
    /// Must return a [`Vec`] of ranges that have been checked, so
    /// those can be removed from the checking list.
    fn update_range(&mut self, text: &mut Text, within: Range<u32>) -> Vec<Range<u32>>;
}

pub struct TreeSitter {
    parser: Parser,
    query: Query,
    tree: Tree,
    forms: &'static [(FormId, Key, Key)],
    name: &'static str,
    keys: Range<Key>,
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

        let (keys, forms) = forms_from_query(name, &query);

        crate::log_file!("start");
        crate::log_file!("");

        while let Some((captures, _)) = captures.next() {
            for cap in captures.captures.iter() {
                crate::log_file!("{cap:?}");
                let range = cap.node.range();
                let (start, end) = (range.start_byte as u32, range.end_byte as u32);
                let (form, start_key, end_key) = forms[cap.index as usize];
                if start != end {
                    text.tags.insert(start, Tag::PushForm(form), start_key);
                    text.tags.insert(end, Tag::PopForm(form), end_key);
                }
            }
        }

        crate::log_file!("");

        Some(TreeSitter { parser, query, tree, forms, name, keys })
    }

    pub fn lang(&self) -> &'static str {
        self.name
    }
}

impl Reader for TreeSitter {
    fn before_change(&mut self, _text: &mut Text, _change: Change<&str>) {}

    fn after_change(
        &mut self,
        text: &mut Text,
        change: Change<&str>,
        within: Range<u32>,
    ) -> Vec<Range<u32>> {
        let within = within.start as usize..within.end as usize;
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

        let mut cursor = QueryCursor::new();
        let buf = TsBuf(&text.buf);

        let start_b = text.point_at_line(start.line()).byte() as usize;
        let end_b = text.point_at_line(added.line() + 1).byte() as usize;

        let range = start_b..end_b;
        let mut ml_ranges = Vec::new();
        cursor.set_byte_range(range.clone());

        // The next two loops concern themselves with determining what regions
        // of the file have been altered.
        // Normally, this is assumed to be just the current line, but in the
        // cases of primarily strings and ml comments, this will (probably)
        // involve the entire rest of the file below.
        let mut query_matches = cursor.captures(&self.query, self.tree.root_node(), buf);
        while let Some((query_match, _)) = query_matches.next() {
            for cap in query_match.captures.iter() {
                let range = cap.node.range();
                if range.start_point.row != range.end_point.row {
                    ml_ranges.push((range, cap.index));
                }
            }
        }

        let mut query_matches = cursor.captures(&self.query, tree.root_node(), buf);
        'ml_range_edited: while let Some((query_match, _)) = query_matches.next() {
            for cap in query_match.captures.iter() {
                let range = cap.node.range();
                let entry = (range, cap.index);

                // If a ml range existed before and after the change, then it can be
                // assumed that it was not altered.
                // If it exists in only one of those times, then the whole rest of the
                // file ahead must be checked, as ml ranges like strings will always
                // alter every subsequent line in the file.
                if range.start_point.row != range.end_point.row
                    && ml_ranges.extract_if(.., |r| *r == entry).next().is_none()
                {
                    ml_ranges.push(entry);
                    break 'ml_range_edited;
                }
            }
        }

        if ml_ranges.is_empty() {
            let range = range.start as u32..range.end as u32;
            text.tags.remove_from(range, self.keys.clone());
        } else {
            cursor.set_byte_range(range.start..within.end);
            let range = range.start as u32..within.end as u32;
            text.tags.remove_from(range, self.keys.clone());
        };

        let mut query_matches = cursor.captures(&self.query, tree.root_node(), buf);
        while let Some((query_match, _)) = query_matches.next() {
            for cap in query_match.captures.iter() {
                crate::log_file!("{cap:?}");
                let bytes = cap.node.byte_range();
                let (form, start_key, end_key) = self.forms[cap.index as usize];
                if bytes.start != bytes.end {
                    text.tags
                        .insert(bytes.start as u32, Tag::PushForm(form), start_key);
                    text.tags
                        .insert(bytes.end as u32, Tag::PopForm(form), end_key);
                }
            }
        }

        self.tree = tree;

        if ml_ranges.is_empty() {
            Vec::new()
        } else {
            vec![within.end as u32..text.len().byte()]
        }
    }

    fn update_range(&mut self, text: &mut Text, range: Range<u32>) -> Vec<Range<u32>> {
        let buf = TsBuf(&text.buf);

        text.tags.remove_from(range.clone(), self.keys.clone());

        let mut cursor = QueryCursor::new();
        let mut query_matches = cursor.captures(&self.query, self.tree.root_node(), buf);
        while let Some((query_match, _)) = query_matches.next() {
            for cap in query_match.captures.iter() {
                let bytes = cap.node.byte_range();
                let (form, start_key, end_key) = self.forms[cap.index as usize];
                if bytes.start != bytes.end {
                    text.tags
                        .insert(bytes.start as u32, Tag::PushForm(form), start_key);
                    text.tags
                        .insert(bytes.start as u32, Tag::PopForm(form), end_key);
                }
            }
        }

        vec![range]
    }
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

fn forms_from_query(
    lang: &'static str,
    query: &Query,
) -> (Range<Key>, &'static [(FormId, Key, Key)]) {
    static LISTS: Mutex<Vec<(&'static str, (Range<Key>, &[(FormId, Key, Key)]))>> =
        Mutex::new(Vec::new());
    let mut lists = LISTS.lock();

    if let Some((_, (keys, forms))) = lists.iter().find(|(l, _)| *l == lang) {
        (keys.clone(), forms)
    } else {
        let mut forms = Vec::new();
        let capture_names = query.capture_names();
        let keys = Key::new_many(capture_names.len() * 2);

        let mut iter_keys = keys.clone();
        for name in capture_names {
            let start = iter_keys.next().unwrap();
            let end = iter_keys.next().unwrap();
            forms.push(if name.contains('.') {
                let refed = name.split('.').next().unwrap().to_string().leak();
                (form::set_weak(name, refed), start, end)
            } else {
                (form::set_weak(name, "Default"), start, end)
            });
        }

        lists.push((lang, (keys, forms.leak())));
        let (_, (keys, forms)) = lists.last().unwrap();
        (keys.clone(), forms)
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
