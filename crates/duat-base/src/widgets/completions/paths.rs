//! A file path [`CompletionsProvider`]
//!
//! Paths have a higher priority for completion then [words],
//! but they only show up if the word contains a path separator
//! character. In practice, this means that path completions
//! only ever show up if you want them to.
use std::{
    fs::ReadDir,
    path::{Path, PathBuf},
};

use duat_core::{
    text::{Point, Spacer, Text, txt},
    ui::Orientation,
    utils::expand_path,
};

use crate::widgets::completions::{CompletionKind, ErasedList, Sealed};

impl CompletionKind for PathBuf {
    fn value(&self) -> String {
        self.to_string_lossy().to_string()
    }

    fn default_fmt(&self) -> Text {
        txt!("[completion.path]{self}[]{Spacer}")
    }
}

/// Completions for [`Path`]s.
///
/// `for_parameters` is used when writing completions on the
/// [`PromptLine`], it adds support for quoted paths.
///
/// [`PromptLine`]: crate::widgets::PromptLine
pub struct PathCompletions;

impl PathCompletions {
    /// Get the start of the `Path` being completed.
    ///
    /// Usually won't be the start of the current word, since
    /// it has to start in a valid path initializer.
    ///
    /// However, if `for_parameters` is set to `true`, this becomes
    /// more permissive, and the path will start matching without
    /// a leading forward slash, for example.
    pub fn get_start(&self, text: &Text, for_parameters: bool) -> Option<usize> {
        #[cfg(not(target_os = "windows"))]
        fn get_start(text: &Text, cursor: Point, for_parameters: bool) -> Option<usize> {
            use duat_core::text::RegexHaystack;

            if for_parameters {
                text.search([" '([^']|\\')*", "[^ \n]*"])
                    .range(..cursor)
                    .next_back()
                    .map(|(pat_id, range)| range.start + (pat_id == 0) as usize)
            } else {
                text.search("[^ /\n\t]*/[^\n]*\\z")
                    .range(..cursor)
                    .next_back()
                    .map(|range| range.start)
            }
        }

        #[cfg(target_os = "windows")]
        fn get_start(&self, text: &Text, cursor: Point, for_parameters: bool) -> Option<usize> {
            use duat_core::text::RegexHaystack;

            if self.for_parameters {
                text.search(["[^ \n]*", " '([^']|\\')*"])
                    .range(..cursor)
                    .next_back()
                    .map(|(pat_id, range)| range.start + 2 * (pat_id == 1) as usize)
            } else {
                text.search("[^ /\\\n\t]*(/|\\\\)[^\n]*\\z")
                    .range(..cursor)
                    .next_back()
                    .map(|range| range.start)
            }
        }

        let main_cursor = text.get_main_sel()?.cursor();
        get_start(text, main_cursor, for_parameters)
    }
}

impl Sealed<String> for PathCompletions {
    fn into_erased(self, start_byte: usize) -> Box<dyn ErasedList> {
        Box::new(InnerPathCompletions { start_byte, list: Vec::new() })
    }
}

struct InnerPathCompletions {
    start_byte: usize,
    list: Vec<(String, PathBuf)>,
}

impl ErasedList for InnerPathCompletions {
    fn match_indices(&mut self, text: &Text, case_insensitive: bool) -> Option<Vec<usize>> {
        let main_byte = text.get_main_sel()?.cursor().byte();

        let prefix = text.get(self.start_byte..main_byte)?.to_string();
        let prefix = match prefix.strip_prefix("'") {
            Some(prefix) => prefix,
            None => &prefix,
        };

        let (cur_dir, prefix, entries) = get_entries(&prefix)?;

        let (prefix, case_insensitive) =
            if case_insensitive && !prefix.chars().any(|char| char.is_uppercase()) {
                (prefix.to_uppercase(), true)
            } else {
                (prefix.to_string(), false)
            };

        let mut entries =
            Vec::from_iter(entries.filter_map(|entry| entry.ok()).filter_map(|entry| {
                let pathbuf = entry.path();

                let mut path = if let Some(cur_dir) = &cur_dir {
                    pathbuf
                        .strip_prefix(cur_dir)
                        .unwrap()
                        .to_string_lossy()
                        .to_string()
                } else {
                    pathbuf.to_string_lossy().to_string()
                };

                if entry.path().is_dir() {
                    path.push(separator());
                }

                if path.chars().any(|char| char.is_whitespace()) {
                    path.insert(0, '\'');
                }

                if case_insensitive {
                    let upper = path.to_uppercase();
                    super::string_cmp(&prefix, &upper)
                        .map(|_| (path.to_string(), PathBuf::from(path)))
                } else {
                    super::string_cmp(&prefix, &path)
                        .map(|_| (path.to_string(), PathBuf::from(path)))
                }
            }));

        entries.sort();
        entries.sort_by_key(|(path, _)| {
            let similarity = if case_insensitive {
                let upper = path.to_uppercase();
                super::string_cmp(&prefix, &upper).unwrap()
            } else {
                super::string_cmp(&prefix, path).unwrap()
            };

            (!path.ends_with(possible_separators()), similarity)
        });

        self.list = entries;

        (!self.list.is_empty()).then(|| (0..self.list.len()).collect())
    }

    fn start_byte(&self) -> usize {
        self.start_byte
    }

    fn value_for_index(&self, i: usize) -> String {
        self.list[i].0.clone()
    }

    fn text_for_index(&mut self, i: usize) -> Text {
        self.list[i].1.default_fmt()
    }

    fn info_for_index(&self, _: usize) -> Option<(Text, Orientation)> {
        None
    }

    fn get(&self, i: usize) -> Box<dyn std::any::Any + Send + 'static> {
        Box::new(self.list[i].1.clone())
    }
}

fn get_entries(prefix: &str) -> Option<(Option<PathBuf>, String, ReadDir)> {
    let expanded = expand_path(prefix).ok()?.to_string();
    let path = Path::new(&expanded);

    if prefix.ends_with(possible_separators()) && path.is_dir() {
        let read_dir = path.read_dir().ok()?;
        Some((None, expanded, read_dir))
    } else if let Some(parent) = path.parent()
        && parent != ""
    {
        let read_dir = parent.read_dir().ok()?;
        Some((None, expanded, read_dir))
    } else {
        let current_dir = std::env::current_dir().ok()?;
        let read_dir = current_dir.read_dir().ok()?;
        Some((Some(current_dir), expanded, read_dir))
    }
}

#[cfg(not(target_os = "windows"))]
fn possible_separators() -> char {
    '/'
}

#[cfg(target_os = "windows")]
fn possible_separators() -> &'static [char] {
    &['/', '\\']
}

#[cfg(not(target_os = "windows"))]
fn separator() -> char {
    '/'
}

#[cfg(target_os = "windows")]
fn separator() -> char {
    '\\'
}
