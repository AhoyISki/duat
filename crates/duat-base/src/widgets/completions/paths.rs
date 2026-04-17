//! A file path [`CompletionsProvider`]
//!
//! Paths have a higher priority for completion then [words],
//! but they only show up if the word contains a path separator
//! character. In practice, this means that path completions
//! only ever show up if you want them to.
use std::{
    fs::ReadDir,
    path::{Path, PathBuf},
    sync::Arc,
};

use duat_core::{
    text::{Point, Spacer, Text, txt},
    utils::expand_path,
};

use crate::widgets::CompletionsProvider;

/// Completions for [`Path`]s.
///
/// `for_parameters` is used when writing completions on the
/// [`PromptLine`], it adds support for quoted paths.
///
/// [`PromptLine`]: crate::widgets::PromptLine
#[derive(Clone)]
pub struct PathCompletions {
    case_insensitive: bool,
    for_parameters: bool,
}

impl PathCompletions {
    /// Returns a new `PathCompletions`
    ///
    /// If `for_parameters` is `false`, then in order for the
    /// completions to show up, a `/` must be part of the string (or
    /// `\` on Windows). This makes this completion more flexible when
    /// working with multiple completions at once.
    pub fn new(case_insensitive: bool, for_parameters: bool) -> Self {
        Self { case_insensitive, for_parameters }
    }
}

impl CompletionsProvider for PathCompletions {
    type Info = ();

    fn default_fmt(entry: &str, _: &Self::Info) -> Text {
        txt!("[path.Completions]{entry}{Spacer}")
    }

    fn matches(&mut self, _: &Text, _: Point, prefix: &str) -> Vec<(Arc<str>, Self::Info)> {
        let prefix = match prefix.strip_prefix("'") {
            Some(prefix) => prefix,
            None => prefix,
        };

        let Some((cur_dir, prefix, entries)) = get_entries(prefix, self.for_parameters) else {
            return Vec::new();
        };

        let (prefix, case_insensitive) =
            if self.case_insensitive && prefix.chars().all(|char| !char.is_uppercase()) {
                (prefix.to_uppercase(), true)
            } else {
                (prefix.to_string(), false)
            };

        let mut entries: Vec<(Arc<str>, _)> = entries
            .filter_map(|entry| entry.ok())
            .filter_map(|entry| {
                let path = entry.path();

                let mut path = if let Some(cur_dir) = &cur_dir {
                    path.strip_prefix(cur_dir).unwrap().to_string_lossy()
                } else {
                    path.to_string_lossy()
                };

                if entry.path().is_dir() {
                    path.to_mut().push(separator());
                }

                if path.chars().any(|char| char.is_whitespace()) {
                    path.to_mut().insert(0, '\'');
                }

                if case_insensitive {
                    let upper = path.to_uppercase();
                    super::string_cmp(&prefix, &upper).map(|_| (path.to_string().into(), ()))
                } else {
                    super::string_cmp(&prefix, &path).map(|_| (path.to_string().into(), ()))
                }
            })
            .collect();

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

        entries
    }

    #[cfg(not(target_os = "windows"))]
    fn get_start(&self, text: &Text, cursor: Point) -> Option<usize> {
        use duat_core::text::RegexHaystack;

        if self.for_parameters {
            text.search([" '([^']|\\')*", "[^ \n]*"])
                .range(..cursor)
                .next_back()
                .map(|(pat_id, range)| range.start + (pat_id == 0) as usize)
        } else {
            text.search("[^ /\n\t]*/.*")
                .range(..cursor)
                .next_back()
                .map(|range| range.start)
        }
    }

    #[cfg(target_os = "windows")]
    fn get_start(&self, text: &Text, cursor: Point) -> Option<usize> {
        use duat_core::text::RegexHaystack;

        if self.for_parameters {
            text.search(["[^ \n]*", " '([^']|\\')*"])
                .range(..cursor)
                .next_back()
                .map(|(pat_id, range)| range.start + 2 * (pat_id == 1) as usize)
        } else {
            text.search("[^ /\\\n\t]*(/|\\\\).*")
                .range(..cursor)
                .next_back()
                .map(|range| range.start)
        }
    }
}

fn get_entries(prefix: &str, for_parameters: bool) -> Option<(Option<PathBuf>, String, ReadDir)> {
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
    } else if for_parameters {
        let current_dir = std::env::current_dir().ok()?;
        let read_dir = current_dir.read_dir().ok()?;
        Some((Some(current_dir), expanded, read_dir))
    } else {
        None
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
