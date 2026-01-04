//! A file path [`CompletionsProvider`]
//!
//! Paths have a higher priority for completion then [words],
//! but they only show up if the word contains a path separator
//! character. In practice, this means that path completions
//! only ever show up if you want them to.
use std::path::Path;

use duat_core::text::{Point, Spacer, Text, txt};

use super::CompletionsList;
use crate::widgets::{CompletionsKind, CompletionsProvider};

pub struct PathCompletions;

impl CompletionsProvider for PathCompletions {
    type Info = ();

    fn default_fmt(entry: &str, _: &Self::Info) -> Text {
        txt!("[path.Completions]{entry}{Spacer}")
    }

    fn get_completions(
        &mut self,
        _: &Text,
        _: Point,
        prefix: &str,
        _: &str,
    ) -> CompletionsList<Self> {
        let Some(entries) = get_entries(prefix) else {
            return CompletionsList {
                entries: Vec::new(),
                kind: CompletionsKind::UnfinishedFiltered,
            };
        };

        let mut entries: Vec<_> = entries
            .filter_map(|entry| entry.ok())
            .filter_map(|entry| {
                let path = entry.path();
                let path = path.to_string_lossy();
                super::string_cmp(prefix, &path).map(|_| (path.to_string(), ()))
            })
            .collect();

        entries.sort();
        entries.sort_by_key(|(path, _)| super::string_cmp(prefix, path).unwrap());

        CompletionsList {
            entries,
            kind: CompletionsKind::UnfinishedFiltered,
        }
    }

    #[cfg(not(target_os = "windows"))]
    fn word_regex(&self) -> String {
        "[^ /\n\t]*/.*".to_string()
    }

    #[cfg(target_os = "windows")]
    fn word_regex(&self) -> String {
        "[^ /\n\t]*(/|\\).*".to_string()
    }

    fn has_changed(&self) -> bool {
        false
    }

    fn default_info_on(&self, (item, _): (&str, &Self::Info)) -> Option<Text> {
        Some(txt!("Hiiii pookie {item}!"))
    }
}

fn get_entries(prefix: &str) -> Option<std::fs::ReadDir> {
    let path = Path::new(prefix);
    if path.is_dir() && prefix.ends_with(separator()) {
        path.read_dir().ok()
    } else {
        let parent = path.parent()?;
        parent.read_dir().ok()
    }
}

#[cfg(not(target_os = "windows"))]
fn separator() -> char {
    '/'
}

#[cfg(target_os = "windows")]
fn separator() -> &[char] {
    &['/', '\\']
}
