//! A file path [`CompletionsProvider`]

use std::{ffi::OsStr, path::Path};

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
        let Some((entries, file_name)) = get_entries(prefix) else {
            duat_core::context::debug!("failed to get entries");
            return CompletionsList {
                entries: Vec::new(),
                kind: CompletionsKind::UnfinishedFiltered,
            };
        };

        let file = file_name.to_string_lossy();

        let mut entries: Vec<_> = entries
            .filter_map(|entry| entry.ok())
            .filter_map(|entry| {
                let path = entry.path();
                let path = path.to_string_lossy();
                super::string_cmp(&file, &path).map(move |_| (path.to_string(), ()))
            })
            .collect();

        entries.sort_by_key(|(path, _)| super::string_cmp(&file, path));

        CompletionsList {
            entries,
            kind: CompletionsKind::UnfinishedFiltered,
        }
    }

    fn word_regex(&self) -> String {
        "[^ /\n\t]*/.*".to_string()
    }

    fn has_changed(&self) -> bool {
        false
    }
}

fn get_entries(prefix: &str) -> Option<(std::fs::ReadDir, &OsStr)> {
    let path = Path::new(prefix);
    duat_core::context::debug!("Path is {path:?}");
    if path.is_dir() {
        path.read_dir().ok().zip(Some(OsStr::new("")))
    } else {
        let parent = path.parent()?;
        parent.read_dir().ok().zip(path.file_name())
    }
}
