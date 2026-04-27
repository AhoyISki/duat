use duat_core::text::{Point, RegexHaystack, Spacer, Text, txt};

use crate::widgets::{CompletionsProvider, completions::string_cmp};

impl<S: AsRef<str> + Send + 'static> CompletionsProvider for Vec<S> {
    type Entry<'e> = &'e str;

    fn matches<'e>(&'e mut self, _: &Text, _: Point, prefix: &str) -> Vec<Self::Entry<'e>> {
        let mut entries: Vec<&str> = self
            .iter()
            .filter_map(|entry| string_cmp(prefix, entry.as_ref()).map(|_| entry.as_ref()))
            .collect();

        entries.sort_by(|lhs, rhs| {
            string_cmp(prefix, lhs)
                .unwrap()
                .cmp(&string_cmp(prefix, rhs).unwrap())
        });

        entries
    }

    fn get_start(&self, text: &Text, cursor: Point) -> Option<usize> {
        Some(text.search(r"\S*").range(..cursor).next_back()?.start)
    }

    fn default_fmt(entry: &Self::Entry<'_>) -> Text {
        txt!("[completion.entry]{entry}[]{Spacer}")
    }

    fn word<'e>(entry: &'e Self::Entry<'e>) -> &'e str {
        entry
    }
}

impl<const N: usize, S: AsRef<str> + Send + 'static> CompletionsProvider for [S; N] {
    type Entry<'e> = &'e str;

    fn matches<'e>(&'e mut self, _: &Text, _: Point, prefix: &str) -> Vec<Self::Entry<'e>> {
        let mut entries: Vec<&str> = self
            .iter()
            .filter_map(|entry| string_cmp(prefix, entry.as_ref()).map(|_| entry.as_ref()))
            .collect();

        entries.sort_by(|lhs, rhs| {
            string_cmp(prefix, lhs)
                .unwrap()
                .cmp(&string_cmp(prefix, rhs).unwrap())
        });

        entries
    }

    fn get_start(&self, text: &Text, cursor: Point) -> Option<usize> {
        Some(text.search(r"\S*").range(..cursor).next_back()?.start)
    }

    fn default_fmt(entry: &Self::Entry<'_>) -> Text {
        txt!("[completion.entry]{entry}[]{Spacer}")
    }

    fn word<'e>(entry: &'e Self::Entry<'e>) -> &'e str {
        entry
    }
}

/// A list of words that can be completed with no replacement
///
/// This list will show completions for all flags which haven't
/// been previously typed on the call. For example, if the list
/// contains `--recursive` and `--repeat`, if the user has typed
/// `:command --recursive --re`, only `--repeat` will show up.
pub struct ExhaustiveCompletionsList<S> {
    /// The list of possible entries.
    pub list: Vec<S>,
    /// Wether only one is allowed.
    pub only_one: bool,
}

impl<S: AsRef<str> + Send + 'static> CompletionsProvider for ExhaustiveCompletionsList<S> {
    type Entry<'e> = &'e str;

    fn matches<'e>(&'e mut self, text: &Text, _: Point, prefix: &str) -> Vec<Self::Entry<'e>> {
        let cursor = text.main_sel().cursor();

        let yet_to_be_typed: Vec<_> = self
            .list
            .iter()
            .filter(|word| !text[..cursor].contains_pat(word.as_ref()).unwrap())
            .collect();

        if yet_to_be_typed.len() < self.list.len() && self.only_one {
            return Vec::new();
        }

        let mut entries: Vec<&str> = yet_to_be_typed
            .into_iter()
            .filter_map(|entry| string_cmp(prefix, entry.as_ref()).map(|_| entry.as_ref()))
            .collect();

        entries.sort_by(|lhs, rhs| {
            string_cmp(prefix, lhs)
                .unwrap()
                .cmp(&string_cmp(prefix, rhs).unwrap())
        });

        entries
    }

    fn get_start(&self, text: &Text, cursor: Point) -> Option<usize> {
        Some(text.search(r"\S*").range(..cursor).next_back()?.start)
    }

    fn default_fmt(entry: &Self::Entry<'_>) -> Text {
        txt!("[completion.entry]{entry}[]{Spacer}")
    }

    fn word<'e>(entry: &'e Self::Entry<'e>) -> &'e str {
        entry
    }
}
