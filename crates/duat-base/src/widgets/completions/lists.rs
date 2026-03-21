use duat_core::text::{Point, RegexHaystack, Spacer, Text, txt};

use crate::widgets::{CompletionsProvider, completions::string_cmp};

impl<S: AsRef<str> + Send + 'static> CompletionsProvider for Vec<S> {
    type Info = ();

    fn default_fmt(entry: &str, _: &Self::Info) -> Text {
        txt!("{entry}{Spacer}")
    }

    fn matches(&mut self, _: &Text, _: Point, prefix: &str) -> Vec<(String, Self::Info)> {
        let mut entries: Vec<_> = self
            .iter()
            .filter_map(|entry| {
                string_cmp(prefix, entry.as_ref()).map(|_| (entry.as_ref().to_string(), ()))
            })
            .collect();

        entries.sort_by(|(lhs, _), (rhs, _)| {
            string_cmp(prefix, lhs)
                .unwrap()
                .cmp(&string_cmp(prefix, rhs).unwrap())
        });

        entries
    }

    fn get_start(&self, text: &Text, caret: Point) -> Option<usize> {
        Some(text.search(r"\S*").range(..caret).next_back()?.start)
    }
}

impl<const N: usize, S: AsRef<str> + Send + 'static> CompletionsProvider for [S; N] {
    type Info = ();

    fn default_fmt(entry: &str, _: &Self::Info) -> Text {
        txt!("{entry}{Spacer}")
    }

    fn matches(&mut self, _: &Text, _: Point, prefix: &str) -> Vec<(String, Self::Info)> {
        let mut entries: Vec<_> = self
            .iter()
            .filter_map(|entry| {
                string_cmp(prefix, entry.as_ref()).map(|_| (entry.as_ref().to_string(), ()))
            })
            .collect();

        entries.sort_by(|(lhs, _), (rhs, _)| {
            string_cmp(prefix, lhs)
                .unwrap()
                .cmp(&string_cmp(prefix, rhs).unwrap())
        });

        entries
    }

    fn get_start(&self, text: &Text, caret: Point) -> Option<usize> {
        Some(text.search(r"\S*").range(..caret).next_back()?.start)
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
    type Info = ();

    fn default_fmt(entry: &str, _: &Self::Info) -> Text {
        txt!("{entry}{Spacer}")
    }

    fn matches(&mut self, text: &Text, caret: Point, prefix: &str) -> Vec<(String, Self::Info)> {
        let yet_to_be_typed: Vec<_> = self
            .list
            .iter()
            .filter(|word| !text[..caret].contains_pat(word.as_ref()).unwrap())
            .collect();

        if yet_to_be_typed.len() < self.list.len() && self.only_one {
            return Vec::new();
        }

        let mut entries: Vec<_> = yet_to_be_typed
            .iter()
            .filter_map(|entry| {
                string_cmp(prefix, entry.as_ref()).map(|_| (entry.as_ref().to_string(), ()))
            })
            .collect();

        entries.sort_by(|(lhs, _), (rhs, _)| {
            string_cmp(prefix, lhs)
                .unwrap()
                .cmp(&string_cmp(prefix, rhs).unwrap())
        });

        entries
    }

    fn get_start(&self, text: &Text, caret: Point) -> Option<usize> {
        Some(text.search(r"\S*").range(..caret).next_back()?.start)
    }
}
