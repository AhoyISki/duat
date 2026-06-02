use std::any::Any;

use crate::widgets::completions::{CompletionKind, ErasedList, Sealed, string_cmp};
use duat_core::text::{RegexHaystack, Spacer, Text, txt};

impl CompletionKind for String {
    #[doc(hidden)]
    fn value(&self) -> &str {
        &self
    }

    #[doc(hidden)]
    fn default_fmt(&self) -> Text {
        txt!("[completion.entry]{self}[]{Spacer}")
    }
}

impl CompletionKind for &'static str {
    #[doc(hidden)]
    fn value(&self) -> &str {
        self
    }

    #[doc(hidden)]
    fn default_fmt(&self) -> Text {
        txt!("[completion.entry]{self}[]{Spacer}")
    }
}

impl<I: IntoIterator<Item = C>, C: CompletionKind> Sealed<C> for I {
    fn into_erased(self, start_byte: usize) -> Box<dyn ErasedList> {
        Box::new(InnerList::new(self.into_iter().collect(), start_byte))
    }
}

#[allow(clippy::type_complexity)]
struct InnerList<C: CompletionKind> {
    list: Vec<C>,
    fmt: Box<dyn FnMut(&C) -> Text + Send>,
    start_byte: usize,
}

impl<C: CompletionKind> InnerList<C> {
    fn new(list: Vec<C>, start_byte: usize) -> Self {
        Self {
            list,
            fmt: Box::new(C::default_fmt),
            start_byte,
        }
    }
}

impl<C: CompletionKind> ErasedList for InnerList<C> {
    /// Get the indices of the matches.
    fn match_indices(&mut self, text: &Text, case_insensitive: bool) -> Option<Vec<usize>> {
        let main_byte = text.get_main_sel()?.cursor().byte();

        if main_byte < self.start_byte {
            return None;
        } else if main_byte == self.start_byte {
            return Some((0..self.list.len()).collect());
        }

        let prefix = &text[self.start_byte..main_byte];
        let (prefix, case_insensitive) =
            if case_insensitive && !prefix.chars().any(|char| char.is_uppercase()) {
                (prefix.to_string().to_uppercase(), true)
            } else {
                (prefix.to_string(), false)
            };

        let mut list = Vec::from_iter(self.list.iter().enumerate().filter_map(|(i, entry)| {
            if case_insensitive {
                let word = entry.value().to_uppercase();
                string_cmp(&prefix, &word).and(Some(i))
            } else {
                string_cmp(&prefix, entry.value()).and(Some(i))
            }
        }));

        list.sort_by_key(|i| {
            if case_insensitive {
                let word = self.list[*i].value().to_uppercase();
                string_cmp(&prefix, &word).unwrap()
            } else {
                string_cmp(&prefix, self.list[*i].value()).unwrap()
            }
        });

        (!list.is_empty()).then_some(list)
    }

    fn text_for_index(&self, i: usize) -> Text {
        self.fmt(&self.list[i])
    }

    fn value_for_index(&self, i: usize) -> String {
        self.list[i].value().to_string()
    }

    fn start_byte(&self) -> usize {
        self.start_byte
    }

    fn info_for_index(&self, i: usize) -> Option<Text> {
        self.list[i].default_info()
    }

    fn get(&self, i: usize) -> Box<dyn Any + Send + 'static> {
        Box::new(self.list[i].clone())
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
}

impl<S: AsRef<str> + Send + 'static> Sealed<S> for ExhaustiveCompletionsList<S> {
    fn into_erased(self, start_byte: usize) -> Box<dyn super::ErasedList> {
        Box::new(InnerExhaustiveList {
            list: self.list.into_iter().map(String::from).collect(),
            start_byte,
        })
    }
}

struct InnerExhaustiveList {
    list: Vec<String>,
    start_byte: usize,
}

impl ErasedList for InnerExhaustiveList {
    fn match_indices(&mut self, text: &Text, case_insensitive: bool) -> Option<Vec<usize>> {
        let main_byte = text.main_sel().cursor().byte();

        let yet_to_be_typed = Vec::from_iter(
            self.list
                .iter()
                .filter(|word| !text[..main_byte].contains_pat(word.as_ref()).unwrap()),
        );

        if yet_to_be_typed.len() < self.list.len() {
            return None;
        }

        let prefix = text.get(self.start_byte..main_byte)?;

        let mut entries = Vec::from_iter(yet_to_be_typed.into_iter().filter_map(|entry| {
            string_cmp(prefix, entry.as_ref()).map(|_| entry.as_ref().to_string())
        }));

        entries.sort_by(|lhs, rhs| {
            string_cmp(prefix, lhs)
                .unwrap()
                .cmp(&string_cmp(prefix, rhs).unwrap())
        });

        Some(entries)
    }

    fn start_byte(&self) -> usize {
        self.start_byte
    }

    fn value_for_index(&self, i: usize) -> String {
        self.list[i].as_str().to_string()
    }

    fn text_for_index(&self, i: usize) -> Text {
        self.list[i].default_fmt()
    }

    fn info_for_index(&self, i: usize) -> Option<Text> {
        todo!()
    }

    fn get(&self, i: usize) -> Box<dyn Any + Send + 'static> {
        todo!()
    }
}
