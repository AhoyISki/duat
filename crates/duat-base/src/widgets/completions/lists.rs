use std::{any::Any, marker::PhantomData};

use duat_core::{
    cmd::CmdDoc,
    data::Pass,
    hook,
    text::{Spacer, Text, txt},
    ui::Orientation,
};

use crate::{
    hooks::{CompletionFocused, CompletionSelected},
    widgets::completions::{CompletionItem, ErasedList, Sealed, string_cmp},
};

impl CompletionItem for String {
    #[doc(hidden)]
    fn value(&self) -> String {
        self.clone()
    }

    #[doc(hidden)]
    fn default_fmt(&self) -> Text {
        txt!("[completion.entry]{self}[]{Spacer}")
    }
}

impl CompletionItem for &'static str {
    #[doc(hidden)]
    fn value(&self) -> String {
        self.to_string()
    }

    #[doc(hidden)]
    fn default_fmt(&self) -> Text {
        txt!("[completion.entry]{self}[]{Spacer}")
    }
}

impl CompletionItem for CmdDoc {
    #[doc(hidden)]
    fn value(&self) -> String {
        self.caller.to_string()
    }

    #[doc(hidden)]
    fn default_fmt(&self) -> Text {
        txt!("[cmd.Completions]{self.caller}{Spacer}")
    }

    #[doc(hidden)]
    fn default_info(&self) -> Option<(Text, Orientation)> {
        let mut builder = Text::builder();

        let short = self.short.as_ref()?;

        if !self.params.is_empty() {
            builder.push(txt!("{}\n\nArguments:", short));

            for param in self.params.iter() {
                let Some(short) = param.short.as_ref() else {
                    continue;
                };

                builder.push(txt!("\n\t- {param.arg_name}: {short}"));
            }
        } else {
            builder.push(Text::clone(short));
        }

        if let Some(long) = self.long.as_ref() {
            builder.push(txt!("\n\n{long}"));
        }

        Some((builder.build(), Orientation::HorTopRight))
    }
}

impl<I: IntoIterator<Item = C>, C: CompletionItem> Sealed<C> for I {
    fn into_erased(self, start_byte: usize, min_prefix: usize) -> Box<dyn ErasedList> {
        Box::new(InnerList {
            list: self.into_iter().collect(),
            fmt: Box::new(C::default_fmt),
            start_byte,
            min_prefix,
        })
    }
}

pub struct InnerList<C: CompletionItem> {
    pub list: Vec<C>,
    fmt: Box<dyn FnMut(&C) -> Text + Send>,
    start_byte: usize,
    min_prefix: usize,
}

impl<C: CompletionItem> ErasedList for InnerList<C> {
    /// Get the indices of the matches.
    fn match_indices(&mut self, text: &Text, case_insensitive: bool) -> Option<Vec<usize>> {
        let main_byte = text.get_main_sel()?.cursor().byte();

        if main_byte < self.start_byte {
            return None;
        } else if main_byte == self.start_byte && self.min_prefix == 0 {
            return Some((0..self.list.len()).collect());
        }

        let prefix = &text[self.start_byte..main_byte];

        if prefix.chars().all(|char| char.is_whitespace())
            || prefix.chars().count() < self.min_prefix
        {
            return None;
        }

        let (prefix, case_insensitive) =
            if case_insensitive && !prefix.chars().any(|char| char.is_uppercase()) {
                (prefix.to_string().to_uppercase(), true)
            } else {
                (prefix.to_string(), false)
            };

        let mut list = Vec::from_iter(self.list.iter().enumerate().filter_map(|(i, item)| {
            if case_insensitive {
                string_cmp(&prefix, &item.value().to_uppercase()).and(Some(i))
            } else {
                string_cmp(&prefix, &item.value()).and(Some(i))
            }
        }));

        list.sort_by_key(|i| {
            if case_insensitive {
                string_cmp(&prefix, &self.list[*i].value().to_uppercase()).unwrap()
            } else {
                string_cmp(&prefix, &self.list[*i].value()).unwrap()
            }
        });

        (!list.is_empty()).then_some(list)
    }

    fn text_for_index(&mut self, i: usize) -> Text {
        (self.fmt)(&self.list[i])
    }

    fn value_for_index(&self, i: usize) -> String {
        self.list[i].value()
    }

    fn start_byte(&self) -> usize {
        self.start_byte
    }

    fn info_for_index(&self, i: usize) -> Option<(Text, Orientation)> {
        self.list[i].default_info()
    }

    fn get(&self, i: usize) -> Box<dyn Any + Send + 'static> {
        Box::new(self.list[i].clone())
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_trigger_selected(&self) -> fn(&mut Pass, super::InnerCompletionEntry) {
        |pa, entry| _ = hook::trigger(pa, CompletionSelected((entry, PhantomData::<C>)))
    }

    fn get_trigger_focused(&self) -> fn(&mut Pass, super::InnerCompletionEntry) {
        |pa, entry| _ = hook::trigger(pa, CompletionFocused((entry, PhantomData::<C>)))
    }
}

/// A list of words that can be completed with no replacement
///
/// This list will show completions for all flags which haven't
/// been previously typed on the call. For example, if the list
/// contains `--recursive` and `--repeat`, if the user has typed
/// `:command --recursive --re`, only `--repeat` will show up.
pub struct ExhaustiveCompletionsList<C> {
    /// The list of possible entries.
    pub list: Vec<C>,
}

impl<C: CompletionItem> Sealed<C> for ExhaustiveCompletionsList<C> {
    fn into_erased(self, start_byte: usize, min_prefix: usize) -> Box<dyn super::ErasedList> {
        Box::new(InnerExhaustiveList { list: self.list, start_byte, min_prefix })
    }
}

struct InnerExhaustiveList<C> {
    list: Vec<C>,
    start_byte: usize,
    min_prefix: usize,
}

impl<C: CompletionItem> ErasedList for InnerExhaustiveList<C> {
    fn match_indices(&mut self, text: &Text, case_insensitive: bool) -> Option<Vec<usize>> {
        let main_byte = text.main_sel().cursor().byte();

        let yet_to_be_typed = Vec::from_iter(
            self.list
                .iter()
                .filter(|entry| text[..main_byte].rfind(&entry.value()).is_none()),
        );

        let prefix = text.get(self.start_byte..main_byte)?.to_string();

        if yet_to_be_typed.len() < self.list.len() || prefix.chars().count() < self.min_prefix {
            return None;
        }

        let (prefix, case_insensitive) =
            if case_insensitive && !prefix.chars().any(|c| c.is_uppercase()) {
                (prefix.to_uppercase(), true)
            } else {
                (prefix, false)
            };

        let mut entries = Vec::from_iter(yet_to_be_typed.into_iter().enumerate().filter_map(
            |(i, entry)| {
                if case_insensitive {
                    string_cmp(&prefix, &entry.value().to_uppercase()).map(|_| i)
                } else {
                    string_cmp(&prefix, &entry.value()).map(|_| i)
                }
            },
        ));

        entries.sort_by(|lhs, rhs| {
            if case_insensitive {
                string_cmp(&prefix, &&self.list[*lhs].value().to_uppercase())
                    .unwrap()
                    .cmp(&string_cmp(&prefix, &&self.list[*rhs].value().to_uppercase()).unwrap())
            } else {
                string_cmp(&prefix, &self.list[*lhs].value())
                    .unwrap()
                    .cmp(&string_cmp(&prefix, &self.list[*rhs].value()).unwrap())
            }
        });

        Some(entries)
    }

    fn start_byte(&self) -> usize {
        self.start_byte
    }

    fn value_for_index(&self, i: usize) -> String {
        self.list[i].value()
    }

    fn text_for_index(&mut self, i: usize) -> Text {
        self.list[i].default_fmt()
    }

    fn info_for_index(&self, _: usize) -> Option<(Text, Orientation)> {
        None
    }

    fn get(&self, i: usize) -> Box<dyn Any + Send + 'static> {
        Box::new(self.list[i].clone())
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_trigger_selected(&self) -> fn(&mut Pass, super::InnerCompletionEntry) {
        |pa, entry| _ = hook::trigger(pa, CompletionSelected((entry, PhantomData::<C>)))
    }

    fn get_trigger_focused(&self) -> fn(&mut Pass, super::InnerCompletionEntry) {
        |pa, entry| _ = hook::trigger(pa, CompletionFocused((entry, PhantomData::<C>)))
    }
}

