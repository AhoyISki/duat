//! A words [`CompletionsProvider`]
//!
//! This provider will record every word in any open [`Buffer`],
//! recording also the origin of said word. It is also responsible for
//! removing words that have been removed from all [`Buffer`]s.
//!
//! It does this via a [`Parser`], which just figures out when a word
//! is added/removed, given a [`Change`].
//!
//! The words will be automatically filtered and sorted by size, using
//! fuzzy search in order to improve matching.
//!
//! [`Buffer`]: duat_core::buffer::Buffer
use std::{
    any::Any, collections::BTreeMap, marker::PhantomData, ops::Range, sync::{Arc, LazyLock, Mutex}
};

use duat_core::{
    Ns,
    buffer::{Buffer, Change},
    context::{self, Handle},
    data::Pass,
    hook::{self, BufferOpened, BufferUpdated, FocusedUpdated},
    text::{RegexHaystack, Spacer, Strs, Text, txt},
    ui::Orientation,
};

use crate::{hooks::{CompletionFocused, CompletionSelected}, widgets::completions::{CompletionItem, Completions, ErasedList, Sealed, string_cmp}};

impl CompletionItem for WordInfo {
    fn value(&self) -> String {
        self.word.clone()
    }

    fn default_fmt(&self) -> Text {
        txt!("[completion.word]{self.word}[]{Spacer}[completion.word.source]{self.source}",)
    }
}

/// Information about an entry in the [`WordCompletions`]
#[derive(Clone, Debug)]
pub struct WordInfo {
    pub word: String,
    /// The first [`Buffer`] in which this word was found
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    pub source: Arc<str>,
    /// How many times this word appears, always greater than 0
    pub count: usize,
    upper: Arc<str>,
}

/// Word completions provider.
pub struct WordCompletions {
    min_prefix: usize,
}

impl WordCompletions {
    /// Enables word completions for the current `Widget`.
    ///
    /// Note that this is different from explicitely calling
    /// [`Completions::add_list`], since that function will add
    /// a list of word completions once, at a specific location in
    /// the [`Text`].
    ///
    /// The purpose of this function is instead to add a "subscription"
    /// to the `Completions`, which will be receiving new word lists
    /// as is deemed necessary.
    ///
    /// You can disable this via [`WordCompletions::disable`].
    ///
    /// [`Completions::add_list`]: super::Completions::add_list
    pub fn enable(pa: &mut Pass, min_prefix: usize) {
        let mut start_byte = None;

        let widget = context::current_widget(pa);
        add_list(pa, &widget, min_prefix, &mut start_byte);

        hook::add::<FocusedUpdated>(move |pa, widget| {
            add_list(pa, widget, min_prefix, &mut start_byte)
        })
        .grouped(*NS)
        .lateness(usize::MAX);
    }

    /// Disables word completions for the current `Widget`.
    pub fn disable(pa: &mut Pass) {
        Completions::remove_list(pa, *NS);
        hook::remove(*NS)
    }
}

fn add_list(pa: &mut Pass, widget: &Handle, min_prefix: usize, start_byte: &mut Option<usize>) {
    let text = widget.text(pa);

    let Some(main_byte) = text.get_main_sel().map(|s| s.cursor().byte()) else {
        return;
    };

    let range = text
        .search(r"\w*\z")
        .range(..main_byte)
        .next_back()
        .unwrap();

    if let Some(start_byte) = &start_byte
        && (range.start == *start_byte)
    {
        return;
    }

    *start_byte = Some(range.start);

    Completions::add_list(pa, WordCompletions { min_prefix }, range.start, 50, *NS);
}

impl Sealed<WordInfo> for WordCompletions {
    fn into_erased(self, start_byte: usize, _: usize) -> Box<dyn ErasedList> {
        Box::new(InnerWordCompletions {
            matches: Vec::new(),
            start_byte,
            min_prefix: self.min_prefix,
        })
    }
}

struct InnerWordCompletions {
    matches: Vec<WordInfo>,
    start_byte: usize,
    min_prefix: usize,
}

impl ErasedList for InnerWordCompletions {
    fn match_indices(&mut self, text: &Text, case_insensitive: bool) -> Option<Vec<usize>> {
        let main_byte = text.get_main_sel()?.cursor().byte();
        let suffix = &text[text.search(r"\A\w*").range(main_byte..).next().unwrap()];

        let prefix = text.get(self.start_byte..main_byte)?;

        if prefix.chars().count() < self.min_prefix {
            return None;
        }

        let (prefix, case_insensitive) =
            if case_insensitive && !prefix.chars().any(|char| char.is_uppercase()) {
                (prefix.to_string().to_uppercase(), true)
            } else {
                (prefix.to_string(), false)
            };

        let mut matches: Vec<_> = BUFFER_WORDS
            .try_lock()
            .ok()?
            .iter()
            .filter_map(|(word, entry)| {
                let cmp = if case_insensitive { &entry.upper } else { word };
                if let Some(difference) = string_cmp(&prefix, cmp) {
                    (!(difference == 0 && entry.count == 1 && cmp[prefix.len()..] == suffix))
                        .then_some(entry.clone())
                } else {
                    None
                }
            })
            .take(200)
            .collect();

        matches.sort_by_key(|entry| {
            let cmp = if case_insensitive {
                entry.upper.as_ref()
            } else {
                entry.word.as_ref()
            };
            (string_cmp(&prefix, cmp), cmp.len())
        });

        self.matches = matches;

        (!self.matches.is_empty()).then(|| (0..self.matches.len()).collect())
    }

    fn start_byte(&self) -> usize {
        self.start_byte
    }

    fn value_for_index(&self, i: usize) -> String {
        self.matches[i].word.clone()
    }

    #[track_caller]
    fn text_for_index(&mut self, i: usize) -> Text {
        self.matches[i].word.default_fmt()
    }

    fn info_for_index(&self, _: usize) -> Option<(Text, Orientation)> {
        None
    }

    fn get(&self, i: usize) -> Box<dyn Any + Send + 'static> {
        Box::new(self.matches[i].clone())
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_trigger_selected(&self) -> fn(&mut Pass, super::InnerCompletionEntry) {
        |pa, entry| _ = hook::trigger(pa, CompletionSelected((entry, PhantomData::<WordInfo>)))
    }

    fn get_trigger_focused(&self) -> fn(&mut Pass, super::InnerCompletionEntry) {
        |pa, entry| _ = hook::trigger(pa, CompletionFocused((entry, PhantomData::<WordInfo>)))
    }
}

#[doc(hidden)]
/// Begin tracking words for word autocompletions
pub(super) fn track_words() {
    let ns = Ns::new();

    hook::add::<BufferOpened>(move |pa, buffer| {
        let buf = buffer.read(pa);

        let source: Arc<str> = buf.name().into();
        let buf = buf.text().to_string();

        std::thread::spawn(move || {
            let mut words = BUFFER_WORDS.lock().unwrap();
            for range in buf.search(r"\w{3,}") {
                let word = buf[range].to_string();
                let upper = word.to_uppercase().into();
                let info = words
                    .entry(word.into())
                    .or_insert_with_key(|word| WordInfo {
                        word: word.to_string(),
                        source: source.clone(),
                        count: 0,
                        upper,
                    });

                info.count += 1;
            }
        });
    });

    hook::add::<BufferUpdated>(move |pa, buffer| update_counts(pa, buffer, ns));
}

fn update_counts(pa: &mut Pass, buffer: &Handle<Buffer>, ns: Ns) {
    fn to_str<'a>(str: &'a str) -> impl Fn(Range<usize>) -> (Range<usize>, &'a str) {
        |range| (range.clone(), &str[range])
    }

    let source: Arc<str> = buffer.read(pa).name().into();
    let (text, moment) = {
        let buf = buffer.read(pa);
        (buf.text(), buf.moment_for(ns))
    };

    if moment.is_empty() {
        return;
    }

    let surrounded = |match_r: Range<usize>, word: &str, change_str: &str, change: &Change| {
        let prefix = if match_r.start == 0
            && let Some(text_range) = text.search(r"\w+\z").range(..change.start()).next_back()
        {
            &text[text_range]
        } else {
            Strs::empty()
        };

        if match_r.end == change_str.len()
            && let Some(text_range) = text.search(r"\A\w+").range(change.added_end()..).next()
        {
            format!("{prefix}{word}{}", &text[text_range])
        } else {
            format!("{prefix}{word}")
        }
    };

    let mut buffer_words = BUFFER_WORDS.lock().unwrap();
    let mut process_word = |word: &str, is_taken: bool| {
        if word.chars().count() < 3 {
            return;
        }
        match (buffer_words.get_mut(word), is_taken) {
            (Some(info), false) => info.count += 1,
            (None, false) => {
                let upper = word.to_uppercase().into();
                buffer_words.insert(
                    word.into(),
                    WordInfo {
                        word: word.to_string(),
                        source: source.clone(),
                        count: 1,
                        upper,
                    },
                );
            }
            (Some(info), true) if info.count > 1 => info.count -= 1,
            (Some(_), true) => {
                buffer_words.remove(word);
            }
            (None, true) => {}
        }
    };

    for change in moment.iter() {
        let added_str = change.added_str();
        let added_words: Vec<_> = added_str.search(r"\w+").map(to_str(added_str)).collect();
        let taken_str = change.taken_str();
        let taken_words: Vec<_> = taken_str.search(r"\w+").map(to_str(taken_str)).collect();

        for (is_taken, mut words, change_str) in [
            (false, added_words, added_str),
            (true, taken_words, taken_str),
        ] {
            let suffix_range = change_str.len()..change_str.len();

            let first = (words.len() > 1).then(|| words.remove(0));

            match (words.pop(), first) {
                (None, None) => {
                    let prefix = surrounded(0..0, "", change_str, &change);
                    let suffix = (!change_str.is_empty())
                        .then(|| surrounded(suffix_range, "", change_str, &change));

                    for word in [Some(prefix), suffix].into_iter().flatten() {
                        process_word(&word, is_taken);
                    }
                }
                (Some((last_range, last_word)), None) => {
                    let prefix =
                        (last_range.start != 0).then(|| surrounded(0..0, "", change_str, &change));
                    let suffix = (last_range.end != change_str.len())
                        .then(|| surrounded(suffix_range, "", change_str, &change));

                    let last_word = surrounded(last_range, last_word, change_str, &change);

                    for word in [prefix, Some(last_word), suffix].into_iter().flatten() {
                        process_word(&word, is_taken);
                    }
                }
                (Some((last_range, last_word)), Some((first_range, first_word))) => {
                    let prefix =
                        (first_range.start != 0).then(|| surrounded(0..0, "", change_str, &change));
                    let first_word = surrounded(first_range, first_word, change_str, &change);

                    let suffix = (last_range.end != change_str.len())
                        .then(|| surrounded(suffix_range, "", change_str, &change));
                    let last_word = surrounded(last_range, last_word, change_str, &change);

                    let on_sides = [
                        prefix.as_ref(),
                        Some(&first_word),
                        Some(&last_word),
                        suffix.as_ref(),
                    ];

                    for word in on_sides
                        .into_iter()
                        .flatten()
                        .map(|word| word.as_str())
                        .chain(words.into_iter().map(|(_, word)| word))
                    {
                        process_word(word, is_taken);
                    }
                }
                (None, Some(_)) => unreachable!(),
            }
        }
    }
}

static BUFFER_WORDS: Mutex<BTreeMap<Arc<str>, WordInfo>> = Mutex::new(BTreeMap::new());
static NS: LazyLock<Ns> = Ns::new_lazy();
