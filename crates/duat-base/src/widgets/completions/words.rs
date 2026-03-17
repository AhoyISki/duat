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
use std::{collections::BTreeMap, ops::Range, sync::Mutex};

use duat_core::{
    buffer::{BufferTracker, Change},
    context::Handle,
    data::Pass,
    hook::{self, BufferOpened, BufferUpdated},
    text::{Point, RegexHaystack, Spacer, Strs, Text, txt},
};

use crate::widgets::completions::{
    CompletionsKind, CompletionsList, CompletionsProvider, string_cmp,
};

static TRACKER: BufferTracker = BufferTracker::new();
static BUFFER_WORDS: Mutex<BTreeMap<String, WordInfo>> = Mutex::new(BTreeMap::new());

pub struct WordCompletions;

impl CompletionsProvider for WordCompletions {
    type Info = WordInfo;

    fn default_fmt(entry: &str, info: &Self::Info) -> Text {
        txt!(
            "[word.Completions]{entry}[]{Spacer}[buffer.source.Completions]{}",
            &info.source
        )
    }

    fn completions(
        &mut self,
        text: &Text,
        caret: Point,
        prefix: &str,
        _: bool,
    ) -> CompletionsList<Self> {
        let suffix = &text[text.search(r"\A\w*").range(caret..).next().unwrap()];

        let mut entries: Vec<_> = BUFFER_WORDS
            .lock()
            .unwrap_or_else(|err| err.into_inner())
            .iter()
            .filter(|&(word, info)| {
                if let Some(difference) = string_cmp(prefix, word) {
                    !(difference == 0 && info.count == 1 && &word[prefix.len()..] == suffix)
                } else {
                    false
                }
            })
            .map(|(entry, info)| (entry.clone(), info.clone()))
            .collect();

        entries.sort_by_key(|(entry, _)| (string_cmp(prefix, entry), entry.len()));

        CompletionsList {
            entries,
            kind: CompletionsKind::UnfinishedFiltered,
        }
    }

    fn get_start(&self, text: &Text, caret: Point) -> Option<usize> {
        text.search(r"\w*\z")
            .range(..caret)
            .next_back()
            .map(|r| r.start)
    }

    fn has_changed(&self) -> bool {
        false
    }
}

/// Information about an entry in the [`WordCompletions`]
#[derive(Clone, Debug)]
pub struct WordInfo {
    /// The first [`Buffer`] in which this word was found
    pub source: String,
    /// How many times this word appears, always greater than 0
    pub count: usize,
}

#[doc(hidden)]
/// Begin tracking words for word autocompletions
pub(super) fn track_words() {
    hook::add::<BufferOpened>(|pa, handle| {
        TRACKER.register_buffer(handle.write(pa));
        let mut words = BUFFER_WORDS.lock().unwrap();
        let buffer = handle.read(pa);
        for range in buffer.text().search(r"\w{3,}") {
            let word = buffer.text()[range].to_string();
            let info = words
                .entry(word)
                .or_insert_with(|| WordInfo { source: buffer.name(), count: 0 });

            info.count += 1;
        }
    });

    hook::add::<BufferUpdated>(update_counts);
}

fn update_counts(pa: &mut Pass, handle: &Handle) {
    fn to_str<'a>(str: &'a str) -> impl Fn(Range<usize>) -> (Range<usize>, &'a str) {
        |range| (range.clone(), &str[range])
    }

    let name = handle.read(pa).name();
    let parts = TRACKER.parts(handle.write(pa)).unwrap();

    if parts.changes.len() == 0 {
        return;
    }

    let surrounded = |match_r: Range<usize>, word: &str, change_str: &str, change: &Change| {
        let prefix = if match_r.start == 0
            && let Some(text_range) = parts
                .strs
                .search(r"\w+\z")
                .range(..change.start())
                .next_back()
        {
            &parts.strs[text_range]
        } else {
            Strs::empty()
        };

        if match_r.end == change_str.len()
            && let Some(text_range) = parts
                .strs
                .search(r"\A\w+")
                .range(change.added_end()..)
                .next()
        {
            format!("{prefix}{word}{}", &parts.strs[text_range])
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
                buffer_words.insert(word.to_string(), WordInfo {
                    source: name.clone(),
                    count: 1,
                });
            }
            (Some(info), true) if info.count > 1 => info.count -= 1,
            (Some(_), true) => {
                buffer_words.remove(word);
            }
            (None, true) => {}
        }
    };

    for change in parts.changes {
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
