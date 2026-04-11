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
    collections::BTreeMap,
    ops::Range,
    sync::{Arc, Mutex},
};

use duat_core::{
    Ns,
    buffer::Change,
    context::Handle,
    data::Pass,
    hook::{self, BufferOpened, BufferUpdated},
    text::{Point, RegexHaystack, Spacer, Strs, Text, txt},
};

use crate::widgets::completions::{CompletionsProvider, string_cmp};

static BUFFER_WORDS: Mutex<BTreeMap<Arc<str>, WordInfo>> = Mutex::new(BTreeMap::new());

/// Word completions provider.
pub struct WordCompletions;

impl CompletionsProvider for WordCompletions {
    type Info = WordInfo;

    fn default_fmt(entry: &str, info: &Self::Info) -> Text {
        txt!(
            "[word.Completions]{entry}[]{Spacer}[buffer.source.Completions]{}",
            &info.source
        )
    }

    fn matches(&mut self, text: &Text, cursor: Point, prefix: &str) -> Vec<(Arc<str>, Self::Info)> {
        let suffix = &text[text.search(r"\A\w*").range(cursor..).next().unwrap()];

        let mut matches: Vec<_> = BUFFER_WORDS
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

        matches.sort_by_key(|(entry, _)| (string_cmp(prefix, entry), entry.len()));

        matches
    }

    fn get_start(&self, text: &Text, cursor: Point) -> Option<usize> {
        text.search(r"\w*\z")
            .range(..cursor)
            .next_back()
            .map(|r| r.start)
    }
}

/// Information about an entry in the [`WordCompletions`]
#[derive(Clone, Debug)]
pub struct WordInfo {
    /// The first [`Buffer`] in which this word was found
    pub source: Arc<str>,
    /// How many times this word appears, always greater than 0
    pub count: usize,
}

#[doc(hidden)]
/// Begin tracking words for word autocompletions
pub(super) fn track_words() {
    let ns = Ns::new();

    hook::add::<BufferOpened>(move |pa, buffer| {
        let mut words = BUFFER_WORDS.lock().unwrap();
        let buf = buffer.read(pa);

        let source: Arc<str> = buf.name().into();
        for range in buf.text().search(r"\w{3,}") {
            let word = buf.text()[range].to_string().into();
            let info = words
                .entry(word)
                .or_insert_with(|| WordInfo { source: source.clone(), count: 0 });

            info.count += 1;
        }
    });

    hook::add::<BufferUpdated>(move |pa, buffer| update_counts(pa, buffer, ns));
}

fn update_counts(pa: &mut Pass, buffer: &Handle, ns: Ns) {
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
                buffer_words.insert(word.into(), WordInfo { source: source.clone(), count: 1 });
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
