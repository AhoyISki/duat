use std::{collections::BTreeMap, sync::Mutex};

use duat_core::{
    buffer::{Buffer, BufferTracker, Parser},
    text::{Point, Spacer, Text, txt},
    ui::Widget,
};

use crate::widgets::completions::{
    CompletionsKind, CompletionsList, CompletionsProvider, string_cmp,
};

static BUFFER_WORDS: Mutex<BTreeMap<String, WordInfo>> = Mutex::new(BTreeMap::new());

pub struct WordCompletions;

impl CompletionsProvider for WordCompletions {
    type Info = WordInfo;

    fn default_fmt(entry: &str, info: &Self::Info) -> Text {
        txt!(
            "[word.Completions]{entry}{Spacer}[buffer.source.Completions]{}",
            &info.source
        )
        .build()
    }

    fn get_completions(&mut self, _: &Text, _: Point, word: &str) -> CompletionsList<Self> {
        let mut entries: Vec<_> = BUFFER_WORDS
            .lock()
            .unwrap()
            .iter()
            .filter(|&(entry, _)| string_cmp(word, entry).is_some())
            .map(|(entry, info)| (entry.clone(), info.clone()))
            .collect();

        entries.sort_by_key(|(entry, _)| string_cmp(word, entry));

        CompletionsList {
            entries,
            kind: CompletionsKind::UnfinishedFiltered,
        }
    }

    fn word_regex(&self) -> String {
        r"[\w]*".to_string()
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

/// A [`Parser`] to add words to [`Completions`]
pub struct WordsCompletionParser {
    tracker: BufferTracker,
}

impl WordsCompletionParser {
    /// Adds the `WordParser` [`Parser`] to this [`Buffer`]
    pub fn add_to_buffer(buffer: &mut Buffer) -> Result<(), Text> {
        let mut words = BUFFER_WORDS.lock().unwrap();
        for word in buffer
            .text()
            .search_fwd(r"\w{3,}", ..10000.min(buffer.text().len().byte()))
            .unwrap()
            .map(|range| buffer.text().strs(range).unwrap().to_string())
        {
            let info = words
                .entry(word)
                .or_insert(WordInfo { source: buffer.name(), count: 0 });
            info.count += 1;
        }

        buffer.add_parser(|mut tracker| {
            tracker.track_changed_lines();
            WordsCompletionParser { tracker }
        })
    }
}

impl Parser for WordsCompletionParser {}
