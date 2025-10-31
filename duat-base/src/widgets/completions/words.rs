use std::{collections::BTreeMap, ops::Range, sync::Mutex};

use duat_core::{
    buffer::{Buffer, BufferTracker, Parser},
    context::Handle,
    data::Pass,
    text::{Change, Matcheable, Point, Spacer, Strs, Text, txt},
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
            .search_fwd(r"\w{3,}", ..)
            .unwrap()
            .map(|range| buffer.text().strs(range).unwrap().to_string())
        {
            let info = words
                .entry(word)
                .or_insert_with(|| WordInfo { source: buffer.name(), count: 0 });
            info.count += 1;
        }

        buffer.add_parser(|mut tracker| {
            tracker.track_changed_lines();
            WordsCompletionParser { tracker }
        })
    }
}

impl Parser for WordsCompletionParser {
    fn update(&mut self, pa: &mut Pass, buffer: &Handle, _: Vec<Range<Point>>) {
        self.tracker.update();
        let moment = self.tracker.moment();
        let buffer = buffer.read(pa);

        if moment.is_empty() {
            return;
        }

        let surrounded = |range: Range<usize>, word: &str, change: &Change| {
            let prefix = if range.start == 0
                && let Some(text_range) = buffer
                    .bytes()
                    .search_fwd(r"[\w]+\z", ..change.start())
                    .unwrap()
                    .next()
            {
                buffer.bytes().strs(text_range).unwrap()
            } else {
                Strs::empty()
            };

            if range.end == change.added_str().len()
                && let Some(text_range) = buffer
                    .bytes()
                    .search_fwd(r"\A[\w]+", change.added_end()..)
                    .unwrap()
                    .next()
            {
                format!("{prefix}{word}{}", buffer.bytes().strs(text_range).unwrap())
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
                        source: buffer.name(),
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

        for change in moment.changes() {
            let added_str = change.added_str();
            let added_words: Vec<_> = added_str.search_fwd(r"[\w]+", ..).unwrap().collect();
            let taken_str = change.taken_str();
            let taken_words: Vec<_> = taken_str.search_fwd(r"[\w]+", ..).unwrap().collect();

            for (is_taken, mut words, str) in [
                (false, added_words, added_str),
                (true, taken_words, taken_str),
            ] {
                let suffix_range = str.len()..str.len();

                match (words.pop(), words.try_remove(0)) {
                    (None, None) => {
                        let prefix = surrounded(0..0, "", &change);
                        let suffix =
                            (!str.is_empty()).then(|| surrounded(suffix_range, "", &change));

                        for word in [Some(prefix), suffix].into_iter().flatten() {
                            process_word(&word, is_taken);
                        }
                    }
                    (Some((last_range, last_word)), None) => {
                        let prefix = (last_range.start != 0).then(|| surrounded(0..0, "", &change));
                        let suffix = (last_range.end != str.len())
                            .then(|| surrounded(suffix_range, "", &change));

                        let last_word = surrounded(last_range, last_word, &change);

                        for word in [prefix, Some(last_word), suffix].into_iter().flatten() {
                            process_word(&word, is_taken);
                        }
                    }
                    (Some((last_range, last_word)), Some((first_range, first_word))) => {
                        let prefix =
                            (first_range.start != 0).then(|| surrounded(0..0, "", &change));
                        let first_word = surrounded(first_range, first_word, &change);

                        let suffix = (last_range.end != str.len())
                            .then(|| surrounded(suffix_range, "", &change));
                        let last_word = surrounded(last_range, last_word, &change);

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
}
