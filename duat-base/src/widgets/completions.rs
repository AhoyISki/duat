use core::arch;
use std::{
    any::Any,
    sync::{LazyLock, Mutex, Once},
};

use duat_core::{
    buffer::{Buffer, BufferTracker, Parser},
    context::{self, Handle},
    data::Pass,
    hook::{self, FocusChanged},
    opts::PrintOpts,
    text::{Point, Spacer, SpawnTag, Tagger, Text, txt},
    ui::{Orientation, SpawnSpecs, Widget},
};

static TAGGER: LazyLock<Tagger> = Tagger::new_static();
static PROVIDERS: LazyLock<Mutex<Vec<Box<dyn Any + Send>>>> =
    LazyLock::new(|| Mutex::new(Vec::new()));

/// A list of completions, used to quickly fill in matches
pub struct Completions {
    text: Text,
    list: Vec<usize>,
    entry_on_top: usize,
    selected_entry: usize,
}

impl Completions {
    /// Spawn the `Completions` list
    pub fn open(pa: &mut Pass) {
        static ONCE: Once = Once::new();
        ONCE.call_once(|| {
            hook::add::<FocusChanged>(|pa, (prev, _)| {
                prev.text_mut(pa).remove_tags(*TAGGER, ..);
                Ok(())
            });
        });

        let handle = context::current_widget(pa).clone();
        handle.text_mut(pa).remove_tags(*TAGGER, ..);

        let Some(main) = handle.text(pa).selections().get_main().cloned() else {
            context::warn!("No Selection to center completions on");
            return;
        };

        let text = handle.text_mut(pa);

        let completions = Self {
            id: BUFFER_WORDS,
            text: Text::new(),
            list,
            selected_entry: 0,
            entry_on_top: 0,
        };

        let specs = SpawnSpecs {
            orientation: Orientation::VerLeftBelow,
            height: Some(20.0),
            width: Some(50.0),
            ..
        };

        text.insert_tag(*TAGGER, range.start, SpawnTag::new(completions, specs));
    }

    /// Closes the `Completions` list
    pub fn close(pa: &mut Pass) {
        let handle = context::current_widget(pa).clone();
        handle.text_mut(pa).remove_tags(*TAGGER, ..);
    }

    /// Goes to the next entry on the list.
    pub fn next_entry(pa: &mut Pass) {
        let Some(handle) = context::windows()
            .handles(pa)
            .find_map(Handle::try_downcast::<Completions>)
        else {
            context::warn!("No Completions open to go to next entry");
            return;
        };

        let height = handle.area().height(pa) as usize;
        let comp = handle.write(pa);
        comp.selected_entry = (comp.selected_entry + 1) % comp.list.len();
        if height > 0 {
            comp.entry_on_top = comp.entry_on_top.clamp(
                comp.selected_entry.saturating_sub(height - 1),
                comp.selected_entry,
            )
        }
    }

    /// Goes to the next entry on the list.
    pub fn prev_entry(pa: &mut Pass) {
        let Some(handle) = context::windows()
            .handles(pa)
            .find_map(Handle::try_downcast::<Completions>)
        else {
            context::warn!("No Completions open to go to next entry");
            return;
        };

        let height = handle.area().height(pa) as usize;
        let comp = handle.write(pa);
        comp.selected_entry = if comp.selected_entry == 0 {
            comp.list.len() - 1
        } else {
            comp.selected_entry - 1
        };
        if height > 0 {
            comp.entry_on_top = comp.entry_on_top.clamp(
                comp.selected_entry.saturating_sub(height - 1),
                comp.selected_entry,
            )
        }
    }

    pub fn add_provider<P: CompletionsProvider>(provider: P) {
        let mut providers = PROVIDERS.lock().unwrap();
        if !providers.iter().any(|p| p.is::<P>()) {
            providers.push(Box::new(provider))
        }
    }
}

impl Widget for Completions {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        let lists = PROVIDERS.lock().unwrap();

        let (comp, area) = handle.write_with_area(pa);
        area.set_height(comp.list.len() as f32).unwrap();

        let mut builder = Text::builder();
        for (i, index) in comp.list.iter().enumerate().skip(comp.entry_on_top) {
            let line = &lists[comp.id.0][*index];

            if i == comp.selected_entry {
                builder.push(txt!("[selected.Completions]{line}{Spacer}\n"));
            } else {
                builder.push(line);
                builder.push('\n');
            }
        }

        comp.text = builder.build();
    }

    fn needs_update(&self, _: &Pass) -> bool {
        false
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }
}

/// A provider for word completions
pub trait CompletionsProvider: Send + 'static {
    /// Additional information about a given entry in the completion
    /// list
    ///
    /// This information is supposed to be displayed alongside the
    /// entry itself, usually on the right side.
    type Info: Clone;

    /// The default formatting for entries from this provider
    ///
    /// Each [`Text`] must only be one line long (Nothing bad happens
    /// if they are multiple lines long, but don't expect the
    /// [`Completions`] to show things correctly).
    fn default_fmt(entry: &str, info: &Self::Info) -> Text;

    /// Get all completions at a given [`Point`] in the [`Text`]
    ///
    /// In conjunction with [`requires_filtering`] and
    /// [`is_incomplete`], you should use this function to provide as
    /// succint a list as possible, while still providing enough
    /// completeness.
    ///
    /// [`requires_filtering`]: CompletionsProvider::requires_filtering
    /// [`is_incomplete`]: CompletionsProvider::is_incomplete
    fn get_completions_at(
        &mut self,
        text: &Text,
        p: Point,
    ) -> Option<CompletionsList<impl Iterator<Item = (String, Self::Info)>>>;

    /// Regex for which characters should be part of a word
    ///
    /// This should almost always be equal to
    /// `opts.word_chars_regex()`, but for some specific providers,
    /// like those that give file paths, this wouldn't be the case.
    fn word_chars(&self, opts: PrintOpts) -> &str;
}

/// A list of entries for completion
///
/// This list is created by a [`CompletionsProvider`], and is used by
/// the [`Completions`] [`Widget`] (or other similar `Widget`s) to
/// provide tab completions to users.
pub struct CompletionsList<I> {
    /// The list of entries to be received by [`get_completions_at`]
    ///
    /// [`get_completions_at`]: CompletionsProvider::get_completions_at
    pub list: I,
    /// What kind of completion entries have been provided
    pub kind: CompletionsKind,
}

/// What kind of completions was given by [`get_completions_at`]
///
/// [`get_completions_at`]: CompletionsProvider::get_completions_at
#[derive(Clone, Copy)]
pub enum CompletionsKind {
    /// Indicates that the entries that were sent are _all_ entries
    /// that exist
    ///
    /// This means that, if the user types any new [word] characters
    /// or deletes old ones, this list will remain unaltered.
    ///
    /// In this case, the [`Completions`] widget will take that
    /// initial list and apply filtering to it in order to narrow down
    /// possible choices.
    ///
    /// [word]: CompletionsProvider::word_chars
    Finished,
    /// Indicates that the entries that were sent are not all entries,
    /// but they're already filtered
    ///
    /// This means that, if the user types any new [word] characters
    /// or deletes old ones, a new list will have to be acquired.
    ///
    /// Unlike in [`CompletionsKind::Finished`] and
    /// [`CompletionsKind::UnfinishedUnfiltered`], the [`Completions`]
    /// widget will not do any filtering of the entries sent.
    ///
    /// [word]: CompletionsProvider::word_chars
    UnfinishedFiltered,
    /// Indicates that the entries that were sent are not all entries,
    /// and they're not filtered
    ///
    /// This means that, if the user types any new [word] characters
    /// or deletes old ones, a new list will have to be acquired.
    ///
    /// In this case, the [`Completions`] widget will take this list
    /// and apply filtering to it in order to narrow down possible
    /// choices.
    ///
    /// [word]: CompletionsProvider::word_chars
    UnfinishedUnfiltered,
}

struct InnerProvider<P: CompletionsProvider> {
    provider: P,
    dist_from_top: Option<usize>,
    target: String,
    filtered_entries: FilteredEntries<P>,
    entries: Vec<(String, P::Info)>,
    fmt: Box<dyn FnMut(&str, &P::Info) -> Text>,
}

impl<P: CompletionsProvider> InnerProvider<P> {
    fn new(mut provider: P, text: &Text, height: usize, opts: PrintOpts) -> Option<(Self, Text)> {
        let (target, caret) = target_word(text, provider.word_chars(opts));

        let CompletionsList { list, kind } = provider.get_completions_at(text, caret)?;
        let entries: Vec<_> = list.collect();

        let mut inner = Self {
            provider,
            dist_from_top: None,
            target: target.clone(),
            filtered_entries: match kind {
                CompletionsKind::Finished => FilteredEntries::CachedFinished({
                    entries
                        .iter()
                        .filter(|(entry, _)| string_cmp(&target, entry).is_some())
                        .map(|(entry, info)| (entry.clone(), info.clone()))
                        .collect()
                }),
                CompletionsKind::UnfinishedFiltered => FilteredEntries::UncachedUnfinished,
                CompletionsKind::UnfinishedUnfiltered => FilteredEntries::CachedUnfinished({
                    entries
                        .iter()
                        .filter(|(entry, _)| string_cmp(&target, entry).is_some())
                        .map(|(entry, info)| (entry.clone(), info.clone()))
                        .collect()
                }),
            },
            entries,
            fmt: Box::new(P::default_fmt),
        };

        let text = inner.text(text, 0, height, opts);

        Some((inner, text))
    }

}

enum FilteredEntries<P: CompletionsProvider> {
    CachedFinished(Vec<(String, P::Info)>),
    CachedUnfinished(Vec<(String, P::Info)>),
    UncachedUnfinished,
}

trait ErasedInnerProvider: Any {
    fn text(&mut self, text: &Text, scroll: i32, height: usize, opts: PrintOpts) -> Text;
}

impl<P: CompletionsProvider> ErasedInnerProvider for InnerProvider<P> {
    fn text(&mut self, text: &Text, scroll: i32, height: usize, opts: PrintOpts) -> Text {
        use FilteredEntries::*;
        let (target, caret) = target_word(text, self.provider.word_chars(opts));

        if let CachedUnfinished(_) | UncachedUnfinished = &self.filtered_entries
            && target != self.target
        {
            self.entries = self
                .provider
                .get_completions_at(text, caret)
                .unwrap()
                .list
                .collect();
        }

        let entries = match (&mut self.filtered_entries, target == self.target) {
            (CachedFinished(entries) | CachedUnfinished(entries), true) => entries,
            (CachedFinished(entries) | CachedUnfinished(entries), false) => {
                *entries = self
                    .entries
                    .iter()
                    .filter(|(entry, _)| string_cmp(&target, entry).is_some())
                    .map(|(entry, info)| (entry.clone(), info.clone()))
                    .collect();

                entries
            }
            (UncachedUnfinished, _) => &self.entries,
        };

        self.target = target;

        if height == 0 || entries.is_empty() {
            self.dist_from_top = None;
            return Text::default();
        }

        if scroll != 0 {
            self.dist_from_top = Some(if let Some(dist) = self.dist_from_top {
                dist.saturating_add_signed(dist as isize).min(height - 1)
            } else if scroll > 0 {
                (scroll.unsigned_abs() as usize).min(height - 1)
            } else {
                height.saturating_sub(scroll.unsigned_abs() as usize)
            })
        }

        let mut builder = Text::builder();

        if let Some(dist) = &mut self.dist_from_top
            && let Some(target_i) = entries.iter().position(|(entry, _)| *entry == self.target)
        {
            *dist = (*dist).min(height - 1);

            let top_i = target_i.saturating_sub(*dist);
            for (i, (entry, info)) in entries.iter().enumerate().skip(top_i).take(height) {
                if i == target_i {
                    builder.push(txt!("[selected.Completions]{}\n", (self.fmt)(entry, info)));
                } else {
                    builder.push(txt!("{}\n", (self.fmt)(entry, info)));
                }
            }
        } else {
            for (entry, info) in entries.iter().take(height) {
                builder.push(txt!("{}\n", (self.fmt)(entry, info)));
            }
        }

        builder.build()
    }
}

/// A simple [`String`] comparison function, which prioritizes matched
/// clusters as well as earlier matches
///
/// The priorization is done in the following order:
///
/// - Matching clusters.
/// - Earlier matches in `cmp`.
/// - Size of `cmp`
///
/// If any `char` in the `target` fails to match (in the correct
/// order), then this function returns [`None`].
///
/// If this function returns [`Some(0)`], then it is an exact match.
fn string_cmp(target: &str, entry: &str) -> Option<usize> {
    let mut diff = 0;
    let mut eq_i = 0;
    let mut cmp_chars = entry.chars().enumerate();

    for char in target.chars() {
        let (i, _) = cmp_chars.find(|&(_, other)| other == char)?;
        diff += i - eq_i;
        eq_i = i + 1;
    }

    Some(diff)
}

fn target_word(text: &Text, word_chars: &str) -> (String, Point) {
    let caret = text.selections().get_main().unwrap().caret();
    let range = text
        .search_rev(word_chars, ..caret)
        .unwrap()
        .next()
        .unwrap_or(caret..caret);

    (text.strs(range.clone()).unwrap().to_string(), caret)
}

/// A [`Parser`] to add words to [`Completions`]
pub struct WordsCompletionParser {
    tracker: BufferTracker,
}

impl WordsCompletionParser {
    /// Adds the `WordParser` [`Parser`] to this [`Buffer`]
    pub fn add_to_buffer(buffer: &mut Buffer) -> Result<(), Text> {
        Completions::add_entries(
            BUFFER_WORDS,
            buffer
                .text()
                .search_fwd(r"\w{3,}", ..10000.min(buffer.text().len().byte()))
                .unwrap()
                .map(|range| buffer.text().strs(range).unwrap().to_string()),
        );

        buffer.add_parser(|tracker| WordsCompletionParser { tracker })
    }
}

impl Parser for WordsCompletionParser {}
