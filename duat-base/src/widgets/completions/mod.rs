use std::{
    any::Any,
    ops::Range,
    sync::{LazyLock, Once},
};

use duat_core::{
    context::{self, Handle},
    data::Pass,
    hook::{self, FocusChanged},
    text::{Point, SpawnTag, Tagger, Text, txt},
    ui::{Orientation, SpawnSpecs, Widget},
};

pub use self::words::{WordCompletions, WordsCompletionParser};

mod paths;
mod words;

static TAGGER: LazyLock<Tagger> = Tagger::new_static();

pub struct CompletionsBuilder {
    providers: ProvidersFn,
}

impl CompletionsBuilder {
    /// Opens the [`Completions`] [`Widget`]
    pub fn open(self, pa: &mut Pass) {
        static ONCE: Once = Once::new();
        ONCE.call_once(|| {
            hook::add::<FocusChanged>(|pa, (prev, _)| {
                prev.text_mut(pa).remove_tags(*TAGGER, ..);
                Ok(())
            });
        });

        let handle = context::current_widget(pa).clone();
        handle.text_mut(pa).remove_tags(*TAGGER, ..);

        let (providers, entries) = (self.providers)(handle.text(pa), 20);

        let Some((spawn_point, text)) = entries else {
            return;
        };

        let specs = SpawnSpecs {
            orientation: Orientation::VerLeftBelow,
            height: Some(text.len().line().min(20) as f32),
            width: Some(50.0),
            ..
        };

        let completions = Completions {
            master: handle.clone(),
            providers,
            text,
            max_height: 20,
        };

        let text = handle.text_mut(pa);
        text.insert_tag(*TAGGER, spawn_point, SpawnTag::new(completions, specs));
    }

    /// Adds a new [`CompletionsProvider`] to be prioritized over
    /// earlier ones
    pub fn add_provider(&mut self, provider: impl CompletionsProvider) {
        let prev = std::mem::replace(
            &mut self.providers,
            Box::new(|_, _| panic!("Not supposed to be called")),
        );

        self.providers = Box::new(move |text, height| {
            let (inner, entries) = InnerProvider::new(provider, text, height);
            let (mut providers, reserve_entries) = prev(text, height);
            providers.insert(0, Box::new(inner));

            (providers, entries.or(reserve_entries))
        });
    }
}

/// A list of completions, used to quickly fill in matches
pub struct Completions {
    master: Handle<dyn Widget>,
    providers: Vec<Box<dyn ErasedInnerProvider>>,
    text: Text,
    max_height: usize,
}

impl Completions {
    /// Returns a new `CompletionsBuilder` with the given
    /// [`CompletionsProvider`]
    ///
    /// You can add more `CompletionsProvider`s by calling
    /// [`CompletionsBuilder::add_provider`].
    pub fn builder(provider: impl CompletionsProvider) -> CompletionsBuilder {
        CompletionsBuilder {
            providers: Box::new(move |text, height| {
                let (inner, entries) = InnerProvider::new(provider, text, height);

                (vec![Box::new(inner)], entries)
            }),
        }
    }

    /// Spawn the `Completions` list
    pub fn open_default(pa: &mut Pass) {
        Self::builder(WordCompletions).open(pa);
    }

    /// Closes the `Completions` list
    pub fn close(pa: &mut Pass) {
        let handle = context::current_widget(pa).clone();
        handle.text_mut(pa).remove_tags(*TAGGER, ..);
    }

    /// Goes to the next entry on the list.
    pub fn scroll(pa: &mut Pass, by: i32) {
        let Some(handle) = context::windows()
            .handles(pa)
            .find_map(Handle::try_downcast::<Completions>)
        else {
            context::warn!("No Completions open to go to next entry");
            return;
        };

        let height = handle.area().height(pa) as usize;
        let (master, comp) = pa
            .try_read_and_write(handle.master().unwrap().widget(), handle.widget())
            .unwrap();

        if let Some((text, _)) = comp
            .providers
            .iter_mut()
            .find_map(|inner| inner.text_and_entry(master.text(), by, height))
        {
            comp.text = text;
        }
    }
}

impl Widget for Completions {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        let (master, comp) = pa
            .try_read_and_write(handle.master().unwrap().widget(), handle.widget())
            .unwrap();

        comp.text = if let Some((text, _)) = comp
            .providers
            .iter_mut()
            .find_map(|inner| inner.text_and_entry(master.text(), 0, comp.max_height))
        {
            text
        } else {
            Text::default()
        };

        let height = (comp.text.len().line() - 1) as f32;
        handle.area().set_height(pa, height).unwrap();
    }

    fn needs_update(&self, pa: &Pass) -> bool {
        let text = self.master.has_changed(pa).then_some(self.master.text(pa));
        self.providers.iter().any(|inner| inner.has_changed(text))
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }
}

/// A provider for word completions
pub trait CompletionsProvider: Send + Sized + 'static {
    /// Additional information about a given entry in the completion
    /// list
    ///
    /// This information is supposed to be displayed alongside the
    /// entry itself, usually on the right side.
    type Info: Clone + Send;

    /// The default formatting for entries from this provider
    ///
    /// Each [`Text`] must only be one line long (Nothing bad happens
    /// if they are multiple lines long, but don't expect the
    /// [`Completions`] to show things correctly).
    fn default_fmt(entry: &str, info: &Self::Info) -> Text;

    /// Get all completions at a given [`Point`] in the [`Text`]
    ///
    /// This will return a [`CompletionsList`], which is contains not
    /// only an [`Iterator`] over the entries, but also a
    /// [`CompletionsKind`], which is useful to tell the
    /// [`Completions`] widget how to handle certain aspects of the
    /// list.
    ///
    /// The `caret` is the position where the main cursor's [caret]
    /// lies, while the `word` is the [`String`] that matched prior to
    /// said caret, given [`Self::word_regex`].
    ///
    /// If the `Iterator` within is empty, then the next
    /// `CompletionsProvider` will be selected to provide the required
    /// completions. If all of them return empty `Iterator`s, then no
    /// completions will be shown.
    ///
    /// [caret]: duat_core::mode::Selection::caret
    /// [`Self::word_regex`]: CompletionsProvider::word_regex
    fn get_completions(&mut self, text: &Text, caret: Point, word: &str) -> CompletionsList<Self>;

    /// Regex for which characters should be part of a word
    ///
    /// It should be something like `[\w]*` for words, or `[^\0]*` for
    /// file paths. You can also use minimum lengths, like `[\w]{3,}`
    /// in order to filter for too short matches.
    fn word_regex(&self) -> String;

    /// Wether the list of completion entries has been updated
    ///
    /// This is particularly useful if the `CompletionsProvider` could
    /// be slow, such as one from an LSP or things of the sort.
    fn has_changed(&self) -> bool;
}

/// A list of entries for completion
///
/// This list is created by a [`CompletionsProvider`], and is used by
/// the [`Completions`] [`Widget`] (or other similar `Widget`s) to
/// provide tab completions to users.
pub struct CompletionsList<P: CompletionsProvider> {
    /// The list of entries to be received by [`get_completions`]
    ///
    /// [`get_completions`]: CompletionsProvider::get_completions
    pub entries: Vec<(String, P::Info)>,
    /// What kind of completion entries have been provided
    pub kind: CompletionsKind,
}

/// What kind of completions was given by [`get_completions`]
///
/// [`get_completions`]: CompletionsProvider::get_completions
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
    word_regex: String,
    fmt: Box<dyn FnMut(&str, &P::Info) -> Text + Send>,

    target: String,
    dist_from_top: Option<usize>,

    filtered_entries: FilteredEntries<P>,
    entries: Vec<(String, P::Info)>,
}

impl<P: CompletionsProvider> InnerProvider<P> {
    fn new(mut provider: P, text: &Text, height: usize) -> (Self, Option<(usize, Text)>) {
        let word_regex = format!(r"{}\z", provider.word_regex());
        let (target, range) = target_word(text, &word_regex);

        let CompletionsList { entries, kind } =
            provider.get_completions(text, text.point_at_byte(range.end), &target);

        let mut inner = Self {
            provider,
            word_regex,
            target: target.clone(),
            dist_from_top: None,
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

        let (text, _) = inner.text_and_entry(text, 0, height).unzip();
        (inner, Some(range.start).zip(text))
    }
}

enum FilteredEntries<P: CompletionsProvider> {
    CachedFinished(Vec<(String, P::Info)>),
    CachedUnfinished(Vec<(String, P::Info)>),
    UncachedUnfinished,
}

trait ErasedInnerProvider: Any + Send {
    fn text_and_entry(
        &mut self,
        text: &Text,
        scroll: i32,
        height: usize,
    ) -> Option<(Text, Option<String>)>;

    fn has_changed(&self, text: Option<&Text>) -> bool;
}

impl<P: CompletionsProvider> ErasedInnerProvider for InnerProvider<P> {
    fn text_and_entry(
        &mut self,
        text: &Text,
        scroll: i32,
        height: usize,
    ) -> Option<(Text, Option<String>)> {
        use FilteredEntries::*;
        let (target, range) = target_word(text, &self.word_regex);

        if let CachedUnfinished(_) | UncachedUnfinished = &self.filtered_entries
            && target != self.target
        {
            self.entries = self
                .provider
                .get_completions(text, text.point_at_byte(range.end), &target)
                .entries;
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
            return None;
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

            let target_i = if scroll != 0 {
                let target_i = target_i.saturating_add_signed(scroll as isize);
                self.target = entries[target_i].0.clone();
                target_i
            } else {
                target_i
            };

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

        Some((builder.build(), (scroll != 0).then(|| self.target.clone())))
    }

    fn has_changed(&self, text: Option<&Text>) -> bool {
        let word_has_changed = text.is_some_and(|text| {
            let (word, _) = target_word(text, &self.word_regex);
            word != self.target
        });

        word_has_changed || self.provider.has_changed()
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

fn target_word(text: &Text, word_chars: &str) -> (String, Range<usize>) {
    let caret = text.selections().get_main().unwrap().caret();
    let range = text
        .search_rev(word_chars, ..caret)
        .unwrap()
        .next()
        .unwrap_or(caret.byte()..caret.byte());

    (text.strs(range.clone()).unwrap().to_string(), range)
}

type ProvidersFn =
    Box<dyn FnOnce(&Text, usize) -> (Vec<Box<dyn ErasedInnerProvider>>, Option<(usize, Text)>)>;
