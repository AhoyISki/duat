use std::{
    any::Any,
    ops::Range,
    sync::{LazyLock, Once},
};

use duat_core::{
    context::{self, Handle},
    data::Pass,
    hook::{self, FocusChanged},
    opts::PrintOpts,
    text::{Point, SpawnTag, Tagger, Text, txt},
    ui::{Orientation, SpawnSpecs, Widget},
};

mod paths;
mod words;

static TAGGER: LazyLock<Tagger> = Tagger::new_static();

pub struct CompletionsBuilder {
    providers: Box<
        dyn FnOnce(
            &Text,
            usize,
            PrintOpts,
        ) -> (Vec<Box<dyn ErasedInnerProvider>>, Option<(Point, Text)>),
    >,
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

        let (providers, entries) = (self.providers)(handle.text(pa), 20, handle.opts(pa));

        let Some((spawn_point, text)) = entries else {
            return;
        };

        let specs = SpawnSpecs {
            orientation: Orientation::VerLeftBelow,
            height: Some(text.len().line().min(20) as f32),
            width: Some(50.0),
            ..
        };

        let completions = Completions { providers, text };

        let text = handle.text_mut(pa);
        text.insert_tag(*TAGGER, spawn_point, SpawnTag::new(completions, specs));
    }

    /// Adds a new [`CompletionsProvider`] to be prioritized over
    /// earlier ones
    pub fn add_provider(&mut self, provider: impl CompletionsProvider) {
        let prev = std::mem::replace(
            &mut self.providers,
            Box::new(|_, _, _| panic!("Not supposed to be called")),
        );

        self.providers = Box::new(move |text, height, opts| {
            let (inner, entries) = InnerProvider::new(provider, text, height, opts);
            let (mut providers, reserve_entries) = prev(text, height, opts);
            providers.insert(0, Box::new(inner));

            (providers, entries.or(reserve_entries))
        });
    }
}

/// A list of completions, used to quickly fill in matches
pub struct Completions {
    providers: Vec<Box<dyn ErasedInnerProvider>>,
    text: Text,
}

impl Completions {
    /// Returns a new `CompletionsBuilder` with the given
    /// [`CompletionsProvider`]
    ///
    /// You can add more `CompletionsProvider`s by calling
    /// [`CompletionsBuilder::add_provider`].
    pub fn builder(provider: impl CompletionsProvider) -> CompletionsBuilder {
        CompletionsBuilder {
            providers: Box::new(move |text, height, opts| {
                let (inner, entries) = InnerProvider::new(provider, text, height, opts);

                (vec![Box::new(inner)], entries)
            }),
        }
    }

    /// Spawn the `Completions` list
    pub fn open_default(pa: &mut Pass) {
        todo!();
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
            .try_read_and_write(handle.master().unwrap().widget(), &handle.widget())
            .unwrap();
        let opts = comp.get_print_opts();

        if let Some(text) = comp
            .providers
            .iter_mut()
            .find_map(|inner| inner.text(master.text(), by, height, opts))
        {
            comp.text = text;
        }
    }
}

impl Widget for Completions {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        let height = handle.area().height(pa) as usize;
        let (master, comp) = pa
            .try_read_and_write(handle.master().unwrap().widget(), &handle.widget())
            .unwrap();
        let opts = comp.get_print_opts();

        if let Some(text) = comp
            .providers
            .iter_mut()
            .find_map(|inner| inner.text(master.text(), 0, height, opts))
        {
            comp.text = text;
        }
    }

    fn needs_update(&self, _: &Pass) -> bool {
        self.providers.iter().any(|inner| inner.has_changed())
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
    /// The point `p` will be the position where the main cursor's
    /// [caret] lies.
    ///
    /// If the `Iterator` within is empty, then the next
    /// `CompletionsProvider` will be selected to provide the required
    /// completions. If all of them return empty `Iterator`s, then no
    /// completions will be shown.
    ///
    /// [caret]: duat_core::mode::Selection::caret
    fn get_completions_at(&mut self, text: &Text, p: Point) -> CompletionsList<Self>;

    /// Regex for which characters should be part of a word
    ///
    /// This should almost always be equal to
    /// `opts.word_chars_regex()`, but for some specific providers,
    /// like those that give file paths, this wouldn't be the case.
    fn word_chars(&self, opts: PrintOpts) -> &str;

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
    /// The list of entries to be received by [`get_completions_at`]
    ///
    /// [`get_completions_at`]: CompletionsProvider::get_completions_at
    pub entries: Vec<(String, P::Info)>,
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
    fmt: Box<dyn FnMut(&str, &P::Info) -> Text + Send>,
}

impl<P: CompletionsProvider> InnerProvider<P> {
    fn new(
        mut provider: P,
        text: &Text,
        height: usize,
        opts: PrintOpts,
    ) -> (Self, Option<(Point, Text)>) {
        let (target, range) = target_word(text, provider.word_chars(opts));

        let CompletionsList { entries, kind } = provider.get_completions_at(text, range.end);

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
        (inner, Some(range.start).zip(text))
    }
}

enum FilteredEntries<P: CompletionsProvider> {
    CachedFinished(Vec<(String, P::Info)>),
    CachedUnfinished(Vec<(String, P::Info)>),
    UncachedUnfinished,
}

trait ErasedInnerProvider: Any + Send {
    fn text(&mut self, text: &Text, scroll: i32, height: usize, opts: PrintOpts) -> Option<Text>;

    fn has_changed(&self) -> bool;
}

impl<P: CompletionsProvider> ErasedInnerProvider for InnerProvider<P> {
    fn text(&mut self, text: &Text, scroll: i32, height: usize, opts: PrintOpts) -> Option<Text> {
        use FilteredEntries::*;
        let (target, range) = target_word(text, self.provider.word_chars(opts));

        if let CachedUnfinished(_) | UncachedUnfinished = &self.filtered_entries
            && target != self.target
        {
            self.entries = self.provider.get_completions_at(text, range.end).entries;
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

        Some(builder.build())
    }

    fn has_changed(&self) -> bool {
        self.provider.has_changed()
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

fn target_word(text: &Text, word_chars: &str) -> (String, Range<Point>) {
    let caret = text.selections().get_main().unwrap().caret();
    let range = text
        .search_rev(word_chars, ..caret)
        .unwrap()
        .next()
        .unwrap_or(caret..caret);

    (text.strs(range.clone()).unwrap().to_string(), range)
}
