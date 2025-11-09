//! Duat's completion widget
//!
//! This widget is heavily inspired by Kakoune's completions in terms
//! of functionality. By default, on duatmode, completions will show
//! up automatically whenever the user is in `insert` mode, and given
//! the tab settings of that mode, the user should be able to just
//! press `<Tab>` to complete everything, making for a very smooth
//! experience for those who don't really use tabs.
//!
//! This widget is very extensible, as you can add your own
//! [`CompletionsProvider`], which are selected by priority. This lets
//! you, for example, have the default completions be provided by an
//! LSP, with fallbacks for path and words completions.
use std::{
    any::Any,
    ops::Range,
    sync::{LazyLock, Once},
};

use duat_core::{
    context::{self, Handle},
    data::Pass,
    form::{self, Form},
    hook::{self, FocusChanged},
    mode::{MouseEvent, MouseEventKind},
    text::{Point, SpawnTag, Tagger, Text, txt},
    ui::{DynSpawnSpecs, Orientation, Widget},
};

pub use self::words::{WordCompletions, WordsCompletionParser};
use crate::widgets::completions::paths::PathCompletions;

mod paths;
mod words;

static TAGGER: LazyLock<Tagger> = Tagger::new_static();

/// A builder for [`Completions`], a [`Widget`] to show word
/// completions
///
/// The `Completions` widget is supposed to give the ability to
/// automatically fill in words, usually by pressing the `Tab`
/// character in order to scroll through a list of options.
///
/// The [`Completions`] will show words that match the word behind the
/// main [`Selection`]'s [caret], and they will automatically follow
/// the [`Selection`] if it moves to other words.
///
/// Initially, even if there is no word before the [caret],
/// completions will be shown, unless you set [`show_without_prefix`]
/// to `false`. However, as the list moves around, completions will
/// only show up if there is a word behind the [caret], as to not be
/// bothersome.
///
/// [`Selection`]: duat_core::mode::Selection
/// [caret]: duat_core::mode::Selection::caret
/// [`show_without_prefix`]: Self::show_without_prefix
pub struct CompletionsBuilder {
    providers: ProvidersFn,
    /// Show the [`Widget`] even if there is no word behind the cursor
    /// This is set to `true` by default when first opening the
    /// `Widget`, but is disabled if the cursor moves around, which
    /// creates a smooth typing experience.
    pub show_without_prefix: bool,
}

impl CompletionsBuilder {
    /// Opens the [`Completions`] [`Widget`]
    ///
    /// This `Widget` works in an autonomous way, that is, it will
    /// follow the main [`Selection`] around, always positioned at the
    /// beginning of the current word.
    ///
    /// If you wish to close the `Widget`, you can call
    /// [`Completions::close`].
    ///
    /// [`Selection`]: duat_core::mode::Selection
    pub fn open(self, pa: &mut Pass) {
        static ONCE: Once = Once::new();
        ONCE.call_once(|| {
            form::set_weak("default.Completions", Form::on_dark_grey());
            form::set_weak("selected.Completions", Form::black().on_grey());
            hook::add::<FocusChanged>(|pa, (prev, _)| {
                prev.text_mut(pa).remove_tags(*TAGGER, ..);
                Ok(())
            });
        });

        let handle = context::current_widget(pa).clone();
        handle.text_mut(pa).remove_tags(*TAGGER, ..);

        let Some(main) = handle.selections(pa).get_main() else {
            context::warn!("Tried spawning [a]Completions[] on a Widget with no [a]Selection[]s");
            return;
        };

        let (providers, start_byte, entries) = (self.providers)(handle.text(pa), 20);

        let completions = Completions {
            master: handle.clone(),
            providers,
            text: entries.unwrap_or_default(),
            max_height: 20,
            start_byte,
            show_without_prefix: self.show_without_prefix,
            last_caret: main.caret(),
        };

        let text = handle.text_mut(pa);
        text.insert_tag(*TAGGER, start_byte, SpawnTag::new(completions, SPAWN_SPECS));
    }

    /// Adds a new [`CompletionsProvider`] to be prioritized over
    /// earlier ones
    pub fn add_provider(&mut self, provider: impl CompletionsProvider) {
        let prev = std::mem::replace(
            &mut self.providers,
            Box::new(|_, _| panic!("Not supposed to be called")),
        );

        self.providers = Box::new(move |text, height| {
            let (inner, start_byte, entries) = InnerProvider::new(provider, text, height);
            let (mut providers, reserve_start_byte, reserve_entries) = prev(text, height);
            providers.insert(0, Box::new(inner));

            let start_byte = entries
                .as_ref()
                .and(Some(start_byte))
                .unwrap_or(reserve_start_byte);

            (providers, start_byte, entries.or(reserve_entries))
        });
    }
}

/// A list of completions, used to quickly fill in matches
pub struct Completions {
    master: Handle<dyn Widget>,
    providers: Vec<Box<dyn ErasedInnerProvider>>,
    text: Text,
    max_height: usize,
    start_byte: usize,
    show_without_prefix: bool,
    last_caret: Point,
}

impl Completions {
    /// Returns a new `CompletionsBuilder` with the given
    /// [`CompletionsProvider`]
    ///
    /// You can add more `CompletionsProvider`s by calling
    /// [`CompletionsBuilder::add_provider`].
    pub fn builder(provider: impl CompletionsProvider) -> CompletionsBuilder {
        let mut builder = CompletionsBuilder {
            providers: Box::new(move |text, height| {
                let (inner, start_byte, entries) = InnerProvider::new(provider, text, height);

                (vec![Box::new(inner)], start_byte, entries)
            }),
            show_without_prefix: true,
        };

        builder.add_provider(PathCompletions);

        builder
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
    pub fn scroll(pa: &mut Pass, scroll: i32) {
        if scroll == 0 {
            context::warn!("Scrolling [a]Completions[] by 0");
            return;
        }

        let Some(handle) = context::windows()
            .handles(pa)
            .find_map(Handle::try_downcast::<Completions>)
        else {
            context::warn!("No Completions open");
            return;
        };

        Completions::update_text_and_position(pa, &handle, scroll);
        handle.write(pa).show_without_prefix = true;
    }

    /// Wether there is an open `Completions` [`Widget`]
    pub fn is_open(pa: &Pass) -> bool {
        context::current_window(pa)
            .handles(pa)
            .any(|handle| handle.widget().is::<Completions>())
    }

    fn update_text_and_position(pa: &mut Pass, handle: &Handle<Self>, scroll: i32) {
        let master_handle = handle.master().unwrap();
        let (master, comp) = pa
            .try_read_and_write(master_handle.widget(), handle.widget())
            .unwrap();

        let mut lists: Vec<_> = comp
            .providers
            .iter_mut()
            .map(|inner| {
                inner.text_and_replacement(
                    master.text(),
                    scroll,
                    comp.max_height,
                    comp.show_without_prefix,
                )
            })
            .collect();

        lists.sort_by_key(|(start, _)| *start);

        if let Some((start_byte, (text, replacement))) = lists
            .into_iter()
            .find_map(|(start, list)| Some(start).zip(list))
        {
            comp.text = text;

            if let Some(replacement) = replacement {
                master_handle.edit_main(pa, |mut c| {
                    c.move_to(start_byte..c.caret().byte());
                    c.replace(&replacement);
                    c.unset_anchor();
                    if !replacement.is_empty() {
                        c.move_hor(1);
                    }
                });
            }

            let comp = handle.write(pa);
            // In this case, move the Completions to a new location
            if start_byte != comp.start_byte {
                let new_comp = Self {
                    master: handle.master().unwrap().clone(),
                    providers: std::mem::take(&mut comp.providers),
                    text: std::mem::take(&mut comp.text),
                    max_height: comp.max_height,
                    start_byte,
                    show_without_prefix: false,
                    last_caret: comp.last_caret,
                };

                let text = master_handle.text_mut(pa);
                text.remove_tags(*TAGGER, ..);
                text.insert_tag(*TAGGER, start_byte, SpawnTag::new(new_comp, SPAWN_SPECS));
            }
        } else {
            comp.text = Text::default();
        }

        let (comp, area) = handle.write_with_area(pa);
        let height = (comp.text.len().line() - 1) as f32;
        area.set_height(height).unwrap();
    }
}

impl Widget for Completions {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        Self::update_text_and_position(pa, handle, 0);
        let master_handle = handle.master().unwrap();
        handle.write(pa).last_caret = master_handle.selections(pa).get_main().unwrap().caret()
    }

    fn needs_update(&self, pa: &Pass) -> bool {
        let text = self.master.has_changed(pa).then_some(self.master.text(pa));
        let main_moved = text
            .as_ref()
            .is_some_and(|text| text.selections().get_main().unwrap().caret() != self.last_caret);

        main_moved || self.providers.iter().any(|inner| inner.has_changed(text))
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn on_mouse_event(pa: &mut Pass, _: &Handle<Self>, event: MouseEvent) {
        match event.kind {
            MouseEventKind::ScrollDown => Self::scroll(pa, 1),
            MouseEventKind::ScrollUp => Self::scroll(pa, -1),
            _ => {}
        }
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
    /// This returns a [`CompletionsList`], which contains a list of
    /// all words that should be listed as well as information
    /// regarding the ["completeness"] of the list.
    ///
    /// The `caret` is the position where the main cursor's [caret]
    /// lies, And the `prefix` and `suffix` are .
    ///
    /// If the `Iterator` within is empty, then the next
    /// `CompletionsProvider` will be selected to provide the required
    /// completions. If all of them return empty `Iterator`s, then no
    /// completions will be shown.
    ///
    /// [caret]: duat_core::mode::Selection::caret
    /// [`Self::word_regex`]: CompletionsProvider::word_regex
    /// ["completeness"]: CompletionsKind
    fn get_completions(
        &mut self,
        text: &Text,
        caret: Point,
        prefix: &str,
        suffix: &str,
    ) -> CompletionsList<Self>;

    /// Regex for which characters should be part of a word
    ///
    /// It should be something like `[\w]*` for words or `[^\0]*` for
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
    /// [word]: CompletionsProvider::word_regex
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
    /// [word]: CompletionsProvider::word_regex
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
    /// [word]: CompletionsProvider::word_regex
    UnfinishedUnfiltered,
}

trait ErasedInnerProvider: Any + Send {
    fn text_and_replacement(
        &mut self,
        text: &Text,
        scroll: i32,
        height: usize,
        show_without_prefix: bool,
    ) -> (usize, Option<(Text, Option<String>)>);

    fn has_changed(&self, text: Option<&Text>) -> bool;
}

#[allow(clippy::type_complexity)]
struct InnerProvider<P: CompletionsProvider> {
    provider: P,
    regexes: [String; 2],
    fmt: Box<dyn FnMut(&str, &P::Info) -> Text + Send>,

    orig_prefix: String,
    current: Option<(String, usize)>,

    filtered_entries: FilteredEntries<P>,
    entries: Vec<(String, P::Info)>,
}

impl<P: CompletionsProvider> InnerProvider<P> {
    fn new(mut provider: P, text: &Text, height: usize) -> (Self, usize, Option<Text>) {
        let prefix_regex = format!(r"{}\z", provider.word_regex());
        let suffix_regex = format!(r"\A{}", provider.word_regex());
        let (range, [prefix, suffix]) = preffix_and_suffix(text, [&prefix_regex, &suffix_regex]);

        let CompletionsList { entries, kind } =
            provider.get_completions(text, text.point_at_byte(range.end), &prefix, &suffix);

        let mut inner = Self {
            provider,
            regexes: [prefix_regex, suffix_regex],
            orig_prefix: prefix.clone(),
            current: None,
            filtered_entries: match kind {
                CompletionsKind::Finished => FilteredEntries::UnfilteredFinished({
                    entries
                        .iter()
                        .filter(|(entry, _)| string_cmp(&prefix, entry).is_some())
                        .map(|(entry, info)| (entry.clone(), info.clone()))
                        .collect()
                }),
                CompletionsKind::UnfinishedFiltered => FilteredEntries::FilteredUnfinished,
                CompletionsKind::UnfinishedUnfiltered => FilteredEntries::UnfilteredUnfinished({
                    entries
                        .iter()
                        .filter(|(entry, _)| string_cmp(&prefix, entry).is_some())
                        .map(|(entry, info)| (entry.clone(), info.clone()))
                        .collect()
                }),
            },
            entries,
            fmt: Box::new(P::default_fmt),
        };

        let (start, text) = inner.text_and_replacement(text, 0, height, true);
        (inner, start, text.unzip().0)
    }
}

impl<P: CompletionsProvider> ErasedInnerProvider for InnerProvider<P> {
    fn text_and_replacement(
        &mut self,
        text: &Text,
        scroll: i32,
        height: usize,
        show_without_prefix: bool,
    ) -> (usize, Option<(Text, Option<String>)>) {
        use FilteredEntries::*;
        let (range, [prefix, suffix]) =
            preffix_and_suffix(text, self.regexes.each_ref().map(|s| s.as_str()));

        // This should only be true if edits other than the one applied by
        // Completions take place.
        let target_changed = self.current.as_ref().is_some_and(|(c, _)| *c != prefix)
            || (self.current.is_none() && self.orig_prefix != prefix);

        if let UnfilteredUnfinished(_) | FilteredUnfinished = &self.filtered_entries
            && target_changed
        {
            self.entries = self
                .provider
                .get_completions(text, text.point_at_byte(range.end), &prefix, &suffix)
                .entries;
        }

        let entries = match (&mut self.filtered_entries, target_changed) {
            (UnfilteredFinished(entries) | UnfilteredUnfinished(entries), false) => entries,
            (UnfilteredFinished(entries) | UnfilteredUnfinished(entries), true) => {
                *entries = self
                    .entries
                    .iter()
                    .filter(|(entry, _)| string_cmp(&prefix, entry).is_some())
                    .map(|(entry, info)| (entry.clone(), info.clone()))
                    .collect();

                entries
            }
            (FilteredUnfinished, _) => &self.entries,
        };

        // If the word was edited, we need to reset the completions.
        if target_changed || entries.is_empty() {
            self.current = None;
            self.orig_prefix = prefix;
        }

        if height == 0 || entries.is_empty() || (range.is_empty() && !show_without_prefix) {
            self.current = None;
            return (range.start, None);
        }

        if scroll != 0 {
            // No try blocks on stable Rust 🤮.
            self.current = (|| -> Option<(String, usize)> {
                if let Some((prev, dist)) = &self.current {
                    let dist = dist.saturating_add_signed(scroll as isize).min(height - 1);
                    let prev_i = entries.iter().position(|(w, _)| w == prev)?;
                    let (word, _) = entries.get(prev_i.checked_add_signed(scroll as isize)?)?;

                    Some((word.clone(), dist))
                } else if scroll > 0 {
                    let scroll = scroll.unsigned_abs() as usize - 1;
                    let dist = (scroll).min(height - 1);
                    let (word, _) = entries.get(scroll)?;

                    Some((word.clone(), dist))
                } else {
                    let scroll = scroll.unsigned_abs() as usize;
                    let dist = height.saturating_sub(scroll);
                    let (word, _) = entries.get(entries.len().checked_sub(scroll)?)?;

                    Some((word.clone(), dist))
                }
            })();
        }

        let mut builder = Text::builder();

        if let Some((word, dist)) = &mut self.current
            && let Some(word_i) = entries.iter().position(|(w, _)| w == word)
        {
            *dist = (*dist).min(height - 1);

            let top_i = word_i.saturating_sub(*dist);
            for (i, (entry, info)) in entries.iter().enumerate().skip(top_i).take(height) {
                if i == word_i {
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

        let replacement = if scroll != 0 {
            self.current
                .clone()
                .map(|(w, _)| w)
                .or_else(|| Some(self.orig_prefix.clone()))
        } else {
            None
        };

        (range.start, Some((builder.build(), replacement)))
    }

    fn has_changed(&self, text: Option<&Text>) -> bool {
        let word_has_changed = text.is_some_and(|text| {
            let (_, [prefix, _]) =
                preffix_and_suffix(text, self.regexes.each_ref().map(|s| s.as_str()));
            prefix != self.orig_prefix
        });

        word_has_changed || self.provider.has_changed()
    }
}

enum FilteredEntries<P: CompletionsProvider> {
    UnfilteredFinished(Vec<(String, P::Info)>),
    UnfilteredUnfinished(Vec<(String, P::Info)>),
    FilteredUnfinished,
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

fn preffix_and_suffix(
    text: &Text,
    [prefix_regex, suffix_regex]: [&str; 2],
) -> (Range<usize>, [String; 2]) {
    let caret = text.selections().get_main().unwrap().caret();
    let prefix_range = text
        .search_rev(prefix_regex, ..caret)
        .unwrap()
        .next()
        .unwrap_or(caret.byte()..caret.byte());
    let suffix_range = text
        .search_fwd(suffix_regex, caret..)
        .unwrap()
        .next()
        .unwrap_or(caret.byte()..caret.byte());

    (
        prefix_range.clone(),
        [prefix_range, suffix_range].map(|range| text.strs(range).unwrap().to_string()),
    )
}

const SPAWN_SPECS: DynSpawnSpecs = DynSpawnSpecs {
    orientation: Orientation::VerLeftBelow,
    height: Some(20.0),
    width: Some(50.0),
    hidden: true,
};

type ProvidersFn =
    Box<dyn FnOnce(&Text, usize) -> (Vec<Box<dyn ErasedInnerProvider>>, usize, Option<Text>)>;
