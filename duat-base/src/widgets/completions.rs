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
        let (target, range) = {
            let range = text
                .search_rev(r"\w*\z", ..main.caret())
                .unwrap()
                .next()
                .unwrap_or(main.caret()..main.caret());
            let target = text.strs(range.clone()).unwrap().to_string();
            (target, range)
        };

        let list = {
            let lists = PROVIDERS.lock().unwrap();
            let mut list: Vec<usize> = lists[BUFFER_WORDS.0]
                .iter()
                .enumerate()
                .filter_map(|(i, entry)| string_cmp(&target, entry).is_some().then_some(i))
                .collect();

            list.sort_by_key(|i| string_cmp(&target, &lists[BUFFER_WORDS.0][*i]).unwrap());

            list
        };

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

struct ProviderFunctions {
}

/// A provider for word completions
pub trait CompletionsProvider: Send + 'static {
    /// Additional information about a given entry in the completion
    /// list
    ///
    /// This information is supposed to be displayed alongside the
    /// entry itself, usually on the right side.
    type Info;

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
    ) -> CompletionsList<impl Iterator<Item = (&str, Self::Info)>>;

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
    /// Wether the items from [`get_completions_at`] should be
    /// filtered
    ///
    /// In the [`Completions`] [`Widget`], this will be done by
    /// filtering out every entry that does not contain _all_ of the
    /// characters on the [current word].
    ///
    /// [`get_completions_at`]: CompletionsProvider::get_completions_at
    /// [current word]: CompletionsProvider::word_chars
    pub requires_filtering: bool,
    /// Wether the list sent by [`get_completions_at`] is complete
    ///
    /// If this is `true`, then the [`Completions`] [`Widget`] will
    /// assume that there are no more entries to be sent, so even if
    /// the user continues typing, as long as they are on the same
    /// [word], then the first list that was sent can be used for
    /// filtering.
    ///
    /// [`get_completions_at`]: CompletionsProvider::get_completions_at
    /// [word]: CompletionsProvider::word_chars
    pub is_complete: bool,
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
