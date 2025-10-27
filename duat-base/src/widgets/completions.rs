use std::sync::{LazyLock, Mutex, Once};

use duat_core::{
    buffer::{Buffer, BufferTracker, Parser},
    context::{self, Handle},
    data::Pass,
    hook::{self, FocusChanged},
    text::{Spacer, SpawnTag, Tagger, Text, txt},
    ui::{Orientation, SpawnSpecs, Widget},
};

#[doc(hidden)]
pub const BUFFER_WORDS: CompletionListId = CompletionListId(0);

static TAGGER: LazyLock<Tagger> = Tagger::new_static();
static COMPLETION_LISTS: LazyLock<Mutex<Vec<Vec<String>>>> =
    LazyLock::new(|| Mutex::new(vec![Vec::new()]));

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CompletionListId(usize);

impl CompletionListId {
    /// Returns a new `CompletionListId`
    fn new() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static COUNT: AtomicUsize = AtomicUsize::new(1);
        CompletionListId(COUNT.fetch_add(1, Ordering::Relaxed))
    }
}

/// A list of completions, used to quickly fill in matches
pub struct Completions {
    id: CompletionListId,
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
            let lists = COMPLETION_LISTS.lock().unwrap();
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

    /// Clears all entries of a [`CompletionListId`]
    pub fn clear_entries(id: CompletionListId) {
        let mut lists = COMPLETION_LISTS.lock().unwrap();
        lists[id.0].clear();
    }

    /// Adds entries to a [`CompletionListId`]
    pub fn add_entries<S: AsRef<str>>(id: CompletionListId, entries: impl Iterator<Item = S>) {
        let mut lists = COMPLETION_LISTS.lock().unwrap();
        let list = &mut lists[id.0];

        for entry in entries {
            if let Err(i) = list.binary_search_by_key(&entry.as_ref(), |entry| entry.as_str()) {
                list.insert(i, entry.as_ref().to_string());
            }
        }
    }

    /// Removes entries from a [`CompletionListId`]
    pub fn remove_entries<'a>(id: CompletionListId, entries: impl Iterator<Item = &'a str>) {
        let mut lists = COMPLETION_LISTS.lock().unwrap();
        let list = &mut lists[id.0];

        for entry in entries {
            if let Ok(i) = list.binary_search_by_key(&entry, |entry| entry.as_str()) {
                list.remove(i);
            }
        }
    }
}

impl Widget for Completions {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        let lists = COMPLETION_LISTS.lock().unwrap();

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
                .search_fwd(r"\w+", ..)
                .unwrap()
                .map(|range| buffer.text().strs(range).unwrap().to_string()),
        );

        buffer.add_parser(|tracker| WordsCompletionParser { tracker })
    }
}

impl Parser for WordsCompletionParser {}
