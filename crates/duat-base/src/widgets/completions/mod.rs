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
    any::{Any, TypeId},
    collections::HashMap,
    ops::Range,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    Ns,
    buffer::Buffer,
    cmd::{
        CfgOrScratch, ColorSchemeArg, Existing, OtherBuffer, Parameter, ReloadOptions,
        ValidFilePath,
    },
    context::{self, Handle},
    data::{Pass, RwData},
    hook::{self, BufferUpdated, KeySent, OnMouseEvent, WidgetOpened, WidgetSwitched},
    mode::{KeyCode, MouseEventKind, event},
    text::{RegexHaystack, Spawn, Text, TextIndex, TextMut, txt},
    ui::{DynSpawnSpecs, Orientation, Side, Widget},
};
use duat_term::Frame;

pub use crate::widgets::completions::{
    commands::CommandsCompletions, lists::ExhaustiveCompletionsList, paths::PathCompletions,
    words::WordCompletions,
};
use crate::{
    hooks::{CompletionFocused, CompletionSelected},
    widgets::Info,
};

mod lists;
mod paths;
mod words;

static WIDGET_NS: LazyLock<Ns> = Ns::new_lazy();
static COMPLETIONS: LazyLock<Mutex<HashMap<TypeId, (usize, ParamCompletions)>>> =
    LazyLock::new(Mutex::default);

/// Initial setup for completions
pub fn completions_setup() {
    words::track_words();
    Completions::set_for_parameter::<ValidFilePath>(75, |_, builder| PathCompletions::new(true));

    Completions::set_for_parameter::<Handle<Buffer>>(50, |pa, builder| {
        let mut list: Vec<String> = context::windows()
            .buffers(pa)
            .into_iter()
            .map(|buf| buf.read(pa).name())
            .collect();

        list.sort_unstable();

        list
    });

    Completions::set_for_parameter::<OtherBuffer>(50, |pa, builder| {
        let current = context::current_buffer(pa).read(pa).name();
        let mut list: Vec<String> = context::windows()
            .buffers(pa)
            .into_iter()
            .map(|buf| buf.read(pa).name())
            .filter(|name| *name != current)
            .collect();

        list.sort_unstable();

        list
    });

    Completions::set_for_parameter::<Existing>(25, |_, builder| ["--existing"]);

    Completions::set_for_parameter::<CfgOrScratch>(30, |_, builder| {
        ["--cfg", "--cfg-manifest", "--scratch"]
    });

    Completions::set_for_parameter::<ReloadOptions>(50, |_, builder| ExhaustiveCompletionsList {
        list: vec!["--clean", "--update"],
    });

    Completions::set_for_parameter::<ColorSchemeArg>(50, |_, builder| {
        duat_core::form::colorscheme_list()
    });

    hook::add::<WidgetOpened<Completions>>(move |pa, completions| {
        Completions::set_frame(pa, completions);

        let completions = completions.clone();
        let ns = Ns::new();

        hook::add::<KeySent>({
            let completions = completions.clone();
            move |pa, key_event| {
                let completions_master = completions.master(pa).unwrap();

                if completions.is_closed()
                    || completions_master.is_closed()
                    || completions_master.selections(pa).is_empty()
                {
                    hook::remove(ns);
                    return;
                }

                let comp = completions.read(pa);
                if let event!(KeyCode::Char(..) | KeyCode::Enter) = key_event
                    && let Some(matches) = &comp.matches
                    && let Some(selected) = &matches.selected
                {
                    let entry = CompletionEntry {
                        index: selected.idx,
                        orig_range: comp.start_byte..comp.start_byte + comp.orig_typed.len(),
                        orig_typed: comp.orig_typed.clone(),
                        replacement: selected.value.clone(),
                        entry: comp.lists[matches.list_idx].1.get(selected.idx),
                    };
                    hook::trigger(pa, CompletionSelected(entry));
                }
            }
        })
        .lateness(0);

        let update_completions = {
            let completions = completions.clone();
            move |pa: &mut Pass| {
                let completions_master = completions.master(pa).unwrap();

                if completions.is_closed()
                    || completions_master.is_closed()
                    || completions_master.selections(pa).is_empty()
                {
                    hook::remove(ns);
                    return;
                }

                Completions::scroll_and_update(pa, &completions, 0);
            }
        };

        if completions.master_buffer(pa).is_some() {
            // NECESSARY, DON'T GET RID OF THIS!!!
            // This is to update the completions only after updating the Buffer.
            hook::add::<BufferUpdated>(move |pa, _| update_completions(pa))
                .lateness(usize::MAX)
                .grouped(ns);
        } else {
            hook::add::<KeySent>(move |pa, _| update_completions(pa))
                .lateness(usize::MAX)
                .grouped(ns);
        }
    });

    hook::add::<WidgetSwitched>(move |pa, (old, _)| {
        old.text_mut(pa).remove_tags(*WIDGET_NS, ..);
        Completions::close(pa);
    });

    hook::add::<OnMouseEvent<Completions>>(|pa, event| match event.kind {
        MouseEventKind::ScrollDown => _ = Completions::scroll(pa, 1),
        MouseEventKind::ScrollUp => _ = Completions::scroll(pa, -1),
        _ => {}
    });
}

/// A completion entry.
///
/// This came from some [`CompletionsProvider`], and is used on the
/// [`CompletionSelected`] and [`CompletionFocused`] hooks.
pub struct CompletionEntry {
    /// The index on the list where this item came from.
    pub index: usize,
    /// The original byte range of the text being replaced.
    pub orig_range: Range<usize>,
    /// What was typed by the user.
    pub orig_typed: String,
    /// What the text was replaced with.
    pub replacement: String,
    entry: Box<dyn Any + Send>,
}

impl std::fmt::Debug for CompletionEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CompletionEntry")
            .field("index", &self.index)
            .field("orig_range", &self.orig_range)
            .field("orig_typed", &self.orig_typed)
            .field("replacement", &self.replacement)
            .finish()
    }
}

impl CompletionEntry {
    /// Returns `Some` if the completion entry came from the given
    /// [provider].
    ///
    /// You should use this function in order to confirm the origin of
    /// a provider before running any post completion hooks.
    ///
    /// [provider]: CompletionsProvider
    pub fn get_as<C: CompletionKind>(&self) -> Option<&C> {
        self.entry.as_ref().downcast_ref()
    }
}

pub trait CompletionEntries<C>: Sealed<C> {}
impl<S: Sealed<C>, C> CompletionEntries<C> for S {}

trait Sealed<C> {
    fn into_erased(self, start_byte: usize) -> Box<dyn ErasedList>;
}

/// A type of object that can be completed and will show up in
/// the [`Completions`] widget.
pub trait CompletionKind: Send + Clone + 'static {
    /// Function to pick a word to match on.
    ///
    /// This word is what will be placed over the current word.
    #[doc(hidden)]
    fn value(&self) -> String;

    /// The default formatting for entries from this provider
    ///
    /// Each [`Text`] must only be one line long (Nothing bad happens
    /// if they are multiple lines long, but don't expect the
    /// [`Completions`] to show things correctly).
    #[doc(hidden)]
    fn default_fmt(&self) -> Text;

    /// Additional information about an entry, which can be shown when
    /// it is selected.
    #[doc(hidden)]
    fn default_info(&self) -> Option<(Text, Orientation)> {
        None
    }
}

/// A list of completions, used to quickly fill in matches
pub struct Completions {
    lists: Vec<(usize, Box<dyn ErasedList>, Ns)>,
    text: Text,
    max_height: usize,
    cur_min_prefix: usize,
    min_prefix: usize,

    start_byte: usize,
    orig_typed: String,
    matches: Option<Matches>,

    info: Option<(Handle<Info>, Orientation)>,
}

impl Completions {
    /// Adds a list of entries to the `Completions`.
    ///
    /// This can be one of three things:
    ///
    /// - Any [`IntoIterator<Item = impl CompletionKind>`].
    /// - The [`WordCompletions`] struct.
    /// - The [`PathCompletions`] struct.
    /// - The [`ExhaustiveCompletionsList`] struct.
    ///
    /// If there was a list before that had the same [`Ns`], that list
    /// will be removed before the new one is added.
    ///
    /// This will also spawn the `Completions` if that is necessary.
    pub fn add_list(
        pa: &mut Pass,
        entries: impl CompletionEntries,
        start: impl TextIndex,
        priority: usize,
        ns: Ns,
    ) {
        let master = context::current_widget(pa);
        let text = master.text(pa);
        let start_byte = start.to_byte_index(text.len());

        let Some(main_byte) = text.get_main_sel() else {
            context::error!("Tried spawning completions on a widget with no selections");
            return;
        };

        if main_byte < start_byte {
            context::error!("Tried adding completion list that starts after cursor");
            return;
        }

        let completions = if let Some(completions) = context::handle_of::<Completions>(pa) {
            let comp = completions.write(pa);
            comp.remove_ns(ns);

            let mut erased = entries.into_erased();

            let (Ok(list_idx) | Err(list_idx)) = comp
                .lists
                .binary_search_by(|(prio, ..)| priority.cmp(prio).reverse());

            if let Some(matches) = &comp.matches
                && comp.lists[matches.list_idx].0 <= priority
                && let Some(list) = erased.match_indices(master.text(pa), true)
            {
                completions.write(pa).matches = Some(Matches { list_idx, list, selected: None });
            }

            let comp = completions.write(pa);
            comp.lists.insert(list_idx, (priority, erased, ns));

            Completions::scroll_and_update(pa, &completions, 0);
        } else {
            let comp = Completions {
                lists: vec![(priority, entries.into_erased(), ns)],
                text: Text::new(),
                max_height: 20,
                cur_min_prefix: 0,
                min_prefix: 0,
                start_byte,
                orig_typed: text[start_byte..main_byte].to_string(),
                matches: None,
                info: None,
            };

            master
                .text_mut(pa)
                .insert_tag(*WIDGET_NS, start_byte, Spawn::new(comp, SPAWN_SPECS));
        };
    }

    /// Removes all completion lists that were added with the given `Ns`.
    pub fn remove_list(pa: &mut Pass, ns: Ns) {
        let Some(completions) = context::handle_of::<Completions>(pa) else {
            return;
        };

        completions.write(pa).remove_ns(ns);

        Completions::scroll_and_update(pa, &completions, 0);
    }

    fn remove_ns(&mut self, ns: Ns) {
        let mut idx = 0;
        self.lists.retain(|(.., other)| {
            if other == ns {
                if let Some(matches) = &self.matches
                    && matches.list_idx == idx
                {
                    self.matches = None;
                };

                idx += 1;
                false
            } else {
                idx += 1;
                true
            }
        });
    }

    /// Sets the [`CompletionEntries`] for a [`Parameter`].
    pub fn set_for_parameter<P: Parameter, C: CompletionEntries>(
        priority: usize,
        entries_fn: impl FnMut(&Pass) -> C,
    ) {
        COMPLETIONS.lock().unwrap().insert(
            TypeId::of::<P>(),
            (
                priority,
                Box::new(Mutex::new(|pa, start_byte| {
                    entries_fn(pa).into_erased(start_byte)
                })),
            ),
        );
    }

    /// Open the `Completions` for a given [`Parameter`]'s [`TypeId`]
    ///
    /// This completions must've been previously added via
    /// [`Completions::set_for_parameter`].
    ///
    /// Returns [`None`] if none of the `TypeId`s had completions set
    /// for them.
    pub fn open_for(pa: &mut Pass, param_type_ids: &[TypeId]) {
        let master = context::current_widget(pa);
        let text = master.text(pa);

        let Some(main_byte) = text.get_main_sel() else {
            context::error!("Tried spawning completions on a widget with no selections");
            return;
        };

        let start_byte = text
            .search([" '([^']|\\')*", "[^ \n]*"])
            .range(..main_byte)
            .next_back()
            .map(|(pat_id, range)| range.start + (pat_id == 0) as usize)
            .unwrap();

        let completions = COMPLETIONS.lock().unwrap();

        let mut lists: Vec<_> = param_type_ids
            .iter()
            .filter_map(|ty| completions.get(ty))
            .map(|(prio, entries_fn)| (prio, entries_fn(pa), Ns::basic()))
            .collect();

        if lists.is_empty() {
            Completions::close(pa);
            return;
        }

        lists.sort_by_key(|(prio, ..)| prio);

        let comp = Completions {
            lists,
            text: Text::new(),
            max_height: 20,
            cur_min_prefix: 0,
            min_prefix: 0,
            start_byte,
            orig_typed: text[start_byte..main_byte].to_string(),
            matches: None,
            info: None,
        };

        master
            .text_mut(pa)
            .insert_tag(*WIDGET_NS, start_byte, Spawn::new(comp, SPAWN_SPECS));
    }

    /// Closes the `Completions` list
    pub fn close(pa: &mut Pass) {
        let handle = context::current_widget(pa);
        handle.text_mut(pa).remove_tags(*WIDGET_NS, ..);
    }

    /// Goes to the next entry on the list.
    pub fn scroll(pa: &mut Pass, scroll: i32) -> Option<(String, String)> {
        if scroll == 0 {
            context::warn!("Scrolling [a]Completions[] by 0");
            return None;
        }

        let handle = context::handle_of::<Completions>(pa)?;

        let main_repl = Completions::scroll_and_update(pa, &handle, scroll);
        handle.write(pa).cur_min_prefix = 0;
        main_repl
    }

    /// Wether there is an open `Completions` [`Widget`]
    pub fn is_open(pa: &Pass) -> bool {
        context::handle_of::<Completions>(pa).is_some()
    }

    #[track_caller]
    fn scroll_and_update(
        pa: &mut Pass,
        completions: &Handle<Self>,
        scroll: i32,
    ) -> Option<(String, String)> {
        let master = completions.master(pa).unwrap();
        let (text, comp, area) = pa.write_many((master.rw_text(), completions, completions.area()));

        let Some(main_byte) = text.get_main_sel() else {
            completions.close(pa);
            return None;
        };

        let matches = {
            // If the user types anything, or anything has changed in the text,
            // the matches should be recalculated.
            let still_valid_matches = text.get(comp.start_byte..main_byte).and_then(|strs| {
                comp.matches.take().filter(|matches| {
                    matches
                        .selected
                        .as_ref()
                        .is_some_and(|sel| sel.value == strs)
                        || (matches.selected.is_none() && comp.orig_typed == strs)
                });
            });

            if let Some(matches) = still_valid_matches {
                matches
            } else {
                let Some((list, list_idx)) = comp
                    .lists
                    .iter_mut()
                    .enumerate()
                    .find_map(|(i, (_, list))| list.match_indices(text, true).zip(Some(i)))
                else {
                    comp.text = Text::default();
                    area.hide();
                    return None;
                };

                comp.orig_typed = text[comp.lists[list_idx].1.start_byte()..main_byte].to_string();

                Matches { list_idx, list, selected: None }
            }
        };

        let (_, list) = &comp.lists[matches.list_idx];

        let original = if let Some(selected) = &matches.selected {
            &selected.value
        } else {
            &comp.orig_typed
        };

        let (new_idx, replacement) = {
            let new_idx = if let Some(selected) = &matches.selected {
                let len = matches.list.len() as i32;
                let idx = (selected.idx as i32 + scroll).rem_euclid(len + 1) as usize;
                (idx != len).then_some(idx)
            } else if scroll != 0 {
                let len = matches.list.len() as i32;
                let idx = scroll.rem_euclid(len + 1) as usize;
                (idx != len).then_some(idx)
            } else {
                None
            };

            let replacement = match (&matches.selected, new_idx) {
                (None, None) => None,
                (None, Some(new_idx)) => Some(list.value_for_index(new_idx)),
                (Some(selected), None) => Some(comp.orig_typed.clone()),
                (Some(selected), Some(new_idx)) => {
                    let new_value = list.value_for_index(new_idx);
                    (new_value != selected.value).then_some(new_value)
                }
            };

            if let Some(replacement) = replacement {
                let mut master_has_changed = false;

                master.edit_all(pa, |mut s| {
                    if !s.text()[..s.cursor().byte()].ends_with(original) {
                        return;
                    }

                    let start = s.cursor().byte() - original.len();

                    master_has_changed = true;

                    s.move_to(start..s.cursor().byte());

                    s.replace(&replacement);
                    s.unset_anchor();
                    if !replacement.is_empty() {
                        s.move_hor(1);
                    }
                });

                if !master_has_changed {
                    master.widget().declare_unwritten();
                    master.area().declare_unwritten();
                }
            } else {
                master.widget().declare_unwritten();
                master.area().declare_unwritten();
            }

            (new_idx, replacement)
        };

        // No try blocks on stable Rust 🤮.
        let height = comp.max_height.min(matches.list.len());
        let dist_from_top = if let Some(replacement) = &replacement {
            if let Some(selected) = &matches.selected {
                selected
                    .dist_from_top
                    .saturating_add_signed(scroll as isize)
                    .min(height - 1)
            } else if scroll > 0 {
                (scroll.unsigned_abs() as usize - 1).min(height - 1);
            } else {
                height.saturating_sub(scroll.unsigned_abs())
            }
        } else {
            0
        };

        let info = if let Some(new_idx) = new_idx
            && let Some(info_text, orientation) = list.info_for_index(new_idx)
        {
            let info = if let Some((info, ori)) = comp.info.take()
                && (!info.is_closed() && ori == orientation)
            {
                Info::set_text(pa, &info, |text| *text = info_text);
                Some(info)
            } else {
                let specs = DynSpawnSpecs { orientation, ..Default::default() };

                let info_handle = completions.spawn_widget(pa, Info::new(info_text), specs);
                completions.write(pa).info = info_handle.clone();
                info_handle
            };

            if let Some(info_handle) = info.as_ref()
                && let Some(area) = info_handle.area().write_as::<duat_term::Area>(pa)
            {
                let mut frame = Frame::default();
                frame.set_text(Side::Above, move |_| {
                    txt!(
                        "[terminal.border.Info]┤[]{}[terminal.border.Info]├",
                        list.value_for_index(new_idx)
                    )
                });
                area.set_frame(frame);
            }

            info.zip(Some(orientation))
        } else if let Some((info, _)) = comp.info.take() {
            info.close(pa);

            None
        };

        // In this case, don't update the Text, and close the Completions instead.
        // Let the Text be updated on the new Completions when it shows up.
        if list.start_byte() != comp.start_byte {
            let new_comp = Self {
                lists: std::mem::take(&mut comp.lists),
                text: Text::new(),
                max_height: comp.max_height,

                start_byte: list.start_byte(),
                cur_min_prefix: comp.min_prefix,
                min_prefix: comp.min_prefix,
                info: comp.info.take(),

                matches: Some(Matches {
                    selected: Some(Selected {
                        idx: new_idx,
                        dist_from_top,
                        value: list.value_for_index(new_idx),
                    }),
                    ..matches
                }),
            };

            let mut text = master.text_mut(pa);
            text.insert_tag(
                *WIDGET_NS,
                list.start_byte(),
                Spawn::new(new_comp, SPAWN_SPECS),
            );

            _ = completions.close(pa);
            return Some(original).zip(replacement);
        }

        let (text, sidebar) = {
            let mut entries = Text::builder();
            let mut sidebar = Text::builder();

            if let Some(new_idx) = new_idx {
                let top_i = new_idx.saturating_sub(dist_from_top);
                for idx in matches.list.iter().skip(top_i).take(height) {
                    if idx == new_idx {
                        entries.push(txt!("[selected.Completions]{}\n", list.text_for_index(idx)));
                        sidebar.push(txt!("[selected.Completions] \n"));
                    } else {
                        entries.push(txt!("{}\n", list.text_for_index(idx)));
                        sidebar.push(txt!("[default.Completions] \n"));
                    }
                }
            } else {
                for idx in matches.list.iter().take(height) {
                    entries.push(txt!("{}\n", list.text_for_index(idx)));
                    sidebar.push(txt!("[default.Completions] \n"));
                }
            }
        };

        if scroll != 0
            && let Some(index) = new_idx
        {
            let comp = completions.read(pa);
            let entry = CompletionEntry {
                index,
                orig_range: todo!(),
                orig_typed: todo!(),
                replacement: todo!(),
                entry: comp.lists[matches.list_idx].1.get(new_idx),
            };

            let result = hook::trigger(pa, CompletionFocused(entry));
        }

        completions.write(pa).matches = Some(Matches {
            selected: new_idx.map(|idx| Selected {
                idx,
                dist_from_top,
                value: completions.read(pa).lists[matches.list_idx]
                    .1
                    .value_for_index(idx),
            }),
            ..matches
        });

        Completions::set_frame(pa, completions, sidebar);

        Some(original).zip(replacement)
    }

    #[track_caller]
    fn set_frame(pa: &mut Pass, completions: &Handle<Self>, sidebar: Text) {
        if let Some(area) = completions.area().write_as::<duat_term::Area>(pa) {
            let mut frame = Frame {
                above: false,
                below: false,
                ..Frame::default()
            };
            frame.set_text(Side::Left, {
                let sidebar = sidebar.clone();
                move |_| sidebar.clone()
            });
            frame.set_text(Side::Right, move |_| sidebar.clone());
            area.set_frame(frame);
        }

        let (comp, area) = completions.write_with_area(pa);
        area.set_width(
            area.size_of_text(comp.print_opts(), &comp.text)
                .unwrap()
                .x
                .max(40.0),
        )
        .unwrap();

        let height = comp.text.end_point().line() as f32;
        let height = if comp.text.is_empty() { 0.0 } else { height };
        area.set_height(height).unwrap();
    }
}

impl Widget for Completions {
    fn text<'p>(widget: &'p RwData<Self>, pa: &'p Pass) -> &'p Text {
        &widget.read(pa).text
    }

    fn text_mut<'p>(widget: &'p RwData<Self>, pa: &'p mut Pass) -> TextMut<'p> {
        widget.write(pa).text.as_mut()
    }
}

trait ErasedList: Send {
    fn match_indices(&mut self, text: &Text, case_insensitive: bool) -> Option<Vec<usize>>;

    fn start_byte(&self) -> usize;

    fn value_for_index(&self, i: usize) -> String;

    fn text_for_index(&self, i: usize) -> Text;

    fn info_for_index(&self, i: usize) -> Option<Text>;

    fn get(&self, i: usize) -> Box<dyn Any + Send + 'static>;
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

const SPAWN_SPECS: DynSpawnSpecs = DynSpawnSpecs {
    orientation: Orientation::VerLeftBelow,
    height: Some(20.0),
    width: Some(50.0),
    hidden: true,
    inside: false,
};

struct Matches {
    list_idx: usize,
    list: Vec<usize>,
    selected: Option<Selected>,
}

struct Selected {
    idx: usize,
    dist_from_top: usize,
    value: String,
}

type ParamCompletions = Box<Mutex<dyn FnMut(&Pass, usize) -> Box<dyn ErasedList> + Send + Sync>>;
