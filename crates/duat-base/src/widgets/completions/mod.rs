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
        CfgOrScratch, CmdDoc, ColorSchemeArg, Existing, OtherBuffer, Parameter, ReloadOptions,
        ValidFilePath,
    },
    context::{self, Handle},
    data::{Pass, RwData},
    hook::{self, BufferUpdated, KeySent, OnMouseEvent, WidgetOpened, WidgetSwitched},
    mode::{KeyCode, MouseEventKind, event},
    text::{RegexHaystack, Text, TextIndex, TextMut, txt},
    ui::{DynSpawnSpecs, Orientation, Side, Widget},
};
use duat_term::Frame;

pub use crate::widgets::completions::{
    lists::ExhaustiveCompletionsList, paths::PathCompletions, words::WordCompletions,
};
use crate::widgets::{Sections, completions::lists::InnerList};

mod lists;
mod paths;
mod words;

/// Initial setup for completions
pub fn completions_setup() {
    words::track_words();
    Completions::set_for_parameter::<ValidFilePath, _>(75, |_| PathCompletions);

    Completions::set_for_parameter::<Handle<Buffer>, _>(50, |pa| {
        let mut list: Vec<String> = context::windows()
            .buffers(pa)
            .into_iter()
            .map(|buf| buf.read(pa).name())
            .collect();

        list.sort_unstable();

        list
    });

    Completions::set_for_parameter::<OtherBuffer, _>(50, |pa| {
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

    Completions::set_for_parameter::<Existing, _>(25, |_| ["--existing"]);

    Completions::set_for_parameter::<CfgOrScratch, _>(30, |_| {
        ["--cfg", "--cfg-manifest", "--scratch"]
    });

    Completions::set_for_parameter::<ReloadOptions, _>(50, |_| ExhaustiveCompletionsList {
        list: vec!["--clean", "--update"],
    });

    Completions::set_for_parameter::<ColorSchemeArg, _>(50, |_| {
        duat_core::form::colorscheme_list()
    });

    hook::add::<WidgetOpened<Completions>>(move |pa, completions| {
        Completions::scroll_and_update(pa, completions, 0);

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
                    let index = matches.list[selected.idx];

                    let trigger_selected = comp.lists[matches.list_idx].1.get_trigger_selected();
                    let entry = InnerCompletionEntry {
                        index,
                        orig_range: comp.start_byte..comp.start_byte + comp.orig_typed.len(),
                        orig_typed: comp.orig_typed.clone(),
                        replacement: selected.value.clone(),
                        entry: comp.lists[matches.list_idx].1.get(index),
                    };

                    trigger_selected(pa, entry);
                }
            }
        })
        .lateness(0)
        .grouped(ns);

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
pub(crate) struct InnerCompletionEntry {
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

impl std::fmt::Debug for InnerCompletionEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CompletionEntry")
            .field("index", &self.index)
            .field("orig_range", &self.orig_range)
            .field("orig_typed", &self.orig_typed)
            .field("replacement", &self.replacement)
            .finish()
    }
}

impl InnerCompletionEntry {
    /// Returns `Some` if the completion entry came from the given
    /// [provider].
    ///
    /// You should use this function in order to confirm the origin of
    /// a provider before running any post completion hooks.
    ///
    /// [provider]: CompletionsProvider
    pub fn get_as<C: 'static>(&self) -> Option<&C> {
        self.entry.as_ref().downcast_ref()
    }
}

/// A list of completion items.
///
/// The only implementors are:
///
/// - [`WordCompletions`].
/// - [`PathCompletions`].
/// - [`impl IntoIterator<Item = impl CompletionItem>`].
/// - [`ExhaustiveCompletionsList<impl CompletionItem>`].
///
/// [`impl IntoIterator<Item = impl CompletionItem>`]: IntoIterator
#[doc(hidden)]
#[allow(private_bounds)]
pub trait CompletionEntries<C>: Sealed<C> {}
impl<S: Sealed<C>, C> CompletionEntries<C> for S {}

trait Sealed<C> {
    fn into_erased(self, start_byte: usize, min_prefix: usize) -> Box<dyn ErasedList>;
}

/// A type of object that can be completed and will show up in
/// the [`Completions`] widget.
pub trait CompletionItem: Send + Clone + 'static {
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
    case_insensitive: bool,

    start_byte: usize,
    orig_typed: String,
    matches: Option<Matches>,

    sections: Option<(Handle<Sections>, Orientation)>,
    is_parameter_list: bool,
}

impl Completions {
    /// Adds a list of entries to the `Completions`.
    ///
    /// This can be one of three things:
    ///
    /// - Any [`IntoIterator<Item = impl CompletionItem>`].
    /// - The [`WordCompletions`] struct.
    /// - The [`PathCompletions`] struct.
    /// - The [`ExhaustiveCompletionsList`] struct.
    ///
    /// If there was a list before that had the same [`Ns`], that list
    /// will be removed before the new one is added.
    ///
    /// This will also spawn the `Completions` if that is necessary.
    #[track_caller]
    pub fn add_list<C: 'static>(
        pa: &mut Pass,
        entries: impl CompletionEntries<C>,
        start: impl TextIndex,
        priority: usize,
        ns: Ns,
    ) {
        let master = context::current_widget(pa);
        let text = master.text(pa);
        let start_byte = start.to_byte_index();

        let Some(main_byte) = text.get_main_sel().map(|s| s.cursor().byte()) else {
            context::error!("Tried spawning completions on a widget with no selections");
            return;
        };

        if main_byte < start_byte {
            context::error!("Tried adding completion list that starts after cursor");
            return;
        }

        if let Some(completions) = context::handle_of::<Self>(pa)
            && let comp = completions.write(pa)
            && !comp.is_parameter_list
        {
            comp.remove_ns(ns);

            let mut erased = entries.into_erased(start_byte, comp.min_prefix);

            let (Ok(list_idx) | Err(list_idx)) = comp
                .lists
                .binary_search_by(|(prio, ..)| prio.cmp(&priority).reverse());

            if let Some(matches) = &comp.matches
                && comp.lists[matches.list_idx].0 <= priority
                && let Some(list) = erased.match_indices(master.text(pa), true)
            {
                completions.write(pa).matches = Some(Matches { list_idx, list, selected: None });
            } else if let Some(matches) = &mut completions.write(pa).matches
                && matches.list_idx >= list_idx
            {
                matches.list_idx += 1;
            }

            let comp = completions.write(pa);
            comp.lists.insert(list_idx, (priority, erased, ns));

            Self::scroll_and_update(pa, &completions, 0);
        } else {
            Self::close(pa);

            let opts = OPTS.lock().unwrap();

            let min_prefix = if TypeId::of::<C>() == TypeId::of::<CmdDoc>() {
                opts.cmd_min_prefix
            } else if master.widget().is::<Buffer>() {
                opts.min_prefix
            } else {
                0
            };
            let text = master.text(pa);

            let comp = Self {
                lists: vec![(priority, entries.into_erased(start_byte, min_prefix), ns)],
                text: Text::new(),
                max_height: 20,
                cur_min_prefix: 0,
                min_prefix,
                case_insensitive: opts.case_insensitive,
                start_byte,
                orig_typed: text[start_byte..main_byte].to_string(),
                matches: None,
                sections: None,
                is_parameter_list: false,
            };

            master.spawn_on_text(pa, comp, start_byte, *WIDGET_NS, SPAWN_SPECS);
        };
    }

    /// Removes all completion lists that were added with the given
    /// `Ns`.
    pub fn remove_list(pa: &mut Pass, ns: Ns) {
        let Some(completions) = context::handle_of::<Self>(pa) else {
            return;
        };

        completions.write(pa).remove_ns(ns);

        Self::scroll_and_update(pa, &completions, 0);
    }

    fn remove_ns(&mut self, ns: Ns) {
        let mut idx = 0;
        self.lists.retain(|(.., other)| {
            if *other == ns {
                if let Some(matches) = &mut self.matches {
                    if matches.list_idx == idx {
                        self.matches = None;
                    } else if matches.list_idx > idx {
                        matches.list_idx -= 1;
                    }
                };

                false
            } else {
                idx += 1;
                true
            }
        });
    }

    /// Sets the [`CompletionEntries`] for a [`Parameter`].
    pub fn set_for_parameter<P: Parameter, C: CompletionEntries<impl Any>>(
        priority: usize,
        mut entries_fn: impl FnMut(&Pass) -> C + Send + Sync + 'static,
    ) {
        let mut completions = PARAM_COMPLETIONS.lock().unwrap();

        let ns = if let Some((ns, ..)) = completions.remove(&TypeId::of::<P>()) {
            ns
        } else {
            Ns::new()
        };

        completions.insert(
            TypeId::of::<P>(),
            (
                ns,
                priority,
                Box::new(Mutex::new(move |pa: &Pass, start_byte| {
                    let opts = OPTS.lock().unwrap();
                    entries_fn(pa).into_erased(start_byte, opts.cmd_min_prefix)
                })),
            ),
        );
    }

    /// Adds the completions for a [`Parameter`], given its
    /// [`TypeId`].
    pub fn add_parameter_list(pa: &mut Pass, param_type_id: TypeId) {
        let master = context::current_widget(pa);
        let text = master.text(pa);

        let Some(main_byte) = text.get_main_sel().map(|s| s.cursor().byte()) else {
            Self::close(pa);
            context::error!("Tried spawning completions on a widget with no selections");
            return;
        };

        let start_byte = text
            .search([" '([^']|\\')*", "[^ \n]*"])
            .range(..main_byte)
            .next_back()
            .map(|(pat_id, range)| range.start + (pat_id == 0) as usize)
            .unwrap();

        let param_completions = PARAM_COMPLETIONS.lock().unwrap();

        let Some((ns, prio, entries_fn)) = param_completions.get(&param_type_id) else {
            return;
        };

        let mut new_list = (*prio, entries_fn.lock().unwrap()(pa, start_byte), *ns);

        if let Some(completions) = context::handle_of::<Self>(pa)
            && let comp = completions.write(pa)
            && (comp.is_parameter_list && comp.start_byte == start_byte)
        {
            comp.remove_ns(*ns);

            let (Ok(list_idx) | Err(list_idx)) = comp
                .lists
                .binary_search_by(|(prio, ..)| prio.cmp(&new_list.0).reverse());

            if let Some(matches) = &comp.matches
                && comp.lists[matches.list_idx].0 <= *prio
                && let Some(list) = new_list.1.match_indices(master.text(pa), true)
            {
                completions.write(pa).matches = Some(Matches { list_idx, list, selected: None });
            } else if let Some(matches) = &mut completions.write(pa).matches
                && matches.list_idx >= list_idx
            {
                matches.list_idx += 1;
            }

            let comp = completions.write(pa);
            comp.lists.insert(list_idx, new_list);

            Self::scroll_and_update(pa, &completions, 0);
        } else {
            Self::close(pa);

            let case_insensitive = OPTS.lock().unwrap().case_insensitive;
            let text = master.text(pa);

            let comp = Self {
                lists: vec![new_list],
                text: Text::new(),
                max_height: 20,
                cur_min_prefix: 0,
                min_prefix: 0,
                case_insensitive,
                start_byte,
                orig_typed: text[start_byte..main_byte].to_string(),
                matches: None,
                sections: None,
                is_parameter_list: true,
            };

            master.spawn_on_text(pa, comp, start_byte, *WIDGET_NS, SPAWN_SPECS);
        };
    }

    /// Removes the completions for a [`Parameter`], given its
    /// [`TypeId`].
    pub fn remove_parameter_list(pa: &mut Pass, param_type_id: TypeId) {
        let param_completions = PARAM_COMPLETIONS.lock().unwrap();

        if let Some(completions) = context::handle_of::<Self>(pa)
            && let comp = completions.write(pa)
            && comp.is_parameter_list
            && let Some((ns, ..)) = param_completions.get(&param_type_id)
        {
            Self::remove_list(pa, *ns);
        }
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

        let handle = context::handle_of::<Self>(pa)?;

        let main_repl = Self::scroll_and_update(pa, &handle, scroll);
        handle.write(pa).cur_min_prefix = 0;
        main_repl
    }

    /// Wether there is an open `Completions` [`Widget`]
    pub fn is_open(pa: &Pass) -> bool {
        context::handle_of::<Self>(pa).is_some()
    }

    /// Wether or not an entry is selected.
    ///
    /// You should use this if you care about wether or not
    /// the user is completing text right now, so you don't
    /// alter the `Completions`.
    pub fn is_selecting(pa: &Pass) -> bool {
        if let Some(completions) = context::handle_of::<Self>(pa)
            && let Some(matches) = &completions.read(pa).matches
            && matches.selected.is_some()
        {
            true
        } else {
            false
        }
    }

    /// Updates a list of [`CompletionItem`]s.
    ///
    /// This function can be used to add more elements, or to change
    /// things in specific elements, by for example adding
    /// documentation to an item on demand.
    ///
    /// Does not do anything if there was no list to update, or if
    /// `Completions` was closed. Returns `true` if it has updated a
    /// list.
    pub fn update_list<C: CompletionItem>(
        pa: &mut Pass,
        ns: Ns,
        func: impl FnOnce(&mut Vec<C>),
    ) -> bool {
        let Some(completions) = context::handle_of::<Self>(pa) else {
            return false;
        };

        let comp = completions.write(pa);

        if let Some((i, inner)) =
            comp.lists
                .iter_mut()
                .enumerate()
                .find_map(|(i, (_, list, other_ns))| {
                    if *other_ns == ns {
                        Some(i).zip(list.as_any_mut().downcast_mut::<InnerList<C>>())
                    } else {
                        None
                    }
                })
        {
            func(&mut inner.list);

            if let Some(matches) = &comp.matches
                && matches.list_idx == i
                && matches.list.iter().any(|idx| *idx >= inner.list.len())
            {
                comp.matches = None;
            }

            Self::scroll_and_update(pa, &completions, 0);

            true
        } else {
            false
        }
    }

    /// Wether there is a list of a specific [`CompletionItem`] with a
    /// specific [`Ns`].
    pub fn has_list<C: CompletionItem>(pa: &Pass, ns: Ns) -> bool {
        let Some(completions) = context::handle_of::<Self>(pa) else {
            return false;
        };

        completions
            .read(pa)
            .lists
            .iter()
            .any(|(_, list, other_ns)| *other_ns == ns && list.as_any().is::<Vec<C>>())
    }

    /// Sets options for `Completions`.
    ///
    /// `case_insensitive` determines how matching will take place. If
    /// it is set to `true`, then unless the word being typed has an
    /// uppercase letter, matching will ignore case.
    ///
    /// `min_prefix` is the amount of characters that should be typed
    /// before completions shows up. This will only affect "list-like"
    /// completions, that is, word and path completions will not be
    /// affected.
    ///
    /// The default is `true` and `0`, respectively.
    pub fn set_opts(case_insensitive: bool, min_prefix: usize, cmd_min_prefix: usize) {
        *OPTS.lock().unwrap() = Opts {
            case_insensitive,
            min_prefix,
            cmd_min_prefix,
        }
    }

    fn scroll_and_update(
        pa: &mut Pass,
        completions: &Handle<Self>,
        scroll: i32,
    ) -> Option<(String, String)> {
        let master = completions.master(pa).unwrap();

        let Some(main_byte) = master.text(pa).get_main_sel().map(|s| s.cursor().byte()) else {
            _ = completions.close(pa);
            return None;
        };

        let (text, comp) = pa.write_many((master.rw_text(), completions));

        let matches = {
            // If the user types anything, or anything has changed in the text,
            // the matches should be recalculated.
            let still_valid_matches = text.get(comp.start_byte..main_byte).and_then(|strs| {
                comp.matches.take().filter(|matches| {
                    !(comp.is_parameter_list && strs.is_empty())
                        && (matches
                            .selected
                            .as_ref()
                            .is_some_and(|sel| sel.value == strs)
                            || (matches.selected.is_none() && comp.orig_typed == strs))
                })
            });

            if let Some(matches) = still_valid_matches {
                matches
            } else {
                let Some((list, list_idx)) =
                    comp.lists
                        .iter_mut()
                        .enumerate()
                        .find_map(|(i, (_, list, _))| {
                            list.match_indices(&text, true)
                                .filter(|list| !list.is_empty())
                                .zip(Some(i))
                        })
                else {
                    comp.text = Text::default();
                    _ = completions.area().set_height(pa, 0.0);
                    return None;
                };

                comp.orig_typed = text[comp.lists[list_idx].1.start_byte()..main_byte].to_string();

                Matches { list_idx, list, selected: None }
            }
        };

        macro_rules! list {
            () => {
                &completions.read(pa).lists[matches.list_idx].1
            };
        }

        let get_idx = |idx: usize| matches.list[idx];

        let original = if let Some(selected) = &matches.selected {
            selected.value.clone()
        } else {
            comp.orig_typed.clone()
        };

        let (new_idx, replacement) = {
            let new_idx = if let Some(selected) = &matches.selected {
                let len = matches.list.len() as i32;
                let idx = (selected.idx as i32 + scroll).rem_euclid(len + 1) as usize;
                (idx != len as usize).then_some(idx)
            } else if scroll != 0 {
                let len = matches.list.len() as i32;
                let idx = (scroll - 1).rem_euclid(len + 1) as usize;
                (idx != len as usize).then_some(idx)
            } else {
                None
            };

            let replacement = match (&matches.selected, new_idx) {
                (None, None) => None,
                (None, Some(new_idx)) => Some(list!().value_for_index(get_idx(new_idx))),
                (Some(_), None) => Some(comp.orig_typed.clone()),
                (Some(_), Some(new_idx)) => {
                    let new_value = list!().value_for_index(get_idx(new_idx));
                    Some(new_value)
                }
            };

            if let Some(replacement) = &replacement
                && matches
                    .selected
                    .as_ref()
                    .is_none_or(|selected| &selected.value != replacement)
            {
                let mut master_has_changed = false;

                master.edit_all(pa, |mut s| {
                    if !s.text()[..s.cursor().byte()].ends_with(&original) {
                        return;
                    }

                    let start = s.cursor().byte() - original.len();

                    master_has_changed = true;

                    s.move_to(start..s.cursor().byte());

                    s.replace(replacement);
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
        let height = completions.read(pa).max_height.min(matches.list.len());
        let dist_from_top = if replacement.is_some() {
            if let Some(selected) = &matches.selected {
                selected
                    .dist_from_top
                    .saturating_add_signed(scroll as isize)
                    .min(height - 1)
            } else if scroll > 0 {
                (scroll.unsigned_abs() as usize - 1).min(height - 1)
            } else {
                height.saturating_sub(scroll.unsigned_abs() as usize)
            }
        } else {
            matches
                .selected
                .as_ref()
                .map(|sel| sel.dist_from_top)
                .unwrap_or(0)
        };

        let sections = if let Some(idx) = new_idx
            && let Some((info_text, orientation)) = list!().info_for_index(get_idx(idx))
        {
            let sections = if let Some((sections, ori)) = completions.write(pa).sections.take()
                && (!sections.is_closed() && ori == orientation)
            {
                Sections::set_section(pa, &sections, Ns::basic(), info_text, None, 0);
                Some(sections)
            } else {
                let specs = DynSpawnSpecs { orientation, ..Default::default() };
                let sections = Sections::new(Ns::basic(), info_text, None, 0);

                let info_handle = completions.spawn_on_widget(pa, sections, specs);
                completions.write(pa).sections =
                    info_handle.clone().map(|sections| (sections, orientation));
                info_handle
            };

            let value = list!().value_for_index(get_idx(idx));

            if let Some(info_handle) = sections.as_ref()
                && let Some(area) = info_handle.area().write_as::<duat_term::Area>(pa)
            {
                let mut frame = Frame::default();
                frame.set_text(Side::Above, move |_| {
                    txt!("[terminal.border.Info]┤[]{value}[terminal.border.Info]├",)
                });
                area.set_frame(frame);
            }

            sections.zip(Some(orientation))
        } else {
            if let Some((sections, _)) = completions.write(pa).sections.take() {
                _ = sections.close(pa);
            }

            None
        };

        // In this case, don't update the Text, and close the Completions
        // instead. Let the Text be updated on the new Completions
        // when it shows up.
        if list!().start_byte() != completions.read(pa).start_byte {
            let orig_typed = master.text(pa)[list!().start_byte()..main_byte].to_string();

            let comp = completions.write(pa);
            let lists = std::mem::take(&mut comp.lists);
            let list = &lists[matches.list_idx].1;

            let start_byte = list.start_byte();
            let selected = new_idx.map(|idx| Selected {
                idx,
                dist_from_top,
                value: list.value_for_index(get_idx(idx)),
            });

            let new_comp = Self {
                lists,
                text: Text::new(),
                max_height: comp.max_height,

                start_byte,
                orig_typed,
                cur_min_prefix: comp.min_prefix,
                min_prefix: comp.min_prefix,
                case_insensitive: comp.case_insensitive,
                sections: comp.sections.take(),

                matches: Some(Matches { selected, ..matches }),
                is_parameter_list: comp.is_parameter_list,
            };

            master.spawn_on_text(pa, new_comp, start_byte, *WIDGET_NS, SPAWN_SPECS);

            _ = completions.close(pa);
            return Some(original).zip(replacement);
        }

        let (text, sidebar) = {
            let mut entries = Text::builder();
            let mut sidebar = Text::builder();

            let list = &mut completions.write(pa).lists[matches.list_idx].1;

            if let Some(new_idx) = new_idx {
                let top_i = new_idx.saturating_sub(dist_from_top);
                for &idx in matches.list.iter().skip(top_i).take(height) {
                    if idx == get_idx(new_idx) {
                        entries.push(txt!("[selected.Completions]{}\n", list.text_for_index(idx)));
                        sidebar.push(txt!("[selected.Completions] \n"));
                    } else {
                        entries.push(txt!("{}\n", list.text_for_index(idx)));
                        sidebar.push(txt!("[default.Completions] \n"));
                    }
                }
            } else {
                for &idx in matches.list.iter().take(height) {
                    entries.push(txt!("{}\n", list.text_for_index(idx)));
                    sidebar.push(txt!("[default.Completions] \n"));
                }
            }

            (entries.build(), sidebar.build())
        };

        if scroll != 0
            && let Some(index) = new_idx
        {
            let comp = completions.read(pa);

            let trigger_focused = comp.lists[matches.list_idx].1.get_trigger_focused();
            let entry = InnerCompletionEntry {
                index,
                orig_range: comp.start_byte..comp.start_byte + comp.orig_typed.len(),
                orig_typed: comp.orig_typed.clone(),
                replacement: list!().value_for_index(get_idx(index)),
                entry: comp.lists[matches.list_idx].1.get(index),
            };

            trigger_focused(pa, entry);
        }

        let comp = completions.write(pa);

        comp.text = text;
        comp.sections = sections;
        comp.matches = Some(Matches {
            selected: new_idx.map(|idx| Selected {
                idx,
                dist_from_top,
                value: comp.lists[matches.list_idx].1.value_for_index(get_idx(idx)),
            }),
            ..matches
        });

        Self::set_frame(pa, completions, sidebar);

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

    fn text_for_index(&mut self, i: usize) -> Text;

    fn info_for_index(&self, i: usize) -> Option<(Text, Orientation)>;

    fn get(&self, i: usize) -> Box<dyn Any + Send + 'static>;

    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn as_any(&self) -> &dyn Any;

    fn get_trigger_selected(&self) -> fn(&mut Pass, InnerCompletionEntry);

    fn get_trigger_focused(&self) -> fn(&mut Pass, InnerCompletionEntry);
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
    height: None,
    width: None,
    hidden: false,
    inside: false,
};

static OPTS: Mutex<Opts> = Mutex::new(Opts {
    case_insensitive: true,
    min_prefix: 0,
    cmd_min_prefix: 0,
});
static WIDGET_NS: LazyLock<Ns> = Ns::new_lazy();
static PARAM_COMPLETIONS: LazyLock<Mutex<HashMap<TypeId, (Ns, usize, ParamCompletions)>>> =
    LazyLock::new(Mutex::default);

#[derive(Debug)]
struct Matches {
    list_idx: usize,
    list: Vec<usize>,
    selected: Option<Selected>,
}

#[derive(Debug)]
struct Selected {
    idx: usize,
    dist_from_top: usize,
    value: String,
}

struct Opts {
    case_insensitive: bool,
    min_prefix: usize,
    cmd_min_prefix: usize,
}

type ParamCompletions = Box<Mutex<dyn FnMut(&Pass, usize) -> Box<dyn ErasedList> + Send>>;
