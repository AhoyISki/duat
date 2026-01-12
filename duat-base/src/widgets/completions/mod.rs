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
    sync::{LazyLock, Mutex, Once},
};

use duat_core::{
    cmd::{CfgOrManifest, Existing, OtherBuffer, Parameter, ValidFilePath},
    context::{self, Handle},
    data::Pass,
    hook::{self, FocusChanged, WidgetOpened},
    mode::{MouseEvent, MouseEventKind},
    text::{Point, SpawnTag, Strs, Tagger, Text, TextMut, txt},
    ui::{Area, DynSpawnSpecs, Orientation, Side, Widget},
};
use duat_term::Frame;

pub use self::commands::CommandsCompletions;
use crate::widgets::{
    Info,
    completions::{paths::PathCompletions, words::WordCompletions},
};

mod commands;
mod paths;
mod words;

static OPENED_PARAM_COMPLETION: Mutex<Option<Vec<TypeId>>> = Mutex::new(None);
static TAGGER: LazyLock<Tagger> = Tagger::new_static();
static COMPLETIONS: LazyLock<Mutex<HashMap<TypeId, (usize, ParamCompletions)>>> =
    LazyLock::new(Mutex::default);

/// Initial setup for completions
///
/// ONLY MEANT TO BE USED BY THE DUAT EXECUTABLE
#[doc(hidden)]
pub fn setup_completions() {
    words::track_words();
    Completions::set_for_parameter::<ValidFilePath>(75, |_, builder| {
        builder.with_provider(paths::PathCompletions::new(true))
    });

    Completions::set_for_parameter::<Handle>(50, |pa, builder| {
        let mut list: Vec<String> = context::windows()
            .buffers(pa)
            .map(|buf| buf.read(pa).name())
            .collect();

        list.sort_unstable();

        builder.with_provider(list)
    });

    Completions::set_for_parameter::<OtherBuffer>(50, |pa, builder| {
        let current = context::current_buffer(pa).read(pa).name();
        let mut list: Vec<String> = context::windows()
            .buffers(pa)
            .map(|buf| buf.read(pa).name())
            .filter(|name| *name != current)
            .collect();

        list.sort_unstable();

        builder.with_provider(list)
    });

    Completions::set_for_parameter::<Existing>(25, |_, builder| {
        builder.with_provider(["--existing"])
    });

    Completions::set_for_parameter::<CfgOrManifest>(30, |_, builder| {
        builder.with_provider(["--cfg", "--cfg-manifest"])
    });
}

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
    providers: Option<ProvidersFn>,
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
            hook::add::<WidgetOpened<Completions>>(Completions::set_frame);
            hook::add::<FocusChanged>(|pa, (prev, _)| prev.text_mut(pa).remove_tags(*TAGGER, ..));
        });

        let handle = context::current_widget(pa).clone();
        handle.text_mut(pa).remove_tags(*TAGGER, ..);

        let Some(main) = handle.selections(pa).get_main() else {
            context::warn!("Tried spawning [a]Completions[] on a Widget with no [a]Selection[]s");
            return;
        };

        let Some((providers, start_byte, entries)) =
            self.providers.map(|call| call(handle.text(pa), 20))
        else {
            return;
        };

        let (text, sidebar) = entries.unwrap_or_default();

        let completions = Completions {
            master: handle.clone(),
            providers,
            text,
            sidebar,
            max_height: 20,
            start_byte,
            show_without_prefix: self.show_without_prefix,
            last_caret: main.caret(),
            info_handle: None,
        };

        let mut text = handle.text_mut(pa);
        text.insert_tag(*TAGGER, start_byte, SpawnTag::new(completions, SPAWN_SPECS));
    }

    /// Adds a new [`CompletionsProvider`] to be prioritized over
    /// earlier ones
    pub fn with_provider(mut self, provider: impl CompletionsProvider) -> Self {
        let prev = self.providers.take();

        self.providers = Some(Box::new(move |text, height| {
            let (inner, start_byte, entries) = InnerProvider::new(provider, text, height);

            let Some((mut providers, reserve_start_byte, reserve_entries)) =
                prev.map(|call| call(text, height))
            else {
                return (vec![Box::new(inner)], start_byte, entries);
            };

            providers.insert(0, Box::new(inner));

            let start_byte = entries
                .as_ref()
                .and(Some(start_byte))
                .unwrap_or(reserve_start_byte);

            (providers, start_byte, entries.or(reserve_entries))
        }));

        self
    }
}

/// A list of completions, used to quickly fill in matches
pub struct Completions {
    master: Handle<dyn Widget>,
    providers: Vec<Box<dyn ErasedInnerProvider>>,
    text: Text,
    sidebar: Text,
    max_height: usize,
    start_byte: usize,
    show_without_prefix: bool,
    last_caret: Point,
    info_handle: Option<Handle<Info>>,
}

impl Completions {
    /// Returns a new `CompletionsBuilder`
    ///
    /// This `CompletionsBuilder` will not have any
    /// [`CompletionsProvider`]s by default, so it won't spawn a
    /// `Completions` list.
    ///
    /// You can add `CompletionsProvider`s by calling
    /// [`CompletionsBuilder::add_provider`].
    pub fn builder() -> CompletionsBuilder {
        CompletionsBuilder {
            providers: None,
            show_without_prefix: true,
        }
    }

    /// Set the [`CompletionsBuilder`] function for a [`Parameter`]
    /// type
    pub fn set_for_parameter<P: Parameter>(
        priority: usize,
        func: impl FnMut(&Pass, CompletionsBuilder) -> CompletionsBuilder + Send + Sync + 'static,
    ) {
        COMPLETIONS
            .lock()
            .unwrap()
            .insert(TypeId::of::<P>(), (priority, Box::new(Mutex::new(func))));
    }

    /// Spawn the `Completions` list
    pub fn open_default(pa: &mut Pass) {
        Self::builder()
            .with_provider(WordCompletions)
            .with_provider(PathCompletions::new(false))
            .open(pa);
    }

    /// Open the `Completions` for a given [`Parameter`]'s [`TypeId`]
    ///
    /// This completions must've been previously added via
    /// [`Completions::set_for_parameter`].
    ///
    /// Returns [`None`] if none of the `TypeId`s had completions set
    /// for them.
    pub fn open_for(pa: &mut Pass, param_type_ids: &[TypeId]) {
        let completions = COMPLETIONS.lock().unwrap();
        let mut opened_param_completion = OPENED_PARAM_COMPLETION.lock().unwrap();

        let mut builder = Completions::builder();
        builder.show_without_prefix = true;

        let mut param_fns: Vec<_> = param_type_ids
            .iter()
            .filter_map(|ty| Some(*ty).zip(completions.get(ty)))
            .collect();

        if param_fns.is_empty() {
            Completions::close(pa);
            return;
        }

        param_fns.sort_by_key(|(_, (priority, _))| priority);

        if param_fns
            .iter()
            .map(|(ty, _)| ty)
            .eq(opened_param_completion.iter().flatten())
        {
            return;
        }

        *opened_param_completion = Some(param_fns.iter().map(|(ty, _)| *ty).collect());

        for (_, (_, param_fn)) in param_fns {
            builder = (param_fn.lock().unwrap())(pa, builder);
        }

        builder.open(pa);
    }

    /// Closes the `Completions` list
    pub fn close(pa: &mut Pass) {
        let handle = context::current_widget(pa).clone();
        handle.text_mut(pa).remove_tags(*TAGGER, ..);
    }

    /// Goes to the next entry on the list.
    pub fn scroll(pa: &mut Pass, scroll: i32) -> Option<(String, String)> {
        if scroll == 0 {
            context::warn!("Scrolling [a]Completions[] by 0");
            return None;
        }

        let handle = context::windows()
            .handles_of::<Completions>(pa)
            .first()
            .cloned()?;

        let main_repl = Completions::update_text_and_position(pa, &handle, scroll);
        handle.write(pa).show_without_prefix = true;
        main_repl
    }

    /// Wether there is an open `Completions` [`Widget`]
    pub fn is_open(pa: &Pass) -> bool {
        context::current_window(pa)
            .handles(pa)
            .any(|handle| handle.widget().is::<Completions>())
    }

    fn update_text_and_position(
        pa: &mut Pass,
        handle: &Handle<Self>,
        scroll: i32,
    ) -> Option<(String, String)> {
        let master_handle = handle.master().unwrap();
        let (master, area, comp) =
            pa.write_many((master_handle.widget(), handle.area(), handle.widget()));

        let mat = {
            let mut lists: Vec<_> = comp
                .providers
                .iter_mut()
                .map(|inner| {
                    let texts_and_match = inner.texts_and_match(
                        master.text(),
                        scroll,
                        Some(area),
                        comp.max_height,
                        comp.show_without_prefix,
                    );
                    (texts_and_match, inner.start_fn())
                })
                .collect();
            lists.sort_by_key(|((start, _), _)| *start);
            lists
                .into_iter()
                .find_map(|((start, list), start_fn)| list.map(|list| ((start, list), start_fn)))
        };

        // Believe it or not, this is necessary to prevent Drop semantincs
        // from invalidating the following code.
        let (other, start_fn) = mat.unzip();

        let main_repl = if let Some((start_byte, ((text, sides), replacement))) = other {
            comp.text = text;
            comp.sidebar = sides;

            let mut main_replacement = None;

            let mut new_start_byte = start_byte;
            if let Some((replacement, info_text)) = replacement {
                // Also necessary, believe it or not.
                let start_fn = start_fn.unwrap();

                let mut starts = master
                    .text()
                    .selections()
                    .iter()
                    .map(|(sel, _)| start_fn(master.text(), sel.caret()))
                    .collect::<Vec<_>>()
                    .into_iter();

                drop(start_fn);
                let mut shift = 0;

                master_handle.edit_all(pa, |mut c| {
                    let start = (starts.next().unwrap() as i32 + shift) as usize;
                    shift += replacement.len() as i32 - (c.caret().byte() as i32 - start as i32);

                    c.move_to(start..c.caret().byte());

                    if c.is_main() {
                        main_replacement = Some((c.selection().to_string(), replacement.clone()));
                    }

                    c.replace(&replacement);
                    c.unset_anchor();
                    if !replacement.is_empty() {
                        c.move_hor(1);
                    }

                    if c.is_main() {
                        new_start_byte = start;
                    }
                });

                if let Some((info_text, orientation)) = info_text {
                    let info_handle = if let Some(info) = handle.read(pa).info_handle.clone() {
                        info.write(pa).text = info_text;
                        Some(info)
                    } else {
                        let specs = DynSpawnSpecs {
                            orientation,
                            width: None,
                            height: None,
                            ..Default::default()
                        };

                        let info_handle = handle.spawn_widget(pa, Info::new(info_text), specs);
                        handle.write(pa).info_handle = info_handle.clone();
                        info_handle
                    };

                    if let Some(info_handle) = info_handle.as_ref()
                        && let Some(area) = info_handle.area().write_as::<duat_term::Area>(pa)
                    {
                        let mut frame = Frame {
                            above: true,
                            below: true,
                            left: true,
                            right: true,
                            ..Default::default()
                        };
                        frame.set_text(Side::Above, move |_| {
                            txt!("[terminal.frame.Info]┤{replacement}[terminal.frame.Info]├")
                        });
                        area.set_frame(frame);
                    }
                } else if let Some(prev) = handle.write(pa).info_handle.take() {
                    let _ = prev.close(pa);
                }
            } else {
                drop(start_fn);
            }

            let comp = handle.write(pa);

            // In this case, move the Completions to a new location
            if start_byte != comp.start_byte {
                let new_comp = Self {
                    master: handle.master().unwrap().clone(),
                    providers: std::mem::take(&mut comp.providers),
                    text: std::mem::take(&mut comp.text),
                    sidebar: std::mem::take(&mut comp.sidebar),
                    max_height: comp.max_height,
                    start_byte: new_start_byte,
                    show_without_prefix: false,
                    last_caret: comp.last_caret,
                    info_handle: comp.info_handle.take(),
                };

                let mut text = master_handle.text_mut(pa);
                text.remove_tags(*TAGGER, ..);
                text.insert_tag(*TAGGER, start_byte, SpawnTag::new(new_comp, SPAWN_SPECS));
                return main_replacement;
            } else {
                comp.start_byte = new_start_byte;
            }

            main_replacement
        } else {
            drop(start_fn);
            comp.text = Text::default();
            comp.sidebar = Text::default();
            None
        };

        let (comp, area) = handle.write_with_area(pa);
        let height = comp.text.len().line() as f32;

        area.set_height(if comp.text.is_empty() { 0.0 } else { height })
            .unwrap();
        Completions::set_frame(pa, handle);

        main_repl
    }

    fn set_frame(pa: &mut Pass, handle: &Handle<Self>) {
        let sidebar = handle.read(pa).sidebar.clone();
        if let Some(area) = handle.area().write_as::<duat_term::Area>(pa) {
            let mut frame = Frame {
                left: true,
                right: true,
                ..Frame::default()
            };
            frame.set_text(Side::Left, {
                let sidebar = sidebar.clone();
                move |_| sidebar.clone()
            });
            frame.set_text(Side::Right, move |_| sidebar.clone());
            area.set_frame(frame);
        }

        let (comp, area) = handle.write_with_area(pa);
        area.set_width(
            area.size_of_text(comp.get_print_opts(), &comp.text)
                .unwrap()
                .x
                .max(40.0),
        )
        .unwrap();
    }
}

impl Widget for Completions {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        Self::update_text_and_position(pa, handle, 0);
        let master_handle = handle.master().unwrap();
        handle.write(pa).last_caret = master_handle.selections(pa).main().caret();

        Completions::set_frame(pa, handle);
    }

    fn needs_update(&self, pa: &Pass) -> bool {
        let text = self.master.has_changed(pa).then_some(self.master.text(pa));
        let main_moved = text
            .as_ref()
            .is_some_and(|text| text.get_main_sel().unwrap().caret() != self.last_caret);

        main_moved || self.providers.iter().any(|inner| inner.has_changed(text))
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> TextMut<'_> {
        self.text.as_mut()
    }

    fn on_mouse_event(pa: &mut Pass, _: &Handle<Self>, event: MouseEvent) {
        match event.kind {
            MouseEventKind::ScrollDown => _ = Self::scroll(pa, 1),
            MouseEventKind::ScrollUp => _ = Self::scroll(pa, -1),
            _ => {}
        }
    }
}

impl Drop for Completions {
    fn drop(&mut self) {
        *OPENED_PARAM_COMPLETION.lock().unwrap() = None;
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
    /// If the `Vec` within is empty, then the next
    /// `CompletionsProvider` will be selected to provide the required
    /// completions. If all of them return empty `Iterator`s, then no
    /// completions will be shown.
    ///
    /// The `target_changed` parameter determines that the prefix has
    /// changed from what was previously selected, either by the user
    /// typing more characters or by some other external factor. This
    /// is useful in circumstances where you want to show completions
    /// based on what the user typed, and not from the entries that
    /// were sent.
    ///
    /// [caret]: duat_core::mode::Selection::caret
    /// [`Self::word_regex`]: CompletionsProvider::word_regex
    /// ["completeness"]: CompletionsKind
    fn completions(
        &mut self,
        text: &Text,
        caret: Point,
        prefix: &str,
        has_changed: bool,
    ) -> CompletionsList<Self>;

    /// Get the starting byte for this completions
    ///
    /// This function should look at the bytes before `from` (and
    /// sometimes after), in order to figure out where the completion
    /// starts.
    ///
    /// For example, if you're completing words, you should logially
    /// look for the start of the word that intersects with the
    /// `from`th byte.
    fn get_start(&self, text: &Text, caret: Point) -> Option<usize>;

    /// Wether the list of completion entries has been updated
    ///
    /// This is particularly useful if the `CompletionsProvider` could
    /// be slow, such as one from an LSP or things of the sort.
    fn has_changed(&self) -> bool;

    /// Additional information about an entry, which can be shown when
    /// it is selected.
    fn default_info_on(&self, _: (&str, &Self::Info)) -> Option<(Text, Orientation)> {
        None
    }
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
    #[allow(clippy::type_complexity)]
    fn texts_and_match(
        &mut self,
        text: &Text,
        scroll: i32,
        area: Option<&mut Area>,
        max_height: usize,
        show_without_prefix: bool,
    ) -> (
        usize,
        Option<((Text, Text), Option<(String, Option<(Text, Orientation)>)>)>,
    );

    fn has_changed(&self, text: Option<&Text>) -> bool;

    #[allow(clippy::type_complexity)]
    fn start_fn(&self) -> Box<dyn Fn(&Text, Point) -> usize + '_>;
}

#[allow(clippy::type_complexity)]
struct InnerProvider<P: CompletionsProvider> {
    provider: P,
    fmt: Box<dyn FnMut(&str, &P::Info) -> Text + Send>,

    orig_prefix: String,
    current: Option<(String, usize)>,

    filtered_entries: FilteredEntries<P>,
    entries: Vec<(String, P::Info)>,
}

impl<P: CompletionsProvider> InnerProvider<P> {
    #[allow(clippy::type_complexity)]
    fn new(mut provider: P, text: &Text, height: usize) -> (Self, usize, Option<(Text, Text)>) {
        let Some(main_caret) = text.get_main_sel().map(|sel| sel.caret()) else {
            panic!("Tried to spawn completions on a Text with no main selection");
        };

        let start = provider
            .get_start(text, main_caret)
            .unwrap_or(main_caret.byte());

        let orig_prefix = text.strs(start..main_caret.byte()).unwrap().to_string();

        let completions = provider.completions(text, main_caret, &orig_prefix, false);

        let filtered_entries = match completions.kind {
            CompletionsKind::Finished => FilteredEntries::UnfilteredFinished(
                completions
                    .entries
                    .iter()
                    .filter(|(entry, _)| string_cmp(&orig_prefix, entry).is_some())
                    .map(|(entry, info)| (entry.clone(), info.clone()))
                    .collect(),
            ),
            CompletionsKind::UnfinishedFiltered => FilteredEntries::FilteredUnfinished,
            CompletionsKind::UnfinishedUnfiltered => FilteredEntries::UnfilteredUnfinished({
                completions
                    .entries
                    .iter()
                    .filter(|(entry, _)| string_cmp(&orig_prefix, entry).is_some())
                    .map(|(entry, info)| (entry.clone(), info.clone()))
                    .collect()
            }),
        };

        let mut inner = Self {
            provider,
            orig_prefix,
            current: None,
            filtered_entries,
            entries: completions.entries,
            fmt: Box::new(P::default_fmt),
        };

        let (start, text) = inner.texts_and_match(text, 0, None, height, true);
        (inner, start, text.unzip().0)
    }
}

impl<P: CompletionsProvider> ErasedInnerProvider for InnerProvider<P> {
    #[track_caller]
    fn texts_and_match(
        &mut self,
        text: &Text,
        scroll: i32,
        area: Option<&mut Area>,
        max_height: usize,
        show_without_prefix: bool,
    ) -> (
        usize,
        Option<((Text, Text), Option<(String, Option<(Text, Orientation)>)>)>,
    ) {
        use FilteredEntries::*;
        let Some(caret) = text.get_main_sel().map(|sel| sel.caret()) else {
            panic!("Tried to spawn completions on a Text with no main selection");
        };

        let start = self.provider.get_start(text, caret).unwrap_or(caret.byte());

        let Some(prefix) = text.strs(start..caret.byte()).as_ref().map(Strs::to_string) else {
            panic!("Failed to get prefix from {:?}", start..caret.byte());
        };

        // This should only be true if edits other than the one applied by
        // Completions take place.
        let target_changed = self.current.as_ref().is_some_and(|(c, _)| *c != prefix)
            || (self.current.is_none() && self.orig_prefix != prefix);

        if let UnfilteredUnfinished(_) | FilteredUnfinished = &self.filtered_entries
            && target_changed
        {
            self.entries = self
                .provider
                .completions(text, caret, &prefix, true)
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

        if entries.is_empty() || (start == caret.byte() && !show_without_prefix) {
            self.current = None;
            return (start, None);
        }

        let height = if let Some(area) = area {
            area.set_height(entries.len().min(max_height) as f32)
                .unwrap();
            area.height() as usize
        } else {
            max_height
        };

        let mut ret_info = None;
        if scroll != 0 {
            // No try blocks on stable Rust 🤮.
            self.current = (|| -> Option<(String, usize)> {
                if let Some((prev, dist)) = &self.current {
                    let dist = dist.saturating_add_signed(scroll as isize).min(height - 1);
                    let prev_i = entries.iter().position(|(w, _)| w == prev)?;
                    let (word, info) = entries.get(prev_i.checked_add_signed(scroll as isize)?)?;

                    ret_info = Some(info);
                    Some((word.clone(), dist))
                } else if scroll > 0 {
                    let scroll = scroll.unsigned_abs() as usize - 1;
                    let dist = (scroll).min(height - 1);
                    let (word, info) = entries.get(scroll)?;

                    ret_info = Some(info);
                    Some((word.clone(), dist))
                } else {
                    let scroll = scroll.unsigned_abs() as usize;
                    let dist = height.saturating_sub(scroll);
                    let (word, info) = entries.get(entries.len().checked_sub(scroll)?)?;

                    ret_info = Some(info);
                    Some((word.clone(), dist))
                }
            })();
        }

        let mut entries_builder = Text::builder();
        let mut sidebar_builder = Text::builder();

        if let Some((word, dist)) = &mut self.current
            && let Some(word_i) = entries.iter().position(|(w, _)| w == word)
        {
            *dist = (*dist).min(height - 1);

            let top_i = word_i.saturating_sub(*dist);
            for (i, (entry, info)) in entries.iter().enumerate().skip(top_i).take(height) {
                if i == word_i {
                    entries_builder
                        .push(txt!("[selected.Completions]{}\n", (self.fmt)(entry, info)));
                    sidebar_builder.push(txt!("[selected.Completions] \n"));
                } else {
                    entries_builder.push(txt!("{}\n", (self.fmt)(entry, info)));
                    sidebar_builder.push(txt!("[default.Completions] \n"));
                }
            }
        } else {
            for (entry, info) in entries.iter().take(height) {
                entries_builder.push(txt!("{}\n", (self.fmt)(entry, info)));
                sidebar_builder.push(txt!("[default.Completions] \n"));
            }
        }

        let replacement = if scroll != 0 {
            self.current
                .clone()
                .map(|(w, _)| {
                    let text = self.provider.default_info_on((&w, ret_info.unwrap()));
                    (w, text)
                })
                .or_else(|| Some((self.orig_prefix.clone(), None)))
        } else {
            None
        };

        let entries = entries_builder.build_no_double_nl();
        let sidebar = sidebar_builder.build_no_double_nl();
        (start, Some(((entries, sidebar), replacement)))
    }

    fn has_changed(&self, text: Option<&Text>) -> bool {
        let Some(main) = text.and_then(|text| text.get_main_sel()) else {
            return false;
        };

        let word_has_changed = text.is_some_and(|text| {
            let start = self
                .provider
                .get_start(text, main.caret())
                .unwrap_or(main.caret().byte());
            let prefix = text.strs(start..main.caret().byte()).unwrap();

            prefix != self.orig_prefix.as_str()
        });

        word_has_changed || self.provider.has_changed()
    }

    fn start_fn(&self) -> Box<dyn Fn(&Text, Point) -> usize + '_> {
        Box::new(|text, caret| self.provider.get_start(text, caret).unwrap_or(caret.byte()))
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

const SPAWN_SPECS: DynSpawnSpecs = DynSpawnSpecs {
    orientation: Orientation::VerLeftBelow,
    height: Some(20.0),
    width: Some(50.0),
    hidden: true,
    inside: false,
};

type ProvidersFn = Box<
    dyn FnOnce(
        &Text,
        usize,
    ) -> (
        Vec<Box<dyn ErasedInnerProvider>>,
        usize,
        Option<(Text, Text)>,
    ),
>;
type ParamCompletions =
    Box<Mutex<dyn FnMut(&Pass, CompletionsBuilder) -> CompletionsBuilder + Send + Sync>>;

mod fixed {
    use duat_core::text::{Point, RegexHaystack, Spacer, Text, txt};

    use crate::widgets::{
        CompletionsKind, CompletionsList, CompletionsProvider, completions::string_cmp,
    };

    impl CompletionsProvider for Vec<String> {
        type Info = ();

        fn default_fmt(entry: &str, _: &Self::Info) -> Text {
            txt!("{entry}{Spacer}")
        }

        fn completions(
            &mut self,
            _: &Text,
            _: Point,
            prefix: &str,
            _: bool,
        ) -> CompletionsList<Self> {
            let mut entries: Vec<_> = self
                .iter()
                .filter_map(|entry| string_cmp(prefix, entry).map(|_| (entry.clone(), ())))
                .collect();

            entries.sort_by(|(lhs, _), (rhs, _)| {
                string_cmp(prefix, lhs)
                    .unwrap()
                    .cmp(&string_cmp(prefix, rhs).unwrap())
            });

            CompletionsList {
                entries,
                kind: CompletionsKind::UnfinishedFiltered,
            }
        }

        fn get_start(&self, text: &Text, caret: Point) -> Option<usize> {
            Some(text.search(r"\S*").range(..caret).next_back()?.start)
        }

        fn has_changed(&self) -> bool {
            false
        }
    }

    impl<const N: usize> CompletionsProvider for [&'static str; N] {
        type Info = ();

        fn default_fmt(entry: &str, _: &Self::Info) -> Text {
            txt!("{entry}{Spacer}")
        }

        fn completions(
            &mut self,
            _: &Text,
            _: Point,
            prefix: &str,
            _: bool,
        ) -> CompletionsList<Self> {
            let mut entries: Vec<_> = self
                .iter()
                .filter_map(|entry| string_cmp(prefix, entry).map(|_| (entry.to_string(), ())))
                .collect();

            entries.sort_by(|(lhs, _), (rhs, _)| {
                string_cmp(prefix, lhs)
                    .unwrap()
                    .cmp(&string_cmp(prefix, rhs).unwrap())
            });

            CompletionsList {
                entries,
                kind: CompletionsKind::UnfinishedFiltered,
            }
        }

        fn get_start(&self, text: &Text, caret: Point) -> Option<usize> {
            Some(text.search(r"\S*").range(..caret).next_back()?.start)
        }

        fn has_changed(&self) -> bool {
            false
        }
    }
}
