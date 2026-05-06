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
    cmd::{
        CfgOrManifest, ColorSchemeArg, Existing, OtherBuffer, Parameter, ReloadOptions,
        ValidFilePath,
    },
    context::{self, Handle},
    data::Pass,
    hook::{self, KeySent, ModeSwitched, OnMouseEvent, WidgetOpened},
    mode::{KeyCode, MouseEventKind, event},
    text::{Point, Spawn, Text, TextMut, txt},
    ui::{Area, DynSpawnSpecs, Orientation, Side, Widget},
};
use duat_term::Frame;

pub use crate::widgets::completions::{
    commands::CommandsCompletions, lists::ExhaustiveCompletionsList, paths::PathCompletions,
    words::WordCompletions,
};
use crate::{
    hooks::{CompletionFinished, CompletionSelected},
    widgets::Info,
};

mod commands;
mod lists;
mod paths;
mod words;

static BUFFER_COMPLETIONS: Mutex<Option<BufferCompletionsFn>> = Mutex::new(None);
static OPENED_PARAM_COMPLETION: Mutex<Option<Vec<TypeId>>> = Mutex::new(None);
static NS: LazyLock<Ns> = Ns::new_lazy();
static COMPLETIONS: LazyLock<Mutex<HashMap<TypeId, (usize, ParamCompletions)>>> =
    LazyLock::new(Mutex::default);

/// Initial setup for completions
///
/// ONLY MEANT TO BE USED BY THE DUAT EXECUTABLE
#[doc(hidden)]
pub fn completions_setup() {
    words::track_words();
    Completions::set_for_parameter::<ValidFilePath>(75, |_, builder| {
        builder.with_provider(paths::PathCompletions::new(true, true))
    });

    Completions::set_for_parameter::<Handle>(50, |pa, builder| {
        let mut list: Vec<String> = context::windows()
            .buffers(pa)
            .into_iter()
            .map(|buf| buf.read(pa).name())
            .collect();

        list.sort_unstable();

        builder.with_provider(list)
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

        builder.with_provider(list)
    });

    Completions::set_for_parameter::<Existing>(25, |_, builder| {
        builder.with_provider(["--existing"])
    });

    Completions::set_for_parameter::<CfgOrManifest>(30, |_, builder| {
        builder.with_provider(["--cfg", "--cfg-manifest"])
    });

    Completions::set_for_parameter::<ReloadOptions>(50, |_, builder| {
        builder.with_provider(ExhaustiveCompletionsList {
            list: vec!["--clean", "--update"],
            only_one: false,
        })
    });

    Completions::set_for_parameter::<ColorSchemeArg>(50, |_, builder| {
        builder.with_provider(duat_core::form::colorscheme_list())
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

                if let event!(KeyCode::Char(..)) = key_event
                    && let Some((_, entry)) = completions.write(pa).current_entry.take()
                {
                    hook::trigger(pa, CompletionFinished(entry));
                }
            }
        })
        .lateness(0);

        hook::add::<KeySent>(move |pa, _| {
            let completions_master = completions.master(pa).unwrap();

            if completions.is_closed()
                || completions_master.is_closed()
                || completions_master.selections(pa).is_empty()
            {
                hook::remove(ns);
                return;
            }

            Completions::update_text_and_position(pa, &completions, 0);
            completions.write(pa).last_cursor = completions_master.selections(pa).main().cursor();
            if !completions.is_closed() {
                Completions::set_frame(pa, &completions);
            }
        })
        .lateness(100_000_000)
        .grouped(ns);
    });

    hook::add::<ModeSwitched>(move |pa, switch| {
        if switch.old.handle != switch.new.handle {
            switch.old.handle.text_mut(pa).remove_tags(*NS, ..);
            Completions::close(pa);
        }
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
/// [`CompletionSelected`] and [`CompletionFinished`] hooks.
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
    pub fn get_for<P: CompletionsProvider>(&self) -> Option<&P::Entry> {
        self.entry.as_ref().downcast_ref()
    }
}

/// A builder for [`Completions`], a [`Widget`] to show word
/// completions
///
/// The `Completions` widget is supposed to give the ability to
/// automatically fill in words, usually by pressing the `Tab`
/// character in order to scroll through a list of options.
///
/// The [`Completions`] will show words that match the word behind the
/// main [`Selection`]'s [cursor], and they will automatically follow
/// the [`Selection`] if it moves to other words.
///
/// Initially, even if there is no word before the [cursor],
/// completions will be shown, unless you set [`show_without_prefix`]
/// to `false`. However, as the list moves around, completions will
/// only show up if there is a word behind the [cursor], as to not be
/// bothersome.
///
/// [`Selection`]: duat_core::mode::Selection
/// [cursor]: duat_core::mode::Selection::cursor
/// [`show_without_prefix`]: Self::show_without_prefix
#[derive(Default)]
pub struct CompletionsBuilder {
    providers: Option<ProvidersFn>,
    /// How many characters should precede the cursor before showing
    /// the [`Completions`].
    pub min_prefix: usize,
    /// On command callers or parameters, many characters should
    /// precede the cursor before showing the [`Completions`].
    pub cmd_min_prefix: usize,
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
        let handle = context::current_widget(pa).clone();
        // Do both removals, because the previous Completions need to be
        // removed immediately.
        handle.text_mut(pa).remove_tags(*NS, ..);
        if let Some(completions) = context::handle_of::<Completions>(pa) {
            _ = completions.close(pa);
        }

        let Some(main) = handle.selections(pa).get_main() else {
            context::warn!("Tried spawning [a]Completions[] on a Widget with no [a]Selection[]s");
            return;
        };

        let Some((providers, provided)) = self
            .providers
            .map(|call| call(handle.text(pa), 20, self.min_prefix))
        else {
            return;
        };

        let list = provided.list.unwrap_or_default();

        let completions = Completions {
            providers,
            text: list.entries_text,
            sidebar: list.sidebar_text,
            max_height: 20,
            start_byte: provided.start,
            cur_min_prefix: self.min_prefix,
            min_prefix: self.min_prefix,
            last_cursor: main.cursor(),
            info_handle: None,
            current_entry: None,
        };

        let mut text = handle.text_mut(pa);
        text.insert_tag(*NS, provided.start, Spawn::new(completions, SPAWN_SPECS));
    }

    /// Adds a new [`CompletionsProvider`] to be prioritized over
    /// earlier ones
    pub fn with_provider(mut self, provider: impl CompletionsProvider) -> Self {
        let prev = self.providers.take();

        self.providers = Some(Box::new(move |text, height, min_prefix| {
            let (inner, provided) = InnerProvider::new(provider, text, height, min_prefix);

            let Some((mut providers, reserve)) = prev.map(|call| call(text, height, min_prefix))
            else {
                return (vec![Box::new(inner)], provided);
            };

            providers.push(Box::new(inner));

            if provided.list.is_some() {
                (providers, provided)
            } else {
                (providers, reserve)
            }
        }));

        self
    }
}

/// A list of completions, used to quickly fill in matches
pub struct Completions {
    providers: Vec<Box<dyn ErasedInnerProvider>>,
    text: Text,
    sidebar: Text,
    max_height: usize,
    start_byte: usize,
    cur_min_prefix: usize,
    min_prefix: usize,
    last_cursor: Point,
    info_handle: Option<Handle<Info>>,
    current_entry: Option<(usize, CompletionEntry)>,
}

impl Completions {
    /// Returns a new `CompletionsBuilder`
    ///
    /// This `CompletionsBuilder` will not have any
    /// [`CompletionsProvider`]s by default, so it won't spawn a
    /// `Completions` list.
    ///
    /// You can add `CompletionsProvider`s by calling
    /// [`CompletionsBuilder::with_provider`].
    pub fn builder() -> CompletionsBuilder {
        CompletionsBuilder {
            providers: None,
            min_prefix: 0,
            cmd_min_prefix: 0,
        }
    }

    /// Set the default completions for `Buffers`.
    pub fn set_default(func: impl FnMut(&mut Pass) -> CompletionsBuilder + Send + 'static) {
        *BUFFER_COMPLETIONS.lock().unwrap() = Some(Box::new(func));
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
        if let Some(func) = BUFFER_COMPLETIONS.lock().unwrap().as_mut() {
            func(pa).open(pa);
        } else {
            Self::builder()
                .with_provider(WordCompletions::new(true))
                .with_provider(PathCompletions::new(true, false))
                .open(pa);
        }
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
        builder.min_prefix = builder.cmd_min_prefix;

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
        handle.text_mut(pa).remove_tags(*NS, ..);
    }

    /// Goes to the next entry on the list.
    pub fn scroll(pa: &mut Pass, scroll: i32) -> Option<(String, String)> {
        if scroll == 0 {
            context::warn!("Scrolling [a]Completions[] by 0");
            return None;
        }

        let handle = context::handle_of::<Completions>(pa)?;

        let main_repl = Completions::update_text_and_position(pa, &handle, scroll);
        handle.write(pa).cur_min_prefix = 0;
        main_repl
    }

    /// Wether there is an open `Completions` [`Widget`]
    pub fn is_open(pa: &Pass) -> bool {
        context::handle_of::<Completions>(pa).is_some()
    }

    /// Update a [`CompletionsProvider`], alongside the last provided
    /// list of entries.
    ///
    /// This function may not be called if the provider is no longer
    /// present.
    pub fn update_provider<P: CompletionsProvider>(
        pa: &mut Pass,
        update: impl FnOnce(&mut P, &mut Vec<P::Entry>),
    ) {
        let Some(completions) = context::handle_of::<Completions>(pa) else {
            return;
        };

        if let Some(inner) = completions
            .write(pa)
            .providers
            .iter_mut()
            .find_map(|provider| provider.as_any().downcast_mut::<InnerProvider<P>>())
        {
            update(&mut inner.provider, &mut inner.matches);
            inner.has_changed = true;

            Completions::update_text_and_position(pa, &completions, 0);
            let completions_master = completions.master(pa).unwrap();
            completions.write(pa).last_cursor = completions_master.selections(pa).main().cursor();
            if !completions.is_closed() {
                Completions::set_frame(pa, &completions);
            }
        }
    }

    #[track_caller]
    fn update_text_and_position<'a>(
        pa: &'a mut Pass,
        completions: &'a Handle<Self>,
        scroll: i32,
    ) -> Option<(String, String)> {
        let master_handle = completions.master(pa).unwrap();
        let (master, (area, comp)) = pa
            .read_and_write_many(
                master_handle.widget(),
                (completions.area(), completions.widget()),
            )
            .unwrap();

        let found_list = {
            let indices = if let Some((main_idx, _)) = &comp.current_entry {
                [
                    0..*main_idx,
                    *main_idx + 1..comp.providers.len(),
                    *main_idx..*main_idx + 1,
                ]
            } else {
                [0..comp.providers.len(), 0..0, 0..0]
            };

            let mut lists = Vec::from_iter(
                comp.providers
                    .get_disjoint_mut(indices.clone())
                    .unwrap()
                    .into_iter()
                    .zip(indices)
                    .flat_map(|(providers, indices)| indices.into_iter().zip(providers))
                    .map(|(idx, provider)| {
                        let provided = provider.process(
                            master.text(),
                            scroll,
                            Some(area),
                            comp.max_height,
                            comp.cur_min_prefix,
                        );
                        (idx, (provided, provider.start_fn()))
                    }),
            );

            lists.sort_by_key(|(_, (provided, _))| provided.start);

            lists
                .into_iter()
                .rev()
                .find_map(|(idx, (provided, start_fn))| {
                    provided
                        .list
                        .map(|list| ((idx, provided.start, list), start_fn))
                })
        };

        // Believe it or not, this is necessary to prevent Drop semantincs
        // from invalidating the following code.
        let (value, start_fn) = found_list.unzip();

        let main_replacement = if let Some((idx, start_byte, list)) = value {
            comp.text = list.entries_text;
            comp.sidebar = list.sidebar_text;
            let mut main_replacement = None;

            let mut new_start_byte = start_byte;
            if let Some(repl) = list.replacement {
                let (word, info) = match repl {
                    Replacement::FromList(entry, info) => {
                        let word = entry.replacement.clone();
                        comp.current_entry = Some((idx, entry));
                        (word, info)
                    }
                    Replacement::WithOrig(word) => {
                        comp.current_entry = None;
                        (word, None)
                    }
                };

                // Also necessary, believe it or not.
                let start_fn = start_fn.unwrap();

                let mut starts = master
                    .text()
                    .selections()
                    .iter()
                    .map(|(sel, _)| start_fn(master.text(), sel.cursor()))
                    .collect::<Vec<_>>()
                    .into_iter();

                drop(start_fn);
                let mut shift = 0;
                let mut master_has_changed = false;

                master_handle.edit_all(pa, |mut s| {
                    let start = (starts.next().unwrap() as i32 + shift) as usize;
                    if &s.text()[start..s.cursor().byte()] == word {
                        return;
                    }

                    master_has_changed = true;
                    shift += word.len() as i32 - (s.cursor().byte() as i32 - start as i32);

                    s.move_to(start..s.cursor().byte());

                    if s.is_main() {
                        main_replacement = Some((s.selection().to_string(), word.clone()));
                    }

                    s.replace(&word);
                    s.unset_anchor();
                    if !word.is_empty() {
                        s.move_hor(1);
                    }

                    if s.is_main() {
                        new_start_byte = start;
                    }
                });

                if !master_has_changed {
                    master_handle.widget().declare_unwritten();
                    master_handle.area().declare_unwritten();
                }

                if let Some((info_text, orientation)) = info {
                    let info_handle = if let Some(info) = completions.read(pa).info_handle.clone()
                        && !info.is_closed()
                    {
                        Info::set_text(pa, &info, |text| *text = info_text);
                        Some(info)
                    } else {
                        let specs = DynSpawnSpecs {
                            orientation,
                            width: None,
                            height: None,
                            ..Default::default()
                        };

                        let info_handle = completions.spawn_widget(pa, Info::new(info_text), specs);
                        completions.write(pa).info_handle = info_handle.clone();
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
                            txt!("[terminal.border.Info]┤[]{word}[terminal.border.Info]├")
                        });
                        area.set_frame(frame);
                    }
                } else if let Some(prev) = completions.write(pa).info_handle.take() {
                    let _ = prev.close(pa);
                }
            } else {
                drop(start_fn);
            }

            if scroll != 0
                && let Some((main_idx, entry)) = completions.write(pa).current_entry.take()
            {
                let result = hook::trigger(pa, CompletionSelected(entry));
                completions.write(pa).current_entry = Some((main_idx, result.0));
            }

            let comp = completions.write(pa);

            // In this case, move the Completions to a new location
            if start_byte != comp.start_byte {
                let new_comp = Self {
                    providers: std::mem::take(&mut comp.providers),
                    text: std::mem::take(&mut comp.text),
                    sidebar: std::mem::take(&mut comp.sidebar),
                    max_height: comp.max_height,
                    start_byte: new_start_byte,
                    cur_min_prefix: comp.min_prefix,
                    min_prefix: comp.min_prefix,
                    last_cursor: comp.last_cursor,
                    info_handle: comp.info_handle.take(),
                    current_entry: comp.current_entry.take(),
                };

                let mut text = master_handle.text_mut(pa);
                text.insert_tag(*NS, start_byte, Spawn::new(new_comp, SPAWN_SPECS));
                _ = completions.close(pa);
                return main_replacement;
            } else {
                comp.start_byte = new_start_byte;
            }

            main_replacement
        } else {
            drop(start_fn);
            comp.text = Text::default();
            comp.sidebar = Text::default();
            comp.current_entry = None;
            None
        };

        Completions::set_frame(pa, completions);

        main_replacement
    }

    #[track_caller]
    fn set_frame(pa: &mut Pass, completions: &Handle<Self>) {
        let sidebar = completions.read(pa).sidebar.clone();
        if let Some(area) = completions.area().write_as::<duat_term::Area>(pa) {
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
    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> TextMut<'_> {
        self.text.as_mut()
    }
}

impl Drop for Completions {
    fn drop(&mut self) {
        *OPENED_PARAM_COMPLETION.lock().unwrap() = None;
    }
}

/// A provider for word completions
pub trait CompletionsProvider: Send + Sized + 'static {
    /// An entry in this `CompletionsProvider`.
    ///
    /// This should typically be either some [`Copy`] type, or a
    /// cheaply cloneable reference, like [`Arc<T>`], or a struct made
    /// of cheaply cloneable references.
    ///
    /// [`Arc<T>`]: std::sync::Arc
    type Entry: Send + Clone;

    /// Wether this provider should be enabled when there is more
    /// than one selection.
    const ALLOW_WITH_MULTIPLE_SELECTIONS: bool;

    /// Get all completion entries based on the [start] and [prefix].
    ///
    /// The start here is the value returned by [`Self::get_start`],
    /// which is used to determine from where the completions widget
    /// should be spawned, and which part of the [`Text`] should be
    /// swapped on completions.
    ///
    /// The prefix is the initial, unmodified text that was between
    /// the start and the main cursor.
    ///
    /// This function will only ever be called if the user modifies
    /// the text by a means other than completion swapping, or if
    /// [`Completions::update_provider`] is called for `Self`.
    ///
    /// For example, if [`Self::get_start`] points to the beginning of
    /// a word, and you spawn a completions, like this:
    ///
    /// ```text
    ///           v- Cursor before this m
    ///           |
    /// This is some text that I'm writing down.
    ///         |
    ///         ^- Completions spawned before this s
    /// ```
    ///
    /// The start will be `8`, and prefix will be `"so"`.
    ///
    /// If the user _types_ something, say, adding a space after `so`:
    ///
    /// ```text
    ///            v- Cursor before this m
    ///            |
    /// This is so me text that I'm writing down.
    ///            |
    ///            ^- Completions spawned before this s
    /// ```
    ///
    /// Now, the start will be `11`, and the prefix will change to
    /// `""`. This is to reflect that typing should update the
    /// completion options.
    ///
    /// [start]: Point
    /// [prefix]: str
    fn matches(&mut self, text: &Text, start: Point, prefix: &str) -> Vec<Self::Entry>;

    /// Get the starting byte for this completions
    ///
    /// This function should look at the bytes before `from` (and
    /// sometimes after), in order to figure out where the completion
    /// starts.
    ///
    /// For example, if you're completing words, you should logially
    /// look for the start of the word that intersects with the
    /// `from`th byte:
    ///
    /// ```text
    ///         v------ starting byte index before b.
    /// This is being typed.
    ///             ^-- main cursor before g.
    /// ```
    ///
    /// If you're typing arguments in a command, you'd return the byte
    /// index of the current argument:
    ///
    /// ```text
    ///       v--------------------------- starting byte index before '.
    /// :edit 'This is a quoted argument
    ///                              ^---< main cursor before e.
    /// ```
    ///
    /// This function is used in order to determine which providers
    /// should be prioritized, giving higher priority to the ones that
    /// have longer matches.
    fn get_start(&self, text: &Text, cursor: Point) -> Option<usize>;

    /// The default formatting for entries from this provider
    ///
    /// Each [`Text`] must only be one line long (Nothing bad happens
    /// if they are multiple lines long, but don't expect the
    /// [`Completions`] to show things correctly).
    fn default_fmt(entry: &Self::Entry) -> Text;

    /// Function to pick a word to match on.
    ///
    /// This word is what will be placed over the current word.
    fn word(entry: &Self::Entry) -> &str;

    /// Additional information about an entry, which can be shown when
    /// it is selected.
    #[allow(unused_variables)]
    fn default_info_on(entry: &Self::Entry) -> Option<(Text, Orientation)> {
        None
    }
}

trait ErasedInnerProvider: Any + Send {
    #[allow(clippy::type_complexity)]
    #[track_caller]
    fn process(
        &mut self,
        text: &Text,
        scroll: i32,
        area: Option<&mut Area>,
        max_height: usize,
        min_prefix: usize,
    ) -> Provided;

    #[allow(clippy::type_complexity)]
    fn start_fn(&self) -> Box<dyn Fn(&Text, Point) -> usize + '_>;

    fn as_any(&mut self) -> &mut dyn Any;
}

#[allow(clippy::type_complexity)]
struct InnerProvider<P: CompletionsProvider> {
    provider: P,
    fmt: Box<dyn FnMut(&P::Entry) -> Text + Send>,

    start: usize,
    typed: String,
    current: Option<(String, (usize, usize))>,
    previous: String,
    matches: Vec<P::Entry>,
    has_changed: bool,
}

impl<P: CompletionsProvider> InnerProvider<P> {
    #[allow(clippy::type_complexity)]
    fn new(mut provider: P, text: &Text, height: usize, min_prefix: usize) -> (Self, Provided) {
        let Some(main_cursor) = text.get_main_sel().map(|sel| sel.cursor()) else {
            panic!("Tried to spawn completions on a Text with no main selection");
        };

        let start = provider
            .get_start(text, main_cursor)
            .unwrap_or(main_cursor.byte());

        let prefix = text[start..main_cursor.byte()].to_string();
        let matches = provider.matches(text, text.point_at_byte(start), &prefix);

        let mut inner = Self {
            provider,
            fmt: Box::new(P::default_fmt),
            start,
            typed: prefix.clone(),
            current: None,
            previous: prefix,
            matches,
            has_changed: false,
        };

        let provided = inner.process(text, 0, None, height, min_prefix);
        (inner, provided)
    }
}

impl<P: CompletionsProvider> ErasedInnerProvider for InnerProvider<P> {
    #[track_caller]
    fn process(
        &mut self,
        text: &Text,
        scroll: i32,
        area: Option<&mut Area>,
        max_height: usize,
        min_prefix: usize,
    ) -> Provided {
        let Some(cursor) = text.get_main_sel().map(|sel| sel.cursor()) else {
            panic!(
                "Tried to update completions on a Text with no main selection {}",
                std::panic::Location::caller()
            );
        };

        // This should only be true if edits other than the one applied by
        // Completions take place.
        let target_changed = text.get(self.start..cursor.byte()).is_none_or(|strs| {
            self.current.as_ref().is_some_and(|(s, _)| strs != *s)
                || (self.current.is_none() && strs != self.typed)
        });

        let (start, matches) = if !self.matches.is_empty() && !target_changed {
            let prefix: &str = self
                .current
                .as_ref()
                .map(|(s, _)| s.as_ref())
                .unwrap_or(&self.typed);

            self.previous = prefix.to_string();

            (cursor.byte() - prefix.len(), &self.matches)
        } else {
            let start = self
                .provider
                .get_start(text, cursor)
                .unwrap_or(cursor.byte());

            self.start = start;
            self.typed = text[start..cursor.byte()].to_string();
            self.current = None;
            self.previous = self.typed.clone();

            self.matches = self
                .provider
                .matches(text, text.point_at_byte(start), &self.typed);

            (cursor.byte() - self.typed.len(), &self.matches)
        };

        if
        //(!P::ALLOW_WITH_MULTIPLE_SELECTIONS && text.selections().len() > 1)
        matches.is_empty() || self.typed.chars().count() < min_prefix {
            self.current = None;
            return Provided { start, list: None };
        }

        let height = if let Some(area) = area {
            area.set_height(matches.len().min(max_height) as f32)
                .unwrap();
            area.height() as usize
        } else {
            max_height
        };

        let mut info = None;
        if scroll != 0 || self.has_changed {
            // No try blocks on stable Rust 🤮.
            let new_current = if let Some((prev_word, (dist, idx))) = &self.current {
                if let dist = dist.saturating_add_signed(scroll as isize).min(height - 1)
                    && let Some(prev_entry) = matches.get(*idx)
                    && P::word(prev_entry) == prev_word
                    && let Some(new_pos) = idx.checked_add_signed(scroll as isize)
                    && let Some(entry) = matches.get(new_pos)
                {
                    Some((entry, (dist, new_pos)))
                } else {
                    None
                }
            } else if scroll > 0 {
                if let scroll = scroll.unsigned_abs() as usize - 1
                    && let dist = (scroll).min(height - 1)
                    && let Some(entry) = matches.get(scroll)
                {
                    Some((entry, (dist, scroll)))
                } else {
                    None
                }
            } else if let scroll = scroll.unsigned_abs() as usize
                && let dist = height.saturating_sub(scroll)
                && let Some(new_pos) = matches.len().checked_sub(scroll)
                && let Some(entry) = matches.get(new_pos)
            {
                Some((entry, (dist, new_pos)))
            } else {
                None
            };
            let (entry, dist) = new_current.unzip();

            info = entry.as_ref().and_then(|entry| P::default_info_on(entry));

            self.current = entry
                .zip(dist)
                .map(|(entry, dist)| (P::word(entry).to_string(), dist));
        }

        let mut entries_builder = Text::builder();
        let mut sidebar_builder = Text::builder();

        if let Some((word, (dist, idx))) = &mut self.current
            && let Some(entry) = matches.get(*idx)
            && P::word(entry) == word
        {
            *dist = (*dist).min(height - 1);

            let top_i = idx.saturating_sub(*dist);
            for (i, entry) in matches.iter().enumerate().skip(top_i).take(height) {
                if i == *idx {
                    entries_builder.push(txt!("[selected.Completions]{}\n", (self.fmt)(entry)));
                    sidebar_builder.push(txt!("[selected.Completions] \n"));
                } else {
                    entries_builder.push(txt!("{}\n", (self.fmt)(entry)));
                    sidebar_builder.push(txt!("[default.Completions] \n"));
                }
            }
        } else {
            self.current = None;

            for entry in matches.iter().take(height) {
                entries_builder.push(txt!("{}\n", (self.fmt)(entry)));
                sidebar_builder.push(txt!("[default.Completions] \n"));
            }
        }

        let replacement = if scroll != 0 || self.has_changed {
            self.current
                .clone()
                .map(|(word, (_, index))| {
                    Replacement::FromList(
                        CompletionEntry {
                            index,
                            orig_range: self.start..self.start + self.typed.len(),
                            orig_typed: self.typed.clone(),
                            replacement: word,
                            entry: Box::new(matches[index].clone()),
                        },
                        info,
                    )
                })
                .or_else(|| Some(Replacement::WithOrig(self.typed.clone())))
        } else {
            None
        };

        self.has_changed = false;

        let entries_text = entries_builder.build();
        let sidebar_text = sidebar_builder.build();
        Provided {
            start,
            list: Some(List { entries_text, sidebar_text, replacement }),
        }
    }

    fn start_fn(&self) -> Box<dyn Fn(&Text, Point) -> usize + '_> {
        Box::new(|text, cursor| {
            if let Some(strs) = text.get(self.start..cursor.byte())
                && strs == self.previous
            {
                self.start
            } else {
                self.provider
                    .get_start(text, cursor)
                    .unwrap_or(cursor.byte())
            }
        })
    }

    fn as_any(&mut self) -> &mut dyn Any {
        self
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

const SPAWN_SPECS: DynSpawnSpecs = DynSpawnSpecs {
    orientation: Orientation::VerLeftBelow,
    height: Some(20.0),
    width: Some(50.0),
    hidden: true,
    inside: false,
};

type ProvidersFn =
    Box<dyn FnOnce(&Text, usize, usize) -> (Vec<Box<dyn ErasedInnerProvider>>, Provided) + Send>;
type ParamCompletions =
    Box<Mutex<dyn FnMut(&Pass, CompletionsBuilder) -> CompletionsBuilder + Send + Sync>>;
type BufferCompletionsFn = Box<dyn FnMut(&mut Pass) -> CompletionsBuilder + Send>;

#[derive(Debug)]
struct Provided {
    start: usize,
    list: Option<List>,
}

#[derive(Debug, Default)]
struct List {
    entries_text: Text,
    sidebar_text: Text,
    replacement: Option<Replacement>,
}

#[derive(Debug)]
enum Replacement {
    FromList(CompletionEntry, Option<(Text, Orientation)>),
    WithOrig(String),
}
