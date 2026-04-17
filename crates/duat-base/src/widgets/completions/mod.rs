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
    sync::{Arc, LazyLock, Mutex},
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
    mode::MouseEventKind,
    text::{Point, Spawn, Strs, Text, TextMut, txt},
    ui::{Area, DynSpawnSpecs, Orientation, Side, Widget},
};
use duat_term::Frame;

use crate::widgets::Info;
pub use crate::widgets::completions::{
    commands::CommandsCompletions, lists::ExhaustiveCompletionsList, paths::PathCompletions,
    words::WordCompletions,
};

mod commands;
mod lists;
mod paths;
mod words;

static BUFFER_COMPLETIONS: Mutex<Option<Box<dyn FnMut() -> CompletionsBuilder + Send>>> =
    Mutex::new(None);
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
        builder.with_provider(paths::PathCompletions::new(true))
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

        let Some((providers, start_byte, entries)) =
            self.providers.map(|call| call(handle.text(pa), 20, self.min_prefix))
        else {
            return;
        };

        let (text, sidebar) = entries.unwrap_or_default();

        let completions = Completions {
            providers,
            text,
            sidebar,
            max_height: 20,
            start_byte,
            cur_min_prefix: self.min_prefix,
            min_prefix: self.min_prefix,
            last_cursor: main.cursor(),
            info_handle: None,
        };

        let mut text = handle.text_mut(pa);
        text.insert_tag(*NS, start_byte, Spawn::new(completions, SPAWN_SPECS));
    }

    /// Adds a new [`CompletionsProvider`] to be prioritized over
    /// earlier ones
    pub fn with_provider(mut self, provider: impl CompletionsProvider) -> Self {
        let prev = self.providers.take();

        self.providers = Some(Box::new(move |text, height, min_prefix| {
            let (inner, start_byte, entries) =
                InnerProvider::new(provider, text, height, min_prefix);

            let Some((mut providers, reserve_start_byte, reserve_entries)) =
                prev.map(|call| call(text, height, min_prefix))
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
    providers: Vec<Box<dyn ErasedInnerProvider>>,
    text: Text,
    sidebar: Text,
    max_height: usize,
    start_byte: usize,
    cur_min_prefix: usize,
    min_prefix: usize,
    last_cursor: Point,
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
    /// [`CompletionsBuilder::with_provider`].
    pub fn builder() -> CompletionsBuilder {
        CompletionsBuilder {
            providers: None,
            min_prefix: 0,
            cmd_min_prefix: 0,
        }
    }

    /// Set the default completions for `Buffers`.
    pub fn set_default(func: impl FnMut() -> CompletionsBuilder + Send + 'static) {
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
            func().open(pa);
        } else {
            Self::builder()
                .with_provider(WordCompletions)
                .with_provider(PathCompletions::new(false))
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

    #[track_caller]
    fn update_text_and_position(
        pa: &mut Pass,
        completions: &Handle<Self>,
        scroll: i32,
    ) -> Option<(String, String)> {
        let master_handle = completions.master(pa).unwrap();
        let (master, area, comp) = pa.write_many((
            master_handle.widget(),
            completions.area(),
            completions.widget(),
        ));

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
                        comp.cur_min_prefix,
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

        let main_replacement = if let Some((start_byte, ((text, sides), replacement))) = other {
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
                    .map(|(sel, _)| start_fn(master.text(), sel.cursor()))
                    .collect::<Vec<_>>()
                    .into_iter();

                drop(start_fn);
                let mut shift = 0;

                master_handle.edit_all(pa, |mut s| {
                    let start = (starts.next().unwrap() as i32 + shift) as usize;
                    shift += replacement.len() as i32 - (s.cursor().byte() as i32 - start as i32);

                    s.move_to(start..s.cursor().byte());

                    if s.is_main() {
                        main_replacement = Some((s.selection().to_string(), replacement.clone()));
                    }

                    s.replace(&replacement);
                    s.unset_anchor();
                    if !replacement.is_empty() {
                        s.move_hor(1);
                    }

                    if s.is_main() {
                        new_start_byte = start;
                    }
                });

                if let Some((info_text, orientation)) = info_text {
                    let info_handle = if let Some(info) = completions.read(pa).info_handle.clone() {
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
                            txt!("[terminal.frame.Info]┤{replacement}[terminal.frame.Info]├")
                        });
                        area.set_frame(frame);
                    }
                } else if let Some(prev) = completions.write(pa).info_handle.take() {
                    let _ = prev.close(pa);
                }
            } else {
                drop(start_fn);
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
    /// Additional information about a given entry in the completion
    /// list
    ///
    /// This information is supposed to be displayed alongside the
    /// entry itself, usually on the right side.
    type Info: Send;

    /// The default formatting for entries from this provider
    ///
    /// Each [`Text`] must only be one line long (Nothing bad happens
    /// if they are multiple lines long, but don't expect the
    /// [`Completions`] to show things correctly).
    fn default_fmt(entry: &str, info: &Self::Info) -> Text;

    /// Get all completions at a given [`Point`] in the [`Text`]
    ///
    /// This function should return a list of all possible matches,
    /// given the prefix and surrounding context. It should be sorted
    /// in an appropriate manner (e.g. by word proximity or
    /// frequency).
    ///
    /// The `cursor` is the position where the main selection's
    /// [cursor] lies, And the `prefix` and `suffix` are .
    ///
    /// If the returned [`Vec`] is empty, then the next provider will
    /// be selected to return a list of matches.
    ///
    /// This function is only called if the prefix changes, which
    /// would happen if the user types something, deletes something,
    /// or moves the cursor around.
    ///
    /// [cursor]: duat_core::mode::Selection::cursor
    fn matches(&mut self, text: &Text, cursor: Point, prefix: &str) -> Vec<(Arc<str>, Self::Info)>;

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
    ///         v------ starting byte index.
    /// This is bein|g typed.
    ///             ^-- main cursor.
    /// ```
    ///
    /// If you're typing arguments in a command, you'd return the byte
    /// index of the current argument:
    ///
    /// ```text
    ///       v--------------------------- starting byte index.
    /// :edit 'This is a quoted argum|ent
    ///                              ^---< main cursor.
    /// ```
    ///
    /// This function is used in order to determine which providers
    /// should be prioritized, giving higher priority to the ones that
    /// have longer matches.
    fn get_start(&self, text: &Text, cursor: Point) -> Option<usize>;

    /// Additional information about an entry, which can be shown when
    /// it is selected.
    #[allow(unused_variables)]
    fn default_info_on(&self, entry: &str, info: &Self::Info) -> Option<(Text, Orientation)> {
        None
    }
}

trait ErasedInnerProvider: Any + Send {
    #[allow(clippy::type_complexity)]
    #[track_caller]
    fn texts_and_match(
        &mut self,
        text: &Text,
        scroll: i32,
        area: Option<&mut Area>,
        max_height: usize,
        min_prefix: usize,
    ) -> (
        usize,
        Option<((Text, Text), Option<(String, Option<(Text, Orientation)>)>)>,
    );

    #[allow(clippy::type_complexity)]
    fn start_fn(&self) -> Box<dyn Fn(&Text, Point) -> usize + '_>;
}

#[allow(clippy::type_complexity)]
struct InnerProvider<P: CompletionsProvider> {
    provider: P,
    fmt: Box<dyn FnMut(&str, &P::Info) -> Text + Send>,

    orig_prefix: String,
    current: Option<(Arc<str>, usize)>,

    matches: Vec<(Arc<str>, P::Info)>,
}

impl<P: CompletionsProvider> InnerProvider<P> {
    #[allow(clippy::type_complexity)]
    fn new(
        mut provider: P,
        text: &Text,
        height: usize,
        min_prefix: usize,
    ) -> (Self, usize, Option<(Text, Text)>) {
        let Some(main_cursor) = text.get_main_sel().map(|sel| sel.cursor()) else {
            panic!("Tried to spawn completions on a Text with no main selection");
        };

        let start = provider
            .get_start(text, main_cursor)
            .unwrap_or(main_cursor.byte());

        let orig_prefix = text[start..main_cursor.byte()].to_string();
        let matches = provider.matches(text, main_cursor, &orig_prefix);

        let mut inner = Self {
            provider,
            orig_prefix,
            current: None,
            matches,
            fmt: Box::new(P::default_fmt),
        };

        let (start, text) = inner.texts_and_match(text, 0, None, height, min_prefix);
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
        min_prefix: usize,
    ) -> (
        usize,
        Option<((Text, Text), Option<(String, Option<(Text, Orientation)>)>)>,
    ) {
        let Some(cursor) = text.get_main_sel().map(|sel| sel.cursor()) else {
            panic!(
                "Tried to update completions on a Text with no main selection {}",
                std::panic::Location::caller()
            );
        };

        let start = self
            .provider
            .get_start(text, cursor)
            .map(|byte| text.point_at_byte(byte))
            .unwrap_or(cursor);

        let Some(prefix) = text.get(start.byte()..cursor.byte()).map(Strs::to_string) else {
            panic!(
                "Failed to get prefix from {:?}",
                start.byte()..cursor.byte()
            );
        };

        // This should only be true if edits other than the one applied by
        // Completions take place.
        let target_changed = self.current.as_ref().is_some_and(|(s, _)| **s != prefix)
            || (self.current.is_none() && self.orig_prefix != prefix);

        if target_changed {
            self.matches = self.provider.matches(text, cursor, &prefix);
            self.current = None;
            self.orig_prefix = prefix;
        }

        if self.matches.is_empty() || cursor.char() < start.char() + min_prefix {
            self.current = None;
            return (start.byte(), None);
        }

        let height = if let Some(area) = area {
            area.set_height(self.matches.len().min(max_height) as f32)
                .unwrap();
            area.height() as usize
        } else {
            max_height
        };

        let mut ret_info = None;
        if scroll != 0 {
            // No try blocks on stable Rust 🤮.
            self.current = (|| -> Option<(Arc<str>, usize)> {
                if let Some((prev, dist)) = &self.current {
                    let dist = dist.saturating_add_signed(scroll as isize).min(height - 1);
                    let prev_i = self.matches.iter().position(|(w, _)| w == prev)?;
                    let (word, info) = self
                        .matches
                        .get(prev_i.checked_add_signed(scroll as isize)?)?;

                    ret_info = Some(info);
                    Some((word.clone(), dist))
                } else if scroll > 0 {
                    let scroll = scroll.unsigned_abs() as usize - 1;
                    let dist = (scroll).min(height - 1);
                    let (word, info) = self.matches.get(scroll)?;

                    ret_info = Some(info);
                    Some((word.clone(), dist))
                } else {
                    let scroll = scroll.unsigned_abs() as usize;
                    let dist = height.saturating_sub(scroll);
                    let (word, info) = self.matches.get(self.matches.len().checked_sub(scroll)?)?;

                    ret_info = Some(info);
                    Some((word.clone(), dist))
                }
            })();
        }

        let mut entries_builder = Text::builder();
        let mut sidebar_builder = Text::builder();

        if let Some((word, dist)) = &mut self.current
            && let Some(word_i) = self.matches.iter().position(|(w, _)| w == word)
        {
            *dist = (*dist).min(height - 1);

            let top_i = word_i.saturating_sub(*dist);
            for (i, (entry, info)) in self.matches.iter().enumerate().skip(top_i).take(height) {
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
            for (entry, info) in self.matches.iter().take(height) {
                entries_builder.push(txt!("{}\n", (self.fmt)(entry, info)));
                sidebar_builder.push(txt!("[default.Completions] \n"));
            }
        }

        let replacement = if scroll != 0 {
            self.current
                .clone()
                .map(|(word, _)| {
                    let text = self.provider.default_info_on(&word, ret_info.unwrap());
                    (word.to_string(), text)
                })
                .or_else(|| Some((self.orig_prefix.clone(), None)))
        } else {
            None
        };

        let entries_text = entries_builder.build_no_double_nl();
        let sidebar_text = sidebar_builder.build_no_double_nl();
        (
            start.byte(),
            Some(((entries_text, sidebar_text), replacement)),
        )
    }

    fn start_fn(&self) -> Box<dyn Fn(&Text, Point) -> usize + '_> {
        Box::new(|text, cursor| {
            self.provider
                .get_start(text, cursor)
                .unwrap_or(cursor.byte())
        })
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

type ProvidersFn = Box<
    dyn FnOnce(
            &Text,
            usize,
            usize,
        ) -> (
            Vec<Box<dyn ErasedInnerProvider>>,
            usize,
            Option<(Text, Text)>,
        ) + Send,
>;
type ParamCompletions =
    Box<Mutex<dyn FnMut(&Pass, CompletionsBuilder) -> CompletionsBuilder + Send + Sync>>;
