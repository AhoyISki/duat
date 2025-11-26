//! Multi modal for controlling the [`Prompt`] widget
//!
//! This mode's purpose is to do actions based on a [`PromptMode`]
//! implementor. `PromptMode` implementors take in the [`Text`] of the
//! [`Prompt`], and output some transformation to said `Text` (e.g.
//! formatting), while also doing actions given the global access
//! through the [`Pass`].
//!
//! Examples of [`PromptMode`]s are [`RunCommands`] and [`IncSearch`].
//! The former is used to run Duat's commands, while the latter
//! searches based on an input regex.
//!
//! `IncSearch` itself is _also_ multimodal, in an even more niche
//! sense. It takes in an [`IncSearcher`] implementor, and searches
//! through the [`Buffer`] according to its rules. Examples of this
//! are [`SearchFwd`] and [`SearchRev`], which take in the regex and
//! search in their respective directions. There are also more
//! "advanced" `IncSearcher`s, like the ones in the `duatmode` crate,
//! which can split a [`Selection`] by a regex, or keeps `Selections`s
//! that match, that sort of thing.
//!
//! [`SearchFwd`]: super::SearchFwd
//! [`SearchRev`]: super::SearchRev
//! [`Selection`]: duat_core::mode::Selection
use std::{
    any::TypeId,
    io::Write,
    sync::{Arc, LazyLock, Mutex, Once},
};

use duat_core::{
    buffer::Buffer,
    cmd,
    context::{self, Handle},
    data::Pass,
    form, hook,
    mode::{self, KeyEvent, event},
    text::{Ghost, Searcher, Tagger, Text, txt},
    ui::{PrintInfo, RwArea, Widget},
};

use super::IncSearcher;
use crate::{
    hooks::{SearchPerformed, SearchUpdated},
    widgets::PromptLine,
};

static HISTORY: Mutex<Vec<(TypeId, Vec<String>)>> = Mutex::new(Vec::new());
static PROMPT_TAGGER: LazyLock<Tagger> = LazyLock::new(Tagger::new);
static TAGGER: LazyLock<Tagger> = LazyLock::new(Tagger::new);
static PREVIEW_TAGGER: LazyLock<Tagger> = LazyLock::new(Tagger::new);

/// A [`Mode`] for the [`PromptLine`]
///
/// This mode abstracts over what the inner [`PromptMode`] actually
/// does, by letting them focus on just updating the [`Text`] and
/// acting on user input, instead of having to worry about which keys
/// do what, and when to update.
///
/// There are currently three [`PromptMode`]s:
///
/// - [`RunCommands`] is just your regular command runner, it can also
///   detect if your [`Parameter`]s are correct and show that in real
///   time.
/// - [`PipeSelections`] pipes each [`Selection`]'s selection in the
///   current [`Buffer`] to an external application, replacing each
///   selection with the returned value.
/// - [`IncSearch`] has a further inner abstraction, [`IncSearcher`],
///   which lets you abstract over what the incremental search will
///   actually do. I.c. will it search for the next ocurrence, split
///   selections by matches, things of the sort.
///
/// [`Parameter`]: cmd::Parameter
/// [`Selection`]: mode::Selection
/// [`Mode`]: duat_core::mode::Mode
pub struct Prompt {
    mode: Box<dyn PromptMode>,
    starting_text: String,
    ty: TypeId,
    clone_fn: Arc<Mutex<ModeCloneFn>>,
    reset_fn: fn(),
    history_index: Option<usize>,
}

impl Prompt {
    /// Returns a new [`Prompt`] from this [`PromptMode`]
    ///
    /// For convenience, you should make it so `new` methods in
    /// [`PromptMode`] implementors return a [`Prompt<Self>`],
    /// rather than the [`PromptMode`] itself.
    pub fn new<M: PromptMode + Clone>(mode: M) -> Self {
        let clone_fn = Arc::new(Mutex::new({
            let mode = mode.clone();
            move || -> Box<dyn PromptMode> { Box::new(mode.clone()) }
        }));

        Self {
            mode: Box::new(mode),
            starting_text: String::new(),
            ty: TypeId::of::<M>(),
            clone_fn,
            reset_fn: mode::reset::<M::ExitWidget>,
            history_index: None,
        }
    }

    /// Returns a new [`Prompt`] with some initial text
    ///
    /// This is useful if you wish to open this [`Mode`] with some
    /// text already in it.
    ///
    /// [`Mode`]: mode::Mode
    pub fn new_with<M: PromptMode + Clone>(mode: M, initial: impl ToString) -> Self {
        let clone_fn = Arc::new(Mutex::new({
            let mode = mode.clone();
            move || -> Box<dyn PromptMode> { Box::new(mode.clone()) }
        }));
        Self {
            mode: Box::new(mode),
            starting_text: initial.to_string(),
            ty: TypeId::of::<M>(),
            clone_fn,
            reset_fn: mode::reset::<M::ExitWidget>,
            history_index: None,
        }
    }

    /// Shows the preview [`Ghost`]
    fn show_preview(&mut self, pa: &mut Pass, handle: Handle<PromptLine>) {
        let history = HISTORY.lock().unwrap();
        if handle.text(pa).is_empty()
            && let Some((_, ty_history)) = history.iter().find(|(ty, _)| *ty == self.ty)
        {
            handle.text_mut(pa).insert_tag_after(
                *PREVIEW_TAGGER,
                0,
                Ghost::new(txt!("[prompt.preview]{}", ty_history.last().unwrap())),
            );
        }
    }
}

impl mode::Mode for Prompt {
    type Widget = PromptLine;

    fn bindings() -> mode::Bindings {
        mode::bindings!(match _ {
            _ => txt!("No key binding declarations, implement [function]Mode::bindings"),
        })
    }

    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<Self::Widget>) {
        use duat_core::mode::KeyCode::*;

        let ty_eq = |&&(ty, _): &&(TypeId, _)| ty == self.ty;

        let mut update = |pa: &mut Pass| {
            let text = std::mem::take(handle.write(pa).text_mut());
            let text = self.mode.update(pa, text, handle.area());
            *handle.write(pa).text_mut() = text;
        };

        let reset = |prompt: &mut Self| {
            if let Some(ret_handle) = prompt.mode.return_handle() {
                mode::reset_to(ret_handle);
            } else {
                (prompt.reset_fn)();
            }
        };

        handle.text_mut(pa).remove_tags(*PREVIEW_TAGGER, ..);

        match key {
            event!(Backspace) => {
                if handle.read(pa).text().is_empty() {
                    handle.write(pa).text_mut().selections_mut().clear();

                    update(pa);

                    if let Some(ret_handle) = self.mode.return_handle() {
                        mode::reset_to(ret_handle);
                    } else {
                        (self.reset_fn)();
                    }
                } else {
                    handle.edit_main(pa, |mut c| {
                        c.move_hor(-1);
                        c.set_anchor_if_needed();
                        c.replace("");
                        c.unset_anchor();
                    });
                    update(pa);
                }
            }
            event!(Delete) => {
                handle.edit_main(pa, |mut c| c.replace(""));
                update(pa);
            }

            event!(Char(char)) => {
                handle.edit_main(pa, |mut c| {
                    c.insert(char);
                    c.move_hor(1);
                });
                update(pa);
            }
            event!(Left) => {
                handle.edit_main(pa, |mut c| c.move_hor(-1));
                update(pa);
            }
            event!(Right) => {
                handle.edit_main(pa, |mut c| c.move_hor(1));
                update(pa);
            }
            event!(Up) => {
                let history = HISTORY.lock().unwrap();
                let Some((_, ty_history)) = history.iter().find(ty_eq) else {
                    return;
                };

                let index = if let Some(index) = &mut self.history_index {
                    *index = index.saturating_sub(1);
                    *index
                } else {
                    self.history_index = Some(ty_history.len() - 1);
                    ty_history.len() - 1
                };

                handle.edit_main(pa, |mut c| {
                    c.move_to(..);
                    c.replace(ty_history[index].clone());
                    c.unset_anchor();
                })
            }
            event!(Down) => {
                let history = HISTORY.lock().unwrap();
                let Some((_, ty_history)) = history.iter().find(ty_eq) else {
                    return;
                };

                if let Some(index) = &mut self.history_index {
                    if *index + 1 < ty_history.len() {
                        *index = (*index + 1).min(ty_history.len() - 1);

                        handle.edit_main(pa, |mut c| {
                            c.move_to(..);
                            c.replace(ty_history[*index].clone());
                            c.unset_anchor();
                        })
                    } else {
                        self.history_index = None;
                        handle.edit_main(pa, |mut c| {
                            c.move_to(..);
                            c.replace("");
                            c.unset_anchor();
                        })
                    }
                };
            }

            event!(Esc) => {
                handle.edit_main(pa, |mut c| {
                    c.move_to(..);
                    c.replace("");
                });
                handle.write(pa).text_mut().selections_mut().clear();
                update(pa);
                reset(self);
            }
            event!(Enter) => {
                handle.write(pa).text_mut().selections_mut().clear();

                if handle.text(pa).is_empty() {
                    let history = HISTORY.lock().unwrap();
                    if let Some((_, ty_history)) = history.iter().find(ty_eq) {
                        handle.edit_main(pa, |mut c| {
                            c.move_to(..);
                            c.replace(ty_history.last().unwrap());
                        });
                    }
                }

                update(pa);
                reset(self);
            }
            _ => {}
        }

        self.show_preview(pa, handle);
    }

    fn on_switch(&mut self, pa: &mut Pass, handle: Handle<Self::Widget>) {
        let text = {
            let pl = handle.write(pa);
            *pl.text_mut() = Text::with_default_main_selection();
            pl.text_mut().replace_range(0..0, &self.starting_text);

            let tag = Ghost::new(match pl.prompt_of_id(self.ty) {
                Some(text) => txt!("{text}[prompt.colon]:"),
                None => txt!("{}[prompt.colon]:", self.mode.prompt()),
            });
            pl.text_mut().insert_tag(*PROMPT_TAGGER, 0, tag);

            std::mem::take(pl.text_mut())
        };

        let text = self.mode.on_switch(pa, text, handle.area());

        *handle.write(pa).text_mut() = text;

        self.show_preview(pa, handle);
    }

    fn before_exit(&mut self, pa: &mut Pass, handle: Handle<Self::Widget>) {
        let text = std::mem::take(handle.write(pa).text_mut());
        if !text.is_empty() {
            let mut history = HISTORY.lock().unwrap();
            if let Some((_, ty_history)) = history.iter_mut().find(|(ty, _)| *ty == self.ty) {
                if ty_history.last().is_none_or(|last| last != text.bytes()) {
                    ty_history.push(text.to_string());
                }
            } else {
                history.push((self.ty, vec![text.to_string()]));
            }
        }

        self.mode.before_exit(pa, text, handle.area());
    }
}

impl Clone for Prompt {
    fn clone(&self) -> Self {
        Self {
            mode: self.clone_fn.lock().unwrap()(),
            starting_text: self.starting_text.clone(),
            ty: self.ty,
            clone_fn: self.clone_fn.clone(),
            reset_fn: self.reset_fn,
            history_index: None,
        }
    }
}

/// A mode to control the [`Prompt`]
///
/// Through the [`Pass`], one can act on the entirety of Duat's shared
/// state:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_base::modes::PromptMode;
/// use duat::prelude::*;
///
/// #[derive(Default, Clone)]
/// struct RealTimeSwitch {
///     initial: Option<String>,
///     current: Option<String>,
///     name_was_correct: bool,
/// };
///
/// impl PromptMode for RealTimeSwitch {
///     type ExitWidget = Buffer;
///
///     fn update(&mut self, pa: &mut Pass, text: Text, area: &ui::RwArea) -> Text {
///         let name = text.to_string();
///
///         self.name_was_correct = if name != *self.current.as_ref().unwrap() {
///             if cmd::buffer(pa, &name).is_ok() {
///                 self.current = Some(name);
///                 true
///             } else {
///                 false
///             }
///         } else {
///             true
///         };
///
///         text
///     }
///
///     fn on_switch(&mut self, pa: &mut Pass, text: Text, area: &ui::RwArea) -> Text {
///         self.initial = Some(context::current_buffer(pa).read(pa).name());
///         self.current = self.initial.clone();
///
///         text
///     }
///
///     fn before_exit(&mut self, pa: &mut Pass, text: Text, area: &ui::RwArea) {
///         if !self.name_was_correct {
///             cmd::buffer(pa, self.initial.take().unwrap());
///         }
///     }
///
///     fn prompt(&self) -> Text {
///         txt!("[prompt]switch to")
///     }
/// }
/// ```
///
/// The [`PromptMode`] above will switch to the buffer with the same
/// name as the one in the [`PromptLine`], returning to the initial
/// buffer if the match failed.
#[allow(unused_variables)]
pub trait PromptMode: Send + 'static {
    /// What [`Widget`] to exit to, upon pressing enter, esc, or
    /// backspace in an empty [`PromptLine`]
    ///
    /// Usually, this would be [`Buffer`]
    type ExitWidget: Widget
    where
        Self: Sized;

    /// Updates the [`PromptLine`] and [`Text`] of the [`Prompt`]
    ///
    /// This function is triggered every time the user presses a key
    /// in the [`Prompt`] mode.
    fn update(&mut self, pa: &mut Pass, text: Text, area: &RwArea) -> Text;

    /// What to do when switchin onto this [`PromptMode`]
    ///
    /// The initial [`Text`] is always empty, except for the [prompt]
    /// [`Ghost`] at the beginning of the line.
    ///
    /// [prompt]: PromptMode::prompt
    fn on_switch(&mut self, pa: &mut Pass, text: Text, area: &RwArea) -> Text {
        text
    }

    /// What to do before exiting the [`PromptMode`]
    ///
    /// This usually involves some sor of "commitment" to the result,
    /// e.g., [`RunCommands`] executes the call, [`IncSearch`]
    /// finishes the search, etc.
    fn before_exit(&mut self, pa: &mut Pass, text: Text, area: &RwArea) {}

    /// What text should be at the beginning of the [`PromptLine`], as
    /// a [`Ghost`]
    fn prompt(&self) -> Text;

    /// An optional returning [`Handle`] for the [`ExitWidget`]
    ///
    /// [`ExitWidget`]: PromptMode::ExitWidget
    fn return_handle(&self) -> Option<Handle<dyn Widget>> {
        None
    }
}

/// Runs Duat commands, with syntax highlighting for correct
/// [`Parameter`]s
///
/// [`Parameter`]: duat_core::cmd::Parameter
#[derive(Default, Clone)]
pub struct RunCommands;

impl RunCommands {
    /// Crates a new [`RunCommands`]
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> Prompt {
        Self::call_once();
        Prompt::new(Self)
    }

    /// Opens a [`RunCommands`] with some initial text
    pub fn new_with(initial: impl ToString) -> Prompt {
        Self::call_once();
        Prompt::new_with(Self, initial)
    }

    fn call_once() {
        static ONCE: Once = Once::new();
        ONCE.call_once(|| {
            form::set_weak("caller.info", "accent.info");
            form::set_weak("caller.error", "accent.error");
            form::set_weak("parameter.info", "default.info");
            form::set_weak("parameter.error", "default.error");
        });
    }
}

impl PromptMode for RunCommands {
    type ExitWidget = Buffer;

    fn update(&mut self, pa: &mut Pass, mut text: Text, _: &RwArea) -> Text {
        text.remove_tags(*TAGGER, ..);

        let command = text.to_string();
        let caller = command.split_whitespace().next();
        if let Some(caller) = caller {
            if let Some((ok_ranges, err_range)) = cmd::check_args(pa, &command) {
                let id = form::id_of!("caller.info");
                text.insert_tag(*TAGGER, 0..caller.len(), id.to_tag(0));

                let default_id = form::id_of!("parameter.info");
                for (range, id) in ok_ranges {
                    text.insert_tag(*TAGGER, range, id.unwrap_or(default_id).to_tag(0));
                }
                if let Some((range, _)) = err_range {
                    let id = form::id_of!("parameter.error");
                    text.insert_tag(*TAGGER, range, id.to_tag(0));
                }
            } else {
                let id = form::id_of!("caller.error");
                text.insert_tag(*TAGGER, 0..caller.len(), id.to_tag(0));
            }
        }

        text
    }

    fn before_exit(&mut self, _: &mut Pass, text: Text, _: &RwArea) {
        let call = text.to_string();
        if !call.is_empty() {
            cmd::queue_notify(call);
        }
    }

    fn prompt(&self) -> Text {
        Text::default()
    }
}

/// The [`PromptMode`] that makes use of [`IncSearcher`]s
///
/// In order to make use of incremental search, you'd do something
/// like this:
///
/// ```rust
/// # duat_core::doc_duat!(duat);
/// # use duat_base::modes::{IncSearch, SearchFwd};
/// use duat::prelude::*;
///
/// #[derive(Clone)]
/// struct Emacs;
///
/// impl Mode for Emacs {
///     type Widget = Buffer;
///
///     fn send_key(&mut self, pa: &mut Pass, event: KeyEvent, handle: Handle) {
///         match event {
///             ctrl!('s') => mode::set(IncSearch::new(SearchFwd)),
///             other_keys_oh_god => todo!(),
///         }
///     }
/// }
/// ```
///
/// This function returns a [`Prompt<IncSearch<SearchFwd>>`],
pub struct IncSearch<I: IncSearcher> {
    inc: I,
    orig: Option<(mode::Selections, PrintInfo)>,
    prev: String,
}

impl<I: IncSearcher> Clone for IncSearch<I> {
    fn clone(&self) -> Self {
        Self {
            inc: self.inc.clone(),
            orig: self.orig.clone(),
            prev: self.prev.clone(),
        }
    }
}

impl<I: IncSearcher> IncSearch<I> {
    /// Returns a [`Prompt`] with [`IncSearch<I>`] as its
    /// [`PromptMode`]
    #[allow(clippy::new_ret_no_self)]
    pub fn new(inc: I) -> Prompt {
        static ONCE: Once = Once::new();
        ONCE.call_once(|| {
            form::set_weak("regex.error", "accent.error");
            form::set_weak("regex.operator", "operator");
            form::set_weak("regex.class", "constant");
            form::set_weak("regex.bracket", "punctuation.bracket");
        });
        Prompt::new(Self { inc, orig: None, prev: String::new() })
    }
}

impl<I: IncSearcher> PromptMode for IncSearch<I> {
    type ExitWidget = Buffer;

    fn update(&mut self, pa: &mut Pass, mut text: Text, _: &RwArea) -> Text {
        let (orig_selections, orig_print_info) = self.orig.as_ref().unwrap();
        text.remove_tags(*TAGGER, ..);

        let handle = context::current_buffer(pa).clone();

        if text == self.prev {
            return text;
        } else {
            let prev = std::mem::replace(&mut self.prev, text.to_string());
            hook::queue(SearchUpdated((prev, self.prev.clone())));
        }

        match Searcher::new(text.to_string()) {
            Ok(searcher) => {
                handle.area().set_print_info(pa, orig_print_info.clone());
                let buffer = handle.write(pa);
                *buffer.selections_mut() = orig_selections.clone();

                let ast = regex_syntax::ast::parse::Parser::new()
                    .parse(&text.to_string())
                    .unwrap();

                crate::tag_from_ast(*TAGGER, &mut text, &ast);

                if !text.is_empty() {
                    self.inc.search(pa, handle.attach_searcher(searcher));
                }
            }
            Err(err) => {
                let regex_syntax::Error::Parse(err) = *err else {
                    unreachable!("As far as I can tell, regex_syntax has goofed up");
                };

                let span = err.span();
                let id = form::id_of!("regex.error");

                text.insert_tag(*TAGGER, span.start.offset..span.end.offset, id.to_tag(0));
            }
        }

        text
    }

    fn on_switch(&mut self, pa: &mut Pass, text: Text, _: &RwArea) -> Text {
        let handle = context::current_buffer(pa);

        self.orig = Some((
            handle.read(pa).selections().clone(),
            handle.area().get_print_info(pa),
        ));

        text
    }

    fn before_exit(&mut self, _: &mut Pass, text: Text, _: &RwArea) {
        if !text.is_empty() {
            if let Err(err) = Searcher::new(text.to_string()) {
                let regex_syntax::Error::Parse(err) = *err else {
                    unreachable!("As far as I can tell, regex_syntax has goofed up");
                };

                let range = err.span().start.offset..err.span().end.offset;
                let err = txt!(
                    "[a]{:?}, \"{}\"[prompt.colon]:[] {}",
                    range,
                    text.strs(range).unwrap(),
                    err.kind()
                );

                context::error!("{err}")
            } else {
                hook::queue(SearchPerformed(text.to_string()));
            }
        }
    }

    fn prompt(&self) -> Text {
        txt!("{}", self.inc.prompt())
    }
}

/// Pipes the selections of a [`Buffer`] through an external command
///
/// This can be useful if you, for example, don't have access to a
/// formatter, but want to format text, so you pass it to
/// [`PipeSelections`] with `fold` as the command, or things of the
/// sort.
#[derive(Clone, Copy)]
pub struct PipeSelections;

impl PipeSelections {
    /// Returns a [`Prompt`] with [`PipeSelections`] as its
    /// [`PromptMode`]
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> Prompt {
        Prompt::new(Self)
    }
}

impl PromptMode for PipeSelections {
    type ExitWidget = Buffer;

    fn update(&mut self, _: &mut Pass, mut text: Text, _: &RwArea) -> Text {
        fn is_in_path(program: &str) -> bool {
            if let Ok(path) = std::env::var("PATH") {
                for p in path.split(":") {
                    let p_str = format!("{p}/{program}");
                    if let Ok(true) = std::fs::exists(p_str) {
                        return true;
                    }
                }
            }
            false
        }

        text.remove_tags(*TAGGER, ..);

        let command = text.to_string();
        let Some(caller) = command.split_whitespace().next() else {
            return text;
        };

        let args = cmd::ArgsIter::new(&command);

        let (caller_id, args_id) = if is_in_path(caller) {
            (form::id_of!("caller.info"), form::id_of!("parameter.indo"))
        } else {
            (
                form::id_of!("caller.error"),
                form::id_of!("parameter.error"),
            )
        };

        let c_s = command.len() - command.trim_start().len();
        text.insert_tag(*TAGGER, c_s..c_s + caller.len(), caller_id.to_tag(0));

        for (_, range) in args {
            text.insert_tag(*TAGGER, range, args_id.to_tag(0));
        }

        text
    }

    fn before_exit(&mut self, pa: &mut Pass, text: Text, _: &RwArea) {
        use std::process::{Command, Stdio};

        let command = text.to_string();
        let Some(caller) = command.split_whitespace().next() else {
            return;
        };

        let handle = context::current_buffer(pa).clone();
        handle.edit_all(pa, |mut c| {
            let Ok(mut child) = Command::new(caller)
                .args(cmd::ArgsIter::new(&command).map(|(a, _)| a))
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
            else {
                return;
            };

            let input: String = c.selection().collect();
            if let Some(mut stdin) = child.stdin.take() {
                std::thread::spawn(move || {
                    stdin.write_all(input.as_bytes()).unwrap();
                });
            }
            if let Ok(out) = child.wait_with_output() {
                let out = String::from_utf8_lossy(&out.stdout);
                c.set_anchor_if_needed();
                c.replace(out);
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]pipe")
    }
}

type ModeCloneFn = dyn Fn() -> Box<dyn PromptMode> + Send;
