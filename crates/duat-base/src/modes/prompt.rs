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
//! [`IncSearch`]: crate::modes::IncSearch
//! [`IncSearcher`]: crate::modes::IncSearcher
use std::{
    any::TypeId,
    io::Write,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    Ns,
    buffer::Buffer,
    cmd,
    context::{self, Handle},
    data::Pass,
    form,
    hook::{self, FocusedUpdated, ModeSwitched},
    mode::{self, KeyEvent, event, shift},
    text::{Inlay, Text, txt},
    ui::RwArea,
};

use crate::widgets::{Completions, PromptLine};

static HISTORY: Mutex<Vec<(TypeId, Vec<String>)>> = Mutex::new(Vec::new());
static NS: LazyLock<Ns> = LazyLock::new(Ns::new);
static PROMPT_NS: LazyLock<Ns> = LazyLock::new(Ns::new);
static PREVIEW_NS: LazyLock<Ns> = LazyLock::new(Ns::new);
static HOOK_NS: LazyLock<Ns> = Ns::new_lazy();

/// Add the [`Prompt`] hook.
pub fn add_prompt_hook() {
    hook::add::<ModeSwitched>(|pa, mut switch| {
        if let Some(prompt) = switch.new.get_as::<Prompt>() {
            let Some(promptline) = context::handle_of::<PromptLine>(pa) else {
                return;
            };
            promptline.set_as_active(pa);

            let text = {
                let pl = promptline.write(pa);
                pl.text = Text::with_default_main_selection();
                pl.text_mut().replace_range(0..0, &prompt.starting_text);

                let tag = Inlay::new(match pl.prompt_of_id(prompt.ty) {
                    Some(text) => txt!("{text}[prompt.colon]:"),
                    None => txt!("{}[prompt.colon]:", prompt.mode.prompt()),
                });
                pl.text_mut().insert_tag(*PROMPT_NS, 0, tag);

                std::mem::take(&mut pl.text)
            };

            let text = prompt.mode.on_switch(pa, text, promptline.area());

            promptline.write(pa).text = text;

            prompt.show_preview(pa, promptline);
        } else if let Some(prompt) = switch.old.get_as::<Prompt>() {
            let Some(promptline) = context::handle_of::<PromptLine>(pa) else {
                return;
            };

            let text = std::mem::take(&mut promptline.write(pa).text);
            if !text.is_empty() {
                let mut history = HISTORY.lock().unwrap();
                if let Some((_, ty_history)) = history.iter_mut().find(|(ty, _)| *ty == prompt.ty) {
                    if ty_history.last().is_none_or(|last| last != &text) {
                        ty_history.push(text.to_string());
                    }
                } else {
                    history.push((prompt.ty, vec![text.to_string()]));
                }
            }

            prompt.mode.before_exit(pa, text, promptline.area());
        }
    });
}

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
///   actually do. I.s. will it search for the next ocurrence, split
///   selections by matches, things of the sort.
///
/// [`Parameter`]: cmd::Parameter
/// [`Selection`]: mode::Selection
/// [`Mode`]: duat_core::mode::Mode
/// [`IncSearch`]: crate::modes::IncSearch
/// [`IncSearcher`]: crate::modes::IncSearcher
pub struct Prompt {
    mode: Box<dyn PromptMode>,
    starting_text: String,
    ty: TypeId,
    history_index: Option<usize>,
}

impl Prompt {
    /// Returns a new [`Prompt`] from this [`PromptMode`]
    ///
    /// For convenience, you should make it so `new` methods in
    /// [`PromptMode`] implementors return a [`Prompt<Self>`],
    /// rather than the [`PromptMode`] itself.
    pub fn new<M: PromptMode>(mode: M) -> Self {
        Self {
            mode: Box::new(mode),
            starting_text: String::new(),
            ty: TypeId::of::<M>(),
            history_index: None,
        }
    }

    /// Returns a new [`Prompt`] with some initial text
    ///
    /// This is useful if you wish to open this [`Mode`] with some
    /// text already in it.
    ///
    /// [`Mode`]: mode::Mode
    pub fn new_with<M: PromptMode>(mode: M, initial: impl ToString) -> Self {
        Self {
            mode: Box::new(mode),
            starting_text: initial.to_string(),
            ty: TypeId::of::<M>(),
            history_index: None,
        }
    }

    /// Returns `true` if the [`PromptMode`] is `P`.
    pub fn is<P: PromptMode>(&self) -> bool {
        self.ty == TypeId::of::<P>()
    }

    /// Shows the preview [`Inlay`]
    fn show_preview(&mut self, pa: &mut Pass, handle: Handle<PromptLine>) {
        let history = HISTORY.lock().unwrap();
        if handle.text(pa).is_empty()
            && let Some((_, ty_history)) = history.iter().find(|(ty, _)| *ty == self.ty)
        {
            handle.text_mut(pa).insert_tag_after(
                *PREVIEW_NS,
                0,
                Inlay::new(txt!("[prompt.preview]{}", ty_history.last().unwrap())),
            );
        }
    }
}

impl mode::Mode for Prompt {
    fn bindings() -> mode::Bindings {
        use mode::KeyCode::*;

        mode::bindings!(match _ {
            event!(Char(..)) => txt!("Insert the character"),
            event!(Left | Right) => txt!("Move cursor"),
            event!(Down | Up) => txt!("Move through command history"),
            event!(Backspace | Delete) => txt!("Remove character or selection"),
            event!(Enter) => txt!("Run command and [mode]leave"),
            event!(Esc) => txt!("[mode]Leave[] without running command"),
        })
    }

    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent) {
        use duat_core::mode::KeyCode::*;

        let promptline = context::handle_of::<PromptLine>(pa).unwrap();

        let ty_eq = |&&(ty, _): &&(TypeId, _)| ty == self.ty;

        let mut update = |pa: &mut Pass| {
            let text = std::mem::take(&mut promptline.write(pa).text);
            let text = self.mode.update(pa, text, promptline.area());
            promptline.write(pa).text = text;
        };

        let reset = |pa: &mut Pass, prompt: &mut Self| {
            if let Some(ret_handle) = prompt.mode.return_handle() {
                mode::reset_to(pa, &ret_handle);
            } else {
                mode::reset::<Buffer>(pa);
            }
        };

        promptline.text_mut(pa).remove_tags(*PREVIEW_NS, ..);

        match key {
            event!(Char(char)) => {
                promptline.edit_main(pa, |mut s| {
                    s.insert(char);
                    s.move_hor(1);
                });
                update(pa);
            }

            event!(Backspace) => {
                if promptline.read(pa).text() == "\n" {
                    update(pa);
                    reset(pa, self);
                } else {
                    promptline.edit_main(pa, |mut s| {
                        if s.move_hor(-1) == -1 {
                            s.set_anchor_if_needed();
                            s.replace("");
                            s.unset_anchor();
                        }
                    });
                    update(pa);
                }
            }
            event!(Delete) => {
                promptline.edit_main(pa, |mut s| {
                    if s.char() != '\n' {
                        s.set_anchor_if_needed();
                        s.replace("");
                    }
                });
                update(pa);
            }

            event!(Left) => {
                promptline.edit_main(pa, |mut s| s.move_hor(-1));
                update(pa);
            }
            event!(Right) => {
                promptline.edit_main(pa, |mut s| s.move_hor(1));
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

                promptline.edit_main(pa, |mut s| {
                    s.move_to(..);
                    s.replace(ty_history[index].clone());
                    s.unset_anchor();
                });

                update(pa);
            }
            event!(Down) => {
                let history = HISTORY.lock().unwrap();
                let Some((_, ty_history)) = history.iter().find(ty_eq) else {
                    return;
                };

                if let Some(index) = &mut self.history_index {
                    if *index + 1 < ty_history.len() {
                        *index = (*index + 1).min(ty_history.len() - 1);

                        promptline.edit_main(pa, |mut s| {
                            s.move_to(..);
                            s.replace(ty_history[*index].clone());
                            s.unset_anchor();
                        })
                    } else {
                        self.history_index = None;
                        promptline.edit_main(pa, |mut s| {
                            s.move_to(..);
                            s.replace("");
                            s.unset_anchor();
                        })
                    }
                };

                update(pa);
            }

            event!(Tab) => {
                Completions::scroll(pa, 1);
                update(pa);
            }
            shift!(BackTab) => {
                Completions::scroll(pa, -1);
                update(pa);
            }

            event!(Esc) => {
                promptline.edit_main(pa, |mut s| {
                    s.move_to(..);
                    s.replace("");
                });

                reset(pa, self);
            }
            event!(Enter) => {
                if promptline.text(pa).is_empty() {
                    let history = HISTORY.lock().unwrap();
                    if let Some((_, ty_history)) = history.iter().find(ty_eq) {
                        promptline.edit_main(pa, |mut s| {
                            s.move_to(..);
                            s.replace(ty_history.last().unwrap());
                        });
                    }
                }

                update(pa);
                reset(pa, self);
            }
            _ => {}
        }

        self.show_preview(pa, promptline);
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
/// #[derive(Default)]
/// struct RealTimeSwitch {
///     initial: Option<String>,
///     current: Option<String>,
///     name_was_correct: bool,
/// };
///
/// impl PromptMode for RealTimeSwitch {
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
    /// Updates the [`PromptLine`] and [`Text`] of the [`Prompt`]
    ///
    /// This function is triggered every time the user presses a key
    /// in the [`Prompt`] mode.
    fn update(&mut self, pa: &mut Pass, text: Text, area: &RwArea) -> Text;

    /// What to do when switching onto this [`PromptMode`]
    ///
    /// The initial [`Text`] is always empty, except for the [prompt]
    /// [`Inlay`] at the beginning of the line.
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
    ///
    /// [`IncSearch`]: crate::modes::IncSearch
    fn before_exit(&mut self, pa: &mut Pass, text: Text, area: &RwArea) {}

    /// What text should be at the beginning of the [`PromptLine`], as
    /// a [`Inlay`]
    fn prompt(&self) -> Text;

    /// An optional returning [`Handle`] for the exiting widget.
    ///
    /// This widget will be focused on when this mode is finished.
    fn return_handle(&self) -> Option<Handle> {
        None
    }
}

/// Runs Duat commands, with syntax highlighting for correct
/// [`Parameter`]s
///
/// [`Parameter`]: duat_core::cmd::Parameter
#[non_exhaustive]
pub struct RunCommands;

impl RunCommands {
    /// Crates a new [`RunCommands`]
    #[allow(clippy::new_ret_no_self)]
    pub fn new() -> Prompt {
        Self::add_hooks();
        Prompt::new(Self)
    }

    /// Opens a [`RunCommands`] with some initial text
    pub fn new_with(initial: impl ToString) -> Prompt {
        Self::add_hooks();
        Prompt::new_with(Self, initial)
    }

    fn add_hooks() {
        let mut completions = None;

        hook::add::<FocusedUpdated<PromptLine>>(move |pa, promptline| {
            let text = promptline.text(pa);
            let Some(main) = text.get_main_sel() else {
                Completions::close(pa);
                return;
            };

            let is_parameter = text[..main.cursor()]
                .chars()
                .rev()
                .any(|char| char.is_whitespace());

            let new_completion = if is_parameter {
                let call = text[..main.cursor()].to_string();
                let Some(parameters) = cmd::last_parsed_parameters(pa, &call) else {
                    completions = None;
                    Completions::close(pa);
                    return;
                };

                Completion::Parameters(parameters)
            } else {
                Completion::Caller
            };

            if completions.as_ref() != Some(&new_completion) {
                let cmd_docs =
                    Vec::from_iter(duat_core::cmd::cmd_list(pa).into_iter().filter_map(|doc| {
                        match doc {
                            cmd::Description::Command(cmd_doc) => Some(cmd_doc),
                            cmd::Description::Alias(_) => None,
                        }
                    }));

                match &new_completion {
                    Completion::Caller => Completions::add_list(pa, cmd_docs, 0, 100, Ns::basic()),
                    Completion::Parameters(params) => Completions::open_for(pa, params),
                }
            }

            completions = Some(new_completion)
        })
        .grouped(*HOOK_NS)
        .lateness(usize::MAX);
    }
}

impl PromptMode for RunCommands {
    fn update(&mut self, pa: &mut Pass, mut text: Text, _: &RwArea) -> Text {
        text.remove_tags(*NS, ..);

        let command = text.to_string();
        let caller = command.split_whitespace().next();
        if let Some(caller) = caller {
            if let Some((ok_ranges, err_range)) = cmd::check_args(pa, &command) {
                let id = form::id_of!("caller.info");
                text.insert_tag(*NS, 0..caller.len(), id.to_tag(0));

                let default_id = form::id_of!("param.info");
                for (range, id) in ok_ranges {
                    text.insert_tag(*NS, range, id.unwrap_or(default_id).to_tag(0));
                }
                if let Some((range, _)) = err_range {
                    let id = form::id_of!("param.error");
                    text.insert_tag(*NS, range, id.to_tag(0));
                }
            } else {
                let id = form::id_of!("caller.error");
                text.insert_tag(*NS, 0..caller.len(), id.to_tag(0));
            }
        }

        text
    }

    fn before_exit(&mut self, pa: &mut Pass, text: Text, _: &RwArea) {
        Completions::close(pa);
        hook::remove(*HOOK_NS);

        let call = text.to_string_no_last_nl();
        if !call.is_empty() {
            _ = cmd::call_notify(pa, call);
        }
    }

    fn prompt(&self) -> Text {
        Text::default()
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

        text.remove_tags(*NS, ..);

        let command = text.to_string();
        let Some(caller) = command.split_whitespace().next() else {
            return text;
        };

        let args = cmd::ArgsIter::new(&command);

        let (caller_id, args_id) = if is_in_path(caller) {
            (form::id_of!("caller.info"), form::id_of!("param.info"))
        } else {
            (form::id_of!("caller.error"), form::id_of!("param.error"))
        };

        let c_s = command.len() - command.trim_start().len();
        text.insert_tag(*NS, c_s..c_s + caller.len(), caller_id.to_tag(0));

        for (_, range, _) in args {
            text.insert_tag(*NS, range, args_id.to_tag(0));
        }

        text
    }

    fn before_exit(&mut self, pa: &mut Pass, text: Text, _: &RwArea) {
        use std::process::{Command, Stdio};

        let command = text.to_string();
        let Some(caller) = command.split_whitespace().next() else {
            return;
        };

        let handle = context::current_buffer(pa);
        handle.edit_all(pa, |mut s| {
            let Ok(mut child) = Command::new(caller)
                .args(cmd::ArgsIter::new(&command).map(|(a, ..)| a))
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
            else {
                return;
            };

            let input = s.selection().to_string();
            if let Some(mut stdin) = child.stdin.take() {
                std::thread::spawn(move || {
                    stdin.write_all(input.as_bytes()).unwrap();
                });
            }
            if let Ok(out) = child.wait_with_output() {
                let out = String::from_utf8_lossy(&out.stdout);
                s.set_anchor_if_needed();
                s.replace(out);
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]pipe")
    }
}

#[derive(Debug, Clone, Eq)]
enum Completion {
    Caller,
    Parameters(Vec<TypeId>),
}

impl PartialEq for Completion {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Parameters(l0), Self::Parameters(r0)) => {
                l0.iter().all(|param| r0.contains(param))
                    && r0.iter().all(|param| l0.contains(param))
            }
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}
