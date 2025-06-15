use std::{io::Write, marker::PhantomData, sync::LazyLock};

use duat_core::{prelude::*, text::Searcher};
use regex_syntax::ast::Ast;

use super::IncSearcher;
use crate::{
    hooks::{SearchPerformed, SearchUpdated},
    widgets::PromptLine,
};

static PROMPT_TAGGER: LazyLock<Tagger> = LazyLock::new(Tagger::new);
static TAGGER: LazyLock<Tagger> = LazyLock::new(Tagger::new);

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
///   current [`File`] to an external application, replacing each
///   selection with the returned value.
/// - [`IncSearch`] has a further inner abstraction, [`IncSearcher`],
///   which lets you abstract over what the incremental search will
///   actually do. I.e. will it search for the next ocurrence, split
///   selections by matches, things of the sort.
///
/// [`Parameter`]: cmd::Parameter
/// [`Selection`]: mode::Selection
#[derive(Clone)]
pub struct Prompt<M: PromptMode<U>, U: Ui>(M, PhantomData<U>);

impl<M: PromptMode<U>, U: Ui> Prompt<M, U> {
    fn new(mode: M) -> Self {
        Self(mode, PhantomData)
    }
}

impl<M: PromptMode<U>, U: Ui> mode::Mode<U> for Prompt<M, U> {
    type Widget = PromptLine<U>;

    fn send_key(&mut self, pa: &mut Pass, key: KeyEvent, handle: Handle<Self::Widget, U>) {
        match key {
            key!(KeyCode::Backspace) => {
                if handle.read(pa, |pl, _| pl.text().is_empty()) {
                    handle.write_selections(pa, |c| c.clear());

                    let text = handle.take_text(pa);
                    let text = self.0.update(pa, text, handle.area());
                    handle.replace_text(pa, text);

                    mode::reset();
                } else {
                    handle.edit_main(pa, |mut e| {
                        e.move_hor(-1);
                        e.replace("");
                    });
                    let text = handle.take_text(pa);
                    let text = self.0.update(pa, text, handle.area());
                    handle.replace_text(pa, text);
                }
            }
            key!(KeyCode::Delete) => {
                handle.edit_main(pa, |mut e| e.replace(""));
                let text = handle.take_text(pa);
                let text = self.0.update(pa, text, handle.area());
                handle.replace_text(pa, text);
            }

            key!(KeyCode::Char(char)) => {
                handle.edit_main(pa, |mut e| {
                    e.insert(char);
                    e.move_hor(1);
                });
                let text = handle.take_text(pa);
                let text = self.0.update(pa, text, handle.area());
                handle.replace_text(pa, text);
            }
            key!(KeyCode::Left) => {
                handle.edit_main(pa, |mut e| e.move_hor(-1));
                let text = handle.take_text(pa);
                let text = self.0.update(pa, text, handle.area());
                handle.replace_text(pa, text);
            }
            key!(KeyCode::Right) => {
                handle.edit_main(pa, |mut e| e.move_hor(1));
                let text = handle.take_text(pa);
                let text = self.0.update(pa, text, handle.area());
                handle.replace_text(pa, text);
            }

            key!(KeyCode::Esc) => {
                let p = handle.read(pa, |wid, _| wid.text().len());
                handle.edit_main(pa, |mut e| {
                    e.move_to_start();
                    e.set_anchor();
                    e.move_to(p);
                    e.replace("");
                });
                handle.write_selections(pa, |c| c.clear());
                let text = handle.take_text(pa);
                let text = self.0.update(pa, text, handle.area());
                handle.replace_text(pa, text);

                mode::reset();
            }
            key!(KeyCode::Enter) => {
                handle.write_selections(pa, |c| c.clear());
                let text = handle.take_text(pa);
                let text = self.0.update(pa, text, handle.area());
                handle.replace_text(pa, text);

                mode::reset();
            }
            _ => {}
        }
    }

    fn on_switch(&mut self, pa: &mut Pass, handle: Handle<Self::Widget, U>) {
        let text = handle.write(pa, |wid, _| {
            *wid.text_mut() = Text::new_with_selections();
            run_once::<M, U>();

            let tag = Ghost(match wid.prompt_of::<M>() {
                Some(text) => text,
                None => self.0.prompt(),
            });
            wid.text_mut().insert_tag(*PROMPT_TAGGER, 0, tag);

            std::mem::take(wid.text_mut())
        });

        let text = self.0.on_switch(pa, text, handle.area());

        handle.widget().replace_text(pa, text);
    }

    fn before_exit(&mut self, pa: &mut Pass, handle: Handle<Self::Widget, U>) {
        let text = handle.take_text(pa);
        self.0.before_exit(pa, text, handle.area());
    }
}

/// A mode to control the [`Prompt`], by acting on its [`Text`] and
/// [`U::Area`]
///
/// Through the [`Pass`], one can act on the entirety of Duat's shared
/// state:
///
/// ```rust
/// use duat_core::prelude::*;
/// use duat_utils::modes::PromptMode;
///
/// #[derive(Default, Clone)]
/// struct RealTimeSwitch {
///     initial: Option<String>,
///     current: Option<String>,
///     name_was_correct: bool,
/// };
///
/// impl<U: Ui> PromptMode<U> for RealTimeSwitch {
///     fn update(&mut self, pa: &mut Pass, text: Text, area: &U::Area) -> Text {
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
///     fn on_switch(&mut self, pa: &mut Pass, text: Text, area: &U::Area) -> Text {
///         self.initial = Some(context::fixed_file::<U>(pa).unwrap().name(pa));
///         self.current = self.initial.clone();
///
///         text
///     }
///
///     fn before_exit(&mut self, pa: &mut Pass, text: Text, area: &U::Area) {
///         if !self.name_was_correct {
///             cmd::buffer(pa, self.initial.take().unwrap());
///         }
///     }
///
///     fn prompt(&self) -> Text {
///         txt!("[prompt]switch to").build()
///     }
/// }
/// ```
///
/// The [`PromptMode`] above will switch to the file with the same
/// name as the one in the [`PromptLine`], returning to the initial
/// file if the match failed.
///
/// [`U::Area`]: Ui::Area
#[allow(unused_variables)]
pub trait PromptMode<U: Ui>: Clone + 'static {
    /// Updates the [`PromptLine`] and [`Text`] of the [`Prompt`]
    ///
    /// This function is triggered every time the user presses a key
    /// in the [`Prompt`] mode.
    fn update(&mut self, pa: &mut Pass, text: Text, area: &U::Area) -> Text;

    /// What to do when switchin onto this [`PromptMode`]
    ///
    /// The initial [`Text`] is always empty, except for the [prompt]
    /// [`Ghost`] at the beginning of the line.
    ///
    /// [prompt]: PromptMode::prompt
    fn on_switch(&mut self, pa: &mut Pass, text: Text, area: &U::Area) -> Text {
        text
    }

    /// What to do before exiting the [`PromptMode`]
    ///
    /// This usually involves some sor of "commitment" to the result,
    /// e.g., [`RunCommands`] executes the call, [`IncSearch`]
    /// finishes the search, etc.
    fn before_exit(&mut self, pa: &mut Pass, text: Text, area: &U::Area) {}

    /// Things to do when this [`PromptMode`] is first instantiated
    fn once() {}

    /// What text should be at the beginning of the [`PromptLine`], as
    /// a [`Ghost`]
    fn prompt(&self) -> Text;
}

/// Runs Duat commands, with syntax highlighting for correct
/// [`Parameter`]s
///
/// [`Parameter`]: duat_core::cmd::Parameter
#[derive(Default, Clone)]
pub struct RunCommands;

impl RunCommands {
    /// Crates a new [`RunCommands`]
    pub fn new<U: Ui>() -> Prompt<Self, U> {
        Prompt::new(Self)
    }
}

impl<U: Ui> PromptMode<U> for RunCommands {
    fn update(&mut self, pa: &mut Pass, mut text: Text, _: &<U as Ui>::Area) -> Text {
        text.remove_tags(*TAGGER, ..);

        let command = text.to_string();
        let caller = command.split_whitespace().next();
        if let Some(caller) = caller {
            if let Some((ok_ranges, err_range)) = cmd::check_args(pa, &command) {
                let id = form::id_of!("caller.info");
                text.insert_tag(*TAGGER, 0..caller.len(), id.to_tag(0));

                let id = form::id_of!("parameter.info");
                for range in ok_ranges {
                    text.insert_tag(*TAGGER, range, id.to_tag(0));
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

    fn before_exit(&mut self, _: &mut Pass, text: Text, _: &<U as Ui>::Area) {
        let call = text.to_string();
        if !call.is_empty() {
            cmd::queue_notify(call);
        }
    }

    fn once() {
        form::set_weak("caller.info", "accent.info");
        form::set_weak("caller.error", "accent.error");
        form::set_weak("parameter.info", "aefault.info");
        form::set_weak("parameter.error", "default.error");
    }

    fn prompt(&self) -> Text {
        txt!("[prompt.colon]:").build()
    }
}

/// The [`PromptMode`] that makes use of [`IncSearcher`]s
///
/// In order to make use of incremental search, you'd do something
/// like this:
///
/// ```rust
/// use duat_core::prelude::*;
/// use duat_utils::modes::{IncSearch, Regular, SearchFwd};
///
/// fn setup_generic_over_ui<U: Ui>() {
///     mode::map::<Regular, U>("<C-s>", IncSearch::new(SearchFwd));
/// }
/// ```
///
/// This function returns a [`Prompt<IncSearch<SearchFwd, U>, U>`],
#[derive(Clone)]
pub struct IncSearch<I: IncSearcher<U>, U: Ui> {
    inc: I,
    orig: Option<(mode::Selections, <U::Area as RawArea>::PrintInfo)>,
    ghost: PhantomData<U>,
    prev: String,
}

impl<I: IncSearcher<U>, U: Ui> IncSearch<I, U> {
    /// Returns a [`Prompt`] with [`IncSearch<I, U>`] as its
    /// [`PromptMode`]
    pub fn new(inc: I) -> Prompt<Self, U> {
        Prompt::new(Self {
            inc,
            orig: None,
            ghost: PhantomData,
            prev: String::new(),
        })
    }
}

impl<I: IncSearcher<U>, U: Ui> PromptMode<U> for IncSearch<I, U> {
    fn update(&mut self, pa: &mut Pass, mut text: Text, _: &<U as Ui>::Area) -> Text {
        let (orig_selections, orig_print_info) = self.orig.as_ref().unwrap();
        text.remove_tags(*TAGGER, ..);

        let handle = context::fixed_file::<U>(pa).unwrap().handle(pa);

        if text == self.prev {
            return text;
        } else {
            let prev = std::mem::replace(&mut self.prev, text.to_string());
            hook::queue(SearchUpdated((prev, self.prev.clone())));
        }

        match Searcher::new(text.to_string()) {
            Ok(searcher) => {
                handle.write(pa, |file, area| {
                    area.set_print_info(orig_print_info.clone());
                    *file.selections_mut().unwrap() = orig_selections.clone();
                });

                let ast = regex_syntax::ast::parse::Parser::new()
                    .parse(&text.to_string())
                    .unwrap();

                tag_from_ast(*TAGGER, &mut text, &ast);

                self.inc.search(pa, handle.attach_searcher(searcher));
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

    fn on_switch(&mut self, pa: &mut Pass, text: Text, _: &<U as Ui>::Area) -> Text {
        let handle = context::fixed_file::<U>(pa).unwrap();
        handle.read(pa, |file, area| {
            self.orig = Some((file.selections().clone(), area.print_info()));
        });

        text
    }

    fn before_exit(&mut self, _: &mut Pass, text: Text, _: &<U as Ui>::Area) {
        if !text.is_empty() {
            if let Err(err) = Searcher::new(text.to_string()) {
                let regex_syntax::Error::Parse(err) = *err else {
                    unreachable!("As far as I can tell, regex_syntax has goofed up");
                };

                let range = err.span().start.offset..err.span().end.offset;
                let err = txt!(
                    "[a]{:?}, \"{}\"[prompt.colon]:[] {}",
                    range,
                    text.strs(range),
                    err.kind()
                );

                context::error!(target: self.inc.prompt().to_string(), "{err}")
            } else {
                hook::queue(SearchPerformed(text.to_string()));
            }
        }
    }

    fn once() {
        form::set_weak("regex.error", "accent.error");
        form::set_weak("regex.operator", "operator");
        form::set_weak("regex.class", "constant");
        form::set_weak("regex.bracket", "punctuation.bracket");
    }

    fn prompt(&self) -> Text {
        txt!("{}[prompt.colon]:", self.inc.prompt()).build()
    }
}

/// Pipes the selections of a [`File`] through an external command
///
/// This can be useful if you, for example, don't have access to a
/// formatter, but want to format text, so you pass it to
/// [`PipeSelections`] with `fold` as the command, or things of the
/// sort.
#[derive(Clone, Copy)]
pub struct PipeSelections<U>(PhantomData<U>);

impl<U: Ui> PipeSelections<U> {
    /// Returns a [`Prompt`] with [`PipeSelections`] as its
    /// [`PromptMode`]
    pub fn new() -> Prompt<Self, U> {
        Prompt::new(Self(PhantomData))
    }
}

impl<U: Ui> PromptMode<U> for PipeSelections<U> {
    fn update(&mut self, _: &mut Pass, mut text: Text, _: &<U as Ui>::Area) -> Text {
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

        let args = cmd::args_iter(&command);

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

    fn before_exit(&mut self, pa: &mut Pass, text: Text, _: &<U as Ui>::Area) {
        use std::process::{Command, Stdio};

        let command = text.to_string();
        let Some(caller) = command.split_whitespace().next() else {
            return;
        };

        let handle = context::fixed_file::<U>(pa).unwrap().handle(pa);
        handle.edit_all(pa, |mut e| {
            let Ok(mut child) = Command::new(caller)
                .args(cmd::args_iter(&command).map(|(a, _)| a))
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
            else {
                return;
            };

            let input: String = e.selection().collect();
            if let Some(mut stdin) = child.stdin.take() {
                std::thread::spawn(move || {
                    stdin.write_all(input.as_bytes()).unwrap();
                });
            }
            if let Ok(out) = child.wait_with_output() {
                let out = String::from_utf8_lossy(&out.stdout);
                e.replace(out);
            }
        });
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]pipe").build()
    }
}

/// Runs the [`once`] function of widgets.
///
/// [`once`]: Widget::once
fn run_once<M: PromptMode<U>, U: Ui>() {
    use std::{any::TypeId, sync::Mutex};

    static LIST: LazyLock<Mutex<Vec<TypeId>>> = LazyLock::new(|| Mutex::new(Vec::new()));

    let mut list = LIST.lock().unwrap();
    if !list.contains(&TypeId::of::<M>()) {
        M::once();
        list.push(TypeId::of::<M>());
    }
}

fn tag_from_ast(tagger: Tagger, text: &mut Text, ast: &Ast) {
    use duat_core::form::FormId;
    use regex_syntax::ast::{Ast::*, Span};

    let mut insert_form = |id: FormId, span: Span| {
        text.insert_tag(tagger, span.start.offset..span.end.offset, id.to_tag(0));
    };

    match ast {
        Empty(_) => {}
        Flags(set_flags) => {
            let id = form::id_of!("Regex.operator.flags");
            insert_form(id, set_flags.span);
        }
        Literal(literal) => {
            let id = form::id_of!("Regex.literal");
            insert_form(id, literal.span);
        }
        Dot(span) => {
            let id = form::id_of!("Regex.operator.dot");
            insert_form(id, **span);
        }
        Assertion(assertion) => {
            let id = form::id_of!("Regex.operator.assertion");
            insert_form(id, assertion.span);
        }
        ClassUnicode(class) => {
            let id = form::id_of!("Regex.class.unicode");
            insert_form(id, class.span);
        }
        ClassPerl(class) => {
            let id = form::id_of!("Regex.class.perl");
            insert_form(id, class.span);
        }
        ClassBracketed(class) => {
            let class_id = form::id_of!("Regex.class.bracketed");
            let bracket_id = form::id_of!("Regex.bracket.class");

            insert_form(class_id, *class.kind.span());

            let range = class.span.start.offset..class.span.start.offset + 1;
            text.insert_tag(tagger, range, bracket_id.to_tag(0));
            let range = class.span.end.offset - 1..class.span.end.offset;
            text.insert_tag(tagger, range, bracket_id.to_tag(0));
        }
        Repetition(repetition) => {
            let id = form::id_of!("Regex.operator.repetition");
            insert_form(id, repetition.op.span);
        }
        Group(group) => {
            let group_id = form::id_of!("Regex.group");
            let bracket_id = form::id_of!("Regex.bracket.group");

            insert_form(group_id, *group.ast.span());

            let range = group.span.start.offset..group.span.start.offset + 1;
            text.insert_tag(tagger, range, bracket_id.to_tag(0));
            let range = group.span.end.offset - 1..group.span.end.offset;
            text.insert_tag(tagger, range, bracket_id.to_tag(0));

            tag_from_ast(tagger, text, &group.ast);
        }
        Alternation(alternation) => {
            let id = form::id_of!("Regex.operator.alternation");

            let mut prev_end = None;

            for ast in alternation.asts.iter() {
                tag_from_ast(tagger, text, ast);

                if let Some(end) = prev_end {
                    let range = end..ast.span().start.offset;
                    text.insert_tag(tagger, range, id.to_tag(0));
                }

                prev_end = Some(ast.span().end.offset);
            }
        }
        Concat(concat) => {
            for ast in concat.asts.iter() {
                tag_from_ast(tagger, text, ast);
            }
        }
    }
}
