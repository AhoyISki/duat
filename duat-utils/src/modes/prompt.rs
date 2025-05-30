use std::{io::Write, marker::PhantomData, sync::LazyLock};

use duat_core::{
    Lender, cmd, context, form,
    hooks::{self, SearchPerformed, SearchUpdated},
    mode::{Cursors, EditHelper, KeyCode, KeyEvent, key, self},
    text::{Key, Point, Searcher, Tag, Text, text},
    ui::{RawArea, Ui},
    widgets::Widget,
};

use crate::widgets::PromptLine;

use super::IncSearcher;

static PROMPT_KEY: LazyLock<Key> = LazyLock::new(Key::new);
static KEY: LazyLock<Key> = LazyLock::new(Key::new);

#[derive(Clone)]
pub struct Prompt<M: PromptMode<U>, U: Ui>(M, PhantomData<U>);

impl<M: PromptMode<U>, U: Ui> Prompt<M, U> {
    fn new(mode: M) -> Self {
        Self(mode, PhantomData)
    }
}

impl<M: PromptMode<U>, U: Ui> mode::Mode<U> for Prompt<M, U> {
    type Widget = PromptLine<U>;

    fn send_key(&mut self, key: KeyEvent, widget: &mut Self::Widget, area: &U::Area) {
        let mut helper = EditHelper::new(widget, area);

        match key {
            key!(KeyCode::Backspace) => {
                if helper.text().is_empty() {
                    helper.cursors_mut().clear();
                    self.0.update(helper.text_mut(), area);
                    self.0.before_exit(helper.text_mut(), area);
                    mode::reset();
                } else {
                    let mut e = helper.edit_main();
                    e.move_hor(-1);
                    e.replace("");
                    self.0.update(helper.text_mut(), area);
                }
            }
            key!(KeyCode::Delete) => {
                helper.edit_main().replace("");
                self.0.update(helper.text_mut(), area);
            }

            key!(KeyCode::Char(char)) => {
                let mut e = helper.edit_main();
                e.insert(char);
                e.move_hor(1);
                self.0.update(helper.text_mut(), area);
            }
            key!(KeyCode::Left) => {
                helper.edit_main().move_hor(-1);
                self.0.update(helper.text_mut(), area);
            }
            key!(KeyCode::Right) => {
                helper.edit_main().move_hor(1);
                self.0.update(helper.text_mut(), area);
            }

            key!(KeyCode::Esc) => {
                let p = helper.text().len();
                let mut e = helper.edit_main();
                e.move_to(Point::default());
                e.set_anchor();
                e.move_to(p);
                e.replace("");
                helper.cursors_mut().clear();
                self.0.update(helper.text_mut(), area);
                self.0.before_exit(helper.text_mut(), area);
                mode::reset();
            }
            key!(KeyCode::Enter) => {
                helper.cursors_mut().clear();
                self.0.update(helper.text_mut(), area);
                self.0.before_exit(helper.text_mut(), area);
                mode::reset();
            }
            _ => {}
        }
    }

    fn on_switch(&mut self, widget: &mut Self::Widget, area: &<U as Ui>::Area) {
        *widget.text_mut() = Text::new_with_cursors();
        run_once::<M, U>();

        let tag = Tag::ghost(0, match widget.prompt_of::<M>() {
            Some(text) => text,
            None => self.0.prompt(),
        });
        widget.text_mut().insert_tag(*PROMPT_KEY, tag);

        self.0.on_switch(widget.text_mut(), area);
    }
}

#[allow(unused_variables)]
pub trait PromptMode<U: Ui>: Clone + Send + 'static {
    fn update(&mut self, text: &mut Text, area: &U::Area) {}

    fn on_switch(&mut self, text: &mut Text, area: &U::Area) {}

    fn before_exit(&mut self, text: &mut Text, area: &U::Area) {}

    fn once() {}

    fn prompt(&self) -> Text;
}

#[derive(Default, Clone)]
pub struct RunCommands;

impl RunCommands {
    pub fn new<U: Ui>() -> Prompt<Self, U> {
        Prompt::new(Self)
    }
}

impl<U: Ui> PromptMode<U> for RunCommands {
    fn update(&mut self, text: &mut Text, _area: &U::Area) {
        text.remove_tags(.., *KEY);

        let command = text.to_string();
        let caller = command.split_whitespace().next();
        if let Some(caller) = caller {
            if let Some((ok_ranges, err_range)) = cmd::check_args(&command) {
                let id = form::id_of!("CallerExists");
                text.insert_tag(*KEY, Tag::form(0..caller.len(), id, 0));

                let id = form::id_of!("ParameterOk");
                for range in ok_ranges {
                    text.insert_tag(*KEY, Tag::form(range, id, 0));
                }
                if let Some((range, _)) = err_range {
                    let id = form::id_of!("ParameterErr");
                    text.insert_tag(*KEY, Tag::form(range, id, 0));
                }
            } else {
                let id = form::id_of!("CallerNotFound");
                text.insert_tag(*KEY, Tag::form(0..caller.len(), id, 0));
            }
        }
    }

    fn before_exit(&mut self, text: &mut Text, _area: &U::Area) {
        let text = std::mem::take(text);

        let command = text.to_string();
        if !command.is_empty() {
            duat_core::thread::spawn(move || cmd::run_notify(command));
        }
    }

    fn once() {
        form::set_weak("CallerExists", "AccentOk");
        form::set_weak("CallerNotFound", "AccentErr");
        form::set_weak("ParameterOk", "DefaultOk");
        form::set_weak("ParameterErr", "DefaultErr");
    }

    fn prompt(&self) -> Text {
        text!("[Prompt.colon]:")
    }
}

#[derive(Clone)]
pub struct IncSearch<I: IncSearcher<U>, U: Ui> {
    inc: I,
    orig: Option<(Cursors, <U::Area as RawArea>::PrintInfo)>,
    ghost: PhantomData<U>,
    prev: String,
}

impl<I: IncSearcher<U>, U: Ui> IncSearch<I, U> {
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
    fn update(&mut self, text: &mut Text, _area: &U::Area) {
        let orig = self.orig.as_ref().unwrap();
        text.remove_tags(.., *KEY);

        let mut ff = context::fixed_file::<U>().unwrap();

        match Searcher::new(text.to_string()) {
            Ok(searcher) => {
                let (mut file, area) = ff.write();
                self.inc.search(orig, &mut file, area, searcher);
            }
            Err(err) => {
                let regex_syntax::Error::Parse(err) = *err else {
                    unreachable!("As far as I can tell, regex_syntax has goofed up");
                };

                let span = err.span();
                let id = form::id_of!("ParseCommandErr");

                text.insert_tag(*KEY, Tag::form(span.start.offset..span.end.offset, id, 0));
            }
        }

        if *text != self.prev {
            let prev = std::mem::replace(&mut self.prev, text.to_string());
            hooks::trigger::<SearchUpdated>((prev, self.prev.clone()));
        }
    }

    fn before_exit(&mut self, text: &mut Text, _area: &<U as Ui>::Area) {
        if !text.is_empty() {
            hooks::trigger::<SearchPerformed>(text.to_string());
        }
    }

    fn on_switch(&mut self, _text: &mut Text, _area: &U::Area) {
        let mut ff = context::fixed_file::<U>().unwrap();
        let (file, area) = ff.read();
        self.orig = Some((file.cursors().clone(), area.print_info()));
    }

    fn once() {
        form::set("Regex.err", "DefaultErr");
    }

    fn prompt(&self) -> Text {
        self.inc.prompt()
    }
}

#[derive(Clone, Copy)]
pub struct PipeSelections<U>(PhantomData<U>);

impl<U: Ui> PipeSelections<U> {
    pub fn new() -> Prompt<Self, U> {
        Prompt::new(Self(PhantomData))
    }
}

impl<U: Ui> PromptMode<U> for PipeSelections<U> {
    fn update(&mut self, text: &mut Text, _area: &U::Area) {
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

        text.remove_tags(.., *KEY);

        let command = text.to_string();
        let Some(caller) = command.split_whitespace().next() else {
            return;
        };

        let args = cmd::args_iter(&command);

        let (caller_id, args_id) = if is_in_path(caller) {
            (form::id_of!("CallerExists"), form::id_of!("ParameterOk"))
        } else {
            (form::id_of!("CallerNotFound"), form::id_of!("ParameterErr"))
        };

        let c_s = command.len() - command.trim_start().len();
        text.insert_tag(*KEY, Tag::form(c_s..c_s + caller.len(), caller_id, 0));

        for (_, range) in args {
            text.insert_tag(*KEY, Tag::form(range, args_id, 0));
        }
    }

    fn before_exit(&mut self, text: &mut Text, _area: &U::Area) {
        use std::process::{Command, Stdio};
        let text = std::mem::take(text);

        let command = text.to_string();
        let Some(caller) = command.split_whitespace().next() else {
            return;
        };

        let mut ff = context::fixed_file::<U>().unwrap();
        let (mut file, area) = ff.write();
        let mut helper = EditHelper::new(&mut *file, area);
        helper.edit_iter().for_each(|mut e| {
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
                duat_core::thread::spawn(move || {
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
        text!("[Prompt]pipe[Prompt.colon]:")
    }
}

/// Runs the [`once`] function of widgets.
///
/// [`once`]: Widget::once
fn run_once<M: PromptMode<U>, U: Ui>() {
    use std::{any::TypeId, sync::LazyLock};

    use duat_core::data::RwData;
    static LIST: LazyLock<RwData<Vec<TypeId>>> = LazyLock::new(|| RwData::new(Vec::new()));

    let mut list = LIST.write();
    if !list.contains(&TypeId::of::<M>()) {
        M::once();
        list.push(TypeId::of::<M>());
    }
}
