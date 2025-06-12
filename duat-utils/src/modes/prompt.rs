use std::{io::Write, marker::PhantomData, sync::LazyLock};

use duat_core::{
    cmd, context,
    data::{Pass, RwData},
    form,
    hook::{self, SearchPerformed, SearchUpdated},
    mode::{self, Cursors, EditHelper, KeyCode, KeyEvent, key},
    text::{Ghost, Key, Point, Searcher, Text, text},
    ui::{RawArea, Ui},
    widget::Widget,
};

use super::IncSearcher;
use crate::widgets::PromptLine;

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

    fn send_key(
        &mut self,
        mut pa: Pass,
        key: KeyEvent,
        widget: RwData<Self::Widget>,
        area: U::Area,
    ) {
        let mut helper = EditHelper::new(&mut pa, widget, area.clone());

        match key {
            key!(KeyCode::Backspace) => {
                if helper.read(&pa, |pl| pl.text().is_empty()) {
                    helper.write_cursors(&mut pa, |c| c.clear());

                    let text = helper.take_text(&mut pa);
                    let text = self.0.update(&mut pa, text, &area);
                    let text = self.0.before_exit(&mut pa, text, &area);

                    helper.replace_text(&mut pa, text);

                    mode::reset();
                } else {
                    helper.edit_main(&mut pa, |mut e| {
                        e.move_hor(-1);
                        e.replace("");
                    });
                    let text = helper.take_text(&mut pa);
                    let text = self.0.update(&mut pa, text, &area);
                    helper.replace_text(&mut pa, text);
                }
            }
            key!(KeyCode::Delete) => {
                helper.edit_main(&mut pa, |mut e| e.replace(""));
                let text = helper.take_text(&mut pa);
                let text = self.0.update(&mut pa, text, &area);
                helper.replace_text(&mut pa, text);
            }

            key!(KeyCode::Char(char)) => {
                helper.edit_main(&mut pa, |mut e| {
                    e.insert(char);
                    e.move_hor(1);
                });
                let text = helper.take_text(&mut pa);
                let text = self.0.update(&mut pa, text, &area);
                helper.replace_text(&mut pa, text);
            }
            key!(KeyCode::Left) => {
                helper.edit_main(&mut pa, |mut e| e.move_hor(-1));
                let text = helper.take_text(&mut pa);
                let text = self.0.update(&mut pa, text, &area);
                helper.replace_text(&mut pa, text);
            }
            key!(KeyCode::Right) => {
                helper.edit_main(&mut pa, |mut e| e.move_hor(1));
                let text = helper.take_text(&mut pa);
                let text = self.0.update(&mut pa, text, &area);
                helper.replace_text(&mut pa, text);
            }

            key!(KeyCode::Esc) => {
                let p = helper.read(&pa, |wid| wid.text().len());
                helper.edit_main(&mut pa, |mut e| {
                    e.move_to(Point::default());
                    e.set_anchor();
                    e.move_to(p);
                    e.replace("");
                });
                helper.write_cursors(&mut pa, |c| c.clear());
                let text = helper.take_text(&mut pa);
                let text = self.0.update(&mut pa, text, &area);
                let text = self.0.before_exit(&mut pa, text, &area);
                helper.replace_text(&mut pa, text);
                mode::reset();
            }
            key!(KeyCode::Enter) => {
                helper.write_cursors(&mut pa, |c| c.clear());
                let text = helper.take_text(&mut pa);
                let text = self.0.update(&mut pa, text, &area);
                let text = self.0.before_exit(&mut pa, text, &area);
                helper.replace_text(&mut pa, text);
                mode::reset();
            }
            _ => {}
        }
    }

    fn on_switch(&mut self, mut pa: Pass, widget: RwData<Self::Widget>, area: <U as Ui>::Area) {
        let text = widget.write(&mut pa, |wid| {
            *wid.text_mut() = Text::new_with_cursors();
            run_once::<M, U>();

            let tag = Ghost(match wid.prompt_of::<M>() {
                Some(text) => text,
                None => self.0.prompt(),
            });
            wid.text_mut().insert_tag(*PROMPT_KEY, 0, tag);

            std::mem::take(wid.text_mut())
        });

        let text = self.0.on_switch(&mut pa, text, &area);

        widget.write(&mut pa, |wid| *wid.text_mut() = text);
    }
}

#[allow(unused_variables)]
pub trait PromptMode<U: Ui>: Clone + 'static {
    fn update(&mut self, pa: &mut Pass, text: Text, area: &U::Area) -> Text;

    fn on_switch(&mut self, pa: &mut Pass, text: Text, area: &U::Area) -> Text {
        text
    }

    fn before_exit(&mut self, pa: &mut Pass, text: Text, area: &U::Area) -> Text {
        text
    }

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
    fn update(&mut self, _: &mut Pass, mut text: Text, _: &<U as Ui>::Area) -> Text {
        text.remove_tags(*KEY, ..);

        let command = text.to_string();
        let caller = command.split_whitespace().next();
        if let Some(caller) = caller {
            if let Some((ok_ranges, err_range)) = cmd::check_args(&command) {
                let id = form::id_of!("CallerExists");
                text.insert_tag(*KEY, 0..caller.len(), id.to_tag(0));

                let id = form::id_of!("ParameterOk");
                for range in ok_ranges {
                    text.insert_tag(*KEY, range, id.to_tag(0));
                }
                if let Some((range, _)) = err_range {
                    let id = form::id_of!("ParameterErr");
                    text.insert_tag(*KEY, range, id.to_tag(0));
                }
            } else {
                let id = form::id_of!("CallerNotFound");
                text.insert_tag(*KEY, 0..caller.len(), id.to_tag(0));
            }
        }

        text
    }

    fn before_exit(&mut self, _: &mut Pass, text: Text, _: &<U as Ui>::Area) -> Text {
        let call = text.to_string();
        if !call.is_empty() {
            cmd::queue_notify(call);
        }

        Text::default()
    }

    fn once() {
        form::set_weak("CallerExists", "AccentOk");
        form::set_weak("CallerNotFound", "AccentErr");
        form::set_weak("ParameterOk", "DefaultOk");
        form::set_weak("ParameterErr", "DefaultErr");
    }

    fn prompt(&self) -> Text {
        text!("[Prompt.colon]:").build()
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
    fn update(&mut self, pa: &mut Pass, mut text: Text, _: &<U as Ui>::Area) -> Text {
        let (orig_cursors, orig_print_info) = self.orig.as_ref().unwrap();
        text.remove_tags(*KEY, ..);

        let handle = context::fixed_file::<U>(&*pa).unwrap();

        match Searcher::new(text.to_string()) {
            Ok(searcher) => {
                handle.write(&mut *pa, |file, area| {
                    area.set_print_info(orig_print_info.clone());
                    *file.cursors_mut().unwrap() = orig_cursors.clone();
                });

                self.inc.search(pa, handle, searcher);
            }
            Err(err) => {
                let regex_syntax::Error::Parse(err) = *err else {
                    unreachable!("As far as I can tell, regex_syntax has goofed up");
                };

                let span = err.span();
                let id = form::id_of!("ParseCommandErr");

                text.insert_tag(*KEY, span.start.offset..span.end.offset, id.to_tag(0));
            }
        }

        if text != self.prev {
            let prev = std::mem::replace(&mut self.prev, text.to_string());
            hook::queue::<SearchUpdated>((prev, self.prev.clone()));
        }

        text
    }

    fn before_exit(&mut self, _: &mut Pass, text: Text, _: &<U as Ui>::Area) -> Text {
        if !text.is_empty() {
            hook::queue::<SearchPerformed>(text.to_string());
        }

        text
    }

    fn on_switch(&mut self, pa: &mut Pass, text: Text, _: &<U as Ui>::Area) -> Text {
        let handle = context::fixed_file::<U>(&*pa).unwrap();
        handle.read(pa, |file, area| {
            self.orig = Some((file.cursors().clone(), area.print_info()));
        });

        text
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

        text.remove_tags(*KEY, ..);

        let command = text.to_string();
        let Some(caller) = command.split_whitespace().next() else {
            return text;
        };

        let args = cmd::args_iter(&command);

        let (caller_id, args_id) = if is_in_path(caller) {
            (form::id_of!("CallerExists"), form::id_of!("ParameterOk"))
        } else {
            (form::id_of!("CallerNotFound"), form::id_of!("ParameterErr"))
        };

        let c_s = command.len() - command.trim_start().len();
        text.insert_tag(*KEY, c_s..c_s + caller.len(), caller_id.to_tag(0));

        for (_, range) in args {
            text.insert_tag(*KEY, range, args_id.to_tag(0));
        }

        text
    }

    fn before_exit(&mut self, pa: &mut Pass, text: Text, _: &<U as Ui>::Area) -> Text {
        use std::process::{Command, Stdio};

        let command = text.to_string();
        let Some(caller) = command.split_whitespace().next() else {
            return text;
        };

        let handle = context::fixed_file::<U>(&*pa).unwrap();
        let mut helper = EditHelper::from_handle(&mut *pa, handle);
        helper.edit_all(pa, |mut e| {
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

        text
    }

    fn prompt(&self) -> Text {
        text!("[Prompt]pipe[Prompt.colon]:").build()
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
