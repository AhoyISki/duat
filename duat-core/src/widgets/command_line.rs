//! An [`ActiveWidget`] capable of running [`Command`]s.
//!
//! This widget is capable of running [`Command`]s that are defined
//! and stored in the [`Commands`] struct. It does so by treating the
//! first word as the `caller` for the [`Command`], while the other
//! words are treated as `arguments` for said [`Command`].
//!
//! There are plans on creating a simple interface for `flags` in the
//! near future.
//!
//! There are also plans to permit overriding [`CommandLine`]'s
//! behaviour, such as making it search for text in a [`Text`], in
//! real time.
//!
//! Currently, you can also change the prompt of a [`CommandLine`],
//! by running the `set-prompt` [`Command`].
use std::{
    marker::PhantomData,
    sync::{Arc, LazyLock},
};

use parking_lot::RwLock;

use crate::{
    commands,
    data::{RoData, RwData, context},
    duat_name,
    forms::{self, Form},
    hooks,
    input::{Commander, InputMethod},
    text::{Ghost, Key, PrintCfg, SavedMatches, Tag, Text, text},
    ui::{PushSpecs, Ui},
    widgets::{ActiveWidget, PassiveWidget, Widget, WidgetCfg},
};

#[derive(Clone)]
pub struct CommandLineCfg<I, U>
where
    I: InputMethod<U, Widget = CommandLine<U>> + Clone + 'static,
    U: Ui,
{
    input: I,
    prompt: String,
    specs: PushSpecs,
    ghost: PhantomData<U>,
}

impl<U: Ui> CommandLineCfg<Commander, U> {
    pub fn new() -> Self {
        CommandLineCfg {
            input: Commander::new(),
            prompt: String::from(":"),
            specs: PushSpecs::below().with_ver_len(1.0),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> Default for CommandLineCfg<Commander, U> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I, U> CommandLineCfg<I, U>
where
    I: InputMethod<U, Widget = CommandLine<U>> + Clone,
    U: Ui,
{
    pub fn with_prompt(self, prompt: impl ToString) -> Self {
        Self { prompt: prompt.to_string(), ..self }
    }

    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs::above().with_ver_len(1.0),
            ..self
        }
    }

    pub fn left_ratioed(self, den: u16, div: u16) -> Self {
        Self {
            specs: PushSpecs::left().with_hor_ratio(den, div),
            ..self
        }
    }

    pub fn with_input<NewI>(self, input: NewI) -> CommandLineCfg<NewI, U>
    where
        NewI: InputMethod<U, Widget = CommandLine<U>> + Clone,
    {
        CommandLineCfg {
            input,
            prompt: self.prompt,
            specs: self.specs,
            ghost: PhantomData,
        }
    }
}

impl<I, U> WidgetCfg<U> for CommandLineCfg<I, U>
where
    I: InputMethod<U, Widget = CommandLine<U>> + Clone,
    U: Ui,
{
    type Widget = CommandLine<U>;

    fn build(self, _: bool) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        let mode: RwData<dyn CommandLineMode<U>> = if hooks::group_exists("CmdLineNotifications") {
            run_once::<ShowNotifications, U>();
            RwData::new_unsized::<ShowNotifications>(Arc::new(RwLock::new(
                <ShowNotifications as CommandLineMode<U>>::new(),
            )))
        } else {
            run_once::<RunCommands, U>();
            RwData::new_unsized::<RunCommands>(Arc::new(RwLock::new(
                <RunCommands as CommandLineMode<U>>::new(),
            )))
        };

        let cmd_line = CommandLine {
            text: Text::new(),
            prompt: RwData::new(self.prompt.clone()),
            mode: RwData::new(mode),
        };

        let checker = {
            let mode = RoData::from(&cmd_line.mode);
            move || mode.read().write().has_changed()
        };
        let widget = Widget::active(cmd_line, self.input);
        (widget, checker, self.specs)
    }
}

/// A multi purpose, [promptable] widget
///
/// This widget, as the name implies, is most associated with running
/// commands. However, it can have a variety of [modes], granting it
/// differing functionality. In Duat, there are 3 predefined modes:
///
/// * [`RunCommands`], which runs commands (duh);
/// * [`ShowNotifications`], which shows notifications, usually about
///   commands;
/// * [`IncSearch`], which is used by [`InputMethod`]s in order to do
///   incremental search from the command line.
///
/// By default, Duat will have the `"CmdLineNotifications"` [hook]
/// active. This hook changes the mode of the [`CommandLine`] to
/// [`ShowNotifications`] whenever it is unfocused. If you don't want
/// this functionality, or want notifications somewhere else, you can
/// use [`hooks::remove_group`].
///
/// [promptable]: ActiveWidget
/// [modes]: CommandLineMode
/// [hook]: crate::hooks
pub struct CommandLine<U: Ui> {
    text: Text,
    prompt: RwData<String>,
    mode: RwData<RwData<dyn CommandLineMode<U>>>,
}

impl<U: Ui> CommandLine<U> {
    pub(crate) fn set_mode<M: CommandLineMode<U>>(&mut self) {
        run_once::<M, U>();
        *self.mode.write() = RwData::new_unsized::<M>(Arc::new(RwLock::new(M::new())));
    }
}

impl<U: Ui> PassiveWidget<U> for CommandLine<U> {
    type Cfg = CommandLineCfg<Commander, U>;

    fn cfg() -> Self::Cfg {
        CommandLineCfg::new()
    }

    fn update(&mut self, _area: &<U as Ui>::Area) {
        self.mode.read().write().update(&mut self.text);
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn print_cfg(&self) -> crate::text::PrintCfg {
        PrintCfg::default_for_input().with_forced_scrolloff()
    }

    fn once() {
        forms::set_weak("Prompt", Form::cyan());
        forms::set_weak("ParseCommandErr", "DefaultErr");

        commands::add_for_widget::<CommandLine<U>, U>(
            ["set-prompt"],
            move |command_line, _, _, mut args| {
                let new_prompt: String = args.collect();
                *command_line.read().prompt.write() = new_prompt;
                Ok(None)
            },
        )
        .unwrap();
    }
}

impl<U: Ui> ActiveWidget<U> for CommandLine<U> {
    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn on_focus(&mut self, _area: &U::Area) {
        self.text = text!({ Ghost(text!({ &self.prompt })) } '\n');
        self.mode.read().write().on_focus(&mut self.text);
    }

    fn on_unfocus(&mut self, _area: &<U as Ui>::Area) {
        self.mode.read().write().on_unfocus(&mut self.text);
    }
}

pub trait CommandLineMode<U: Ui>: Send + Sync + 'static {
    fn new() -> Self
    where
        Self: Sized;

    fn on_focus(&mut self, _text: &mut Text) {}

    fn on_unfocus(&mut self, _text: &mut Text) {}

    fn update(&mut self, _text: &mut Text) {}

    fn has_changed(&mut self) -> bool {
        false
    }

    fn once()
    where
        Self: Sized,
    {
    }
}

pub struct RunCommands {
    key: Key,
}

impl<U: Ui> CommandLineMode<U> for RunCommands {
    fn new() -> Self {
        commands::switch_to::<CommandLine<U>, U>();
        Self { key: Key::new() }
    }

    fn update(&mut self, text: &mut Text) {
        text.remove_tags_of(self.key);

        let command = text.to_string();
        let caller = command.split_whitespace().next();
        if let Some(caller) = caller {
            if commands::caller_exists(caller) {
                let id = forms::id_of!("CallerExists");
                text.insert_tag(0, Tag::PushForm(id), self.key);
                text.insert_tag(caller.len(), Tag::PopForm(id), self.key);
            } else {
                let id = forms::id_of!("CallerNotFound");
                text.insert_tag(0, Tag::PushForm(id), self.key);
                text.insert_tag(caller.len(), Tag::PopForm(id), self.key);
            }
        }
    }

    fn on_unfocus(&mut self, text: &mut Text) {
        let text = std::mem::take(text);

        let cmd = text.to_string();
        if !cmd.is_empty() {
            crate::thread::queue(move || commands::run_notify(cmd));
        }
    }

    fn once() {
        forms::set_weak("CallerExists", "AccentOk");
        forms::set_weak("CallerNotFound", "AccentErr");
    }
}

pub struct ShowNotifications {
    notifications: &'static RwData<Text>,
    has_changed: bool,
}

impl<U: Ui> CommandLineMode<U> for ShowNotifications {
    fn new() -> Self {
        Self {
            notifications: context::notifications(),
            has_changed: false,
        }
    }

    fn has_changed(&mut self) -> bool {
        self.has_changed = self.notifications.has_changed();
        self.has_changed
    }

    fn update(&mut self, text: &mut Text) {
        if self.has_changed {
            self.has_changed = false;
            *text = self.notifications.read().clone();
        } else {
            *text = Text::new();
        }
    }
}

pub struct IncSearch {
    list: RwData<Vec<SavedMatches>>,
    error_range: Option<(usize, usize)>,
    key: Key,
}

impl<U: Ui> CommandLineMode<U> for IncSearch {
    fn new() -> Self {
        commands::switch_to::<CommandLine<U>, U>();
        Self {
            list: RwData::default(),
            error_range: None,
            key: Key::new(),
        }
    }

    fn update(&mut self, text: &mut Text) {
        if let Some((start, end)) = self.error_range.take() {
            text.remove_tags_on(start, self.key);
            text.remove_tags_on(end, self.key);
        }

        let cur_file = context::cur_file::<U>().unwrap();
        let mut list = self.list.write();

        if let Some(saved) = list.iter_mut().find(|s| s.pat_is(text)) {
            let searcher = saved.searcher();
            cur_file.mutate_data(|file, area, input| {
                input.write().search_inc(file, area, searcher);
            });
        } else {
            match SavedMatches::new(text.to_string()) {
                Ok(mut saved) => {
                    if let Some(prev) = list.iter().rev().find(|s| s.is_prefix_of(&saved)) {
                        saved.take_matches_from(prev);
                    }

                    let searcher = saved.searcher();
                    cur_file.mutate_data(|file, area, input| {
                        input.write().search_inc(file, area, searcher);
                    });

                    list.push(saved);
                }
                Err(error) => {
                    let regex_syntax::Error::Parse(error) = *error else {
                        unreachable!("As far as I can tell, this would be a bug with regex_syntax");
                    };

                    let span = error.span();
                    let id = crate::forms::id_of!("ParseCommandErr");

                    text.insert_tag(span.start.offset, Tag::PushForm(id), self.key);
                    text.insert_tag(span.end.offset, Tag::PopForm(id), self.key);
                    self.error_range = Some((span.start.offset, span.end.offset));
                }
            }
        }
    }

    fn on_focus(&mut self, _text: &mut Text) {
        context::cur_file::<U>()
            .unwrap()
            .mutate_data(|file, area, input| input.write().begin_inc_search(file, area));
    }

    fn on_unfocus(&mut self, _text: &mut Text) {
        context::cur_file::<U>()
            .unwrap()
            .mutate_data(|file, area, input| input.write().end_inc_search(file, area));
    }
}

/// Runs the [`once`] function of widgets.
///
/// [`once`]: PassiveWidget::once
fn run_once<M: CommandLineMode<U>, U: Ui>() {
    static ONCE_LIST: LazyLock<RwData<Vec<&'static str>>> =
        LazyLock::new(|| RwData::new(Vec::new()));

    let mut once_list = ONCE_LIST.write();
    if !once_list.contains(&duat_name::<M>()) {
        M::once();
        once_list.push(duat_name::<M>());
    }
}
