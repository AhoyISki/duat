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
    sync::{
        atomic::{AtomicBool, Ordering},
        LazyLock,
    },
};

use crate::{
    data::{Context, RoData, RwData},
    hooks,
    input::{Commander, InputMethod},
    forms::{self, Form},
    text::{err, text, Ghost, PrintCfg, Text},
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

impl<U> CommandLineCfg<Commander, U>
where
    U: Ui,
{
    pub fn new() -> Self {
        CommandLineCfg {
            input: Commander::new(),
            prompt: String::from(":"),
            specs: PushSpecs::below().with_ver_len(1.0),
            ghost: PhantomData,
        }
    }
}

impl<U> Default for CommandLineCfg<Commander, U>
where
    U: Ui,
{
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

    fn build(self, context: Context<U>, _: bool) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        let mode = if hooks::group_exists("CmdLineNotifications") {
            RwData::new(context.get_cmd_mode("ShowNotifications").unwrap())
        } else {
            RwData::new(context.get_cmd_mode("RunCommands<Ui>").unwrap())
        };

        let cmd_line = CommandLine {
            text: Text::new(),
            prompt: RwData::new(self.prompt.clone()),
            mode,
        };

        let checker = {
            let mode = RoData::from(&cmd_line.mode);
            move || mode.read().read().has_changed()
        };
        let widget = Widget::active(cmd_line, self.input);
        (widget, checker, self.specs)
    }
}

/// An [`ActiveWidget<U>`] whose primary purpose is to execute
/// [`Command`]s.
///
/// While this is the primary purpose of the [`CommandLine<U>`], in
/// the future, it will be able to change its functionality to, for
/// example, search for pieces of text on a
/// [`File`][parsec_core::file::File] in real time.
pub struct CommandLine<U>
where
    U: Ui,
{
    text: Text,
    prompt: RwData<String>,
    mode: RwData<RwData<dyn CommandLineMode<U>>>,
}

impl<U> CommandLine<U>
where
    U: Ui,
{
    pub fn cfg() -> CommandLineCfg<Commander, U> {
        CommandLineCfg::new()
    }
}

impl<U> PassiveWidget<U> for CommandLine<U>
where
    U: Ui,
{
    fn build(context: Context<U>, on_file: bool) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        CommandLineCfg::new().build(context, on_file)
    }

    fn update(&mut self, _area: &<U as Ui>::Area) {
        self.mode.read().read().update(&mut self.text);
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn print_cfg(&self) -> &crate::text::PrintCfg {
        static CFG: LazyLock<PrintCfg> =
            LazyLock::new(|| PrintCfg::default_for_input().with_forced_scrolloff());
        &CFG
    }

    fn once(context: Context<U>) {
        forms::set_weak_form("Prompt", Form::new().cyan());

        context
            .commands
            .add_for_widget::<CommandLine<U>>(
                ["set-prompt"],
                move |command_line, _, _, mut args| {
                    let new_prompt: String = args.collect();
                    *command_line.read().prompt.write() = new_prompt;
                    Ok(None)
                },
            )
            .unwrap();

        context
            .commands
            .add_for_widget::<CommandLine<U>>(["set-cmd-mode"], move |cmd_line, _, _, mut args| {
                let new_mode = args.next()?;
                *cmd_line.read().mode.write() = context
                    .get_cmd_mode(new_mode)
                    .ok_or(err!("There is no " [*a] new_mode [] " mode."))?;

                Ok(None)
            })
            .unwrap()
    }
}

impl<U> ActiveWidget<U> for CommandLine<U>
where
    U: Ui,
{
    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn on_focus(&mut self, _area: &U::Area) {
        self.text = text!({ Ghost(text!({ &self.prompt })) } '\n');
        self.mode.read().read().on_focus(&mut self.text);
    }

    fn on_unfocus(&mut self, _area: &<U as Ui>::Area) {
        self.mode.read().read().on_unfocus(&mut self.text);
    }
}

unsafe impl<U> Send for CommandLine<U> where U: Ui {}

pub trait CommandLineMode<U>: Sync + Send
where
    U: Ui,
{
    fn on_focus(&self, _text: &mut Text) {}

    fn on_unfocus(&self, _text: &mut Text) {}

    fn update(&self, _text: &mut Text) {}

    fn has_changed(&self) -> bool {
        false
    }
}

pub struct RunCommands<U>(Context<U>)
where
    U: Ui;

impl<U> RunCommands<U>
where
    U: Ui,
{
    #[doc(hidden)]
    pub fn new(context: Context<U>) -> Self {
        Self(context)
    }
}

impl<U> CommandLineMode<U> for RunCommands<U>
where
    U: Ui,
{
    fn on_unfocus(&self, text: &mut Text) {
        let text = std::mem::take(text);

        let cmd = text.to_string();
        if !cmd.is_empty() {
            crate::thread::spawn(|| self.0.commands.run_notify(cmd));
        }
    }
}

pub struct ShowNotifications {
    notifications: RoData<Text>,
    has_changed: AtomicBool,
}

impl ShowNotifications {
    #[doc(hidden)]
    pub fn new<U>(context: Context<U>) -> Self
    where
        U: Ui,
    {
        Self {
            notifications: RoData::from(context.notifications()),
            has_changed: AtomicBool::new(false),
        }
    }
}

impl<U> CommandLineMode<U> for ShowNotifications
where
    U: Ui,
{
    fn has_changed(&self) -> bool {
        let has_changed = self.notifications.has_changed();
        if has_changed {
            self.has_changed.store(true, Ordering::Release);
        }
        has_changed
    }

    fn update(&self, text: &mut Text) {
        if self.has_changed.fetch_and(false, Ordering::Acquire) {
            *text = self.notifications.read().clone();
        }
    }
}
