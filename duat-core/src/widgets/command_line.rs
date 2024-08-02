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
    sync::{atomic::AtomicUsize, LazyLock},
};

use crate::{
    data::{Context, RoData, RwData},
    input::{Commander, InputMethod},
    palette::{self, Form},
    text::{text, Ghost, PrintCfg, Text},
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
            specs: PushSpecs::below().with_ver_length(1.0),
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
        Self {
            prompt: prompt.to_string(),
            ..self
        }
    }

    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs::above().with_ver_length(1.0),
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
        let cmd_line = CommandLine {
            text: Text::new(),
            prompt: RwData::new(self.prompt.clone()),
            context,
            mode: RwData::new(context.get_cmd_mode("RunCommands").unwrap()),
        };

        let checker = {
            let mode = RoData::from(&cmd_line.mode);
            move || mode.read().read().has_changed()
        };
        let widget = Widget::active(cmd_line, RwData::new(self.input));
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
    context: Context<U>,
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
        self.mode
            .read()
            .write()
            .update(&mut self.text, self.context);
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
        palette::set_weak_form("Prompt", Form::new().cyan());

        context
            .commands
            .add_for_widget::<CommandLine<U>>(
                ["set-prompt"],
                move |command_line, _, _, mut args| {
                    let new_prompt: String = args.collect();
                    *command_line.prompt.write() = new_prompt;
                    Ok(None)
                },
            )
            .unwrap();
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
        self.text = text!({ Ghost(text!({ &self.prompt })) });
        self.mode
            .read()
            .write()
            .on_focus(&mut self.text, self.context);
    }

    fn on_unfocus(&mut self, _area: &<U as Ui>::Area) {
        self.mode
            .read()
            .write()
            .on_unfocus(&mut self.text, self.context);
    }
}

unsafe impl<U> Send for CommandLine<U> where U: Ui {}

pub trait CommandLineMode<U>: Sync + Send
where
    U: Ui,
{
    fn new() -> Self
    where
        Self: Sized;

    fn on_focus(&mut self, _text: &mut Text, _context: Context<U>) {}

    fn on_unfocus(&mut self, _text: &mut Text, _context: Context<U>) {}

    fn update(&mut self, _text: &mut Text, _context: Context<U>) {}

    fn has_changed(&self) -> bool {
        false
    }
}

pub struct RunCommands;

impl<U> CommandLineMode<U> for RunCommands
where
    U: Ui,
{
    fn new() -> Self
    where
        Self: Sized,
    {
        Self
    }

    fn on_unfocus(&mut self, text: &mut Text, context: Context<U>) {
        let text = std::mem::take(text);

        let cmd = text.to_string();
        if !cmd.is_empty() {
            context.spawn(|| context.commands.run(cmd));
        }
    }
}
