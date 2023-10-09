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
use super::{ActiveWidget, PassiveWidget, Widget, WidgetCfg};
use crate::{
    commands,
    data::RwData,
    input::{Commander, InputMethod},
    palette::{self, Form},
    text::{text, Ghost, Text},
    ui::{Area, PushSpecs, Ui},
};

#[derive(Clone)]
pub struct CommandLineCfg<I>
where
    I: InputMethod<Widget = CommandLine> + Clone + 'static,
{
    input: I,
    prompt: String,
    specs: PushSpecs,
}

impl CommandLineCfg<Commander> {
    pub fn new() -> CommandLineCfg<Commander> {
        CommandLineCfg {
            input: Commander::new(),
            prompt: String::from(":"),
            specs: PushSpecs::below().with_lenght(1.0),
        }
    }
}

impl Default for CommandLineCfg<Commander> {
    fn default() -> Self {
        Self::new()
    }
}

impl<I> CommandLineCfg<I>
where
    I: InputMethod<Widget = CommandLine> + Clone,
{
    pub fn with_prompt(self, prompt: impl ToString) -> Self {
        Self {
            prompt: prompt.to_string(),
            ..self
        }
    }

    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs::above().with_lenght(1.0),
            ..self
        }
    }

    pub fn left_with_percent(self, percent: u16) -> Self {
        Self {
            specs: PushSpecs::left().with_percent(percent),
            ..self
        }
    }

    pub fn with_input<NewI>(self, input: NewI) -> CommandLineCfg<NewI>
    where
        NewI: InputMethod<Widget = CommandLine> + Clone,
    {
        CommandLineCfg {
            input,
            prompt: self.prompt,
            specs: self.specs,
        }
    }
}

impl<I> WidgetCfg for CommandLineCfg<I>
where
    I: InputMethod<Widget = CommandLine> + Clone,
{
    type Widget = CommandLine;

    fn build<U: Ui>(self) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        let command_line = CommandLine {
            text: Text::new(" "),
            prompt: RwData::new(self.prompt.clone()),
        };

        let widget = Widget::active(command_line, RwData::new(self.input));
        (widget, || false, self.specs)
    }
}

/// An [`ActionableWidget<U>`] whose primary purpose is to execute
/// [`Command`]s.
///
/// While this is the primary purpose of the [`CommandLine<U>`], in
/// the future, it will be able to change its functionality to, for
/// example, search for pieces of text on a
/// [`FileWidget<U>`][crate::widgets::FileWidget] in real time.
pub struct CommandLine {
    text: Text,
    prompt: RwData<String>,
}

impl CommandLine {
    pub fn cfg() -> CommandLineCfg<Commander> {
        CommandLineCfg::new()
    }
}

impl PassiveWidget for CommandLine {
    fn build<U: Ui>() -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        CommandLineCfg::new().build()
    }

    fn update(&mut self, _area: &impl Area) {}

    fn text(&self) -> &Text {
        &self.text
    }

    fn print(&mut self, area: &impl Area) {
        area.print(self.text(), self.print_cfg(), palette::painter())
    }

    fn once() {
        palette::set_weak_form("Prompt", Form::new().cyan());

        commands::add_for_widget::<CommandLine>(["set-prompt"], move |command_line, _, _, args| {
            let new_prompt: Vec<&str> = args.collect();
            let new_prompt = new_prompt.join(" ");
            *command_line.prompt.write() = new_prompt;
            Ok(None)
        })
        .unwrap();
    }
}

impl ActiveWidget for CommandLine {
    fn mut_text(&mut self) -> &mut Text {
        &mut self.text
    }

    fn on_focus(&mut self, _area: &impl Area) {
        self.text = text!({ Ghost(text!({ &self.prompt })) });
    }

    fn on_unfocus(&mut self, _area: &impl Area) {
        let text = std::mem::take(&mut self.text);

        let cmd = text.iter_chars_at(0).collect::<String>();
        std::thread::spawn(|| commands::run(cmd));
    }
}

unsafe impl Send for CommandLine {}
