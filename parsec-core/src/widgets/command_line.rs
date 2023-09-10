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
use super::{ActiveWidget, ActiveWidgetCfg, PassiveWidget, Widget};
use crate::{
    commands::{Command, Commands},
    data::{AsAny, ReadableData, RwData},
    input::{Commander, InputMethod},
    text::Text,
    ui::{Area, PushSpecs, Ui},
    Controler,
};

#[derive(Clone)]
pub struct CommandLineCfg<I>
where
    I: InputMethod<Widget = CommandLine> + Clone + 'static,
{
    input: RwData<I>,
    prompt: String,
    specs: PushSpecs,
}

impl CommandLineCfg<Commander> {
    pub fn new() -> CommandLineCfg<Commander> {
        CommandLineCfg {
            input: RwData::new(Commander::new()),
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
}

impl<I> ActiveWidgetCfg for CommandLineCfg<I>
where
    I: InputMethod<Widget = CommandLine> + Clone,
{
    type Widget = CommandLine;
    type WithInput<NewI> = CommandLineCfg<NewI>
    where
        NewI: InputMethod<Widget = Self::Widget> + Clone;

    fn builder<U>(
        self,
    ) -> impl FnOnce(&Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
    {
        move |controler| {
            let command_line = CommandLine {
                text: Text::new_string(" "),
                commands: controler.commands(),
                prompt: RwData::new(self.prompt.clone()),
            };

            add_commands(controler, &command_line);

            let widget = Widget::active(command_line, self.input.clone());
            (widget, Box::new(|| false), self.specs)
        }
    }

    fn with_input<NewI>(self, input: NewI) -> Self::WithInput<NewI>
    where
        NewI: InputMethod<Widget = Self::Widget> + Clone,
    {
        Self::WithInput {
            input: RwData::new(input),
            prompt: self.prompt,
            specs: self.specs,
        }
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
    commands: RwData<Commands>,
    prompt: RwData<String>,
}

impl PassiveWidget for CommandLine {
    fn build<U>(controler: &Controler<U>) -> (Widget<U>, Box<dyn Fn() -> bool>, PushSpecs)
    where
        U: Ui,
        Self: Sized,
    {
        Self::config().builder()(controler)
    }

    fn update(&mut self, _area: &impl Area) {}

    fn text(&self) -> &Text {
        &self.text
    }
}

impl ActiveWidget for CommandLine {
    type Config = CommandLineCfg<Commander>;

    fn config() -> Self::Config
    where
        Self: Sized,
    {
        CommandLineCfg::new()
    }

    fn mut_text(&mut self) -> &mut Text {
        &mut self.text
    }

    fn on_focus(&mut self, _area: &impl Area) {
        self.text = Text::new_string(&self.prompt);
    }

    fn on_unfocus(&mut self, _area: &impl Area) {
        let text = std::mem::replace(&mut self.text, Text::default_string());

        // A '\n' indicates that the command has been "entered".
        let cmd = text
            .iter_chars_at(self.prompt.read().chars().count() - 1)
            .collect::<String>();
        let _ = self.commands.read().try_exec(cmd);
    }
}

impl AsAny for CommandLine {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

unsafe impl Send for CommandLine {}

/// Adds the commands of the [`CommandLine<U>`] to the [`Manager`]'s
/// [`Commands`].
fn add_commands<U>(manager: &Controler<U>, command_line: &CommandLine)
where
    U: Ui,
{
    let prompt = command_line.prompt.clone();
    let set_prompt = Command::new(vec!["set-prompt"], move |_, new_prompt| {
        *prompt.write() = String::from(new_prompt.next().unwrap_or(""));
        Ok(None)
    });

    manager.commands.write().try_add(set_prompt).unwrap();
}
