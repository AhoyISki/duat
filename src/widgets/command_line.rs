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
use parsec_core::{
    commands,
    data::RwData,
    input::{Cursors, InputMethod},
    palette::{self, Form},
    text::{text, Ghost, Text},
    ui::{Area, PushSpecs, Ui},
    Globals,
};

use super::{ActiveWidget, PassiveWidget, Widget, WidgetCfg};

#[derive(Clone)]
pub struct CommandLineCfg<I, U>
where
    I: InputMethod<U, Widget = CommandLine<U>> + Clone + 'static,
    U: Ui,
{
    input: I,
    prompt: String,
    specs: PushSpecs,
}

impl<U> CommandLineCfg<Commander, U>
where
    U: Ui,
{
    pub fn new() -> CommandLineCfg<Commander, U> {
        CommandLineCfg {
            input: Commander::new(),
            prompt: String::from(":"),
            specs: PushSpecs::below().with_lenght(1.0),
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

    pub fn with_input<NewI>(self, input: NewI) -> CommandLineCfg<NewI, U>
    where
        NewI: InputMethod<U, Widget = CommandLine<U>> + Clone,
    {
        CommandLineCfg {
            input,
            prompt: self.prompt,
            specs: self.specs,
        }
    }
}

impl<I, U> WidgetCfg<U> for CommandLineCfg<I, U>
where
    I: InputMethod<U, Widget = CommandLine<U>> + Clone,
{
    type Widget = CommandLine<U>;

    fn build(self, globals: Globals<U>) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        let command_line = CommandLine {
            text: Text::new(" "),
            prompt: RwData::new(self.prompt.clone()),
            globals,
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
pub struct CommandLine<U> {
    text: Text,
    prompt: RwData<String>,
    globals: Globals<U>,
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
    fn build(_globals: Globals<U>) -> (Widget<U>, impl Fn() -> bool, PushSpecs) {
        CommandLineCfg::new().build()
    }

    fn update(&mut self, _area: &impl Area) {}

    fn text(&self) -> &Text {
        &self.text
    }

    fn print(&mut self, area: &impl Area) {
        area.print(self.text(), self.print_cfg(), palette::painter())
    }

    fn once(globals: Globals<U>) {
        palette::set_weak_form("Prompt", Form::new().cyan());

        globals
            .commands
            .add_for_widget::<CommandLine>(["set-prompt"], move |command_line, _, _, mut args| {
                let new_prompt: String = args.collect();
                *command_line.prompt.write() = new_prompt;
                Ok(None)
            })
            .unwrap();
    }
}

impl<U> ActiveWidget<U> for CommandLine<U>
where
    U: Ui,
{
    fn mut_text(&mut self) -> &mut Text {
        &mut self.text
    }

    fn on_focus(&mut self, _area: &impl Area) {
        self.text = text!({ Ghost(text!({ &self.prompt })) });
    }

    fn on_unfocus(&mut self, _area: &impl Area) {
        let text = std::mem::take(&mut self.text);

        let cmd = text.iter_chars_at(0).collect::<String>();
        std::thread::spawn(|| self.globals.commands.run(cmd));
    }
}

unsafe impl<U: Ui> Send for CommandLine<U> {}

#[derive(Clone)]
pub struct Commander {
    cursors: Cursors,
}

impl Commander {
    pub fn new() -> Self {
        Self {
            cursors: Cursors::new(),
        }
    }
}

impl Default for Commander {
    fn default() -> Self {
        Self::new()
    }
}

impl<U> InputMethod<U> for Commander
where
    U: Ui,
{
    type Widget = CommandLine<U>;

    fn send_key(
        &mut self,
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &U::Area,
        globals: Globals<U>,
    ) {
        let mut editor = MultiCursorEditor::new(widget, &mut self.cursors, area);

        match key {
            key!(KeyCode::Backspace) => {
                editor.move_main(|mover| {
                    mover.set_anchor();
                    mover.move_hor(-1);
                });
                editor.edit_on_main(|editor| {
                    editor.replace("");
                });
                editor.move_main(|mover| {
                    mover.unset_anchor();
                });
            }
            key!(KeyCode::Delete) => {
                editor.move_main(|mover| {
                    mover.set_anchor();
                    mover.move_hor(1);
                });
                editor.edit_on_main(|editor| {
                    editor.replace("");
                });
                editor.move_main(|mover| {
                    mover.unset_anchor();
                });
            }
            key!(KeyCode::Char(ch)) => {
                editor.edit_on_main(|editor| editor.insert(ch));
                editor.move_main(|mover| mover.move_hor(1));
            }
            key!(KeyCode::Char(ch), KeyModifiers::SHIFT) => {
                editor.edit_on_main(|editor| editor.insert(ch));
                editor.move_main(|mover| mover.move_hor(1));
            }

            key!(KeyCode::Left) => {
                editor.move_main(|mover| {
                    mover.unset_anchor();
                    mover.move_hor(-1)
                });
            }
            key!(KeyCode::Right) => {
                editor.move_main(|mover| {
                    mover.unset_anchor();
                    mover.move_hor(1)
                });
            }

            key!(KeyCode::Esc) => {
                editor.move_main(|mover| {
                    mover.move_hor(isize::MIN);
                    mover.set_anchor();
                    mover.move_hor(isize::MAX);
                });

                editor.edit_on_main(|editor| editor.replace(""));

                self.cursors = Cursors::default();
                globals.commands.return_to_file().unwrap();
            }
            key!(KeyCode::Enter) => {
                self.cursors = Cursors::default();
                globals.commands.return_to_file().unwrap();
            }
            _ => {}
        }
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}
