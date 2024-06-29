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
use duat_core::{
    data::RwData,
    input::{key, Cursors, InputMethod, KeyCode, KeyEvent, KeyModifiers, MultiCursorEditor},
    palette::{self, Form},
    text::{text, Ghost, Text},
    ui::{Area, PushSpecs},
    widgets::{ActiveWidget, PassiveWidget, Widget, WidgetCfg},
    Context,
};

use crate::Ui;

#[derive(Clone)]
pub struct CommandLineCfg<I>
where
    I: InputMethod<Ui, Widget = CommandLine> + Clone + 'static,
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
    I: InputMethod<Ui, Widget = CommandLine> + Clone,
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
        NewI: InputMethod<Ui, Widget = CommandLine> + Clone,
    {
        CommandLineCfg {
            input,
            prompt: self.prompt,
            specs: self.specs,
        }
    }
}

impl<I> WidgetCfg<Ui> for CommandLineCfg<I>
where
    I: InputMethod<Ui, Widget = CommandLine> + Clone,
{
    type Widget = CommandLine;

    fn build(self, context: Context<Ui>, _: bool) -> (Widget<Ui>, impl Fn() -> bool, PushSpecs) {
        let command_line = CommandLine {
            text: Text::from(" "),
            prompt: RwData::new(self.prompt.clone()),
            context,
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
    context: Context<Ui>,
}

impl CommandLine {
    pub fn cfg() -> CommandLineCfg<Commander> {
        CommandLineCfg::new()
    }
}

impl PassiveWidget<Ui> for CommandLine {
    fn build(context: Context<Ui>, on_file: bool) -> (Widget<Ui>, impl Fn() -> bool, PushSpecs) {
        CommandLineCfg::new().build(context, on_file)
    }

    fn update(&mut self, _area: &<Ui as duat_core::ui::Ui>::Area) {}

    fn text(&self) -> &Text {
        &self.text
    }

    fn print(&mut self, area: &<Ui as duat_core::ui::Ui>::Area) {
        area.print(self.text(), self.print_cfg(), palette::painter())
    }

    fn once(context: Context<Ui>) {
        palette::set_weak_form("Prompt", Form::new().cyan());

        context
            .commands
            .add_for_widget::<CommandLine>(["set-prompt"], move |command_line, _, _, mut args| {
                let new_prompt: String = args.collect();
                *command_line.prompt.write() = new_prompt;
                Ok(None)
            })
            .unwrap();
    }
}

impl ActiveWidget<Ui> for CommandLine {
    fn mut_text(&mut self) -> &mut Text {
        &mut self.text
    }

    fn on_focus(&mut self, _area: &<Ui as duat_core::ui::Ui>::Area) {
        self.text = text!({ Ghost(text!({ &self.prompt })) });
    }

    fn on_unfocus(&mut self, _area: &<Ui as duat_core::ui::Ui>::Area) {
        let text = std::mem::take(&mut self.text);

        let cmd = text.iter_chars_at(0).collect::<String>();
        std::thread::spawn(|| self.context.commands.run(cmd));
    }
}

unsafe impl Send for CommandLine {}

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

impl InputMethod<Ui> for Commander {
    type Widget = CommandLine;

    fn send_key(
        &mut self,
        key: KeyEvent,
        widget: &RwData<Self::Widget>,
        area: &<Ui as duat_core::ui::Ui>::Area,
        context: Context<Ui>,
    ) {
        let mut editor = MultiCursorEditor::new(widget, area, &mut self.cursors);

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
                context.commands.return_to_file().unwrap();
            }
            key!(KeyCode::Enter) => {
                self.cursors = Cursors::default();
                context.commands.return_to_file().unwrap();
            }
            _ => {}
        }
    }

    fn cursors(&self) -> Option<&Cursors> {
        Some(&self.cursors)
    }
}
