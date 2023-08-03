//! An [`ActionableWidget<U>`] capable of running [`Command`]s.
//!
//! This widget is capable of running [`Command`]s that are defined
//! and stored in the [`Commands`] struct. It does so by treating the
//! first word as the `caller` for the [`Command`], while the other
//! words are treated as `arguments` for said [`Command`].
//!
//! There are plans on creating a simple interface for `flags` in the
//! near future.
//!
//! There are also plans to permit overriding [`CommandLine<U>`]'s
//! behaviour, such as making it search for text in a [`Text<U>`], in
//! real time.
//!
//! Currently, you can also change the prompt of a [`CommandLine<U>`],
//! by running the `set-prompt` [`Command`].
use super::{ActSchemeWidget, EditAccum, Widget, WidgetType};
use crate::{
    commands::{Command, Commands},
    data::{AsAny, ReadableData, RwData},
    position::{Cursor, Editor, Mover},
    text::{PrintCfg, Text},
    ui::{Constraint, PrintInfo, PushSpecs, Ui},
    Controler
};

/// An [`ActionableWidget<U>`] whose primary purpose is to execute
/// [`Command`]s.
///
/// While this is the primary purpose of the [`CommandLine<U>`], in
/// the future, it will be able to change its functionality to, for
/// example, search for pieces of text on a
/// [`FileWidget<U>`][crate::widgets::FileWidget] in real time.
pub struct CommandLine<U>
where
    U: Ui
{
    text: Text,
    print_info: U::PrintInfo,
    cursor: [Cursor; 1],
    commands: RwData<Commands>,
    prompt: RwData<String>
}

impl<U> CommandLine<U>
where
    U: Ui + 'static
{
    /// Returns a function that outputs a [`CommandLine<U>`] with a
    /// custom prompt.
    pub fn prompt_fn(
        prompt: impl ToString
    ) -> impl FnOnce(&Controler<U>) -> (WidgetType<U>, Box<dyn Fn() -> bool>, PushSpecs) {
        let prompt = prompt.to_string();
        move |controler| {
            let command_line = Self {
                text: Text::default_string(),
                print_info: U::PrintInfo::default(),
                cursor: [Cursor::default()],
                commands: controler.commands(),
                prompt: RwData::new(prompt + " ")
            };

            add_commands(controler, &command_line);

            let widget_type = WidgetType::scheme_input(command_line);
            (widget_type, Box::new(|| false), PushSpecs::below(Constraint::Length(1.0)))
        }
    }

    /// Returns a function that outputs a [`CommandLine<U>`] with
    /// `":"` as a prompt.
    pub fn default_fn()
    -> impl FnOnce(&Controler<U>) -> (WidgetType<U>, Box<dyn Fn() -> bool>, PushSpecs) {
        move |manager| {
            let command_line = CommandLine::<U> {
                text: Text::default_string(),
                print_info: U::PrintInfo::default(),
                cursor: [Cursor::default()],
                commands: manager.commands(),
                prompt: RwData::new(String::from(": "))
            };

            add_commands(manager, &command_line);

            let widget_type = WidgetType::scheme_input(command_line);
            (widget_type, Box::new(|| false), PushSpecs::below(Constraint::Length(1.0)))
        }
    }
}

impl<U> Widget<U> for CommandLine<U>
where
    U: Ui + 'static
{
    fn update(&mut self, area: &U::Area) {
        let print_cfg = PrintCfg::default();
        self.print_info.scroll_to_gap(&self.text, self.cursor[0].caret(), area, &print_cfg);
    }

    fn text(&self) -> &Text {
        &self.text
    }
}

impl<U> ActSchemeWidget<U> for CommandLine<U>
where
    U: Ui + 'static
{
    fn editor<'a>(&'a mut self, _: usize, edit_accum: &'a mut EditAccum) -> Editor<U> {
        Editor::new(&mut self.cursor[0], &mut self.text, edit_accum, None, None)
    }

    fn mover<'a>(&'a mut self, _: usize, area: &'a U::Area) -> Mover<U> {
        Mover::new(&mut self.cursor[0], &self.text, area, PrintCfg::default())
    }

    fn members_for_cursor_tags(&mut self) -> (&mut Text, &[Cursor], usize) {
        (&mut self.text, self.cursor.as_slice(), 0)
    }

    fn cursors(&self) -> &[Cursor] {
        self.cursor.as_slice()
    }

    fn mut_cursors(&mut self) -> Option<&mut Vec<Cursor>> {
        None
    }

    fn main_cursor_index(&self) -> usize {
        0
    }

    fn mut_main_cursor_index(&mut self) -> Option<&mut usize> {
        None
    }

    fn on_focus(&mut self, area: &U::Area) {
        self.text = Text::new_string(&self.prompt);
        let chars = self.prompt.read().chars().count() as isize;
        self.cursor[0].move_hor(chars, &self.text, area, &PrintCfg::default());
        self.text.add_cursor_tags(self.cursor.as_slice(), 0);
    }

    fn on_unfocus(&mut self, _area: &U::Area) {
        let text = std::mem::replace(&mut self.text, Text::default_string());
        self.cursor[0] = Cursor::default();
        self.text.remove_cursor_tags(self.cursor.as_slice());

        // A '\n' indicates that the command has been "entered".
        let cmd = text.iter_chars_at(self.prompt.read().chars().count() - 1).collect::<String>();
        let _ = self.commands.read().try_exec(cmd);
    }
}

impl<U> AsAny for CommandLine<U>
where
    U: Ui + 'static
{
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

unsafe impl<U> Send for CommandLine<U> where U: Ui {}
unsafe impl<U> Sync for CommandLine<U> where U: Ui {}

/// Adds the commands of the [`CommandLine<U>`] to the [`Manager`]'s
/// [`Commands`].
fn add_commands<U>(manager: &Controler<U>, command_line: &CommandLine<U>)
where
    U: Ui
{
    let prompt = command_line.prompt.clone();
    let set_prompt = Command::new(vec!["set-prompt"], move |_, new_prompt| {
        *prompt.write() = String::from(new_prompt.next().unwrap_or(""));
        Ok(None)
    });

    manager.commands.write().try_add(set_prompt).unwrap();
}
