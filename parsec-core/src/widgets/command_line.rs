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
use super::{ActionableWidget, EditAccum, NormalWidget, Widget};
use crate::{
    commands::{Command, Commands},
    data::{DownCastableData, RwData},
    position::{Cursor, Editor, Mover},
    text::{PrintCfg, Text},
    ui::{PrintInfo, PushSpecs, Ui},
    Manager
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
    text: Text<U>,
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
    pub fn prompt_fn(prompt: impl ToString) -> impl FnOnce(&Manager<U>, PushSpecs) -> Widget<U> {
        let prompt = prompt.to_string();
        move |manager, _| {
            let command_line = CommandLine::<U> {
                text: Text::default_string(),
                print_info: U::PrintInfo::default(),
                cursor: [Cursor::default()],
                commands: manager.commands(),
                prompt: RwData::new(prompt)
            };

            add_commands(manager, &command_line);

            Widget::actionable(command_line, Box::new(|| false))
        }
    }

    /// Returns a function that outputs a [`CommandLine<U>`] with
    /// `":"` as a prompt.
    pub fn default_fn() -> impl FnOnce(&Manager<U>, PushSpecs) -> Widget<U> {
        move |manager, _| {
            let command_line = CommandLine::<U> {
                text: Text::default_string(),
                print_info: U::PrintInfo::default(),
                cursor: [Cursor::default()],
                commands: manager.commands(),
                prompt: RwData::new(String::from(":"))
            };

            add_commands(manager, &command_line);

            Widget::actionable(command_line, Box::new(|| false))
        }
    }
}

impl<U> NormalWidget<U> for CommandLine<U>
where
    U: Ui + 'static
{
    fn update(&mut self, label: &U::Label) {
        let print_cfg = PrintCfg::default();
        self.print_info
            .scroll_to_gap(&self.text, self.cursor[0].caret(), label, &print_cfg);
    }

    fn text(&self) -> &Text<U> {
        &self.text
    }
}

impl<U> ActionableWidget<U> for CommandLine<U>
where
    U: Ui + 'static
{
    fn editor<'a>(&'a mut self, _: usize, edit_accum: &'a mut EditAccum) -> Editor<U> {
        Editor::new(&mut self.cursor[0], &mut self.text, edit_accum, None, None)
    }

    fn mover<'a>(&'a mut self, _: usize, label: &'a U::Label) -> Mover<U> {
        Mover::new(&mut self.cursor[0], &self.text, label, PrintCfg::default())
    }

    fn members_for_cursor_tags(&mut self) -> (&mut Text<U>, &[Cursor], usize) {
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

    fn on_focus(&mut self, label: &U::Label) {
        self.text = Text::new_string(&self.prompt);
        let chars = self.prompt.read().chars().count() as isize;
        self.cursor[0].move_hor::<U>(chars, &self.text, label, &PrintCfg::default());
        self.text.add_cursor_tags(self.cursor.as_slice(), 0);
    }

    fn on_unfocus(&mut self, _label: &U::Label) {
        let text = std::mem::replace(&mut self.text, Text::default_string());
        self.cursor[0] = Cursor::default();
        self.text.remove_cursor_tags(self.cursor.as_slice());

        // A '\n' indicates that the command has been "entered".
        if let Some('\n') = text.iter_chars_at(0).last() {
            let cmd = text.iter_chars_at(self.prompt.read().chars().count()).collect::<String>();
            let _ = self.commands.read().try_exec(cmd);
        };
    }
}

impl<U> DownCastableData for CommandLine<U>
where
    U: Ui + 'static
{
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

/// Adds the commands of the [`CommandLine<U>`] to the [`Manager`]'s
/// [`Commands`].
fn add_commands<U>(manager: &Manager<U>, command_line: &CommandLine<U>)
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
