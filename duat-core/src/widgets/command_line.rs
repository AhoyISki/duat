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
use std::{marker::PhantomData, sync::Arc};

use parking_lot::RwLock;

use super::File;
use crate::{
    data::{Context, RoData, RwData},
    forms::{self, Form},
    hooks,
    input::{Commander, InputMethod},
    log_info,
    text::{Ghost, Key, PrintCfg, SavedMatches, Tag, Text, text},
    ui::{Area, PushSpecs, Ui},
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
        let mode = RwData::new(if hooks::group_exists("CmdLineNotifications") {
            RwData::<dyn CommandLineMode<U>>::new_unsized::<ShowNotifications>(Arc::new(
                RwLock::new(ShowNotifications::new(context)),
            ))
        } else {
            RwData::<dyn CommandLineMode<U>>::new_unsized::<RunCommands<U>>(Arc::new(RwLock::new(
                RunCommands::new(context),
            )))
        });

        let cmd_line = CommandLine {
            text: Text::new(),
            prompt: RwData::new(self.prompt.clone()),
            mode,
        };

        let checker = {
            let mode = RoData::from(&cmd_line.mode);
            move || mode.read().write().has_changed()
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

    pub(crate) fn set_mode<M: CommandLineMode<U>>(&mut self, context: Context<U>) {
        *self.mode.write() = RwData::new_unsized::<M>(Arc::new(RwLock::new(M::new(context))));
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
        self.mode.read().write().update(&mut self.text);
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn print_cfg(&self) -> crate::text::PrintCfg {
        PrintCfg::default_for_input().with_forced_scrolloff()
    }

    fn once(context: Context<U>) {
        forms::set_weak("Prompt", Form::cyan());
        forms::set_weak("ParseCommandErr", "DefaultErr");

        context
            .add_cmd_for_widget::<CommandLine<U>>(
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

impl<U> ActiveWidget<U> for CommandLine<U>
where
    U: Ui,
{
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

unsafe impl<U> Send for CommandLine<U> where U: Ui {}

pub trait CommandLineMode<U>: Sync + Send + 'static
where
    U: Ui,
{
    fn new(context: Context<U>) -> Self
    where
        Self: Sized;

    fn on_focus(&mut self, _text: &mut Text) {}

    fn on_unfocus(&mut self, _text: &mut Text) {}

    fn update(&mut self, _text: &mut Text) {}

    fn has_changed(&mut self) -> bool {
        false
    }
}

pub struct RunCommands<U>(Context<U>)
where
    U: Ui;

impl<U> CommandLineMode<U> for RunCommands<U>
where
    U: Ui,
{
    fn new(context: Context<U>) -> Self
    where
        Self: Sized,
    {
        crate::switch_to::<CommandLine<U>>();
        Self(context)
    }

    fn on_unfocus(&mut self, text: &mut Text) {
        let text = std::mem::take(text);

        let cmd = text.to_string();
        if !cmd.is_empty() {
            let context = self.0;
            crate::thread::queue(move || context.run_cmd_notify(cmd));
        }
    }
}

pub struct ShowNotifications {
    notifications: &'static RwData<Text>,
    has_changed: bool,
}

impl<U> CommandLineMode<U> for ShowNotifications
where
    U: Ui,
{
    fn new(context: Context<U>) -> Self
    where
        Self: Sized,
    {
        Self {
            notifications: context.notifications(),
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

pub struct IncSearch<U>
where
    U: Ui,
{
    list: RwData<Vec<SavedMatches>>,
    context: Context<U>,
    error_range: Option<(usize, usize)>,
    key: Key,
}

impl<U> CommandLineMode<U> for IncSearch<U>
where
    U: Ui,
{
    fn new(context: Context<U>) -> Self
    where
        Self: Sized,
    {
        crate::switch_to::<CommandLine<U>>();
        Self {
            list: RwData::default(),
            context,
            error_range: None,
            key: Key::new(),
        }
    }

    fn update(&mut self, text: &mut Text) {
        if let Some((start, end)) = self.error_range.take() {
            text.remove_tags_on(start, self.key);
            text.remove_tags_on(end, self.key);
        }

        let cur_file = self.context.cur_file().unwrap();
        let mut list = self.list.write();

        if let Some(saved) = list.iter_mut().find(|s| s.pat_is(text)) {
            let searcher = saved.searcher();
            cur_file.mutate_data(|file, area, input| {
                self.update_inc_search(file, area, input, searcher);
            });
        } else {
            match SavedMatches::new(text.to_string()) {
                Ok(mut saved) => {
                    if let Some(prev) = list.iter().rev().find(|s| s.is_prefix_of(&saved)) {
                        saved.take_matches_from(prev);
                    }

                    let searcher = saved.searcher();
                    cur_file.mutate_data(|file, area, input| {
                        self.update_inc_search(file, area, input, searcher);
                    });

                    list.push(saved);
                }
                Err(error) => {
                    let regex_syntax::Error::Parse(error) = *error else {
                        unreachable!("As far as I can tell, this would be a bug with regex_syntax");
                    };

                    let span = error.span();
                    let id = crate::forms::to_id!("ParseCommandErr");

                    text.insert_tag(span.start.offset, Tag::PushForm(id), self.key);
                    text.insert_tag(span.end.offset, Tag::PopForm(id), self.key);
                }
            }
        }
    }

    fn on_focus(&mut self, _text: &mut Text) {
        let cur_file = self.context.cur_file().unwrap();
        cur_file.mutate_data(|file, area, input| {
            input.write().begin_inc_search(file, area, self.context)
        });
    }

    fn on_unfocus(&mut self, _text: &mut Text) {
        let cur_file = self.context.cur_file().unwrap();
        cur_file.mutate_data(|file, area, input| {
            input.write().end_inc_search(file, area, self.context)
        });
    }
}

impl<U> IncSearch<U>
where
    U: Ui,
{
    fn update_inc_search(
        &self,
        file: &RwData<File>,
        area: &<U as Ui>::Area,
        input: &RwData<dyn InputMethod<U>>,
        searcher: crate::text::Searcher,
    ) {
        let mut input = input.write();
        if let Some(cursors) = input.cursors() {
            <File as ActiveWidget<U>>::text_mut(&mut file.write()).remove_cursor_tags(cursors);
        }

        input.search_inc(file, area, self.context, searcher);

        let mut file = file.write();

        if let Some(cursors) = input.cursors() {
            <File as ActiveWidget<U>>::text_mut(&mut file).add_cursor_tags(cursors);

            area.scroll_around_point(
                file.text(),
                cursors.main().caret(),
                <File as PassiveWidget<U>>::print_cfg(&file),
            );
        }

        <File as PassiveWidget<U>>::update(&mut file, area);
        <File as PassiveWidget<U>>::print(&mut file, area);
    }
}
