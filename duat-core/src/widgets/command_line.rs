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
        Arc,
    },
};

use parking_lot::RwLock;

use crate::{
    data::{Context, CurFile, RoData, RwData},
    forms::{self, Form},
    hooks,
    input::{Commander, InputMethod},
    text::{text, Ghost, PrintCfg, SavedMatches, Text},
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
        self.mode.read().read().update(&mut self.text);
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn print_cfg(&self) -> crate::text::PrintCfg {
        PrintCfg::default_for_input().with_forced_scrolloff()
    }

    fn once(context: Context<U>) {
        forms::set_weak("Prompt", Form::new().cyan());

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
        self.mode.read().read().on_focus(&mut self.text);
    }

    fn on_unfocus(&mut self, _area: &<U as Ui>::Area) {
        self.mode.read().read().on_unfocus(&mut self.text);
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

impl<U> CommandLineMode<U> for RunCommands<U>
where
    U: Ui,
{
    fn new(context: Context<U>) -> Self
    where
        Self: Sized,
    {
        context.switch_to::<CommandLine<U>>();
        Self(context)
    }

    fn on_unfocus(&self, text: &mut Text) {
        let text = std::mem::take(text);

        let cmd = text.to_string();
        if !cmd.is_empty() {
            let context = self.0;
            crate::thread::spawn(move || context.run_cmd_notify(cmd));
        }
    }
}

pub struct ShowNotifications {
    notifications: &'static RwData<Text>,
    has_changed: AtomicBool,
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
            has_changed: AtomicBool::default(),
        }
    }

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

pub struct IncSearch<U>
where
    U: Ui,
{
    list: RwData<Vec<SavedMatches>>,
    cur_file: &'static CurFile<U>,
}

impl<U> CommandLineMode<U> for IncSearch<U>
where
    U: Ui,
{
    fn new(context: Context<U>) -> Self
    where
        Self: Sized,
    {
        let inc_search = Self {
            list: RwData::default(),
            cur_file: context.cur_file().unwrap(),
        };

        crate::thread::spawn(move || {
            inc_search
                .cur_file
                .mutate_dyn_input(|input| input.write().begin_inc_search());
            context.switch_to::<CommandLine<U>>();
        });

        inc_search
    }

    fn update(&self, text: &mut Text) {
        let mut list = self.list.write();
        if let Some(saved) = list.iter_mut().find(|s| s.pat_is(text)) {
            let searcher = saved.searcher();
            self.cur_file
                .mutate_dyn_input(|input| input.write().search_inc(searcher))
        } else if let Ok(mut saved) = SavedMatches::new(text.to_string()) {
            if let Some(prev) = list.iter().find(|s| s.is_prefix_of(&saved)) {
                saved.take_matches_from(prev);
            }

            let searcher = saved.searcher();
            self.cur_file
                .mutate_dyn_input(|input| input.write().search_inc(searcher));

            list.push(saved);
        }
    }
}

impl<U> Drop for IncSearch<U>
where
    U: Ui,
{
    fn drop(&mut self) {
        self.cur_file
            .mutate_dyn_input(|input| input.write().end_inc_search())
    }
}
