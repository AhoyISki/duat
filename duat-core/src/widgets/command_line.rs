//! A [`Widget`] that can have varying functionality
//!
//! Its primary purpose, as the name implies, is to run [commands],
//! but it can also [show notifications], do [incremental search], and
//! you can even [implement your own functionality] for the
//! [`CmdLine`].
//!
//! [commands]: cmd
//! [show notifications]: ShowNotifications
//! [incremental search]: IncSearch
//! [implement your own functionality]: CmdLineMode
use std::{
    any::TypeId,
    marker::PhantomData,
    sync::{
        Arc, LazyLock,
        atomic::{AtomicBool, Ordering},
    },
};

use parking_lot::RwLock;

use super::File;
use crate::{
    cfg::PrintCfg,
    cmd,
    data::{RoData, RwData, context},
    form::{self, Form},
    hooks::{self, KeySent},
    mode::{self, Command, IncSearcher},
    text::{Ghost, Key, Searcher, Tag, Text, text},
    ui::{PushSpecs, Ui},
    widgets::{Widget, WidgetCfg},
};

pub struct CmdLineCfg<U> {
    prompt: String,
    specs: PushSpecs,
    ghost: PhantomData<U>,
}

impl<U> CmdLineCfg<U> {
    pub fn new() -> Self {
        CmdLineCfg {
            prompt: String::from(":"),
            specs: PushSpecs::below().with_ver_len(1.0),
            ghost: PhantomData,
        }
    }
}

impl<U: Ui> Default for CmdLineCfg<U> {
    fn default() -> Self {
        Self::new()
    }
}

impl<U: Ui> CmdLineCfg<U> {
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
}

impl<U: Ui> WidgetCfg<U> for CmdLineCfg<U> {
    type Widget = CmdLine<U>;

    fn build(self, _: bool) -> (Self::Widget, impl Fn() -> bool, PushSpecs) {
        let mode: RwData<dyn CmdLineMode<U>> = if hooks::group_exists("CmdLineNotifications") {
            run_once::<ShowNotifications<U>, U>();
            RwData::new_unsized::<ShowNotifications<U>>(Arc::new(RwLock::new(
                ShowNotifications::new(),
            )))
        } else {
            run_once::<RunCommands<U>, U>();
            RwData::new_unsized::<RunCommands<U>>(Arc::new(RwLock::new(RunCommands::new())))
        };

        let widget = CmdLine {
            text: Text::default(),
            prompt: RwData::new(self.prompt.clone()),
            mode: RwData::new(mode),
        };

        let checker = {
            let mode = RoData::from(&widget.mode);
            move || mode.read().write().has_changed()
        };

        (widget, checker, self.specs)
    }
}

/// A multi purpose text widget
///
/// This widget, as the name implies, is most associated with running
/// commands. However, it can have a variety of [modes], granting it
/// differing functionality. In Duat, there are 3 predefined modes:
///
/// * [`RunCommands`], which runs commands (duh);
/// * [`ShowNotifications`], which shows notifications, usually about
///   commands;
/// * [`IncSearch<Inc>`], which will perform an incremental search,
///   based on [`Inc`].
///
/// By default, Duat will have the `"CmdLineNotifications"` [hook]
/// active. This hook changes the mode of the [`CmdLine`] to
/// [`ShowNotifications`] whenever it is unfocused. If you don't want
/// this functionality, or want notifications somewhere else, you can
/// use [`hooks::remove`].
///
/// [modes]: CmdLineMode
/// [`Inc`]: IncSearcher
/// [hook]: crate::hooks
pub struct CmdLine<U: Ui> {
    text: Text,
    prompt: RwData<String>,
    mode: RwData<RwData<dyn CmdLineMode<U>>>,
}

impl<U: Ui> CmdLine<U> {
    pub(crate) fn set_mode<M: CmdLineMode<U>>(&mut self, mode: M) {
        run_once::<M, U>();
        *self.mode.write() = RwData::new_unsized::<M>(Arc::new(RwLock::new(mode)));
    }
}

impl<U: Ui> Widget<U> for CmdLine<U> {
    type Cfg = CmdLineCfg<U>;

    fn cfg() -> Self::Cfg {
        CmdLineCfg::new()
    }

    fn update(&mut self, _area: &<U as Ui>::Area) {
        self.mode.read().write().update(&mut self.text);
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn print_cfg(&self) -> PrintCfg {
        PrintCfg::default_for_input().with_forced_scrolloff()
    }

    fn once() -> Result<(), crate::Error<()>> {
        form::set_weak("Prompt", Form::cyan());
        form::set_weak("ParseCommandErr", "DefaultErr");

        cmd::add_for!(
            "set-prompt",
            |cmd_line: CmdLine<U>, _: U::Area, new: String| {
                *cmd_line.prompt.write() = new;
                Ok(None)
            }
        )
    }

    fn on_focus(&mut self, _area: &U::Area) {
        self.text = text!({ Ghost(text!({ &self.prompt })) });
        self.mode.read().write().on_focus(&mut self.text);
    }

    fn on_unfocus(&mut self, _area: &<U as Ui>::Area) {
        self.mode.read().write().on_unfocus(&mut self.text);
    }
}

pub trait CmdLineMode<U: Ui>: Send + Sync + 'static {
    fn clone(&self) -> Self
    where
        Self: Sized;

    fn on_focus(&mut self, _text: &mut Text) {}

    fn on_unfocus(&mut self, _text: &mut Text) {}

    fn update(&mut self, _text: &mut Text) {}

    fn has_changed(&mut self) -> bool {
        false
    }

    fn once()
    where
        Self: Sized,
    {
    }
}

pub struct RunCommands<U> {
    key: Key,
    ghost: PhantomData<U>,
}

impl<U: Ui> RunCommands<U> {
    pub fn new() -> Self {
        mode::set::<U>(Command);
        Self { key: Key::new(), ghost: PhantomData }
    }
}

impl<U: Ui> CmdLineMode<U> for RunCommands<U> {
    fn clone(&self) -> Self {
        Self::new()
    }

    fn update(&mut self, text: &mut Text) {
        text.remove_tags_on(.., self.key);

        let command = text.to_string();
        let caller = command.split_whitespace().next();
        if let Some(caller) = caller {
            if let Some((ok_ranges, err_range)) = cmd::check_params(&command) {
                let id = form::id_of!("CallerExists");
                text.insert_tag(0, Tag::PushForm(id), self.key);
                text.insert_tag(caller.len(), Tag::PopForm(id), self.key);

                let id = form::id_of!("ParameterOk");
                for range in ok_ranges {
                    text.insert_tag(range.start, Tag::PushForm(id), self.key);
                    text.insert_tag(range.end, Tag::PopForm(id), self.key);
                }
                if let Some((range, _)) = err_range {
                    let id = form::id_of!("ParameterErr");
                    text.insert_tag(range.start, Tag::PushForm(id), self.key);
                    text.insert_tag(range.end, Tag::PopForm(id), self.key);
                }
            } else {
                let id = form::id_of!("CallerNotFound");
                text.insert_tag(0, Tag::PushForm(id), self.key);
                text.insert_tag(caller.len(), Tag::PopForm(id), self.key);
            }
        }
    }

    fn on_unfocus(&mut self, text: &mut Text) {
        let text = std::mem::take(text);

        let cmd = text.to_string();
        if !cmd.is_empty() {
            crate::thread::queue(move || cmd::run_notify(cmd));
        }
    }

    fn once() {
        form::set_weak("CallerExists", "AccentOk");
        form::set_weak("CallerNotFound", "AccentErr");
        form::set_weak("ParameterOk", "DefaultOk");
        form::set_weak("ParameterErr", "DefaultErr");
    }
}

impl<U: Ui> Default for RunCommands<U> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ShowNotifications<U> {
    notifications: RwData<Text>,
    text: Text,
    ghost: PhantomData<U>,
}

impl<U: Ui> ShowNotifications<U> {
    pub fn new() -> Self {
        Self {
            notifications: context::notifications().clone(),
            text: Text::default(),
            ghost: PhantomData,
        }
    }
}

static REMOVE_NOTIFS: AtomicBool = AtomicBool::new(false);

impl<U: Ui> CmdLineMode<U> for ShowNotifications<U> {
    fn clone(&self) -> Self {
        Self::new()
    }

    fn has_changed(&mut self) -> bool {
        if self.notifications.has_changed() {
            REMOVE_NOTIFS.store(false, Ordering::Release);
            self.text = self.notifications.read().clone();
            true
        } else {
            !self.text.is_empty() && REMOVE_NOTIFS.load(Ordering::Acquire)
        }
    }

    fn update(&mut self, text: &mut Text) {
        if REMOVE_NOTIFS.load(Ordering::Acquire) {
            self.text = Text::default();
            REMOVE_NOTIFS.store(false, Ordering::Release);
        }
        if self.notifications.has_changed() {
            self.text = self.notifications.read().clone();
        }
        *text = self.text.clone();
    }

    fn once()
    where
        Self: Sized,
    {
        hooks::add::<KeySent<U>>(|_| {
            REMOVE_NOTIFS.store(true, Ordering::Release);
        });
    }
}

impl<U: Ui> Default for ShowNotifications<U> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct IncSearch<I: IncSearcher<U>, U: Ui> {
    fn_or_inc: FnOrInc<I, U>,
    key: Key,
    ghost: PhantomData<U>,
}

impl<I: IncSearcher<U>, U: Ui> IncSearch<I, U> {
    pub fn new(f: impl IncFn<I, U> + Send + Sync + 'static) -> Self {
        mode::set::<U>(Command);

        Self {
            fn_or_inc: FnOrInc::Fn(Some(Box::new(f))),
            key: Key::new(),
            ghost: PhantomData,
        }
    }
}

impl<I: IncSearcher<U>, U: Ui> CmdLineMode<U> for IncSearch<I, U> {
    fn clone(&self) -> Self {
        Self::new(I::new)
    }

    fn update(&mut self, text: &mut Text) {
        let FnOrInc::Inc(inc, _) = &mut self.fn_or_inc else {
            unreachable!();
        };

        text.remove_tags_on(.., self.key);

        let cur_file = context::cur_file::<U>().unwrap();

        match Searcher::new(text.to_string()) {
            Ok(searcher) => {
                cur_file.mutate_data(|file, area| {
                    inc.search(file, area, searcher);
                });
            }
            Err(err) => {
                let regex_syntax::Error::Parse(err) = *err else {
                    unreachable!("As far as I can tell, this would be a bug with regex_syntax");
                };

                let span = err.span();
                let id = crate::form::id_of!("ParseCommandErr");

                text.insert_tag(span.start.offset, Tag::PushForm(id), self.key);
                text.insert_tag(span.end.offset, Tag::PopForm(id), self.key);
            }
        }
    }

    fn on_focus(&mut self, _text: &mut Text) {
        context::cur_file::<U>().unwrap().mutate_data(|file, area| {
            self.fn_or_inc.as_inc(file, area);
        })
    }

    fn on_unfocus(&mut self, _text: &mut Text) {
        let FnOrInc::Inc(inc, _) = &mut self.fn_or_inc else {
            unreachable!();
        };

        context::cur_file::<U>()
            .unwrap()
            .mutate_data(|file, area| inc.finish(file, area));
    }
}

/// Runs the [`once`] function of widgets.
///
/// [`once`]: Widget::once
fn run_once<M: CmdLineMode<U>, U: Ui>() {
    static LIST: LazyLock<RwData<Vec<TypeId>>> = LazyLock::new(|| RwData::new(Vec::new()));

    let mut list = LIST.write();
    if !list.contains(&TypeId::of::<M>()) {
        M::once();
        list.push(TypeId::of::<M>());
    }
}

enum FnOrInc<I, U: Ui> {
    Fn(Option<Box<dyn IncFn<I, U> + Send + Sync>>),
    Inc(I, PhantomData<U>),
}

impl<I, U: Ui> FnOrInc<I, U> {
    fn as_inc(&mut self, file: &RwData<File>, area: &U::Area) {
        let FnOrInc::Fn(f) = self else {
            unreachable!();
        };

        let inc = f.take().unwrap()(file, area);
        *self = FnOrInc::Inc(inc, PhantomData);
    }
}

trait IncFn<I, U: Ui> = FnOnce(&RwData<File>, &U::Area) -> I;
