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
use std::{any::TypeId, collections::HashMap, marker::PhantomData};

use crate::{
    cfg::PrintCfg,
    form, hooks,
    mode::PromptMode,
    text::Text,
    ui::{PushSpecs, Ui},
    widgets::{Widget, WidgetCfg},
};

pub struct CmdLineCfg<U> {
    prompts: HashMap<TypeId, Text>,
    specs: PushSpecs,
    _ghost: PhantomData<U>,
}

impl<U> CmdLineCfg<U> {
    pub fn new() -> Self {
        CmdLineCfg {
            prompts: HashMap::new(),
            specs: PushSpecs::below().with_ver_len(1.0),
            _ghost: PhantomData,
        }
    }
}

impl<U: Ui> Default for CmdLineCfg<U> {
    fn default() -> Self {
        Self::new()
    }
}

impl<U: Ui> CmdLineCfg<U> {
    /// Changes the default [prompt] for a given [mode]
    ///
    /// [prompt]: Text
    /// [mode]: PromptMode
    pub fn set_prompt<M: PromptMode<U>>(mut self, prompt: Text) -> Self {
        self.prompts.insert(TypeId::of::<M>(), prompt);
        self
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
        let specs = if hooks::group_exists("HideCmdLine") {
            self.specs.with_ver_len(0.0)
        } else {
            self.specs
        };

        let widget = CmdLine {
            text: Text::default(),
            prompts: HashMap::new(),
            _ghost: PhantomData,
        };

        (widget, || false, specs)
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
pub struct CmdLine<U> {
    text: Text,
    prompts: HashMap<TypeId, Text>,
    _ghost: PhantomData<U>,
}

impl<U: Ui> CmdLine<U> {
    /// Returns the prompt for a [`CmdLineMode`] if there is any
    pub fn prompt_of<M: PromptMode<U>>(&self) -> Option<Text> {
        self.prompts.get(&TypeId::of::<M>()).cloned()
    }

    /// Sets the prompt for the given [`CmdLineMode`]
    pub fn set_prompt<M: PromptMode<U>>(&mut self, text: Text) {
        self.prompts.entry(TypeId::of::<M>()).or_insert(text);
    }
}

impl<U: Ui> Widget<U> for CmdLine<U> {
    type Cfg = CmdLineCfg<U>;

    fn cfg() -> Self::Cfg {
        CmdLineCfg::new()
    }

    fn on_unfocus(&mut self, _area: &<U as Ui>::Area) {
        self.text = Text::new();
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
        form::set_weak("Prompt", "DefaultOk");
        form::set_weak("Prompt.colon", "AccentOk");
        Ok(())
    }
}
