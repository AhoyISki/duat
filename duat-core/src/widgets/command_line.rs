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

/// A multi purpose text [`Widget`]
///
/// This [`Widget`] will be used by a [`Prompt`]-type [`Mode`], which
/// in turn will make use of a [`PromptMode`]. These are "ways of
/// interpreting the input". In Duat, there are 3 built-in
/// [`PromptMode`]s:
///
/// - [`RunCommands`]: Will interpret the prompt as a Duat command to
///   be executed.
/// - [`IncSearch`]: Will read the prompt as a regex, and modify the
///   active [`File`] according to a given [`IncSearcher`].
/// - [`PipeSelections`]: Will pass each selection to a shell
///   command, replacing the selections with the `stdout`.
///
/// [`Prompt`]: crate::mode::Prompt
/// [`Mode`]: crate::mode::Mode
/// [`RunCommands`]: crate::mode::RunCommands
/// [`IncSearch`]: crate::mode::IncSearch
/// [`IncSearcher`]: crate::mode::IncSearcher
/// [`PipeSelections`]: crate::mode::PipeSelections
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
        Self::Cfg {
            prompts: HashMap::new(),
            specs: PushSpecs::below().with_ver_len(1.0),
            _ghost: PhantomData,
        }
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

#[doc(hidden)]
pub struct CmdLineCfg<U> {
    prompts: HashMap<TypeId, Text>,
    specs: PushSpecs,
    _ghost: PhantomData<U>,
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
