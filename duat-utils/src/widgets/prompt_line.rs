//! A multi-purpose widget
//!
//! This widget serves many purposes, of note is [running commands],
//! but it can also be used for [incremental search],
//! [piping selections], and even [user made functionalities].
//!
//! These functionalities are all controled by a [`Mode`] that has
//! control over the [`Text`] in the [`PromptLine`]. By default, that
//! is the [`Prompt`] mode, which can take in a [`PromptMode`] in
//! order to determine how the [`Text`] will be interpreted.
//!
//! [running commands]: crate::mode::RunCommands
//! [incremental search]: crate::mode::IncSearch
//! [piping selections]: crate::mode::PipeSelections
//! [user made functionalities]: PromptMode
//! [`Mode`]: crate::mode::Mode
use std::{any::TypeId, collections::HashMap, marker::PhantomData};

use duat_core::prelude::*;

use crate::modes::PromptMode;

impl<U: Ui> WidgetCfg<U> for PromptLineCfg<U> {
    type Widget = PromptLine<U>;

    fn build(self, _: &mut Pass, _: Option<FileHandle<U>>) -> (Self::Widget, PushSpecs) {
        let specs = if hook::group_exists("HidePromptLine") {
            self.specs.with_ver_len(0.0)
        } else {
            self.specs
        };

        let widget = PromptLine {
            text: Text::default(),
            prompts: HashMap::new(),
            _ghost: PhantomData,
        };

        (widget, specs)
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
/// - [`PipeSelections`]: Will pass each selection to a shell command,
///   replacing the selections with the `stdout`.
///
/// [`Prompt`]: crate::mode::Prompt
/// [`Mode`]: crate::mode::Mode
/// [`RunCommands`]: crate::mode::RunCommands
/// [`IncSearch`]: crate::mode::IncSearch
/// [`File`]: super::File
/// [`IncSearcher`]: crate::mode::IncSearcher
/// [`PipeSelections`]: crate::mode::PipeSelections
pub struct PromptLine<U> {
    text: Text,
    prompts: HashMap<TypeId, Text>,
    _ghost: PhantomData<U>,
}

impl<U: Ui> PromptLine<U> {
    /// Returns the prompt for a [`PromptMode`] if there is any
    pub fn prompt_of<M: PromptMode<U>>(&self) -> Option<Text> {
        self.prompts.get(&TypeId::of::<M>()).cloned()
    }

    /// Sets the prompt for the given [`PromptMode`]
    pub fn set_prompt<M: PromptMode<U>>(&mut self, text: Text) {
        self.prompts.entry(TypeId::of::<M>()).or_insert(text);
    }
}

impl<U: Ui> Widget<U> for PromptLine<U> {
    type Cfg = PromptLineCfg<U>;

    fn update(_: &mut Pass, _: RwData<Self>, _: &<U as Ui>::Area) {}

    fn on_unfocus(pa: &mut Pass, widget: RwData<Self>, _: &<U as Ui>::Area) {
        widget.write(pa, |wid| wid.text = Text::new());
    }

    fn needs_update(&self) -> bool {
        false
    }

    fn cfg() -> Self::Cfg {
        Self::Cfg {
            prompts: HashMap::new(),
            specs: PushSpecs::below().with_ver_len(1.0),
            _ghost: PhantomData,
        }
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn print_cfg(&self) -> PrintCfg {
        PrintCfg::default_for_input().with_forced_horizontal_scrolloff()
    }

    fn once() -> Result<(), Text> {
        form::set_weak("Prompt", "DefaultOk");
        form::set_weak("Prompt.colon", "AccentOk");
        Ok(())
    }
}

#[doc(hidden)]
pub struct PromptLineCfg<U> {
    prompts: HashMap<TypeId, Text>,
    specs: PushSpecs,
    _ghost: PhantomData<U>,
}

impl<U: Ui> PromptLineCfg<U> {
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

    /// Pushes this [`PromptLine`] to the left
    pub fn left_ratioed(self, den: u16, div: u16) -> Self {
        Self {
            specs: PushSpecs::left().with_hor_ratio(den, div),
            ..self
        }
    }
}
