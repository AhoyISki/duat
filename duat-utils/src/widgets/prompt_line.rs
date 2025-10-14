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
//! [`Prompt`]: crate::modes::Prompt
use std::{any::TypeId, collections::HashMap, marker::PhantomData};

use duat_core::{
    prelude::*,
    ui::{PushTarget, Side},
};

use crate::modes::PromptMode;

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
/// [`Prompt`]: crate::modes::Prompt
/// [`Mode`]: duat_core::mode::Mode
/// [`RunCommands`]: crate::modes::RunCommands
/// [`IncSearch`]: crate::modes::IncSearch
/// [`File`]: duat_core::file::File
/// [`IncSearcher`]: crate::modes::IncSearcher
/// [`PipeSelections`]: crate::modes::PipeSelections
pub struct PromptLine<U: Ui> {
    text: Text,
    prompts: HashMap<TypeId, Text>,
    _ghost: PhantomData<U>,
}

impl<U: Ui> PromptLine<U> {
    /// Returns a [`PromptLineBuilder`], which can be used to push
    /// `PromptLine`s around
    pub fn builder() -> PromptLineBuilder<U> {
        PromptLineBuilder::default()
    }

    /// Returns the prompt for a [`PromptMode`] if there is any
    pub fn prompt_of<M: PromptMode<U>>(&self) -> Option<Text> {
        self.prompts.get(&TypeId::of::<M>()).cloned()
    }

    /// Sets the prompt for the given [`PromptMode`]
    pub fn set_prompt<M: PromptMode<U>>(&mut self, text: Text) {
        self.prompts.entry(TypeId::of::<M>()).or_insert(text);
    }

    /// Returns the prompt for a [`TypeId`], if there is any
    pub fn prompt_of_id(&self, id: TypeId) -> Option<Text> {
        self.prompts.get(&id).cloned()
    }
}

impl<U: Ui> Widget<U> for PromptLine<U> {
    fn update(pa: &mut Pass, handle: &Handle<Self, U>) {
        let pl = handle.read(pa);
        if let Some(main) = pl.text.selections().get_main() {
            handle
                .area(pa)
                .scroll_around_point(&pl.text, main.caret(), pl.get_print_cfg());
        }
    }

    fn needs_update(&self, _: &Pass) -> bool {
        false
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn get_print_cfg(&self) -> PrintCfg {
        *PrintCfg::default_for_input().set_forced_horizontal_scrolloff(true)
    }
}

#[doc(hidden)]
#[derive(Default)]
pub struct PromptLineBuilder<U: Ui> {
    prompts: Option<HashMap<TypeId, Text>> = None,
    specs: PushSpecs = PushSpecs { side: Side::Below, height: Some(1.0), .. },
    _ghost: PhantomData<U> = PhantomData
}

impl<U: Ui> PromptLineBuilder<U> {
    pub fn push_on(
        self,
        pa: &mut Pass,
        push_target: &impl PushTarget<U>,
    ) -> Handle<PromptLine<U>, U> {
        let prompt_line = PromptLine {
            text: Text::default(),
            prompts: self.prompts.unwrap_or_default(),
            _ghost: PhantomData,
        };

        push_target.push_outer(pa, prompt_line, self.specs)
    }

    /// Changes the default [prompt] for a given [mode]
    ///
    /// [prompt]: Text
    /// [mode]: PromptMode
    pub fn set_prompt<M: PromptMode<U>>(mut self, prompt: Text) -> Self {
        self.prompts
            .get_or_insert_default()
            .insert(TypeId::of::<M>(), prompt);
        self
    }

    /// Places the [`PromptLine`] above, as opposed to below
    pub fn above(self) -> Self {
        Self {
            specs: PushSpecs { side: Side::Above, ..self.specs },
            ..self
        }
    }

    /// Places the [`PromptLine`] below, this is the default
    pub fn below(self) -> Self {
        Self {
            specs: PushSpecs { side: Side::Below, ..self.specs },
            ..self
        }
    }

    /// Hides the [`PromptLine`] by default
    pub fn hidden(self) -> Self {
        Self {
            specs: PushSpecs { hidden: true, ..self.specs },
            ..self
        }
    }

    /// Push to the left, to be used by [`FooterWidgets`]
    ///
    /// [`FooterWidgets`]: crate::widgets::FooterWidgets
    pub(crate) fn left(self) -> Self {
        Self {
            specs: PushSpecs { side: Side::Left, ..self.specs },
            ..self
        }
    }
}
