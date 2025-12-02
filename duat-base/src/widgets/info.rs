//! The simplest widget, just shows [`Text`]
//!
//! This is a very simple `Widget`, it basically just exists so I
//! don't have to define a new `Widget` every time I want to show
//! static information.
use duat_core::{context::Handle, data::Pass, text::Text, ui::Widget};

/// A simple static widget, meant to simply convery information
///
/// This is the most flexible of widgets, you can use it anywhere,
/// just by pushing it around, or spawning it however you want, by
/// making use of the [`PushSpecs`], [`DynSpawnSpecs`] o
/// [`StaticSpawnSpecs`].
///
/// [`PushSpecs`]: duat_core::ui::PushSpecs
/// [`DynSpawnSpecs`]: duat_core::ui::DynSpawnSpecs
/// [`StaticSpawnSpecs`]: duat_core::ui::StaticSpawnSpecs
pub struct Info {
    text: Text,
}

impl Info {
    /// Returns a new `Info` widget
    ///
    /// This is the only [`Widget`] in `duat-base` that can be
    /// acquired this way, since it's supposed to be versatile in
    /// where you position it. Every other widget has to be placed in
    /// specific locations, so they don't offer a `new` method, which
    /// could be used in order to place them willy nilly.
    pub fn new(text: Text) -> Self {
        Self { text }
    }
}

impl Widget for Info {
    fn update(_: &mut Pass, _: &Handle<Self>) {}

    fn needs_update(&self, _: &Pass) -> bool {
        false
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }
}
