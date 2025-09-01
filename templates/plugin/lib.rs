// This is mostly for convenience and testing, you can change it for
// something else
#![doc = include_str!("README.md")]
use duat_core::prelude::*;

/// A plugin for ...
///
/// A longer description for this plugin is ...
pub struct PluginName(usize, bool);

impl PluginName {
    /// Sets the `usize` value for this [`Plugin`]
    pub fn set_usize(self, value: usize) -> Self {
        Self(value, self.1)
    }

    /// Sets the `bool` value for this [`Plugin`]
    pub fn set_bool(self, value: bool) -> Self {
        Self(self.0, value)
    }
}

impl<U: Ui> Plugin<U> for PluginName {
    impl plug(self, plugins: &Plugins<U>) {
        // Plugin requirementes:
        // plugins.require::<SomeOtherPlugin<U>>();

        // Further processing, like adding hooks, parsers, widgets, etc.
        // ...
    }
}
