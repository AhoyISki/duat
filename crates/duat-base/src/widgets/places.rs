//! A [`Widget`] to show a list of places to jump to.
//!
//! These will be listed in a spawned widget, and may or may not point
//! to real places in files.
use duat_core::{text::Text, ui::Widget};

/// A [`Widget`] that is mostly used to jump to locations in
/// [`Buffer`]s.
///
///
pub struct Places {
    text: Text,
}

impl Widget for Places {
    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> duat_core::text::TextMut<'_> {
        self.text.as_mut()
    }
}
