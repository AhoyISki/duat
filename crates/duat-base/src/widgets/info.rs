//! The simplest widget, just shows [`Text`]
//!
//! This is a very simple `Widget`, it basically just exists so I
//! don't have to define a new `Widget` every time I want to show
//! static information.
use std::sync::Once;

use duat_core::{
    context::Handle,
    data::Pass,
    hook::{self, OnMouseEvent, WidgetOpened},
    mode::MouseEventKind,
    opts::PrintOpts,
    text::{Text, TextMut},
    ui::Widget,
};

/// Adds the hooks for the [`Info`] widget.
pub fn add_info_hooks() {
    use MouseEventKind::{ScrollDown, ScrollUp};

    hook::add::<OnMouseEvent<Info>>(|pa, (info, event)| match event.kind {
        ScrollDown | ScrollUp => {
            let (info, area) = info.write_with_area(pa);
            let scroll = if let ScrollDown = event.kind { 3 } else { -3 };
            area.scroll_ver(&info.text, scroll, info.print_opts());
        }
        _ => {}
    });

    hook::add::<WidgetOpened<Info>>(|pa, info| {
        let (info, area) = info.write_with_area(pa);
        let size = area.size_of_text(info.print_opts(), &info.text).unwrap();
        _ = area.set_width(size.x);
        _ = area.set_height(size.y);
    });
}

/// A simple static widget, meant to just convey information
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
    /// The [`Text`] that will be shown by this widget
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
        static ONCE: Once = Once::new();
        ONCE.call_once(|| {});

        Self { text }
    }

    /// Mutate the [`Text`] of this `Info`.
    ///
    /// This will also resize the widget to fit as much of it as
    /// possible.
    pub fn set_text(pa: &mut Pass, info: &Handle<Self>, func: impl FnOnce(&mut Text)) {
        let (info, area) = info.write_with_area(pa);
        func(&mut info.text);

        let size = area.size_of_text(info.print_opts(), &info.text).unwrap();
        _ = area.set_width(size.x);
        _ = area.set_height(size.y);
    }
}

impl Widget for Info {
    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> TextMut<'_> {
        self.text.as_mut()
    }

    fn print_opts(&self) -> PrintOpts {
        let mut opts = PrintOpts::new();
        opts.wrap_lines = true;
        opts.tabstop = 2;
        opts
    }
}
