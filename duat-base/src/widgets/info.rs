//! The simplest widget, just shows [`Text`]
//!
//! This is a very simple `Widget`, it basically just exists so I
//! don't have to define a new `Widget` every time I want to show
//! static information.
use duat_core::{
    context::Handle,
    data::Pass,
    mode::{MouseEvent, MouseEventKind},
    opts::PrintOpts,
    text::{Text, TextMut},
    ui::Widget,
};

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
    pub text: Text,
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
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        let (info, area) = handle.write_with_area(pa);
        let _ = area.set_width(
            area.width_of_text(info.get_print_opts(), &info.text)
                .unwrap(),
        );
    }

    fn needs_update(&self, _: &Pass) -> bool {
        false
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> TextMut<'_> {
        self.text.as_mut()
    }

    fn on_mouse_event(pa: &mut Pass, handle: &Handle<Self>, event: MouseEvent) {
        use MouseEventKind::{ScrollDown, ScrollUp};
        match event.kind {
            ScrollDown | ScrollUp => {
                let (info, area) = handle.write_with_area(pa);
                let scroll = if let ScrollDown = event.kind { 3 } else { -3 };
                area.scroll_ver(&info.text, scroll, info.get_print_opts());
            }
            _ => {}
        }
    }

    fn get_print_opts(&self) -> PrintOpts {
        let mut opts = PrintOpts::new();
        opts.wrap_lines = true;
        opts
    }
}
