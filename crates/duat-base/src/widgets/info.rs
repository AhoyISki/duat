//! The simplest widget, just shows [`Text`]
//!
//! This is a very simple `Widget`, it basically just exists so I
//! don't have to define a new `Widget` every time I want to show
//! static information.
use std::sync::{LazyLock, Mutex, Once};

use duat_core::{
    Ns,
    context::{self, Handle},
    data::{Pass, RwData},
    hook::{self, KeyTyped, OnMouseEvent, WidgetOpened},
    mode::MouseEventKind,
    opts::PrintOpts,
    text::{Text, TextMut},
    txt,
    ui::{DynSpawnSpecs, Orientation, Side, Widget},
};
use duat_term::Frame;

static ORIENTATION: Mutex<Orientation> = Mutex::new(Orientation::VerRightBelow);

/// Adds the hooks for the [`Info`] widget.
pub fn info_setup() {
    use MouseEventKind::{ScrollDown, ScrollUp};

    hook::add::<OnMouseEvent<Info>>(|pa, event| match event.kind {
        ScrollDown | ScrollUp => {
            let (info, area) = event.handle.write_with_area(pa);
            let scroll = if let ScrollDown = event.kind { 3 } else { -3 };
            area.scroll_ver(&info.text, scroll as f32, info.print_opts());
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
    is_corner: bool,
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

        Self { text, is_corner: false }
    }

    /// Sets the [`Text`] for the corner `Info`.
    ///
    /// This is a special kind of `Info`, of which only one can exist
    /// at once.
    pub fn set_corner(pa: &mut Pass, text: Text, title: Option<Text>, fragile: bool) {
        static NS: LazyLock<Ns> = Ns::new_lazy();

        let info = if let Some(info) = context::windows()
            .handles_of::<Info>(pa)
            .into_iter()
            .find(|info| info.read(pa).is_corner)
        {
            Info::set_text(pa, &info, |t| *t = text);
            info
        } else {
            let orientation = *ORIENTATION.lock().unwrap();
            let buffer = context::current_buffer(pa);
            let Some(info) = buffer.spawn_widget(
                pa,
                Info { text, is_corner: true },
                DynSpawnSpecs {
                    orientation,
                    width: None,
                    height: None,
                    hidden: false,
                    inside: true,
                },
            ) else {
                return;
            };
            if let Some(term_area) = buffer.area().write_as::<duat_term::Area>(pa) {
                let mut frame = Frame::default();
                if let Some(title) = title {
                    frame.set_text(Side::Above, move |_| {
                        txt!("[terminal.border.Info]┤[]{title}[terminal.border.Info]├")
                    });
                }
                term_area.set_frame(frame)
            }

            info
        };

        if fragile && !hook::group_exists(*NS) {
            hook::add_once::<KeyTyped>(move |pa, _| {
                _ = info.close(pa);
            })
            .grouped(*NS)
            .lateness(0);
        }
    }

    /// Closes the corner [`Info`], if one was opened.
    pub fn close_corner(pa: &mut Pass) {
        if let Some(info) = context::windows()
            .handles_of::<Info>(pa)
            .into_iter()
            .find(|info| info.read(pa).is_corner)
        {
            _ = info.close(pa);
        }
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

    /// Sets the [`Orientation`] where the corner [`Info`] will be spawned.
    pub fn set_corner_orientation(orientation: Orientation) {
        *ORIENTATION.lock().unwrap() = orientation;
    }

    /// Wether this is the corner `Info`.
    pub fn is_corner(&self) -> bool {
        self.is_corner
    }
}

impl Widget for Info {
    fn text<'p>(widget: &'p RwData<Self>, pa: &'p Pass) -> &'p Text {
        &widget.read(pa).text
    }

    fn text_mut<'p>(widget: &'p RwData<Self>, pa: &'p mut Pass) -> TextMut<'p> {
        widget.write(pa).text.as_mut()
    }

    fn print_opts(&self) -> PrintOpts {
        let mut opts = PrintOpts::new();
        opts.wrap_lines = true;
        opts.tabstop = 2;
        opts
    }
}
