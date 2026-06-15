//! A widget for sectioned information and links.
//!
//! This widget can be used to show information about things in Duat.
//! It has the ability to hold information about any number of things
//! through the use of [namespaced] sections.
//!
//! It also features links, which are sections of the [`Info`] that
//! can be highlighted one by one, and then be selected in order to
//! e.g. jump to locations.
//!
//! This widget is complementarily used by the [`Completions`] and
//! [`Gutter`] widgets.
//!
//! [namespaced]: Ns
//! [`Completions`]: super::Completions
//! [`Gutter`]: super::Gutter
use std::{
    ops::Range,
    path::PathBuf,
    sync::{LazyLock, Mutex},
};

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

    hook::add::<WidgetOpened<Info>>(Info::resize);
}

/// A widget to show sectioned [`Text`]s, with links for actions.
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
    sections: Vec<(usize, Ns, Text, Vec<InfoLink>)>,
    is_corner: bool,
}

impl Info {
    /// Returns a new `Info` widget.
    ///
    /// This widget can be modified by anyone in order to show more
    /// [`Text`]s with more links in different sections of the
    /// `Info`.
    ///
    /// This is the only [`Widget`] in `duat-base` that can be
    /// acquired this way, since it's supposed to be versatile in
    /// where you position it. Every other widget has to be placed in
    /// specific locations, so they don't offer a `new` method, which
    /// could be used in order to place them willy nilly.
    pub fn new(ns: Ns, text: Text, priority: usize) -> Self {
        Self {
            text: text.clone(),
            sections: vec![(priority, ns, text, Vec::new())],
            is_corner: false,
        }
    }

    /// Spawns the `Info` on the designated `Info` corner.
    ///
    /// If there is already a corner `Info`, then this will replace
    /// that `Info` with this one instead.
    pub fn spawn_on_corner(self, pa: &mut Pass, title: Option<Text>, fragile: bool) {
        static NS: LazyLock<Ns> = Ns::new_lazy();

        let info = if let Some(info) = context::windows()
            .handles_of::<Info>(pa)
            .into_iter()
            .find(|info| info.read(pa).is_corner)
        {
            *info.write(pa) = self;
            info
        } else {
            let orientation = *ORIENTATION.lock().unwrap();
            let buffer = context::current_buffer(pa);
            let Some(info) = buffer.spawn_on_widget(pa, self, DynSpawnSpecs {
                orientation,
                width: None,
                height: None,
                hidden: false,
                inside: true,
            }) else {
                return;
            };
            info
        };

        if let Some(area) = info.area().write_as::<duat_term::Area>(pa) {
            let mut frame = Frame::default();
            if let Some(title) = title {
                frame.set_text(Side::Above, move |_| {
                    txt!("[terminal.border.Info]┤[]{title}[terminal.border.Info]├")
                });
            }
            area.set_frame(frame)
        }

        if fragile && !hook::group_exists(*NS) {
            hook::add_once::<KeyTyped>(move |pa, _| {
                _ = info.close(pa);
            })
            .grouped(*NS)
            .lateness(0);
        }
    }

    /// Get the corner `Info`, if there is one open.
    pub fn get_corner(pa: &Pass) -> Option<Handle<Info>> {
        context::windows()
            .handles_of::<Info>(pa)
            .into_iter()
            .find(|info| info.read(pa).is_corner)
    }

    /// Add a section to this `Info`.
    ///
    /// If a section with this `Ns` already existed, it will be
    /// replaced with this new one.
    pub fn set_section(pa: &mut Pass, info: &Handle<Info>, ns: Ns, text: Text) {
        let inf = info.write(pa);
        if let Some(section) = inf.sections.iter_mut().find(|(_, other, ..)| *other == ns) {
            *section = (section.0, ns, text, Vec::new());
        }

        inf.sections
            .sort_unstable_by(|(lprio, ..), (rprio, ..)| lprio.cmp(rprio).reverse());

        let mut builder = Text::builder();
        for (.., text, _) in &inf.sections {
            builder.push_ref(text);
        }

        inf.text = builder.build();

        Info::resize(pa, info);
    }

    /// Sets the [`Orientation`] where the corner [`Info`] will be
    /// spawned.
    pub fn set_corner_orientation(orientation: Orientation) {
        *ORIENTATION.lock().unwrap() = orientation;
    }

    /// Wether this is the corner `Info`.
    pub fn is_corner(&self) -> bool {
        self.is_corner
    }

    fn resize(pa: &mut Pass, info: &Handle<Self>) {
        let (info, area) = info.write_with_area(pa);
        let size = area.size_of_text(info.print_opts(), &info.text).unwrap();
        _ = area.set_width(size.x);
        _ = area.set_height(size.y);
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

/// A link to be used in the [`Info`].
///
/// This link will represent a focuseable section of the `Info`
/// widget's [`Text`].
pub struct InfoLink {
    /// The byte range that this link covers. Must be contained in
    /// `0..text.len()`.
    pub range: Range<usize>,
    /// [`Text`] to display when focusing on the link.
    pub hover_text: Option<Text>,
    /// An action to be done upon selecting the link.
    pub action: Option<InfoLinkAction>,
}

/// An action that should be performed upon selecting an [`InfoLink`].
pub struct InfoLinkAction(InnerLinkAction);

impl InfoLinkAction {
    /// Returns a new goto `Link`, which, upon selection, will jump to
    /// a path an byte range.
    pub fn goto(path: PathBuf, range: Range<usize>) -> Self {
        Self(InnerLinkAction::Goto(path, range))
    }

    /// Returns a new function `Link` which, upon selection, will
    /// trigger an action.
    pub fn function(func: impl FnMut(&mut Pass) + Send + 'static) -> Self {
        Self(InnerLinkAction::Func(Mutex::new(Box::new(func))))
    }
}

enum InnerLinkAction {
    Goto(PathBuf, Range<usize>),
    Func(Mutex<Box<dyn FnMut(&mut Pass) + Send>>),
}
