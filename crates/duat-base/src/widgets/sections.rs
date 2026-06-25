//! A widget for sectioned sectionsrmation and links.
//!
//! This widget can be used to show sectionsrmation about things in
//! Duat. It has the ability to hold sectionsrmation about any number
//! of things through the use of [namespaced] sections.
//!
//! It also features links, which are sections of the [`Sections`]
//! that can be highlighted one by one, and then be selected in order
//! to e.g. jump to locations.
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
    form::{self, Form},
    hook::{self, KeyTyped, OnMouseEvent, WidgetOpened},
    mode::MouseEventKind,
    opts::PrintOpts,
    text::{Text, TextMut},
    txt,
    ui::{DynSpawnSpecs, Orientation, Side, Widget},
};
use duat_term::Frame;

static ORIENTATION: Mutex<Orientation> = Mutex::new(Orientation::VerRightBelow);

/// Adds the hooks for the [`Sections`] widget.
pub fn sections_setup() {
    use MouseEventKind::{ScrollDown, ScrollUp};

    hook::add::<OnMouseEvent<Sections>>(|pa, event| match event.kind {
        ScrollDown | ScrollUp => {
            let (sections, area) = event.handle.write_with_area(pa);
            let scroll = if let ScrollDown = event.kind { 3 } else { -3 };
            area.scroll_ver(&sections.text, scroll as f32, sections.print_opts());
        }
        _ => {}
    });

    hook::add::<WidgetOpened<Sections>>(Sections::update);

    form::set_weak("sections.header", Form::new().bold());
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
pub struct Sections {
    /// The [`Text`] that will be shown by this widget
    text: Text,
    sections: Vec<Section>,
    is_corner: bool,
}

impl Sections {
    /// Returns a new `Sections` widget.
    ///
    /// This widget can be modified by anyone in order to show more
    /// [`Text`]s with more links in different sections of the
    /// `Sections`.
    ///
    /// This is the only [`Widget`] in `duat-base` that can be
    /// acquired this way, since it's supposed to be versatile in
    /// where you position it. Every other widget has to be placed in
    /// specific locations, so they don't offer a `new` method, which
    /// could be used in order to place them willy nilly.
    pub fn new(ns: Ns, text: Text, title: Option<String>, priority: usize) -> Self {
        Self {
            text: Text::new(),
            sections: vec![Section::new(priority, ns, text, title, Vec::new())],
            is_corner: false,
        }
    }

    /// Spawns the `Sections` on the designated `Sections` corner.
    ///
    /// If there is already a corner `Sections`, then this will
    /// replace that `Sections` with this one instead.
    pub fn spawn_on_corner(mut self, pa: &mut Pass, fragile: bool) -> Handle<Sections> {
        static NS: LazyLock<Ns> = Ns::new_lazy();
        self.is_corner = true;

        let sections = if let Some(sections) = context::windows()
            .handles_of::<Sections>(pa)
            .into_iter()
            .find(|sections| sections.read(pa).is_corner)
        {
            *sections.write(pa) = self;
            sections
        } else {
            let orientation = *ORIENTATION.lock().unwrap();
            let buffer = context::current_buffer(pa);
            buffer
                .spawn_on_widget(pa, self, DynSpawnSpecs {
                    orientation,
                    width: None,
                    height: None,
                    hidden: false,
                    inside: true,
                })
                .unwrap()
        };

        if fragile && !hook::group_exists(*NS) {
            let sections = sections.clone();
            hook::add_once::<KeyTyped>(move |pa, _| {
                _ = sections.close(pa);
            })
            .grouped(*NS)
            .lateness(0);
        }

        sections
    }

    /// Get the corner `Sections`, if there is one open.
    pub fn get_corner(pa: &Pass) -> Option<Handle<Sections>> {
        context::windows()
            .handles_of::<Sections>(pa)
            .into_iter()
            .find(|sections| sections.read(pa).is_corner)
    }

    /// Add a section to this `Sections`.
    ///
    /// If a section with this `Ns` already existed, it will be
    /// replaced with this new one.
    pub fn set_section(
        pa: &mut Pass,
        sections: &Handle<Sections>,
        ns: Ns,
        text: Text,
        title: Option<String>,
        priority: usize,
    ) {
        let sec = sections.write(pa);
        if let Some(section) = sec.sections.iter_mut().find(|section| section.ns == ns) {
            *section = Section::new(section.priority, ns, text, title, Vec::new());
        } else {
            sec.sections
                .push(Section::new(priority, ns, text, title, Vec::new()));
        }

        sec.sections
            .sort_unstable_by(|l, r| l.priority.cmp(&r.priority).reverse());

        Sections::update(pa, sections);
    }

    /// Remove a section from this `Sections`.
    pub fn remove_section(pa: &mut Pass, sections: &Handle<Self>, ns: Ns) {
        let sec = sections.write(pa);
        sec.sections.retain(|section| section.ns != ns);

        if sec.sections.is_empty() {
            _ = sections.close(pa);
        } else {
            Sections::update(pa, sections);
        }
    }

    /// Sets the [`Orientation`] where the corner [`Sections`] will be
    /// spawned.
    pub fn set_corner_orientation(orientation: Orientation) {
        *ORIENTATION.lock().unwrap() = orientation;
    }

    /// Wether this is the corner `Sections`.
    pub fn is_corner(&self) -> bool {
        self.is_corner
    }

    fn update(pa: &mut Pass, sections: &Handle<Self>) {
        let (sec, area) = sections.write_with_area(pa);

        if let Some(area) = area.get_as_mut::<duat_term::Area>() {
            let mut frame = Frame::default();

            if sec.sections.len() == 1
                && let Some(title) = sec.sections[0].title.clone()
            {
                frame.set_text(Side::Above, move |_| {
                    txt!("[terminal.border.Sections]┤[]{title}[terminal.border.Sections]├")
                });
            }

            area.set_frame(frame);
        }

        let mut builder = Text::builder();
        for (i, section) in sec.sections.iter().enumerate() {
            if sec.sections.len() > 1
                && let Some(title) = &section.title
            {
                let nl = if i == 0 { "" } else { "\n" };
                builder.push(txt!("{nl}[sections.header]{title}:\n"));
            }
            builder.push_ref(&section.text);
        }

        sec.text = builder.build();

        let size = area.size_of_text(sec.print_opts(), &sec.text).unwrap();
        _ = area.set_width(size.x);
        _ = area.set_height(size.y);
    }
}

impl Widget for Sections {
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

/// A link to be used in the [`Sections`].
///
/// This link will represent a focuseable section of the `Sections`
/// widget's [`Text`].
pub struct SectionsLink {
    /// The byte range that this link covers. Must be contained in
    /// `0..text.len()`.
    pub range: Range<usize>,
    /// [`Text`] to display when focusing on the link.
    pub hover_text: Option<Text>,
    /// An action to be done upon selecting the link.
    pub action: Option<SectionsLinkAction>,
}

/// An action that should be performed upon selecting an
/// [`SectionsLink`].
pub struct SectionsLinkAction(InnerLinkAction);

impl SectionsLinkAction {
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

struct Section {
    priority: usize,
    ns: Ns,
    text: Text,
    title: Option<String>,
    links: Vec<SectionsLink>,
}

impl Section {
    fn new(
        priority: usize,
        ns: Ns,
        text: Text,
        title: Option<String>,
        links: Vec<SectionsLink>,
    ) -> Self {
        Self { priority, ns, text, title, links }
    }
}
