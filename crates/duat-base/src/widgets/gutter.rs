//! A gutter to add side information relating to the [`Buffer`]
//!
//! This struct is meant to be used by plugins like `duat-lsp`, which
//! can show diagnostics about a `Buffer`. This [`Widget`] will then
//! be used to show that there are errors in the `Buffer`.
//!
//! Additionally, this module contains functions that are used to add
//! errors to a `Buffer`.
//!
//! [`Buffer`]: duat_core::buffer::Buffer
use std::{
    collections::HashMap,
    ops::Range,
    sync::{Arc, Mutex, Once},
};

use duat_core::{
    Ns,
    buffer::{Buffer, Moment},
    context::{self, Handle},
    data::Pass,
    form::{self, Form, FormId},
    hook::{self, BufferOpened, BufferPrinted, BufferUpdated, OnMouseEvent},
    text::{Inlay, Overlay, Text, TextRange, TwoPoints},
    txt,
    ui::{Area, Coord, PushSpecs, Side, Widget},
};

/// A struct to hold diagnostic hints about a [`Buffer`].
///
/// It sits on the sides of the `Buffer`, and tells you when there are
/// things to note about specific lines. These may be hints, warnings,
/// errors, or custom annotations.
///
/// [`Buffer`]: duat_core::buffer::Buffer
pub struct Gutter {
    text: Text,
    entries: HashMap<Ns, Vec<GutterEntry>>,
    opts: GutterOpts,
    mouse_coord: Option<Coord>,
}

fn initial_setup() {
    form::set_weak("gutter.hint", Form::mimic("default.info"));
    form::set_weak("gutter.warning", Form::mimic("default.warning"));
    form::set_weak("gutter.error", Form::mimic("default.error"));
    form::set_weak("buffer.hint", Form::new().underline_grey().underlined());
    form::set_weak(
        "buffer.warning",
        Form::new().underline_yellow().underlined(),
    );
    form::set_weak("buffer.error", Form::new().underline_red().underlined());

    let ns = Ns::new();
    let mouse_msg_ns = Ns::new();

    hook::add::<BufferOpened>(move |pa, buffer| _ = buffer.read(pa).moment_for(ns));
    hook::add::<BufferUpdated>(move |pa, buffer| {
        let Some((gutter, _)) = buffer.get_related::<Gutter>(pa).first().cloned() else {
            return;
        };

        let (gtr, buf) = pa.write_many((&gutter, buffer));
        gtr.apply_changes(buf.moment_for(ns));

        let (gtr, buf, area) = pa.write_many((&gutter, buffer, buffer.area()));
        let mut opts = gtr.opts.clone();
        let popts = buf.print_opts();

        let (related_to_hovered, hovered_ids) = gtr
            .mouse_coord
            .filter(|&coord| coord >= area.top_left() && coord < area.bottom_right())
            .and_then(|coord| {
                let real = area
                    .points_at_coord(buf.text(), coord, popts)?
                    .as_within()?
                    .real;

                let ids = Vec::from_iter(
                    gtr.entries
                        .iter()
                        .flat_map(|(_, entries)| entries)
                        .filter_map(|entry| entry.range.contains(&real.byte()).then_some(entry.id)),
                );

                let related = Vec::from_iter(
                    ID_RELATIONS
                        .lock()
                        .unwrap()
                        .iter()
                        .filter(|related| ids.iter().any(|id| related.contains(id)))
                        .flatten()
                        .copied(),
                );

                Some((related, ids))
            })
            .unwrap_or_default();

        let entries_to_show = gtr
            .entries
            .iter()
            .flat_map(|(_, entries)| entries.iter().rev())
            .filter(|entry| {
                let display = gtr.opts.symbol_opts(entry.kind).display;
                !display.always_show() && related_to_hovered.contains(&entry.id)
            });

        for entry in entries_to_show {
            // Replace the related entries's displays with an inline one, in order
            // to not move the `Text` around, displacing it from the mouse
            // position.
            if !hovered_ids.contains(&entry.id) {
                let display = &mut opts.symbol_opts_mut(entry.kind).display;
                let prev = std::mem::replace(display, GutterDisplay::EndOfLine(false));
                entry.insert_on(mouse_msg_ns, &opts, buf, area, true);
                opts.symbol_opts_mut(entry.kind).display = prev;
            } else {
                entry.insert_on(mouse_msg_ns, &opts, buf, area, true);
            }
        }
    })
    .lateness(100_000_000);

    hook::add::<BufferPrinted>(move |pa, buffer| {
        buffer.text_parts(pa).tags.remove(mouse_msg_ns, ..);
    });

    hook::add::<BufferUpdated>(|pa, buffer| {
        let Some((gutter, _)) = buffer.get_related::<Gutter>(pa).first().cloned() else {
            return;
        };

        gutter.write(pa).text = Gutter::form_text(gutter.read(pa), pa, buffer);
    })
    .lateness(usize::MAX);

    hook::add::<OnMouseEvent<Buffer>>(move |pa, event| {
        let Some((gutter, _)) = event.handle.get_related::<Gutter>(pa).first().cloned() else {
            return;
        };

        gutter.write(pa).mouse_coord = Some(event.coord);
    })
    .lateness(usize::MAX);

    hook::add::<OnMouseEvent>(move |pa, _| {
        for gutter in context::windows().handles_of::<Gutter>(pa) {
            let gt = gutter.write(pa);
            if gt.mouse_coord.take().is_some() {
                let (buffer, _) = gutter.get_related::<Buffer>(pa).first().cloned().unwrap();
                buffer.request_update();
            }
        }
    })
    .lateness(usize::MAX);
}

impl Gutter {
    /// A builder for a `Gutter`.
    pub fn builder() -> GutterOpts {
        static ONCE: Once = Once::new();
        ONCE.call_once(initial_setup);

        GutterOpts {
            hint: GutterSymbolOpts {
                symbol: 'i',
                display: GutterDisplay::OwnLines(false),
            },
            warning: GutterSymbolOpts {
                symbol: '!',
                display: GutterDisplay::OwnLines(false),
            },
            error: GutterSymbolOpts {
                symbol: '*',
                display: GutterDisplay::OwnLines(false),
            },
            renderer: Some(Arc::new(Mutex::new(default_renderer))),
        }
    }

    fn form_text(&self, pa: &Pass, buffer: &Handle) -> Text {
        let printed_line_numbers = buffer.printed_line_numbers(pa);
        let text = buffer.text(pa);

        let mut builder = Text::builder();

        for (idx, line) in printed_line_numbers.iter().enumerate() {
            if idx > 0 && (line.is_wrapped || line.is_ghost) {
                builder.push(" \n");
                continue;
            };

            let mut kind = None;
            let range = text.line(line.number).byte_range();

            for (_, entries) in self.entries.iter() {
                let (Ok(idx) | Err(idx)) =
                    entries.binary_search_by(|entry| entry.range.start.cmp(&range.start));

                let mut iter = entries[idx..].iter();
                while let Some(entry) = iter.next()
                    && entry.range.start < range.end
                {
                    kind = kind.max(Some(entry.kind))
                }
            }

            if let Some(kind) = kind {
                let (symbol, symbol_form) = match kind {
                    EntryKind::Hint => (self.opts.hint.symbol, form::id_of!("gutter.hint")),
                    EntryKind::Warning => {
                        (self.opts.warning.symbol, form::id_of!("gutter.warning"))
                    }
                    EntryKind::Error => (self.opts.error.symbol, form::id_of!("gutter.error")),
                    EntryKind::_Custom(symbol, symbol_form, _) => (symbol, symbol_form),
                };

                builder.push(symbol_form);
                builder.push(symbol);
                builder.push(FormId::default());
                builder.push("\n");
            } else {
                builder.push(" \n");
            }
        }

        builder.build()
    }

    fn apply_changes(&mut self, moment: Moment) {
        let sh = |value: &mut usize, shift: i32| {
            *value = value.saturating_add_signed(shift as isize);
        };

        for (_, entries) in self.entries.iter_mut() {
            let mut shift = 0;
            let mut iter = entries.iter_mut().enumerate();
            let mut to_remove = Vec::new();

            for change in moment.iter() {
                let mut is_contained = |i: usize, range: Range<usize>| {
                    let change_range = change.taken_range();
                    let change_range = change_range.start.byte()..change_range.end.byte();
                    if change_range.contains(&range.start) || change_range.contains(&range.end) {
                        to_remove.push(i);
                        true
                    } else {
                        false
                    }
                };

                if let Some((_, entry)) = iter.find_map(|(i, entry)| {
                    sh(&mut entry.range.start, shift);
                    sh(&mut entry.range.end, shift);

                    (!is_contained(i, entry.range.clone())
                        && entry.range.end > change.start().byte())
                    .then_some((i, entry))
                }) {
                    let start_shift =
                        change.shift()[0] * (entry.range.start > change.start().byte()) as i32;

                    sh(&mut entry.range.start, start_shift);
                    sh(&mut entry.range.end, change.shift()[0]);
                }
                shift += change.shift()[0];
            }

            for idx in to_remove.into_iter().rev() {
                entries.remove(idx);
            }
        }
    }
}

impl Widget for Gutter {
    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> duat_core::text::TextMut<'_> {
        self.text.as_mut()
    }
}

/// Options for the [`Gutter`].
///
/// You can change the character of hints, warnings and errors, and
/// you can also set how they should be displayed by default.
#[derive(Clone)]
pub struct GutterOpts {
    /// Hints are information that doesn't necessarily indicate that
    /// something's wrong, but may be related to an actual issue.
    ///
    /// By default, they are shown as `'i'` on the [`Gutter`], and the
    /// hint's [`Text`] is only shown when hovering over it.
    ///
    /// On the `Gutter`, it makes use of the `gutter.hint` [`Form`],
    /// while on the [`Buffer`], it makes use of the `buffer.hint`
    /// `Form`.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    pub hint: GutterSymbolOpts,
    /// Warnings are problems with your code that don't necessarily
    /// prevent it from working or compiling, but otherwise represent
    /// inadequacies or things that could be improved upon.
    ///
    /// By default, they are shown as `'!'` on the [`Gutter`], and the
    /// hint's [`Text`] is only shown when hovering over it.
    ///
    /// On the `Gutter`, it makes use of the `gutter.warning`
    /// [`Form`], while on the [`Buffer`], it makes use of the
    /// `buffer.warning` `Form`.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    pub warning: GutterSymbolOpts,
    /// Errors are fundamental issues with your code. Either the
    /// compiler couldn't figure out what you meant, or the code is
    /// invalid for some reason.
    ///
    /// By default, they are shown as `'*'` on the [`Gutter`], and the
    /// hint's [`Text`] is shown as [`Inlay`] text on separate lines.
    ///
    /// On the `Gutter`, it makes use of the `gutter.error`
    /// [`Form`], while on the [`Buffer`], it makes use of the
    /// `buffer.error` `Form`.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    pub error: GutterSymbolOpts,
    renderer: Option<Arc<Mutex<Renderer>>>,
}

impl GutterOpts {
    /// Places a [`Gutter`] around a [`Buffer`].
    ///
    /// The [`Widget`] will be pushed on the "outside". That is, if
    /// there are other widgets pushed on the buffer, this one will be
    /// placed around them.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    pub fn push_on(self, pa: &mut Pass, handle: &Handle) -> Handle<Gutter> {
        let text = Text::from(" \n".repeat(handle.text(pa).end_point().line()));

        handle.push_outer_widget(
            pa,
            Gutter {
                text,
                entries: HashMap::new(),
                opts: self,
                mouse_coord: None,
            },
            PushSpecs {
                side: Side::Left,
                width: Some(1.0),
                ..PushSpecs::default()
            },
        )
    }

    /// The mutable [`GutterSymbolOpts`] for a given [`EntryKind`]
    pub fn symbol_opts_mut(&mut self, kind: EntryKind) -> &mut GutterSymbolOpts {
        match kind {
            EntryKind::Hint => &mut self.hint,
            EntryKind::Warning => &mut self.warning,
            EntryKind::Error => &mut self.error,
            EntryKind::_Custom(..) => todo!(),
        }
    }

    /// The [`GutterSymbolOpts`] for a given [`EntryKind`]
    pub fn symbol_opts(&self, kind: EntryKind) -> &GutterSymbolOpts {
        match kind {
            EntryKind::Hint => &self.hint,
            EntryKind::Warning => &self.warning,
            EntryKind::Error => &self.error,
            EntryKind::_Custom(..) => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GutterSymbolOpts {
    symbol: char,
    display: GutterDisplay,
}

/// An entry in the [`Gutter`].
///
/// This contains a range in the [`Text`] and a message, in the form
/// of a `Text`.
pub struct GutterEntry {
    range: Range<usize>,
    msg: Text,
    kind: EntryKind,
    id: GutterEntryId,
}

impl GutterEntry {
    pub fn insert_on(
        &self,
        ns: Ns,
        opts: &GutterOpts,
        buf: &mut Buffer,
        area: &Area,
        is_hovered: bool,
    ) {
        let popts = buf.print_opts();

        let from = |opts: GutterSymbolOpts| {
            (
                is_hovered ^ matches!(opts.display, GutterDisplay::OwnLines(true)),
                opts.display,
            )
        };

        let (form_tag, (do_show, display)) = match self.kind {
            EntryKind::Hint => (form::id_of!("buffer.hint").to_tag(190), from(opts.hint)),
            EntryKind::Warning => (
                form::id_of!("buffer.warning").to_tag(191),
                from(opts.warning),
            ),
            EntryKind::Error => (form::id_of!("buffer.error").to_tag(192), from(opts.error)),
            EntryKind::_Custom(..) => todo!(),
        };

        buf.text_parts()
            .tags
            .insert(ns, self.range.clone(), form_tag);

        if !do_show {
            return;
        }

        let Some(line) = buf.text()[self.range.clone()].lines().last() else {
            return;
        };

        let line_range = line.range();
        let lnum = line_range.start.line();
        let two_points = TwoPoints::new_after_ghost(line_range.start);
        let Some(columns) = area.columns_at(buf.text(), two_points, popts) else {
            return;
        };

        let mut parts = buf.text_parts();

        let line_end = parts.strs.line(lnum).byte_range().end;
        match display {
            GutterDisplay::EndOfLine(_) => {
                let msg = txt!("  {self.msg}");
                parts.tags.insert(ns, line_end - 1, Overlay::new(msg));
            }
            GutterDisplay::Spawn(_) => todo!(),
            GutterDisplay::SpawnCorner(..) => todo!(),
            GutterDisplay::OwnLines(_) => {
                let mut msg = txt!("{self.msg}\n");
                let line_ranges = Vec::from_iter(msg.lines().map(|line| line.byte_range()));

                for range in line_ranges.into_iter().rev() {
                    if range.start + 1 < range.end {
                        msg.replace_range(range.start..range.start, &SPACES[..columns.wrapped]);
                    }
                }

                parts.tags.insert(ns, line_end, Inlay::new(msg))
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EntryKind {
    Hint,
    Warning,
    Error,
    _Custom(char, FormId, FormId),
}

/// How to display the accompanying [`Text`] message to a [`Gutter`]
/// entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(unused)]
pub enum GutterDisplay {
    /// The [`Text`] will be shown at the end of the line, potentially
    /// running off out of screen.
    ///
    /// If [`GutterEntryBuilder::only_on_hover`] is not called, this
    /// display method will default to always be shown.
    EndOfLine(AlwaysShow),
    /// The [`Text`] will be shown as a spawned widget near the
    /// entry's range.
    ///
    /// If [`GutterEntryBuilder::only_on_hover`] is not called, this
    /// display method will default to show up only on hover.
    Spawn(AlwaysShow),
    /// The [`Text`] will be show as a spawned widget on one of the
    /// corners.
    ///
    /// If [`OnWindow`] is set to true, this will spawn it on the
    /// corners of the window. Otherwise, it will be spawned on the
    /// corners of the [`Buffer`]
    ///
    /// If [`GutterEntryBuilder::only_on_hover`] is not called, this
    /// display method will default to show up only on hover.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    SpawnCorner(AlwaysShow, Corner, OnWindow),
    /// The [`Text`] will be shown as [`Inlay`] lines under the
    /// entry's range.
    ///
    /// If [`GutterEntryBuilder::only_on_hover`] is not called, this
    /// display method will default to always be shown.
    OwnLines(AlwaysShow),
}

impl GutterDisplay {
    /// Wether this [`GutterEntry`] should always be displayed.
    pub fn always_show(&self) -> bool {
        match *self {
            GutterDisplay::EndOfLine(always) => always,
            GutterDisplay::Spawn(always) => always,
            GutterDisplay::SpawnCorner(always, ..) => always,
            GutterDisplay::OwnLines(always) => always,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(unused)]
pub enum Corner {
    TopLeft,
    TopRight,
    BottomRight,
    BottomLeft,
}

fn insert_entry(
    pa: &mut Pass,
    ns: Ns,
    gutter: Handle<Gutter>,
    buffer: &Handle,
    entry: GutterEntry,
) {
    let gtr = gutter.write(pa);
    let renderer = gtr.opts.renderer.take().unwrap();
    let opts = gtr.opts.clone();
    duat_core::utils::catch_panic(|| {
        renderer.lock().unwrap()(pa, ns, &entry, &opts, buffer, false)
    });

    let gtr = gutter.write(pa);
    gtr.opts.renderer = Some(renderer);
    let entries = gtr.entries.entry(ns).or_default();

    let (Ok(idx) | Err(idx)) = entries.binary_search_by(|e| e.range.start.cmp(&entry.range.start));
    entries.insert(idx, entry);
}

#[allow(private_bounds)]
trait Sealed {}
/// Trait for adding gutter entries to a [`Buffer`].
///
/// [`Buffer`]: duat_core::buffer::Buffer
#[allow(private_bounds)]
pub trait GutterBuffer: Sealed {
    /// Remove all [`Gutter`] entries from a given [`Ns`].
    fn remove_gutter_entries(&self, pa: &mut Pass, ns: Ns);

    /// Add a hint to the [`Gutter`] and the [`Buffer`].
    ///
    /// This could just be useful information, like the fact that
    /// something won't be included in compilation because of a `cfg`
    /// attribute.
    fn add_hint(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text) -> GutterEntryId;

    /// Add a warning to the [`Gutter`] and the [`Buffer`].
    ///
    /// This could be improvements that you could do to your code, or
    /// ways in which it is innadequate that don't necessarily hinder
    /// it from working properly.
    fn add_warning(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text)
    -> GutterEntryId;

    /// Add an error to the [`Gutter`] and the [`Buffer`].
    ///
    /// These are fundamental issues in your code, and either prevent
    /// compilation, or prevent it from working properly.
    fn add_error(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text) -> GutterEntryId;

    /// Wether  this [`Buffer`] has a [`Gutter`] or not.
    ///
    /// This should return `true` everytime, unless you disable this
    /// functionality.
    fn has_gutter(&self, pa: &Pass) -> bool;
}

impl Sealed for Handle {}
impl GutterBuffer for Handle {
    #[track_caller]
    fn remove_gutter_entries(&self, pa: &mut Pass, ns: Ns) {
        let Some((gutter, _)) = self.get_related::<Gutter>(pa).first().cloned() else {
            panic!("Tried to remove Gutter entries on Buffer with no Gutter");
        };

        self.text_mut(pa).remove_tags(ns, ..);
        let gtr = gutter.write(pa);
        let entries = gtr.entries.remove(&ns);

        let mut extant_ids = EXTANT_IDS.lock().unwrap();
        for entry in entries.into_iter().flatten() {
            extant_ids.retain(|id| *id != entry.id);
        }

        ID_RELATIONS.lock().unwrap().retain_mut(|ids| {
            ids.retain(|id| id.1 != ns);
            !ids.is_empty()
        });
    }

    #[track_caller]
    fn add_hint(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text) -> GutterEntryId {
        let Some((gutter, _)) = self.get_related::<Gutter>(pa).first().cloned() else {
            panic!("Tried to add a Gutter entry on Buffer with no Gutter");
        };

        let text = self.text(pa);
        let range = range.to_range(text.len());
        let id = GutterEntryId::new(ns);
        let entry = GutterEntry { range, msg, kind: EntryKind::Hint, id };
        insert_entry(pa, ns, gutter, self, entry);

        id
    }

    #[track_caller]
    fn add_warning(
        &self,
        pa: &mut Pass,
        ns: Ns,
        range: impl TextRange,
        msg: Text,
    ) -> GutterEntryId {
        let Some((gutter, _)) = self.get_related::<Gutter>(pa).first().cloned() else {
            panic!("Tried to add a Gutter entry on Buffer with no Gutter");
        };

        let text = self.text(pa);
        let range = range.to_range(text.len());
        let id = GutterEntryId::new(ns);
        let entry = GutterEntry { range, msg, kind: EntryKind::Warning, id };
        insert_entry(pa, ns, gutter, self, entry);

        id
    }

    #[track_caller]
    fn add_error(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text) -> GutterEntryId {
        let Some((gutter, _)) = self.get_related::<Gutter>(pa).first().cloned() else {
            panic!("Tried to add a Gutter entry on Buffer with no Gutter");
        };

        let text = self.text(pa);
        let range = range.to_range(text.len());
        let id = GutterEntryId::new(ns);
        let entry = GutterEntry { range, msg, kind: EntryKind::Error, id };
        insert_entry(pa, ns, gutter, self, entry);

        id
    }

    fn has_gutter(&self, pa: &Pass) -> bool {
        !self.get_related::<Gutter>(pa).is_empty()
    }
}

/// The default [`Gutter`] renderer.
///
/// You can use this if you want to render things differently in some
/// situations, but not all.
pub fn default_renderer(
    pa: &mut Pass,
    ns: Ns,
    entry: &GutterEntry,
    opts: &GutterOpts,
    buffer: &Handle,
    is_hovered: bool,
) {
    let (buf, area) = buffer.write_with_area(pa);
    entry.insert_on(ns, opts, buf, area, is_hovered);
}

/// An id for a [`Gutter`] entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GutterEntryId(usize, Ns);

impl GutterEntryId {
    /// Returns a new `GutterEntryId`.
    fn new(ns: Ns) -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};
        static COUNT: AtomicUsize = AtomicUsize::new(0);

        let id = Self(COUNT.fetch_add(1, Relaxed), ns);

        EXTANT_IDS.lock().unwrap().push(id);

        id
    }

    /// Declare that the gutter entries for the given
    /// [`GutterEntryId`]s are related to this one and to each other.
    ///
    /// One poignant example is that of rust borrow check errors,
    /// where an earlier borrow in a certain code location is related
    /// to a borrow failure in another code location.
    ///
    /// The most common effect that this will have is grouped display
    /// of entries based on cursor posisition. For example, if you
    /// hover over an entry which displays only when hovered, all
    /// related entries will also be displayed.
    #[track_caller]
    pub fn relate_with_other_entries(&self, entries: impl IntoIterator<Item = GutterEntryId>) {
        let ids = Vec::from_iter(std::iter::once(*self).chain(entries));
        let extant_ids = EXTANT_IDS.lock().unwrap();

        assert!(
            ids.iter().all(|id| extant_ids.contains(id)),
            "Attempted to add entry relations to Gutter, but not all entries still exist"
        );

        ID_RELATIONS.lock().unwrap().push(ids);
    }
}

const SPACES: &str = unsafe { std::str::from_utf8_unchecked(&[b' '; 1000]) };
static ID_RELATIONS: Mutex<Vec<Vec<GutterEntryId>>> = Mutex::new(Vec::new());
static EXTANT_IDS: Mutex<Vec<GutterEntryId>> = Mutex::new(Vec::new());

type Renderer = dyn FnMut(&mut Pass, Ns, &GutterEntry, &GutterOpts, &Handle, bool) + 'static + Send;
type AlwaysShow = bool;
type OnWindow = bool;
