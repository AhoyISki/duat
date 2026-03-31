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
use std::{collections::HashMap, ops::Range, sync::Once};

use duat_core::{
    Ns,
    buffer::{Buffer, Moment},
    context::{self, Handle, WidgetRelation},
    data::Pass,
    form::{self, Form, FormId},
    hook::{self, BufferOpened, BufferUpdated, OnMouseEvent},
    text::{Ghost, Text, TextParts, TextRange, TwoPoints},
    txt,
    ui::{Coord, PushSpecs, Side, Widget},
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

impl Gutter {
    /// A builder for a `Gutter`.
    pub fn builder() -> GutterOpts {
        static ONCE: Once = Once::new();

        ONCE.call_once(|| {
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
            let msg_ns = Ns::new();

            hook::add::<BufferOpened>(move |pa, buffer| _ = buffer.read(pa).moment_for(ns));
            hook::add::<BufferUpdated>(move |pa, buffer| {
                let Some((gutter, _)) = buffer.get_related::<Gutter>(pa).first().cloned() else {
                    return;
                };

                let printed_line_ranges = buffer.printed_line_ranges(pa);

                let (gt, buf) = pa.write_many((&gutter, buffer));
                gt.apply_changes(buf.moment_for(ns));
                gutter.write(pa).text = Gutter::form_text(gutter.read(pa), pa, buffer);

                let (gt, buf, area) = pa.write_many((&gutter, buffer, buffer.area()));
                buf.text_parts().tags.remove(msg_ns, ..);
                let opts = buf.print_opts();

                let mouse_point = gt.mouse_coord.and_then(|coord| {
                    Some(
                        area.points_at_coord(buf.text(), coord, opts)?
                            .as_within()?
                            .real,
                    )
                });

                let entries = gt
                    .entries
                    .iter()
                    .flat_map(|(_, entries)| entries)
                    .filter(|entry| {
                        let is_onscreen = printed_line_ranges
                            .iter()
                            .any(|range| range.contains(&entry.range.end));

                        let display = match entry.kind {
                            EntryKind::Hint => gt.opts.hint.display,
                            EntryKind::Warning => gt.opts.warning.display,
                            EntryKind::Error => gt.opts.error.display,
                            EntryKind::_Custom(..) => todo!(),
                        };

                        let do_show = match display {
                            GutterDisplay::OwnLines(always) => {
                                always
                                    || mouse_point
                                        .is_some_and(|point| entry.range.contains(&point.byte()))
                            }
                            GutterDisplay::Inline(_) => todo!(),
                            GutterDisplay::Spawn(_) => todo!(),
                            GutterDisplay::SpawnCorner(..) => todo!(),
                        };

                        do_show && is_onscreen
                    });

                for entry in entries {
                    let Some(line) = buf.text()[entry.range.clone()].lines().last() else {
                        continue;
                    };

                    let range = line.range();
                    let lnum = range.start.line();
                    let Some(columns) =
                        area.columns_at(buf.text(), TwoPoints::new_after_ghost(range.start), opts)
                    else {
                        continue;
                    };

                    let mut parts = buf.text_parts();

                    let inlay = Ghost::inlay(txt!("{}{entry.msg}\n", " ".repeat(columns.wrapped)));
                    let line_end = parts.strs.line(lnum).byte_range().end;
                    parts.tags.insert(msg_ns, line_end, inlay)
                }
            })
            .priority(100_000_000);

            hook::add::<OnMouseEvent<Buffer>>(move |pa, event| {
                let Some((gutter, _)) = event.handle.get_related::<Gutter>(pa).first().cloned()
                else {
                    return;
                };

                gutter.write(pa).mouse_coord = Some(event.coord);
            })
            .priority(usize::MAX);

            hook::add::<OnMouseEvent>(move |pa, _| {
                for gutter in context::windows().handles_of::<Gutter>(pa) {
                    let gt = gutter.write(pa);
                    if gt.mouse_coord.take().is_some() {
                        let (buffer, _) =
                            gutter.get_related::<Buffer>(pa).first().cloned().unwrap();
                        buffer.request_update();
                    }
                }
            })
            .priority(usize::MAX);
        });

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
                display: GutterDisplay::OwnLines(true),
            },
            renderer: Some(Box::new(default_renderer)),
        }
    }

    fn form_text(&self, pa: &Pass, buffer: &Handle) -> Text {
        let printed_line_numbers = buffer.printed_line_numbers(pa);
        let text = buffer.text(pa);

        let mut builder = Text::builder();

        for (idx, line) in printed_line_numbers.iter().enumerate() {
            if idx > 0 && line.is_wrapped {
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
    /// hint's [`Text`] is shown as [`Ghost`] text on separate lines.
    ///
    /// On the `Gutter`, it makes use of the `gutter.error`
    /// [`Form`], while on the [`Buffer`], it makes use of the
    /// `buffer.error` `Form`.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    pub error: GutterSymbolOpts,
    renderer: Option<Box<Renderer>>,
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GutterSymbolOpts {
    symbol: char,
    display: GutterDisplay,
}

/// Related entries on the [`Gutter`].
pub struct GutterEntries {
    /// The entries that are related.
    list: Vec<GutterEntry>,
    /// How to display the entry's message.
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
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum EntryKind {
    Hint,
    Warning,
    Error,
    _Custom(char, FormId, FormId),
}

/// How to display the accompanying [`Text`] message to a [`Gutter`]
/// entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GutterDisplay {
    /// The [`Text`] will be shown at the end of the line, potentially
    /// running off out of screen.
    ///
    /// If [`GutterEntryBuilder::only_on_hover`] is not called, this
    /// display method will default to always be shown.
    Inline(OnlyOnHover),
    /// The [`Text`] will be shown as a spawned widget near the
    /// entry's range.
    ///
    /// If [`GutterEntryBuilder::only_on_hover`] is not called, this
    /// display method will default to show up only on hover.
    Spawn(OnlyOnHover),
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
    SpawnCorner(OnlyOnHover, Corner, OnWindow),
    /// The [`Text`] will be shown as [`Ghost`] lines under the
    /// entry's range.
    ///
    /// If [`GutterEntryBuilder::only_on_hover`] is not called, this
    /// display method will default to always be shown.
    OwnLines(OnlyOnHover),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Corner {
    TopLeft,
    TopRight,
    BottomRight,
    BottomLeft,
}

/// A builder for a [`Gutter`] entry.
///
/// This lets you add more related messages to this entry, which will
/// make their display cohesive. You may, for example, have an error
/// that happens in a specific line, because of a decision you made on
/// another line (e.g. borrowing errors on Rust), which should be
/// interlinked with this error, in order to show more cohesive
/// diagnostics.
pub struct GutterEntryBuilder<'p> {
    ns: Ns,
    gbuf: &'p mut GutteredBuffer<'p>,
    entries: GutterEntries,
}

impl<'g> GutterEntryBuilder<'g> {
    /// Add a hint that is related to this entry.
    ///
    /// This could be something like the first borrow, which prevented
    /// a future borrow from making sense (in Rust).
    pub fn add_related_hint(mut self, range: impl TextRange, msg: Text) -> Self {
        let text = self.gbuf.buffer.text(self.gbuf.pa);
        let range = range.to_range(text.len());

        self.entries
            .list
            .push(GutterEntry { range, msg, kind: EntryKind::Hint });

        self
    }

    /// Add a warning that is related to this entry.
    pub fn add_related_warning(mut self, range: impl TextRange, msg: Text) -> Self {
        let text = self.gbuf.buffer.text(self.gbuf.pa);
        let range = range.to_range(text.len());

        self.entries
            .list
            .push(GutterEntry { range, msg, kind: EntryKind::Warning });

        self
    }

    /// Add an error that is related to this entry.
    ///
    /// This could be more errors on the same range, since it's
    /// possible that multiple things went wrong, or more context
    /// would be helpful.
    pub fn add_related_error(mut self, range: impl TextRange, msg: Text) -> Self {
        let text = self.gbuf.buffer.text(self.gbuf.pa);
        let range = range.to_range(text.len());

        self.entries
            .list
            .push(GutterEntry { range, msg, kind: EntryKind::Error });

        self
    }
}

impl<'g> Drop for GutterEntryBuilder<'g> {
    fn drop(&mut self) {
        let (buf, gutter) = self
            .gbuf
            .pa
            .write_many((self.gbuf.buffer, &self.gbuf.gutter));

        let mut renderer = gutter.opts.renderer.take().unwrap();
        renderer(&self.entries, self.ns, buf.text_mut().parts());
        gutter.opts.renderer = Some(renderer);

        let entries = gutter.entries.entry(self.ns).or_default();

        for entry in std::mem::take(&mut self.entries.list) {
            let (Ok(idx) | Err(idx)) =
                entries.binary_search_by(|e| e.range.start.cmp(&entry.range.start));
            entries.insert(idx, entry);
        }
    }
}

/// A struct that lets you add entries to a [`Buffer`]'s [`Gutter`].
///
/// While adding entries to the `Gutter`, those will also be displayed
/// on the `Buffer`.
///
/// [`Buffer`]: duat_core::buffer::Buffer
pub struct GutteredBuffer<'g> {
    ns: Ns,
    pa: &'g mut Pass,
    buffer: &'g Handle,
    gutter: Handle<Gutter>,
}

impl<'g> GutteredBuffer<'g> {
    /// Remove all entries from a given [`Ns`].
    pub fn remove_entries(&mut self, ns: Ns) {
        self.gutter.write(self.pa).entries.remove(&ns);
        self.buffer.text_mut(self.pa).remove_tags(ns, ..);
    }

    /// Add a hint to the [`Gutter`].
    ///
    /// This could just be useful information, like the fact that
    /// something won't be included in compilation because of a `cfg`
    /// attribute.
    pub fn add_hint(&'g mut self, range: impl TextRange, msg: Text) -> GutterEntryBuilder<'g> {
        let text = self.buffer.text(self.pa);
        let range = range.to_range(text.len());
        let display = self.gutter.read(self.pa).opts.hint.display;

        GutterEntryBuilder {
            ns: self.ns,
            gbuf: self,
            entries: GutterEntries {
                list: vec![GutterEntry { range, msg, kind: EntryKind::Hint }],
                display,
            },
        }
    }

    /// Add a warning to the [`Gutter`].
    ///
    /// This could be improvements that you could do to your code, or
    /// ways in which it is innadequate that don't necessarily hinder
    /// it from working properly.
    pub fn add_warning(&'g mut self, range: impl TextRange, msg: Text) -> GutterEntryBuilder<'g> {
        let text = self.buffer.text(self.pa);
        let range = range.to_range(text.len());
        let display = self.gutter.read(self.pa).opts.warning.display;

        GutterEntryBuilder {
            ns: self.ns,
            gbuf: self,
            entries: GutterEntries {
                list: vec![GutterEntry { range, msg, kind: EntryKind::Warning }],
                display,
            },
        }
    }

    /// Add an error to the [`Gutter`].
    ///
    /// These are fundamental issues in your code, and either prevent
    /// compilation, or prevent it from working properly.
    pub fn add_error(&'g mut self, range: impl TextRange, msg: Text) -> GutterEntryBuilder<'g> {
        let text = self.buffer.text(self.pa);
        let range = range.to_range(text.len());
        let display = self.gutter.read(self.pa).opts.error.display;

        GutterEntryBuilder {
            ns: self.ns,
            gbuf: self,
            entries: GutterEntries {
                list: vec![GutterEntry { range, msg, kind: EntryKind::Error }],
                display,
            },
        }
    }
}

#[allow(private_bounds)]
trait Sealed {}
/// Trait for getting a [`Gutter`] [`Buffer`] combo.
///
/// [`Buffer`]: duat_core::buffer::Buffer
#[allow(private_bounds)]
pub trait GetGuttered: Sealed {
    /// Get a [`GutteredBuffer`] struct, letting you add new entries
    /// to the [`Gutter`].
    fn get_guttered<'g>(&'g self, pa: &'g mut Pass, ns: Ns) -> Option<GutteredBuffer<'g>>;
}

impl Sealed for Handle {}
impl GetGuttered for Handle {
    fn get_guttered<'g>(&'g self, pa: &'g mut Pass, ns: Ns) -> Option<GutteredBuffer<'g>> {
        let gutter = self.get_related(pa).first().cloned();
        if let Some((gutter, WidgetRelation::Pushed)) = gutter {
            Some(GutteredBuffer { ns, pa, buffer: self, gutter })
        } else {
            None
        }
    }
}

/// The default [`Gutter`] renderer.
///
/// You can use this if you want to render things differently in some
/// situations, but not all.
pub fn default_renderer(entries: &GutterEntries, ns: Ns, mut parts: TextParts<'_>) {
    for entry in &entries.list {
        let form_tag = match entry.kind {
            EntryKind::Hint => form::id_of!("buffer.hint").to_tag(190),
            EntryKind::Warning => form::id_of!("buffer.warning").to_tag(191),
            EntryKind::Error => form::id_of!("buffer.error").to_tag(192),
            EntryKind::_Custom(.., text_form) => text_form.to_tag(193),
        };

        parts.tags.insert(ns, entry.range.clone(), form_tag);
    }
}

type Renderer = dyn FnMut(&GutterEntries, Ns, TextParts<'_>) + 'static + Send;
type OnlyOnHover = bool;
type OnWindow = bool;
