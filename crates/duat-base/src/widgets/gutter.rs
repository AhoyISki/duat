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
    context::{Handle, WidgetRelation},
    data::Pass,
    form::{self, Form, FormId},
    hook::{self, BufferUpdated},
    text::{Ghost, Tagger, Text, TextParts, TextRange},
    txt,
    ui::{PushSpecs, Side, Widget},
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
    entries: HashMap<Gutterer, HashMap<usize, Vec<GutterEntry>>>,
    opts: GutterOpts,
}

impl Gutter {
    /// A builder for a `Gutter`.
    pub fn builder() -> GutterOpts {
        static ONCE: Once = Once::new();

        ONCE.call_once(|| {
            form::set_weak("gutter.hint", Form::mimic("default.hint"));
            form::set_weak("gutter.warning", Form::mimic("default.warning"));
            form::set_weak("gutter.error", Form::mimic("default.error"));
            form::set_weak("buffer.hint", Form::new().underline_grey().underlined());
            form::set_weak(
                "buffer.warning",
                Form::new().underline_yellow().underlined(),
            );
            form::set_weak("buffer.error", Form::new().underline_red().underlined());

            hook::add::<BufferUpdated>(|pa, buffer| {
                let Some((gutter, _)) = buffer.get_related::<Gutter>(pa).next() else {
                    return;
                };

                gutter.write(pa).text = gutter.read(pa).form_text(pa, buffer);
            });
        });

        GutterOpts {
            hint: GutterSymbolOpts {
                symbol: 'i',
                display: GutterDisplay::Spawn(true),
            },
            warning: GutterSymbolOpts {
                symbol: '!',
                display: GutterDisplay::Spawn(true),
            },
            error: GutterSymbolOpts {
                symbol: '*',
                display: GutterDisplay::OwnLines(false),
            },
            renderer: Some(Box::new(default_renderer)),
        }
    }

    fn form_text(&self, pa: &Pass, buffer: &Handle) -> Text {
        let printed_line_numbers = buffer.printed_line_numbers(pa);

        let mut builder = Text::builder();

        for (idx, line) in printed_line_numbers.iter().enumerate() {
            if idx > 0 && line.is_wrapped {
                builder.push(" \n");
                continue;
            };

            let mut kind = None;

            for (_, entries) in self.entries.iter() {
                if let Some(entries) = entries.get(&line.number) {
                    for entry in entries {
                        kind = kind.max(Some(entry.kind));
                    }
                }
            }

            if let Some(kind) = kind {
                let (symbol, symbol_form) = match kind {
                    EntryKind::Hint => (self.opts.hint.symbol, form::id_of!("self.hint")),
                    EntryKind::Warning => (self.opts.warning.symbol, form::id_of!("self.warning")),
                    EntryKind::Error => (self.opts.error.symbol, form::id_of!("self.error")),
                    EntryKind::Custom(symbol, symbol_form, _) => (symbol, symbol_form),
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
}

impl Widget for Gutter {
    fn update(_: &mut Pass, _: &Handle<Self>) {}

    fn needs_update(&self, _: &Pass) -> bool {
        false
    }

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
    /// [`Ghost`]: duat_core::text::Ghost
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
    pub symbol: char,
    pub display: GutterDisplay,
}

/// Related entries on the [`Gutter`].
pub struct GutterEntries {
    /// The entries that are related.
    pub list: Vec<GutterEntry>,
    /// How to display the entry's message.
    pub display: GutterDisplay,
}

/// An entry in the [`Gutter`].
///
/// This contains a range in the [`Text`] and a message, in the form
/// of a `Text`.
pub struct GutterEntry {
    pub range: Range<usize>,
    pub msg: Text,
    kind: EntryKind,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum EntryKind {
    Hint,
    Warning,
    Error,
    Custom(char, FormId, FormId),
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
    gutterer: Gutterer,
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
        renderer(&self.entries, self.gutterer, buf.text_mut().parts());
        gutter.opts.renderer = Some(renderer);

        let map = gutter.entries.entry(self.gutterer).or_default();

        for entry in std::mem::take(&mut self.entries.list) {
            let line = buf.text().point_at_byte(entry.range.start).line();
            map.entry(line).or_default().push(entry);
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
    gutterer: Gutterer,
    pa: &'g mut Pass,
    buffer: &'g Handle,
    gutter: Handle<Gutter>,
}

impl<'g> GutteredBuffer<'g> {
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
            gutterer: self.gutterer,
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
            gutterer: self.gutterer,
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
            gutterer: self.gutterer,
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
    fn get_guttered<'g>(
        &'g self,
        pa: &'g mut Pass,
        gutterer: Gutterer,
    ) -> Option<GutteredBuffer<'g>>;
}

impl Sealed for Handle {}
impl GetGuttered for Handle {
    fn get_guttered<'g>(
        &'g self,
        pa: &'g mut Pass,
        gutterer: Gutterer,
    ) -> Option<GutteredBuffer<'g>> {
        let gutter = self.get_related(pa).next();
        if let Some((gutter, WidgetRelation::Pushed)) = gutter {
            Some(GutteredBuffer { gutterer, pa, buffer: self, gutter })
        } else {
            None
        }
    }
}

/// The default [`Gutter`] renderer.
///
/// You can use this if you want to render things differently in some
/// situations, but not all.
pub fn default_renderer(entries: &GutterEntries, gutterer: Gutterer, mut parts: TextParts<'_>) {
    let tagger = gutterer.0;

    for entry in &entries.list {
        let form_tag = match entry.kind {
            EntryKind::Hint => form::id_of!("buffer.hint").to_tag(190),
            EntryKind::Warning => form::id_of!("buffer.warning").to_tag(191),
            EntryKind::Error => form::id_of!("buffer.error").to_tag(192),
            EntryKind::Custom(.., text_form) => text_form.to_tag(193),
        };

        parts.tags.insert(tagger, entry.range.clone(), form_tag);

        match entries.display {
            GutterDisplay::Inline(_) => {},
            GutterDisplay::Spawn(_) => {},
            GutterDisplay::SpawnCorner(..) => {},
            GutterDisplay::OwnLines(_) => {
                let msg_start = parts
                    .strs
                    .line(parts.strs.point_at_byte(entry.range.end).line())
                    .end_point();
                parts
                    .tags
                    .insert(tagger, msg_start, Ghost::new(txt!("{entry.msg}\n")));
            }
        }
    }
}

type Renderer = dyn FnMut(&GutterEntries, Gutterer, TextParts<'_>) + 'static + Send;
type OnlyOnHover = bool;
type OnWindow = bool;

/// A namesapce for [`Gutter`] entries.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Gutterer(Tagger);

impl Gutterer {
    /// Returns a new [`Gutterer`], a struct for adding/removing
    /// [`Gutter`] entries.
    pub fn new() -> Self {
        Self(Tagger::new())
    }
}

impl Default for Gutterer {
    fn default() -> Self {
        Self::new()
    }
}
