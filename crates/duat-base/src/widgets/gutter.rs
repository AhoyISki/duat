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
    iter::Peekable,
    ops::Range,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    Ns,
    buffer::{Buffer, Moment},
    context::{self, Handle},
    data::Pass,
    form::{self, FormId},
    hook::{self, BufferOpened, BufferUpdated, KeyTyped, OnMouseEvent},
    opts::PrintOpts,
    storage::bincode,
    text::{Builder, Inlay, Mask, Overlay, Point, Text, TextRange, TwoPoints},
    txt,
    ui::{Area, Coord, Corner, PushSpecs, Side, Widget},
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
    entries: Vec<GutterEntry>,
    opts: GutterOpts,
    ns: Ns,
}

/// Initial setup for the [`Gutter`].
pub fn gutter_setup() {
    hook::add::<BufferOpened>(move |pa, buffer| _ = buffer.read(pa).moment_for(*MOMENT_NS));
    hook::add::<BufferUpdated>(update).lateness(100_000_000);

    hook::add::<BufferUpdated>(|pa, buffer| {
        let Some((gutter, _)) = buffer.get_related::<Gutter>(pa).first().cloned() else {
            return;
        };

        gutter.write(pa).text = Gutter::form_text(gutter.read(pa), pa, buffer);
    })
    .lateness(usize::MAX);

    hook::add::<OnMouseEvent<Buffer>>(move |_, event| {
        let mut current = CURRENT.lock().unwrap();
        current.mouse_coord = Some(event.coord);
        current.related.clear();
    })
    .lateness(usize::MAX);

    hook::add::<KeyTyped>(|_, _| CURRENT.lock().unwrap().mouse_coord = None);
}

fn update(pa: &mut Pass, buffer: &Handle) {
    let Some((gutter, _)) = buffer.get_related::<Gutter>(pa).first().cloned() else {
        return;
    };

    let (gtr, buf) = pa.write_many((&gutter, buffer));
    if gtr.entries.is_empty() {
        return;
    }

    gtr.apply_changes(buf.moment_for(*MOMENT_NS));

    let (gtr, buf, area) = pa.write_many((&gutter, buffer, buffer.area()));
    let popts = buf.print_opts();

    let get_entry_lists = |point: Point, on_whole_line: bool| {
        let direct = Vec::from_iter(gtr.entries.iter().filter_map(|entry| {
            if on_whole_line {
                let range = {
                    let range = buf.text()[entry.range.clone()].range();
                    range.start.line()..range.end.line() + 1
                };
                range.contains(&point.line()).then_some(entry.id)
            } else {
                entry.range.contains(&point.byte()).then_some(entry.id)
            }
        }));

        let related = Vec::from_iter(
            ID_RELATIONS
                .lock()
                .unwrap()
                .iter()
                .filter(|related| direct.iter().any(|id| related.contains(id)))
                .flatten()
                .copied(),
        );

        (direct, related)
    };

    let mut current = CURRENT.lock().unwrap();

    let (updated, active_entries) = if let Some(coord) = current.mouse_coord
        && (coord >= area.top_left() && coord < area.bottom_right())
        && let Some(point) = (|| {
            Some(
                area.points_at_coord(buf.text(), coord, popts)?
                    .as_within()?
                    .real,
            )
        })() {
        (
            true,
            Some((
                get_entry_lists(point, gtr.opts.hover_whole_line),
                Movement::Hovered,
            )),
        )
    } else if area.is_active() {
        (
            true,
            Some((
                get_entry_lists(buf.selections().main().cursor(), false),
                Movement::Cursored,
            )),
        )
    } else {
        (false, None)
    };

    let direct = if let Some(((direct, related), movement)) = active_entries {
        current.related = related;
        Some((direct, movement))
    } else {
        None
    };

    let mut entries = gtr
        .entries
        .iter_mut()
        .map(|entry| {
            if let Some((direct, movement)) = &direct
                && direct.contains(&entry.id)
            {
                (entry, Some(*movement))
            } else if current.related.contains(&entry.id) {
                (entry, Some(Movement::Related))
            } else {
                (entry, None)
            }
        })
        .peekable();

    while let Some((entry, _)) = entries.peek() {
        let lnum = buf.text().point_at_byte(entry.range.end).line();
        insert_gutter_entries(&mut entries, gtr.ns, lnum, &gtr.opts, buf, area);
    }

    if updated {
        let buffer = buffer.clone();
        context::queue(move |pa| {
            for other in context::current_window(pa).buffers(pa) {
                if other != buffer {
                    update(pa, &other);
                }
            }
        })
    }
}

impl Gutter {
    /// A builder for a `Gutter`.
    pub fn builder() -> GutterOpts {
        GutterOpts::default()
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

            let (Ok(idx) | Err(idx)) = self
                .entries
                .binary_search_by(|entry| entry.range.start.cmp(&range.start));

            let mut iter = self.entries[idx..].iter();
            while let Some(entry) = iter.next()
                && entry.range.start < range.end
            {
                kind = Some(match kind {
                    Some(kind) => entry.kind.min(kind),
                    None => entry.kind,
                });
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

        let mut iter = self.entries.iter_mut().enumerate();
        let mut shift = 0;
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

                (!is_contained(i, entry.range.clone()) && entry.range.end > change.start().byte())
                    .then_some((i, entry))
            }) {
                let start_shift =
                    change.shift()[0] * (entry.range.start > change.start().byte()) as i32;

                sh(&mut entry.range.start, start_shift);
                sh(&mut entry.range.end, change.shift()[0]);
            }
            shift += change.shift()[0];
        }

        for (_, entry) in iter {
            sh(&mut entry.range.start, shift);
            sh(&mut entry.range.end, shift);
        }

        for idx in to_remove.into_iter().rev() {
            self.entries.remove(idx);
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
#[derive(Clone, Copy)]
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
    /// Which [`Corner`] to spawn when spawning a corner widget.
    ///
    /// The default is [`Corner::TopRight`].
    pub corner: Corner,
    /// Wether to spawn on the window's corner instead of the
    /// [`Buffer`].
    ///
    /// The default is `false`.
    pub spawn_on_window_corner: bool,
    /// Wether to consider the whole line as hovered instead of just
    /// the entries containing the mouse pointer.
    ///
    /// This is useful because a hover is "deliberate", so you
    /// probably want as much information out of it as possible.
    ///
    /// The default is `true`.
    pub hover_whole_line: bool,
}

impl GutterOpts {
    /// Returns a new `GutterOpts`.
    pub const fn new() -> Self {
        Self {
            hint: GutterSymbolOpts {
                symbol: 'i',
                default_display: None,
                hover_display: Some(GutterDisplay::RightUnder),
                cursor_display: Some(GutterDisplay::EndOfLine),
                related_display: Some(GutterDisplay::EndOfLine),
            },
            warning: GutterSymbolOpts {
                symbol: '!',
                default_display: Some(GutterDisplay::EndOfLine),
                hover_display: Some(GutterDisplay::RightUnder),
                cursor_display: Some(GutterDisplay::EndOfLine),
                related_display: Some(GutterDisplay::EndOfLine),
            },
            error: GutterSymbolOpts {
                symbol: '*',
                default_display: Some(GutterDisplay::EndOfLine),
                hover_display: Some(GutterDisplay::RightUnder),
                cursor_display: Some(GutterDisplay::EndOfLine),
                related_display: Some(GutterDisplay::EndOfLine),
            },
            corner: Corner::TopRight,
            spawn_on_window_corner: false,
            hover_whole_line: true,
        }
    }

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
                entries: Vec::new(),
                opts: self,
                ns: Ns::new(),
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

impl Default for GutterOpts {
    fn default() -> Self {
        Self::new()
    }
}

/// Options for symbols in the [`Gutter`].
///
/// This includes how the message associated with the symbol should be
/// displayed when the cursor is hovered, when the mouse is hovered,
/// when [related to one of those two], and when not.
///
/// [related to one of those two]: GutterEntryId::relate_with_other_entries
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GutterSymbolOpts {
    /// Which symbol to show on the [`Gutter`]
    pub symbol: char,
    /// How the text should be shown by default.
    ///
    /// `None` means that it shouldn't be shown at all.
    pub default_display: Option<GutterDisplay>,
    /// How the text should be shown when hovered.
    ///
    /// `None` means that it shouldn't be shown at all.
    pub hover_display: Option<GutterDisplay>,
    /// How the text should be shown when the cursor is placed within
    /// its range.
    ///
    /// `None` means that it shouldn't be shown at all.
    pub cursor_display: Option<GutterDisplay>,
    /// How the text should be shown when it is related to another
    /// entry that has been hovered or cursored over.
    ///
    /// It's [`Some(GutterDisplay::EndOfLine)`] by default.
    ///
    /// [`Some(GutterDisplay::EndOfLine)`]: GutterDisplay::EndOfLine
    pub related_display: Option<GutterDisplay>,
}

/// An entry in the [`Gutter`].
///
/// This contains a range in the [`Text`] and a message, in the form
/// of a `Text`.
#[derive(Debug)]
pub struct GutterEntry {
    range: Range<usize>,
    msg: Text,
    kind: EntryKind,
    id: GutterEntryId,
    place: Option<BufferPlace>,
    ns: Ns,
}

/// Inserts multiple [`GutterEntry`]s that are in the same line.
fn insert_gutter_entries<'g>(
    iter: &mut Peekable<impl Iterator<Item = (&'g mut GutterEntry, Option<Movement>)>>,
    ns: Ns,
    lnum: usize,
    opts: &GutterOpts,
    buf: &mut Buffer,
    area: &Area,
) {
    let display = |kind: EntryKind, movement| match movement {
        Some(Movement::Hovered) => opts.symbol_opts(kind).hover_display,
        Some(Movement::Cursored) => opts.symbol_opts(kind).cursor_display,
        Some(Movement::Related) => opts.symbol_opts(kind).related_display,
        None => opts.symbol_opts(kind).default_display,
    };

    let popts = buf.print_opts();
    let mut start_of_next = buf.text().len();

    let entries = Vec::from_iter(
        std::iter::from_fn(|| {
            iter.next_if(|(entry, _)| {
                let next_lnum = buf.text().point_at_byte(entry.range.end).line();
                if next_lnum == lnum {
                    true
                } else {
                    if next_lnum < buf.text().end_point().line() {
                        start_of_next = buf.text().line(next_lnum).byte_range().start;
                    }
                    false
                }
            })
        })
        .map(|(entry, movement)| {
            let place = match display(entry.kind, movement) {
                Some(GutterDisplay::EndOfLine) => Some(BufferPlace::EndOfLine({
                    let lnum = buf.text().point_at_byte(entry.range.end).line();
                    buf.text().line(lnum).len()
                })),
                Some(GutterDisplay::RightUnder) => {
                    inlay_column(entry.range.clone(), buf, area, popts).map(BufferPlace::RightUnder)
                }
                Some(GutterDisplay::Spawn) => Some(BufferPlace::Spawned),
                Some(GutterDisplay::Corner) | None => None,
            };
            (entry, movement, place)
        }),
    );

    // Most likely scenario: Nothing has changed in how these Gutter
    // entries should be displayed, so do nothing.
    if entries
        .iter()
        .all(|(entry, _, place)| entry.place == *place)
    {
        return;
    }

    let mut parts = buf.text_parts();
    let line_range = parts.strs.line(lnum).byte_range();
    // Pick as much range as possible, in order to account for the
    // possibility of the RightUnder Tag having moved because of changes.
    parts.tags.remove(ns, line_range.start + 1..=start_of_next);

    let mut inlays = Vec::new();
    let mut overlays = Vec::new();

    for (entry, _, place) in entries {
        entry.place = place;
        let Some(place) = entry.place else {
            continue;
        };

        match place {
            BufferPlace::EndOfLine(_) => overlays.push((entry.kind, &entry.msg)),
            BufferPlace::RightUnder(column) => inlays.push((column, entry.kind, &entry.msg)),
            BufferPlace::Spawned => todo!(),
        }
    }

    if !inlays.is_empty() {
        parts.tags.insert(ns, line_range.end, make_inlay(inlays));
    }
    if !overlays.is_empty() {
        let pos = line_range.end - 1;
        parts.tags.insert(ns, pos, make_overlay(overlays));
    }
}

fn make_inlay(mut inlays: Vec<(usize, EntryKind, &Text)>) -> Inlay {
    const LEN: usize = '─'.len_utf8();
    const LINES: &str = {
        const BYTES: [[u8; LEN]; 150] = {
            let mut encoded = [0; LEN];
            '─'.encode_utf8(&mut encoded);
            [encoded; 150]
        };
        unsafe { std::str::from_utf8_unchecked(BYTES.as_flattened()) }
    };
    static CONN: LazyLock<Overlay> = LazyLock::new(|| Overlay::new(txt!("│")));

    inlays.sort_unstable_by(|(l_column, l_kind, l_text), (r_column, r_kind, r_text)| {
        l_column
            .cmp(r_column)
            .then(l_kind.cmp(r_kind).reverse())
            .then(l_text.len().cmp(&r_text.len()))
    });

    let mut prefix = {
        let column = inlays.last().unwrap().0.max(2);
        txt!("[diagnostic.line:240]{}", &SPACES[..column + 1])
    };
    let prefix_len = prefix.len() - 1;
    let conn = &*CONN;

    let mut text = Text::new();
    let mut prev_column = None;

    for (column, _, msg) in inlays {
        let columns_are_eq = prev_column == Some(column);
        let char = if columns_are_eq { '├' } else { '└' };

        let mut line_prefix = prefix.clone();
        line_prefix.replace_range(
            column..prefix_len,
            format!("{char}{}", &LINES[..(prefix_len - column - 1) * LEN]),
        );

        if columns_are_eq {
            line_prefix.remove_tags(Ns::basic(), column);
        }

        let line_ranges = Vec::from_iter(msg.lines().map(|line| line.byte_range()));

        let mut msg = msg.clone();
        msg.replace_range(msg.len()..msg.len(), "\n");

        for (i, line_range) in line_ranges.into_iter().enumerate().rev() {
            if i == 0 {
                msg.insert_text(line_range.start, &line_prefix);
                // Add it now, so the next lines are also connected.
            } else {
                msg.insert_text(line_range.start, &prefix);
            }
        }

        if !columns_are_eq {
            prefix.insert_tag(Ns::basic(), column, conn.clone());
        }

        text.insert_text(0, &msg);
        prev_column = Some(column)
    }

    text.insert_tag(Ns::basic(), .., Mask("diagnostic"));

    Inlay::new(text)
}

fn make_overlay(mut overlays: Vec<(EntryKind, &Text)>) -> Overlay {
    overlays.sort_unstable_by(|(l_kind, l_text), (r_kind, r_text)| {
        l_kind.cmp(r_kind).then(l_text.len().cmp(&r_text.len()))
    });

    let fmt = |i: usize, mut builder: Builder, msg: &Text| {
        if i == 0 {
            builder.push(Mask("diagnostic"));
            builder.push(txt!("  {msg}"));
        } else {
            builder.push(txt!(" [diagnostic.line]│[] {msg}"));
        }
        builder
    };

    Overlay::new(
        overlays
            .into_iter()
            .enumerate()
            .fold(Text::builder(), |builder, (i, (_, msg))| {
                if msg.end_point().line() > 1 {
                    let mut msg = msg.clone();
                    msg.replace_range(msg.lines().next().unwrap().byte_range().end - 1.., "");
                    fmt(i, builder, &msg)
                } else {
                    fmt(i, builder, msg)
                }
            })
            .build(),
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum EntryKind {
    _Custom(char, FormId, FormId),
    Error,
    Warning,
    Hint,
}

/// How to display the accompanying [`Text`] message to a [`Gutter`]
/// entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(unused)]
pub enum GutterDisplay {
    /// The [`Text`] will be shown at the end of the line, potentially
    /// running off out of screen.
    EndOfLine,
    /// The [`Text`] will be shown as a spawned widget near the
    /// entry's range.
    ///
    /// While you _can_ set this as the any display option, I would
    /// advise limiting the usage of this method to [`hover_display`],
    /// since many widgets spawned at once will look really jarring.
    ///
    /// [`hover_display`]: GutterSymbolOpts::hover_display
    Spawn,
    /// The [`Text`] will be show as a spawned widget on one of the
    /// corners.
    ///
    /// If [`OnWindow`] is set to true, this will spawn it on the
    /// corners of the window. Otherwise, it will be spawned on the
    /// corners of the [`Buffer`]
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    Corner,
    /// The [`Text`] will be shown as [`Inlay`] lines under the
    /// entry's range.
    RightUnder,
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
    ///
    /// Note: This function won't add duplicated entries, instead
    /// returning the [`GutterEntryId`] of the entry that was already
    /// in there.
    fn add_hint(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text) -> GutterEntryId;

    /// Add a warning to the [`Gutter`] and the [`Buffer`].
    ///
    /// This could be improvements that you could do to your code, or
    /// ways in which it is innadequate that don't necessarily hinder
    /// it from working properly.
    ///
    /// Note: This function won't add duplicated entries, instead
    /// returning the [`GutterEntryId`] of the entry that was already
    /// in there.
    fn add_warning(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text)
    -> GutterEntryId;

    /// Add an error to the [`Gutter`] and the [`Buffer`].
    ///
    /// These are fundamental issues in your code, and either prevent
    /// compilation, or prevent it from working properly.
    ///
    /// Note: This function won't add duplicated entries, instead
    /// returning the [`GutterEntryId`] of the entry that was already
    /// in there.
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

        let (gtr, buf) = pa.write_many((&gutter, self));
        buf.text_mut().remove_tags(ns, ..);

        let moment = buf.moment_for(*MOMENT_NS);
        if !moment.is_empty() {
            gtr.apply_changes(moment);
        }

        let removed_line_ranges = Vec::from_iter(
            gtr.entries
                .extract_if(.., |entry| entry.ns == ns)
                .map(|entry| {
                    let lnum = buf.text().point_at_byte(entry.range.end).line();
                    let line_range = buf.text().line(lnum).byte_range();
                    buf.text_mut()
                        .remove_tags(gtr.ns, line_range.end - 1..=line_range.end);
                    line_range
                }),
        );

        for entry in &mut gtr.entries {
            if removed_line_ranges
                .iter()
                .any(|range| range.contains(&entry.range.end))
            {
                entry.place = None;
            }
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

        let (gtr, buf) = pa.write_many((&gutter, self));
        let range = range.to_range(buf.text().len());

        if let Some(entry) = gtr
            .entries
            .iter()
            .find(|entry| entry.range == range && entry.msg == msg && entry.kind == EntryKind::Hint)
        {
            return entry.id;
        }

        let form_tag = form::id_of!("buffer.hint").to_tag(190);
        buf.text_mut().insert_tag(ns, range.clone(), form_tag);

        let id = GutterEntryId::new(ns);
        let entry = GutterEntry {
            range,
            msg,
            kind: EntryKind::Hint,
            id,
            place: None,
            ns,
        };

        let moment = buf.moment_for(*MOMENT_NS);
        if !moment.is_empty() {
            gtr.apply_changes(moment);
        }

        let (Ok(idx) | Err(idx)) = gtr
            .entries
            .binary_search_by(|e| e.range.start.cmp(&entry.range.start));
        gtr.entries.insert(idx, entry);

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

        let (gtr, buf) = pa.write_many((&gutter, self));
        let range = range.to_range(buf.text().len());

        if let Some(entry) = gtr.entries.iter().find(|entry| {
            entry.range == range && entry.msg == msg && entry.kind == EntryKind::Warning
        }) {
            return entry.id;
        }

        let form_tag = form::id_of!("buffer.warn").to_tag(191);
        buf.text_mut().insert_tag(ns, range.clone(), form_tag);

        let id = GutterEntryId::new(ns);
        let entry = GutterEntry {
            range,
            msg,
            kind: EntryKind::Warning,
            id,
            place: None,
            ns,
        };

        let moment = buf.moment_for(*MOMENT_NS);
        if !moment.is_empty() {
            gtr.apply_changes(moment);
        }

        let (Ok(idx) | Err(idx)) = gtr
            .entries
            .binary_search_by(|e| e.range.start.cmp(&entry.range.start));
        gtr.entries.insert(idx, entry);

        id
    }

    #[track_caller]
    fn add_error(&self, pa: &mut Pass, ns: Ns, range: impl TextRange, msg: Text) -> GutterEntryId {
        let Some((gutter, _)) = self.get_related::<Gutter>(pa).first().cloned() else {
            panic!("Tried to add a Gutter entry on Buffer with no Gutter");
        };

        let (gtr, buf) = pa.write_many((&gutter, self));
        let range = range.to_range(buf.text().len());

        if let Some(entry) = gtr.entries.iter().find(|entry| {
            entry.range == range && entry.msg == msg && entry.kind == EntryKind::Error
        }) {
            return entry.id;
        }

        let form_tag = form::id_of!("buffer.error").to_tag(192);
        buf.text_mut().insert_tag(ns, range.clone(), form_tag);

        let id = GutterEntryId::new(ns);
        let entry = GutterEntry {
            range,
            msg,
            kind: EntryKind::Error,
            id,
            place: None,
            ns,
        };

        let (gtr, buf) = pa.write_many((&gutter, self));
        let moment = buf.moment_for(*MOMENT_NS);
        if !moment.is_empty() {
            gtr.apply_changes(moment);
        }

        let (Ok(idx) | Err(idx)) = gtr
            .entries
            .binary_search_by(|e| e.range.start.cmp(&entry.range.start));
        gtr.entries.insert(idx, entry);

        id
    }

    fn has_gutter(&self, pa: &Pass) -> bool {
        !self.get_related::<Gutter>(pa).is_empty()
    }
}

/// An id for a [`Gutter`] entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, bincode::Decode, bincode::Encode)]
#[bincode(crate = "duat_core::context::cache::bincode")]
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

        let mut id_relations = ID_RELATIONS.lock().unwrap();
        if !id_relations.contains(&ids) {
            id_relations.push(ids);
        }
    }
}

const SPACES: &str = unsafe { std::str::from_utf8_unchecked(&[b' '; 1000]) };
static ID_RELATIONS: Mutex<Vec<Vec<GutterEntryId>>> = Mutex::new(Vec::new());
static EXTANT_IDS: Mutex<Vec<GutterEntryId>> = Mutex::new(Vec::new());
static MOMENT_NS: LazyLock<Ns> = Ns::new_lazy();
static CURRENT: Mutex<Current> = Mutex::new(Current { related: Vec::new(), mouse_coord: None });

#[derive(Debug, Clone, Copy)]
enum Movement {
    Hovered,
    Cursored,
    Related,
}

/// Where to place a [`GutterEntry`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BufferPlace {
    EndOfLine(usize),
    RightUnder(usize),
    Spawned,
}

fn inlay_column(range: Range<usize>, buf: &Buffer, area: &Area, opts: PrintOpts) -> Option<usize> {
    let line_range = buf.text()[range.clone()]
        .lines()
        .last()
        .unwrap_or(&buf.text()[range])
        .range();

    let two_points = TwoPoints::new_after_ghost(line_range.start);
    Some(area.columns_at(buf.text(), two_points, opts)?.wrapped)
}

struct Current {
    related: Vec<GutterEntryId>,
    mouse_coord: Option<Coord>,
}
