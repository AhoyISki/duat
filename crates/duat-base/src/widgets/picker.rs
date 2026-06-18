//! A [`Widget`] to show a list of places (mostly) to jump to.
//!
//! These will be listed in a spawned widget, and may or may not point
//! to real places in files. It can also be useed for other things,
//! like listing code actions and their respective results if they are
//! performed.
use std::{any::Any, marker::PhantomData, ops::Range, path::PathBuf};

use duat_core::{
    buffer::Buffer,
    cmd,
    context::{self, Handle},
    data::{Pass, RwData},
    hook::{self, FocusedUpdated, WidgetSwitched},
    mode,
    opts::PrintOpts,
    text::{Text, TextMut},
    ui::{Coord, PushSpecs, Side, StaticSpawnSpecs, Widget},
};
use duat_term::Frame;

use crate::hooks::{PickerEntryFocused, PickerEntrySelected};

/// Setup for the [`Picker`] widget.
pub(crate) fn picker_setup() {
    hook::add::<WidgetSwitched>(|pa, (old, new)| {
        if let Some(picker) = context::handle_of::<Picker>(pa)
            && new.widget().is::<Buffer>()
        {
            _ = picker.close(pa);
            _ = picker.read(pa).preview.clone().close(pa);
        } else if let Some(preview) = old.get_as::<Preview>()
            && let pv = preview.write(pa)
            && let Some(PreviewMode::BufferPreview(idx, .., filetype)) = &pv.modes[pv.current]
        {
            let (path, text) = &mut pv.preview_texts[*idx];

            duat_core::try_or_log_err! {
                let file = std::fs::OpenOptions::new()
                    .write(true)
                    .truncate(true)
                    .create(true)
                    .open(path)?;
                text.save_on(file)?;
            }

            text.clear_tags();
            if let Some(filetype) = filetype {
                duat_treesitter::parse_as(text.as_mut(), .., filetype);
            }
        }
    });

    hook::add::<FocusedUpdated<Picker>>(|pa, picker| {
        let idx = {
            let pkr = picker.write(pa);
            let byte = pkr.text.main_sel().cursor().byte();

            pkr.maps
                .iter()
                .position(|(r, _)| r.contains(&byte))
                .unwrap()
        };

        let pv = picker.write_then(pa, |pkr| &pkr.preview);
        pv.current = idx;
        if let Some(mode) = &pv.modes[idx] {
            let popts = pv.print_opts();
            let range = mode.range();

            let (text, area) =
                picker.write_then(pa, |pkr| (pkr.preview.rw_text(), pkr.preview.area()));

            let point = text.point_at_byte(range.start);
            let dist = area.height() / 2.0;
            area.scroll_to(&text, point.to_two_points_after(), dist, popts);

            return;
        }

        let pkr = picker.write(pa);
        let (range, item) = pkr.maps.remove(idx);
        let preview = pkr.preview.clone();

        let item = (pkr.focused_fn)(pa, item, PickerPreview(preview));

        let pkr = picker.write(pa);
        pkr.maps.insert(idx, (range, item));
    });

    hook::add::<FocusedUpdated<Preview>>(|pa, preview| {
        let pv = preview.read(pa);

        if let Some(PreviewMode::BufferPreview(.., Some(filetype))) = &pv.modes[pv.current] {
            let filetype = *filetype;
            let popts = preview.opts(pa);
            let (mut text, area) = preview.write_text_and_area(pa);

            let start = area.start_points(&text, popts);
            let end = area.end_points(&text, popts);

            text.clear_tags();
            duat_treesitter::parse_as(text, start.real..end.real, filetype);
        }
    })
    .lateness(usize::MAX);

    hook::add::<PickerEntryFocused<FilePlace>>(|pa, (place, preview)| {
        preview.set_place(pa, place.clone());
    });

    hook::add::<PickerEntrySelected<FilePlace>>(|pa, place| {
        let coords = |text: &Text| {
            let start = text.point_at_byte(place.range.start);
            let end = text.point_at_byte(place.range.end);
            format!(
                "{}:{}..{}:{}",
                start.line() + 1,
                start.char_col(text) + 1,
                end.line() + 1,
                end.char_col(text)
            )
        };

        let Some(preview) = context::handle_of::<Preview>(pa) else {
            let text = match std::fs::read_to_string(&place.path).map(Text::from) {
                Ok(text) => text,
                Err(err) => {
                    context::error!("couldn't read {place.path}: {err}");
                    return;
                }
            };

            let coords = coords(&text);
            _ = cmd::call_notify(
                pa,
                format!("open '{}' {coords}", place.path.to_string_lossy()),
            );

            return;
        };

        let pv = preview.write(pa);
        let on_new_window = pv.on_new_window;

        match pv.modes.remove(pv.current) {
            Some(PreviewMode::BufferMirror(buffer, range)) => {
                buffer.edit_main(pa, |mut s| s.move_to(range.clone()));
                buffer.remove_extra_selections(pa);
                mode::reset_to(pa, &buffer);
            }
            Some(PreviewMode::BufferPreview(idx, range, _)) => {
                let (path, text) = pv.preview_texts.remove(idx);

                if let Some(buffer) = context::buffer_from_path(pa, &path) {
                    buffer.text_mut(pa).replace_range(.., text.to_string());
                    buffer.edit_main(pa, |mut s| s.move_to(range.clone()));
                    buffer.remove_extra_selections(pa);
                    mode::reset_to(pa, &buffer);
                } else {
                    duat_core::try_or_log_err! {
                        let file = std::fs::OpenOptions::new()
                            .write(true)
                            .truncate(true)
                            .create(true)
                            .open(&path)?;
                        text.save_on(file)?;
                    };

                    let cmd = if on_new_window { "open" } else { "edit" };
                    let path = path.to_string_lossy();

                    let coords = coords(&text);
                    _ = cmd::call_notify(pa, format!("{cmd} '{path}' {coords}",));
                }
            }
            Some(PreviewMode::Text(..)) | None => {}
        }
    })
    .lateness(50);
}

/// A [`Widget`] that is mostly used to jump to locations in
/// [`Buffer`]s.
///
/// [`Buffer`]: duat_core::buffer::Buffer
pub struct Picker {
    maps: Vec<(Range<usize>, Entry)>,
    text: Text,
    preview: Handle<Preview>,
    focused_fn: fn(&mut Pass, Entry, PickerPreview) -> Entry,
    selected_fn: fn(&mut Pass, Entry),
}

impl Picker {
    /// Spawn a new picker for jumping to [`Buffer`] locations.
    ///
    /// If the list is empty, does nothing. If the list has only one
    /// element, then the `Picker` won't be spawned, but instead that
    /// one element will be picked automatically.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    #[track_caller]
    pub fn spawn_jumps(pa: &mut Pass, list: impl IntoIterator<Item = (Text, FilePlace)>) {
        let window = context::current_window(pa).clone();
        let w_width = window.width();
        let w_height = window.height();
        let width = w_width / 3.0;
        let height = 2.0 * w_height / 3.0;

        let specs = StaticSpawnSpecs {
            top_left: Coord {
                x: ((w_width / 2.0) - width),
                y: ((w_height - height) / 2.0),
            },
            size: Coord { x: width, y: height },
            fractional_repositioning: Some(true),
            ..Default::default()
        };

        let mut iter = list.into_iter();

        let Some(first_place) = iter.next() else {
            return;
        };

        let mut pv = Preview {
            empty: Text::new(),
            modes: vec![None],
            current: 0,
            on_new_window: true,
            preview_texts: Vec::new(),
        };

        let mut builder = Text::builder();
        let mut maps = Vec::from_iter(
            [first_place]
                .into_iter()
                .chain(iter.inspect(|_| pv.modes.push(None)))
                .map(|(text, place)| {
                    let len = builder.len();
                    builder.push_ref(&text);
                    builder.push("\n");

                    (
                        len..len + text.len(),
                        Box::new(place) as Box<dyn Any + Send + 'static>,
                    )
                }),
        );

        if maps.len() == 1
            && let Some((_, first)) = maps.pop()
        {
            hook::trigger(pa, PickerEntrySelected::<FilePlace>((first, PhantomData)));
            return;
        }

        let preview = window.spawn(pa, pv, specs);

        let picker = preview.push_inner_widget(
            pa,
            Picker {
                maps,
                text: {
                    let mut text = builder.build();
                    text.populate_selections();
                    text
                },
                preview: preview.clone(),
                focused_fn: |pa, item, preview| {
                    hook::trigger(
                        pa,
                        PickerEntryFocused::<FilePlace>((item, preview, PhantomData)),
                    )
                    .0
                    .0
                },
                selected_fn: |pa, item| {
                    _ = hook::trigger(pa, PickerEntrySelected::<FilePlace>((item, PhantomData)))
                },
            },
            PushSpecs {
                side: Side::Left,
                width: Some(width),
                cluster: false,
                ..Default::default()
            },
        );

        mode::reset_to(pa, &picker);

        if let Some(area) = picker.area().write_as::<duat_term::Area>(pa) {
            let frame = Frame {
                left: true,
                right: true,
                above: true,
                below: true,
                ..Frame::default()
            };

            area.set_frame(frame);
        }
    }

    /// Selects the current entry.
    ///
    /// If this is supposet to take place on a new `Window`, then
    /// set `on_new_window` to `true`.
    pub fn select_current(pa: &mut Pass) {
        let Some(picker) = context::handle_of::<Picker>(pa) else {
            context::warn!("Tried selecting the [a]Picker[] entry, but there was no Picker open");
            return;
        };

        _ = picker.close(pa);
        _ = picker.read(pa).preview.clone().close(pa);
        let current = picker.read(pa).preview.read(pa).current;
        let pkr = picker.write(pa);

        let (_, entry) = pkr.maps.remove(current);
        (pkr.selected_fn)(pa, entry);
    }

    /// Focus on the [`Picker`]'s preview pane.
    pub fn focus_preview(pa: &mut Pass) {
        let Some(preview) = context::handle_of::<Preview>(pa) else {
            context::warn!(
                "Tried going to the [a]Picker[]'s preview, but there was no Picker open"
            );
            return;
        };

        mode::reset_to(pa, &preview);
    }

    /// Focus from the [`Picker`]'s prevew pane, back to the `Picker`.
    pub fn unfocus_preview(pa: &mut Pass) {
        let Some(picker) = context::handle_of::<Picker>(pa) else {
            context::warn!("Tried returning to [a]Picker[], but there was no Picker open");
            return;
        };

        mode::reset_to(pa, &picker);
    }

    /// Wether there is a `Picker` open right now.
    pub fn is_open(pa: &Pass) -> bool {
        context::handle_of::<Picker>(pa).is_some()
    }

    /// Wether Duat is currently focused on the preview.
    pub fn is_on_preview(pa: &Pass) -> bool {
        context::current_widget(pa).widget().is::<Preview>()
    }
}

impl Widget for Picker {
    fn text<'p>(widget: &'p RwData<Self>, pa: &'p Pass) -> &'p Text {
        &widget.read(pa).text
    }

    fn text_mut<'p>(widget: &'p RwData<Self>, pa: &'p mut Pass) -> TextMut<'p> {
        widget.write(pa).text.as_mut()
    }

    fn print_opts(&self) -> PrintOpts {
        PrintOpts {
            wrap_lines: true,
            ..PrintOpts::default_for_input()
        }
    }
}

/// A location to jump to in a [`Picker`].
#[derive(Clone, Debug)]
pub struct FilePlace {
    /// The path this `BufferPlace` points to.
    pub path: PathBuf,
    /// The range on the [`Buffer`] that should be selected, in byte
    /// indices.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    pub range: Range<usize>,
}

impl FilePlace {
    /// Returns a new `FilePlace`.
    pub fn new(path: PathBuf, range: Range<usize>) -> Self {
        let path = if let Some(path) = path.to_str()
            && let Ok(path) = duat_core::utils::expand_path(&path)
        {
            let path = PathBuf::from(path.to_string());
            path.canonicalize().unwrap_or(path)
        } else {
            path.canonicalize().unwrap_or(path)
        };

        Self { path, range }
    }
}

/// A preview for the [`Picker`] widget.
///
/// This is the [`Text`] that is shown on the right of the `Picker`,
/// and it may represent a place that can be jumped to, or some
/// arbitrary `Text`.
pub struct PickerPreview(Handle<Preview>);

impl PickerPreview {
    /// Previews a place in a [`Buffer`] or file.
    ///
    /// If this points to a `Buffer` that is already open, then the
    /// preview will mirror the [`Text`] of said `Buffer`, such that
    /// any edits that take place in either one will be reflected on
    /// the other.
    ///
    /// Otherwise, a preview of the given file's `Text` will be opened
    /// up.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    #[track_caller]
    pub fn set_place(&self, pa: &mut Pass, place: FilePlace) {
        let mode = if let Some(buffer) = context::buffer_from_path(pa, &place.path) {
            PreviewMode::BufferMirror(buffer, place.range.clone())
        } else if let Some(idx) = self
            .0
            .read(pa)
            .preview_texts
            .iter()
            .position(|(path, _)| *path == place.path)
        {
            let filetype = duat_filetype::from_filename(&place.path);
            PreviewMode::BufferPreview(idx, place.range.clone(), filetype)
        } else {
            let content = match std::fs::read_to_string(&place.path) {
                Ok(content) => content,
                Err(err) => panic!("{err}"),
            };

            let mut text = Text::from(content);

            let filetype = if let Some(filetype) = duat_filetype::from_filename(&place.path) {
                duat_treesitter::parse_as(text.as_mut(), .., filetype);
                Some(filetype)
            } else {
                None
            };

            let pv = self.0.write(pa);
            pv.preview_texts.push((place.path, text));

            PreviewMode::BufferPreview(pv.preview_texts.len() - 1, place.range.clone(), filetype)
        };

        let pv = self.0.write(pa);
        pv.modes[pv.current] = Some(mode);

        let points = self
            .0
            .text(pa)
            .point_at_byte(place.range.start)
            .to_two_points_after();

        let height = self.0.area().height(pa);
        self.0.scroll_to(pa, points, height / 2.0);
    }
}

struct Preview {
    empty: Text,
    modes: Vec<Option<PreviewMode>>,
    current: usize,
    on_new_window: bool,
    preview_texts: Vec<(PathBuf, Text)>,
}

impl Widget for Preview {
    fn text<'p>(widget: &'p RwData<Self>, pa: &'p Pass) -> &'p Text {
        let pv = widget.read(pa);
        match pv.modes[pv.current].as_ref() {
            Some(PreviewMode::BufferMirror(buffer, _)) => buffer.text(pa),
            Some(PreviewMode::BufferPreview(idx, ..)) => &pv.preview_texts[*idx].1,
            Some(PreviewMode::Text(text, _)) => text,
            None => &pv.empty,
        }
    }

    fn text_mut<'p>(widget: &'p RwData<Self>, pa: &'p mut Pass) -> TextMut<'p> {
        let pv = widget.write(pa);
        match &mut pv.modes[pv.current].as_mut() {
            Some(PreviewMode::BufferPreview(idx, ..)) => {
                // SAFETY: Doing this to circumvent current borrow checking
                // limitations.
                let ptr = &raw mut pv.preview_texts[*idx].1;
                unsafe { ptr.as_mut_unchecked() }.as_mut_with_strs_mutation()
            }
            Some(PreviewMode::Text(text, _)) => {
                // SAFETY: Doing this to circumvent current borrow checking
                // limitations.
                let ptr = text as *mut Text;
                unsafe { ptr.as_mut_unchecked() }.as_mut_with_strs_mutation()
            }
            Some(PreviewMode::BufferMirror(..)) => widget.write_then(pa, |pv| {
                let Some(PreviewMode::BufferMirror(buffer, _)) = &pv.modes[pv.current] else {
                    unreachable!();
                };
                buffer.rw_text()
            }),
            None => {
                // SAFETY: Doing this to circumvent current borrow checking
                // limitations.
                let ptr = &raw mut pv.empty;
                unsafe { ptr.as_mut_unchecked() }.as_mut_with_strs_mutation()
            }
        }
    }

    fn print_opts(&self) -> PrintOpts {
        PrintOpts::default_for_input()
    }
}

pub enum PreviewMode {
    BufferMirror(Handle<Buffer>, Range<usize>),
    BufferPreview(usize, Range<usize>, Option<&'static str>),
    Text(Text, Range<usize>),
}

impl PreviewMode {
    /// The [`TwoPoints`] which should be centered.
    pub fn range(&self) -> Range<usize> {
        match self {
            Self::BufferMirror(_, points) => points.clone(),
            Self::BufferPreview(.., points, _) => points.clone(),
            Self::Text(_, points) => points.clone(),
        }
    }
}

type Entry = Box<dyn Any + Send>;
