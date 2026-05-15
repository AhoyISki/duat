//! A [`Widget`] to show a list of places (mostly) to jump to.
//!
//! These will be listed in a spawned widget, and may or may not point
//! to real places in files. It can also be useed for other things,
//! like listing code actions and their respective results if they are
//! performed.
use std::{any::Any, marker::PhantomData, ops::Range, path::PathBuf};

use duat_core::{
    cmd,
    context::{self, Handle},
    data::{Pass, RwData},
    hook::{self, FocusedUpdated, WidgetOpened, WidgetSwitched},
    mode,
    opts::PrintOpts,
    text::{Text, TextMut},
    ui::{Coord, PushSpecs, Side, StaticSpawnSpecs, Widget},
};
use duat_term::Frame;

use crate::hooks::PickerEntryFocused;

/// Setup for the [`Picker`] widget.
pub(crate) fn picker_setup() {
    hook::add::<WidgetSwitched>(|pa, (old, _)| {
        if old.widget().is::<Picker>() || old.widget().is::<PickerPreview>() {
            _ = old.close(pa);
        }
    });

    hook::add::<WidgetOpened<Picker>>(|pa, picker| {
        mode::reset_to(pa, picker);
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

    hook::add::<PickerEntryFocused<FilePlace>>(|pa, (place, preview)| {
        preview.set_place(pa, place.clone());
    });
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
            context::warn!("Tried spawning a [a]Picker[] with [a]0[] entries");
            return;
        };

        let mut pv = Preview {
            empty: Text::new(),
            modes: vec![None],
            current: 0,
        };

        let mut builder = Text::builder();
        let maps = Vec::from_iter(
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
                selected_fn: |pa, item| {},
            },
            PushSpecs {
                side: Side::Left,
                width: Some(width),
                ..Default::default()
            },
        );

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
    pub fn select_current(pa: &mut Pass, on_new_window: bool) {
        let Some(picker) = context::handle_of::<Picker>(pa) else {
            context::warn!("Tried selecting the [a]Picker[] entry, but there was no Picker open");
            return;
        };

        _ = picker.close(pa);
        let pv = picker.write_then(pa, |pkr| &pkr.preview);
        match pv.modes.remove(pv.current) {
            Some(PreviewMode::BufferMirror(buffer, range)) => {
                mode::reset_to(pa, &buffer);
                buffer.selections_mut(pa).remove_extras();
                buffer.edit_main(pa, |mut s| s.move_to(range.clone()));
            }
            Some(PreviewMode::BufferPreview(path, mut text, range)) => {
                if let Some(buffer) = context::buffer_from_path(pa, &path) {
                    buffer.text_mut(pa).replace_range(.., text.to_string());
                    mode::reset_to(pa, &buffer);
                    buffer.selections_mut(pa).remove_extras();
                    buffer.edit_main(pa, |mut s| s.move_to(range.clone()));
                } else {
                    duat_core::try_or_log_err! {
                        let file = std::fs::OpenOptions::new().write(true).create(true).open(&path)?;
                        text.save_on(file)?;
                    };

                    let call = if on_new_window { "open" } else { "edit" };
                    if cmd::call_notify(pa, format!("{call} {}", path.to_string_lossy())).is_ok() {
                        let buffer = context::current_buffer(pa);
                        buffer.selections_mut(pa).remove_extras();
                        buffer.edit_main(pa, |mut s| s.move_to(range.clone()));
                    }
                }
            }
            Some(PreviewMode::Text(..)) | None => {}
        }
    }

    pub fn focus_preview(pa: &mut Pass) {}

    pub fn unfocus_preview(pa: &mut Pass) {}

    /// Wether there is a `Picker` open right now.
    pub fn is_open(pa: &Pass) -> bool {
        context::handle_of::<Picker>(pa).is_some()
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
        PrintOpts::default_for_input()
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
        } else {
            let content = match std::fs::read_to_string(&place.path) {
                Ok(content) => content,
                Err(err) => panic!("{err}"),
            };

            PreviewMode::BufferPreview(
                PathBuf::from(&place.path),
                Text::from(content),
                place.range.clone(),
            )
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
}

impl Widget for Preview {
    fn text<'p>(widget: &'p RwData<Self>, pa: &'p Pass) -> &'p Text {
        let pv = widget.read(pa);
        match pv.modes[pv.current].as_ref() {
            Some(PreviewMode::BufferMirror(buffer, _)) => buffer.text(pa),
            Some(PreviewMode::BufferPreview(_, text, _)) => text,
            Some(PreviewMode::Text(text, _)) => text,
            None => &pv.empty,
        }
    }

    fn text_mut<'p>(widget: &'p RwData<Self>, pa: &'p mut Pass) -> TextMut<'p> {
        let pv = widget.write(pa);
        match &mut pv.modes[pv.current].as_mut() {
            Some(PreviewMode::BufferPreview(_, text, _) | PreviewMode::Text(text, _)) => {
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
    BufferMirror(Handle, Range<usize>),
    BufferPreview(PathBuf, Text, Range<usize>),
    Text(Text, Range<usize>),
}

impl PreviewMode {
    /// The [`TwoPoints`] which should be centered.
    pub fn range(&self) -> Range<usize> {
        match self {
            Self::BufferMirror(_, points) => points.clone(),
            Self::BufferPreview(.., points) => points.clone(),
            Self::Text(_, points) => points.clone(),
        }
    }
}

type Entry = Box<dyn Any + Send>;
