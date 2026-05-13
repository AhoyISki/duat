//! A [`Widget`] to show a list of places (mostly) to jump to.
//!
//! These will be listed in a spawned widget, and may or may not point
//! to real places in files. It can also be useed for other things,
//! like listing code actions and their respective results if they are
//! performed.
use std::{any::Any, ops::Range};

use duat_core::{
    context,
    data::Pass,
    text::{Point, Text},
    ui::{Coord, StaticSpawnSpecs, Widget},
};
use duat_term::Frame;

/// A [`Widget`] that is mostly used to jump to locations in
/// [`Buffer`]s.
///
/// [`Buffer`]: duat_core::buffer::Buffer
pub struct Picker {
    maps: Vec<(Range<usize>, Box<dyn Any + Send>)>,
    text: Text,
    current: usize,
}

impl Picker {
    /// Spawn a new picker for jumping to [`Buffer`] locations.
    ///
    /// [`Buffer`]: duat_core::buffer::Buffer
    pub fn spawn_jumps(
        pa: &mut Pass,
        list: impl IntoIterator<Item = (Text, BufferPlace<impl AsRef<str>>)>,
    ) {
        let window = context::current_window(pa).clone();
        let w_width = window.width();
        let w_height = window.height();
        let width = 2.0 * w_width / 3.0;
        let height = 2.0 * w_height / 3.0;

        let specs = StaticSpawnSpecs {
            top_left: Coord {
                x: ((w_width - width) / 2.0),
                y: ((w_height - height) / 2.0),
            },
            size: Coord { x: width, y: height },
            fractional_repositioning: Some(true),
            ..Default::default()
        };

        let mut builder = Text::builder();
        let maps = Vec::from_iter(list.into_iter().map(|(text, location)| {
            let len = builder.len();
            builder.push_ref(&text);
            builder.push("\n");

            (
                len..len + text.len(),
                Box::new(location.to_string_location()) as Box<dyn Any + Send + 'static>,
            )
        }));

        let picker = Picker {
            maps,
            text: builder.build(),
            current: 0,
        };

        let handle = window.spawn(pa, picker, specs);

        if let Some(area) = handle.area().write_as::<duat_term::Area>(pa) {
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

    /// Scroll the `Picker`'s entries.
    pub fn scroll(pa: &mut Pass, by: i32) {}
}

impl Widget for Picker {
    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> duat_core::text::TextMut<'_> {
        self.text.as_mut()
    }
}

/// A location to jump to in a [`Picker`].
#[derive(Clone, Copy, Debug)]
pub struct BufferPlace<S: AsRef<str>> {
    /// The path this `BufferPlace` points to.
    pub path: S,
    /// Where in the [`Buffer`] it points to.
    pub point: Point,
}

impl<S: AsRef<str>> BufferPlace<S> {
    /// Creates a `BufferPlace<String>` from this `BufferPlace`.
    pub fn to_string_location(&self) -> BufferPlace<String> {
        BufferPlace {
            path: self.path.as_ref().to_string(),
            point: self.point,
        }
    }
}

