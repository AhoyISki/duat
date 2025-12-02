//! A `Widget` to show available bindings and remaps
//!
//! This widget is automatically displayed when deemed necessary.
//! Normally, this is done on some modes, and when typing remapped key
//! sequences, be them maps or aliases.
use std::sync::Once;

use duat_core::{
    context::{self, Handle},
    data::Pass,
    form,
    hook::{self, FocusChanged, KeyTyped},
    mode::{self, Description, MouseEvent, MouseEventKind},
    text::{Text, txt},
    ui::{DynSpawnSpecs, PushSpecs, Side, Widget},
};
use duat_term::{Frame, FrameStyle};

/// A [`Widget`] to display what [keys] will do
///
/// [keys]: mode::KeyEvent
pub struct WhichKey(Text, Option<Handle<WhichKeyDescriptions>>);

impl WhichKey {
    /// Open the `WhichKey` widget
    ///
    /// You can optionally pass an
    #[allow(clippy::type_complexity)] // ??? where?
    pub fn open(
        pa: &mut Pass,
        mut fmt: Option<Box<dyn FnMut(Description) -> Option<(Text, Text)>>>,
        mut specs: DynSpawnSpecs,
    ) {
        static ONCE: Once = Once::new();
        ONCE.call_once(|| {
            form::set("default.WhichKeyDescriptions", "default.WhichKey");
        });

        let mut keys_builder = Text::builder();
        let mut descs_builder = Text::builder();

        let (title, descs) = mode::current_seq_descriptions(pa);
        let title = title.cloned();
        for desc in descs {
            if let Some(fmt) = fmt.as_mut() {
                if let Some((keys, desc)) = fmt(desc) {
                    keys_builder.push(keys);
                    descs_builder.push(desc);
                }
            } else if let Some(text) = desc.text
                && !text.is_empty()
            {
                keys_builder.push(txt!("{}[colon.WhichKey]:", desc.keys.into_text()));
                descs_builder.push(txt!("{text}"));
            } else {
                continue;
            }

            keys_builder.push('\n');
            descs_builder.push('\n');
        }

        let keys = WhichKey(keys_builder.build_no_double_nl(), None);
        let mut descs = WhichKeyDescriptions(descs_builder.build_no_double_nl(), None);

        let handles: Vec<_> = context::windows()
            .handles(pa)
            .filter(|handle| {
                handle.widget().is::<WhichKey>() || handle.widget().is::<WhichKeyDescriptions>()
            })
            .cloned()
            .collect();
        for handle in handles {
            let _ = handle.close(pa);
        }

        if let Some(height) = specs.height.as_mut() {
            *height = keys.text().len().line().min(*height as usize) as f32;
        }

        let keys_handle = context::current_buffer(pa)
            .clone()
            .spawn_widget(pa, keys, specs)
            .unwrap();
        descs.1 = Some(keys_handle.clone());

        let title = title.unwrap_or_else(|| txt!("{}", crate::state::mode_name().call(pa)));
        if let Some(area) = keys_handle.area().write_as::<duat_term::Area>(pa) {
            use duat_core::text::AlignCenter;

            let mut frame = Frame {
                left: true,
                right: true,
                above: true,
                below: true,
                style: Some(FrameStyle::Rounded),
                ..Frame::default()
            };
            frame.set_text(Side::Above, move |_| {
                txt!("{AlignCenter}[terminal.frame]┤[]{title}[terminal.frame]├")
            });
            area.set_frame(frame);
        }

        let (keys, area) = keys_handle.write_with_area(pa);
        if let Ok(width) = area.width_of_text(keys.get_print_opts(), keys.text()) {
            area.set_width(width + 1.0).unwrap();
        }

        let descs_handle = keys_handle.push_inner_widget(pa, descs, PushSpecs {
            side: Side::Right,
            ..Default::default()
        });
        keys_handle.write(pa).1 = Some(descs_handle.clone());

        let (descs, area) = descs_handle.write_with_area(pa);
        if let Ok(width) = area.width_of_text(descs.get_print_opts(), descs.text()) {
            area.set_width(width).unwrap();
        }

        hook::add::<KeyTyped>({
            let keys_handle = keys_handle.clone();
            let descs_handle = descs_handle.clone();
            move |pa, _| {
                _ = keys_handle.close(pa);
                Ok(_ = descs_handle.close(pa))
            }
        })
        .once();
        hook::add::<FocusChanged>(move |pa, _| {
            _ = keys_handle.close(pa);
            Ok(_ = descs_handle.close(pa))
        })
        .once();
    }
}

impl Widget for WhichKey {
    fn update(_: &mut Pass, _: &Handle<Self>) {}

    fn needs_update(&self, _: &Pass) -> bool {
        false
    }

    fn text(&self) -> &Text {
        &self.0
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.0
    }

    fn on_mouse_event(pa: &mut Pass, handle: &Handle<Self>, event: MouseEvent) {
        use MouseEventKind::{ScrollDown, ScrollUp};
        match event.kind {
            ScrollDown | ScrollUp => {
                let (keys, area) = handle.write_with_area(pa);
                let scroll = if let ScrollDown = event.kind { 3 } else { -3 };
                area.scroll_ver(&keys.0, scroll, keys.get_print_opts());

                let handle = keys.1.clone().unwrap();
                let (descs, area) = handle.write_with_area(pa);
                area.scroll_ver(&descs.0, scroll, descs.get_print_opts());
            }
            _ => {}
        }
    }
}

struct WhichKeyDescriptions(Text, Option<Handle<WhichKey>>);

impl Widget for WhichKeyDescriptions {
    fn update(_: &mut Pass, _: &Handle<Self>) {}

    fn needs_update(&self, _: &Pass) -> bool {
        false
    }

    fn text(&self) -> &Text {
        &self.0
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.0
    }

    fn on_mouse_event(pa: &mut Pass, handle: &Handle<Self>, event: MouseEvent) {
        match event.kind {
            MouseEventKind::ScrollDown | MouseEventKind::ScrollUp => {
                let (descs, area) = handle.write_with_area(pa);
                let scroll = if let MouseEventKind::ScrollDown = event.kind {
                    3
                } else {
                    -3
                };
                area.scroll_ver(&descs.0, scroll, descs.get_print_opts());

                let handle = descs.1.clone().unwrap();
                let (keys, area) = handle.write_with_area(pa);
                area.scroll_ver(&keys.0, scroll, keys.get_print_opts());
            }
            _ => {}
        }
    }
}
