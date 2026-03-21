//! A `Widget` to show available bindings and remaps
//!
//! This widget is automatically displayed when deemed necessary.
//! Normally, this is done on some modes, and when typing remapped key
//! sequences, be them maps or aliases.
use std::sync::Once;

use duat_core::{
    context::{self, Handle},
    data::Pass,
    form::{self, Form},
    hook::{self, FocusChanged, KeyTyped, OnMouseEvent},
    mode::{self, Description, MouseEventKind},
    text::{Text, TextMut, txt},
    ui::{DynSpawnSpecs, PushSpecs, Side, Widget},
};
use duat_term::Frame;

/// Add the hooks for the [`WhichKey`].
pub fn add_whichkey_hooks() {
    hook::add::<OnMouseEvent<WhichKey>>(move |pa, (whichkey, event)| {
        use MouseEventKind::{ScrollDown, ScrollUp};
        match event.kind {
            ScrollDown | ScrollUp => {
                let (wk, area) = whichkey.write_with_area(pa);
                let scroll = if let ScrollDown = event.kind { 3 } else { -3 };
                area.scroll_ver(&wk.0, scroll, wk.print_opts());

                let whichkey_desc = wk.1.clone().unwrap();
                let (wkd, area) = whichkey_desc.write_with_area(pa);
                area.scroll_ver(&wkd.0, scroll, wkd.print_opts());
            }
            _ => {}
        }
    });

    hook::add::<OnMouseEvent<WhichKeyDescriptions>>(move |pa, (whichkey_desc, event)| {
        use MouseEventKind::{ScrollDown, ScrollUp};
        match event.kind {
            ScrollDown | ScrollUp => {
                let (wkd, area) = whichkey_desc.write_with_area(pa);
                let scroll = if let MouseEventKind::ScrollDown = event.kind {
                    3
                } else {
                    -3
                };
                area.scroll_ver(&wkd.0, scroll, wkd.print_opts());

                let whichkey = wkd.1.clone().unwrap();
                let (wk, area) = whichkey.write_with_area(pa);
                area.scroll_ver(&wk.0, scroll, wk.print_opts());
            }
            _ => {}
        }
    });
}

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
            form::set(
                "default.WhichKeyDescriptions",
                Form::mimic("default.WhichKey"),
            );
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
            .collect();
        for handle in handles {
            let _ = handle.close(pa);
        }

        if let Some(height) = specs.height.as_mut() {
            *height = keys.text().end_point().line().min(*height as usize) as f32;
        }

        let keys_handle = context::current_buffer(pa)
            .spawn_widget(pa, keys, specs)
            .unwrap();
        descs.1 = Some(keys_handle.clone());

        let title = title.unwrap_or_else(|| txt!("{}", crate::state::mode_name().call(pa)));
        if let Some(area) = keys_handle.area().write_as::<duat_term::Area>(pa) {
            use duat_core::text::Spacer;

            let mut frame = Frame {
                left: true,
                right: true,
                above: true,
                below: true,
                ..Frame::default()
            };
            frame.set_text(Side::Above, move |_| {
                txt!("{Spacer}[terminal.frame]┤[]{title}[terminal.frame]├{Spacer}")
            });
            area.set_frame(frame);
        }

        let (keys, area) = keys_handle.write_with_area(pa);
        if let Ok(size) = area.size_of_text(keys.print_opts(), keys.text()) {
            area.set_width(size.x + 1.0).unwrap();
        }

        let descs_handle = keys_handle.push_inner_widget(pa, descs, PushSpecs {
            side: Side::Right,
            ..Default::default()
        });
        keys_handle.write(pa).1 = Some(descs_handle.clone());

        let (descs, area) = descs_handle.write_with_area(pa);
        if let Ok(size) = area.size_of_text(descs.print_opts(), descs.text()) {
            area.set_width(size.x).unwrap();
        }

        hook::add_once::<KeyTyped>({
            let keys_handle = keys_handle.clone();
            let descs_handle = descs_handle.clone();
            move |pa, _| {
                _ = keys_handle.close(pa);
                _ = descs_handle.close(pa);
            }
        });

        hook::add_once::<FocusChanged>(move |pa, _| {
            _ = keys_handle.close(pa);
            _ = descs_handle.close(pa);
        });
    }
}

impl Widget for WhichKey {
    fn text(&self) -> &Text {
        &self.0
    }

    fn text_mut(&mut self) -> TextMut<'_> {
        self.0.as_mut()
    }
}

struct WhichKeyDescriptions(Text, Option<Handle<WhichKey>>);

impl Widget for WhichKeyDescriptions {
    fn text(&self) -> &Text {
        &self.0
    }

    fn text_mut(&mut self) -> TextMut<'_> {
        self.0.as_mut()
    }
}
