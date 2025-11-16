use duat_core::{
    context::{self, Handle},
    data::Pass,
    hook::{self, FocusChanged, KeyTyped},
    mode::{self, MouseEvent, MouseEventKind},
    text::{Spacer, Text, txt},
    ui::{DynSpawnSpecs, Widget},
};

/// A [`Widget`] to display what [keys] will do
///
/// [keys]: mode::KeyEvent
pub struct WhichKey(Text);

impl WhichKey {
    /// Open the `WhichKey` widget
    pub fn open(pa: &mut Pass, mut specs: DynSpawnSpecs) {
        let mut builder = Text::builder();

        for desc in mode::current_seq_descriptions(pa) {
            builder.push(txt!("{}{Spacer}", desc.keys.into_text()));
            if let Some(text) = desc.text {
                builder.push(txt!("{text}\n"));
            } else {
                builder.push('\n');
            }
        }

        let wk = WhichKey(builder.build_no_double_nl());

        let handles: Vec<_> = context::windows().handles_of::<WhichKey>(pa).collect();
        for handle in handles {
            let _ = handle.close(pa);
        }

        if let Some(height) = specs.height.as_mut() {
            *height = wk.text().len().line().min(*height as usize) as f32;
        }

        let handle = context::current_buffer(pa)
            .clone()
            .spawn_widget(pa, wk, specs)
            .unwrap();

        let (wk, area) = handle.write_with_area(pa);
        if let Ok(width) = area.width_of_text(wk.get_print_opts(), wk.text()) {
            area.set_width(width + 3.0).unwrap();
        }

        hook::add::<KeyTyped>({
            let handle = handle.clone();
            move |pa, _| Ok(_ = handle.close(pa))
        })
        .once();
        hook::add::<FocusChanged>(move |pa, _| Ok(_ = handle.close(pa))).once();
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
        match event.kind {
            MouseEventKind::ScrollDown | MouseEventKind::ScrollUp => {
                let (wk, area) = handle.write_with_area(pa);
                let scroll = if let MouseEventKind::ScrollDown = event.kind {
                    3
                } else {
                    -3
                };
                area.scroll_ver(&wk.0, scroll, wk.get_print_opts());
            }
            _ => {}
        }
    }
}
