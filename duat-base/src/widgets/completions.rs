use std::sync::{LazyLock, Once};

use duat_core::{
    context::{self, Handle},
    data::Pass,
    hook::{self, FocusChanged},
    opts::PrintOpts,
    text::{SpawnTag, Tagger, Text, txt},
    ui::{Orientation, SpawnSpecs, Widget},
};

static TAGGER: LazyLock<Tagger> = Tagger::new_static();

pub struct Completions {
    text: Text,
    list: Vec<String>,
    active: usize,
}

impl Completions {
    /// Spawn the `Completions` list
    pub fn open(pa: &mut Pass) {
        static ONCE: Once = Once::new();
        ONCE.call_once(|| {
            hook::add::<FocusChanged>(|pa, (prev, _)| {
                prev.text_mut(pa).remove_tags(*TAGGER, ..);
                Ok(())
            })
        });

        let handle = context::current_widget(pa).clone();
        handle.text_mut(pa).remove_tags(*TAGGER, ..);

        let Some(main) = handle.text(pa).selections().get_main().cloned() else {
            context::warn!("No Selection to center completions on");
            return;
        };

        let text = handle.text_mut(pa);

        let completions = Self {
            text: Text::new(),
            list: vec![
                "word 1".to_string(),
                "word 2".to_string(),
                "word 3 lmao".to_string(),
            ],
            active: 0,
        };

        let specs = SpawnSpecs {
            orientation: Orientation::VerLeftBelow,
            height: Some(20.0),
            width: Some(50.0),
            ..
        };

        text.insert_tag(*TAGGER, main.caret(), SpawnTag::new(completions, specs));
    }

    /// Closes the `Completions` list
    pub fn close(pa: &mut Pass) {
        let handle = context::current_widget(pa).clone();
        handle.text_mut(pa).remove_tags(*TAGGER, ..);
    }
}

impl Widget for Completions {
    fn update(pa: &mut Pass, handle: &Handle<Self>) {
        let comp = handle.write(pa);
        let mut builder = Text::builder();
        for (i, line) in comp.list.iter().enumerate() {
            if i == comp.active {
                builder.push(txt!("[selected.Completions]{line}\n"));
            } else {
                builder.push(line);
                builder.push('\n');
            }
        }
    }

    fn needs_update(&self, _: &Pass) -> bool {
        false
    }

    fn text(&self) -> &Text {
        &self.text
    }

    fn text_mut(&mut self) -> &mut Text {
        &mut self.text
    }

    fn get_print_opts(&self) -> PrintOpts {
        let mut opts = PrintOpts::new();
        opts.scrolloff.y = 0;
        opts
    }
}
