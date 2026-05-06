//! A [`CompletionsProvider`] for commands
//!
//! This provider will show all available commands, as well as
//! information about the commands.
use duat_core::{
    cmd::{CmdDoc, Description},
    data::Pass,
    text::{Point, RegexHaystack, Spacer, Text, txt},
    ui::Orientation,
};

use crate::widgets::{CompletionsProvider, completions::string_cmp};

/// A [`CompletionsProvider`] for available commands
pub struct CommandsCompletions(Vec<Description>);

impl CommandsCompletions {
    /// Returns a new [`CompletionsProvider`] for commands
    pub fn new(pa: &mut Pass) -> Self {
        Self(duat_core::cmd::cmd_list(pa))
    }
}

impl CompletionsProvider for CommandsCompletions {
    type Entry = CmdDoc;

    const ALLOW_WITH_MULTIPLE_SELECTIONS: bool = false;

    fn matches(&mut self, _: &Text, _: Point, prefix: &str) -> Vec<Self::Entry> {
        let mut matches = Vec::from_iter(self.0.iter().filter_map(|desc| {
            if let Description::Command(desc) = desc
                && string_cmp(prefix, &desc.caller).is_some()
            {
                Some(desc.clone())
            } else {
                None
            }
        }));

        matches.sort_by(|lhs, rhs| {
            (string_cmp(prefix, &lhs.caller), &lhs.caller)
                .cmp(&(string_cmp(prefix, &rhs.caller), &rhs.caller))
        });

        matches
    }

    fn get_start(&self, text: &Text, cursor: Point) -> Option<usize> {
        Some(text.search(r"[^\s]*").range(..cursor).next_back()?.start)
    }

    fn default_info_on(doc: &Self::Entry) -> Option<(Text, Orientation)> {
        let mut builder = Text::builder();

        let short = doc.short.as_ref()?;

        if !doc.params.is_empty() {
            builder.push(txt!("{}\n\nArguments:", short));

            for param in doc.params.iter() {
                let Some(short) = param.short.as_ref() else {
                    continue;
                };

                builder.push(txt!("\n\t- {param.arg_name}: {short}"));
            }
        } else {
            builder.push(Text::clone(short));
        }

        if let Some(long) = doc.long.as_ref() {
            builder.push(txt!("\n\n{long}"));
        }

        Some((builder.build(), Orientation::HorTopRight))
    }

    fn default_fmt(entry: &Self::Entry) -> Text {
        txt!("[cmd.Completions]{entry.caller}{Spacer}")
    }

    fn word(entry: &Self::Entry) -> &str {
        &entry.caller
    }
}
