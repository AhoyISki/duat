//! A [`CompletionsProvider`] for commands
//!
//! This provider will show all available commands, as well as
//! information about the commands.
use std::sync::Arc;

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
    type Info = CmdDoc;

    fn default_fmt(entry: &str, _: &Self::Info) -> Text {
        txt!("[cmd.Completions]{entry}{Spacer}")
    }

    fn matches(&mut self, _: &Text, _: Point, prefix: &str) -> Vec<(Arc<str>, Self::Info)> {
        let mut matches = Vec::from_iter(
            self.0
                .iter()
                .filter_map(|desc| {
                    if let Description::Command(desc) = desc
                        && string_cmp(prefix, &desc.caller).is_some()
                    {
                        Some(desc)
                    } else {
                        None
                    }
                })
                .map(|desc| (desc.caller.clone(), desc.clone())),
        );

        matches.sort_by(|(lhs, _), (rhs, _)| {
            (string_cmp(prefix, lhs), lhs).cmp(&(string_cmp(prefix, rhs), rhs))
        });

        matches
    }

    fn get_start(&self, text: &Text, caret: Point) -> Option<usize> {
        Some(text.search(r"[^\s]*").range(..caret).next_back()?.start)
    }

    fn default_info_on(&self, _: &str, doc: &Self::Info) -> Option<(Text, Orientation)> {
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
}
