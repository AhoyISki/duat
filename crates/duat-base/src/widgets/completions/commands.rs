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

use crate::widgets::{
    CompletionsKind, CompletionsList, CompletionsProvider, completions::string_cmp,
};

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

    fn completions(&mut self, _: &Text, _: Point, prefix: &str, _: bool) -> CompletionsList<Self> {
        let mut entries: Vec<_> = self
            .0
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
            .map(|desc| (desc.caller.to_string(), desc.clone()))
            .collect();

        entries.sort_by(|(lhs, _), (rhs, _)| {
            (string_cmp(prefix, lhs), lhs).cmp(&(string_cmp(prefix, rhs), rhs))
        });

        CompletionsList {
            entries,
            kind: CompletionsKind::UnfinishedFiltered,
        }
    }

    fn get_start(&self, text: &Text, caret: Point) -> Option<usize> {
        Some(text.search(r"[^\s]*").range(..caret).next_back()?.start)
    }

    fn has_changed(&self) -> bool {
        false
    }

    fn default_info_on(&self, (_, doc): (&str, &Self::Info)) -> Option<(Text, Orientation)> {
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
