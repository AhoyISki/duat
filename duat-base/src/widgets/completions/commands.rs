//! A [`CompletionsProvider`] for commands
//!
//! This provider will show all available commands 
use duat_core::{
    cmd::{CmdDescription, Description},
    data::Pass,
    text::{Point, Spacer, Text, txt},
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
    type Info = CmdDescription;

    fn default_fmt(entry: &str, _: &Self::Info) -> Text {
        txt!("[cmd.Completions]{entry}{Spacer}")
    }

    fn get_completions(
        &mut self,
        _: &Text,
        _: Point,
        prefix: &str,
        _: &str,
    ) -> CompletionsList<Self> {
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

        CompletionsList { entries, kind: CompletionsKind::UnfinishedFiltered }
    }

    fn word_regex(&self) -> String {
        r"[^\s]*".to_string()
    }

    fn has_changed(&self) -> bool {
        false
    }

    fn default_info_on(&self, (_, info): (&str, &Self::Info)) -> Option<Text> {
        info.doc.as_ref().map(|doc| {
            doc.long
                .as_ref()
                .map(|long| Text::clone(long))
                .unwrap_or(Text::clone(&doc.short))
        })
    }
}
