use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use duat_base::{
    hooks::{CompletionFocused, CompletionSelected},
    modes::{Prompt, PromptMode},
    widgets::{Completions, CompletionsProvider},
};
use duat_core::{
    Ns, context,
    data::Pass,
    form, hook, mode,
    text::{Point, RegexHaystack, Spacer, Text},
    txt,
    ui::RwArea,
};
use lsp_types::{
    CodeAction, CodeActionOrCommand, Command, ExecuteCommandParams, RenameParams,
    TextDocumentPositionParams, WorkDoneProgressParams,
    request::{CodeActionResolveRequest, ExecuteCommand, Rename},
};

use crate::{Encoding, handle_workspace_edit, server::Server};

/// Add the hooks necessary for the custom modes to work.
pub fn setup_hooks() {
    hook::add::<CompletionSelected>(|pa, entry| {
        let Some(entry) = entry.get_for::<Actions>() else {
            return;
        };

        match &entry.a_or_c {
            ActionOrCommand::Action(arc_action, _) => {
                let encoding = entry.encoding;
                let server = entry.server.clone();
                let do_action = move |pa: &mut Pass, action: CodeAction| {
                    if let Some(edit) = &action.edit {
                        duat_core::try_or_log_err! {
                            handle_workspace_edit(pa, edit.clone(), encoding)?;
                        }
                    }
                    if let Some(command) = &action.command {
                        server.send_request_with_id::<ExecuteCommand>(
                            jsonrpc_lite::Id::Str(command.title.clone()),
                            ExecuteCommandParams {
                                command: command.title.clone(),
                                arguments: Vec::new(),
                                work_done_progress_params: WorkDoneProgressParams::default(),
                            },
                            |_, _| {},
                        )
                    }
                };

                let action = arc_action.lock().unwrap();
                if action.1 {
                    let code_action = action.0.clone();
                    entry
                        .server
                        .send_request_with_id::<CodeActionResolveRequest>(
                            jsonrpc_lite::Id::Str(action.0.title.clone()),
                            code_action,
                            move |pa, result| {
                                do_action(pa, result);
                            },
                        );
                } else {
                    do_action(pa, action.0.clone());
                }
            }
            ActionOrCommand::Command(command) => {
                entry.server.send_request_with_id::<ExecuteCommand>(
                    jsonrpc_lite::Id::Str(command.title.clone()),
                    ExecuteCommandParams {
                        command: command.title.clone(),
                        arguments: Vec::new(),
                        work_done_progress_params: WorkDoneProgressParams::default(),
                    },
                    |_, _| {},
                )
            }
        }
    });

    hook::add::<CompletionFocused>(|_, entry| {
        if let Some(entry) = entry.get_for::<Actions>()
            && let ActionOrCommand::Action(action, _) = &entry.a_or_c
            && !action.lock().unwrap().1
        {
            let action = action.clone();
            let code_action = action.lock().unwrap().0.clone();
            entry
                .server
                .send_request_with_id::<CodeActionResolveRequest>(
                    jsonrpc_lite::Id::Str(title(&entry.a_or_c).to_string()),
                    code_action,
                    move |_, result| {
                        let mut action = action.lock().unwrap();
                        action.0 = result;
                        action.1 = true;
                    },
                );
        }
    });
}

/// A mode for renaming symbols.
pub struct RenameSymbol {
    server: Server,
    doc_pos: TextDocumentPositionParams,
    chars_regex: &'static str,
    encoding: Encoding,
}

impl RenameSymbol {
    pub fn new(
        server: Server,
        doc_pos: TextDocumentPositionParams,
        chars_regex: &'static str,
        encoding: Encoding,
    ) -> Self {
        Self { server, doc_pos, chars_regex, encoding }
    }
}

impl PromptMode for RenameSymbol {
    fn update(&mut self, _: &mut Pass, mut text: Text, _: &RwArea) -> Text {
        text.remove_tags(Ns::basic(), ..);

        if text.matches_pat(self.chars_regex).unwrap() {
            text.insert_tag(Ns::basic(), .., form::id_of!("lsp.rename.info").to_tag(0));
        } else {
            text.insert_tag(Ns::basic(), .., form::id_of!("lsp.rename.error").to_tag(0));
        }

        text
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]rename-symbol")
    }

    fn before_exit(&mut self, _: &mut Pass, text: Text, _: &RwArea) {
        let mut new_name = text.to_string();
        new_name.truncate(new_name.trim_end().len());

        if new_name.is_empty() {
            context::error!("Name can't be empty");
        } else {
            let encoding = self.encoding;
            self.server.send_request::<Rename>(
                RenameParams {
                    new_name,
                    text_document_position: self.doc_pos.clone(),
                    work_done_progress_params: WorkDoneProgressParams::default(),
                },
                move |pa, result| {
                    if let Some(result) = result {
                        duat_core::try_or_log_err! { handle_workspace_edit(pa, result, encoding)? };
                    }
                },
            )
        }
    }
}

pub struct DoCodeAction;

impl DoCodeAction {
    /// If not in this mode yet, set it, otherwise, add the actions.
    pub fn set_or_add(
        pa: &mut Pass,
        server: Server,
        can_resolve: bool,
        encoding: Encoding,
        actions: Vec<CodeActionOrCommand>,
    ) {
        let actions = actions
            .into_iter()
            .map(|ca_or_c| match ca_or_c {
                CodeActionOrCommand::CodeAction(action) => {
                    let title = action.title.clone();
                    ActionOrCommand::Action(Arc::new(Mutex::new((action, !can_resolve))), title)
                }
                CodeActionOrCommand::Command(command) => ActionOrCommand::Command(command),
            })
            .collect();

        if Completions::has_provider::<Actions>(pa) {
            Completions::update_provider::<Actions>(pa, |provider, _| {
                provider.0.insert((server, encoding), actions);
            });
        } else {
            mode::set(pa, Prompt::new(DoCodeAction));
            Completions::builder()
                .with_provider(Actions(HashMap::from_iter([((server, encoding), actions)])))
                .open(pa);
        }
    }
}

impl PromptMode for DoCodeAction {
    fn update(&mut self, _: &mut Pass, text: Text, _: &RwArea) -> Text {
        text
    }

    fn prompt(&self) -> Text {
        txt!("[prompt]execute code action")
    }
}

struct Actions(HashMap<(Server, Encoding), Vec<ActionOrCommand>>);

impl CompletionsProvider for Actions {
    const ALLOW_WITH_MULTIPLE_SELECTIONS: bool = false;

    type Entry = Action;

    fn matches(&mut self, _: &Text, _: Point, prefix: &str) -> Vec<Self::Entry> {
        let (prefix, case_insensitive) = if prefix.chars().any(|c| c.is_uppercase()) {
            (prefix.to_string(), false)
        } else {
            (prefix.to_uppercase(), true)
        };

        self.0
            .iter()
            .flat_map(|((server, encoding), actions)| {
                let mut matches = Vec::from_iter(
                    actions
                        .iter()
                        .filter(|a_or_c| {
                            if case_insensitive {
                                string_cmp(&prefix, &title(a_or_c).to_uppercase()).is_some()
                            } else {
                                string_cmp(&prefix, title(a_or_c)).is_some()
                            }
                        })
                        .map(|a_or_c| Action {
                            a_or_c: a_or_c.clone(),
                            server: server.clone(),
                            encoding: *encoding,
                        }),
                );

                matches.sort_by_key(|entry| {
                    if case_insensitive {
                        let cmp = title(&entry.a_or_c).to_uppercase();
                        (string_cmp(&prefix, &cmp), cmp.len())
                    } else {
                        let title = title(&entry.a_or_c);
                        (string_cmp(&prefix, title), title.len())
                    }
                });

                matches
            })
            .collect()
    }

    fn get_start(&self, _: &Text, _: Point) -> Option<usize> {
        Some(0)
    }

    fn default_fmt(entry: &Self::Entry) -> Text {
        let form = if matches!(entry.a_or_c, ActionOrCommand::Action(..)) {
            form::id_of!("completions.lsp.code_action")
        } else {
            form::id_of!("completions.lsp.command")
        };

        txt!(
            "{form}{}[]{Spacer} [completion.lsp.detail.source]{}",
            title(&entry.a_or_c),
            entry.server.name()
        )
    }

    fn word(entry: &Self::Entry) -> &str {
        title(&entry.a_or_c)
    }
}

#[derive(Clone)]
struct Action {
    a_or_c: ActionOrCommand,
    server: Server,
    encoding: Encoding,
}

#[derive(Clone)]
enum ActionOrCommand {
    Action(Arc<Mutex<(CodeAction, bool)>>, String),
    Command(Command),
}

fn string_cmp(target: &str, entry: &str) -> Option<usize> {
    let mut diff = 0;
    let mut eq_i = 0;
    let mut cmp_chars = entry.chars().enumerate();

    for char in target.chars() {
        let (i, _) = cmp_chars.find(|&(_, other)| other == char)?;
        diff += i - eq_i;
        eq_i = i + 1;
    }

    Some(diff)
}

fn title(ca_or_c: &ActionOrCommand) -> &str {
    match ca_or_c {
        ActionOrCommand::Action(_, title) => &title,
        ActionOrCommand::Command(command) => &command.title,
    }
}
