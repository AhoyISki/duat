use std::sync::Arc;

use duat_base::{
    BaseBuffer,
    hooks::{CompletionFinished, CompletionSelected},
    widgets::Completions,
};
use duat_core::{
    Ns, context,
    data::Pass,
    hook,
    text::{Point, RegexHaystack, Spacer, Text},
    txt,
    ui::Orientation,
};
use jsonrpc_lite::Id;
use lsp_types::{
    CompletionContext, CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse,
    CompletionTextEdit, CompletionTriggerKind, Documentation, InsertTextFormat, InsertTextMode,
    PartialResultParams, TextDocumentIdentifier, TextDocumentPositionParams, Uri,
    WorkDoneProgressParams,
    request::{Completion, ResolveCompletionItem},
};

use crate::{Encoding, parser::PARSERS, path_to_uri, server::Server};

pub fn setup_hooks() {
    
    hook::add::<CompletionSelected>(|_, entry| {
        let Some(lsp_entry) = entry.get_for::<LspCompletions>() else {
            return;
        };

        if lsp_entry.documentation.is_some() {
            return;
        }

        let server_ns = lsp_entry.server_ns;
        let idx = entry.index;
        let list_idx = lsp_entry.list_idx;

        let old_item = lsp_entry.item.clone();

        crate::server::on_ns(lsp_entry.server_ns, |server| {
            server.send_request_with_id::<ResolveCompletionItem>(
                Id::Str(format!("{}{}", lsp_entry.label, entry.index)),
                old_item.as_ref().clone(),
                move |pa, new_item| {
                    Completions::update_provider(pa, move |comp: &mut LspCompletions, list| {
                        let Some((.., Some(comp))) =
                            comp.lists.iter_mut().find(|(s, ..)| s.ns() == server_ns)
                        else {
                            return;
                        };

                        let idx = list
                            .get(idx)
                            .filter(|entry| entry.item == old_item)
                            .and(Some(idx))
                            .or_else(|| list.iter().position(|entry| entry.item == old_item));

                        if let Some(idx) = idx {
                            list[idx] = Entry {
                                list_idx,
                                server_ns,
                                item: Arc::new(new_item.clone()),
                            };
                        }

                        let list_idx = comp
                            .entries
                            .get(list_idx)
                            .filter(|item| **item == old_item)
                            .and(Some(list_idx))
                            .or_else(|| comp.entries.iter().position(|item| *item == old_item));

                        if let Some(list_idx) = list_idx {
                            comp.entries[list_idx] = Arc::new(new_item.clone());
                        }
                    });
                },
            );
        });
    });

    hook::add::<CompletionFinished>(|pa, entry| {
        let buffer = context::current_buffer(pa);
        let popts = buffer.opts(pa);

        let Some(lsp_entry) = entry.get_for::<LspCompletions>() else {
            return;
        };

        let encoding = crate::server::on_ns(lsp_entry.server_ns, |server| {
            server.capabilities().map(Encoding::new)
        })
        .flatten()
        .unwrap();

        let text = buffer.text(pa);

        let replacement = if let Some(edit) = &lsp_entry.text_edit {
            let (range, new_text) = match edit {
                CompletionTextEdit::Edit(edit) => (edit.range, &edit.new_text),
                CompletionTextEdit::InsertAndReplace(edit) => (edit.replace, &edit.new_text),
            };

            let start = encoding.byte_from_pos(text, range.start);
            // It would otherwise replace just what was typed by the user, not the
            // auto-replaced text.
            let end = start.map(|s| s + entry.replacement.len());

            if let (Some(start), Some(end)) = (start, end) {
                Some((start..end, new_text))
            } else {
                None
            }
        } else if let Some(insert) = &lsp_entry.insert_text {
            let cursor = text.main_sel().cursor().byte();

            Some((cursor - insert.len()..cursor, insert))
        } else {
            None
        };

        if let Some((range, edit)) = replacement {
            let edited;
            let edit = if let None | Some(InsertTextMode::ADJUST_INDENTATION) =
                lsp_entry.insert_text_mode
                && let indent = text
                    .line(text.point_at_byte(range.start).line())
                    .indent(popts)
                && indent > 0
            {
                let indented = format!("\n{}", " ".repeat(indent));
                edited = edit.replace("\n", &indented);
                &edited
            } else {
                edit
            };

            if let Some(InsertTextFormat::SNIPPET) = lsp_entry.insert_text_format {
                buffer.replace_with_snippet(pa, range, edit);
                buffer.jump_snippets(pa, 0);
                buffer.edit_all(pa, |mut s| s.replace(""));
            } else {
                buffer.text_mut(pa).replace_range(range, edit);
            }
        }

        let mut text = buffer.text_mut(pa);

        for edit in lsp_entry.additional_text_edits.iter().flatten().rev() {
            let start = encoding.byte_from_pos(&text, edit.range.start);
            let end = encoding.byte_from_pos(&text, edit.range.end);

            if let (Some(start), Some(end)) = (start, end) {
                text.replace_range(start..end, &edit.new_text);
            }
        }
    })
    .lateness(0);
}

pub struct LspCompletions {
    uri: Uri,
    lists: Vec<(Server, Encoding, Option<List>)>,
    case_insensitive: bool,
}

impl LspCompletions {
    /// Returns a new `LspCompletions` if the current `Buffer` is
    /// elligible.
    pub fn new(pa: &mut Pass, case_insensitive: bool) -> Option<Self> {
        let buffer = context::current_buffer(pa);

        let (parser, buf) = PARSERS.write(pa, &buffer)?;

        let uri = path_to_uri(&buf.path()).unwrap();

        let lists = Vec::from_iter(parser.servers.iter().filter_map(|server| {
            let capabilities = server.capabilities()?;
            capabilities.completion_provider.as_ref()?;
            Some((server.clone(), Encoding::new(capabilities), None))
        }));

        for (server, encoding, _) in &lists {
            send_update_request(buf.text(), server, *encoding, uri.clone());
        }

        Some(Self { uri, lists, case_insensitive })
    }
}

impl duat_base::widgets::CompletionsProvider for LspCompletions {
    type Entry = Entry;

    const ALLOW_WITH_MULTIPLE_SELECTIONS: bool = false;

    fn default_fmt(entry: &Self::Entry) -> Text {
        let (details, description) = entry
            .label_details
            .as_ref()
            .map(|d| (d.detail.as_ref(), d.description.as_ref()))
            .unwrap_or_default();

        let detail = if let Some(details) = details {
            txt!("[completion.lsp.detail]{details}")
        } else {
            Text::new()
        };
        
        let description = if description != details
            && let Some(description) = description
            && description != &entry.label
        {
            txt!("[completion.lsp.description]{description}")
        } else if let Some(kind) = &entry.kind {
            match *kind {
                CompletionItemKind::TEXT => txt!("[completion.lsp.kind.text] "),
                CompletionItemKind::METHOD => txt!("[completion.lsp.kind.method] "),
                CompletionItemKind::FUNCTION => txt!("[completion.lsp.kind.function] "),
                CompletionItemKind::CONSTRUCTOR => txt!("[completion.lsp.kind.constructor] "),
                CompletionItemKind::FIELD => txt!("[completion.lsp.kind.field] "),
                CompletionItemKind::VARIABLE => txt!("[completion.lsp.kind.variable] "),
                CompletionItemKind::CLASS => txt!("[completion.lsp.kind.class] "),
                CompletionItemKind::INTERFACE => txt!("[completion.lsp.kind.interface] "),
                CompletionItemKind::MODULE => txt!("[completion.lsp.kind.module] "),
                CompletionItemKind::PROPERTY => txt!("[completion.lsp.kind.property] "),
                CompletionItemKind::UNIT => txt!("[completion.lsp.kind.property] "),
                CompletionItemKind::VALUE => txt!("[completion.lsp.kind.value] "),
                CompletionItemKind::ENUM => txt!("[completion.lsp.kind.enum] "),
                CompletionItemKind::KEYWORD => txt!("[completion.lsp.kind.keyword] "),
                CompletionItemKind::SNIPPET => txt!("[completion.lsp.kind.snippet] "),
                CompletionItemKind::COLOR => txt!("[completion.lsp.kind.color] "),
                CompletionItemKind::FILE => txt!("[completion.lsp.kind.file] "),
                CompletionItemKind::REFERENCE => txt!("[completion.lsp.kind.reference] "),
                CompletionItemKind::FOLDER => txt!("[completion.lsp.kind.folder] "),
                CompletionItemKind::ENUM_MEMBER => txt!("[completion.lsp.kind.enum_member] "),
                CompletionItemKind::CONSTANT => txt!("[completion.lsp.kind.constant] "),
                CompletionItemKind::STRUCT => txt!("[completion.lsp.kind.struct] "),
                CompletionItemKind::EVENT => txt!("[completion.lsp.kind.event] "),
                CompletionItemKind::OPERATOR => txt!("[completion.lsp.kind.operator] "),
                CompletionItemKind::TYPE_PARAMETER => {
                    txt!("[completion.lsp.kind.type_parameter] ")
                }
                _ => Text::new(),
            }
        } else {
            Text::new()
        };

        txt!("[completion.lsp.label]{entry.label}[]{detail} {Spacer}{description}")
    }

    fn matches(&mut self, text: &Text, _: Point, prefix: &str) -> Vec<Self::Entry> {
        let (prefix, case_insensitive) =
            if self.case_insensitive && prefix.chars().all(|char| !char.is_uppercase()) {
                (prefix.to_uppercase(), true)
            } else {
                (prefix.to_string(), false)
            };

        let matches = self.lists.iter().find_map(|(server, encoding, list)| {
            let Some(list) = list else {
                send_update_request(text, server, *encoding, self.uri.clone());
                return None;
            };

            if list.is_incomplete {
                send_update_request(text, server, *encoding, self.uri.clone());
            }

            let list = Vec::from_iter(list.entries.iter().enumerate().filter_map(|(idx, item)| {
                let label = item.filter_text.as_deref().unwrap_or(&item.label);

                if case_insensitive {
                    string_cmp(&prefix, &label.to_uppercase()).map(|_| Entry {
                        list_idx: idx,
                        server_ns: server.ns(),
                        item: item.clone(),
                    })
                } else {
                    string_cmp(&prefix, label).map(|_| Entry {
                        list_idx: idx,
                        server_ns: server.ns(),
                        item: item.clone(),
                    })
                }
            }));

            (!list.is_empty()).then_some(list)
        });

        matches.unwrap_or_default()
    }

    fn get_start(&self, text: &Text, cursor: Point) -> Option<usize> {
        get_start(text, cursor)
    }

    fn word(entry: &Self::Entry) -> &str {
        let common_prefix = |text: &str| {
            let common_prefix = text
                .chars()
                .zip(entry.label.chars())
                .take_while(|(l, r)| l == r)
                .fold(0, |len, (char, _)| len + char.len_utf8());
            &entry.label[..common_prefix]
        };

        if let Some(filter) = &entry.filter_text {
            let label = common_prefix(filter);
            if label.is_empty() { filter } else { label }
        } else {
            &entry.label
        }
    }

    fn default_info_on(entry: &Self::Entry) -> Option<(Text, Orientation)> {
        let text = if let Some(doc) = &entry.documentation {
            match doc {
                Documentation::String(string) => Some(Text::from(string.clone())),
                Documentation::MarkupContent(markup_content) => {
                    Some(Text::from(markup_content.value.clone()))
                }
            }
        } else {
            None
        };

        text.zip(Some(Orientation::HorTopRight))
    }
}

fn send_update_request(text: &Text, server: &Server, encoding: Encoding, uri: Uri) {
    let cursor = text.selections().main().cursor();

    server.send_request::<Completion>(
        CompletionParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: encoding.pos_from_point(text, cursor),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
            context: Some(CompletionContext {
                trigger_kind: CompletionTriggerKind::INVOKED,
                trigger_character: None,
            }),
        },
        {
            let ns = server.ns();
            move |pa, result| {
                let (Some(result), true) = (result, Completions::is_open(pa)) else {
                    return;
                };

                let new_list = match result {
                    CompletionResponse::Array(items) => List {
                        is_incomplete: false,
                        entries: items.into_iter().map(Arc::new).collect(),
                    },
                    CompletionResponse::List(list) => List {
                        is_incomplete: list.is_incomplete,
                        entries: list.items.into_iter().map(Arc::new).collect(),
                    },
                };

                Completions::update_provider(pa, move |completions: &mut LspCompletions, _| {
                    let (.., old) = completions
                        .lists
                        .iter_mut()
                        .find(|(server, ..)| ns == server.ns())
                        .unwrap();
                    *old = Some(new_list);
                });
            }
        },
    );
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

fn get_start(text: &Text, cursor: Point) -> Option<usize> {
    text.search(r"\w*\z")
        .range(..cursor)
        .next_back()
        .map(|r| r.start)
}

struct List {
    is_incomplete: bool,
    entries: Vec<Arc<CompletionItem>>,
}

/// A completion item entry.
#[derive(Debug, Clone)]
pub struct Entry {
    list_idx: usize,
    server_ns: Ns,
    item: Arc<CompletionItem>,
}

impl std::ops::Deref for Entry {
    type Target = CompletionItem;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}
