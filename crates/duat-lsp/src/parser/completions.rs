use std::sync::{Arc, LazyLock};

use duat_base::{
    BaseBuffer,
    hooks::{CompletionFocused, CompletionSelected},
    widgets::Completions,
};
use duat_core::{
    Ns,
    buffer::Buffer,
    context,
    data::Pass,
    hook::{self, Idled},
    text::{Point, RegexHaystack, Spacer, Text},
    txt,
    ui::Orientation,
};
use jsonrpc_lite::Id;
use lsp_types::{
    CompletionContext, CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse,
    CompletionTextEdit, CompletionTriggerKind, Documentation, InsertReplaceEdit, InsertTextFormat,
    InsertTextMode, PartialResultParams, TextDocumentIdentifier, TextDocumentPositionParams, Uri,
    WorkDoneProgressParams,
    request::{Completion, ResolveCompletionItem},
};

use crate::{Encoding, parser::PARSERS, path_to_uri, server::Server};

/// A completion item entry.
#[derive(Debug, Clone)]
pub struct Entry {
    server_ns: Ns,
    item: Arc<CompletionItem>,
    encoding: Encoding,
}

impl duat_base::widgets::CompletionItem for Entry {
    fn value(&self) -> String {
        let common_prefix = |text: &str| {
            let common_prefix = text
                .chars()
                .zip(self.label.chars())
                .take_while(|(l, r)| l == r)
                .fold(0, |len, (char, _)| len + char.len_utf8());
            &self.label[..common_prefix]
        };

        if let Some(filter) = &self.filter_text {
            let label = common_prefix(filter);
            if label.is_empty() { filter } else { label }.to_string()
        } else {
            self.label.clone()
        }
    }

    fn default_fmt(&self) -> Text {
        let (details, description) = self
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
            && description != &self.label
        {
            txt!("[completion.lsp.description]{description}")
        } else if let Some(kind) = &self.kind {
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

        txt!("[completion.lsp.label]{self.label}[]{detail} {Spacer}{description}")
    }

    fn default_info(&self) -> Option<(Text, Orientation)> {
        let text = if let Some(doc) = &self.documentation {
            match doc {
                Documentation::String(string) => Some(Text::from(string.clone())),
                Documentation::MarkupContent(markup_content) => {
                    let mut text = Text::from(markup_content.value.clone());

                    duat_treesitter::parse_as(text.as_mut(), .., "markdown");

                    Some(text)
                }
            }
        } else {
            None
        };

        text.zip(Some(Orientation::HorTopRight))
    }
}

impl std::ops::Deref for Entry {
    type Target = CompletionItem;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

/// Setup hooks for completions.
pub fn setup_hooks() {
    hook::add::<CompletionFocused<Entry>>(|_, entry| {
        if entry.documentation.is_some() {
            return;
        }

        let server_ns = entry.server_ns;
        let idx = entry.index;

        let old_item = entry.item.item.clone();

        crate::server::on_ns(entry.server_ns, |server| {
            server.send_request_with_id::<ResolveCompletionItem>(
                Id::Str(format!("{}{}", entry.label, entry.index)),
                old_item.as_ref().clone(),
                move |pa, new_item| {
                    Completions::update_list::<Entry>(pa, *NS, move |list| {
                        let idx = list
                            .get(idx)
                            .filter(|entry| entry.item == old_item)
                            .and(Some(idx))
                            .or_else(|| list.iter().position(|entry| entry.item == old_item));

                        let item = CompletionItem {
                            label: old_item.label.clone(),
                            insert_text: old_item.insert_text.clone(),
                            insert_text_format: old_item.insert_text_format,
                            insert_text_mode: old_item.insert_text_mode,
                            text_edit: old_item.text_edit.clone(),
                            additional_text_edits: old_item.additional_text_edits.clone(),
                            ..new_item
                        };

                        if let Some(idx) = idx {
                            list[idx] = Entry {
                                server_ns,
                                item: Arc::new(item),
                                encoding: list[idx].encoding,
                            };
                        }
                    });
                },
            );
        });
    });

    hook::add::<CompletionSelected<Entry>>(|pa, entry| {
        let buffer = context::current_buffer(pa);
        let popts = buffer.opts(pa);

        let encoding = crate::server::on_ns(entry.item.server_ns, |server| {
            server.capabilities().map(Encoding::new)
        })
        .flatten()
        .unwrap();

        let replacement = if let Some(edit) = &entry.item.text_edit {
            let (range, new_text) = match edit {
                CompletionTextEdit::Edit(edit) => (edit.range, &edit.new_text),
                CompletionTextEdit::InsertAndReplace(edit) => (edit.replace, &edit.new_text),
            };

            let text = buffer.text(pa);

            let start = encoding.byte_from_pos(text, range.start);
            let end = encoding.byte_from_pos(text, range.end);

            if let (Some(start), Some(end)) = (start, end) {
                Some((Some(start..end), new_text))
            } else {
                None
            }
        } else {
            entry.item.insert_text.as_ref().map(|insert| (None, insert))
        };

        if let Some((replace_range, edit)) = replacement {
            let mut text = buffer.text_mut(pa);

            let range = if let Some(replace_range) = replace_range {
                let start = entry.orig_range.start;
                let end = entry.orig_range.start + entry.replacement.len();
                text.replace_range(start..end, entry.orig_typed);
                replace_range
            } else {
                entry.orig_range.clone()
            };

            let edited;
            let edit = if let None | Some(InsertTextMode::ADJUST_INDENTATION) =
                entry.item.insert_text_mode
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

            if let Some(InsertTextFormat::SNIPPET) = entry.insert_text_format {
                buffer.replace_with_snippet(pa, range, edit);
                buffer.jump_snippets(pa, 0);
                buffer.edit_all(pa, |mut s| s.replace(""));
            } else {
                buffer.text_mut(pa).replace_range(range, edit);
            }
        }

        let mut text = buffer.text_mut(pa);

        for edit in entry.additional_text_edits.iter().flatten().rev() {
            let start = encoding.byte_from_pos(&text, edit.range.start);
            let end = encoding.byte_from_pos(&text, edit.range.end);

            if let (Some(start), Some(end)) = (start, end) {
                text.replace_range(start..end, &edit.new_text);
            }
        }
    })
    .lateness(0);
}

pub struct LspCompletions {}

impl LspCompletions {
    /// Enables completion for LSPs.
    ///
    /// This will request a list of completions immediately,
    /// and will also request new lists automatically when
    /// duat [idles].
    ///
    /// [idles]: Idled
    pub fn enable(pa: &mut Pass) {
        update_completions(pa);
        hook::add::<Idled>(|pa, _| update_completions(pa)).grouped(*NS);
    }

    /// Disables completion for LSPs.
    pub fn disable(pa: &mut Pass) {
        hook::remove(*NS);
        Completions::remove_list(pa, *NS);
    }
}

fn update_completions(pa: &mut Pass) {
    if Completions::is_selecting(pa) {
        return;
    }

    let Some(buffer) = context::current_widget(pa).get_as::<Buffer>() else {
        return;
    };

    let Some((parser, buf)) = PARSERS.write(pa, &buffer) else {
        return;
    };

    let uri = path_to_uri(&buf.path()).unwrap();

    for server in &parser.servers {
        let Some(encoding) = server.capabilities().and_then(|cap| {
            cap.completion_provider.as_ref()?;
            Some(Encoding::new(cap))
        }) else {
            continue;
        };

        send_update_request(buf.text(), server, encoding, uri.clone());
    }
}

fn send_update_request(text: &Text, server: &Server, encoding: Encoding, uri: Uri) {
    let cursor = text.selections().main().cursor();
    let path = crate::uri_to_path(uri.clone());

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
            let server_ns = server.ns();
            move |pa, result| {
                let (Some(result), true) = (result, Completions::is_open(pa)) else {
                    return;
                };

                let list = match result {
                    CompletionResponse::Array(items) => {
                        Vec::from_iter(items.into_iter().map(|item| Entry {
                            server_ns,
                            item: Arc::new(item),
                            encoding,
                        }))
                    }
                    CompletionResponse::List(list) => {
                        Vec::from_iter(list.items.into_iter().map(|item| Entry {
                            server_ns,
                            item: Arc::new(item),
                            encoding,
                        }))
                    }
                };

                let Some(buffer) = context::buffer_from_path(pa, &path) else {
                    return;
                };
                let Some(cursor) = buffer.text(pa).get_main_sel().map(|sel| sel.cursor()) else {
                    return;
                };

                let start = get_start(&list, buffer.text(pa), cursor);

                Completions::add_list(pa, list, start, 100, *NS);
            }
        },
    );
}

fn get_start(list: &[Entry], text: &Text, cursor: Point) -> usize {
    let mut inferred_byte = None;

    for entry in list {
        if let Some(CompletionTextEdit::InsertAndReplace(edit @ InsertReplaceEdit { .. })) =
            &entry.text_edit
            && let Some(start_byte) = entry.encoding.byte_from_pos(text, edit.replace.start)
        {
            if let Some(byte) = inferred_byte
                && byte != start_byte
            {
                inferred_byte = None;
                break;
            } else if start_byte > cursor.byte() {
                break;
            } else {
                inferred_byte = Some(start_byte);
            }
        }
    }

    if let Some(byte) = inferred_byte {
        byte
    } else {
        text.search(r"\w*\z")
            .range(..cursor)
            .next_back()
            .map(|r| r.start)
            .unwrap()
    }
}

static NS: LazyLock<Ns> = Ns::new_lazy();
