use duat_base::widgets::Completions;
use duat_core::{
    context,
    data::Pass,
    text::{Point, RegexHaystack, Spacer, Text},
    txt,
    ui::Orientation,
};
use lsp_types::{
    CompletionContext, CompletionItem, CompletionItemKind, CompletionList, CompletionParams,
    CompletionResponse, CompletionTriggerKind, Documentation, PartialResultParams,
    TextDocumentIdentifier, TextDocumentPositionParams, Uri, WorkDoneProgressParams,
    request::Completion,
};

use crate::{Encoding, parser::PARSERS, path_to_uri, server::Server};

pub struct LspCompletions {
    uri: Uri,
    lists: Vec<(Server, Encoding, Option<CompletionList>)>,
    case_insensitive: bool,
    state: (Point, String),
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

        let state = {
            let cursor = buf.selections().main().cursor();
            let start = get_start(buf.text(), cursor)
                .map(|b| buf.text().point_at_byte(b))
                .unwrap_or(cursor);
            (start, buf.text()[start..cursor].to_string())
        };

        Some(Self { uri, lists, case_insensitive, state })
    }
}

impl duat_base::widgets::CompletionsProvider for LspCompletions {
    type Entry<'e> = &'e CompletionItem;

    fn default_fmt(entry: &Self::Entry<'_>) -> Text {
        let details = if let Some(details) = &entry.label_details
            && let Some(detail) = &details.detail
        {
            txt!("[completion.lsp.detail]{detail}")
        } else {
            Text::new()
        };

        let kind = if let Some(kind) = &entry.kind {
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

        txt!("[completion.lsp.label]{entry.label}[]{details}{Spacer}{kind}")
    }

    fn matches(&mut self, text: &Text, start: Point, prefix: &str) -> Vec<Self::Entry<'_>> {
        let state_changed = self.state.0 != start || self.state.1 != prefix;
        self.state = (start, prefix.to_string());

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

            if state_changed && list.is_incomplete {
                send_update_request(text, server, *encoding, self.uri.clone());
            }

            let list = Vec::from_iter(list.items.iter().filter_map(|item| {
                let label = item.filter_text.as_deref().unwrap_or(&item.label);

                if case_insensitive {
                    string_cmp(&prefix, &label.to_uppercase()).and(Some(item))
                } else {
                    string_cmp(&prefix, label).and(Some(item))
                }
            }));

            (!list.is_empty()).then_some(list)
        });

        matches.unwrap_or_default()
    }

    fn get_start(&self, text: &Text, cursor: Point) -> Option<usize> {
        get_start(text, cursor)
    }

    fn word<'e>(entry: &'e Self::Entry<'_>) -> &'e str {
        entry.insert_text.as_deref().unwrap_or(&entry.label)
    }

    fn default_info_on(entry: &Self::Entry<'_>) -> Option<(Text, Orientation)> {
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

                let list = match result {
                    CompletionResponse::Array(items) => {
                        CompletionList { is_incomplete: false, items }
                    }
                    CompletionResponse::List(list) => list,
                };

                Completions::update_provider(pa, move |completions: &mut LspCompletions| {
                    let (.., old) = completions
                        .lists
                        .iter_mut()
                        .find(|(server, ..)| ns == server.ns())
                        .unwrap();
                    *old = Some(list);
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
