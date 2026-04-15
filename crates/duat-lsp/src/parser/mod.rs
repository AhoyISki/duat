use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    buffer::{Buffer, PerBuffer},
    context::{self, Handle},
    data::Pass,
    hook::{self, BufferClosed, BufferOpened, BufferSaved, BufferUpdated, ConfigUnloaded},
    storage,
    text::TextMut,
};
use duat_filetype::PassFileType;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentFormattingParams, FormattingOptions, OneOf,
    ServerCapabilities, TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
    TextDocumentSyncCapability, TextDocumentSyncSaveOptions, TextEdit, Uri,
    VersionedTextDocumentIdentifier, WorkDoneProgressParams,
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument,
    },
    request::Formatting,
};

use crate::{
    Encoding,
    parser::semantic_tokens::BufferTokens,
    path_to_uri,
    server::{self, Server},
};

pub mod diagnostics;
mod semantic_tokens;

/// LSP functions for a [`Buffer`].
pub trait LspBuffer {
    /// Format this [`Buffer`] via a 'textDocument/formatting'
    /// request.
    ///
    /// If the `server_name` is `None`, will look for the first
    /// available server capable of answering this request.
    ///
    /// Do note that the formatting won't happen immediately, and will
    /// fail if any changes are made to the `Buffer` while the request
    /// is being processed.
    fn lsp_format(&self, pa: &mut Pass, server_name: Option<&str>);
}

impl LspBuffer for Handle {
    fn lsp_format(&self, pa: &mut Pass, server_name: Option<&str>) {
        fn can_format(capabilities: &ServerCapabilities) -> bool {
            if let Some(provider) = capabilities.document_formatting_provider.as_ref()
                && let OneOf::Left(true) | OneOf::Right(_) = provider
            {
                true
            } else {
                false
            }
        }

        let buf = self.write(pa);
        let uri = path_to_uri(&buf.path());
        let opts = buf.opts;
        let version = buf.text().version().strs;

        let Some((parser, uri)) = PARSERS
            .get_mut(buf)
            .filter(|parser| !parser.servers.is_empty())
            .and_then(|parser| Some((parser, uri?)))
        else {
            context::warn!("{} has no registered LSP servers", buf.name_txt());
            return;
        };

        let server = if let Some(server_name) = server_name {
            if let Some(server) = parser
                .servers
                .iter()
                .find(|server| server.name() == server_name)
                && let Some(capabilities) = server.capabilities()
                && can_format(capabilities)
            {
                server
            } else {
                context::warn!("Server [a]{server_name}[] is not capable of formatting");
                return;
            }
        } else if let Some((capabilities, server)) = parser
            .servers
            .iter()
            .find_map(|server| server.capabilities().zip(Some(server)))
            && can_format(capabilities)
        {
            server
        } else {
            context::warn!(
                "{} has no LSP servers capable of formatting",
                buf.name_txt()
            );
            return;
        };

        let buffer = self.clone();
        server.send_request::<Formatting>(
            DocumentFormattingParams {
                text_document: TextDocumentIdentifier { uri },
                options: FormattingOptions {
                    tab_size: opts.tabstop as u32,
                    insert_spaces: true,
                    properties: HashMap::new(),
                    trim_trailing_whitespace: Some(true),
                    insert_final_newline: None,
                    trim_final_newlines: None,
                },
                work_done_progress_params: WorkDoneProgressParams { work_done_token: None },
            },
            move |pa, result| {
                let Some(mut edits) = result else {
                    return;
                };

                let buf = buffer.write(pa);
                if buf.text().version().strs != version {
                    return;
                }

                edits.sort_by(|lhs, rhs| lhs.range.start.cmp(&rhs.range.start));

                let mut text = buf.text_mut();
                for edit in edits.into_iter().rev() {
                    apply_edit(&mut text, edit);
                }
            },
        );
    }
}

/// The struct responsible for updating each individual [`Buffer`].
pub struct Parser {
    uri: Uri,
    pub servers: Vec<Server>,
    /// The [`SemanticToken`]s that have been applied to the `Buffer`.
    ///
    /// [`SemanticToken`]: lsp_types::SemanticToken
    pub tokens: BufferTokens,
}

impl Parser {
    /// Acquire a `Parser` for a given [`Buffer`], if one was added
    /// for it.
    pub fn write_for<'p>(
        pa: &'p mut Pass,
        buffer: &'p Handle,
    ) -> Option<(&'p mut Parser, &'p mut Buffer)> {
        PARSERS.write(pa, buffer)
    }

    /// The [`Uri`] of the [`Buffer`].
    pub fn uri(&self) -> &Uri {
        &self.uri
    }
}

static PARSERS: PerBuffer<Parser> = PerBuffer::new();

pub fn setup_hooks() {
    static OPENED_BUFFERS: LazyLock<Mutex<HashSet<PathBuf>>> =
        LazyLock::new(|| Mutex::new(storage::get_if(|_| true).unwrap_or_default()));

    hook::add::<ConfigUnloaded>(|pa, is_quitting| {
        if !is_quitting {
            _ = storage::store(pa, std::mem::take(&mut *OPENED_BUFFERS.lock().unwrap()))
        }
    });

    hook::add::<BufferOpened>(|pa, buffer| {
        if let Some(filetype) = buffer.filetype(pa) {
            let path = buffer.read(pa).path();

            let Some(servers) = server::get_servers_for(&path) else {
                return;
            };

            let Some(uri) = path_to_uri(&path) else {
                context::warn!("File path is not valid UTF8, won't connect to language servers");
                return;
            };

            let text = buffer.text(pa);

            let mut opened_buffers = OPENED_BUFFERS.lock().unwrap();
            if opened_buffers.insert(path) {
                server::on_all_servers(|server| {
                    server.send_notification::<DidOpenTextDocument>(DidOpenTextDocumentParams {
                        text_document: TextDocumentItem {
                            uri: uri.clone(),
                            language_id: filetype.to_string(),
                            version: text.version().strs as i32,
                            text: text.to_string(),
                        },
                    });
                });
            }

            let (parser, buf) = PARSERS.register(
                pa,
                buffer,
                Parser {
                    uri,
                    servers: servers.clone(),
                    tokens: BufferTokens::default(),
                },
            );

            for server in &servers {
                server.send_semantic_tokens_request(buffer, parser);
                _ = buf.moment_for(server.ns());
            }

            diagnostics::add_initial(pa, &servers, buffer);
        }
    });

    hook::add::<BufferUpdated>(move |pa, buffer| {
        let Some((parser, buf)) = PARSERS.write(pa, buffer) else {
            return;
        };
        for server in &parser.servers {
            let Some(encoding) = server.capabilities().map(Encoding::new) else {
                continue;
            };

            let moment = buf.moment_for(server.ns());
            if moment.is_empty() {
                continue;
            }

            let notification = DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: parser.uri.clone(),
                    version: buf.text().version().strs as i32,
                },
                content_changes: moment
                    .iter()
                    .map(|change| {
                        let start = encoding.pos_from_point(buf.text(), change.start());
                        let end =
                            encoding.taken_end_pos(start, change.taken_end(), change.taken_str());

                        TextDocumentContentChangeEvent {
                            range: Some(lsp_types::Range { start, end }),
                            range_length: None,
                            text: change.added_str().to_string(),
                        }
                    })
                    .collect(),
            };
            server.send_notification::<DidChangeTextDocument>(notification);

            server.send_semantic_tokens_request(buffer, parser);
        }
    });

    hook::add::<BufferSaved>(|pa, (buffer, _)| {
        if let Some((parser, buf)) = PARSERS.write(pa, buffer) {
            for server in &parser.servers {
                let text = server.capabilities().and_then(|cap| {
                    let opts = match cap.text_document_sync.as_ref()? {
                        TextDocumentSyncCapability::Kind(_) => None,
                        TextDocumentSyncCapability::Options(opts) => Some(opts),
                    };

                    match opts.as_ref()?.save.as_ref()? {
                        TextDocumentSyncSaveOptions::Supported(_) => None,
                        TextDocumentSyncSaveOptions::SaveOptions(opts) => {
                            opts.include_text?.then_some(buf.text().to_string())
                        }
                    }
                });

                server.send_notification::<DidSaveTextDocument>(DidSaveTextDocumentParams {
                    text_document: TextDocumentIdentifier { uri: parser.uri.clone() },
                    text,
                });
            }
        }
    });

    hook::add::<BufferClosed>(|pa, buffer| {
        let path = buffer.read(pa).path();
        if let Some(uri) = path_to_uri(&path) {
            server::on_all_servers(|server| {
                server.send_notification::<DidCloseTextDocument>(DidCloseTextDocumentParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                });
            });
            OPENED_BUFFERS.lock().unwrap().remove(&path);
        }
    });
}

fn apply_edit(text: &mut TextMut, edit: TextEdit) {
    let range = edit.range;
    let start = text.point_at_coords(range.start.line as usize, range.start.character as usize);
    let end = text.point_at_coords(range.end.line as usize, range.end.character as usize);
    text.replace_range(start..end, edit.new_text);
}
