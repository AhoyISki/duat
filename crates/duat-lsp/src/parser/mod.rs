use std::{
    collections::HashSet,
    path::PathBuf,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    Ns,
    buffer::{Buffer, PerBuffer},
    context::{self, Handle},
    data::Pass,
    hook::{self, BufferClosed, BufferOpened, BufferUpdated},
    storage,
};
use duat_filetype::PassFileType;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem, Uri,
    VersionedTextDocumentIdentifier,
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
};

use crate::{
    Encoding,
    parser::semantic_tokens::BufferTokens,
    path_to_uri,
    server::{self, Server},
};

mod semantic_tokens;

/// The struct responsible for updating each individual [`Buffer`].
pub struct Parser {
    uri: Uri,
    ns: Ns,
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
        handle: &'p Handle,
    ) -> Option<(&'p mut Parser, &'p mut Buffer)> {
        PARSERS.write(pa, handle)
    }
}

static PARSERS: PerBuffer<Parser> = PerBuffer::new();

pub fn setup_hooks() {
    #[derive(Default, storage::bincode::Decode, storage::bincode::Encode)]
    #[bincode(crate = "duat_core::storage::bincode")]
    struct OpenedBuffers(Mutex<HashSet<PathBuf>>);

    static OPENED_BUFFERS: LazyLock<OpenedBuffers> =
        LazyLock::new(|| storage::get_if(|_| true).unwrap_or_default());

    hook::add::<BufferOpened>(|pa, handle| {
        if let Some(filetype) = handle.filetype(pa) {
            let path = handle.read(pa).path();

            let Some(servers) = server::get_servers_for(&path) else {
                return;
            };

            let Some(uri) = path_to_uri(&path) else {
                context::warn!("File path is not valid UTF8, won't connect to language servers");
                return;
            };

            let text = handle.text(pa);

            let mut opened_buffers = OPENED_BUFFERS.0.lock().unwrap();
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

            let (parser, buffer) = PARSERS.register(pa, handle, Parser {
                uri,
                ns: Ns::new(),
                servers: servers.clone(),
                tokens: BufferTokens::default(),
            });

            for server in servers {
                server.send_semantic_tokens_request(buffer.path(), handle, parser);
            }
        }
    });

    hook::add::<BufferUpdated>(|pa, handle| {
        if let Some((parser, buf)) = PARSERS.write(pa, handle)
            && let moment = buf.moment_for(parser.ns)
            && moment.is_empty()
        {
            for server in &parser.servers {
                let bytes = server
                    .capabilities()
                    .map(Encoding::new)
                    .unwrap_or_default()
                    .num_bytes();

                let notification = DidChangeTextDocumentParams {
                    text_document: VersionedTextDocumentIdentifier {
                        uri: parser.uri.clone(),
                        version: buf.text().version().strs as i32,
                    },
                    content_changes: moment
                        .iter()
                        .map(|change| {
                            let start = change.start();

                            TextDocumentContentChangeEvent {
                                range: Some(lsp_types::Range {
                                    start: lsp_types::Position {
                                        line: start.line() as u32,
                                        character: (start.byte_col(buf.text()) / bytes) as u32,
                                    },
                                    end: lsp_types::Position {
                                        line: change.taken_end().line() as u32,
                                        character: change.taken_byte_col(buf.text()) as u32,
                                    },
                                }),
                                range_length: None,
                                text: change.added_str().to_string(),
                            }
                        })
                        .collect(),
                };
                server.send_notification::<DidChangeTextDocument>(notification);

                server.send_semantic_tokens_request(buf.path(), handle, parser);
            }
        }
    });

    hook::add::<BufferClosed>(|pa, handle| {
        let path = handle.read(pa).path();
        if let Some(uri) = path_to_uri(&path) {
            server::on_all_servers(|server| {
                server.send_notification::<DidCloseTextDocument>(DidCloseTextDocumentParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                });
            });
            OPENED_BUFFERS.0.lock().unwrap().remove(&path);
        }
    });
}
