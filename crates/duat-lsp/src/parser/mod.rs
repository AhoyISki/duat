use duat_core::{
    buffer::{Buffer, BufferTracker, PerBuffer},
    context::{self, Handle},
    data::Pass,
    hook::{self, BufferClosed, BufferOpened, BufferUpdated},
};
use duat_filetype::PassFileType;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem, Uri,
    VersionedTextDocumentIdentifier,
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
};

use crate::{
    path_to_uri,
    server::{self, Server},
};

/// The struct responsible for updating each individual [`Buffer`].
pub struct Parser {
    uri: Uri,
    servers: Vec<Server>,
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

	/// The list of servers serving this `Parser`.
    pub fn servers(&self) -> &[Server] {
        &self.servers
    }
}

static PARSERS: PerBuffer<Parser> = PerBuffer::new();
static TRACKER: BufferTracker = BufferTracker::new();

pub fn setup_hooks() {
    hook::add::<BufferOpened>(|pa, handle| {
        if let Some(filetype) = handle.filetype(pa)
            && let Some(path) = handle.read(pa).path_kind().as_path()
        {
            let Some(uri) = path_to_uri(&path) else {
                context::warn!("File path is not valid UTF8, won't connect to language servers");
                return;
            };
            let Some(servers) = server::get_servers_for(&path) else {
                return;
            };

            let text = handle.text(pa);

            if !handle.read(pa).was_reloaded() {
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

            TRACKER.register_buffer(handle.write(pa));
            PARSERS.register(pa, handle, Parser { uri, servers });
        }
    });

    hook::add::<BufferUpdated>(|pa, handle| {
        if let Some((parser, buffer)) = PARSERS.write(pa, handle)
            && let Some(parts) = TRACKER.parts(buffer)
            && parts.changes.len() > 0
        {
            for server in &parser.servers {
                let bytes = server.position_encoding().num_bytes();

                let notification = DidChangeTextDocumentParams {
                    text_document: VersionedTextDocumentIdentifier {
                        uri: parser.uri.clone(),
                        version: parts.version().strs as i32,
                    },
                    content_changes: parts
                        .changes
                        .clone()
                        .map(|change| {
                            let (start, end) = (change.start(), change.taken_end());

                            TextDocumentContentChangeEvent {
                                range: Some(lsp_types::Range {
                                    start: lsp_types::Position {
                                        line: start.line() as u32,
                                        character: (start.byte_col(parts.strs) / bytes) as u32,
                                    },
                                    end: lsp_types::Position {
                                        line: end.line() as u32,
                                        character: (end.byte_col(parts.strs) / bytes) as u32,
                                    },
                                }),
                                range_length: None,
                                text: change.added_str().to_string(),
                            }
                        })
                        .collect(),
                };
                server.send_notification::<DidChangeTextDocument>(notification);
            }
        }
    });

    hook::add::<BufferClosed>(|pa, handle| {
        if let Some(path) = handle.read(pa).path_kind().as_path()
            && let Some(uri) = path_to_uri(&path)
        {
            server::on_all_servers(|server| {
                server.send_notification::<DidCloseTextDocument>(DidCloseTextDocumentParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                });
            });
        }
    });
}
