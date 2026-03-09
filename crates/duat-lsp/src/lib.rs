use std::path::Path;

use duat_core::{
    Plugin,
    buffer::{BufferTracker, PerBuffer},
    context,
    hook::{self, BufferOpened, BufferUnloaded, BufferUpdated},
};
use duat_filetype::PassFileType;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem, Uri,
    VersionedTextDocumentIdentifier,
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
};

use crate::server::Server;

mod config;
mod server;

pub struct DuatLsp;

struct BufferInfo {
    uri: Uri,
    servers: Vec<Server>,
}

static SERVERS: PerBuffer<BufferInfo> = PerBuffer::new();
static TRACKER: BufferTracker = BufferTracker::new();

impl Plugin for DuatLsp {
    #[inline(never)]
    fn plug(self, _: &duat_core::Plugins) {
        hook::add::<BufferOpened>(|pa, handle| {
            if let Some(filetype) = handle.filetype(pa)
                && let Some(path) = handle.read(pa).path_kind().as_path()
            {
                let Some(uri) = file_uri(&path) else {
                    context::warn!(
                        "File path is not valid UTF8, won't connect to language servers"
                    );
                    return;
                };
                let Some(servers) = server::get_servers_for(&path) else {
                    return;
                };

                let text = handle.text(pa);

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

                TRACKER.register_buffer(handle.write(pa));
                SERVERS.register(pa, handle, BufferInfo { uri, servers });
            }
        });

        hook::add::<BufferUpdated>(|pa, handle| {
            if let Some((info, buffer)) = SERVERS.write(pa, handle)
                && let Some(parts) = TRACKER.parts(buffer)
            {
                for server in &info.servers {
                    let bytes = server.position_encoding().num_bytes();

                    let notification = DidChangeTextDocumentParams {
                        text_document: VersionedTextDocumentIdentifier {
                            uri: info.uri.clone(),
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

        hook::add::<BufferUnloaded>(|pa, handle| {
            if let Some(path) = handle.read(pa).path_kind().as_path()
                && let Some(uri) = file_uri(&path)
            {
                server::on_all_servers(|server| {
                    server.send_notification::<DidCloseTextDocument>(DidCloseTextDocumentParams {
                        text_document: TextDocumentIdentifier { uri: uri.clone() },
                    });
                });
            }
        });
    }
}

fn file_uri(path: &Path) -> Option<Uri> {
    pub fn encode_uri(str: &str) -> String {
        let mut encoded = String::with_capacity(str.len());
        for c in str.bytes() {
            match c {
                b'A'..=b'Z'
                | b'a'..=b'z'
                | b'0'..=b'9'
                | b'-'
                | b'_'
                | b'.'
                | b'!'
                | b'~'
                | b'*'
                | b'\''
                | b'('
                | b')'
                | b';'
                | b','
                | b'/'
                | b'?'
                | b':'
                | b'@'
                | b'&'
                | b'='
                | b'+'
                | b'$'
                | b'#' => encoded.push(char::from_u32(c as _).unwrap()),
                c => {
                    encoded.push('%');
                    encoded.push_str(&format!("{:02x}", c));
                }
            }
        }
        encoded
    }

    format!("file://{}", encode_uri(path.to_str()?))
        .parse()
        .ok()
}
