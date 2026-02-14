use std::path::Path;

use duat_core::{
    Plugin, context,
    hook::{self, BufferOpened, BufferUnloaded},
};
use duat_filetype::PassFileType;
use lsp_types::{
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, TextDocumentIdentifier,
    TextDocumentItem, Uri,
    notification::{DidCloseTextDocument, DidOpenTextDocument},
};

mod config;
mod server;

pub struct DuatLsp;

impl Plugin for DuatLsp {
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
                if let Some(_servers) = server::get_servers_for(&path) {}

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
