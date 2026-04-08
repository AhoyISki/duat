use std::path::{Path, PathBuf};

use lsp_types::{ServerCapabilities, Uri};

mod config;
mod parser;
mod server;

pub struct DuatLsp;

impl DuatLsp {
    /// Adds the `DuatLsp` plugin.
    ///
    /// *DON'T USE THIS DIRECTLY, USE `duat::plug` INSTEAD*.
    #[doc(hidden)]
    #[inline(never)]
    pub fn _plug(self) {
        parser::setup_hooks();
    }
}

fn path_to_uri(path: &Path) -> Option<Uri> {
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

fn uri_to_path(uri: Uri) -> PathBuf {
    let encoded = uri.path().as_str().to_string();
    let mut decoded = encoded.to_string();

    for (i, _) in encoded.rmatch_indices('%') {
        let byte = u8::from_str_radix(&encoded[i + 1..i + 3], 16).unwrap();
        // SAFETY: Before encoding, this was a utf8 string.
        unsafe {
            decoded.as_mut_vec().splice(i..i + 3, [byte]);
        }
    }

    PathBuf::from(decoded)
}

/// An encoding for text positions.
#[derive(Default)]
pub enum Encoding {
    Utf8,
    #[default]
    Utf16,
    Utf32,
}

impl Encoding {
    /// Returns the `Encoding` being used by a server.
    pub fn new(cap: &ServerCapabilities) -> Self {
        match cap.position_encoding.as_ref().map(|enc| enc.as_str()) {
            Some("utf-8") | None => Encoding::Utf8,
            Some("utf-16") => Encoding::Utf16,
            Some("utf-32") => Encoding::Utf32,
            Some(_) => Encoding::Utf16,
        }
    }

    /// The number of bytes occupied by a single code point.
    pub fn num_bytes(&self) -> usize {
        match self {
            Encoding::Utf8 => 1,
            Encoding::Utf16 => 2,
            Encoding::Utf32 => 4,
        }
    }
}
