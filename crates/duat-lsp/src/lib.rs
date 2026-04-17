use std::{
    ops::ControlFlow,
    path::{Path, PathBuf},
};

use duat_core::{
    buffer::Buffer,
    context::Handle,
    data::Pass,
    form::{self, Form},
    mode::{self, KeyEvent, Mode, User, event},
    text::{Point, Strs},
    txt,
};
use lsp_types::{Position, ServerCapabilities, Uri};

pub use crate::parser::LspBuffer;

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
        form::set("lsp.info", Form::mimic("default.info"));
        form::set("lsp.hint", Form::mimic("default.info"));
        form::set("lsp.warn", Form::mimic("default.warn"));
        form::set("lsp.error", Form::mimic("default.error"));
        form::set("lsp.tt.info", Form::mimic("accent.info"));
        form::set("lsp.tt.hint", Form::mimic("accent.info"));
        form::set("lsp.tt.warn", Form::mimic("accent.warn"));
        form::set("lsp.tt.error", Form::mimic("accent.error"));

        parser::setup_hooks();

        mode::map::<User>("l", |pa: &mut _| mode::set(pa, Lsp)).doc(txt!("Enter [mode]Lsp[] mode"));
    }
}

struct Lsp;

impl Mode for Lsp {
    type Widget = Buffer;

    fn bindings() -> duat_core::mode::Bindings {
        mode::bindings!(match _ {
            event!('f') => txt!("Format the buffer"),
        })
    }

    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent, buffer: Handle<Self::Widget>) {
        match key_event {
            event!('f') => buffer.lsp_format(pa, None),
            _ => {}
        }
        mode::reset::<Buffer>(pa);
    }
}

fn path_to_uri(path: &Path) -> Option<Uri> {
    pub fn encode_uri(str: &str) -> String {
        let mut encoded = String::with_capacity(str.len());
        for s in str.bytes() {
            match s {
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
                | b'#' => encoded.push(char::from_u32(s as _).unwrap()),
                s => {
                    encoded.push('%');
                    encoded.push_str(&format!("{:02x}", s));
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
#[derive(Default, Clone, Copy)]
enum Encoding {
    Utf8,
    #[default]
    Utf16,
}

impl Encoding {
    /// Returns the `Encoding` being used by a server.
    fn new(cap: &ServerCapabilities) -> Self {
        match cap.position_encoding.as_ref().map(|enc| enc.as_str()) {
            Some("utf-8") | None => Encoding::Utf8,
            Some("utf-16") => Encoding::Utf16,
            Some("utf-32") => panic!("utf-32 is not supported"),
            Some(_) => Encoding::Utf16,
        }
    }

    /// Returns a codepoint index from a [`Position`].
    #[track_caller]
    fn byte_from_pos(&self, strs: &Strs, pos: Position) -> Option<usize> {
        match self {
            Encoding::Utf8 => {
                let byte = strs.get_point_at_coords(pos.line as usize, 0)?.byte();
                Some(byte + pos.character as usize)
            }
            Encoding::Utf16 => {
                if pos.line as usize > strs.end_point().line() {
                    return None;
                }

                let line = strs.line(pos.line as usize);
                let mut codepoints = 0;
                line.chars()
                    .try_fold(line.start_point().byte(), |byte, char| {
                        if codepoints == pos.character {
                            ControlFlow::Break(byte)
                        } else {
                            codepoints += if char as u32 > 0xffff { 2 } else { 1 };
                            ControlFlow::Continue(byte + char.len_utf8())
                        }
                    })
                    .break_value()
            }
        }
    }

    /// Returns a [`Position`] from a [`Point`].
    #[track_caller]
    fn pos_from_point(&self, strs: &Strs, point: Point) -> Position {
        match self {
            Encoding::Utf8 => {
                lsp_types::Position::new(point.line() as u32, point.byte_col(strs) as u32)
            }
            Encoding::Utf16 => {
                let start = strs.line(point.line()).range().start;
                let codepoints = strs[start..point]
                    .chars()
                    .fold(0, |cp, char| cp + if char as u32 > 0xffff { 2 } else { 1 });

                lsp_types::Position::new(point.line() as u32, codepoints as u32)
            }
        }
    }

    /// Returns a [`Position`] from a [`taken_end`] and [`taken_str`].
    ///
    /// [`taken_end`]: duat_core::buffer::Change::taken_end
    /// [`taken_str`]: duat_core::buffer::Change::taken_str
    #[track_caller]
    fn taken_end_pos(&self, start: Position, taken_end: Point, taken_str: &str) -> Position {
        let start = if start.line as usize == taken_end.line() {
            start.character
        } else {
            0
        };

        let last_line = match taken_str.rsplit_once('\n') {
            Some((_, last_line)) => last_line,
            None => taken_str,
        };

        match self {
            Encoding::Utf8 => {
                lsp_types::Position::new(taken_end.line() as u32, start + last_line.len() as u32)
            }
            Encoding::Utf16 => {
                let character = last_line.chars().fold(start, |cp, char| {
                    cp + if char as u32 > 0xffff { 2 } else { 1 }
                });
                lsp_types::Position::new(taken_end.line() as u32, character)
            }
        }
    }
}
