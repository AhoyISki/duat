use std::{
    cell::LazyCell,
    collections::HashMap,
    ops::ControlFlow,
    path::{Path, PathBuf},
};

use duat_base::{BaseBuffer, widgets::FilePlace};
use duat_core::{
    buffer::Buffer,
    cmd, context,
    data::Pass,
    form::{self, Form},
    mode::{self, KeyEvent, Mode, User, event},
    text::{Point, Strs, Text},
    txt,
};
use lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, HoverParams, OneOf, Position, ReferenceContext,
    ReferenceParams, ServerCapabilities, TextDocumentIdentifier, TextDocumentPositionParams,
    TypeDefinitionProviderCapability, Uri, WorkDoneProgressParams,
    request::{GotoDefinition, GotoTypeDefinition, HoverRequest, References},
};

pub use crate::parser::{LspBuffer, LspCompletions};

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
        form::set_weak("lsp.info", Form::mimic("default.info"));
        form::set_weak("lsp.hint", Form::mimic("default.info"));
        form::set_weak("lsp.warn", Form::mimic("default.warn"));
        form::set_weak("lsp.error", Form::mimic("default.error"));
        form::set_weak("lsp.tt.info", Form::mimic("accent.info"));
        form::set_weak("lsp.tt.hint", Form::mimic("accent.info"));
        form::set_weak("lsp.tt.warn", Form::mimic("accent.warn"));
        form::set_weak("lsp.tt.error", Form::mimic("accent.error"));

        form::set_weak("completion.lsp.detail", Form::mimic("comment"));
        form::set_weak("completion.lsp.kind", Form::mimic("function"));

        form::set_weak("picker.lsp.buffer", Form::mimic("buffer"));
        form::set_weak("picker.lsp.line", Form::mimic("comment"));

        parser::setup_hooks();

        mode::map::<User>("l", |pa: &mut _| mode::set(pa, Lsp)).doc(txt!("Enter [mode]Lsp[] mode"));
    }
}

/// The mode for lsp actions.
pub struct Lsp;

impl Mode for Lsp {
    fn bindings() -> duat_core::mode::Bindings {
        mode::bindings!(match _ {
            event!('h') => txt!("Show hover info"),
            event!('f') => txt!("Format the buffer"),
            event!('d') => txt!("Go to definition"),
            event!('y') => txt!("Go to type definition"),
            event!('r') => txt!("Go to references"),
        })
    }

    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent) {
        let buffer = context::current_buffer(pa);

        match key_event {
            event!('f') => buffer.lsp_format(pa, None),
            event!('h') if let Some(servers) = server::get_servers_for(buffer.read(pa)) => {
                for server in servers {
                    let Some(capabilities) = server.capabilities() else {
                        continue;
                    };
                    let encoding = Encoding::new(capabilities);
                    if capabilities.hover_provider.is_none() {
                        continue;
                    }

                    let buf = buffer.read(pa);
                    let uri = path_to_uri(&buf.path()).unwrap();

                    server.send_request::<HoverRequest>(
                        HoverParams {
                            text_document_position_params: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri },
                                position: encoding
                                    .pos_from_point(buf.text(), buf.text().main_sel().cursor()),
                            },
                            work_done_progress_params: WorkDoneProgressParams::default(),
                        },
                        |_, result| {
                            duat_core::debug!("{result:#?}");
                        },
                    );
                }

                buffer.hover_gutter_entries_on(pa, buffer.selections(pa).main().cursor())
            }
            event!('d') if let Some(servers) = server::get_servers_for(buffer.read(pa)) => {
                if let Some((server, capabilities)) = servers.iter().find_map(|server| {
                    Some(server).zip(server.capabilities()).filter(|(_, cap)| {
                        matches!(
                            &cap.definition_provider,
                            Some(OneOf::Left(true) | OneOf::Right(..))
                        )
                    })
                }) {
                    let encoding = Encoding::new(capabilities);
                    let buf = buffer.read(pa);
                    let uri = path_to_uri(&buf.path()).unwrap();

                    server.send_request::<GotoDefinition>(
                        GotoDefinitionParams {
                            partial_result_params: lsp_types::PartialResultParams::default(),
                            text_document_position_params: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri },
                                position: encoding
                                    .pos_from_point(buf.text(), buf.text().main_sel().cursor()),
                            },
                            work_done_progress_params: WorkDoneProgressParams::default(),
                        },
                        move |pa, result| goto_definitions(result, pa, encoding),
                    );
                }
            }
            event!('y') if let Some(servers) = server::get_servers_for(buffer.read(pa)) => {
                if let Some((server, capabilities)) = servers.iter().find_map(|server| {
                    Some(server).zip(server.capabilities()).filter(|(_, cap)| {
                        matches!(
                            &cap.type_definition_provider,
                            Some(
                                TypeDefinitionProviderCapability::Simple(true)
                                    | TypeDefinitionProviderCapability::Options(..)
                            )
                        )
                    })
                }) {
                    let encoding = Encoding::new(capabilities);
                    let buf = buffer.read(pa);
                    let uri = path_to_uri(&buf.path()).unwrap();

                    server.send_request::<GotoTypeDefinition>(
                        GotoDefinitionParams {
                            partial_result_params: lsp_types::PartialResultParams::default(),
                            text_document_position_params: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri },
                                position: encoding
                                    .pos_from_point(buf.text(), buf.text().main_sel().cursor()),
                            },
                            work_done_progress_params: WorkDoneProgressParams::default(),
                        },
                        move |pa, result| goto_definitions(result, pa, encoding),
                    );
                }
            }
            event!('r') if let Some(servers) = server::get_servers_for(buffer.read(pa)) => {
                if let Some((server, capabilities)) = servers.iter().find_map(|server| {
                    Some(server).zip(server.capabilities()).filter(|(_, cap)| {
                        matches!(
                            &cap.references_provider,
                            Some(OneOf::Left(true) | OneOf::Right(..))
                        )
                    })
                }) {
                    let encoding = Encoding::new(capabilities);
                    let buf = buffer.read(pa);
                    let uri = path_to_uri(&buf.path()).unwrap();

                    server.send_request::<References>(
                        ReferenceParams {
                            partial_result_params: lsp_types::PartialResultParams::default(),
                            text_document_position: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri },
                                position: encoding
                                    .pos_from_point(buf.text(), buf.text().main_sel().cursor()),
                            },
                            work_done_progress_params: WorkDoneProgressParams::default(),
                            context: ReferenceContext { include_declaration: true },
                        },
                        move |pa, result| {
                            let Some(result) = result else {
                                return;
                            };

                            spawn_picker(
                                pa,
                                encoding,
                                result
                                    .into_iter()
                                    .map(|location| (location.uri, location.range)),
                            );
                        },
                    );
                }
            }
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
                    encoded.push_str(&format!("{:02X}", s));
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
#[derive(Debug, Default, Clone, Copy)]
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
    fn byte_from_pos(&self, strs: &Strs, pos: Position) -> Option<usize> {
        match self {
            Encoding::Utf8 => {
                let byte = strs.get_point_at_coords(pos.line as usize, 0)?.byte();
                let line = strs.line(pos.line as usize);
                (line.len() >= pos.character as usize).then_some(byte + pos.character as usize)
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

    /// Returns a codepoint index from a [`Position`] in an `&str`
    fn byte_from_pos_str<'s>(&self, str: &'s str, pos: Position) -> Option<(&'s str, usize)> {
        let mut byte = 0;
        let line = str
            .split_inclusive('\n')
            .enumerate()
            .find_map(|(i, line)| {
                if i as u32 == pos.line {
                    Some(line)
                } else {
                    byte += line.len();
                    None
                }
            })?;

        match self {
            Encoding::Utf8 => (line.len() >= pos.character as usize)
                .then_some((line, byte + pos.character as usize)),
            Encoding::Utf16 => {
                let mut codepoints = 0;
                line.chars()
                    .try_fold(byte, |byte, char| {
                        if codepoints == pos.character {
                            ControlFlow::Break((line, byte))
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

fn goto_definitions(locations: Option<GotoDefinitionResponse>, pa: &mut Pass, encoding: Encoding) {
    let Some(locations) = locations else {
        return;
    };

    let locations = match locations {
        GotoDefinitionResponse::Scalar(l) => GotoDefinitionResponse::Scalar(l),
        GotoDefinitionResponse::Array(mut ls) if ls.len() == 1 => {
            GotoDefinitionResponse::Scalar(ls.remove(0))
        }
        GotoDefinitionResponse::Array(ls) => GotoDefinitionResponse::Array(ls),
        GotoDefinitionResponse::Link(ls) => GotoDefinitionResponse::Link(ls),
    };

    match locations {
        GotoDefinitionResponse::Scalar(location) => {
            let path = uri_to_path(location.uri);

            if cmd::call(pa, format!("open {}", path.to_string_lossy())).is_ok() {
                let buffer = context::current_buffer(pa);

                let start = encoding.byte_from_pos(buffer.text(pa), location.range.start);
                let end = encoding.byte_from_pos(buffer.text(pa), location.range.end);

                if let (Some(start), Some(end)) = (start, end) {
                    buffer.edit_main(pa, |mut s| s.move_to(start..end));
                }
            };
        }
        GotoDefinitionResponse::Array(locations) => {
            spawn_picker(
                pa,
                encoding,
                locations
                    .into_iter()
                    .map(|location| (location.uri, location.range)),
            );
        }
        GotoDefinitionResponse::Link(links) => {
            spawn_picker(
                pa,
                encoding,
                links
                    .into_iter()
                    .map(|link| (link.target_uri, link.target_selection_range)),
            );
        }
    };
}

fn spawn_picker(
    pa: &mut Pass,
    encoding: Encoding,
    iter: impl IntoIterator<Item = (Uri, lsp_types::Range)>,
) {
    let mut map = LazyCell::new(HashMap::new);

    let jumps = iter.into_iter().filter_map(|(uri, range)| {
        let path = uri_to_path(uri);
        let file = map
            .entry(path.clone())
            .or_insert_with_key(|path| std::fs::read_to_string(path))
            .as_deref()
            .ok()?;

        let (line, start) = encoding.byte_from_pos_str(file, range.start)?;
        let (_, end) = encoding.byte_from_pos_str(file, range.end)?;

        let line = line.trim_end();

        Some((
            txt!("[picker.lsp.line]{line}: [picker.lsp.buffer]{path}"),
            FilePlace::new(path, start..end),
        ))
    });

    duat_base::widgets::Picker::spawn_jumps(pa, jumps);
}
