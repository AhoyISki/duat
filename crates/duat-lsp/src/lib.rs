use std::{
    cell::LazyCell,
    collections::HashMap,
    ops::ControlFlow,
    path::{Path, PathBuf},
};

use duat_base::{modes::Prompt, widgets::FilePlace};
use duat_core::{
    buffer::Buffer, context,
    data::Pass,
    form::{self, Form},
    mode::{self, KeyEvent, Mode, User, event},
    text::{Point, RegexHaystack, Strs, Text, TextMut},
    txt,
    ui::Widget,
};
use lsp_types::{
    CodeActionContext, CodeActionParams, CodeActionProviderCapability, CodeActionTriggerKind,
    DocumentChangeOperation, DocumentChanges, GotoDefinitionParams, GotoDefinitionResponse,
    HoverParams, HoverProviderCapability, OneOf, OptionalVersionedTextDocumentIdentifier,
    PartialResultParams, Position, ReferenceContext, ReferenceParams, ResourceOp,
    ServerCapabilities, TextDocumentEdit, TextDocumentIdentifier, TextDocumentPositionParams,
    TextEdit, TypeDefinitionProviderCapability, Uri, WorkDoneProgressParams, WorkspaceEdit,
    request::{CodeActionRequest, GotoDefinition, GotoTypeDefinition, HoverRequest, References},
};

pub use crate::parser::{LspBuffer, LspCompletions};
use crate::{config::LanguageServerConfig, parser::diagnostics};

mod config;
mod hover;
mod modes;
mod parser;
mod server;

/// Options for `duat-lsp`.
#[derive(Default)]
pub struct DuatLspOpts {
    lang_configs: HashMap<&'static str, HashMap<String, LanguageServerConfig>>,
}

impl DuatLspOpts {
    /// Sets the language server configuration for a given language.
    pub fn set_for_language(&mut self, lang: &'static str, config: &str) {
        let server_configs = duat_core::try_or_log_err! {
            toml::from_str::<HashMap<_, _>>(config)?
        };

        self.lang_configs.insert(lang, server_configs);
    }
}

/// Sets the [`DuatLspOpts`].
pub fn set_opts(lsp_opts: DuatLspOpts) {
    for (lang, server_configs) in lsp_opts.lang_configs {
        config::set_for(lang, server_configs)
    }
}

/// The [`Plugin`] for `duat-lsp`
///
/// [`Plugin`]: https://docs.rs/duat/latest/trait.Plugin.html
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
        form::set_weak("completion.lsp.command", Form::mimic("function"));
        form::set_weak("completion.lsp.detail.source", Form::mimic("comment"));

        form::set_weak("picker.lsp.buffer", Form::mimic("buffer"));
        form::set_weak("picker.lsp.line", Form::mimic("comment"));

        form::set_weak("rename.error", Form::mimic("default.error"));

        parser::setup_hooks();
        modes::setup_hooks();

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
            event!('a') => txt!("List code actions"),
            event!('R') => txt!("Rename symbol"),
        })
    }

    fn send_key(&mut self, pa: &mut Pass, key_event: KeyEvent) {
        let buffer = context::current_buffer(pa);

        let Some(servers) = server::get_servers_for(buffer.read(pa)) else {
            mode::reset::<Buffer>(pa);
            return;
        };

        let buf = buffer.read(pa);
        let uri = path_to_uri(&buf.path()).unwrap();

        macro_rules! get {
            ($get_field:expr, $pattern:pat) => {{
                let pred: for<'a> fn(&'a ServerCapabilities) -> &'a Option<_> = $get_field;
                servers.iter().find_map(|server| {
                    Some(server)
                        .zip(server.capabilities())
                        .and_then(|(server, cap)| {
                            if let Some(field) = pred(cap).as_ref()
                                && matches!(field, $pattern)
                            {
                                Some((server, Encoding::new(cap), cap, field))
                            } else {
                                None
                            }
                        })
                })
            }};
        }

        match key_event {
            event!('f') => buffer.lsp_format(pa, None),
            event!('h')
                if let Some((server, encoding, ..)) = get!(
                    |cap| &cap.hover_provider,
                    HoverProviderCapability::Simple(true) | HoverProviderCapability::Options(..)
                ) =>
            {
                server.send_request::<HoverRequest>(
                    HoverParams {
                        text_document_position_params: doc_pos(buf, uri.clone(), encoding),
                        work_done_progress_params: WorkDoneProgressParams::default(),
                    },
                    move |pa, result| {
                        if let Some(result) = result {
                            hover::hover(pa, encoding, result);
                        }
                    },
                );
            }
            event!('d')
                if let Some((server, encoding, ..)) = get!(
                    |cap| &cap.definition_provider,
                    OneOf::Left(true) | OneOf::Right(..)
                ) =>
            {
                server.send_request::<GotoDefinition>(
                    GotoDefinitionParams {
                        partial_result_params: lsp_types::PartialResultParams::default(),
                        text_document_position_params: doc_pos(buf, uri, encoding),
                        work_done_progress_params: WorkDoneProgressParams::default(),
                    },
                    move |pa, result| goto_definitions(result, pa, encoding),
                );
            }
            event!('y')
                if let Some((server, encoding, ..)) = get!(
                    |cap| &cap.type_definition_provider,
                    TypeDefinitionProviderCapability::Simple(true)
                        | TypeDefinitionProviderCapability::Options(..)
                ) =>
            {
                server.send_request::<GotoTypeDefinition>(
                    GotoDefinitionParams {
                        partial_result_params: lsp_types::PartialResultParams::default(),
                        text_document_position_params: doc_pos(buf, uri, encoding),
                        work_done_progress_params: WorkDoneProgressParams::default(),
                    },
                    move |pa, result| goto_definitions(result, pa, encoding),
                );
            }
            event!('r')
                if let Some((server, encoding, ..)) = get!(
                    |cap| &cap.references_provider,
                    OneOf::Left(true) | OneOf::Right(..)
                ) =>
            {
                server.send_request::<References>(
                    ReferenceParams {
                        partial_result_params: lsp_types::PartialResultParams::default(),
                        text_document_position: doc_pos(buf, uri, encoding),
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
            event!('a') => {
                for server in servers {
                    let Some(capabilities) = &server.capabilities() else {
                        continue;
                    };

                    let encoding = Encoding::new(capabilities);
                    let text = buf.text();
                    let start = encoding.pos_from_point(text, buf.text().main_sel().start_point());
                    let end = encoding.pos_from_point(text, buf.text().main_sel().end_point(text));

                    let can_resolve = match &capabilities.code_action_provider {
                        Some(CodeActionProviderCapability::Simple(false)) | None => continue,
                        Some(CodeActionProviderCapability::Options(options)) => {
                            options.resolve_provider == Some(true)
                        }
                        Some(CodeActionProviderCapability::Simple(true)) => false,
                    };

                    server.send_request::<CodeActionRequest>(
                        CodeActionParams {
                            text_document: TextDocumentIdentifier { uri: uri.clone() },
                            range: lsp_types::Range { start, end },
                            context: CodeActionContext {
                                diagnostics: diagnostics::get_for(buf, &server),
                                only: None,
                                trigger_kind: Some(CodeActionTriggerKind::INVOKED),
                            },
                            work_done_progress_params: WorkDoneProgressParams::default(),
                            partial_result_params: PartialResultParams::default(),
                        },
                        {
                            let server = server.clone();
                            move |pa, result| {
                                if let Some(result) = result
                                    && !result.is_empty()
                                {
                                    modes::DoCodeAction::set_or_add(
                                        pa,
                                        server,
                                        can_resolve,
                                        encoding,
                                        result,
                                    );
                                }
                            }
                        },
                    );
                }
            }
            event!('R')
                if let Some((server, encoding, ..)) = get!(
                    |cap| &cap.rename_provider,
                    OneOf::Left(true) | OneOf::Right(..)
                ) =>
            {
                let popts = buf.print_opts();
                let doc_pos = doc_pos(buf, uri, encoding);

                let placeholder = {
                    let text = buf.text();
                    let main = text.main_sel().cursor().byte();

                    let bef_regex = format!("{}*\\z", popts.word_chars_regex());
                    let bef = text.search(bef_regex).range(..main).next_back().unwrap();

                    let aft_regex = format!("\\A{}*", popts.word_chars_regex());
                    let aft = text.search(aft_regex).range(main..).next().unwrap();

                    text[bef.start..aft.end].to_string()
                };

                if !placeholder.is_empty() {
                    mode::set(
                        pa,
                        Prompt::new_with(
                            modes::RenameSymbol::new(
                                server.clone(),
                                doc_pos,
                                popts.word_chars_regex(),
                                encoding,
                            ),
                            placeholder,
                        ),
                    );
                    return;
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
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
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
            spawn_picker(pa, encoding, [(location.uri, location.range)]);
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

fn doc_pos(buf: &Buffer, uri: Uri, encoding: Encoding) -> TextDocumentPositionParams {
    TextDocumentPositionParams {
        text_document: TextDocumentIdentifier { uri },
        position: encoding.pos_from_point(buf.text(), buf.text().main_sel().cursor()),
    }
}

fn handle_workspace_edit(
    pa: &mut Pass,
    edit: WorkspaceEdit,
    encoding: Encoding,
) -> Result<(), Text> {
    let apply_edit = |pa: &mut Pass, mut edit: TextDocumentEdit| {
        let path = uri_to_path(edit.text_document.uri.clone());
        let buffer = context::buffer_from_path(pa, &path);

        if let Some(buffer) = buffer {
            edit.edits.sort_by_key(|lhs| match lhs {
                OneOf::Left(edit) => edit.range.start,
                OneOf::Right(annotated_edit) => annotated_edit.text_edit.range.start,
            });

            let mut text = buffer.text_mut(pa);
            for edit in edit.edits.into_iter().rev() {
                apply_edit(
                    &mut text,
                    match edit {
                        OneOf::Left(edit) => edit,
                        OneOf::Right(annotated_edit) => annotated_edit.text_edit,
                    },
                    encoding,
                );
            }

            _ = buffer.save(pa);
        } else {
            let mut file = std::fs::read_to_string(&path)?;

            for edit in edit.edits.into_iter().rev() {
                let edit = match edit {
                    OneOf::Left(edit) => edit,
                    OneOf::Right(annotated_edit) => annotated_edit.text_edit,
                };

                let start = encoding.byte_from_pos_str(&file, edit.range.start);
                let end = encoding.byte_from_pos_str(&file, edit.range.end);

                if let (Some((_, start)), Some((_, end))) = (start, end) {
                    file.replace_range(start..end, &edit.new_text);
                }
            }

            std::fs::write(&path, file)?;
        }

        Result::<(), Text>::Ok(())
    };

    if let Some(changes) = edit.document_changes {
        match changes {
            DocumentChanges::Edits(edits) => {
                for edit in edits {
                    apply_edit(pa, edit)?;
                }
            }
            DocumentChanges::Operations(ops) => {
                for op in ops {
                    match op {
                        DocumentChangeOperation::Op(ResourceOp::Create(create)) => {
                            let path = uri_to_path(create.uri);
                            if let Some(opts) = create.options
                                && opts.overwrite != Some(true)
                                && let Some(true) = opts.ignore_if_exists
                            {
                                if std::fs::exists(&path)? {
                                    context::error!("Path already exists: [buffer]{path}");
                                } else {
                                    std::fs::write(&path, "")?;
                                }
                            } else {
                                std::fs::write(&path, "")?;
                            }
                        }
                        DocumentChangeOperation::Op(ResourceOp::Rename(rename)) => {
                            let old_path = uri_to_path(rename.old_uri);
                            let new_path = uri_to_path(rename.new_uri);
                            if let Some(opts) = rename.options
                                && opts.overwrite != Some(true)
                                && let Some(true) = opts.ignore_if_exists
                            {
                                if std::fs::exists(&new_path)? {
                                    return Err(txt!("Path already exists: [buffer]{new_path}"));
                                } else {
                                    std::fs::rename(&old_path, &new_path)?;
                                }
                            } else {
                                std::fs::rename(&old_path, &new_path)?;
                            }

                            if let Some(buffer) = context::buffer_from_path(pa, &old_path) {
                                buffer.set_path(pa, &new_path);
                            }
                        }
                        DocumentChangeOperation::Op(ResourceOp::Delete(delete)) => {
                            let path = uri_to_path(delete.uri);
                            if path.is_dir() {
                                if let Some(opts) = delete.options {
                                    if opts.recursive == Some(false)
                                        && std::fs::read_dir(&path)?.next().is_none()
                                    {
                                        return Err(txt!("Directory is not empty: [buffer]{path}"));
                                    } else if std::fs::exists(&path)? {
                                        std::fs::remove_dir_all(path)?;
                                    }
                                } else {
                                    std::fs::remove_dir_all(path)?;
                                }
                            } else if std::fs::exists(&path)? {
                                std::fs::remove_file(path)?;
                            }
                        }
                        DocumentChangeOperation::Edit(edit) => {
                            apply_edit(pa, edit)?;
                        }
                    }
                }
            }
        }
    } else if let Some(edits) = edit.changes {
        for (uri, edits) in edits {
            let edit = TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier { uri, version: None },
                edits: edits.into_iter().map(OneOf::Left).collect(),
            };

            apply_edit(pa, edit)?;
        }
    } else {
        return Ok(());
    };

    Ok(())
}

fn apply_edit(text: &mut TextMut, edit: TextEdit, encoding: Encoding) {
    let range = edit.range;
    let start = encoding.byte_from_pos(text, range.start);
    let end = encoding.byte_from_pos(text, range.end);
    if let (Some(start), Some(end)) = (start, end) {
        text.replace_range(start..end, edit.new_text);
    }
}

