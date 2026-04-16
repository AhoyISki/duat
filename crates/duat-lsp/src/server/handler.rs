use std::{
    path::PathBuf,
    sync::{LazyLock, Mutex},
};

use duat_core::{
    context::{self, Handle},
    notify::Watcher,
};
use globset::{Glob, GlobMatcher};
use lsp_types::{
    DidChangeWatchedFilesParams, DidChangeWatchedFilesRegistrationOptions, FileChangeType,
    FileEvent, GlobPattern, OneOf, PartialResultParams, SemanticTokensDeltaParams,
    SemanticTokensParams, SemanticTokensResult, TextDocumentIdentifier, WatchKind,
    WorkDoneProgressParams,
    notification::{DidChangeWatchedFiles, Notification, PublishDiagnostics},
    request::{
        RegisterCapability, Request, SemanticTokensFullDeltaRequest, SemanticTokensFullRequest,
    },
};
use serde_json::Value;

use crate::{
    parser::{self, Parser},
    server::bridge::ServerBridge,
};

macro_rules! get_method_params {
    ($Type:ty, $params:expr, $Trait:ident) => {{
        let Some(params) = $params.map(Into::into) else {
            context::warn!(
                "Got [a]{}[] request with [a]no[] parameters",
                <$Type as $Trait>::METHOD
            );
            return;
        };

        match serde_json::from_value::<<$Type as $Trait>::Params>(params) {
            Ok(params) => params,
            Err(err) => {
                context::warn!("LSP: {err}");
                return;
            }
        }
    }};
}

macro_rules! deserialize_opt {
    ($Type:ty, $value:expr) => {{
        let Some(value) = $value else {
            context::warn!("Expected [a]{}[], got [a]None", stringify!($Type));
            return;
        };

        match serde_json::from_value::<$Type>(value) {
            Ok(params) => params,
            Err(err) => {
                context::warn!("LSP: {err}");
                return;
            }
        }
    }};
}

pub fn handle_request(bridge: &ServerBridge, request: jsonrpc_lite::Request) {
    match request.method.as_str() {
        RegisterCapability::METHOD => {
            let params = get_method_params!(RegisterCapability, request.params, Request);

            for registration in params.registrations {
                match registration.method.as_str() {
                    DidChangeWatchedFiles::METHOD => {
                        let options = deserialize_opt!(
                            DidChangeWatchedFilesRegistrationOptions,
                            registration.register_options
                        );

                        let mut roots = Vec::new();
                        let mut subs = SUBSCRIPTIONS.lock().unwrap();

                        for watcher in options.watchers {
                            let (glob, root) = match watcher.glob_pattern {
                                GlobPattern::String(path) => (None, PathBuf::from(path)),
                                GlobPattern::Relative(relative) => (
                                    Some(relative.pattern),
                                    crate::uri_to_path(match relative.base_uri {
                                        OneOf::Left(folder) => folder.uri,
                                        OneOf::Right(uri) => uri,
                                    }),
                                ),
                            };

                            roots.push(root.clone());

                            let subscription = FileWatchSubscription {
                                root,
                                glob: match glob.map(|glob| Glob::new(&glob)) {
                                    None => None,
                                    Some(Ok(glob)) => Some(glob.compile_matcher()),
                                    Some(Err(err)) => {
                                        context::error!("Invalid glob: {err}");
                                        continue;
                                    }
                                },
                                kind: watcher.kind.unwrap_or(WatchKind::all()),
                            };

                            if let Some((_, subs)) =
                                subs.iter_mut().find(|(b, _)| b.ns() == bridge.ns())
                            {
                                subs.push(subscription);
                            } else {
                                subs.push((bridge.clone(), vec![subscription]));
                            };
                        }
                        drop(subs);

                        for root in roots {
                            if let Ok(true) = root.try_exists()
                                && let Err(err) = WATCHER.watch_recursive(&root)
                            {
                                context::error!("Watcher: [a]{err}[] on {root:?}");
                                continue;
                            }
                        }
                    }
                    _method => {} // context::warn!("[a]{method}[] registration not yet handled"),
                }
            }

            bridge.send_success(request.id, Value::Null);
        }
        _ => bridge.send_success(request.id, Value::Null),
    }
}

pub fn handle_notification(bridge: &ServerBridge, notification: jsonrpc_lite::Notification) {
    match notification.method.as_str() {
        PublishDiagnostics::METHOD => {
            let Some(encoding) = bridge.encoding.get().copied() else {
                return;
            };

            let params = get_method_params!(PublishDiagnostics, notification.params, Notification);
            let ns = bridge.ns();

            context::queue(move |pa| {
                let (uri, list) = (params.uri, params.diagnostics);
                parser::diagnostics::add(pa, ns, uri, list, encoding)
            });
        }
        "$/progress" => {}
        _method => {} // context::warn!("[a]{method}[] request not yet handled"),
    }
}

/// Send a request for the semantic tokens to a given server.
impl ServerBridge {
    /// Sends a semantic token request.
    pub fn send_semantic_tokens_request(&self, buffer: &Handle, parser: &Parser) {
        use lsp_types::SemanticTokensServerCapabilities::*;

        let server_ns = self.ns();
        if !parser
            .servers
            .iter()
            .any(|server| server.bridge.ns() == server_ns)
        {
            return;
        }

        let work_done_progress_params = WorkDoneProgressParams { work_done_token: None };
        let partial_result_params = PartialResultParams { partial_result_token: None };
        let text_document = TextDocumentIdentifier { uri: parser.uri().clone() };
        let handle = buffer.clone();

        if let Some(result_id) = parser.tokens.result_id(self.ns()) {
            self.send_request::<SemanticTokensFullDeltaRequest>(
                SemanticTokensDeltaParams {
                    work_done_progress_params,
                    partial_result_params,
                    text_document,
                    previous_result_id: result_id,
                },
                move |pa, result| {
                    let Some(result) = result else {
                        return;
                    };

                    let (parser, buffer) = Parser::write_for(pa, &handle).unwrap();
                    if let Some(server) = parser
                        .servers
                        .iter()
                        .find(|server| server.ns() == server_ns)
                        && let Some(cap) = server.capabilities()
                        && let Some(options) = &cap.semantic_tokens_provider
                    {
                        use lsp_types::SemanticTokensFullDeltaResult::*;

                        match result {
                            Tokens(tokens) => {
                                let options = match options {
                                    SemanticTokensOptions(options) => options,
                                    SemanticTokensRegistrationOptions(options) => {
                                        &options.semantic_tokens_options
                                    }
                                };

                                parser.tokens.apply_full(
                                    buffer.text_mut().parts(),
                                    tokens,
                                    server_ns,
                                    &options.legend,
                                );
                            }
                            TokensDelta(delta) => parser.tokens.apply_delta(
                                buffer.text_mut().parts(),
                                delta,
                                server_ns,
                            ),
                            PartialTokensDelta { .. } => todo!(),
                        }
                    }
                },
            );
        } else {
            self.send_request::<SemanticTokensFullRequest>(
                SemanticTokensParams {
                    work_done_progress_params,
                    partial_result_params,
                    text_document,
                },
                move |pa, result| {
                    let Some(SemanticTokensResult::Tokens(tokens)) = result else {
                        return;
                    };

                    let (parser, buffer) = Parser::write_for(pa, &handle).unwrap();
                    if let Some(server) = parser
                        .servers
                        .iter()
                        .find(|server| server.ns() == server_ns)
                        && let Some(cap) = server.capabilities()
                        && let Some(options) = &cap.semantic_tokens_provider
                    {
                        let options = match options {
                            SemanticTokensOptions(options) => options,
                            SemanticTokensRegistrationOptions(options) => {
                                &options.semantic_tokens_options
                            }
                        };

                        parser.tokens.apply_full(
                            buffer.text_mut().parts(),
                            tokens,
                            server_ns,
                            &options.legend,
                        );
                    }
                },
            );
        }
    }
}

static SUBSCRIPTIONS: Mutex<Vec<(ServerBridge, Vec<FileWatchSubscription>)>> =
    Mutex::new(Vec::new());

static WATCHER: LazyLock<Watcher> = LazyLock::new(|| {
    Watcher::new(|event, _| {
        use duat_core::notify::event::EventKind;
        let Ok(event) = event else {
            return;
        };

        let subs = SUBSCRIPTIONS.lock().unwrap();

        let (watch_kind, change_type) = match event.kind {
            EventKind::Create(_) => (WatchKind::Create, FileChangeType::CREATED),
            EventKind::Modify(_) => (WatchKind::Change, FileChangeType::CHANGED),
            EventKind::Remove(_) => (WatchKind::Delete, FileChangeType::DELETED),
            EventKind::Any | EventKind::Access(_) | EventKind::Other => return,
        };

        for (bridge, subs) in subs.iter() {
            let changes = Vec::from_iter(
                event
                    .paths
                    .iter()
                    .filter(|path| {
                        subs.iter().any(|sub| {
                            path.starts_with(&sub.root)
                                && sub.glob.as_ref().is_none_or(|glob| glob.is_match(path))
                                && sub.kind.contains(watch_kind)
                        })
                    })
                    .map(|path| FileEvent {
                        uri: crate::path_to_uri(path).unwrap(),
                        typ: change_type,
                    }),
            );

            if !changes.is_empty() {
                bridge.send_notification::<DidChangeWatchedFiles>(DidChangeWatchedFilesParams {
                    changes,
                });
            }
        }
    })
    .unwrap()
});

struct FileWatchSubscription {
    root: PathBuf,
    glob: Option<GlobMatcher>,
    kind: WatchKind,
}
