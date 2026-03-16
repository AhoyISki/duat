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
    notification::{DidChangeWatchedFiles, Notification},
    request::{
        RegisterCapability, Request, SemanticTokensFullDeltaRequest, SemanticTokensFullRequest,
        SemanticTokensRefresh,
    },
};

use crate::{parser::Parser, server::bridge::ServerBridge};

macro_rules! get_method_params {
    ($Request:ty, $params:expr) => {{
        let Some(params) = $params.map(Into::into) else {
            context::warn!(
                "Got [a]{}[] request with [a]no[] parameters",
                <$Request as Request>::METHOD
            );
            return;
        };

        match serde_json::from_value::<<$Request as Request>::Params>(params) {
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
            let params = get_method_params!(RegisterCapability, request.params);

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
                                subs.iter_mut().find(|(b, _)| b.id() == bridge.id())
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
                    method => context::warn!("[a]{method}[] registration not yet handled"),
                }
            }
        }
        SemanticTokensRefresh::METHOD => {
            let bridge = bridge.clone();

            context::queue(move |pa| {
                for handle in context::buffers(pa) {
                    let Some((parser, buffer)) = Parser::write_for(pa, &handle) else {
                        return;
                    };

                    bridge.send_semantic_tokens_request(buffer.path(), &handle, parser);
                }
            })
        }
        method if method.contains("workDoneProgress") => {}
        method => {} // context::warn!("[a]{method}[] request not yet handled"),
    }
}

/// Send a request for the semantic tokens to a given server.
impl ServerBridge {
    pub fn send_semantic_tokens_request(&self, path: PathBuf, handle: &Handle, parser: &Parser) {
        use lsp_types::SemanticTokensServerCapabilities::*;

        let server_id = self.id();
        if !parser
            .servers
            .iter()
            .any(|server| server.bridge.id() == server_id)
        {
            return;
        }

        let work_done_progress_params = WorkDoneProgressParams { work_done_token: None };
        let partial_result_params = PartialResultParams { partial_result_token: None };
        let text_document = TextDocumentIdentifier { uri: crate::path_to_uri(&path).unwrap() };
        let handle = handle.clone();

        if let Some(result_id) = parser.tokens.result_id(self.id()) {
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
                        .find(|server| server.id() == server_id)
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
                                    server_id,
                                    &options.legend,
                                );
                            }
                            TokensDelta(delta) => parser.tokens.apply_delta(
                                buffer.text_mut().parts(),
                                delta,
                                server_id,
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
                        .find(|server| server.id() == server_id)
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
                            server_id,
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
            let changes = event
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
                })
                .collect();

            bridge.send_notification::<DidChangeWatchedFiles>(DidChangeWatchedFilesParams {
                changes,
            });
        }
    })
    .unwrap()
});

struct FileWatchSubscription {
    root: PathBuf,
    glob: Option<GlobMatcher>,
    kind: WatchKind,
}
