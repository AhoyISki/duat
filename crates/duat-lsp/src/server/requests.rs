use std::{
    path::PathBuf,
    sync::{LazyLock, Mutex},
};

use duat_core::{context, notify::Watcher};
use globset::{Glob, GlobMatcher};
use lsp_types::{
    DidChangeWatchedFilesParams, DidChangeWatchedFilesRegistrationOptions, FileChangeType,
    FileEvent, GlobPattern, OneOf, PartialResultParams, SemanticTokensParams, SemanticTokensResult,
    TextDocumentIdentifier, WatchKind, WorkDoneProgressParams,
    notification::{DidChangeWatchedFiles, Notification},
    request::{RegisterCapability, Request, SemanticTokensFullRequest, SemanticTokensRefresh},
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
                        continue;
                    };

                    if !parser
                        .servers()
                        .iter()
                        .any(|server| server.bridge.id() == bridge.id())
                    {
                        continue;
                    }

                    bridge.send_request::<SemanticTokensFullRequest>(
                        SemanticTokensParams {
                            work_done_progress_params: WorkDoneProgressParams {
                                work_done_token: None,
                            },
                            partial_result_params: PartialResultParams {
                                partial_result_token: None,
                            },
                            text_document: TextDocumentIdentifier {
                                uri: crate::path_to_uri(&buffer.path()).unwrap(),
                            },
                        },
                        |pa, result| {
                            let Some(result) = result else {
                                return;
                            };

                            match result {
                                SemanticTokensResult::Tokens(semantic_tokens) => {}
                                SemanticTokensResult::Partial(semantic_tokens_partial_result) => {
                                    todo!()
                                }
                            }
                        },
                    );
                }
            })
        }
        method => context::warn!("[a]{method}[] request not yet handled"),
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
