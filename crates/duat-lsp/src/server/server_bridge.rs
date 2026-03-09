use std::{
    any::TypeId,
    collections::HashMap,
    io::{BufRead, Write},
    sync::{
        Arc, Mutex,
        mpsc::{self, Sender},
    },
};

use duat_core::{
    context,
    data::Pass,
    process::{PersistentChild, PersistentWriter},
    storage::{
        self,
        bincode::{Decode, Encode},
    },
    text::{Text, txt},
};
use jsonrpc_lite::{Id, JsonRpc};
use lsp_types::{
    InitializedParams,
    notification::{Cancel, Initialized, Notification},
    request::{Initialize, Request},
};
use serde_json::Value;

use crate::config::LanguageServerConfig;

/// Communication abstraction for communication with language servers.
#[derive(Clone)]
pub struct ServerBridge {
    name: String,
    child: Arc<Mutex<PersistentChild>>,
    server_tx: Sender<JsonRpcFn>,
    callbacks: Callbacks,
    initialize_backlog: Arc<Mutex<Option<Vec<JsonRpcFn>>>>,
}

impl ServerBridge {
    /// Returns a new server bridge, there will be one for each server
    /// that is active on each `Buffer`.
    pub fn new(server_name: String, config: &LanguageServerConfig) -> Result<Self, Text> {
        use std::io::ErrorKind;

        let child = {
            let cmd_name = config.command.as_deref().unwrap_or(&server_name);
            let mut command = std::process::Command::new(cmd_name);
            command
                .args(&config.args)
                .envs(&config.envs)
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped());

            duat_core::process::spawn_persistent(&mut command).map_err(|err| match err.kind() {
                ErrorKind::NotFound | ErrorKind::PermissionDenied => {
                    txt!("{err}: [a]{cmd_name}")
                }
                _ => txt!("{err}"),
            })?
        };

        Ok(Self::from_parts(server_name.to_string(), child, true))
    }

    fn from_parts(name: String, mut child: PersistentChild, is_initializing: bool) -> Self {
        let (server_tx, server_rx) = mpsc::channel();

        let mut stdin = child.stdin.take().unwrap();
        let mut stdout = child.get_stdout().unwrap();
        let mut stderr = child.get_stderr().unwrap();

        std::thread::spawn({
            let server_name = name.clone();
            move || {
                let mut line = String::new();
                loop {
                    match stderr.read_line(&mut line) {
                        Ok(0) => break stderr,
                        Ok(_) => {
                            context::debug!("[log.bracket]([]{server_name}[log.bracket])[]{line}");
                            line.clear();
                        }
                        Err(err) => context::error!("{err}"),
                    }
                }
            }
        });

        let server_bridge = Self {
            name,
            child: Arc::new(Mutex::new(child)),
            server_tx,
            callbacks: Callbacks::default(),
            initialize_backlog: Arc::new(Mutex::new(is_initializing.then_some(Vec::new()))),
        };

        std::thread::spawn({
            let callbacks = server_bridge.callbacks.clone();
            move || {
                if let Err(err) = stdout_loop(callbacks, &mut stdout) {
                    context::error!("{err}");
                }
            }
        });

        std::thread::spawn(move || {
            if let Err(err) = stdin_loop(server_rx, &mut stdin) {
                context::error!("{err}");
            }
            stdin
        });

        server_bridge
    }

    /// Sends a request alongside its parameters.
    #[track_caller]
    pub fn send_request<R: Request + 'static>(
        &self,
        params: R::Params,
        callback: impl FnOnce(&mut Pass, R::Result) + Send + 'static,
    ) {
        let message = Box::new(move || {
            let params = serde_json::to_value(params).map_err(|err| {
                std::io::Error::other(format!("Failed to parse parameters: {err}"))
            })?;

            Ok(JsonRpc::request_with_params(
                method_id(R::METHOD).unwrap(),
                R::METHOD,
                params,
            ))
        });

        if TypeId::of::<R>() != TypeId::of::<Initialize>()
            && let Some(backlog) = self.initialize_backlog.lock().unwrap().as_mut()
        {
            backlog.push(message);
        } else {
            self.server_tx.send(message).unwrap();
        }

        let mut callbacks = self.callbacks.lock().unwrap_or_else(|err| err.into_inner());

        let callback = move |pa: &mut Pass, result: Value| {
            callback(pa, serde_json::from_value(result).unwrap())
        };

        if callbacks
            .insert(method_id(R::METHOD).unwrap(), Box::new(callback))
            .is_some()
        {
            self.cancel::<R>()
        }
    }

    pub fn cancel<R: Request>(&self) {
        let message = Box::new(move || {
            Ok(JsonRpc::request_with_params(
                Id::Num(0),
                Cancel::METHOD,
                serde_json::to_value(method_id(R::METHOD).unwrap())?,
            ))
        });

        if let Some(backlog) = self.initialize_backlog.lock().unwrap().as_mut() {
            backlog.push(message);
        } else {
            self.server_tx.send(message).unwrap();
        }
    }

    /// Sends a notification alongside its parameters.
    pub fn send_notification<N: Notification>(&self, params: N::Params) {
        let message = Box::new(move || {
            let params = serde_json::to_value(params).map_err(|err| {
                std::io::Error::other(format!("Failed to parse parameters: {err}"))
            })?;

            Ok(JsonRpc::notification_with_params(N::METHOD, params))
        });

        if let Some(backlog) = self.initialize_backlog.lock().unwrap().as_mut() {
            backlog.push(message);
        } else {
            self.server_tx.send(message).unwrap();
        }
    }

    /// Declares that the server has responded to the initialize
    /// request.
    ///
    /// This will send the backlogged messages to the server.
    pub fn declare_initialized(&self) {
        self.server_tx
            .send(Box::new(move || {
                Ok(JsonRpc::notification_with_params(
                    Initialized::METHOD,
                    serde_json::to_value(InitializedParams {}).unwrap(),
                ))
            }))
            .unwrap();

        let backlog = self.initialize_backlog.lock().unwrap().take();
        for message in backlog.into_iter().flatten() {
            self.server_tx.send(message).unwrap();
        }
    }

    /// Wether the underlying server was already initialized.
    pub fn is_initialized(&self) -> bool {
        self.initialize_backlog.lock().unwrap().is_none()
    }

    /// The name of the server.
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl<Context> Decode<Context> for ServerBridge {
    fn decode<D: storage::bincode::de::Decoder<Context = Context>>(
        decoder: &mut D,
    ) -> Result<Self, storage::bincode::error::DecodeError> {
        let server_name = Decode::decode(decoder)?;
        let child = Decode::decode(decoder)?;
        Ok(Self::from_parts(server_name, child, false))
    }
}

storage::bincode::impl_borrow_decode!(ServerBridge);

impl Encode for ServerBridge {
    fn encode<E: storage::bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), storage::bincode::error::EncodeError> {
        self.name.encode(encoder)?;
        self.child.lock().unwrap().encode(encoder)
    }
}

fn stdin_loop(
    server_rx: mpsc::Receiver<JsonRpcFn>,
    stdin: &mut PersistentWriter,
) -> std::io::Result<()> {
    for jsonrpc_fn in server_rx {
        let content = serde_json::to_string(&jsonrpc_fn()?)?;
        stdin.on_writer(|writer| {
            write!(writer, "Content-Length: {}\r\n\r\n{content}", content.len())?;
            writer.flush()
        })?;
    }

    Ok(())
}

fn stdout_loop(callbacks: Callbacks, stdout: &mut impl BufRead) -> std::io::Result<()> {
    use std::io::Error;
    loop {
        let content_len = {
            let mut content_len = None;
            let mut header = String::new();
            loop {
                header.clear();
                // Exit if the connection was dropped.
                if stdout.read_line(&mut header)? == 0 {
                    return Ok(());
                }
                let header = header.trim();

                if let Some(len) = header.strip_prefix("Content-Length: ") {
                    match len.parse::<usize>() {
                        Ok(len) => content_len = Some(len),
                        Err(err) => {
                            return Err(Error::other(format!(
                                "Failed to parse content length: {err}"
                            )));
                        }
                    }
                } else if header.is_empty() {
                    break;
                }
            }

            match content_len {
                Some(len) => len,
                None => return Err(std::io::Error::other("No Content-Length Provided")),
            }
        };

        let msg = {
            let mut content = vec![0; content_len];
            stdout.read_exact(&mut content)?;
            String::from_utf8(content)
                .map_err(|err| Error::other(format!("String not valid UTF-8: {err}")))?
        };

        match serde_json::from_str::<JsonRpc>(&msg) {
            Ok(content) => {
                let mut callbacks = callbacks.lock().unwrap_or_else(|err| err.into_inner());
                match content {
                    JsonRpc::Request(content) => {
                        context::debug!("Server requested: {content:#?}");
                    }
                    JsonRpc::Notification(content) => {
                        context::debug!("Server notified: {content:#?}");
                    }
                    JsonRpc::Success(_) => {
                        if let Some(id) = content.get_id()
                            && let Some(callback) = callbacks.remove(&id)
                        {
                            let value = content.get_result().unwrap().clone();
                            context::queue(move |pa| callback(pa, value));
                        }
                    }
                    JsonRpc::Error(content) => {
                        context::error!("LSP: {content:#?}");
                        continue;
                    }
                };
            }
            Err(err) => return Err(Error::other(format!("Failed to parse LSP message: {err}"))),
        }
    }
}

fn method_id(method_name: &str) -> Option<Id> {
    use lsp_types::request::*;
    match method_name {
        Initialize::METHOD => Some(Id::Num(0)),
        Shutdown::METHOD => Some(Id::Num(1)),
        ShowMessageRequest::METHOD => Some(Id::Num(2)),
        RegisterCapability::METHOD => Some(Id::Num(3)),
        UnregisterCapability::METHOD => Some(Id::Num(4)),
        WorkspaceSymbolRequest::METHOD => Some(Id::Num(5)),
        WorkspaceSymbolResolve::METHOD => Some(Id::Num(6)),
        ExecuteCommand::METHOD => Some(Id::Num(7)),
        WillSaveWaitUntil::METHOD => Some(Id::Num(8)),
        Completion::METHOD => Some(Id::Num(9)),
        ResolveCompletionItem::METHOD => Some(Id::Num(10)),
        HoverRequest::METHOD => Some(Id::Num(11)),
        SignatureHelpRequest::METHOD => Some(Id::Num(12)),
        GotoDeclaration::METHOD => Some(Id::Num(13)),
        GotoDefinition::METHOD => Some(Id::Num(14)),
        References::METHOD => Some(Id::Num(15)),
        DocumentHighlightRequest::METHOD => Some(Id::Num(16)),
        DocumentSymbolRequest::METHOD => Some(Id::Num(17)),
        CodeActionRequest::METHOD => Some(Id::Num(18)),
        CodeLensRequest::METHOD => Some(Id::Num(19)),
        CodeLensResolve::METHOD => Some(Id::Num(20)),
        DocumentLinkRequest::METHOD => Some(Id::Num(21)),
        DocumentLinkResolve::METHOD => Some(Id::Num(22)),
        ApplyWorkspaceEdit::METHOD => Some(Id::Num(23)),
        RangeFormatting::METHOD => Some(Id::Num(24)),
        OnTypeFormatting::METHOD => Some(Id::Num(25)),
        Formatting::METHOD => Some(Id::Num(26)),
        Rename::METHOD => Some(Id::Num(27)),
        DocumentColor::METHOD => Some(Id::Num(28)),
        ColorPresentationRequest::METHOD => Some(Id::Num(29)),
        FoldingRangeRequest::METHOD => Some(Id::Num(30)),
        PrepareRenameRequest::METHOD => Some(Id::Num(31)),
        GotoImplementation::METHOD => Some(Id::Num(32)),
        GotoTypeDefinition::METHOD => Some(Id::Num(33)),
        SelectionRangeRequest::METHOD => Some(Id::Num(34)),
        WorkspaceFoldersRequest::METHOD => Some(Id::Num(35)),
        WorkspaceConfiguration::METHOD => Some(Id::Num(36)),
        WorkDoneProgressCreate::METHOD => Some(Id::Num(37)),
        CallHierarchyIncomingCalls::METHOD => Some(Id::Num(38)),
        CallHierarchyOutgoingCalls::METHOD => Some(Id::Num(39)),
        MonikerRequest::METHOD => Some(Id::Num(40)),
        LinkedEditingRange::METHOD => Some(Id::Num(41)),
        CallHierarchyPrepare::METHOD => Some(Id::Num(42)),
        TypeHierarchyPrepare::METHOD => Some(Id::Num(43)),
        SemanticTokensFullRequest::METHOD => Some(Id::Num(44)),
        SemanticTokensFullDeltaRequest::METHOD => Some(Id::Num(45)),
        SemanticTokensRangeRequest::METHOD => Some(Id::Num(46)),
        InlayHintRequest::METHOD => Some(Id::Num(47)),
        InlineValueRequest::METHOD => Some(Id::Num(48)),
        DocumentDiagnosticRequest::METHOD => Some(Id::Num(49)),
        WorkspaceDiagnosticRequest::METHOD => Some(Id::Num(50)),
        WorkspaceDiagnosticRefresh::METHOD => Some(Id::Num(51)),
        TypeHierarchySupertypes::METHOD => Some(Id::Num(52)),
        TypeHierarchySubtypes::METHOD => Some(Id::Num(53)),
        WillCreateFiles::METHOD => Some(Id::Num(54)),
        WillRenameFiles::METHOD => Some(Id::Num(55)),
        WillDeleteFiles::METHOD => Some(Id::Num(56)),
        SemanticTokensRefresh::METHOD => Some(Id::Num(57)),
        CodeLensRefresh::METHOD => Some(Id::Num(58)),
        InlayHintRefreshRequest::METHOD => Some(Id::Num(59)),
        InlineValueRefreshRequest::METHOD => Some(Id::Num(60)),
        CodeActionResolveRequest::METHOD => Some(Id::Num(61)),
        InlayHintResolveRequest::METHOD => Some(Id::Num(62)),
        ShowDocument::METHOD => Some(Id::Num(63)),
        _ => None,
    }
}

type JsonRpcFn = Box<dyn FnOnce() -> std::io::Result<JsonRpc> + Send>;
type Callbacks = Arc<Mutex<HashMap<Id, Box<dyn FnOnce(&mut Pass, Value) + Send + 'static>>>>;
