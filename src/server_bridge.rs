use std::{
    collections::HashMap,
    io::{BufRead, BufReader, BufWriter, Write},
    sync::{
        Arc, Mutex,
        mpsc::{self, Sender},
    },
};

use duat_core::{
    context,
    data::Pass,
    hook::{self, ConfigUnloaded},
    text::{Text, txt},
};
use jsonrpc_lite::{Id, JsonRpc};
use lsp_types::{
    DidOpenTextDocumentParams, InitializeParams, InitializedParams, TextDocumentItem,
    notification::{Cancel, DidOpenTextDocument, Exit, Initialized, Notification},
    request::{Initialize, Request, Shutdown},
};
use serde_json::Value;

/// Communication abstraction for communication with language servers.
#[derive(Clone)]
pub struct ServerBridge {
    server_tx: Sender<ServerMessage>,
    callbacks: Callbacks,
}

impl ServerBridge {
    /// Returns a new server bridge, there will be one for each server
    /// that is active on each `Buffer`.
    pub fn new(
        server_name: &str,
        cmd: &str,
        args: &[&str],
        env: &[(&str, &str)],
    ) -> Result<Self, Text> {
        use std::io::ErrorKind;

        let mut child = std::process::Command::new(cmd)
            .args(args)
            .envs(env.iter().copied())
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .map_err(|err| match err.kind() {
                ErrorKind::NotFound | ErrorKind::PermissionDenied => {
                    txt!("{err}: [a]{cmd}")
                }
                _ => txt!("{err}"),
            })?;

        let (server_tx, server_rx) = mpsc::channel();

        let stdin = BufWriter::new(child.stdin.take().expect("Couldn't take stdin"));
        let stdout = BufReader::new(child.stdout.take().expect("Couldn't take stdin"));
        let mut stderr = BufReader::new(child.stderr.take().expect("Couldn't take stdin"));

        let stderr_handle = std::thread::spawn({
            let server_name = server_name.to_string();
            move || {
                let mut line = String::new();
                loop {
                    match stderr.read_line(&mut line) {
                        Ok(0) => {
                            break;
                        }
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
            server_tx,
            callbacks: Callbacks::default(),
        };

        let stdout_handle = std::thread::spawn({
            let callbacks = server_bridge.callbacks.clone();
            move || {
                if let Err(err) = stdout_loop(callbacks, stdout) {
                    context::error!("{err}");
                }
            }
        });

        let stdin_handle = std::thread::spawn(move || {
            if let Err(err) = stdin_loop(server_rx, stdin) {
                context::error!("{err}");
            }
        });

        server_bridge.send_request::<Initialize>(get_initialize_params(), {
            let server_bridge = server_bridge.clone();
            move |pa, response| {
                context::debug!("{response:#?}");
                server_bridge.send_notification::<Initialized>(InitializedParams {});

                let handle = context::current_buffer(pa);
                let text = handle.text(pa);
                server_bridge.send_notification::<DidOpenTextDocument>(DidOpenTextDocumentParams {
                    text_document: TextDocumentItem {
                        uri: "file:///home/mateus/.config/duat/src/lib.rs"
                            .parse()
                            .unwrap(),
                        language_id: "rust".to_string(),
                        version: text.version().strs as i32,
                        text: text.to_string(),
                    },
                });
            }
        });

        hook::add_once::<ConfigUnloaded>({
            let server_bridge = server_bridge.clone();
            move |_, _| {
                server_bridge.send_request::<Shutdown>((), {
                    let server_bridge = server_bridge.clone();
                    move |_, _| {
                        server_bridge.send_notification::<Exit>(());
                        if let Err(err) = child.wait() {
                            context::error!("Failed to close language server: {err}");
                        }
                        drop(child);
                        server_bridge
                            .server_tx
                            .send(ServerMessage::ExitReceiver)
                            .unwrap();
                        for join_handle in [stderr_handle, stdout_handle, stdin_handle] {
                            join_handle.join().unwrap();
                        }
                    }
                });
            }
        });

        Ok(server_bridge)
    }

    /// Sends a request alongside its parameters.
    #[track_caller]
    pub fn send_request<R: Request>(
        &self,
        params: R::Params,
        callback: impl FnOnce(&mut Pass, R::Result) + Send + 'static,
    ) {
        self.server_tx
            .send(ServerMessage::JsonRpcFn(Box::new(move || {
                let params = serde_json::to_value(params).map_err(|err| {
                    std::io::Error::other(format!("Failed to parse parameters: {err}"))
                })?;

                Ok(JsonRpc::request_with_params(
                    method_id(R::METHOD).unwrap(),
                    R::METHOD,
                    params,
                ))
            })))
            .unwrap();
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
        self.server_tx
            .send(ServerMessage::JsonRpcFn(Box::new(move || {
                Ok(JsonRpc::request_with_params(
                    Id::Num(0),
                    Cancel::METHOD,
                    serde_json::to_value(method_id(R::METHOD).unwrap())?,
                ))
            })))
            .unwrap();
    }

    /// Sends a notification alongside its parameters.
    pub fn send_notification<N: Notification>(&self, params: N::Params) {
        self.server_tx
            .send(ServerMessage::JsonRpcFn(Box::new(move || {
                let params = serde_json::to_value(params).map_err(|err| {
                    std::io::Error::other(format!("Failed to parse parameters: {err}"))
                })?;

                Ok(JsonRpc::notification_with_params(N::METHOD, params))
            })))
            .unwrap();
    }
}

fn stdin_loop(
    server_rx: mpsc::Receiver<ServerMessage>,
    mut stdin: impl Write,
) -> std::io::Result<()> {
    for server_message in server_rx {
        let content = serde_json::to_string(&match server_message {
            ServerMessage::JsonRpcFn(jsonrpc_fn) => {
                let msg = jsonrpc_fn()?;
                context::debug!("sending {msg:#?}");
                msg
            }
            ServerMessage::ExitReceiver => return Ok(()),
        })?;

        write!(stdin, "Content-Length: {}\r\n\r\n{content}", content.len())?;
        stdin.flush()?;
    }

    Ok(())
}

fn stdout_loop(callbacks: Callbacks, mut stdout: impl BufRead) -> std::io::Result<()> {
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
                    JsonRpc::Request(_) => {
                        context::debug!("Is request: {content:#?}");
                    }
                    JsonRpc::Notification(_) => {}
                    JsonRpc::Success(_) => {
                        if let Some(id) = content.get_id()
                            && let Some(callback) = callbacks.remove(&id)
                        {
                            let value = content.get_result().unwrap().clone();
                            context::queue(move |pa| callback(pa, value));
                        }
                    }
                    JsonRpc::Error(_) => {
                        context::error!("LSP: {content:#?}");
                        continue;
                    }
                };
            }
            Err(err) => return Err(Error::other(format!("Failed to parse LSP message: {err}"))),
        }
    }
}

fn get_initialize_params() -> InitializeParams {
    use lsp_types::*;

    let no_dynamic_registration =
        DynamicRegistrationClientCapabilities { dynamic_registration: Some(false) };

    let symbol_kind = SymbolKindCapability {
        value_set: Some(vec![
            SymbolKind::FILE,
            SymbolKind::MODULE,
            SymbolKind::NAMESPACE,
            SymbolKind::PACKAGE,
            SymbolKind::CLASS,
            SymbolKind::METHOD,
            SymbolKind::PROPERTY,
            SymbolKind::FIELD,
            SymbolKind::CONSTRUCTOR,
            SymbolKind::ENUM,
            SymbolKind::INTERFACE,
            SymbolKind::FUNCTION,
            SymbolKind::VARIABLE,
            SymbolKind::CONSTANT,
            SymbolKind::STRING,
            SymbolKind::NUMBER,
            SymbolKind::BOOLEAN,
            SymbolKind::ARRAY,
            SymbolKind::OBJECT,
            SymbolKind::KEY,
            SymbolKind::NULL,
            SymbolKind::ENUM_MEMBER,
            SymbolKind::STRUCT,
            SymbolKind::EVENT,
            SymbolKind::OPERATOR,
            SymbolKind::TYPE_PARAMETER,
        ]),
    };

    let goto_capability = GotoCapability {
        dynamic_registration: Some(false),
        link_support: Some(false),
    };

    InitializeParams {
        process_id: Some(std::process::id()),
        #[allow(deprecated)]
        root_path: Some("/home/mateus/.config/duat".to_string()),
        #[allow(deprecated)]
        root_uri: Some("file:///home/mateus/.config/duat".parse().unwrap()),
        initialization_options: None,
        capabilities: ClientCapabilities {
            workspace: Some(WorkspaceClientCapabilities {
                apply_edit: Some(true),
                workspace_edit: Some(WorkspaceEditClientCapabilities {
                    document_changes: Some(true),
                    resource_operations: Some(vec![
                        ResourceOperationKind::Create,
                        ResourceOperationKind::Rename,
                        ResourceOperationKind::Delete,
                    ]),
                    failure_handling: Some(FailureHandlingKind::Abort),
                    normalizes_line_endings: Some(false),
                    change_annotation_support: Some(
                        ChangeAnnotationWorkspaceEditClientCapabilities {
                            groups_on_label: Some(true),
                        },
                    ),
                }),
                did_change_configuration: Some(DynamicRegistrationClientCapabilities {
                    dynamic_registration: Some(true),
                }),
                did_change_watched_files: Some(DidChangeWatchedFilesClientCapabilities {
                    dynamic_registration: Some(true),
                    relative_pattern_support: Some(true),
                }),
                symbol: Some(WorkspaceSymbolClientCapabilities {
                    dynamic_registration: Some(false),
                    symbol_kind: Some(symbol_kind.clone()),
                    tag_support: Some(TagSupport { value_set: vec![SymbolTag::DEPRECATED] }),
                    resolve_support: Some(WorkspaceSymbolResolveSupportCapability {
                        properties: vec!["location.range".to_string()],
                    }),
                }),
                execute_command: Some(no_dynamic_registration),
                workspace_folders: Some(true),
                configuration: Some(true),
                semantic_tokens: Some(SemanticTokensWorkspaceClientCapabilities {
                    refresh_support: Some(true),
                }),
                code_lens: Some(lsp_types::CodeLensWorkspaceClientCapabilities {
                    refresh_support: Some(true),
                }),
                file_operations: Some(lsp_types::WorkspaceFileOperationsClientCapabilities {
                    dynamic_registration: Some(false),
                    did_create: Some(true),
                    will_create: Some(true),
                    did_rename: Some(true),
                    will_rename: Some(true),
                    did_delete: Some(true),
                    will_delete: Some(true),
                }),
                inline_value: Some(InlineValueWorkspaceClientCapabilities {
                    refresh_support: Some(true),
                }),
                inlay_hint: Some(InlayHintWorkspaceClientCapabilities {
                    refresh_support: Some(true),
                }),
                diagnostic: Some(DiagnosticWorkspaceClientCapabilities {
                    refresh_support: Some(true),
                }),
            }),
            text_document: Some(TextDocumentClientCapabilities {
                synchronization: Some(TextDocumentSyncClientCapabilities {
                    dynamic_registration: Some(false),
                    will_save: Some(true),
                    will_save_wait_until: Some(false),
                    did_save: Some(true),
                }),
                completion: Some(CompletionClientCapabilities {
                    dynamic_registration: Some(false),
                    completion_item: Some(CompletionItemCapability {
                        snippet_support: Some(true),
                        commit_characters_support: Some(true),
                        documentation_format: Some(vec![
                            MarkupKind::Markdown,
                            MarkupKind::PlainText,
                        ]),
                        deprecated_support: Some(true),
                        preselect_support: Some(false),
                        tag_support: Some(TagSupport {
                            value_set: vec![CompletionItemTag::DEPRECATED],
                        }),
                        insert_replace_support: Some(true),
                        resolve_support: Some(CompletionItemCapabilityResolveSupport {
                            properties: vec![
                                "additionalTextEdits".to_string(),
                                "detail".to_string(),
                                "documentation".to_string(),
                            ],
                        }),
                        insert_text_mode_support: None,
                        label_details_support: Some(true),
                    }),
                    completion_item_kind: Some(CompletionItemKindCapability {
                        value_set: Some(vec![
                            CompletionItemKind::TEXT,
                            CompletionItemKind::METHOD,
                            CompletionItemKind::FUNCTION,
                            CompletionItemKind::CONSTRUCTOR,
                            CompletionItemKind::FIELD,
                            CompletionItemKind::VARIABLE,
                            CompletionItemKind::CLASS,
                            CompletionItemKind::INTERFACE,
                            CompletionItemKind::MODULE,
                            CompletionItemKind::PROPERTY,
                            CompletionItemKind::UNIT,
                            CompletionItemKind::VALUE,
                            CompletionItemKind::ENUM,
                            CompletionItemKind::KEYWORD,
                            CompletionItemKind::SNIPPET,
                            CompletionItemKind::COLOR,
                            CompletionItemKind::FILE,
                            CompletionItemKind::REFERENCE,
                            CompletionItemKind::FOLDER,
                            CompletionItemKind::ENUM_MEMBER,
                            CompletionItemKind::CONSTANT,
                            CompletionItemKind::STRUCT,
                            CompletionItemKind::EVENT,
                            CompletionItemKind::OPERATOR,
                            CompletionItemKind::TYPE_PARAMETER,
                        ]),
                    }),
                    context_support: Some(true),
                    insert_text_mode: Some(InsertTextMode::AS_IS),
                    // Don't get it tbh.
                    completion_list: None,
                }),
                hover: Some(HoverClientCapabilities {
                    dynamic_registration: Some(false),
                    content_format: Some(vec![MarkupKind::Markdown, MarkupKind::PlainText]),
                }),
                signature_help: Some(SignatureHelpClientCapabilities {
                    dynamic_registration: Some(false),
                    signature_information: Some(SignatureInformationSettings {
                        documentation_format: Some(vec![
                            MarkupKind::Markdown,
                            MarkupKind::PlainText,
                        ]),
                        parameter_information: Some(ParameterInformationSettings {
                            label_offset_support: Some(true),
                        }),
                        active_parameter_support: Some(true),
                    }),
                    context_support: Some(true),
                }),
                references: Some(no_dynamic_registration),
                document_highlight: Some(no_dynamic_registration),
                document_symbol: Some(DocumentSymbolClientCapabilities {
                    dynamic_registration: Some(false),
                    symbol_kind: Some(symbol_kind),
                    hierarchical_document_symbol_support: Some(true),
                    tag_support: Some(TagSupport { value_set: vec![SymbolTag::DEPRECATED] }),
                }),
                formatting: Some(no_dynamic_registration),
                range_formatting: Some(no_dynamic_registration),
                on_type_formatting: Some(no_dynamic_registration),
                declaration: Some(goto_capability),
                definition: Some(goto_capability),
                type_definition: Some(goto_capability),
                implementation: Some(goto_capability),
                code_action: Some(CodeActionClientCapabilities {
                    dynamic_registration: Some(false),
                    code_action_literal_support: Some(CodeActionLiteralSupport {
                        code_action_kind: CodeActionKindLiteralSupport {
                            value_set: vec![
                                CodeActionKind::EMPTY.as_str().to_string(),
                                CodeActionKind::QUICKFIX.as_str().to_string(),
                                CodeActionKind::REFACTOR.as_str().to_string(),
                                CodeActionKind::REFACTOR_EXTRACT.as_str().to_string(),
                                CodeActionKind::REFACTOR_INLINE.as_str().to_string(),
                                CodeActionKind::REFACTOR_REWRITE.as_str().to_string(),
                                CodeActionKind::SOURCE.as_str().to_string(),
                                CodeActionKind::SOURCE_ORGANIZE_IMPORTS.as_str().to_string(),
                                CodeActionKind::SOURCE_FIX_ALL.as_str().to_string(),
                            ],
                        },
                    }),
                    is_preferred_support: Some(true),
                    disabled_support: Some(true),
                    data_support: Some(true),
                    resolve_support: Some(CodeActionCapabilityResolveSupport {
                        properties: vec!["edit".to_string()],
                    }),
                    honors_change_annotations: Some(false),
                }),
                code_lens: Some(no_dynamic_registration),
                document_link: Some(DocumentLinkClientCapabilities {
                    dynamic_registration: Some(false),
                    tooltip_support: Some(true),
                }),
                color_provider: Some(no_dynamic_registration),
                rename: Some(RenameClientCapabilities {
                    dynamic_registration: Some(false),
                    prepare_support: Some(true),
                    prepare_support_default_behavior: Some(
                        PrepareSupportDefaultBehavior::IDENTIFIER,
                    ),
                    honors_change_annotations: Some(false),
                }),
                publish_diagnostics: Some(PublishDiagnosticsClientCapabilities {
                    related_information: Some(true),
                    tag_support: Some(TagSupport {
                        value_set: vec![DiagnosticTag::UNNECESSARY, DiagnosticTag::DEPRECATED],
                    }),
                    version_support: Some(true),
                    code_description_support: Some(true),
                    data_support: Some(true),
                }),
                folding_range: Some(FoldingRangeClientCapabilities {
                    dynamic_registration: Some(false),
                    range_limit: None,
                    line_folding_only: Some(false),
                    folding_range_kind: Some(FoldingRangeKindCapability {
                        value_set: Some(vec![
                            FoldingRangeKind::Comment,
                            FoldingRangeKind::Imports,
                            FoldingRangeKind::Region,
                        ]),
                    }),
                    folding_range: Some(FoldingRangeCapability { collapsed_text: Some(true) }),
                }),
                selection_range: Some(SelectionRangeClientCapabilities {
                    dynamic_registration: Some(false),
                }),
                linked_editing_range: Some(LinkedEditingRangeClientCapabilities {
                    dynamic_registration: Some(false),
                }),
                call_hierarchy: Some(no_dynamic_registration),
                semantic_tokens: Some(SemanticTokensClientCapabilities {
                    dynamic_registration: Some(false),
                    requests: SemanticTokensClientCapabilitiesRequests {
                        range: Some(true),
                        full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                    },
                    token_types: vec![
                        SemanticTokenType::NAMESPACE,
                        SemanticTokenType::TYPE,
                        SemanticTokenType::CLASS,
                        SemanticTokenType::ENUM,
                        SemanticTokenType::INTERFACE,
                        SemanticTokenType::STRUCT,
                        SemanticTokenType::TYPE_PARAMETER,
                        SemanticTokenType::PARAMETER,
                        SemanticTokenType::VARIABLE,
                        SemanticTokenType::PROPERTY,
                        SemanticTokenType::ENUM_MEMBER,
                        SemanticTokenType::EVENT,
                        SemanticTokenType::FUNCTION,
                        SemanticTokenType::METHOD,
                        SemanticTokenType::MACRO,
                        SemanticTokenType::KEYWORD,
                        SemanticTokenType::MODIFIER,
                        SemanticTokenType::COMMENT,
                        SemanticTokenType::STRING,
                        SemanticTokenType::NUMBER,
                        SemanticTokenType::REGEXP,
                        SemanticTokenType::OPERATOR,
                        SemanticTokenType::DECORATOR,
                    ],
                    token_modifiers: vec![
                        SemanticTokenModifier::DECLARATION,
                        SemanticTokenModifier::DEFINITION,
                        SemanticTokenModifier::READONLY,
                        SemanticTokenModifier::STATIC,
                        SemanticTokenModifier::DEPRECATED,
                        SemanticTokenModifier::ABSTRACT,
                        SemanticTokenModifier::ASYNC,
                        SemanticTokenModifier::MODIFICATION,
                        SemanticTokenModifier::DOCUMENTATION,
                        SemanticTokenModifier::DEFAULT_LIBRARY,
                    ],
                    formats: vec![TokenFormat::RELATIVE],
                    overlapping_token_support: Some(true),
                    multiline_token_support: Some(true),
                    server_cancel_support: Some(true),
                    augments_syntax_tokens: Some(true),
                }),
                moniker: None,
                type_hierarchy: Some(no_dynamic_registration),
                inline_value: Some(no_dynamic_registration),
                inlay_hint: Some(InlayHintClientCapabilities {
                    dynamic_registration: Some(false),
                    resolve_support: Some(InlayHintResolveClientCapabilities {
                        properties: vec!["edit".to_string()],
                    }),
                }),
                diagnostic: Some(DiagnosticClientCapabilities {
                    dynamic_registration: Some(false),
                    related_document_support: Some(false),
                }),
                inline_completion: Some(InlineCompletionClientCapabilities {
                    dynamic_registration: Some(false),
                }),
            }),
            notebook_document: None,
            window: Some(WindowClientCapabilities {
                work_done_progress: Some(true),
                show_message: Some(ShowMessageRequestClientCapabilities {
                    message_action_item: Some(MessageActionItemCapabilities {
                        additional_properties_support: Some(false),
                    }),
                }),
                show_document: Some(ShowDocumentClientCapabilities { support: true }),
            }),
            general: Some(GeneralClientCapabilities {
                regular_expressions: Some(RegularExpressionsClientCapabilities {
                    engine: "regex-cursor".to_string(),
                    version: Some("0.1.5".to_string()),
                }),
                markdown: Some(MarkdownClientCapabilities {
                    parser: "duat-treesitter".to_string(),
                    version: Some("0.4.0".to_string()),
                    allowed_tags: None,
                }),
                stale_request_support: Some(StaleRequestSupportClientCapabilities {
                    cancel: true,
                    retry_on_content_modified: Vec::new(),
                }),
                position_encodings: Some(vec![
                    PositionEncodingKind::UTF8,
                    PositionEncodingKind::UTF16,
                ]),
            }),
            offset_encoding: None,
            experimental: None,
        },
        trace: Some(TraceValue::Off),
        workspace_folders: Some(vec![WorkspaceFolder {
            uri: "file:///home/mateus/.config/duat"
                .to_string()
                .parse()
                .unwrap(),
            name: "/home/mateus/.config/duat".to_string(),
        }]),
        client_info: Some(ClientInfo {
            name: "duat".to_string(),
            version: Some("0.9.0".to_string()),
        }),
        locale: None,
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: Some(NumberOrString::Number(0)),
        },
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

enum ServerMessage {
    JsonRpcFn(Box<dyn FnOnce() -> std::io::Result<JsonRpc> + Send>),
    ExitReceiver,
}

type Callbacks = Arc<Mutex<HashMap<Id, Box<dyn FnOnce(&mut Pass, Value) + Send + 'static>>>>;
