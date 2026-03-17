//! This module should deal with everything related to server
//! configuration.
//!
//! It should handle user settings, sending initialization requests,
//! and informing capabilities to the rest of the plugin.
use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use globset::{Glob, GlobSet};
use lsp_types::InitializeParams;
use serde::{Deserialize, Deserializer, de::Visitor};
use serde_json::Value;

mod languages;
pub use languages::get_for;

use crate::file_uri;

/// Configuration settings for each server.
#[derive(Debug, Clone, Default, Deserialize)]
pub struct LanguageServerConfig {
    pub command: Option<String>,
    #[serde(default)]
    pub args: Vec<String>,
    #[serde(default)]
    pub envs: HashMap<String, String>,
    // TODO: Investigate what this is about.
    pub settings_section: Option<String>,
    pub settings: Option<Value>,
    pub experimental: Option<Value>,
    #[serde(default)]
    #[serde(deserialize_with = "deserialize_globs")]
    pub root_globs: (GlobSet, Vec<String>),
    #[serde(default)]
    pub symbol_names: HashMap<String, String>,
    #[serde(default)]
    pub is_single_instance: bool,
}

impl LanguageServerConfig {
    /// Get the root directory for a given [`Path`].
    pub fn rootdir_for(&self, path: &Path) -> PathBuf {
        assert!(path.is_absolute(), "Path is not absolute, it should be");

        let mut path = path.to_path_buf();
        while !path.is_dir() {
            path.pop();
        }

        let globset = &self.root_globs.0;

        for ancestor in [path.as_ref()].into_iter().chain(path.ancestors()) {
            let Ok(entries) = ancestor.read_dir() else {
                continue;
            };

            if entries
                .filter_map(|entry| entry.ok())
                .any(|entry| globset.is_match(entry.path()))
            {
                return ancestor.to_path_buf();
            }
        }

        path
    }
}

impl Eq for LanguageServerConfig {}

impl PartialEq for LanguageServerConfig {
    fn eq(&self, other: &Self) -> bool {
        self.command == other.command
            && self.args == other.args
            && self.envs == other.envs
            && self.settings_section == other.settings_section
            && self.settings == other.settings
            && self.experimental == other.experimental
            && self.root_globs.1 == other.root_globs.1
            && self.symbol_names == other.symbol_names
            && self.is_single_instance == other.is_single_instance
    }
}

pub fn get_initialize_params(path: &Path, config: &LanguageServerConfig) -> InitializeParams {
    use lsp_types::*;

    let rootdir = config.rootdir_for(path);
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
        root_path: Some(rootdir.to_str().unwrap().to_string()),
        #[allow(deprecated)]
        root_uri: Some(file_uri(&rootdir).unwrap()),
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
                    insert_text_mode: None,
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
            uri: file_uri(&rootdir).unwrap(),
            name: rootdir.to_str().unwrap().to_string(),
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

fn deserialize_globs<'de, D: Deserializer<'de>>(de: D) -> Result<(GlobSet, Vec<String>), D::Error> {
    struct GlobPairVisitor;

    impl<'de> Visitor<'de> for GlobPairVisitor {
        type Value = (GlobSet, Vec<String>);

        fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("Expected a list of glob patterns")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'de>,
        {
            let mut globset = GlobSet::builder();
            let mut strings = Vec::new();

            while let Some(glob) = seq.next_element::<Glob>()? {
                strings.push(glob.glob().to_string());
                globset.add(glob);
            }

            Ok((globset.build().unwrap(), strings))
        }
    }

    de.deserialize_seq(GlobPairVisitor)
}
