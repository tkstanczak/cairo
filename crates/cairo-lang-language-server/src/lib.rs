//! Cairo language server.
//!
//! Implements the LSP protocol over stdin/out.

use std::collections::{HashMap, HashSet};
use std::panic::AssertUnwindSafe;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{bail, Error};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::{setup_project, update_crate_roots_from_project_config};
use cairo_lang_defs::db::{get_all_path_leafs, DefsGroup};
use cairo_lang_defs::ids::{
    ConstantLongId, EnumLongId, ExternFunctionLongId, ExternTypeLongId, FileIndex,
    FreeFunctionLongId, FunctionTitleId, FunctionWithBodyId, ImplDefLongId, ImplFunctionLongId,
    LanguageElementId, LookupItemId, ModuleFileId, ModuleId, ModuleItemId, StructLongId,
    TraitLongId, UseLongId,
};
use cairo_lang_diagnostics::{DiagnosticEntry, Diagnostics, ToOption};
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::db::{
    init_dev_corelib, AsFilesGroupMut, FilesGroup, FilesGroupEx, PrivRawFileContentQuery,
};
use cairo_lang_filesystem::detect::detect_corelib;
use cairo_lang_filesystem::ids::{CrateLongId, Directory, FileId, FileLongId};
use cairo_lang_filesystem::span::{FileSummary, TextOffset, TextPosition, TextWidth};
use cairo_lang_formatter::{get_formatted_file, FormatterConfig};
use cairo_lang_lowering::db::LoweringGroup;
use cairo_lang_lowering::diagnostic::LoweringDiagnostic;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_project::ProjectConfig;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::items::us::get_use_segments;
use cairo_lang_semantic::resolve::{AsSegments, ResolvedGenericItem};
use cairo_lang_semantic::SemanticDiagnostic;
use cairo_lang_starknet::plugin::StarkNetPlugin;
use cairo_lang_syntax::node::ast::PathSegment;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::GetIdentifier;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::stable_ptr::SyntaxStablePtr;
use cairo_lang_syntax::node::utils::is_grandparent_of_kind;
use cairo_lang_syntax::node::{ast, SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;
use cairo_lang_utils::{try_extract_matches, OptionHelper, Upcast};
use log::warn;
use lsp::notification::Notification;
use salsa::InternKey;
use semantic_highlighting::token_kind::SemanticTokenKind;
use semantic_highlighting::SemanticTokensTraverser;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::{Error as LSPError, Result as LSPResult};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use vfs::{ProvideVirtualFileRequest, ProvideVirtualFileResponse};

use crate::completions::{colon_colon_completions, dot_completions, generic_completions};
use crate::scarb_service::{is_scarb_manifest_path, ScarbService};

mod scarb_service;
mod semantic_highlighting;

pub mod completions;
pub mod vfs;

const MAX_CRATE_DETECTION_DEPTH: usize = 20;

pub async fn serve_language_service() {
    #[cfg(feature = "runtime-agnostic")]
    use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    #[cfg(feature = "runtime-agnostic")]
    let (stdin, stdout) = (stdin.compat(), stdout.compat_write());

    let db = RootDatabase::builder()
        .with_cfg(CfgSet::from_iter([Cfg::name("test")]))
        .with_semantic_plugin(Arc::new(StarkNetPlugin::default()))
        .build()
        .expect("Failed to initialize Cairo compiler database.");

    let (service, socket) = LspService::build(|client| Backend::new(client, db))
        .custom_method("vfs/provide", Backend::vfs_provide)
        .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}

#[derive(Clone, Default, PartialEq, Eq)]
pub struct FileDiagnostics {
    pub parser: Diagnostics<ParserDiagnostic>,
    pub semantic: Diagnostics<SemanticDiagnostic>,
    pub lowering: Diagnostics<LoweringDiagnostic>,
}
impl std::panic::UnwindSafe for FileDiagnostics {}
#[derive(Clone, Default)]
pub struct State {
    pub file_diagnostics: HashMap<FileId, FileDiagnostics>,
    pub open_files: HashSet<FileId>,
}
impl std::panic::UnwindSafe for State {}

#[derive(Clone)]
pub struct NotificationService {
    pub client: Client,
}
impl NotificationService {
    pub fn new(client: Client) -> Self {
        Self { client }
    }
    pub async fn notify_resolving_start(&self) {
        self.client.send_notification::<ScarbResolvingStart>(ScarbResolvingStartParams {}).await;
    }
    pub async fn notify_resolving_finish(&self) {
        self.client.send_notification::<ScarbResolvingFinish>(ScarbResolvingFinishParams {}).await;
    }
    pub async fn notify_scarb_missing(&self) {
        self.client.send_notification::<ScarbPathMissing>(ScarbPathMissingParams {}).await;
    }
}
pub struct Backend {
    pub client: Client,
    // TODO(spapini): Remove this once we support ParallelDatabase.
    // State mutex should only be taken after db mutex is taken, to avoid deadlocks.
    pub db_mutex: tokio::sync::Mutex<RootDatabase>,
    pub state_mutex: tokio::sync::Mutex<State>,
    pub scarb: ScarbService,
    pub notification: NotificationService,
}
fn from_pos(pos: TextPosition) -> Position {
    Position { line: pos.line as u32, character: pos.col as u32 }
}
impl Backend {
    pub fn new(client: Client, db: RootDatabase) -> Self {
        let notification = NotificationService::new(client.clone());
        Self {
            client,
            db_mutex: db.into(),
            notification: notification.clone(),
            state_mutex: State::default().into(),
            scarb: ScarbService::new(notification),
        }
    }

    /// Runs a function with a database snapshot.
    /// Catches panics and returns Err.
    async fn with_db<F, T>(&self, f: F) -> LSPResult<T>
    where
        F: FnOnce(&RootDatabase) -> T + std::panic::UnwindSafe,
    {
        let db_mut = self.db_mutex.lock().await;
        let db = db_mut.snapshot();
        drop(db_mut);
        std::panic::catch_unwind(AssertUnwindSafe(|| f(&db))).map_err(|_| {
            eprintln!("Caught panic in LSP worker thread.");
            LSPError::internal_error()
        })
    }

    /// Locks and gets a database instance.
    async fn db_mut(&self) -> tokio::sync::MutexGuard<'_, RootDatabase> {
        self.db_mutex.lock().await
    }

    // TODO(spapini): Consider managing vfs in a different way, using the
    // client.send_notification::<UpdateVirtualFile> call.

    // Refresh diagnostics and send diffs to client.
    async fn refresh_diagnostics(&self) -> LSPResult<()> {
        let real_state = self.state_mutex.lock().await;
        let state = real_state.clone();
        drop(real_state);
        let (state, res) = self
            .with_db(|db| {
                let mut state = state;
                let mut res = vec![];
                // Get all files. Try to go over open files first.
                let mut files_set: OrderedHashSet<_> = state.open_files.iter().copied().collect();
                for crate_id in db.crates() {
                    for module_id in db.crate_modules(crate_id).iter() {
                        for file_id in db.module_files(*module_id).unwrap_or_default() {
                            files_set.insert(file_id);
                        }
                    }
                }

                // Get all diagnostics.
                for file_id in files_set.iter().copied() {
                    let uri = get_uri(db, file_id);
                    let new_file_diagnostics = FileDiagnostics {
                        parser: db.file_syntax_diagnostics(file_id),
                        semantic: db.file_semantic_diagnostics(file_id).unwrap_or_default(),
                        lowering: db.file_lowering_diagnostics(file_id).unwrap_or_default(),
                    };
                    // Since we are using Arcs, this comparison should be efficient.
                    if let Some(old_file_diagnostics) = state.file_diagnostics.get(&file_id) {
                        if old_file_diagnostics == &new_file_diagnostics {
                            continue;
                        }
                    }
                    let mut diags = Vec::new();
                    get_diagnostics(db.upcast(), &mut diags, &new_file_diagnostics.parser);
                    get_diagnostics(db.upcast(), &mut diags, &new_file_diagnostics.semantic);
                    get_diagnostics(db.upcast(), &mut diags, &new_file_diagnostics.lowering);
                    state.file_diagnostics.insert(file_id, new_file_diagnostics);

                    res.push((uri, diags));
                }

                // Clear old diagnostics.
                let old_files: Vec<_> = state.file_diagnostics.keys().copied().collect();
                for file_id in old_files {
                    if files_set.contains(&file_id) {
                        continue;
                    }
                    state.file_diagnostics.remove(&file_id);
                    let uri = get_uri(db, file_id);
                    res.push((uri, Vec::new()));
                }

                (state, res)
            })
            .await?;
        let mut real_state = self.state_mutex.lock().await;
        *real_state = state;
        drop(real_state);

        for (uri, diags) in res {
            self.client.publish_diagnostics(uri, diags, None).await
        }

        Ok(())
    }

    pub async fn vfs_provide(
        &self,
        params: ProvideVirtualFileRequest,
    ) -> LSPResult<ProvideVirtualFileResponse> {
        self.with_db(|db| {
            let file_id = file(db, params.uri);
            ProvideVirtualFileResponse { content: db.file_content(file_id).map(|s| (*s).clone()) }
        })
        .await
    }

    /// Get corelib path fallback from the client configuration.
    ///
    /// The value is set by the user under the `cairo1.corelibPath` key in client configuration.
    /// The value is not required to be set.
    /// The path may omit the `corelib/src` or `src` suffix.
    async fn get_corelib_fallback_path(&self) -> Option<PathBuf> {
        const CORELIB_CONFIG_SECTION: &str = "cairo1.corelibPath";
        let item = vec![ConfigurationItem {
            scope_uri: None,
            section: Some(CORELIB_CONFIG_SECTION.to_string()),
        }];
        let corelib_response = self.client.configuration(item).await;
        match corelib_response.map_err(Error::from) {
            Ok(value_vec) => {
                if let Some(Value::String(value)) = value_vec.get(0) {
                    if !value.is_empty() {
                        let root_path: PathBuf = value.into();

                        let mut path = root_path.clone();
                        path.push("corelib");
                        path.push("src");
                        if path.exists() {
                            return Some(path);
                        }

                        let mut path = root_path.clone();
                        path.push("src");
                        if path.exists() {
                            return Some(path);
                        }

                        if root_path.exists() {
                            return Some(root_path);
                        }
                    }
                }
            }
            Err(err) => {
                let err =
                    err.context("Failed to get configuration under `cairo1.corelibPath` key.");
                warn!("{err:?}");
            }
        };
        None
    }

    /// Tries to detect the crate root the config that contains a cairo file, and add it to the
    /// system.
    async fn detect_crate_for(&self, db: &mut RootDatabase, file_path: &str) {
        let corelib_fallback = self.get_corelib_fallback_path().await;
        if self.scarb.is_scarb_project(file_path.into()) {
            if self.scarb.is_scarb_found() {
                // Carrying out Scarb based setup.
                let corelib = match self.scarb.corelib_path(file_path.into()).await {
                    Ok(corelib) => corelib,
                    Err(err) => {
                        let err =
                            err.context("Failed to obtain scarb corelib path from manifest file.");
                        warn!("{err:?}");
                        None
                    }
                };
                if let Some(corelib) = corelib.or_else(detect_corelib).or(corelib_fallback) {
                    init_dev_corelib(db, corelib);
                } else {
                    warn!("Failed to find corelib path.");
                }

                match self.scarb.crate_roots(file_path.into()).await {
                    Ok(create_roots) => update_crate_roots(db, create_roots),
                    Err(err) => {
                        let err =
                            err.context("Failed to obtain scarb metadata from manifest file.");
                        warn!("{err:?}");
                    }
                };
                return;
            } else {
                warn!("Not resolving Scarb metadata from manifest file due to missing Scarb path.");
                self.notification.notify_scarb_missing().await;
            }
        }

        // Scarb based setup not possible.
        if let Some(corelib) = detect_corelib().or(corelib_fallback) {
            init_dev_corelib(db, corelib);
        } else {
            warn!("Failed to find corelib path.");
        }

        // Fallback to cairo_project manifest format.
        let mut path = PathBuf::from(file_path);
        for _ in 0..MAX_CRATE_DETECTION_DEPTH {
            path.pop();
            // Check for a cairo project file.
            if let Ok(config) = ProjectConfig::from_directory(path.as_path()) {
                update_crate_roots_from_project_config(db, config);
                return;
            };
        }

        // Fallback to a single file.
        if let Err(err) = setup_project(&mut *db, PathBuf::from(file_path).as_path()) {
            eprintln!("Error loading file {file_path} as a single crate: {err}");
        }
    }

    /// Reload crate detection for all open files.
    pub async fn reload(&self) -> LSPResult<()> {
        let mut db = self.db_mut().await;
        for file in self.state_mutex.lock().await.open_files.iter() {
            let file = db.lookup_intern_file(*file);
            if let FileLongId::OnDisk(file_path) = file {
                if let Some(file_path) = file_path.to_str() {
                    self.detect_crate_for(&mut db, file_path).await;
                }
            }
        }
        drop(db);
        self.refresh_diagnostics().await
    }
}

#[derive(Debug)]
pub struct ScarbPathMissing {}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct ScarbPathMissingParams {}

impl Notification for ScarbPathMissing {
    type Params = ScarbPathMissingParams;
    const METHOD: &'static str = "scarb/could-not-find-scarb-executable";
}

#[derive(Debug)]
pub struct ScarbResolvingStart {}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct ScarbResolvingStartParams {}

impl Notification for ScarbResolvingStart {
    type Params = ScarbResolvingStartParams;
    const METHOD: &'static str = "scarb/resolving-start";
}

#[derive(Debug)]
pub struct ScarbResolvingFinish {}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct ScarbResolvingFinishParams {}

impl Notification for ScarbResolvingFinish {
    type Params = ScarbResolvingFinishParams;
    const METHOD: &'static str = "scarb/resolving-finish";
}

pub enum ServerCommands {
    Reload,
}

impl TryFrom<String> for ServerCommands {
    type Error = anyhow::Error;

    fn try_from(value: String) -> anyhow::Result<Self> {
        match value.as_str() {
            "cairo1.reload" => Ok(ServerCommands::Reload),
            _ => bail!("Unrecognized command: {value}"),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> LSPResult<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    all_commit_characters: None,
                    work_done_progress_options: Default::default(),
                    completion_item: None,
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["cairo1.reload".to_string()],
                    work_done_progress_options: Default::default(),
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensOptions {
                        legend: SemanticTokensLegend {
                            token_types: SemanticTokenKind::legend(),
                            token_modifiers: vec![],
                        },
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                        ..SemanticTokensOptions::default()
                    }
                    .into(),
                ),
                document_formatting_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        // Register patterns for client file watcher.
        // This is used to detect changes to Scarb.toml and invalidate .cairo files.
        let registration_options = DidChangeWatchedFilesRegistrationOptions {
            watchers: vec!["/**/*.cairo", "/**/Scarb.toml"]
                .into_iter()
                .map(|glob_pattern| FileSystemWatcher {
                    glob_pattern: GlobPattern::String(glob_pattern.to_string()),
                    kind: None,
                })
                .collect(),
        };
        let registration = Registration {
            id: "workspace/didChangeWatchedFiles".to_string(),
            method: "workspace/didChangeWatchedFiles".to_string(),
            register_options: Some(serde_json::to_value(registration_options).unwrap()),
        };
        let result = self.client.register_capability(vec![registration]).await;
        if let Err(err) = result {
            warn!("Failed to register workspace/didChangeWatchedFiles event: {:#?}", err);
        }
    }

    async fn shutdown(&self) -> LSPResult<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {}

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {}

    async fn did_change_watched_files(&self, params: DidChangeWatchedFilesParams) {
        // Invalidate changed cairo files.
        let mut db = self.db_mut().await;
        for change in &params.changes {
            if is_cairo_file_path(&change.uri) {
                let file = file(&db, change.uri.clone());
                PrivRawFileContentQuery.in_db_mut(db.as_files_group_mut()).invalidate(&file);
            }
        }
        drop(db);
        // Reload workspace if Scarb.toml changed.
        for change in params.changes {
            if is_scarb_manifest_path(&change.uri) {
                self.reload().await.ok();
            }
        }
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> LSPResult<Option<Value>> {
        let command = ServerCommands::try_from(params.command);
        if let Ok(cmd) = command {
            match cmd {
                ServerCommands::Reload => {
                    self.reload().await?;
                }
            }
        }

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let mut db = self.db_mut().await;
        let uri = params.text_document.uri;

        // Try to detect the crate for physical files.
        // The crate for virtual files is already known.
        if uri.scheme() == "file" {
            let path = uri.path();
            self.detect_crate_for(&mut db, path).await;
        }

        let file = file(&db, uri.clone());
        self.state_mutex.lock().await.open_files.insert(file);
        drop(db);
        self.refresh_diagnostics().await.ok();
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let text =
            if let [TextDocumentContentChangeEvent { text, .. }] = &params.content_changes[..] {
                text
            } else {
                eprintln!("Unexpected format of document change.");
                return;
            };
        let mut db = self.db_mut().await;
        let uri = params.text_document.uri;
        let file = file(&db, uri.clone());
        db.override_file_content(file, Some(Arc::new(text.into())));
        drop(db);
        self.refresh_diagnostics().await.ok();
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let mut db = self.db_mut().await;
        let file = file(&db, params.text_document.uri);
        PrivRawFileContentQuery.in_db_mut(db.as_files_group_mut()).invalidate(&file);
        db.override_file_content(file, None);
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut db = self.db_mut().await;
        let file = file(&db, params.text_document.uri);
        self.state_mutex.lock().await.open_files.remove(&file);
        db.override_file_content(file, None);
        drop(db);
        self.refresh_diagnostics().await.ok();
    }

    async fn completion(&self, params: CompletionParams) -> LSPResult<Option<CompletionResponse>> {
        self.with_db(|db| {
            let text_document_position = params.text_document_position;
            let file_uri = text_document_position.text_document.uri;
            eprintln!("Complete {file_uri}");
            let file = file(db, file_uri);
            let position = text_document_position.position;

            let Some((mut node, lookup_items)) = get_node_and_lookup_items(db, file, position)
            else {
                return None;
            };

            // Find module.
            let module_id = find_node_module(db, file, node.clone()).on_none(|| {
                eprintln!("Hover failed. Failed to find module.");
            })?;
            let file_index = FileIndex(0);
            let module_file_id = ModuleFileId(module_id, file_index);

            // Skip trivia.
            while ast::Trivium::is_variant(node.kind(db))
                || node.kind(db) == SyntaxKind::Trivia
                || node.kind(db).is_token()
            {
                node = node.parent().unwrap_or(node);
            }

            let trigger_kind =
                params.context.map(|it| it.trigger_kind).unwrap_or(CompletionTriggerKind::INVOKED);

            match completion_kind(db, node) {
                CompletionKind::Dot(expr) => {
                    dot_completions(db, lookup_items, expr).map(CompletionResponse::Array)
                }
                CompletionKind::ColonColon(segments) if !segments.is_empty() => {
                    colon_colon_completions(db, module_file_id, lookup_items, segments)
                        .map(CompletionResponse::Array)
                }
                _ if trigger_kind == CompletionTriggerKind::INVOKED => {
                    Some(CompletionResponse::Array(generic_completions(
                        db,
                        module_file_id,
                        lookup_items,
                    )))
                }
                _ => None,
            }
        })
        .await
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> LSPResult<Option<SemanticTokensResult>> {
        self.with_db(|db| {
            let file_uri = params.text_document.uri;
            let file = file(db, file_uri.clone());
            let syntax = if let Ok(syntax) = db.file_syntax(file) {
                syntax
            } else {
                eprintln!("Semantic analysis failed. File '{file_uri}' does not exist.");
                return None;
            };

            let node = syntax.as_syntax_node();
            let mut data: Vec<SemanticToken> = Vec::new();
            SemanticTokensTraverser::default().find_semantic_tokens(
                db.upcast(),
                file,
                &mut data,
                node,
            );
            Some(SemanticTokensResult::Tokens(SemanticTokens { result_id: None, data }))
        })
        .await
    }

    async fn formatting(
        &self,
        params: DocumentFormattingParams,
    ) -> LSPResult<Option<Vec<TextEdit>>> {
        self.with_db(|db| {
            let file_uri = params.text_document.uri;
            let file = file(db, file_uri.clone());
            let syntax = if let Ok(syntax) = db.file_syntax(file) {
                syntax
            } else {
                eprintln!("Formatting failed. File '{file_uri}' does not exist.");
                return None;
            };
            if !db.file_syntax_diagnostics(file).is_empty() {
                eprintln!("Formatting failed. File '{file_uri}' has syntax errors.");
                return None;
            }
            let new_text = get_formatted_file(
                db.upcast(),
                &syntax.as_syntax_node(),
                FormatterConfig::default(),
            );

            let file_summary = if let Some(summary) = db.file_summary(file) {
                summary
            } else {
                eprintln!("Formatting failed. Cannot get summary for file '{file_uri}'.");
                return None;
            };
            let old_line_count = if let Ok(count) = file_summary.line_count().try_into() {
                count
            } else {
                eprintln!("Formatting failed. Line count out of bound in file '{file_uri}'.");
                return None;
            };

            Some(vec![TextEdit {
                range: Range {
                    start: Position { line: 0, character: 0 },
                    end: Position { line: old_line_count, character: 0 },
                },
                new_text,
            }])
        })
        .await
    }

    async fn hover(&self, params: HoverParams) -> LSPResult<Option<Hover>> {
        self.with_db(|db| {
            let file_uri = params.text_document_position_params.text_document.uri;
            eprintln!("Hover {file_uri}");
            let file = file(db, file_uri);
            let position = params.text_document_position_params.position;
            let Some((node, lookup_items)) = get_node_and_lookup_items(db, file, position) else {
                return None;
            };
            let Some(lookup_item_id) = lookup_items.into_iter().next() else {
                return None;
            };
            let function_id = match lookup_item_id {
                LookupItemId::ModuleItem(ModuleItemId::FreeFunction(free_function_id)) => {
                    FunctionWithBodyId::Free(free_function_id)
                }
                LookupItemId::ImplFunction(impl_function_id) => {
                    FunctionWithBodyId::Impl(impl_function_id)
                }
                _ => {
                    return None;
                }
            };

            // Build texts.
            let mut hints = Vec::new();
            if let Some(hint) = get_pat_hint(db, function_id, node.clone()) {
                hints.push(MarkedString::String(hint));
            } else if let Some(hint) = get_expr_hint(db, function_id, node.clone()) {
                hints.push(MarkedString::String(hint));
            };
            if let Some(hint) = get_identifier_hint(db, lookup_item_id, node) {
                hints.push(MarkedString::String(hint));
            };

            Some(Hover { contents: HoverContents::Array(hints), range: None })
        })
        .await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> LSPResult<Option<GotoDefinitionResponse>> {
        eprintln!("Goto definition");
        self.with_db(|db| {
            let syntax_db = db.upcast();
            let file_uri = params.text_document_position_params.text_document.uri;
            let file = file(db, file_uri.clone());
            let position = params.text_document_position_params.position;
            let Some((node, lookup_items)) = get_node_and_lookup_items(db, file, position) else {
                return None;
            };
            for lookup_item_id in lookup_items {
                if node.kind(syntax_db) != SyntaxKind::TokenIdentifier {
                    continue;
                }
                let identifier =
                    ast::TerminalIdentifier::from_syntax_node(syntax_db, node.parent().unwrap());
                let Some(item) =
                    db.lookup_resolved_generic_item_by_ptr(lookup_item_id, identifier.stable_ptr())
                else {
                    continue;
                };

                let defs_db = db.upcast();
                let (module_id, file_index, stable_ptr) = match item {
                    ResolvedGenericItem::Constant(item) => (
                        item.parent_module(defs_db),
                        item.file_index(defs_db),
                        item.untyped_stable_ptr(defs_db),
                    ),
                    ResolvedGenericItem::Module(item) => {
                        (item, FileIndex(0), db.intern_stable_ptr(SyntaxStablePtr::Root))
                    }
                    ResolvedGenericItem::GenericFunction(item) => {
                        let title = match item {
                            GenericFunctionId::Free(id) => FunctionTitleId::Free(id),
                            GenericFunctionId::Extern(id) => FunctionTitleId::Extern(id),
                            GenericFunctionId::Impl(id) => {
                                // Note: Only the trait title is returned.
                                FunctionTitleId::Trait(id.function)
                            }
                        };
                        (
                            title.parent_module(defs_db),
                            title.file_index(defs_db),
                            title.untyped_stable_ptr(defs_db),
                        )
                    }
                    ResolvedGenericItem::GenericType(generic_type) => (
                        generic_type.parent_module(defs_db),
                        generic_type.file_index(defs_db),
                        generic_type.untyped_stable_ptr(defs_db),
                    ),
                    ResolvedGenericItem::GenericTypeAlias(type_alias) => (
                        type_alias.parent_module(defs_db),
                        type_alias.file_index(defs_db),
                        type_alias.untyped_stable_ptr(defs_db),
                    ),
                    ResolvedGenericItem::GenericImplAlias(impl_alias) => (
                        impl_alias.parent_module(defs_db),
                        impl_alias.file_index(defs_db),
                        impl_alias.untyped_stable_ptr(defs_db),
                    ),
                    ResolvedGenericItem::Variant(variant) => (
                        variant.id.parent_module(defs_db),
                        variant.id.file_index(defs_db),
                        variant.id.stable_ptr(defs_db).untyped(),
                    ),
                    ResolvedGenericItem::Trait(trt) => (
                        trt.parent_module(defs_db),
                        trt.file_index(defs_db),
                        trt.stable_ptr(defs_db).untyped(),
                    ),
                    ResolvedGenericItem::Impl(imp) => (
                        imp.parent_module(defs_db),
                        imp.file_index(defs_db),
                        imp.stable_ptr(defs_db).untyped(),
                    ),
                    ResolvedGenericItem::TraitFunction(trait_function) => (
                        trait_function.parent_module(defs_db),
                        trait_function.file_index(defs_db),
                        trait_function.stable_ptr(defs_db).untyped(),
                    ),
                    ResolvedGenericItem::Variable(item, var) => (
                        item.parent_module(defs_db),
                        item.file_index(defs_db),
                        var.untyped_stable_ptr(defs_db),
                    ),
                };

                let file = if let Ok(files) = db.module_files(module_id) {
                    files[file_index.0]
                } else {
                    return None;
                };

                let uri = get_uri(db, file);
                let syntax = if let Ok(syntax) = db.file_syntax(file) {
                    syntax
                } else {
                    eprintln!("Formatting failed. File '{file_uri}' does not exist.");
                    return None;
                };
                let node = syntax.as_syntax_node().lookup_ptr(syntax_db, stable_ptr);
                let span = node.span_without_trivia(syntax_db);

                let start = from_pos(span.start.position_in_file(db.upcast(), file).unwrap());
                let end = from_pos(span.end.position_in_file(db.upcast(), file).unwrap());

                return Some(GotoDefinitionResponse::Scalar(Location {
                    uri,
                    range: Range { start, end },
                }));
            }
            None
        })
        .await
    }
}

enum CompletionKind {
    Dot(ast::ExprBinary),
    ColonColon(Vec<PathSegment>),
}

fn completion_kind(db: &RootDatabase, node: SyntaxNode) -> CompletionKind {
    eprintln!("node.kind: {:#?}", node.kind(db));
    match node.kind(db) {
        SyntaxKind::TerminalDot => {
            let parent = node.parent().unwrap();
            if parent.kind(db) == SyntaxKind::ExprBinary {
                return CompletionKind::Dot(ast::ExprBinary::from_syntax_node(db, parent));
            }
        }
        SyntaxKind::TerminalColonColon => {
            let parent = node.parent().unwrap();
            eprintln!("parent.kind: {:#?}", parent.kind(db));
            if parent.kind(db) == SyntaxKind::ExprPath {
                return completion_kind_from_path_node(db, parent);
            }
            let grandparent = parent.parent().unwrap();
            eprintln!("grandparent.kind: {:#?}", grandparent.kind(db));
            if grandparent.kind(db) == SyntaxKind::ExprPath {
                return completion_kind_from_path_node(db, grandparent);
            }
            let (use_ast, should_pop) = if parent.kind(db) == SyntaxKind::UsePathLeaf {
                (ast::UsePath::Leaf(ast::UsePathLeaf::from_syntax_node(db, parent)), true)
            } else if grandparent.kind(db) == SyntaxKind::UsePathLeaf {
                (ast::UsePath::Leaf(ast::UsePathLeaf::from_syntax_node(db, grandparent)), true)
            } else if parent.kind(db) == SyntaxKind::UsePathSingle {
                (ast::UsePath::Single(ast::UsePathSingle::from_syntax_node(db, parent)), false)
            } else if grandparent.kind(db) == SyntaxKind::UsePathSingle {
                (ast::UsePath::Single(ast::UsePathSingle::from_syntax_node(db, grandparent)), false)
            } else {
                eprintln!("Generic");
                return CompletionKind::ColonColon(vec![]);
            };
            let mut segments = vec![];
            let Ok(()) = get_use_segments(db.upcast(), &use_ast, &mut segments) else {
                eprintln!("Generic");
                return CompletionKind::ColonColon(vec![]);
            };
            if should_pop {
                segments.pop();
            }
            eprintln!("ColonColon");
            return CompletionKind::ColonColon(segments);
        }
        SyntaxKind::TerminalIdentifier => {
            let parent = node.parent().unwrap();
            eprintln!("parent.kind: {:#?}", parent.kind(db));
            let grandparent = parent.parent().unwrap();
            eprintln!("grandparent.kind: {:#?}", grandparent.kind(db));
            if grandparent.kind(db) == SyntaxKind::ExprPath {
                if grandparent.children(db).next().unwrap().stable_ptr() != parent.stable_ptr() {
                    // Not first segment.
                    eprintln!("Not first segment");
                    return completion_kind_from_path_node(db, grandparent);
                }
                // First segment.
                let grandgrandparent = grandparent.parent().unwrap();
                eprintln!("grandgrandparent.kind: {:#?}", grandgrandparent.kind(db));
                if grandgrandparent.kind(db) == SyntaxKind::ExprBinary {
                    let expr = ast::ExprBinary::from_syntax_node(db, grandgrandparent.clone());
                    if matches!(
                        ast::ExprBinary::from_syntax_node(db, grandgrandparent).op(db),
                        ast::BinaryOperator::Dot(_)
                    ) {
                        eprintln!("Dot");
                        return CompletionKind::Dot(expr);
                    }
                }
            }
            if grandparent.kind(db) == SyntaxKind::UsePathLeaf {
                let use_ast = ast::UsePathLeaf::from_syntax_node(db, grandparent);
                let mut segments = vec![];
                let Ok(()) =
                    get_use_segments(db.upcast(), &ast::UsePath::Leaf(use_ast), &mut segments)
                else {
                    eprintln!("Generic");
                    return CompletionKind::ColonColon(vec![]);
                };
                segments.pop();
                eprintln!("ColonColon");
                return CompletionKind::ColonColon(segments);
            }
        }
        _ => (),
    }
    eprintln!("Generic");
    CompletionKind::ColonColon(vec![])
}

fn completion_kind_from_path_node(db: &RootDatabase, parent: SyntaxNode) -> CompletionKind {
    eprintln!("completion_kind_from_path_node: {}", parent.clone().get_text_without_trivia(db));
    let expr = ast::ExprPath::from_syntax_node(db, parent);
    eprintln!("has_tail: {}", expr.has_tail(db));
    let mut segments = expr.to_segments(db);
    if expr.has_tail(db) {
        segments.pop();
    }
    CompletionKind::ColonColon(segments)
}

/// If the ast node is a lookup item, return the corresponding id. Otherwise, return None.
/// See [LookupItemId].
fn lookup_item_from_ast(
    db: &dyn SemanticGroup,
    module_file_id: ModuleFileId,
    node: SyntaxNode,
) -> Vec<LookupItemId> {
    let syntax_db = db.upcast();
    // TODO(spapini): Handle trait items.
    match node.kind(syntax_db) {
        SyntaxKind::ItemConstant => vec![LookupItemId::ModuleItem(ModuleItemId::Constant(
            db.intern_constant(ConstantLongId(
                module_file_id,
                ast::ItemConstant::from_syntax_node(syntax_db, node).stable_ptr(),
            )),
        ))],
        SyntaxKind::FunctionWithBody => {
            if is_grandparent_of_kind(syntax_db, &node, SyntaxKind::ImplBody) {
                vec![LookupItemId::ImplFunction(db.intern_impl_function(ImplFunctionLongId(
                    module_file_id,
                    ast::FunctionWithBody::from_syntax_node(syntax_db, node).stable_ptr(),
                )))]
            } else {
                vec![LookupItemId::ModuleItem(ModuleItemId::FreeFunction(db.intern_free_function(
                    FreeFunctionLongId(
                        module_file_id,
                        ast::FunctionWithBody::from_syntax_node(syntax_db, node).stable_ptr(),
                    ),
                )))]
            }
        }
        SyntaxKind::ItemExternFunction => vec![LookupItemId::ModuleItem(
            ModuleItemId::ExternFunction(db.intern_extern_function(ExternFunctionLongId(
                module_file_id,
                ast::ItemExternFunction::from_syntax_node(syntax_db, node).stable_ptr(),
            ))),
        )],
        SyntaxKind::ItemExternType => vec![LookupItemId::ModuleItem(ModuleItemId::ExternType(
            db.intern_extern_type(ExternTypeLongId(
                module_file_id,
                ast::ItemExternType::from_syntax_node(syntax_db, node).stable_ptr(),
            )),
        ))],
        SyntaxKind::ItemTrait => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Trait(db.intern_trait(TraitLongId(
                module_file_id,
                ast::ItemTrait::from_syntax_node(syntax_db, node).stable_ptr(),
            ))))]
        }
        SyntaxKind::ItemImpl => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Impl(db.intern_impl(ImplDefLongId(
                module_file_id,
                ast::ItemImpl::from_syntax_node(syntax_db, node).stable_ptr(),
            ))))]
        }
        SyntaxKind::ItemStruct => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Struct(db.intern_struct(StructLongId(
                module_file_id,
                ast::ItemStruct::from_syntax_node(syntax_db, node).stable_ptr(),
            ))))]
        }
        SyntaxKind::ItemEnum => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Enum(db.intern_enum(EnumLongId(
                module_file_id,
                ast::ItemEnum::from_syntax_node(syntax_db, node).stable_ptr(),
            ))))]
        }
        SyntaxKind::ItemUse => {
            // Item use is not a lookup item, so we need to collect all UseLeaf, which are lookup
            // items.
            let item_use = ast::ItemUse::from_syntax_node(db.upcast(), node);
            let path_leaves = get_all_path_leafs(db.upcast(), item_use.use_path(syntax_db));
            let mut res = Vec::new();
            for path_leaf in path_leaves {
                let use_long_id = UseLongId(module_file_id, path_leaf.stable_ptr());
                let lookup_item_id =
                    LookupItemId::ModuleItem(ModuleItemId::Use(db.intern_use(use_long_id)));
                res.push(lookup_item_id);
            }
            res
        }
        _ => vec![],
    }
}

/// Given a position in a file, return the syntax node for the token at that position, and all the
/// lookup items above this node.
fn get_node_and_lookup_items(
    db: &(dyn SemanticGroup + 'static),
    file: FileId,
    position: Position,
) -> Option<(SyntaxNode, Vec<LookupItemId>)> {
    let mut res = Vec::new();
    let syntax_db = db.upcast();
    let filename = file.file_name(db.upcast());

    // Get syntax for file.
    let syntax = db.file_syntax(file).to_option().on_none(|| {
        eprintln!("Formatting failed. File '{filename}' does not exist.");
    })?;

    // Get file summary and content.
    let file_summary = db.file_summary(file).on_none(|| {
        eprintln!("Hover failed. File '{filename}' does not exist.");
    })?;
    let content = db.file_content(file).on_none(|| {
        eprintln!("Hover failed. File '{filename}' does not exist.");
    })?;

    // Find offset for position.
    let offset = position_to_offset(file_summary, position, &content)?;
    let node = syntax.as_syntax_node().lookup_offset(syntax_db, offset);

    // Find module.
    let module_id = find_node_module(db, file, node.clone()).on_none(|| {
        eprintln!("Hover failed. Failed to find module.");
    })?;
    let file_index = FileIndex(0);
    let module_file_id = ModuleFileId(module_id, file_index);

    // Find containing function.
    let mut item_node = node.clone();
    loop {
        for item in lookup_item_from_ast(db, module_file_id, item_node.clone()) {
            res.push(item);
        }
        match item_node.parent() {
            Some(next_node) => {
                item_node = next_node;
            }
            None => return Some((node, res)),
        }
    }
}

fn position_to_offset(
    file_summary: Arc<FileSummary>,
    position: Position,
    content: &str,
) -> Option<TextOffset> {
    let mut offset = *file_summary.line_offsets.get(position.line as usize).on_none(|| {
        eprintln!("Hover failed. Position out of bounds.");
    })?;
    let mut chars_it = offset.take_from(content).chars();
    for _ in 0..position.character {
        let c = chars_it.next().on_none(|| {
            eprintln!("Position does not exist.");
        })?;
        offset = offset.add_width(TextWidth::from_char(c));
    }
    Some(offset)
}

fn find_node_module(
    db: &dyn SemanticGroup,
    main_file: FileId,
    mut node: SyntaxNode,
) -> Option<ModuleId> {
    let modules: Vec<_> = db.file_modules(main_file).into_iter().flatten().collect();
    let mut module = *modules.first()?;
    let syntax_db = db.upcast();

    let mut inner_module_names = vec![];
    while let Some(parent) = node.parent() {
        node = parent;
        if node.kind(syntax_db) == SyntaxKind::ItemModule {
            inner_module_names.push(
                ast::ItemModule::from_syntax_node(syntax_db, node.clone())
                    .stable_ptr()
                    .name_green(syntax_db)
                    .identifier(syntax_db),
            );
        }
    }
    for name in inner_module_names.into_iter().rev() {
        let submodule = try_extract_matches!(
            db.module_item_by_name(module, name).ok()??,
            ModuleItemId::Submodule
        )?;
        module = ModuleId::Submodule(submodule);
    }
    Some(module)
}

/// If the node is an identifier, retrieves a hover hint for it.
fn get_identifier_hint(
    db: &(dyn SemanticGroup + 'static),
    lookup_item_id: LookupItemId,
    node: SyntaxNode,
) -> Option<String> {
    let syntax_db = db.upcast();
    if node.kind(syntax_db) != SyntaxKind::TokenIdentifier {
        return None;
    }
    let identifier = ast::TerminalIdentifier::from_syntax_node(syntax_db, node.parent().unwrap());
    let item = db.lookup_resolved_generic_item_by_ptr(lookup_item_id, identifier.stable_ptr())?;

    // TODO(spapini): Also include concrete item hints.
    // TODO(spapini): Format this better.
    Some(format!("`{}`", item.full_path(db)))
}

/// If the node is an expression, retrieves a hover hint for it.
fn get_expr_hint(
    db: &(dyn SemanticGroup + 'static),
    function_id: FunctionWithBodyId,
    node: SyntaxNode,
) -> Option<String> {
    let semantic_expr = nearest_semantic_expr(db, node, function_id)?;
    // Format the hover text.
    Some(format!("Type: `{}`", semantic_expr.ty().format(db)))
}

/// Returns the semantic expression for the current node.
fn nearest_semantic_expr(
    db: &dyn SemanticGroup,
    mut node: SyntaxNode,
    function_id: FunctionWithBodyId,
) -> Option<cairo_lang_semantic::Expr> {
    loop {
        let syntax_db = db.upcast();
        if ast::Expr::is_variant(node.kind(syntax_db)) {
            let expr_node = ast::Expr::from_syntax_node(syntax_db, node.clone());
            if let Some(expr_id) =
                db.lookup_expr_by_ptr(function_id, expr_node.stable_ptr()).to_option()
            {
                let semantic_expr = db.expr_semantic(function_id, expr_id);
                return Some(semantic_expr);
            }
        }
        node = node.parent()?;
    }
}

/// If the node is a pattern, retrieves a hover hint for it.
fn get_pat_hint(
    db: &(dyn SemanticGroup + 'static),
    function_id: FunctionWithBodyId,
    node: SyntaxNode,
) -> Option<String> {
    let semantic_pattern = nearest_semantic_pat(db, node, function_id)?;
    // Format the hover text.
    Some(format!("Type: `{}`", semantic_pattern.ty().format(db)))
}

/// Returns the semantic pattern for the current node.
fn nearest_semantic_pat(
    db: &dyn SemanticGroup,
    mut node: SyntaxNode,
    function_id: FunctionWithBodyId,
) -> Option<cairo_lang_semantic::Pattern> {
    loop {
        let syntax_db = db.upcast();
        if ast::Pattern::is_variant(node.kind(syntax_db)) {
            let pattern_node = ast::Pattern::from_syntax_node(syntax_db, node.clone());
            if let Some(pattern_id) =
                db.lookup_pat_by_ptr(function_id, pattern_node.stable_ptr()).to_option()
            {
                let semantic_pattern = db.pat_semantic(function_id, pattern_id);
                return Some(semantic_pattern);
            }
        }
        node = node.parent()?;
    }
}

fn update_crate_roots(db: &mut dyn SemanticGroup, crate_roots: Vec<(CrateLongId, Directory)>) {
    for (crate_long_id, crate_root) in crate_roots {
        let crate_id = db.intern_crate(crate_long_id);
        db.set_crate_root(crate_id, Some(crate_root));
    }
}

fn is_cairo_file_path(file_path: &Url) -> bool {
    file_path.path().ends_with(".cairo")
}

/// Gets a FileId from a URI.
fn file(db: &RootDatabase, uri: Url) -> FileId {
    match uri.scheme() {
        "file" => {
            let path = uri.to_file_path().unwrap();
            FileId::new(db, path)
        }
        "vfs" => {
            let id = uri.host_str().unwrap().parse::<usize>().unwrap();
            FileId::from_intern_id(id.into())
        }
        _ => panic!(),
    }
}

/// Gets the canonical URI for a file.
fn get_uri(db: &RootDatabase, file_id: FileId) -> Url {
    let virtual_file = match db.lookup_intern_file(file_id) {
        FileLongId::OnDisk(path) => return Url::from_file_path(path).unwrap(),
        FileLongId::Virtual(virtual_file) => virtual_file,
    };
    let uri = Url::parse(
        format!("vfs://{}/{}.cairo", file_id.as_intern_id().as_usize(), virtual_file.name).as_str(),
    )
    .unwrap();
    uri
}

/// Converts internal format diagnostics to LSP format.
fn get_diagnostics<T: DiagnosticEntry>(
    db: &T::DbType,
    diags: &mut Vec<Diagnostic>,
    diagnostics: &Diagnostics<T>,
) {
    for diagnostic in diagnostics.get_all() {
        let location = diagnostic.location(db);
        let message = diagnostic.format(db);
        let start =
            from_pos(location.span.start.position_in_file(db.upcast(), location.file_id).unwrap());
        let end =
            from_pos(location.span.start.position_in_file(db.upcast(), location.file_id).unwrap());
        diags.push(Diagnostic { range: Range { start, end }, message, ..Diagnostic::default() });
    }
}
