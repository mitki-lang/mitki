use std::collections::hash_map::Entry;
use std::fs;

use anyhow::Result;
use lsp_types::notification::Notification as _;
use lsp_types::request::Request as _;
use mitki_inputs::File;
use text_size::{TextRange, TextSize};

use super::{Server, file_position};

pub(crate) async fn request(server: &mut Server, request: lsp_server::Request) {
    let lsp_server::Request { id, method, params } = request;

    if method == lsp_types::request::GotoDefinition::METHOD {
        let params = match crate::from_json(lsp_types::request::GotoDefinition::METHOD, &params) {
            Ok(params) => params,
            Err(error) => {
                server.respond(lsp_server::Response::new_err(
                    id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    error.to_string(),
                ));
                return;
            }
        };
        let result = handle_goto_definition(server, params).await;
        server.respond(crate::result_to_response::<lsp_types::request::GotoDefinition>(id, result));
        return;
    }

    if method == lsp_types::request::HoverRequest::METHOD {
        let params = match crate::from_json(lsp_types::request::HoverRequest::METHOD, &params) {
            Ok(params) => params,
            Err(error) => {
                server.respond(lsp_server::Response::new_err(
                    id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    error.to_string(),
                ));
                return;
            }
        };
        let result = handle_hover(server, params).await;
        server.respond(crate::result_to_response::<lsp_types::request::HoverRequest>(id, result));
        return;
    }

    if method == lsp_types::request::DocumentDiagnosticRequest::METHOD {
        let params = match crate::from_json(
            lsp_types::request::DocumentDiagnosticRequest::METHOD,
            &params,
        ) {
            Ok(params) => params,
            Err(error) => {
                server.respond(lsp_server::Response::new_err(
                    id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    error.to_string(),
                ));
                return;
            }
        };
        let result = handle_document_diagnostic(server, params).await;
        server.respond(crate::result_to_response::<lsp_types::request::DocumentDiagnosticRequest>(
            id, result,
        ));
        return;
    }

    if method == lsp_types::request::SemanticTokensFullRequest::METHOD {
        let params = match crate::from_json(
            lsp_types::request::SemanticTokensFullRequest::METHOD,
            &params,
        ) {
            Ok(params) => params,
            Err(error) => {
                server.respond(lsp_server::Response::new_err(
                    id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    error.to_string(),
                ));
                return;
            }
        };
        let result = handle_semantic_tokens_full(server, params).await;
        server.respond(crate::result_to_response::<lsp_types::request::SemanticTokensFullRequest>(
            id, result,
        ));
        return;
    }

    if method == lsp_types::request::SemanticTokensFullDeltaRequest::METHOD {
        let params = match crate::from_json(
            lsp_types::request::SemanticTokensFullDeltaRequest::METHOD,
            &params,
        ) {
            Ok(params) => params,
            Err(error) => {
                server.respond(lsp_server::Response::new_err(
                    id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    error.to_string(),
                ));
                return;
            }
        };
        let result = handle_semantic_tokens_full_delta(server, params).await;
        server.respond(crate::result_to_response::<
            lsp_types::request::SemanticTokensFullDeltaRequest,
        >(id, result));
        return;
    }

    if method == lsp_types::request::SemanticTokensRangeRequest::METHOD {
        let params =
            match crate::from_json(lsp_types::request::SemanticTokensRangeRequest::METHOD, &params)
            {
                Ok(params) => params,
                Err(error) => {
                    server.respond(lsp_server::Response::new_err(
                        id,
                        lsp_server::ErrorCode::InvalidParams as i32,
                        error.to_string(),
                    ));
                    return;
                }
            };
        let result = handle_semantic_tokens_range(server, params).await;
        server.respond(
            crate::result_to_response::<lsp_types::request::SemanticTokensRangeRequest>(id, result),
        );
        return;
    }

    if method == lsp_types::request::InlayHintRequest::METHOD {
        let params = match crate::from_json(lsp_types::request::InlayHintRequest::METHOD, &params) {
            Ok(params) => params,
            Err(error) => {
                server.respond(lsp_server::Response::new_err(
                    id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    error.to_string(),
                ));
                return;
            }
        };
        let result = handle_inlay_hint(server, params).await;
        server
            .respond(crate::result_to_response::<lsp_types::request::InlayHintRequest>(id, result));
        return;
    }

    eprintln!("unknown request method: {method}");
    server.respond(lsp_server::Response::new_err(
        id,
        lsp_server::ErrorCode::MethodNotFound as i32,
        "unknown request".to_owned(),
    ));
}

async fn handle_goto_definition(
    server: &mut Server,
    params: lsp_types::GotoDefinitionParams,
) -> Result<Option<lsp_types::GotoDefinitionResponse>> {
    let (file_position, line_index) =
        file_position(server, &params.text_document_position_params).await;

    match server.analysis.goto_definition(file_position).await {
        Some((origin_selection_range, target_range)) => {
            Ok(Some(lsp_types::GotoDefinitionResponse::Link(vec![lsp_types::LocationLink {
                origin_selection_range: to_lsp_range(&line_index, origin_selection_range).into(),
                target_uri: params.text_document_position_params.text_document.uri.clone(),
                target_range: to_lsp_range(&line_index, target_range),
                target_selection_range: to_lsp_range(&line_index, target_range),
            }])))
        }
        None => Ok(None),
    }
}

async fn handle_hover(
    server: &mut Server,
    params: lsp_types::HoverParams,
) -> Result<Option<lsp_types::Hover>> {
    let (file_position, line_index) =
        file_position(server, &params.text_document_position_params).await;

    match server.analysis.hover(file_position).await {
        Some(result) => Ok(Some(lsp_types::Hover {
            contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: result.contents,
            }),
            range: Some(to_lsp_range(&line_index, result.range)),
        })),
        None => Ok(None),
    }
}

async fn handle_document_diagnostic(
    server: &mut Server,
    params: lsp_types::DocumentDiagnosticParams,
) -> Result<lsp_types::DocumentDiagnosticReportResult> {
    let file = ensure_file(server, &params.text_document.uri)?;
    let diagnostics = diagnostics_for_file(server, file).await;

    Ok(lsp_types::DocumentDiagnosticReport::Full(lsp_types::RelatedFullDocumentDiagnosticReport {
        related_documents: None,
        full_document_diagnostic_report: lsp_types::FullDocumentDiagnosticReport {
            result_id: None,
            items: diagnostics,
        },
    })
    .into())
}

async fn handle_semantic_tokens_full(
    _server: &mut Server,
    _params: lsp_types::SemanticTokensParams,
) -> Result<Option<lsp_types::SemanticTokensResult>> {
    Ok(None)
}

async fn handle_semantic_tokens_range(
    _server: &mut Server,
    _params: lsp_types::SemanticTokensRangeParams,
) -> Result<Option<lsp_types::SemanticTokensRangeResult>> {
    Ok(None)
}

async fn handle_semantic_tokens_full_delta(
    _server: &mut Server,
    _params: lsp_types::SemanticTokensDeltaParams,
) -> Result<Option<lsp_types::SemanticTokensFullDeltaResult>> {
    Ok(None)
}

async fn handle_inlay_hint(
    server: &mut Server,
    params: lsp_types::InlayHintParams,
) -> Result<Option<Vec<lsp_types::InlayHint>>> {
    let file = server.file(&params.text_document.uri);
    let line_index = file.line_index(server.analysis.db()).await;

    let start = position_to_offset(&line_index, params.range.start);
    let end = position_to_offset(&line_index, params.range.end);
    let range = TextRange::new(start, end);

    let hints = server
        .analysis
        .inlay_hints(file, range)
        .await
        .into_iter()
        .map(|hint| {
            let position = line_index.line_col(hint.offset);
            lsp_types::InlayHint {
                position: lsp_types::Position::new(position.line, position.col),
                label: lsp_types::InlayHintLabel::String(hint.label),
                kind: Some(lsp_types::InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: None,
                padding_right: None,
                data: None,
            }
        })
        .collect();

    Ok(Some(hints))
}

pub(crate) async fn notification(server: &mut Server, notification: lsp_server::Notification) {
    let lsp_server::Notification { method, params } = notification;

    if method == lsp_types::notification::DidOpenTextDocument::METHOD {
        if let Some(params) =
            parse_notification::<lsp_types::notification::DidOpenTextDocument>(&method, &params)
            && let Err(error) = handle_did_open_text_document(server, params).await
        {
            eprintln!("{}: {}", method, error);
        }
        return;
    }

    if method == lsp_types::notification::DidChangeTextDocument::METHOD {
        if let Some(params) =
            parse_notification::<lsp_types::notification::DidChangeTextDocument>(&method, &params)
            && let Err(error) = handle_did_change_text_document(server, params).await
        {
            eprintln!("{}: {}", method, error);
        }
        return;
    }

    if method == lsp_types::notification::DidSaveTextDocument::METHOD {
        if let Some(params) =
            parse_notification::<lsp_types::notification::DidSaveTextDocument>(&method, &params)
            && let Err(error) = handle_did_save_text_document(server, params).await
        {
            eprintln!("{}: {}", method, error);
        }
        return;
    }

    if !method.starts_with("$/") {
        eprintln!("unhandled notification: {method}");
    }
}

fn parse_notification<N>(method: &str, params: &serde_json::Value) -> Option<N::Params>
where
    N: lsp_types::notification::Notification,
{
    match crate::from_json::<N::Params>(N::METHOD, params) {
        Ok(params) => Some(params),
        Err(error) => {
            eprintln!("invalid notification params for {method}: {error}");
            None
        }
    }
}

async fn handle_did_open_text_document(
    server: &mut Server,
    params: lsp_types::DidOpenTextDocumentParams,
) -> Result<()> {
    let lsp_types::TextDocumentItem { uri, language_id: _, version: _, text } =
        params.text_document;

    let file = match server.files.entry(uri.clone()) {
        Entry::Occupied(occupied) => {
            let file = *occupied.get();
            file.set_text(server.analysis.db_mut()).to(text);
            file
        }
        Entry::Vacant(vacant) => {
            *vacant.insert(File::new(server.analysis.db(), uri.path().as_str().into(), text))
        }
    };

    publish_diagnostics(server, uri, file).await?;

    Ok(())
}

async fn handle_did_change_text_document(
    server: &mut Server,
    params: lsp_types::DidChangeTextDocumentParams,
) -> Result<()> {
    let uri = params.text_document.uri;
    let file = ensure_file(server, &uri)?;
    let change = params.content_changes.into_iter().next().unwrap();
    file.set_text(server.analysis.db_mut()).to(change.text);
    publish_diagnostics(server, uri, file).await?;
    Ok(())
}

async fn handle_did_save_text_document(
    server: &mut Server,
    params: lsp_types::DidSaveTextDocumentParams,
) -> Result<()> {
    let uri = params.text_document.uri;
    let file = ensure_file(server, &uri)?;
    publish_diagnostics(server, uri, file).await?;
    Ok(())
}

fn ensure_file(server: &mut Server, uri: &lsp_types::Uri) -> Result<File> {
    if let Some(file) = server.files.get(uri).copied() {
        return Ok(file);
    }

    let mut path = uri.path().as_str();
    if path.starts_with('/')
        && path.as_bytes().get(2) == Some(&b':')
        && path.as_bytes().get(1).is_some_and(|b| b.is_ascii_alphabetic())
    {
        path = &path[1..];
    }
    let text = fs::read_to_string(path).unwrap_or_default();
    let file = File::new(server.analysis.db(), uri.path().as_str().into(), text);
    server.files.insert(uri.clone(), file);
    Ok(file)
}

async fn diagnostics_for_file(server: &Server, file: File) -> Vec<lsp_types::Diagnostic> {
    let line_index = file.line_index(server.analysis.db()).await;
    mitki_db::check_file(server.analysis.db(), file)
        .await
        .expect("failed to compute diagnostics")
        .iter()
        .map(|diagnostic: &mitki_db::Diagnostic| {
            lsp_types::Diagnostic::new(
                to_lsp_range(&line_index, diagnostic.range()),
                Some(match diagnostic.level() {
                    mitki_db::Level::Error => lsp_types::DiagnosticSeverity::ERROR,
                    mitki_db::Level::Warning => lsp_types::DiagnosticSeverity::WARNING,
                    mitki_db::Level::Info => lsp_types::DiagnosticSeverity::INFORMATION,
                    mitki_db::Level::Note | mitki_db::Level::Help => {
                        lsp_types::DiagnosticSeverity::HINT
                    }
                }),
                None,
                Some("mitki".to_string()),
                diagnostic.message().to_string(),
                None,
                None,
            )
        })
        .collect()
}

async fn publish_diagnostics(server: &Server, uri: lsp_types::Uri, file: File) -> Result<()> {
    let diagnostics = diagnostics_for_file(server, file).await;
    let params = lsp_types::PublishDiagnosticsParams { uri, diagnostics, version: None };
    let notification = lsp_server::Notification::new(
        lsp_types::notification::PublishDiagnostics::METHOD.to_string(),
        serde_json::to_value(params)?,
    );
    server.connection.sender.send(notification.into())?;
    Ok(())
}

fn to_lsp_range(line_index: &mitki_inputs::LineIndex, range: TextRange) -> lsp_types::Range {
    let start = line_index.line_col(range.start());
    let end = line_index.line_col(range.end());

    lsp_types::Range {
        start: lsp_types::Position::new(start.line, start.col),
        end: lsp_types::Position::new(end.line, end.col),
    }
}

fn position_to_offset(
    line_index: &mitki_inputs::LineIndex,
    position: lsp_types::Position,
) -> TextSize {
    let line_range = line_index.line(position.line).unwrap();
    let col = TextSize::from(position.character);
    let clamped_len = col.min(line_range.len());
    line_range.start() + clamped_len
}
