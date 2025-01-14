use std::collections::hash_map::Entry;

use anyhow::Result;
use mitki_inputs::File;
use salsa::Setter;
use text_size::TextRange;

use super::notifications::NotificationDispatcher;
use super::requests::RequestDispatcher;
use super::{Server, file_position};

pub(crate) fn request(server: &mut Server, request: lsp_server::Request) {
    RequestDispatcher::new(request, server)
        .on::<lsp_types::request::GotoDefinition>(handle_goto_definition)
        .on::<lsp_types::request::DocumentDiagnosticRequest>(handle_document_diagnostic)
        .on::<lsp_types::request::SemanticTokensFullRequest>(handle_semantic_tokens_full)
        .on::<lsp_types::request::SemanticTokensFullDeltaRequest>(handle_semantic_tokens_full_delta)
        .on::<lsp_types::request::SemanticTokensRangeRequest>(handle_semantic_tokens_range)
        .finish();
}

#[expect(clippy::needless_pass_by_value)]
fn handle_goto_definition(
    server: &mut Server,
    params: lsp_types::GotoDefinitionParams,
) -> Result<Option<lsp_types::GotoDefinitionResponse>> {
    let file_position = file_position(server, &params.text_document_position_params);
    let line_index = file_position.file.line_index(server.analysis.db());

    match server.analysis.goto_definition(file_position) {
        Some((origin_selection_range, target_range)) => {
            Ok(Some(lsp_types::GotoDefinitionResponse::Link(vec![lsp_types::LocationLink {
                origin_selection_range: to_lsp_range(line_index, origin_selection_range).into(),
                target_uri: params.text_document_position_params.text_document.uri.clone(),
                target_range: to_lsp_range(line_index, target_range),
                target_selection_range: to_lsp_range(line_index, target_range),
            }])))
        }
        None => Ok(None),
    }
}

#[expect(clippy::needless_pass_by_value)]
fn handle_document_diagnostic(
    server: &mut Server,
    params: lsp_types::DocumentDiagnosticParams,
) -> Result<lsp_types::DocumentDiagnosticReportResult> {
    let file = server.file(&params.text_document.uri);
    let line_index = file.line_index(server.analysis.db());

    let diagnostics = mitki_db::check_file::accumulated(server.analysis.db(), file)
        .into_iter()
        .map(|diagnostic: mitki_db::Diagnostic| {
            lsp_types::Diagnostic::new(
                to_lsp_range(line_index, diagnostic.range()),
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
        .collect();

    Ok(lsp_types::DocumentDiagnosticReport::Full(lsp_types::RelatedFullDocumentDiagnosticReport {
        related_documents: None,
        full_document_diagnostic_report: lsp_types::FullDocumentDiagnosticReport {
            result_id: None,
            items: diagnostics,
        },
    })
    .into())
}

fn handle_semantic_tokens_full(
    _server: &mut Server,
    _params: lsp_types::SemanticTokensParams,
) -> Result<Option<lsp_types::SemanticTokensResult>> {
    Ok(None)
}

fn handle_semantic_tokens_range(
    _server: &mut Server,
    _params: lsp_types::SemanticTokensRangeParams,
) -> Result<Option<lsp_types::SemanticTokensRangeResult>> {
    Ok(None)
}

fn handle_semantic_tokens_full_delta(
    _server: &mut Server,
    _params: lsp_types::SemanticTokensDeltaParams,
) -> Result<Option<lsp_types::SemanticTokensFullDeltaResult>> {
    Ok(None)
}

pub(crate) fn notification(server: &mut Server, notification: lsp_server::Notification) {
    NotificationDispatcher::new(notification, server)
        .on::<lsp_types::notification::DidOpenTextDocument>(handle_did_open_text_document)
        .on::<lsp_types::notification::DidChangeTextDocument>(handle_did_change_text_document)
        .finish();
}

fn handle_did_open_text_document(
    server: &mut Server,
    params: lsp_types::DidOpenTextDocumentParams,
) -> Result<()> {
    let lsp_types::TextDocumentItem { uri, language_id: _, version: _, text } =
        params.text_document;

    match server.files.entry(uri.clone()) {
        Entry::Occupied(occupied) => {
            let file = *occupied.get();
            file.set_text(server.analysis.db_mut()).to(text);
            file
        }
        Entry::Vacant(vacant) => {
            *vacant.insert(File::new(server.analysis.db(), uri.path().as_str().into(), text))
        }
    };

    Ok(())
}

fn handle_did_change_text_document(
    server: &mut Server,
    params: lsp_types::DidChangeTextDocumentParams,
) -> Result<()> {
    let file = server.file(&params.text_document.uri);
    let change = params.content_changes.into_iter().next().unwrap();
    file.set_text(server.analysis.db_mut()).to(change.text);
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
