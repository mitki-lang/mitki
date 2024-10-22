use std::collections::hash_map::Entry;

use anyhow::Result;
use mitki_db::File;
use salsa::Setter;

use super::notifications::NotificationDispatcher;
use super::requests::RequestDispatcher;
use super::{Server, file_position};

pub(crate) fn request(server: &mut Server, request: lsp_server::Request) {
    RequestDispatcher::new(request, server)
        .on::<lsp_types::request::GotoDefinition>(handle_goto_definition)
        .on::<lsp_types::request::DocumentDiagnosticRequest>(handle_document_diagnostic)
        .finish();
}

fn handle_goto_definition(
    server: &mut Server,
    params: lsp_types::GotoDefinitionParams,
) -> Result<Option<lsp_types::GotoDefinitionResponse>> {
    let file_position = file_position(server, params.text_document_position_params);
    server.analysis.goto_definition(file_position);
    Ok(None)
}

fn handle_document_diagnostic(
    server: &mut Server,
    params: lsp_types::DocumentDiagnosticParams,
) -> Result<lsp_types::DocumentDiagnosticReportResult> {
    let file = server.file(&params.text_document.uri);
    let line_index = file.line_index(server.analysis.db());
    let diagnostics =
        mitki_db::check_file::accumulated::<mitki_db::Diagnostic>(server.analysis.db(), file)
            .into_iter()
            .map(|diagnostic| {
                let start = line_index.line_col(diagnostic.range.start());
                let end = line_index.line_col(diagnostic.range.end());

                lsp_types::Diagnostic::new(
                    lsp_types::Range {
                        start: lsp_types::Position::new(start.line, start.col),
                        end: lsp_types::Position::new(end.line, end.col),
                    },
                    lsp_types::DiagnosticSeverity::ERROR.into(),
                    None,
                    Some("mitki".to_string()),
                    diagnostic.message,
                    None,
                    None,
                )
            })
            .collect::<Vec<_>>();

    Ok(lsp_types::DocumentDiagnosticReportResult::Report(
        lsp_types::DocumentDiagnosticReport::Full(lsp_types::RelatedFullDocumentDiagnosticReport {
            related_documents: None,
            full_document_diagnostic_report: lsp_types::FullDocumentDiagnosticReport {
                result_id: None,
                items: diagnostics,
            },
        }),
    ))
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
