mod api;
mod notifications;
mod requests;

use std::collections::HashMap;

use anyhow::Result;
use mitki_db::File;
use text_size::TextSize;

use crate::FilePosition;
use crate::analysis::Analysis;

pub struct Server {
    connection: lsp_server::Connection,
    io_threads: lsp_server::IoThreads,
    analysis: Analysis,
    files: HashMap<lsp_types::Uri, File>,
}

impl Server {
    fn server_capabilities() -> lsp_types::ServerCapabilities {
        lsp_types::ServerCapabilities {
            diagnostic_provider: Some(lsp_types::DiagnosticServerCapabilities::Options(
                lsp_types::DiagnosticOptions {
                    identifier: Some(env!("CARGO_PKG_NAME").to_string()),
                    ..Default::default()
                },
            )),
            text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
                lsp_types::TextDocumentSyncKind::FULL,
            )),
            definition_provider: Some(lsp_types::OneOf::Left(true)),
            ..lsp_types::ServerCapabilities::default()
        }
    }

    pub fn new() -> Result<Self> {
        let (connection, io_threads) = lsp_server::Connection::stdio();

        let (initialize_id, _initialize_params) = match connection.initialize_start() {
            Ok(it) => it,
            Err(protocol_error) => {
                if protocol_error.channel_is_disconnected() {
                    io_threads.join()?;
                }
                return Err(protocol_error.into());
            }
        };

        let initialize_data = serde_json::json!({
            "capabilities": Self::server_capabilities(),
        });

        if let Err(protocol_error) = connection.initialize_finish(initialize_id, initialize_data) {
            if protocol_error.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(protocol_error.into());
        }

        Ok(Self {
            connection,
            io_threads,
            analysis: Analysis::default(),
            files: HashMap::default(),
        })
    }

    fn file(&self, uri: &lsp_types::Uri) -> File {
        *self.files.get(uri).unwrap()
    }

    fn respond(&mut self, response: lsp_server::Response) {
        self.connection.sender.send(response.into()).unwrap();
    }

    pub fn run(mut self) -> Result<()> {
        let receiver = self.connection.receiver.clone();
        for message in &receiver {
            match message {
                lsp_server::Message::Request(request) => api::request(&mut self, request),
                lsp_server::Message::Response(_response) => {}
                lsp_server::Message::Notification(notification) => {
                    api::notification(&mut self, notification)
                }
            }
        }

        self.io_threads.join().map_err(Into::into)
    }
}

fn from_json<T: serde::de::DeserializeOwned>(
    what: &'static str,
    json: &serde_json::Value,
) -> Result<T> {
    serde_json::from_value(json.clone())
        .map_err(|e| anyhow::format_err!("Failed to deserialize {what}: {e}; {json}"))
}

fn result_to_response<R>(
    id: lsp_server::RequestId,
    result: Result<R::Result>,
) -> lsp_server::Response
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
    R::Result: serde::Serialize,
{
    match result {
        Ok(resp) => lsp_server::Response::new_ok(id, &resp),
        Err(error) => lsp_server::Response::new_err(id, -32603, error.to_string()),
    }
}

fn file_position(server: &Server, tdpp: lsp_types::TextDocumentPositionParams) -> FilePosition {
    let file = server.file(&tdpp.text_document.uri);
    let offset = {
        let line_index = file.line_index(server.analysis.db());
        let line_range = line_index.line(tdpp.position.line).unwrap();
        let col = TextSize::from(tdpp.position.character);
        let clamped_len = col.min(line_range.len());
        line_range.start() + clamped_len
    };

    FilePosition { file, offset }
}
