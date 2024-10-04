pub struct Ide {
    db: Database,
    connection: lsp_server::Connection,
    #[allow(dead_code)]
    io_threads: lsp_server::IoThreads,
}

impl Ide {
    fn server_capabilities() -> lsp_types::ServerCapabilities {
        lsp_types::ServerCapabilities {
            text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
                lsp_types::TextDocumentSyncKind::FULL,
            )),
            ..lsp_types::ServerCapabilities::default()
        }
    }

    pub fn new() -> anyhow::Result<Self> {
        let (connection, io_threads) = lsp_server::Connection::stdio();

        let (id, _) = connection.initialize_start()?;

        let initialize_data = serde_json::json!({
            "capabilities": Self::server_capabilities(),

        });

        connection.initialize_finish(id, initialize_data)?;

        let sender = connection.sender.clone();
        Ok(Self { connection, io_threads, db: Database::new(sender) })
    }

    pub fn start(self) -> anyhow::Result<()> {
        let Ide { mut db, connection, io_threads } = self;

        for message in &connection.receiver {
            match message {
                lsp_server::Message::Request(request) => {
                    if connection.handle_shutdown(&request)? {
                        return Ok(());
                    }
                }
                lsp_server::Message::Response(_) => {}
                lsp_server::Message::Notification(notify) => {
                    if let Some(params) =
                        as_notification::<lsp_types::notification::DidOpenTextDocument>(&notify)
                    {
                        db.did_open(params)
                    } else if let Some(params) =
                        as_notification::<lsp_types::notification::DidChangeTextDocument>(&notify)
                    {
                        db.did_change(params)
                    }
                }
            }
        }

        io_threads.join()?;

        Ok(())
    }
}

#[salsa::db]
#[derive(Default, Clone)]
struct Salsa {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for Salsa {
    fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
}

struct Database {
    salsa: Salsa,
    files: std::collections::HashMap<lsp_types::Uri, mitki_db::File>,
    threads: threadpool::ThreadPool,
    sender: crossbeam_channel::Sender<lsp_server::Message>,
}

impl Database {
    fn new(sender: crossbeam_channel::Sender<lsp_server::Message>) -> Self {
        Self { salsa: <_>::default(), files: <_>::default(), threads: <_>::default(), sender }
    }

    fn did_open(&mut self, params: lsp_types::DidOpenTextDocumentParams) {
        use std::collections::hash_map::Entry;

        use salsa::Setter as _;

        let text_document = params.text_document;
        let path = text_document.uri;
        let text = text_document.text;

        let file = match self.files.entry(path.clone()) {
            Entry::Occupied(occupied) => {
                let file = *occupied.get();
                file.set_text(&mut self.salsa).to(text);
                file
            }
            Entry::Vacant(vacant) => {
                *vacant.insert(mitki_db::File::new(&self.salsa, path.path().as_str().into(), text))
            }
        };

        self.check_file(path, file);
    }

    fn did_change(&mut self, params: lsp_types::DidChangeTextDocumentParams) {
        use salsa::Setter as _;

        let file = self.files[&params.text_document.uri];
        let change = params.content_changes.into_iter().next().unwrap();

        file.set_text(&mut self.salsa).to(change.text);
        self.check_file(params.text_document.uri, file);
    }

    fn check_file(&self, uri: lsp_types::Uri, file: mitki_db::File) {
        let db = self.salsa.clone();
        let sender = self.sender.clone();

        self.threads.execute(move || {
            let text = file.text(&db);
            let diagnostics: Vec<mitki_db::Diagnostic> =
                mitki_db::check_file::accumulated::<mitki_db::Diagnostic>(&db, file);

            let line_index = line_index::LineIndex::new(text);

            let diagnostics = diagnostics
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
                        None,
                        diagnostic.message,
                        None,
                        None,
                    )
                })
                .collect::<Vec<_>>();

            let notification = new_notification::<lsp_types::notification::PublishDiagnostics>(
                lsp_types::PublishDiagnosticsParams { uri, diagnostics, version: None },
            );
            sender.send(lsp_server::Message::Notification(notification)).unwrap();
        });
    }
}

fn as_notification<T>(notify: &lsp_server::Notification) -> Option<T::Params>
where
    T: lsp_types::notification::Notification,
    T::Params: serde::de::DeserializeOwned,
{
    if notify.method != T::METHOD {
        return None;
    }

    Some(serde_json::from_value(notify.params.clone()).unwrap_or_else(|error| {
        panic!("Invalid notification\nMethod: {}\n error: {error}", notify.method)
    }))
}

fn new_notification<T>(params: T::Params) -> lsp_server::Notification
where
    T: lsp_types::notification::Notification,
{
    lsp_server::Notification {
        method: T::METHOD.to_owned(),
        params: serde_json::to_value(&params).unwrap(),
    }
}
