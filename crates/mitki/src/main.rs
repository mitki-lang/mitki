use anyhow::Context;
use camino::Utf8PathBuf;
use clap::Parser;
use mitki_db::check_file;
use mitki_errors::{Diagnostic, Renderer};
use mitki_inputs::File;
use salsa::DatabaseImpl;

#[derive(Parser)]
enum Options {
    Run { path: Utf8PathBuf },
    Ide,
}

fn main() -> anyhow::Result<()> {
    match Options::parse() {
        Options::Run { path } => {
            use std::io::Write as _;

            let db = DatabaseImpl::default();
            let text = std::fs::read_to_string(&path)
                .with_context(|| format!("failed to read `{path}`"))?;

            let renderer = Renderer::styled();

            let file = File::new(&db, path, text);
            let diagnostics = check_file::accumulated::<Diagnostic>(&db, file);

            let path = file.path(&db).as_str();
            let text = file.text(&db).as_str();

            let stderr = std::io::stderr();
            let mut lock = stderr.lock();

            for diagnostic in diagnostics {
                writeln!(lock, "{}", diagnostic.render(&renderer, path, text))?;
            }

            Ok(())
        }
        Options::Ide => mitki_lsp_server::Server::new()?.run(),
    }
}
