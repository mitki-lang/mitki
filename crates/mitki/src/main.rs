mod db;

use anyhow::Context;
use camino::Utf8PathBuf;
use clap::Parser;
use db::{File, check_file};
use mitki_errors::{Diagnostic, Renderer};
use salsa::DatabaseImpl;

#[derive(Parser)]
enum Options {
    Run { path: Utf8PathBuf },
}

fn main() -> anyhow::Result<()> {
    match Options::parse() {
        Options::Run { path } => {
            let db = DatabaseImpl::default();
            let text = std::fs::read_to_string(&path)
                .with_context(|| format!("failed to read `{path}`"))?;

            let renderer = Renderer::styled();

            let file = File::new(&db, path, text);
            let diagnostics = check_file::accumulated::<Diagnostic>(&db, file);

            let path = file.path(&db).as_str();
            let text = file.text(&db).as_str();

            for diagnosti in diagnostics {
                eprintln!("{}", diagnosti.render(&renderer, path, text));
            }

            Ok(())
        }
    }
}
