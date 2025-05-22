#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(clap::Parser)]
enum Options {
    #[command(alias = "r")]
    Run {
        path: camino::Utf8PathBuf,
    },
    Lsp,
}

fn main() -> anyhow::Result<()> {
    use clap::Parser as _;

    match Options::parse() {
        Options::Run { path } => {
            use std::io::Write as _;

            use anyhow::Context as _;

            let db = mitki_db::RootDatabase::default();
            let text = std::fs::read_to_string(&path)
                .with_context(|| format!("failed to read `{path}`"))?;

            let file = mitki_inputs::File::new(&db, path, text);
            let path = file.path(&db).as_str();
            let text = file.text(&db).as_str();

            let mut stderr = std::io::stderr().lock();
            let renderer = mitki_errors::Renderer::styled();

            for diagnostic in mitki_db::check_file(&db, file) {
                writeln!(stderr, "{}", diagnostic.render(&renderer, path, text))?;
            }

            Ok(())
        }
        Options::Lsp => mitki_lsp_server::Server::new()?.run(),
    }
}
