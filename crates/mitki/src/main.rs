#[cfg(not(miri))]
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

#[tokio::main(flavor = "multi_thread")]
async fn main() -> anyhow::Result<()> {
    use clap::Parser as _;

    match Options::parse() {
        Options::Run { path } => {
            use std::io::Write as _;

            use anyhow::Context as _;

            let db = mitki_db::RootDatabase::default();
            let text = std::fs::read_to_string(&path)
                .with_context(|| format!("failed to read `{path}`"))?;

            let file = mitki_inputs::File::new(&db, path, text);
            let path = file.path(&db);
            let text = file.text(&db);

            let mut stderr = std::io::stderr().lock();
            let renderer = mitki_errors::Renderer::styled();

            for diagnostic in
                mitki_db::check_file(&db, file).await.expect("failed to compute diagnostics").iter()
            {
                writeln!(stderr, "{}", diagnostic.render(&renderer, path.as_str(), text.as_ref()))?;
            }

            Ok(())
        }
        Options::Lsp => mitki_lsp_server::Server::new()?.run().await,
    }
}
