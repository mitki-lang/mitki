mod analysis;
mod server;

pub use server::Server;

struct FilePosition {
    pub file: mitki_db::File,
    pub offset: text_size::TextSize,
}

fn pick_best_token<'db>(
    db: &'db dyn salsa::Database,
    tokens: mitki_yellow::TokenAtOffset<'db>,
    f: impl Fn(mitki_yellow::SyntaxKind) -> usize,
) -> Option<mitki_yellow::RedToken<'db>> {
    tokens.max_by_key(move |t| f(t.kind(db)))
}
