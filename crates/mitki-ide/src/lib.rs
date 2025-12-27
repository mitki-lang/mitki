mod analysis;

pub use analysis::Analysis;

#[derive(Clone, Copy)]
pub struct FilePosition {
    pub file: mitki_inputs::File,
    pub offset: text_size::TextSize,
}

fn pick_best_token<'db>(
    tokens: mitki_yellow::TokenAtOffset<mitki_yellow::RedToken<'db>>,
    f: impl Fn(mitki_yellow::SyntaxKind) -> usize,
) -> Option<mitki_yellow::RedToken<'db>> {
    tokens.filter(|token| !token.is_trivia()).max_by_key(move |t| f(t.kind()))
}
