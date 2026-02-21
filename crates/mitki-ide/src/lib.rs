mod analysis;

pub use analysis::{Analysis, HoverResult, InlayHint};

#[derive(Clone, Copy)]
pub struct FilePosition {
    pub file: mitki_inputs::File,
    pub offset: text_size::TextSize,
}

pub fn extract_cursor_offset(text: &str) -> (text_size::TextSize, String) {
    let marker = "$0";
    let cursor_pos = text.find(marker).expect("Cursor marker not found");
    let mut new_text = String::with_capacity(text.len() - marker.len());
    new_text.push_str(&text[..cursor_pos]);
    new_text.push_str(&text[cursor_pos + marker.len()..]);
    (text_size::TextSize::from(cursor_pos as u32), new_text)
}

#[derive(Clone, Copy)]
struct NameAtOffset<'db> {
    token: mitki_yellow::SyntaxToken<'db>,
    node: mitki_yellow::SyntaxNode<'db>,
}

fn find_name_at_offset<'db>(
    root: mitki_yellow::SyntaxNode<'db>,
    offset: text_size::TextSize,
    is_valid_parent: impl Fn(mitki_yellow::SyntaxKind) -> bool,
) -> Option<NameAtOffset<'db>> {
    let tokens = root.token_at_offset(offset);
    let token = pick_best_token(tokens, |kind| match kind {
        mitki_yellow::SyntaxKind::NAME => 2,
        _ => 1,
    })?;

    let node = token.parent();
    if !is_valid_parent(node.kind()) {
        return None;
    }

    Some(NameAtOffset { token, node })
}

fn pick_best_token<'db>(
    tokens: mitki_yellow::TokenAtOffset<mitki_yellow::SyntaxToken<'db>>,
    f: impl Fn(mitki_yellow::SyntaxKind) -> usize,
) -> Option<mitki_yellow::SyntaxToken<'db>> {
    tokens.filter(|token| !token.is_trivia()).max_by_key(move |t| f(t.kind()))
}
