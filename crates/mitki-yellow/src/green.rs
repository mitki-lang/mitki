use salsa::Database;
use text_size::TextSize;
use triomphe::ThinArc;

use crate::{NodeOrToken, SyntaxKind};

pub type Green<'db> = NodeOrToken<GreenNode<'db>, GreenToken<'db>>;

impl Green<'_> {
    pub fn text_len(&self, db: &dyn Database) -> TextSize {
        match self {
            NodeOrToken::Node(node) => node.text_len(db),
            NodeOrToken::Token(token) => TextSize::new(token.text(db).len() as u32),
        }
    }
}

#[salsa::interned(constructor = alloc)]
pub struct GreenNode<'db> {
    pub kind: SyntaxKind,
    #[return_ref]
    pub children: Vec<Green<'db>>,
    pub text_len: TextSize,
}

impl<'db> GreenNode<'db> {
    pub fn new(db: &'db dyn Database, kind: SyntaxKind, children: Vec<Green<'db>>) -> Self {
        let text_len = children.iter().map(|child| child.text_len(db)).sum();
        Self::alloc(db, kind, children, text_len)
    }
}

#[salsa::interned]
pub struct GreenToken<'db> {
    pub leading: GreenTrivia,
    pub kind: SyntaxKind,
    #[return_ref]
    pub text: Box<str>,
    pub trailing: GreenTrivia,
}

impl<'db> GreenToken<'db> {
    fn leading_trailing_total_len(self, db: &'db dyn Database) -> (TextSize, TextSize, TextSize) {
        let leading_len = self.leading(db).len();
        let trailing_len = self.trailing(db).len();
        let total_len = self.text(db).len() as u32;

        (leading_len, trailing_len, total_len.into())
    }

    pub fn text_trimmed(self, db: &'db dyn Database) -> &'db str {
        let (leading_len, trailing_len, total_len) = self.leading_trailing_total_len(db);

        let start: usize = leading_len.into();
        let end: usize = (total_len - trailing_len).into();

        &self.text(db)[start..end]
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct GreenTrivia {
    ptr: Option<ThinArc<TextSize, TriviaPiece>>,
}

impl std::fmt::Debug for GreenTrivia {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GreenTrivia")
            .field("pieces", &self.pieces())
            .field("total_len", &self.len())
            .finish()
    }
}

impl GreenTrivia {
    pub fn new(pieces: &[TriviaPiece]) -> Self {
        let total_len = pieces.iter().map(|piece| piece.len).sum();
        Self { ptr: Some(ThinArc::from_header_and_slice(total_len, pieces)) }
    }

    pub const fn empty() -> Self {
        Self { ptr: None }
    }

    pub fn len(&self) -> TextSize {
        match self.ptr {
            None => TextSize::new(0),
            Some(ref ptr) => ptr.header.header,
        }
    }

    pub fn pieces(&self) -> &[TriviaPiece] {
        match &self.ptr {
            None => &[],
            Some(ptr) => &ptr.slice,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TriviaPiece {
    pub kind: TriviaPieceKind,
    pub len: TextSize,
}

impl TriviaPiece {
    pub fn new(kind: TriviaPieceKind, len: TextSize) -> Self {
        Self { kind, len }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TriviaPieceKind {
    Whitespace,
    SingleLineComment,
}

#[cfg(test)]
mod tests {
    use salsa::DatabaseImpl;

    use super::*;

    fn whitespace(len: u32) -> GreenTrivia {
        GreenTrivia::new(&[TriviaPiece::new(TriviaPieceKind::Whitespace, len.into())])
    }

    #[test]
    fn token_text() {
        let db = DatabaseImpl::new();

        let token = GreenToken::new(
            &db,
            whitespace(3),
            SyntaxKind::VAL_KW,
            "\n\t val \t\t".into(),
            whitespace(3),
        );

        assert_eq!("\n\t val \t\t", token.text(&db).as_ref());
        assert_eq!("val", token.text_trimmed(&db));
    }
}
