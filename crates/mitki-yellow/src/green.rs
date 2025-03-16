use salsa::Database;
use text_size::{TextRange, TextSize};
use triomphe::ThinArc;

use crate::{NodeOrToken, SyntaxKind};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TriviaPieceKind {
    Whitespace,
    Newline,
    SingleLineComment,
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

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct GreenTrivia {
    ptr: Option<ThinArc<TextSize, TriviaPiece>>,
}

impl GreenTrivia {
    pub fn new(pieces: &[TriviaPiece]) -> Self {
        let total_len = pieces.iter().map(|piece| piece.len).sum();
        Self { ptr: Some(ThinArc::from_header_and_slice(total_len, pieces)) }
    }

    pub fn whitespace(len: u32) -> GreenTrivia {
        GreenTrivia::new(&[TriviaPiece::new(TriviaPieceKind::Whitespace, len.into())])
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

impl std::fmt::Debug for GreenTrivia {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GreenTrivia")
            .field("pieces", &self.pieces())
            .field("total_len", &self.len())
            .finish()
    }
}

#[salsa::interned(debug, constructor = alloc)]
pub struct GreenNode<'db> {
    pub kind: SyntaxKind,
    #[return_ref]
    pub children: Vec<GreenChild<'db>>,
    pub text_len: TextSize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GreenChild<'db> {
    Node { offset: TextSize, node: GreenNode<'db> },
    Token { offset: TextSize, token: GreenToken<'db> },
}

impl<'db> GreenChild<'db> {
    pub fn into_node(self) -> Option<GreenNode<'db>> {
        match self {
            GreenChild::Node { node, .. } => Some(node),
            GreenChild::Token { .. } => None,
        }
    }

    pub fn offset(&self) -> TextSize {
        match self {
            GreenChild::Node { offset, .. } | GreenChild::Token { offset, .. } => *offset,
        }
    }

    fn green(self) -> Green<'db> {
        match self {
            GreenChild::Node { node, .. } => Green::Node(node),
            GreenChild::Token { token, .. } => Green::Token(token),
        }
    }

    fn range(&self, db: &dyn Database) -> TextRange {
        TextRange::at(self.offset(), self.green().text_len(db))
    }
}

#[salsa::interned(debug)]
pub struct GreenToken<'db> {
    pub leading: GreenTrivia,
    pub kind: SyntaxKind,
    #[return_ref]
    pub text: Box<str>,
    pub trailing: GreenTrivia,
}

impl<'db> GreenToken<'db> {
    pub fn text_len(&self, db: &'db dyn Database) -> TextSize {
        TextSize::new(self.text(db).len() as u32)
    }

    pub fn leading_trailing_total_len(
        self,
        db: &'db dyn Database,
    ) -> (TextSize, TextSize, TextSize) {
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

impl<'db> GreenNode<'db> {
    pub fn new(db: &'db dyn Database, kind: SyntaxKind, children: Vec<Green<'db>>) -> Self {
        let mut text_len = TextSize::new(0);
        let children: Vec<_> = children
            .into_iter()
            .map(|green| {
                let offset = text_len;
                text_len += green.text_len(db);

                match green {
                    NodeOrToken::Node(node) => GreenChild::Node { offset, node },
                    NodeOrToken::Token(token) => GreenChild::Token { offset, token },
                }
            })
            .collect();

        Self::alloc(db, kind, children, text_len)
    }

    pub fn child_at_range(
        &self,
        db: &'db dyn Database,
        range: TextRange,
    ) -> Option<(TextSize, GreenChild<'db>)> {
        let children = self.children(db);
        let position = children
            .binary_search_by(|child| {
                let child_range = child.range(db);
                TextRange::ordering(child_range, range)
            })
            .unwrap_or_else(|position| position.saturating_sub(1));

        let child = children
            .get(position)
            .filter(|child| child.range(db).contains_range(range))
            .copied()?;

        Some((TextSize::new(position as u32), child))
    }
}

pub type Green<'db> = NodeOrToken<GreenNode<'db>, GreenToken<'db>>;

impl<'db> Green<'db> {
    pub fn text_len(&self, db: &dyn Database) -> TextSize {
        match self {
            NodeOrToken::Node(node) => node.text_len(db),
            NodeOrToken::Token(token) => token.text_len(db),
        }
    }

    pub fn into_token(self) -> Option<GreenToken<'db>> {
        match self {
            NodeOrToken::Node(_) => None,
            NodeOrToken::Token(token) => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use salsa::DatabaseImpl;

    use super::*;

    #[test]
    fn token_text() {
        let db = DatabaseImpl::new();

        let token = GreenToken::new(
            &db,
            GreenTrivia::whitespace(3),
            SyntaxKind::VAL_KW,
            "\n\t val \t\t",
            GreenTrivia::whitespace(3),
        );

        assert_eq!("\n\t val \t\t", token.text(&db).as_ref());
        assert_eq!("val", token.text_trimmed(&db));
    }
}
