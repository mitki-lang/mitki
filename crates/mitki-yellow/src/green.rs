use text_size::TextSize;
use triomphe::ThinArc;

use crate::{NodeOrToken, Symbol, SyntaxKind};

pub type Green<'db> = NodeOrToken<GreenNode<'db>, GreenToken<'db>>;

#[salsa::interned]
pub struct GreenNode<'db> {
    pub kind: SyntaxKind,
    pub children: Vec<Green<'db>>,
}

#[salsa::interned]
pub struct GreenToken<'db> {
    pub leading: GreenTrivia,
    pub kind: SyntaxKind,
    pub text: Symbol<'db>,
    pub trailing: GreenTrivia,
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct GreenTrivia {
    ptr: Option<ThinArc<TextSize, TriviaPiece>>,
}

impl GreenTrivia {
    pub fn whitespaces(len: TextSize) -> Self {
        Self::new(&[TriviaPiece::new(TriviaPieceKind::Whitespace, len)])
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
