//! Trivia pieces attached to tokens.

use text_size::TextSize;

/// Kinds of trivia stored alongside tokens.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TriviaPieceKind {
    Whitespace,
    Newline,
    SingleLineComment,
}

/// A trivia fragment with its kind and length.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct TriviaPiece {
    pub kind: TriviaPieceKind,
    pub len: TextSize,
}

impl TriviaPiece {
    /// Creates a new trivia piece with the given kind and length.
    pub fn new(kind: TriviaPieceKind, len: TextSize) -> Self {
        Self { kind, len }
    }
}
