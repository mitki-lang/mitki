use text_size::TextSize;

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
