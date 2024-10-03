mod builder;
mod green;
mod symbol;
mod syntax_kind;

pub use builder::Builder;
pub use green::{Green, GreenNode, GreenToken, GreenTrivia, TriviaPiece, TriviaPieceKind};
pub use symbol::Symbol;
pub use syntax_kind::SyntaxKind;

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub enum NodeOrToken<N, T> {
    Node(N),
    Token(T),
}
