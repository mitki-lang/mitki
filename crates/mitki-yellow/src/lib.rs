pub mod ast;
mod builder;
pub mod cursor;
mod green;
mod red;
mod syntax_kind;

pub use builder::Builder;
pub use green::{
    Green, GreenChild, GreenNode, GreenToken, GreenTrivia, TriviaPiece, TriviaPieceKind,
};
pub use red::{Red, RedNode, RedNodePtr, RedToken, TokenAtOffset};
pub use syntax_kind::SyntaxKind;

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
pub enum NodeOrToken<N, T> {
    Node(N),
    Token(T),
}
