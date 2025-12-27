//! Lossless, immutable syntax tree with parent pointers and attached trivia.
//!
//! The tree is built once and then navigated by offset-based, lifetime-guided
//! handles without allocation or refcounting.

/// Typed AST wrappers around the raw syntax tree.
pub mod ast;
mod builder;
mod maybe_dangling;
mod nodes;
mod syntax;
mod syntax_kind;
mod syntax_set;
mod trivia;

/// Incremental builder for constructing a `SyntaxTree`.
pub use builder::Builder;
/// Primary syntax tree API types and adapters.
pub use syntax::{
    NodeOrToken, Red, RedNode, RedNodePtr, RedToken, SyntaxElement, SyntaxNode, SyntaxToken,
    SyntaxTree, TokenAtOffset,
};
/// Token and node kinds used throughout the tree.
pub use syntax_kind::SyntaxKind;
/// Compact set for grouping `SyntaxKind` values.
pub use syntax_set::SyntaxSet;
/// Trivia pieces attached to tokens.
pub use trivia::{TriviaPiece, TriviaPieceKind};
