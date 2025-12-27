//! Owning tree storage.

use super::token::Token;
use crate::maybe_dangling::MaybeDangling;
use crate::nodes::Nodes;

/// Backing storage for a syntax tree.
pub(crate) struct TreeInner {
    /// The source text for all nodes/tokens.
    pub(crate) text: MaybeDangling<Box<str>>,
    /// Token storage; index 0 is a fake sentinel to make ranges uniform.
    pub(crate) tokens: MaybeDangling<Box<[Token]>>,
    /// Node and list storage plus child arrays.
    pub(crate) nodes: Nodes,
}
