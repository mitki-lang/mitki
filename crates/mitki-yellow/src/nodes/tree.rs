use super::token::Token;
use crate::maybe_dangling::MaybeDangling;
use crate::nodes::Nodes;

pub(crate) struct TreeInner {
    pub(crate) text: MaybeDangling<Box<str>>,
    /// Always starts with a fake token.
    pub(crate) tokens: MaybeDangling<Box<[Token]>>,
    pub(crate) nodes: Nodes,
}
