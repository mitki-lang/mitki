//! Node storage and tagged child pointers.
//!
//! Child pointers encode their kind in the low bits, so the pointed-to types
//! are aligned to 4 bytes to keep those bits available.

use std::marker::PhantomData;

use text_size::{TextRange, TextSize};

use crate::maybe_dangling::MaybeDangling;
use crate::nodes::TreeInner;
use crate::nodes::token::{Token, TokenRef};
use crate::{NodeOrToken, SyntaxKind, TokenAtOffset};

/// Owning storage for all nodes, lists, and child arrays.
///
/// Raw pointers stored in nodes and tokens are only valid while this storage
/// lives.
#[expect(unused, reason = "the fields are referenced by raw pointers")]
pub(crate) struct Nodes {
    pub(crate) nodes: MaybeDangling<Box<[Node]>>,
    pub(crate) node_children: MaybeDangling<Box<[NodeOrListOrToken]>>,
    pub(crate) lists: MaybeDangling<Box<[List]>>,
    pub(crate) list_children: MaybeDangling<Box<[*const Node]>>,
}

impl Nodes {
    /// Returns the root node (always index 0).
    #[inline]
    pub(crate) fn root(&self) -> &Node {
        &self.nodes[0]
    }
}

unsafe impl Send for Nodes {}
unsafe impl Sync for Nodes {}

/// Raw node stored in the tree arena.
#[repr(align(4))]
pub(crate) struct Node {
    pub(crate) parent: *const Self,
    pub(crate) children: *const NodeOrListOrToken,
    pub(crate) children_len: u32,
    pub(crate) kind: SyntaxKind,
    pub(crate) first_token: u32,
    pub(crate) last_token: u32,
}

impl Node {
    /// Returns the text range covered by this node.
    #[inline]
    pub(crate) fn text_range(&self, tree: &TreeInner) -> TextRange {
        TextRange::new(self.first_token(tree).start(), self.last_token(tree).end())
    }

    /// Returns the node text slice from the backing source.
    #[inline]
    pub(crate) fn text<'a>(&'a self, tree: &'a TreeInner) -> &'a str {
        let range = self.text_range(tree);
        unsafe { tree.text.get_unchecked(usize::from(range.start())..usize::from(range.end())) }
    }

    /// Returns the first token spanned by this node.
    #[inline]
    pub(crate) fn first_token(&self, tree: &TreeInner) -> TokenRef<'_> {
        TokenRef {
            ptr: unsafe { tree.tokens.as_ptr().add(self.first_token as usize) },
            _marker: PhantomData,
        }
    }

    /// Returns the last token spanned by this node.
    #[inline]
    pub(crate) fn last_token(&self, tree: &TreeInner) -> TokenRef<'_> {
        TokenRef {
            ptr: unsafe { tree.tokens.as_ptr().add(self.last_token as usize) },
            _marker: PhantomData,
        }
    }

    /// Returns the parent node if present.
    #[inline]
    pub(crate) fn parent(&self) -> Option<&Self> {
        unsafe { self.parent.as_ref() }
    }

    /// Returns the child slice (nodes, lists, and tokens).
    #[inline]
    pub(crate) fn children(&self) -> &[NodeOrListOrToken] {
        unsafe { std::slice::from_raw_parts(self.children, self.children_len as usize) }
    }

    #[inline]
    fn tokens_range<'a>(&self, tree: &'a TreeInner) -> &'a [Token] {
        let start = self.first_token(tree).ptr;
        let len = self.last_token - self.first_token;
        unsafe { std::slice::from_raw_parts(start, len as usize) }
    }

    /// Finds the token at the given offset within this node.
    #[inline]
    pub(crate) fn token_at_offset<'a>(
        &self,
        tree: &'a TreeInner,
        offset: TextSize,
    ) -> TokenAtOffset<TokenRef<'a>> {
        let tokens_range = self.tokens_range(tree);
        let index = tokens_range.partition_point(|token| token.end <= offset);
        if index >= tree.tokens.len() {
            return TokenAtOffset::None;
        }
        let second_token = unsafe { tree.tokens.as_ptr().add(self.first_token as usize + index) };
        let second_token = TokenRef { ptr: second_token, _marker: PhantomData };
        if second_token.end() <= offset {
            return TokenAtOffset::None;
        }
        if let Some(first_token) = second_token.prev_token(tree)
            && first_token.end() == offset
        {
            TokenAtOffset::Between(first_token, second_token)
        } else {
            TokenAtOffset::Single(second_token)
        }
    }

    /// Returns the smallest element that fully covers `range`.
    #[inline]
    pub(crate) fn covering_element<'a>(
        &'a self,
        tree: &'a TreeInner,
        range: TextRange,
    ) -> NodeOrToken<&'a Self, TokenRef<'a>> {
        let token = self
            .token_at_offset(tree, range.start())
            .right_biased()
            .expect("range is not inside the node");
        if token.text_range().contains_range(range) {
            return NodeOrToken::Token(token);
        }
        let mut ancestors = std::iter::successors(Some(token.parent()), |it| it.parent());
        let result = ancestors
            .find(|ancestor| ancestor.text_range(tree).contains_range(range))
            .expect("range is not inside the node");
        NodeOrToken::Node(result)
    }
}

/// The typed view of a tagged child pointer.
pub(crate) enum ChildKind<'a> {
    Token(TokenRef<'a>),
    Node(&'a Node),
    List(&'a List),
}

/// Tagged pointer to either a node, list, or token.
#[derive(Clone, Copy)]
pub(crate) struct NodeOrListOrToken(*const ());

impl NodeOrListOrToken {
    const TAG_MASK: usize = 0b11;
    const PTR_MASK: usize = !Self::TAG_MASK;
    const TOKEN_TAG: usize = 0b00;
    const NODE_TAG: usize = 0b01;
    const LIST_TAG: usize = 0b10;

    #[inline]
    pub(crate) fn new_token(ptr: *const Token) -> Self {
        Self(ptr.map_addr(|addr| addr | Self::TOKEN_TAG).cast())
    }

    #[inline]
    pub(crate) fn new_node(ptr: *const Node) -> Self {
        Self(ptr.map_addr(|addr| addr | Self::NODE_TAG).cast())
    }

    #[inline]
    pub(crate) fn new_list(ptr: *const List) -> Self {
        Self(ptr.map_addr(|addr| addr | Self::LIST_TAG).cast())
    }

    #[inline]
    pub(super) fn untagged_ptr(self) -> *const () {
        self.0.map_addr(|addr| addr & Self::PTR_MASK)
    }

    #[inline]
    #[allow(dead_code)]
    pub(crate) fn as_token(&self) -> Option<TokenRef<'_>> {
        if (self.0.addr() & Self::TAG_MASK) == Self::TOKEN_TAG {
            Some(TokenRef { ptr: self.untagged_ptr().cast::<Token>(), _marker: PhantomData })
        } else {
            None
        }
    }

    #[inline]
    #[allow(dead_code)]
    pub(crate) fn as_node(&self) -> Option<&Node> {
        if (self.0.addr() & Self::TAG_MASK) == Self::NODE_TAG {
            Some(unsafe { &*self.untagged_ptr().cast::<Node>() })
        } else {
            None
        }
    }

    #[inline]
    #[allow(dead_code)]
    pub(crate) fn as_list(&self) -> Option<&List> {
        if (self.0.addr() & Self::TAG_MASK) == Self::LIST_TAG {
            Some(unsafe { &*self.untagged_ptr().cast::<List>() })
        } else {
            None
        }
    }

    #[inline]
    pub(crate) fn kind(&self) -> ChildKind<'_> {
        let ptr = self.untagged_ptr();
        match self.0.addr() & Self::TAG_MASK {
            Self::TOKEN_TAG => {
                ChildKind::Token(TokenRef { ptr: ptr.cast::<Token>(), _marker: PhantomData })
            }
            Self::NODE_TAG => unsafe { ChildKind::Node(&*ptr.cast::<Node>()) },
            Self::LIST_TAG => unsafe { ChildKind::List(&*ptr.cast::<List>()) },
            _ => unsafe { std::hint::unreachable_unchecked() },
        }
    }
}

/// List node containing child nodes only.
#[repr(align(4))]
pub(crate) struct List {
    pub(crate) children: *const [*const Node],
}

impl List {
    /// Returns the list children as references.
    #[inline]
    pub(crate) fn children(&self) -> &[&Node] {
        // SAFETY: Layout is equivalent.
        unsafe { std::mem::transmute::<&[*const Node], &[&Node]>(&*self.children) }
    }
}
