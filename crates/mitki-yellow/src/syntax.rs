//! Public syntax tree API built on immutable, parent-linked nodes.

use std::fmt;
use std::marker::PhantomData;

use text_size::{TextRange, TextSize};

use crate::SyntaxKind;
use crate::nodes::{ChildKind, List, Node, NodeOrListOrToken, TokenRef, TokenRefIter, TreeInner};

/// Owned syntax tree for a single source text.
pub struct SyntaxTree<'db> {
    pub(crate) tree: TreeInner,
    pub(crate) _marker: PhantomData<&'db ()>,
}

impl<'db> SyntaxTree<'db> {
    /// Returns the root syntax node.
    #[inline]
    pub fn root(&'db self) -> SyntaxNode<'db> {
        SyntaxNode { tree: &self.tree, node: self.tree.nodes.root() }
    }

    /// Returns the full source text for this tree.
    #[inline]
    pub fn text(&self) -> &str {
        &self.tree.text
    }
}

impl fmt::Debug for SyntaxTree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxTree").field("text_len", &self.text().len()).finish_non_exhaustive()
    }
}

unsafe impl salsa::Update for SyntaxTree<'_> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        let old_value = unsafe { &mut *old_pointer };
        if old_value.text() == new_value.text() {
            false
        } else {
            *old_value = new_value;
            true
        }
    }
}

/// Token handle tied to the lifetime of the tree.
#[derive(Clone, Copy)]
pub struct SyntaxToken<'a> {
    tree: &'a TreeInner,
    token: TokenRef<'a>,
}

impl<'a> SyntaxToken<'a> {
    /// Returns this token's kind.
    #[inline]
    pub fn kind(self) -> SyntaxKind {
        self.token.get().kind
    }

    /// Returns `true` if this token is trivia.
    #[inline]
    pub fn is_trivia(self) -> bool {
        self.kind().is_trivia()
    }

    /// Returns the token text range including attached trivia.
    #[inline]
    pub fn text_range(self) -> TextRange {
        let first_token = self.leading_trivia().next().unwrap_or(self);
        let last_token = self.trailing_trivia().next_back().unwrap_or(self);
        TextRange::new(first_token.token.start(), last_token.token.end())
    }

    /// Returns the range including attached trivia.
    #[inline]
    pub fn range(self) -> TextRange {
        self.text_range()
    }

    /// Returns the token text range excluding trivia.
    #[inline]
    pub fn trimmed_range(self) -> TextRange {
        self.token.text_range()
    }

    /// Returns the token text including trivia.
    #[inline]
    pub fn text(self) -> &'a str {
        &self.tree.text[self.text_range()]
    }

    /// Returns the token text excluding trivia.
    #[inline]
    pub fn text_trimmed(self) -> &'a str {
        self.token.text(self.tree)
    }

    /// Returns the previous token if any.
    #[inline]
    pub fn prev_token(self) -> Option<Self> {
        Some(Self { tree: self.tree, token: self.token.prev_token(self.tree)? })
    }

    /// Returns the next token if any.
    #[inline]
    pub fn next_token(self) -> Option<Self> {
        Some(Self { tree: self.tree, token: self.token.next_token(self.tree)? })
    }

    /// Iterates over leading trivia tokens.
    #[inline]
    pub fn leading_trivia(self) -> TriviaIter<'a> {
        TriviaIter { tree: self.tree, tokens: self.token.leading_trivia() }
    }

    /// Iterates over trailing trivia tokens.
    #[inline]
    pub fn trailing_trivia(self) -> TriviaIter<'a> {
        TriviaIter { tree: self.tree, tokens: self.token.trailing_trivia() }
    }

    /// Returns the parent node.
    #[inline]
    pub fn parent(self) -> SyntaxNode<'a> {
        SyntaxNode { tree: self.tree, node: self.token.parent() }
    }

    /// Returns an iterator of parent nodes, starting from the immediate parent.
    #[inline]
    pub fn parent_ancestors(self) -> impl Iterator<Item = SyntaxNode<'a>> + Clone {
        self.parent().ancestors()
    }
}

/// Iterator over trivia tokens.
#[derive(Clone)]
pub struct TriviaIter<'a> {
    tree: &'a TreeInner,
    tokens: TokenRefIter<'a>,
}

impl<'a> Iterator for TriviaIter<'a> {
    type Item = SyntaxToken<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        Some(SyntaxToken { tree: self.tree, token: self.tokens.next()? })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.tokens.len();
        (len, Some(len))
    }

    #[inline]
    fn last(mut self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        self.next_back()
    }
}

impl<'a> DoubleEndedIterator for TriviaIter<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        Some(SyntaxToken { tree: self.tree, token: self.tokens.next_back()? })
    }
}

impl ExactSizeIterator for TriviaIter<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.tokens.len()
    }
}

/// Node handle tied to the lifetime of the tree.
#[derive(Clone, Copy)]
pub struct SyntaxNode<'a> {
    tree: &'a TreeInner,
    node: &'a Node,
}

impl<'a> SyntaxNode<'a> {
    /// Returns this node's kind.
    #[inline]
    pub fn kind(self) -> SyntaxKind {
        self.node.kind
    }

    /// Returns the first token spanned by this node.
    #[inline]
    pub fn first_token(self) -> SyntaxToken<'a> {
        SyntaxToken { tree: self.tree, token: self.node.first_token(self.tree) }
    }

    /// Returns the last token spanned by this node.
    #[inline]
    pub fn last_token(self) -> SyntaxToken<'a> {
        SyntaxToken { tree: self.tree, token: self.node.last_token(self.tree) }
    }

    /// Returns the text range covered by this node.
    #[inline]
    pub fn text_range(self) -> TextRange {
        self.node.text_range(self.tree)
    }

    /// Returns the range covered by this node.
    #[inline]
    pub fn range(self) -> TextRange {
        self.text_range()
    }

    /// Returns the range with leading/trailing trivia trimmed away.
    #[inline]
    pub fn trimmed_range(self) -> TextRange {
        non_trivia_range(self)
    }

    /// Returns the text slice covered by this node.
    #[inline]
    pub fn text(self) -> &'a str {
        self.node.text(self.tree)
    }

    /// Returns the text slice excluding leading/trailing trivia.
    #[inline]
    pub fn text_trimmed(self) -> &'a str {
        &self.tree.text[self.trimmed_range()]
    }

    /// Returns the parent node if any.
    #[inline]
    pub fn parent(self) -> Option<Self> {
        Some(Self { tree: self.tree, node: self.node.parent()? })
    }

    /// Returns an iterator of ancestors starting from this node.
    #[inline]
    pub fn ancestors(self) -> impl Iterator<Item = SyntaxNode<'a>> + Clone {
        std::iter::successors(Some(self), |it| it.parent())
    }

    /// Iterates children including tokens and list nodes.
    #[inline]
    pub fn children_with_tokens_and_lists(self) -> ChildrenWithTokensAndLists<'a> {
        ChildrenWithTokensAndLists { tree: self.tree, children: self.node.children().iter() }
    }

    /// Iterates children including list nodes, skipping tokens.
    #[inline]
    pub fn children_with_lists(self) -> ChildrenWithLists<'a> {
        ChildrenWithLists { inner: self.children_with_tokens_and_lists() }
    }

    /// Iterates children including tokens, flattening lists.
    #[inline]
    pub fn children_with_tokens(self) -> ChildrenWithTokens<'a> {
        ChildrenWithTokens {
            active_list_iter: None,
            children: self.children_with_tokens_and_lists(),
        }
    }

    #[inline]
    /// Iterates child nodes, skipping tokens.
    pub fn children(self) -> Children<'a> {
        Children { inner: self.children_with_tokens() }
    }

    /// Returns the token at the given offset, if any.
    #[inline]
    pub fn token_at_offset(self, offset: TextSize) -> TokenAtOffset<SyntaxToken<'a>> {
        self.node
            .token_at_offset(self.tree, offset)
            .map(|token| SyntaxToken { tree: self.tree, token })
    }

    /// Returns the smallest element that fully covers `range`.
    #[inline]
    pub fn covering_element(self, range: TextRange) -> SyntaxElement<'a> {
        match self.node.covering_element(self.tree, range) {
            NodeOrToken::Node(node) => NodeOrToken::Node(SyntaxNode { tree: self.tree, node }),
            NodeOrToken::Token(token) => NodeOrToken::Token(SyntaxToken { tree: self.tree, token }),
        }
    }
}

/// Node or token element inside the tree.
pub type SyntaxElement<'a> = NodeOrToken<SyntaxNode<'a>, SyntaxToken<'a>>;

/// List node wrapper that provides indexed access to child nodes.
#[derive(Clone, Copy)]
pub struct SyntaxList<'a> {
    list: &'a List,
    tree: &'a TreeInner,
}

impl<'a> SyntaxList<'a> {
    /// Returns the number of children in the list.
    #[inline]
    pub fn len(self) -> usize {
        self.list.children().len()
    }

    /// Returns `true` when the list is empty.
    #[inline]
    pub fn is_empty(self) -> bool {
        self.len() == 0
    }

    /// Returns the child at `idx`, if present.
    #[inline]
    pub fn get(self, idx: usize) -> Option<SyntaxNode<'a>> {
        Some(SyntaxNode { tree: self.tree, node: self.list.children().get(idx)? })
    }

    /// Returns the child at `idx`, panicking if out of bounds.
    #[inline]
    #[track_caller]
    pub fn at(self, idx: usize) -> SyntaxNode<'a> {
        SyntaxNode { tree: self.tree, node: self.list.children()[idx] }
    }

    /// Returns an iterator over list children.
    #[inline]
    pub fn iter(self) -> SyntaxListIter<'a> {
        SyntaxListIter { tree: self.tree, iter: self.list.children().iter() }
    }
}

/// Iterator over `SyntaxList` children.
pub struct SyntaxListIter<'a> {
    tree: &'a TreeInner,
    iter: std::slice::Iter<'a, &'a Node>,
}

impl Clone for SyntaxListIter<'_> {
    #[inline]
    fn clone(&self) -> Self {
        Self { tree: self.tree, iter: self.iter.clone() }
    }
}

impl<'a> Iterator for SyntaxListIter<'a> {
    type Item = SyntaxNode<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|&node| SyntaxNode { tree: self.tree, node })
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }

    #[inline]
    fn last(mut self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        self.next_back()
    }
}

impl<'a> DoubleEndedIterator for SyntaxListIter<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.iter.next_back().map(|&node| SyntaxNode { tree: self.tree, node })
    }
}

impl ExactSizeIterator for SyntaxListIter<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.iter.len()
    }
}

/// Child element that may be a node, token, or list.
#[derive(Clone, Copy)]
pub enum NodeOrTokenOrList<'a> {
    Node(SyntaxNode<'a>),
    Token(SyntaxToken<'a>),
    List(SyntaxList<'a>),
}

/// Iterator over children including tokens and lists.
pub struct ChildrenWithTokensAndLists<'a> {
    tree: &'a TreeInner,
    children: std::slice::Iter<'a, NodeOrListOrToken>,
}

impl Clone for ChildrenWithTokensAndLists<'_> {
    #[inline]
    fn clone(&self) -> Self {
        Self { tree: self.tree, children: self.children.clone() }
    }
}

impl<'a> ChildrenWithTokensAndLists<'a> {
    #[inline]
    fn map_child(&self, child: Option<&'a NodeOrListOrToken>) -> Option<NodeOrTokenOrList<'a>> {
        let tree = self.tree;
        child.map(|child| match child.kind() {
            ChildKind::Token(token) => NodeOrTokenOrList::Token(SyntaxToken { tree, token }),
            ChildKind::Node(node) => NodeOrTokenOrList::Node(SyntaxNode { tree, node }),
            ChildKind::List(list) => NodeOrTokenOrList::List(SyntaxList { list, tree }),
        })
    }
}

impl<'a> Iterator for ChildrenWithTokensAndLists<'a> {
    type Item = NodeOrTokenOrList<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let child = self.children.next();
        self.map_child(child)
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.children.size_hint()
    }

    #[inline]
    fn last(mut self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        self.next_back()
    }
}

impl<'a> DoubleEndedIterator for ChildrenWithTokensAndLists<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        let child = self.children.next_back();
        self.map_child(child)
    }
}

impl ExactSizeIterator for ChildrenWithTokensAndLists<'_> {
    #[inline]
    fn len(&self) -> usize {
        self.children.len()
    }
}

/// Child element that is either a node or list.
pub enum NodeOrList<'a> {
    Node(SyntaxNode<'a>),
    List(SyntaxList<'a>),
}

/// Iterator over children including lists but skipping tokens.
pub struct ChildrenWithLists<'a> {
    inner: ChildrenWithTokensAndLists<'a>,
}

impl Clone for ChildrenWithLists<'_> {
    #[inline]
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<'a> ChildrenWithLists<'a> {
    #[inline]
    fn filter_child(child: NodeOrTokenOrList<'a>) -> Option<NodeOrList<'a>> {
        match child {
            NodeOrTokenOrList::Node(it) => Some(NodeOrList::Node(it)),
            NodeOrTokenOrList::List(it) => Some(NodeOrList::List(it)),
            NodeOrTokenOrList::Token(_) => None,
        }
    }

    #[inline]
    fn iter(self) -> impl DoubleEndedIterator<Item = NodeOrList<'a>> {
        self.inner.filter_map(|child| match child {
            NodeOrTokenOrList::Node(it) => Some(NodeOrList::Node(it)),
            NodeOrTokenOrList::List(it) => Some(NodeOrList::List(it)),
            NodeOrTokenOrList::Token(_) => None,
        })
    }
}

impl<'a> Iterator for ChildrenWithLists<'a> {
    type Item = NodeOrList<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.find_map(Self::filter_child)
    }

    #[inline]
    fn fold<B, F>(self, init: B, f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        self.iter().fold(init, f)
    }

    #[inline]
    fn for_each<F>(self, f: F)
    where
        Self: Sized,
        F: FnMut(Self::Item),
    {
        self.iter().for_each(f);
    }

    #[inline]
    fn last(mut self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        self.next_back()
    }
}

impl<'a> DoubleEndedIterator for ChildrenWithLists<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.by_ref().rev().find_map(Self::filter_child)
    }

    #[inline]
    fn rfold<B, F>(self, init: B, f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        self.iter().rfold(init, f)
    }
}

/// Iterator over children including tokens, flattening list nodes.
pub struct ChildrenWithTokens<'a> {
    active_list_iter: Option<SyntaxListIter<'a>>,
    children: ChildrenWithTokensAndLists<'a>,
}

impl Clone for ChildrenWithTokens<'_> {
    #[inline]
    fn clone(&self) -> Self {
        Self { active_list_iter: self.active_list_iter.clone(), children: self.children.clone() }
    }
}

impl<'a> Iterator for ChildrenWithTokens<'a> {
    type Item = SyntaxElement<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(list_iter) = &mut self.active_list_iter {
                match list_iter.next() {
                    Some(list_item) => return Some(SyntaxElement::Node(list_item)),
                    None => self.active_list_iter = None,
                }
            }

            match self.children.next()? {
                NodeOrTokenOrList::Node(it) => return Some(SyntaxElement::Node(it)),
                NodeOrTokenOrList::Token(it) => return Some(SyntaxElement::Token(it)),
                NodeOrTokenOrList::List(it) => self.active_list_iter = Some(it.iter()),
            }
        }
    }

    #[inline]
    fn for_each<F>(self, mut f: F)
    where
        Self: Sized,
        F: FnMut(Self::Item),
    {
        self.fold((), |(), item| f(item))
    }

    #[inline]
    fn fold<B, F>(self, mut init: B, mut f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        for child in self.children {
            match child {
                NodeOrTokenOrList::Node(it) => init = f(init, SyntaxElement::Node(it)),
                NodeOrTokenOrList::Token(it) => init = f(init, SyntaxElement::Token(it)),
                NodeOrTokenOrList::List(it) => {
                    for child in it.iter() {
                        init = f(init, SyntaxElement::Node(child))
                    }
                }
            }
        }
        init
    }
}

/// Iterator over child nodes only.
pub struct Children<'a> {
    inner: ChildrenWithTokens<'a>,
}

impl<'a> Children<'a> {
    #[inline]
    fn filter_child(child: SyntaxElement<'a>) -> Option<SyntaxNode<'a>> {
        match child {
            SyntaxElement::Node(it) => Some(it),
            SyntaxElement::Token(_) => None,
        }
    }

    #[inline]
    fn iter(self) -> impl Iterator<Item = SyntaxNode<'a>> {
        self.inner.filter_map(|child| match child {
            SyntaxElement::Node(it) => Some(it),
            SyntaxElement::Token(_) => None,
        })
    }
}

impl<'a> Iterator for Children<'a> {
    type Item = SyntaxNode<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.find_map(Self::filter_child)
    }

    #[inline]
    fn fold<B, F>(self, init: B, f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        self.iter().fold(init, f)
    }

    #[inline]
    fn for_each<F>(self, f: F)
    where
        Self: Sized,
        F: FnMut(Self::Item),
    {
        self.iter().for_each(f);
    }
}

/// Preorder traversal over nodes.
#[derive(Clone)]
pub struct Preorder<'a> {
    inner: PreorderWithTokens<'a>,
}

impl<'a> Preorder<'a> {
    #[inline]
    fn new(start: SyntaxNode<'a>) -> Preorder<'a> {
        Preorder { inner: PreorderWithTokens::new(start) }
    }

    /// Skips the current subtree during traversal.
    #[inline]
    pub fn skip_subtree(&mut self) {
        self.inner.skip_subtree();
    }
}

impl<'a> Iterator for Preorder<'a> {
    type Item = WalkEvent<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.find_map(|item| match item {
            WalkEventWithTokens::EnterNode(it) => Some(WalkEvent::Enter(it)),
            WalkEventWithTokens::LeaveNode(it) => Some(WalkEvent::Leave(it)),
            WalkEventWithTokens::Token(_) => None,
        })
    }
}

/// Preorder walk event for nodes.
#[derive(Clone, Copy)]
pub enum WalkEvent<'a> {
    Enter(SyntaxNode<'a>),
    Leave(SyntaxNode<'a>),
}

/// Preorder traversal over nodes and tokens.
#[derive(Clone)]
pub struct PreorderWithTokens<'a> {
    stack: Vec<(SyntaxNode<'a>, ChildrenWithTokens<'a>)>,
    root: Option<SyntaxNode<'a>>,
}

impl<'a> PreorderWithTokens<'a> {
    #[inline]
    fn new(start: SyntaxNode<'a>) -> PreorderWithTokens<'a> {
        PreorderWithTokens { stack: Vec::with_capacity(128), root: Some(start) }
    }

    /// Skips the current subtree during traversal.
    #[inline]
    pub fn skip_subtree(&mut self) {
        assert!(self.stack.pop().is_some(), "must have a subtree to skip");
    }
}

impl<'a> Iterator for PreorderWithTokens<'a> {
    type Item = WalkEventWithTokens<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let Some((_, active_node)) = self.stack.last_mut() else {
            let root = self.root?;
            self.root = None;
            return Some(WalkEventWithTokens::EnterNode(root));
        };
        match active_node.next() {
            Some(SyntaxElement::Node(child)) => {
                self.stack.push((child, child.children_with_tokens()));
                Some(WalkEventWithTokens::EnterNode(child))
            }
            Some(SyntaxElement::Token(child)) => Some(WalkEventWithTokens::Token(child)),
            None => {
                let (exited_node, _) = self.stack.pop().expect("should have an exited-from node");
                Some(WalkEventWithTokens::LeaveNode(exited_node))
            }
        }
    }
}

/// Preorder walk event including tokens.
#[derive(Clone, Copy)]
pub enum WalkEventWithTokens<'a> {
    EnterNode(SyntaxNode<'a>),
    LeaveNode(SyntaxNode<'a>),
    Token(SyntaxToken<'a>),
}

/// Stable identifier for a node by kind and non-trivia text range.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct SyntaxNodePtr {
    /// Node kind used for lookup.
    pub kind: SyntaxKind,
    /// Non-trivia text range used for lookup.
    pub range: TextRange,
}

impl SyntaxNodePtr {
    /// Builds a pointer from a concrete node.
    pub fn new(node: &SyntaxNode<'_>) -> Self {
        Self { kind: node.kind(), range: non_trivia_range(*node) }
    }

    /// Attempts to resolve this pointer within `root`.
    pub fn try_to_node<'a>(&self, root: &SyntaxNode<'a>) -> Option<SyntaxNode<'a>> {
        if root.parent().is_some() {
            return None;
        }

        let element = root.covering_element(self.range);
        let start_node = match element {
            NodeOrToken::Node(node) => node,
            NodeOrToken::Token(token) => token.parent(),
        };

        std::iter::successors(Some(start_node), |node| node.parent())
            .find(|node| node.kind() == self.kind && non_trivia_range(*node) == self.range)
    }

    #[track_caller]
    pub fn to_node<'a>(&self, root: &SyntaxNode<'a>) -> SyntaxNode<'a> {
        self.try_to_node(root).unwrap()
    }
}

fn non_trivia_range(node: SyntaxNode<'_>) -> TextRange {
    let first = first_non_trivia_token(node);
    let last = last_non_trivia_token(node);
    match (first, last) {
        (Some(first), Some(last)) => {
            TextRange::new(first.trimmed_range().start(), last.trimmed_range().end())
        }
        _ => TextRange::empty(node.text_range().start()),
    }
}

fn first_non_trivia_token(node: SyntaxNode<'_>) -> Option<SyntaxToken<'_>> {
    let mut token = node.first_token();
    let last_ptr = node.node.last_token(node.tree).ptr();
    loop {
        if !token.is_trivia() {
            return Some(token);
        }
        if token.token.ptr() == last_ptr {
            return None;
        }
        token = token.next_token()?;
    }
}

fn last_non_trivia_token(node: SyntaxNode<'_>) -> Option<SyntaxToken<'_>> {
    let mut token = node.last_token();
    let first_ptr = node.node.first_token(node.tree).ptr();
    loop {
        if !token.is_trivia() {
            return Some(token);
        }
        if token.token.ptr() == first_ptr {
            return None;
        }
        token = token.prev_token()?;
    }
}

impl<'a> SyntaxNode<'a> {
    /// Returns a preorder iterator over nodes.
    #[inline]
    pub fn preorder(self) -> Preorder<'a> {
        Preorder::new(self)
    }

    /// Returns a preorder iterator over nodes and tokens.
    #[inline]
    pub fn preorder_with_tokens(self) -> PreorderWithTokens<'a> {
        PreorderWithTokens::new(self)
    }
}

/// Node-or-token wrapper used throughout the API.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeOrToken<N, T> {
    Node(N),
    Token(T),
}

impl<N, T> NodeOrToken<N, T> {
    /// Converts into the node variant, if any.
    pub fn into_node(self) -> Option<N> {
        match self {
            NodeOrToken::Node(node) => Some(node),
            NodeOrToken::Token(_) => None,
        }
    }

    /// Converts into the token variant, if any.
    pub fn into_token(self) -> Option<T> {
        match self {
            NodeOrToken::Node(_) => None,
            NodeOrToken::Token(token) => Some(token),
        }
    }

    /// Returns a shared reference to the node, if any.
    pub fn as_node(&self) -> Option<&N> {
        match self {
            NodeOrToken::Node(node) => Some(node),
            NodeOrToken::Token(_) => None,
        }
    }

    /// Returns a shared reference to the token, if any.
    pub fn as_token(&self) -> Option<&T> {
        match self {
            NodeOrToken::Node(_) => None,
            NodeOrToken::Token(token) => Some(token),
        }
    }
}

impl<N: fmt::Display, T: fmt::Display> fmt::Display for NodeOrToken<N, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeOrToken::Node(node) => fmt::Display::fmt(node, f),
            NodeOrToken::Token(token) => fmt::Display::fmt(token, f),
        }
    }
}

/// There might be zero, one or two tokens at a given offset.
#[derive(Clone, Debug)]
pub enum TokenAtOffset<T> {
    /// No tokens at offset.
    None,
    /// Only a single token at offset.
    Single(T),
    /// Offset is exactly between two tokens.
    Between(T, T),
}

impl<T> TokenAtOffset<T> {
    /// Maps tokens to a different type.
    pub fn map<F: Fn(T) -> U, U>(self, f: F) -> TokenAtOffset<U> {
        match self {
            TokenAtOffset::None => TokenAtOffset::None,
            TokenAtOffset::Single(it) => TokenAtOffset::Single(f(it)),
            TokenAtOffset::Between(l, r) => TokenAtOffset::Between(f(l), f(r)),
        }
    }

    /// Convert to option, preferring the right token in case of a tie.
    pub fn right_biased(self) -> Option<T> {
        match self {
            Self::None => None,
            Self::Single(node) => Some(node),
            Self::Between(_, right) => Some(right),
        }
    }

    /// Convert to option, preferring the left token in case of a tie.
    pub fn left_biased(self) -> Option<T> {
        match self {
            Self::None => None,
            Self::Single(node) => Some(node),
            Self::Between(left, _) => Some(left),
        }
    }
}

impl<T> Iterator for TokenAtOffset<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        match std::mem::replace(self, Self::None) {
            Self::None => None,
            Self::Single(node) => {
                *self = Self::None;
                Some(node)
            }
            Self::Between(left, right) => {
                *self = Self::Single(right);
                Some(left)
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::None => (0, Some(0)),
            Self::Single(_) => (1, Some(1)),
            Self::Between(_, _) => (2, Some(2)),
        }
    }
}

impl<T> ExactSizeIterator for TokenAtOffset<T> {}
