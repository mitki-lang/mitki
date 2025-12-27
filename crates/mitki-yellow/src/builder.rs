//! Incremental builder for the immutable syntax tree.

use std::marker::PhantomData;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ptr::null;

use text_size::TextSize;

use crate::maybe_dangling::MaybeDangling;
use crate::nodes::{AttachedTrivia, TreeInner};
use crate::{SyntaxKind, SyntaxTree, TriviaPiece, TriviaPieceKind};

struct Node {
    parent: Option<usize>,
    /// A `ManuallyDrop` so that `Node` doesn't have a `Drop` glue.
    children: ManuallyDrop<Vec<ChildKind>>,
    children_ptr: usize,
    children_len: u32,
    kind: SyntaxKind,
    first_last_token: Option<(u32, u32)>,
}

struct List {
    children: ManuallyDrop<Vec<usize>>,
    children_ptr: usize,
    children_len: usize,
}

enum ChildKind {
    Token(usize),
    Node(usize),
    List(usize),
}

struct Token {
    kind: SyntaxKind,
    attached_trivia: AttachedTrivia,
    end: TextSize,
    parent: usize,
}

/// Builds a `SyntaxTree` from parser events.
pub struct Builder<'db> {
    nodes: Vec<Node>,
    node_children: Vec<ChildKind>,
    lists: Vec<List>,
    list_children: Vec<usize>,
    tokens: Vec<Token>,
    text: Box<str>,
    _marker: PhantomData<&'db ()>,

    node_children_pool: Vec<Vec<ChildKind>>,
    list_children_pool: Vec<Vec<usize>>,
    opened: Vec<Opened>,
    text_len: TextSize,
    last_token_index: u32,
}

#[derive(Debug, Clone, Copy)]
enum Opened {
    Node(usize),
    List(usize),
}

impl Drop for Builder<'_> {
    fn drop(&mut self) {
        if !std::thread::panicking() && !self.opened.is_empty() {
            panic!("you should call `Builder::finish()`");
        }
    }
}

const DEFAULT_TREE_DEPTH: usize = 128;
const DEFAULT_TREE_SIZE: usize = 1024;
const DEFAULT_CHILDREN_LEN: usize = 10;

impl<'db> Builder<'db> {
    /// Creates a new builder for `text`.
    ///
    /// The internal token buffer is seeded with a fake token at index 0 to make
    /// token ranges uniform.
    pub fn new(_db: &'db dyn salsa::Database, text: &str) -> Self {
        let mut tokens = Vec::with_capacity(DEFAULT_TREE_SIZE);
        tokens.push(Token {
            kind: SyntaxKind::TOMBSTONE,
            attached_trivia: AttachedTrivia::new(false, false, 0),
            end: TextSize::new(0),
            parent: 0,
        });
        Self {
            nodes: Vec::with_capacity(DEFAULT_TREE_SIZE),
            node_children: Vec::with_capacity(DEFAULT_TREE_SIZE),
            lists: Vec::with_capacity(DEFAULT_TREE_SIZE),
            list_children: Vec::with_capacity(DEFAULT_TREE_SIZE),
            tokens,
            text: text.into(),
            _marker: PhantomData,

            node_children_pool: Vec::with_capacity(DEFAULT_TREE_DEPTH),
            list_children_pool: Vec::with_capacity(DEFAULT_TREE_DEPTH),
            opened: Vec::with_capacity(DEFAULT_TREE_DEPTH),
            text_len: TextSize::new(0),
            last_token_index: 0,
        }
    }

    /// Retrieves a recycled node-children buffer or allocates a new one.
    fn new_node_children_vec(&mut self) -> Vec<ChildKind> {
        self.node_children_pool.pop().unwrap_or_else(|| Vec::with_capacity(DEFAULT_CHILDREN_LEN))
    }

    /// Returns a node-children buffer to the pool.
    fn recycle_node_children_vec(&mut self, vec: Vec<ChildKind>) {
        self.node_children_pool.push(vec);
    }

    /// Retrieves a recycled list-children buffer or allocates a new one.
    fn new_list_children_vec(&mut self) -> Vec<usize> {
        self.list_children_pool.pop().unwrap_or_else(|| Vec::with_capacity(DEFAULT_CHILDREN_LEN))
    }

    /// Returns a list-children buffer to the pool.
    fn recycle_list_children_vec(&mut self, vec: Vec<usize>) {
        self.list_children_pool.push(vec);
    }

    /// Returns the most recently opened node or list.
    fn last_opened(&self) -> Opened {
        *self.opened.last().expect("no opened nodes?")
    }

    /// Returns the most recently opened node or panics if it's a list.
    #[track_caller]
    fn expect_last_opened_node(&self) -> usize {
        match self.last_opened() {
            Opened::Node(it) => it,
            Opened::List(_) => panic!("expected an opened node, found an opened list"),
        }
    }

    /// Starts a new node of the given kind.
    pub fn start_node(&mut self, kind: SyntaxKind) {
        let parent = self.last_parent();
        let new_node = self.nodes.len();
        let new_children = self.new_node_children_vec();
        self.nodes.push(Node {
            parent,
            children: ManuallyDrop::new(new_children),
            children_ptr: 0,
            children_len: 0,
            kind,
            first_last_token: None,
        });
        match self.last_opened_opt() {
            Some(Opened::Node(direct_parent)) => {
                self.nodes[direct_parent].children.push(ChildKind::Node(new_node))
            }
            Some(Opened::List(direct_parent)) => self.lists[direct_parent].children.push(new_node),
            None => {
                // Root node.
            }
        }
        self.opened.push(Opened::Node(new_node));
    }

    /// Returns the most recent opened node index, skipping lists.
    fn last_parent(&mut self) -> Option<usize> {
        self.opened.iter().rev().find_map(|opened| match *opened {
            Opened::Node(node) => Some(node),
            Opened::List(_) => None,
        })
    }

    /// Returns the last opened entry, if any.
    fn last_opened_opt(&self) -> Option<Opened> {
        self.opened.last().copied()
    }

    /// Finishes the most recently started node.
    pub fn finish_node(&mut self) {
        let node = self.expect_last_opened_node();
        self.opened.pop();
        let node = &mut self.nodes[node];
        if node.first_last_token.is_none() {
            node.first_last_token = Some((self.last_token_index, self.last_token_index));
        }
        let mut children = std::mem::take(&mut *node.children);
        node.children_ptr = self.node_children.len();
        node.children_len = children.len().try_into().unwrap();
        self.node_children.append(&mut children);
        self.recycle_node_children_vec(children);
    }

    /// Starts a list under the current node.
    pub fn start_list(&mut self) {
        let parent = self.expect_last_opened_node();
        let new_list = self.lists.len();
        let new_children = self.new_list_children_vec();
        self.lists.push(List {
            children: ManuallyDrop::new(new_children),
            children_ptr: 0,
            children_len: 0,
        });
        self.nodes[parent].children.push(ChildKind::List(new_list));
        self.opened.push(Opened::List(new_list));
    }

    /// Finishes the most recently started list.
    pub fn finish_list(&mut self) {
        let Opened::List(list) = self.last_opened() else {
            panic!("expected an opened list, found an opened node");
        };
        self.opened.pop();
        let list = &mut self.lists[list];
        let mut children = std::mem::take(&mut *list.children);
        list.children_ptr = self.list_children.len();
        list.children_len = children.len();
        self.list_children.append(&mut children);
        self.recycle_list_children_vec(children);
    }

    /// Adds a token with its leading and trailing trivia.
    pub fn token(
        &mut self,
        leading_trivia: impl ExactSizeIterator<Item = TriviaPiece>,
        kind: SyntaxKind,
        text_len: TextSize,
        trailing_trivia: impl ExactSizeIterator<Item = TriviaPiece>,
    ) {
        let parent = self.expect_last_opened_node();
        let (leading_trivia_len, leading_trivia) = ensure_exact_size_iter(leading_trivia);
        let (trailing_trivia_len, trailing_trivia) = ensure_exact_size_iter(trailing_trivia);
        let mut push_text_len = |text_len| {
            self.text_len += text_len;
            assert!(self.text.is_char_boundary(usize::from(self.text_len)));
            self.text_len
        };
        let first_token = self.tokens.len();
        self.tokens.extend(leading_trivia.map(|piece| Token {
            kind: trivia_piece_kind(piece.kind),
            attached_trivia: AttachedTrivia::new(false, false, 0),
            end: push_text_len(piece.len),
            parent,
        }));
        let token = self.tokens.len();
        self.tokens.push(Token {
            kind,
            attached_trivia: AttachedTrivia::new(
                leading_trivia_len != 0,
                trailing_trivia_len != 0,
                leading_trivia_len,
            ),
            end: push_text_len(text_len),
            parent,
        });
        self.nodes[parent].children.push(ChildKind::Token(token));
        self.tokens.extend(trailing_trivia.map(|piece| Token {
            kind: trivia_piece_kind(piece.kind),
            attached_trivia: AttachedTrivia::new(false, false, trailing_trivia_len),
            end: push_text_len(piece.len),
            parent,
        }));
        let last_token = first_token + leading_trivia_len + trailing_trivia_len;

        self.update_first_last_tokens(first_token, last_token);
        self.last_token_index = last_token.try_into().unwrap();
    }

    /// Updates token ranges for all open ancestor nodes.
    fn update_first_last_tokens(&mut self, first_token: usize, last_token: usize) {
        // Walk ancestors, update first and last token.
        let first_token = first_token.try_into().unwrap();
        let last_token = last_token.try_into().unwrap();
        for ancestor in &self.opened {
            let Opened::Node(node) = *ancestor else { continue };
            let node = &mut self.nodes[node];
            match &mut node.first_last_token {
                // First token inside this node, so also first and last token.
                None => node.first_last_token = Some((first_token, last_token)),
                // There was already a token. It is the first token, but we're after it so (maybe)
                // we're last.
                Some((_f, l)) => *l = last_token,
            }
        }
    }

    /// Finishes building and returns the immutable `SyntaxTree`.
    pub fn finish(self) -> SyntaxTree<'db> {
        assert!(self.opened.is_empty());
        assert!(!self.nodes.is_empty());
        let tree = self.finish_impl();
        SyntaxTree { tree, _marker: PhantomData }
    }

    /// Finalizes buffers into stable tree storage.
    #[expect(unsafe_code)]
    fn finish_impl(mut self) -> TreeInner {
        // Important: The addresses here must be stable, so we must preallocate.
        let mut tokens = new_boxed_slice(&self.tokens);
        let mut nodes = new_boxed_slice(&self.nodes);
        let mut node_children = new_boxed_slice(&self.node_children);
        let mut lists = new_boxed_slice(&self.lists);
        let mut list_children = new_boxed_slice(&self.list_children);

        create_boxed_slice(&mut tokens, &self.tokens, |token, _| crate::nodes::Token {
            kind: token.kind,
            attached_trivia: token.attached_trivia,
            end: token.end,
            parent: ptr(&mut nodes, token.parent),
        });
        create_boxed_slice(&mut nodes, &self.nodes, |node, nodes| {
            let (first_token, last_token) = node.first_last_token.expect("node without tokens");
            crate::nodes::Node {
                parent: node.parent.map_or_else(null, |parent| ptr(nodes, parent)),
                children: ptr(&mut node_children, node.children_ptr),
                children_len: node.children_len,
                kind: node.kind,
                first_token,
                last_token,
            }
        });
        create_boxed_slice(&mut node_children, &self.node_children, |child, _| match *child {
            ChildKind::Token(it) => {
                crate::nodes::NodeOrListOrToken::new_token(ptr(&mut tokens, it))
            }
            ChildKind::Node(it) => crate::nodes::NodeOrListOrToken::new_node(ptr(&mut nodes, it)),
            ChildKind::List(it) => crate::nodes::NodeOrListOrToken::new_list(ptr(&mut lists, it)),
        });
        create_boxed_slice(&mut lists, &self.lists, |list, _| crate::nodes::List {
            children: std::ptr::slice_from_raw_parts(
                ptr(&mut list_children, list.children_ptr),
                list.children_len,
            ),
        });
        create_boxed_slice(&mut list_children, &self.list_children, |child, _| {
            ptr(&mut nodes, *child)
        });

        let tree = unsafe {
            TreeInner {
                text: MaybeDangling::new(std::mem::take(&mut self.text)),
                tokens: tokens.assume_init(),
                nodes: crate::nodes::Nodes {
                    nodes: nodes.assume_init(),
                    node_children: node_children.assume_init(),
                    lists: lists.assume_init(),
                    list_children: list_children.assume_init(),
                },
            }
        };
        self.opened.clear();
        tree
    }
}

/// Returns a stable pointer to an element in the output buffer.
#[inline]
#[expect(unsafe_code)]
fn ptr<T>(out: &mut MaybeDangling<Box<[MaybeUninit<T>]>>, index: usize) -> *const T {
    let out_slice = unsafe { &*out.as_mut_ptr() };
    if out_slice.is_empty() {
        debug_assert_eq!(index, 0);
        return std::ptr::dangling();
    }
    debug_assert!(index < out_slice.len());
    unsafe { (&raw const (**out.as_mut_ptr())[index]).cast::<T>() }
}

/// Allocates an uninitialized boxed slice with the same length as `input`.
#[inline]
fn new_boxed_slice<T>(input: &[impl Sized]) -> MaybeDangling<Box<[MaybeUninit<T>]>> {
    MaybeDangling::new(Box::new_uninit_slice(input.len()))
}

/// Maps `input` into the output buffer using `mapper`.
#[expect(unsafe_code)]
fn create_boxed_slice<T, U>(
    out: &mut MaybeDangling<Box<[MaybeUninit<U>]>>,
    input: &[T],
    mut mapper: impl FnMut(&T, &mut MaybeDangling<Box<[MaybeUninit<U>]>>) -> U,
) {
    let out_ptr = unsafe { &raw mut **out.as_mut_ptr() };
    assert_eq!(out_ptr.len(), input.len());
    let mut out_ptr = out_ptr.cast::<MaybeUninit<U>>();
    for item in input {
        unsafe { out_ptr.write(MaybeUninit::new(mapper(item, out))) };
        out_ptr = unsafe { out_ptr.add(1) };
    }
}

/// Ensures an `ExactSizeIterator` has at least as reported items, for safety.
///
/// Note: you must use the returned len, not a len you saved from before
/// (otherwise a malicious iterator could return different lengths).
#[inline]
fn ensure_exact_size_iter<T>(
    mut iter: impl ExactSizeIterator<Item = T>,
) -> (usize, impl Iterator<Item = T>) {
    let len = iter.len();
    let iter = (0..len)
        .map(move |_| iter.next().unwrap_or_else(|| panic!("iter should have {len} items")));
    (len, iter)
}

/// Maps trivia piece kinds to syntax kinds.
#[inline]
fn trivia_piece_kind(kind: TriviaPieceKind) -> SyntaxKind {
    match kind {
        TriviaPieceKind::Whitespace => SyntaxKind::WHITESPACE,
        TriviaPieceKind::Newline => SyntaxKind::NEWLINE,
        TriviaPieceKind::SingleLineComment => SyntaxKind::LINE_COMMENT,
    }
}
