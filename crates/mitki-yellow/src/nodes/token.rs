//! Token storage and trivia attachment helpers.

use std::marker::PhantomData;

use text_size::{TextRange, TextSize};

use super::node::Node;
use super::tree::TreeInner;
use crate::SyntaxKind;

/// Raw token stored in the tree arena.
#[derive(Clone, Copy)]
#[repr(align(4))]
pub(crate) struct Token {
    pub(crate) kind: SyntaxKind,
    pub(crate) attached_trivia: AttachedTrivia,
    pub(crate) end: TextSize,
    pub(crate) parent: *const Node,
}

unsafe impl Send for Token {}
unsafe impl Sync for Token {}

/// Compact encoding for trivia attachment metadata.
#[derive(Clone, Copy)]
pub(crate) struct AttachedTrivia {
    /// Encodes leading/trailing presence and trivia length in a single `u16`.
    ///
    /// Layout:
    /// - bit 0: has leading trivia
    /// - bit 1: has trailing trivia
    /// - bits 2..: trivia length (leading for real tokens, trailing for first
    ///   trailing token)
    raw: u16,
}

impl AttachedTrivia {
    const MAX_TRIVIA_LEN: usize = (1 << u16::BITS) - 1;

    #[inline]
    pub(crate) fn new(
        has_leading_trivia: bool,
        has_trailing_trivia: bool,
        trivia_len: usize,
    ) -> Self {
        assert!(trivia_len <= Self::MAX_TRIVIA_LEN);
        Self {
            raw: ((trivia_len << 2)
                | (usize::from(has_trailing_trivia) << 1)
                | usize::from(has_leading_trivia)) as u16,
        }
    }

    #[inline]
    pub(crate) fn has_leading_trivia(self) -> bool {
        (self.raw & 0b01) != 0
    }

    #[inline]
    pub(crate) fn has_trailing_trivia(self) -> bool {
        (self.raw & 0b10) != 0
    }

    #[inline]
    pub(crate) fn trivia_len(self) -> usize {
        (self.raw >> 2) as usize
    }
}

/// Raw token handle; not a real reference to satisfy Stacked Borrows.
#[derive(Clone, Copy)]
pub(crate) struct TokenRef<'a> {
    pub(super) ptr: *const Token,
    pub(super) _marker: PhantomData<&'a ()>,
}

impl<'a> TokenRef<'a> {
    #[inline]
    pub(crate) fn get(self) -> &'a Token {
        unsafe { &*self.ptr }
    }

    #[inline]
    pub(crate) fn ptr(self) -> *const Token {
        self.ptr
    }

    /// Returns the previous token even if it is the fake sentinel.
    #[inline]
    fn prev_maybe_fake_token(self) -> &'a Token {
        unsafe { &*self.ptr.sub(1) }
    }

    #[inline]
    pub(crate) fn start(self) -> TextSize {
        self.prev_maybe_fake_token().end
    }

    #[inline]
    pub(crate) fn end(self) -> TextSize {
        self.get().end
    }

    #[inline]
    pub(crate) fn text_range(self) -> TextRange {
        let start = self.prev_maybe_fake_token().end;
        TextRange::new(start, self.get().end)
    }

    #[inline]
    pub(crate) fn text(self, tree: &'a TreeInner) -> &'a str {
        let range = self.text_range();
        unsafe { tree.text.get_unchecked(usize::from(range.start())..usize::from(range.end())) }
    }

    #[inline]
    pub(crate) fn prev_token(self, tree: &TreeInner) -> Option<Self> {
        let prev_token = unsafe { self.ptr.sub(1) };
        if tree.tokens.as_ptr() == prev_token {
            None
        } else {
            Some(Self { ptr: prev_token, _marker: PhantomData })
        }
    }

    #[inline]
    pub(crate) fn next_token(self, tree: &TreeInner) -> Option<Self> {
        let prev_token = unsafe { self.ptr.add(1) };
        if tree.tokens.as_ptr_range().end == prev_token {
            None
        } else {
            Some(Self { ptr: prev_token, _marker: PhantomData })
        }
    }

    #[inline]
    pub(crate) fn leading_trivia(self) -> TokenRefIter<'a> {
        if !self.get().attached_trivia.has_leading_trivia() {
            return unsafe { TokenRefIter::new(std::ptr::dangling(), 0) };
        }

        let trivia_len = self.get().attached_trivia.trivia_len();
        let trivia_start = unsafe { self.ptr.sub(trivia_len) };
        unsafe { TokenRefIter::new(trivia_start, trivia_len) }
    }

    #[inline]
    pub(crate) fn trailing_trivia(self) -> TokenRefIter<'a> {
        if !self.get().attached_trivia.has_trailing_trivia() {
            return unsafe { TokenRefIter::new(std::ptr::dangling(), 0) };
        }

        let trivia_start = unsafe { self.ptr.add(1) };
        let trivia_len = unsafe { (*trivia_start).attached_trivia.trivia_len() };
        unsafe { TokenRefIter::new(trivia_start, trivia_len) }
    }

    #[inline]
    pub(crate) fn parent(self) -> &'a Node {
        unsafe { &*self.get().parent }
    }
}

/// Iterator over trivia tokens attached to a token.
#[derive(Clone)]
pub(crate) struct TokenRefIter<'a> {
    start: *const Token,
    end: *const Token,
    _marker: PhantomData<&'a ()>,
}

impl<'a> TokenRefIter<'a> {
    #[inline]
    unsafe fn new(start: *const Token, len: usize) -> Self {
        Self { start, end: unsafe { start.add(len) }, _marker: PhantomData }
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        unsafe { self.end.offset_from(self.start) as usize }
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

impl<'a> Iterator for TokenRefIter<'a> {
    type Item = TokenRef<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.is_empty() {
            None
        } else {
            let result = TokenRef { ptr: self.start, _marker: PhantomData };
            unsafe {
                self.start = self.start.add(1);
            }
            Some(result)
        }
    }
}

impl<'a> DoubleEndedIterator for TokenRefIter<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.is_empty() {
            None
        } else {
            unsafe {
                self.end = self.end.sub(1);
            }
            Some(TokenRef { ptr: self.end, _marker: PhantomData })
        }
    }
}
