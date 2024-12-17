use std::rc::Rc;

use salsa::Database;
use text_size::{TextRange, TextSize};

use crate::cursor::{Preorder, PreorderWithTokens};
use crate::green::GreenChild;
use crate::{GreenNode, GreenToken, NodeOrToken, SyntaxKind};

pub type RedNode<'db> = RedData<'db, GreenNode<'db>>;
pub type RedToken<'db> = RedData<'db, GreenToken<'db>>;
pub type Red<'db> = NodeOrToken<RedNode<'db>, RedToken<'db>>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct RedNodePtr {
    pub kind: SyntaxKind,
    pub range: TextRange,
}

impl RedNodePtr {
    pub fn new(db: &dyn Database, node: &RedNode) -> Self {
        Self { kind: node.kind(db), range: node.text_range(db) }
    }

    pub fn try_to_node<'db>(
        &self,
        db: &'db dyn Database,
        root: &RedNode<'db>,
    ) -> Option<RedNode<'db>> {
        if root.parent().is_some() {
            return None;
        }

        std::iter::successors(Some(root.clone()), |node| {
            node.child_or_token_at_range(db, self.range)?.into_node()
        })
        .find(|node| node.text_range(db) == self.range && node.kind(db) == self.kind)
    }

    pub fn to_node<'db>(&self, db: &'db dyn Database, root: &RedNode<'db>) -> RedNode<'db> {
        self.try_to_node(db, root).unwrap()
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RedData<'db, T> {
    data: Rc<RedDataInner<'db, T>>,
}

#[derive(PartialEq, Eq, Debug)]
struct RedDataInner<'db, T> {
    parent: Option<RedNode<'db>>,
    index: TextSize,
    offset: TextSize,
    green: T,
}

impl<'db, T> RedData<'db, T> {
    pub fn new(parent: Option<RedNode<'db>>, index: TextSize, offset: TextSize, green: T) -> Self {
        Self { data: Rc::new(RedDataInner { parent, index, offset, green }) }
    }

    pub fn index(&self) -> TextSize {
        self.data.index
    }

    pub fn parent(&self) -> Option<&RedNode<'db>> {
        self.data.parent.as_ref()
    }

    fn green_siblings(&self, db: &'db dyn Database) -> std::slice::Iter<'db, GreenChild<'db>> {
        match self.parent() {
            Some(parent) => parent.green().children(db).iter(),
            None => [].iter(),
        }
    }

    pub fn next_sibling_or_token(&self, db: &'db dyn Database) -> Option<Red<'db>> {
        let mut siblings = self.green_siblings(db).enumerate();
        let index = self.index() + TextSize::new(1);

        siblings.nth(index.into()).and_then(|(index, child)| {
            let index = TextSize::new(index as u32);
            let parent = self.parent()?.clone();
            let offset = parent.data.offset + child.offset();

            match child {
                GreenChild::Node { node, .. } => {
                    Red::Node(RedData::new(parent.into(), index, offset, *node)).into()
                }
                GreenChild::Token { token, .. } => {
                    Red::Token(RedData::new(parent.into(), index, offset, *token)).into()
                }
            }
        })
    }

    pub fn prev_sibling_or_token(&self, db: &'db dyn Database) -> Option<Red<'db>> {
        let siblings = self.green_siblings(db).enumerate();
        let index = self.index() + TextSize::new(1);

        siblings.rev().nth(index.into()).and_then(|(index, child)| {
            let index = TextSize::new(index as u32);
            let parent = self.parent()?.clone();
            let offset = parent.data.offset + child.offset();

            match child {
                GreenChild::Node { node, .. } => {
                    Red::Node(RedData::new(parent.into(), index, offset, *node)).into()
                }
                GreenChild::Token { token, .. } => {
                    Red::Token(RedData::new(parent.into(), index, offset, *token)).into()
                }
            }
        })
    }
}

impl<'db> RedNode<'db> {
    pub fn new_root(root: GreenNode<'db>) -> Self {
        Self::new(None, TextSize::new(0), 0.into(), root)
    }

    pub fn token_in_direction(
        &self,
        db: &'db dyn Database,
        direction: crate::cursor::Direction,
    ) -> Option<RedToken<'db>> {
        PreorderWithTokens::new(db, self.clone(), direction).find_map(|event| {
            if let crate::cursor::WalkEvent::Enter(element) = event {
                element.into_token()
            } else {
                None
            }
        })
    }

    pub fn first_token(&self, db: &'db dyn Database) -> Option<RedToken<'db>> {
        self.token_in_direction(db, crate::cursor::Direction::Next)
    }

    pub fn last_token(&self, db: &'db dyn Database) -> Option<RedToken<'db>> {
        self.token_in_direction(db, crate::cursor::Direction::Prev)
    }

    pub fn ancestors(&self) -> impl Iterator<Item = RedNode<'db>> {
        std::iter::successors(Some(self.clone()), |node| node.parent().cloned())
    }

    pub fn preorder(self, db: &'db dyn Database) -> Preorder<'db> {
        Preorder::new(db, self.clone())
    }

    pub fn first_child_or_token(&self, db: &'db dyn Database) -> Option<Red<'db>> {
        self.green().children(db).iter().next().map(|child| match child {
            GreenChild::Node { node, .. } => {
                Red::Node(RedData::new(self.clone().into(), 0.into(), self.data.offset, *node))
            }
            GreenChild::Token { token, .. } => {
                Red::Token(RedData::new(self.clone().into(), 0.into(), self.data.offset, *token))
            }
        })
    }

    pub fn last_child_or_token(&self, db: &'db dyn Database) -> Option<Red<'db>> {
        self.green().children(db).iter().next_back().map(|child| match child {
            GreenChild::Node { offset, node } => Red::Node(RedData::new(
                self.clone().into(),
                TextSize::new(self.green().children(db).len() as u32),
                self.data.offset + offset,
                *node,
            )),
            GreenChild::Token { offset, token } => Red::Token(RedData::new(
                self.clone().into(),
                TextSize::new(self.green().children(db).len() as u32),
                self.data.offset + offset,
                *token,
            )),
        })
    }

    pub fn children(&self, db: &'db dyn Database) -> impl Iterator<Item = RedNode<'db>> {
        let mut next = self.first_child(db);

        std::iter::from_fn(move || {
            next.take().inspect(|prev| {
                next = prev.next_sibling(db);
            })
        })
    }

    fn child_or_token_at_range(&self, db: &'db dyn Database, range: TextRange) -> Option<Red<'db>> {
        let range = range - self.data.offset;
        self.data.green.child_at_range(db, range).map(move |(index, green)| {
            let parent = Some(self.clone());
            let offset = self.data.offset + green.offset();
            match green {
                GreenChild::Node { node, .. } => {
                    Red::Node(RedNode::new(parent, index, offset, node))
                }
                GreenChild::Token { token, .. } => {
                    Red::Token(RedToken::new(parent, index, offset, token))
                }
            }
        })
    }

    pub fn children_with_tokens(&self, db: &'db dyn Database) -> impl Iterator<Item = Red<'db>> {
        let mut next = self.first_child_or_token(db);

        std::iter::from_fn(move || {
            next.take().inspect(|prev| {
                next = match prev {
                    NodeOrToken::Node(node) => node.next_sibling_or_token(db),
                    NodeOrToken::Token(token) => token.next_sibling_or_token(db),
                };
            })
        })
    }

    pub fn next_sibling(&self, db: &'db dyn Database) -> Option<RedNode<'db>> {
        let mut siblings = self.green_siblings(db).enumerate();
        siblings.nth(self.index().into());
        siblings.find_map(|(index, child)| {
            let index = TextSize::new(index as u32);
            child.into_node().and_then(|green| {
                let parent = self.parent()?.clone();
                let offset = parent.data.offset + child.offset();
                RedNode::new(parent.clone().into(), index, offset, green).into()
            })
        })
    }

    pub fn green(&self) -> GreenNode<'db> {
        self.data.green
    }

    pub fn kind(&self, db: &dyn Database) -> SyntaxKind {
        self.green().kind(db)
    }

    pub fn first_child(&self, db: &'db dyn Database) -> Option<RedNode<'db>> {
        self.green().children(db).iter().enumerate().find_map(|(index, child)| {
            child.into_node().map(|green| {
                RedNode::new(
                    self.clone().into(),
                    TextSize::new(index as u32),
                    self.data.offset + child.offset(),
                    green,
                )
            })
        })
    }

    pub fn text_range(&self, db: &'db dyn Database) -> TextRange {
        let offset = self.data.offset;
        let len = self.green().text_len(db);
        TextRange::at(offset, len)
    }

    pub fn text_trimmed_range(&self, db: &'db dyn Database) -> TextRange {
        let range = self.text_range(db);
        let mut start = range.start();
        let mut end = range.end();

        let mut first_token = self.first_token(db);
        if let Some(first_token) = first_token.take() {
            let (leading_len, trailing_len, total_len) =
                first_token.green().leading_trailing_total_len(db);
            let token_len = total_len - leading_len - trailing_len;
            if token_len == 0.into() {
                start += total_len;
            } else {
                start += leading_len;
            }
        }

        if let Some(last_token) = self.last_token(db) {
            let (leading_len, trailing_len, total_len) =
                last_token.green().leading_trailing_total_len(db);
            let token_len = total_len - leading_len - trailing_len;
            if token_len == 0.into() {
                end -= total_len;
            } else {
                end -= trailing_len;
            }
        }

        TextRange::new(start, end.max(start))
    }

    pub fn token_at_offset(&self, db: &'db dyn Database, offset: TextSize) -> TokenAtOffset<'db> {
        let range = self.text_trimmed_range(db);

        if range.is_empty() {
            return TokenAtOffset::None;
        }

        let mut children = self.children_with_tokens(db).filter(|child| {
            let child_range = child.text_trimmed_range(db);
            !child_range.is_empty() && child_range.contains_inclusive(offset)
        });

        let left = match children.next() {
            Some(child) => child,
            None => return TokenAtOffset::None,
        };

        let right = children.next();

        if let Some(right) = right {
            match (left.token_at_offset(db, offset), right.token_at_offset(db, offset)) {
                (TokenAtOffset::Single(left_token), TokenAtOffset::Single(right_token)) => {
                    TokenAtOffset::Between(left_token, right_token)
                }
                (TokenAtOffset::Single(token), TokenAtOffset::None)
                | (TokenAtOffset::None, TokenAtOffset::Single(token)) => {
                    TokenAtOffset::Single(token)
                }
                _ => TokenAtOffset::None,
            }
        } else {
            left.token_at_offset(db, offset)
        }
    }
}

impl<'db> RedToken<'db> {
    pub fn green(&self) -> GreenToken<'db> {
        self.data.green
    }

    pub fn kind(&self, db: &dyn Database) -> SyntaxKind {
        self.green().kind(db)
    }

    pub fn text_range(&self, db: &dyn Database) -> TextRange {
        let offset = self.data.offset;
        let len = self.green().text_len(db);
        TextRange::at(offset, len)
    }

    pub fn text_trimmed_range(&self, db: &dyn Database) -> TextRange {
        let green_token = self.green();
        let leading_len = green_token.leading(db).len();
        let trailing_len = green_token.trailing(db).len();

        let range = self.text_range(db);
        TextRange::new(range.start() + leading_len, range.end() - trailing_len)
    }
}

impl<'db> Red<'db> {
    pub fn parent(&self) -> Option<&RedNode<'db>> {
        match self {
            NodeOrToken::Node(node) => node.parent(),
            NodeOrToken::Token(token) => token.parent(),
        }
    }

    pub fn next_sibling_or_token(&self, db: &'db dyn Database) -> Option<Red<'db>> {
        match self {
            NodeOrToken::Node(node) => node.next_sibling_or_token(db),
            NodeOrToken::Token(token) => token.next_sibling_or_token(db),
        }
    }

    pub fn prev_sibling_or_token(&self, db: &'db dyn Database) -> Option<Red<'db>> {
        match self {
            NodeOrToken::Node(node) => node.prev_sibling_or_token(db),
            NodeOrToken::Token(token) => token.prev_sibling_or_token(db),
        }
    }

    pub fn into_node(self) -> Option<RedNode<'db>> {
        match self {
            NodeOrToken::Node(node) => Some(node),
            NodeOrToken::Token(_) => None,
        }
    }

    pub fn into_token(self) -> Option<RedToken<'db>> {
        match self {
            NodeOrToken::Node(_) => None,
            NodeOrToken::Token(token) => Some(token),
        }
    }

    pub fn text_range(&self, db: &'db dyn Database) -> TextRange {
        match self {
            NodeOrToken::Node(node) => node.text_range(db),
            NodeOrToken::Token(token) => token.text_range(db),
        }
    }

    pub fn text_trimmed_range(&self, db: &'db dyn Database) -> TextRange {
        match self {
            NodeOrToken::Node(node) => node.text_trimmed_range(db),
            NodeOrToken::Token(token) => token.text_trimmed_range(db),
        }
    }

    pub fn token_at_offset(&self, db: &'db dyn Database, offset: TextSize) -> TokenAtOffset<'db> {
        match self {
            NodeOrToken::Node(node) => node.token_at_offset(db, offset),
            NodeOrToken::Token(token) => TokenAtOffset::Single(token.clone()),
        }
    }
}

pub enum TokenAtOffset<'db> {
    None,
    Single(RedToken<'db>),
    Between(RedToken<'db>, RedToken<'db>),
}

impl<'db> Iterator for TokenAtOffset<'db> {
    type Item = RedToken<'db>;

    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::replace(self, TokenAtOffset::None) {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(token) => Some(token),
            TokenAtOffset::Between(left, right) => {
                *self = TokenAtOffset::Single(right);
                Some(left)
            }
        }
    }
}
