use std::rc::Rc;

use salsa::Database;
use text_size::{TextRange, TextSize};

use crate::{GreenNode, GreenToken, NodeOrToken, SyntaxKind};

pub type RedNode<'db> = RedData<'db, GreenNode<'db>>;
pub type RedToken<'db> = RedData<'db, GreenToken<'db>>;
pub type Red<'db> = NodeOrToken<RedNode<'db>, RedToken<'db>>;

#[derive(Clone)]
pub struct RedData<'db, T> {
    data: Rc<RedDataInner<'db, T>>,
}

struct RedDataInner<'db, T> {
    parent: Option<RedNode<'db>>,
    text_offset: TextSize,
    green: T,
}

impl<'db, T> RedData<'db, T> {
    pub fn new(parent: Option<RedNode<'db>>, text_offset: TextSize, green: T) -> Self {
        Self { data: Rc::new(RedDataInner { parent, text_offset, green }) }
    }

    pub fn parent(&self) -> Option<RedNode<'db>> {
        self.data.parent.clone()
    }
}

impl<'db> RedNode<'db> {
    pub fn new_root(root: GreenNode<'db>) -> Self {
        Self::new(None, 0.into(), root)
    }

    pub fn green(&self) -> GreenNode<'db> {
        self.data.green
    }

    pub fn kind(&self, db: &dyn Database) -> SyntaxKind {
        self.green().kind(db)
    }

    pub fn text_range(&self, db: &'db dyn Database) -> TextRange {
        let offset = self.data.text_offset;
        let len = self.green().text_len(db);
        TextRange::at(offset, len)
    }

    pub fn text_trimmed_range(&self, db: &'db dyn Database) -> TextRange {
        let range = self.text_range(db);
        let mut start = range.start();
        let mut end = range.end();

        let tokens: Vec<_> = self.children(db).filter_map(Red::into_token).collect();
        for first_token in tokens.iter() {
            let (leading_len, trailing_len, total_len) =
                first_token.green().leading_trailing_total_len(db);
            let token_len = total_len - leading_len - trailing_len;
            if token_len == 0.into() {
                start += total_len;
            } else {
                start += leading_len;
                break;
            }
        }

        for last_token in tokens.iter().rev() {
            let (leading_len, trailing_len, total_len) =
                last_token.green().leading_trailing_total_len(db);
            let token_len = total_len - leading_len - trailing_len;
            if token_len == 0.into() {
                end -= total_len;
            } else {
                end -= trailing_len;
                break;
            }
        }

        TextRange::new(start, end.max(start))
    }

    pub fn children(&self, db: &'db dyn Database) -> impl Iterator<Item = Red<'db>> + '_ {
        let mut offset_in_parent = TextSize::new(0);

        self.green().children(db).iter().map(move |&green_child| {
            let text_offset = self.data.text_offset + offset_in_parent;
            offset_in_parent += green_child.text_len(db);

            match green_child {
                NodeOrToken::Node(node) => {
                    Red::Node(RedNode::new(Some(self.clone()), text_offset, node))
                }
                NodeOrToken::Token(token) => {
                    Red::Token(RedToken::new(Some(self.clone()), text_offset, token))
                }
            }
        })
    }

    pub fn token_at_offset(&self, db: &'db dyn Database, offset: TextSize) -> TokenAtOffset<'db> {
        let range = self.text_trimmed_range(db);

        if range.is_empty() {
            return TokenAtOffset::None;
        }

        let mut children = self.children(db).filter(|child| {
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
        let offset = self.data.text_offset;
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
