use salsa::Database;
use text_size::{TextRange, TextSize};

use crate::{GreenNode, GreenToken, NodeOrToken, SyntaxKind};

pub type Red<'db> = NodeOrToken<RedNode<'db>, RedToken<'db>>;

impl<'db> Red<'db> {
    pub fn kind(self, db: &dyn Database) -> SyntaxKind {
        match self {
            NodeOrToken::Node(node) => node.kind(db),
            NodeOrToken::Token(token) => token.kind(db),
        }
    }

    pub fn into_node(self) -> Option<RedNode<'db>> {
        match self {
            NodeOrToken::Node(node) => node.into(),
            _ => None,
        }
    }

    pub fn into_token(self) -> Option<RedToken<'db>> {
        match self {
            NodeOrToken::Token(token) => token.into(),
            _ => None,
        }
    }
}

#[salsa::tracked]
pub struct RedNode<'db> {
    pub parent: Option<RedNode<'db>>,
    pub text_offset: TextSize,
    pub green: GreenNode<'db>,
}

impl<'db> RedNode<'db> {
    pub fn new_root(db: &'db dyn Database, root: GreenNode<'db>) -> Self {
        Self::new(db, None, TextSize::new(0), root)
    }

    pub fn kind(self, db: &'db dyn Database) -> SyntaxKind {
        self.green(db).kind(db)
    }

    pub fn children(self, db: &'db dyn Database) -> impl Iterator<Item = Red<'db>> + 'db {
        let mut offset_in_parent = TextSize::new(0);

        self.green(db).children(db).iter().map(move |&green_child| {
            let text_offset = self.text_offset(db) + offset_in_parent;
            offset_in_parent += green_child.text_len(db);

            match green_child {
                NodeOrToken::Node(node) => {
                    Red::Node(RedNode::new(db, self.into(), text_offset, node))
                }
                NodeOrToken::Token(token) => {
                    Red::Token(RedToken::new(db, self.into(), text_offset, token))
                }
            }
        })
    }
}

#[salsa::tracked]
pub struct RedToken<'db> {
    pub parent: Option<RedNode<'db>>,
    pub text_offset: TextSize,
    pub green: GreenToken<'db>,
}

impl<'db> RedToken<'db> {
    pub fn kind(self, db: &'db dyn Database) -> SyntaxKind {
        self.green(db).kind(db)
    }

    fn text_range(&self, db: &'db dyn Database) -> TextRange {
        let offset = self.text_offset(db);
        let len = self.green(db).text_len(db);
        TextRange::at(offset, len)
    }

    pub fn text_trimmed_range(&self, db: &'db dyn Database) -> TextRange {
        let green_token = self.green(db);
        let leading_len = green_token.leading(db).len();
        let trailing_len = green_token.trailing(db).len();

        let range = self.text_range(db);
        TextRange::new(range.start() + leading_len, range.end() - trailing_len)
    }
}
