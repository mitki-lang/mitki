use salsa::Database;

use crate::SyntaxKind::*;
use crate::{GreenNode, Red, RedNode, RedToken};

pub trait Node<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self>
    where
        Self: Sized;

    fn syntax(self) -> RedNode<'db>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Module<'db>(RedNode<'db>);

impl<'db> Module<'db> {
    pub fn exprs(self, db: &'db dyn Database) -> impl Iterator<Item = Expr<'db>> + 'db {
        self.0.children(db).filter_map(Red::into_node).filter_map(|syntax| Expr::cast(db, syntax))
    }
}

impl<'db> Module<'db> {
    pub fn new(db: &'db dyn Database, root: GreenNode<'db>) -> Self {
        Self(RedNode::new_root(db, root))
    }
}

impl<'db> Node<'db> for Module<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self>
    where
        Self: Sized,
    {
        (syntax.kind(db) == MODULE).then_some(Self(syntax))
    }

    fn syntax(self) -> RedNode<'db> {
        self.0
    }
}

#[derive(Debug)]
pub enum Expr<'db> {
    Literal(Literal<'db>),
    Binary(Binary<'db>),
    Postfix(Postfix<'db>),
    Prefix(Prefix<'db>),
}

impl<'db> Node<'db> for Expr<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self>
    where
        Self: Sized,
    {
        match syntax.kind(db) {
            LITERAL => Expr::Literal(Literal(syntax)).into(),
            BINARY_EXPR => Expr::Binary(Binary(syntax)).into(),
            POSTFIX_EXPR => Expr::Postfix(Postfix(syntax)).into(),
            PREFIX_EXPR => Expr::Prefix(Prefix(syntax)).into(),
            _ => None,
        }
    }

    fn syntax(self) -> RedNode<'db> {
        match self {
            Expr::Literal(literal) => literal.0,
            Expr::Binary(binary) => binary.0,
            Expr::Postfix(postfix) => postfix.0,
            Expr::Prefix(prefix) => prefix.0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Literal<'db>(RedNode<'db>);

impl<'db> Literal<'db> {
    pub fn kind(self, db: &'db dyn Database) -> LiteralKind<'db> {
        self.0.children(db).filter_map(Red::into_token).next().map(LiteralKind::Int).unwrap()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Binary<'db>(RedNode<'db>);

impl<'db> Binary<'db> {
    pub fn lhs(self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0
            .children(db)
            .filter_map(Red::into_node)
            .next()
            .and_then(|syntax| Expr::cast(db, syntax))
    }

    pub fn op(self, db: &'db dyn Database) -> Option<&'db str> {
        self.0
            .children(db)
            .filter_map(Red::into_token)
            .next()
            .map(|syntax| syntax.green(db).text_trimmed(db))
    }

    pub fn rhs(self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0
            .children(db)
            .filter_map(Red::into_node)
            .nth(1)
            .and_then(|syntax| Expr::cast(db, syntax))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Postfix<'db>(RedNode<'db>);

impl<'db> Postfix<'db> {
    pub fn expr(self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0
            .children(db)
            .filter_map(Red::into_node)
            .next()
            .and_then(|syntax| Expr::cast(db, syntax))
    }

    pub fn op(self, db: &'db dyn Database) -> Option<&'db str> {
        self.0
            .children(db)
            .filter_map(Red::into_token)
            .next()
            .map(|syntax| syntax.green(db).text_trimmed(db))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Prefix<'db>(RedNode<'db>);

impl<'db> Prefix<'db> {
    pub fn op(self, db: &'db dyn Database) -> Option<&'db str> {
        self.0
            .children(db)
            .filter_map(Red::into_token)
            .next()
            .map(|syntax| syntax.green(db).text_trimmed(db))
    }

    pub fn expr(self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0
            .children(db)
            .filter_map(Red::into_node)
            .next()
            .and_then(|syntax| Expr::cast(db, syntax))
    }
}

pub enum LiteralKind<'db> {
    Int(RedToken<'db>),
}
