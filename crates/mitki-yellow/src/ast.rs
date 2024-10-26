use salsa::Database;

use crate::SyntaxKind::*;
use crate::{GreenNode, Red, RedNode, RedToken};

pub trait Node<'db>: Sized {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self>;

    fn syntax(&self) -> &RedNode<'db>;
}

pub struct Module<'db>(pub RedNode<'db>);

impl<'db> Module<'db> {
    pub fn new(root: GreenNode<'db>) -> Self {
        Self(RedNode::new_root(root))
    }

    pub fn items(&self, db: &'db dyn Database) -> impl Iterator<Item = Item<'db>> + '_ {
        self.0.children(db).filter_map(move |syntax| Item::cast(db, syntax))
    }
}

impl<'db> Node<'db> for Module<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        (syntax.kind(db) == MODULE).then_some(Self(syntax))
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

pub enum Item<'db> {
    Function(Function<'db>),
}

impl<'db> Node<'db> for Item<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            FN => Item::Function(Function(syntax)).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        match self {
            Item::Function(function) => function.syntax(),
        }
    }
}

pub struct Function<'db>(RedNode<'db>);

impl<'db> Function<'db> {
    pub fn name(&self, db: &'db dyn Database) -> Option<Name<'db>> {
        child(db, &self.0)
    }
}

impl<'db> Node<'db> for Function<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            FN => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

pub enum Expr<'db> {
    Literal(Literal<'db>),
    Binary(Binary<'db>),
    Postfix(Postfix<'db>),
    Prefix(Prefix<'db>),
}

impl<'db> Node<'db> for Expr<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            LITERAL => Expr::Literal(Literal(syntax)).into(),
            BINARY_EXPR => Expr::Binary(Binary(syntax)).into(),
            POSTFIX_EXPR => Expr::Postfix(Postfix(syntax)).into(),
            PREFIX_EXPR => Expr::Prefix(Prefix(syntax)).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        match self {
            Expr::Literal(literal) => &literal.0,
            Expr::Binary(binary) => &binary.0,
            Expr::Postfix(postfix) => &postfix.0,
            Expr::Prefix(prefix) => &prefix.0,
        }
    }
}

pub struct Literal<'db>(RedNode<'db>);

impl<'db> Literal<'db> {
    pub fn kind(&self, db: &'db dyn Database) -> LiteralKind<'db> {
        let token = self.0.children_with_tokens(db).find_map(Red::into_token).unwrap();

        match token.kind(db) {
            INT_NUMBER => LiteralKind::Int(token),
            FLOAT_NUMBER => LiteralKind::Float(token),
            _ => unreachable!(),
        }
    }
}

pub struct Binary<'db>(RedNode<'db>);

impl<'db> Binary<'db> {
    pub fn lhs(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0.children(db).next().and_then(|syntax| Expr::cast(db, syntax))
    }

    pub fn op(&self, db: &'db dyn Database) -> Option<&'db str> {
        self.0
            .children_with_tokens(db)
            .find_map(Red::into_token)
            .map(|syntax| syntax.green().text_trimmed(db))
    }

    pub fn rhs(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0.children(db).nth(1).and_then(|syntax| Expr::cast(db, syntax))
    }
}

pub struct Postfix<'db>(RedNode<'db>);

impl<'db> Postfix<'db> {
    pub fn expr(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0.children(db).next().and_then(|syntax| Expr::cast(db, syntax))
    }

    pub fn op(&self, db: &'db dyn Database) -> Option<&'db str> {
        self.0
            .children_with_tokens(db)
            .find_map(Red::into_token)
            .map(|syntax| syntax.green().text_trimmed(db))
    }
}

pub struct Prefix<'db>(RedNode<'db>);

impl<'db> Prefix<'db> {
    pub fn op(&self, db: &'db dyn Database) -> Option<&'db str> {
        self.0
            .children_with_tokens(db)
            .find_map(Red::into_token)
            .map(|syntax| syntax.green().text_trimmed(db))
    }

    pub fn expr(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0.children(db).next().and_then(|syntax| Expr::cast(db, syntax))
    }
}

pub enum LiteralKind<'db> {
    Int(RedToken<'db>),
    Float(RedToken<'db>),
}

pub struct Name<'db>(RedNode<'db>);

impl<'db> Name<'db> {
    pub fn as_str(&self, _db: &'db dyn Database) -> &'db str {
        todo!()
    }
}

impl<'db> Node<'db> for Name<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            IDENT => Self(syntax).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

fn child<'db, N: Node<'db>>(db: &'db dyn Database, parent: &RedNode<'db>) -> Option<N> {
    parent.children(db).find_map(|syntax| N::cast(db, syntax))
}
