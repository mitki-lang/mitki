use salsa::Database;

use crate::SyntaxKind::*;
use crate::{GreenNode, Red, RedNode, RedToken};

pub trait Node<'db>: Sized {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self>;

    fn syntax(&self) -> &RedNode<'db>;
}

pub trait HasName<'db>: Node<'db> {
    fn name(&self, db: &'db dyn Database) -> Option<Name<'db>> {
        child(db, self.syntax())
    }
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
    pub fn params(&self, db: &'db dyn Database) -> Option<Params<'db>> {
        child(db, &self.0)
    }

    pub fn ret_type(&self, db: &'db dyn Database) -> Option<RetType<'db>> {
        child(db, self.syntax())
    }

    pub fn body(&self, db: &'db dyn Database) -> Option<Block<'db>> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RetType<'db> {
    pub(crate) syntax: RedNode<'db>,
}

impl<'db> RetType<'db> {
    #[inline]
    pub fn ty(&self, db: &'db dyn Database) -> Option<Type<'db>> {
        child(db, &self.syntax)
    }
}

impl<'db> Node<'db> for RetType<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            RETURN_TYPE => Some(Self { syntax }),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.syntax
    }
}

impl<'db> HasName<'db> for Function<'db> {}

pub struct Params<'db>(RedNode<'db>);

impl<'db> Params<'db> {
    pub fn iter(&self, db: &'db dyn Database) -> impl Iterator<Item = Param<'db>> + '_ {
        self.0.children(db).filter_map(move |syntax| Param::cast(db, syntax))
    }
}

impl<'db> Node<'db> for Params<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            PARAM_LIST => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

pub struct Param<'db>(RedNode<'db>);

impl<'db> Param<'db> {
    pub fn name(&self, db: &'db dyn Database) -> Name<'db> {
        child(db, self.syntax()).unwrap()
    }
}

impl<'db> Node<'db> for Param<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            PARAM => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

pub struct Block<'db>(RedNode<'db>);

impl<'db> Block<'db> {
    pub fn stmts(&self, db: &'db dyn Database) -> impl Iterator<Item = Stmt<'db>> + '_ {
        self.0.children(db).filter_map(move |syntax| Stmt::cast(db, syntax))
    }

    pub fn tail_expr(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        child(db, self.syntax())
    }
}

impl<'db> Node<'db> for Block<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            STMT_LIST => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

pub enum Stmt<'db> {
    Val(Val<'db>),
    Expr(ExprStmt<'db>),
}

impl<'db> Node<'db> for Stmt<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            VAL_STMT => Stmt::Val(Val(syntax)).into(),
            EXPR_STMT => Stmt::Expr(ExprStmt(syntax)).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        match self {
            Stmt::Val(val) => val.syntax(),
            Stmt::Expr(expr) => expr.syntax(),
        }
    }
}

pub struct Val<'db>(RedNode<'db>);

impl<'db> Val<'db> {
    pub fn ty(&self, db: &'db dyn Database) -> Option<Type<'db>> {
        child(db, self.syntax())
    }

    pub fn expr(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        child(db, self.syntax())
    }
}

impl<'db> Node<'db> for Val<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            VAL_STMT => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for Val<'db> {}

pub struct ExprStmt<'db>(RedNode<'db>);

impl<'db> ExprStmt<'db> {
    pub fn expr(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        child(db, self.syntax())
    }

    pub fn semi(&self, db: &'db dyn Database) -> Option<RedToken<'_>> {
        self.syntax()
            .children_with_tokens(db)
            .filter_map(Red::into_token)
            .find(|token| token.kind(db) == SEMICOLON)
    }
}

impl<'db> Node<'db> for ExprStmt<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            EXPR_STMT => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

pub enum Expr<'db> {
    Path(Path<'db>),
    Literal(Literal<'db>),
    Binary(Binary<'db>),
    Postfix(Postfix<'db>),
    Prefix(Prefix<'db>),
    If(IfExpr<'db>),
    Closure(Closure<'db>),
    Call(CallExpr<'db>),
}

impl<'db> Node<'db> for Expr<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            PATH_EXPR => Expr::Path(Path(syntax)).into(),
            LITERAL => Expr::Literal(Literal(syntax)).into(),
            BINARY_EXPR => Expr::Binary(Binary(syntax)).into(),
            POSTFIX_EXPR => Expr::Postfix(Postfix(syntax)).into(),
            PREFIX_EXPR => Expr::Prefix(Prefix(syntax)).into(),
            IF_EXPR => Expr::If(IfExpr(syntax)).into(),
            CLOSURE_EXPR => Expr::Closure(Closure(syntax)).into(),
            CALL_EXPR => Expr::Call(CallExpr(syntax)).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        match self {
            Expr::Path(path) => path.syntax(),
            Expr::Literal(literal) => &literal.0,
            Expr::Binary(binary) => &binary.0,
            Expr::Postfix(postfix) => &postfix.0,
            Expr::Prefix(prefix) => &prefix.0,
            Expr::If(if_) => if_.syntax(),
            Expr::Closure(closure) => closure.syntax(),
            Expr::Call(call) => call.syntax(),
        }
    }
}

pub struct Path<'db>(RedNode<'db>);

impl<'db> Node<'db> for Path<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            PATH_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for Path<'db> {}

pub struct Literal<'db>(RedNode<'db>);

impl<'db> Literal<'db> {
    pub fn kind(&self, db: &'db dyn Database) -> LiteralKind<'db> {
        let token = self.0.children_with_tokens(db).find_map(Red::into_token).unwrap();

        match token.kind(db) {
            INT_NUMBER => LiteralKind::Int(token),
            FLOAT_NUMBER => LiteralKind::Float(token),
            STRING => LiteralKind::String(token),
            CHAR => LiteralKind::Char(token),
            kind @ (TRUE_KW | FALSE_KW) => LiteralKind::Bool(kind == TRUE_KW),
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

pub struct IfExpr<'db>(RedNode<'db>);

impl<'db> IfExpr<'db> {
    pub fn condition(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        child(db, self.syntax())
    }

    pub fn then_branch(&self, db: &'db dyn Database) -> Option<Block<'db>> {
        self.syntax().children(db).nth(1).and_then(|syntax| Block::cast(db, syntax))
    }

    pub fn else_branch(&self, db: &'db dyn Database) -> Option<Block<'db>> {
        self.syntax().children(db).nth(2).and_then(|syntax| Block::cast(db, syntax))
    }
}

pub struct Closure<'db>(RedNode<'db>);

impl<'db> Closure<'db> {
    pub fn params(&self, db: &'db dyn Database) -> Option<Params<'db>> {
        child(db, &self.0)
    }

    pub fn body(&self, db: &'db dyn Database) -> Block<'db> {
        child(db, self.syntax()).unwrap()
    }
}

impl<'db> Node<'db> for Closure<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            CLOSURE_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

pub struct CallExpr<'db>(RedNode<'db>);

impl<'db> CallExpr<'db> {
    pub fn callee(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        child(db, self.syntax())
    }
}

impl<'db> Node<'db> for CallExpr<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            CALL_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

impl<'db> Node<'db> for IfExpr<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            IF_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

pub enum LiteralKind<'db> {
    Bool(bool),
    Int(RedToken<'db>),
    Float(RedToken<'db>),
    String(RedToken<'db>),
    Char(RedToken<'db>),
}

pub struct Name<'db>(RedNode<'db>);

impl<'db> Name<'db> {
    pub fn as_str(&self, db: &'db dyn Database) -> &'db str {
        self.syntax()
            .children_with_tokens(db)
            .find_map(Red::into_token)
            .unwrap()
            .green()
            .text_trimmed(db)
    }
}

impl<'db> Node<'db> for Name<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            IDENT | NAME_REF => Self(syntax).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

pub enum Type<'db> {
    Path(PathType<'db>),
}

impl<'db> Node<'db> for Type<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            PATH_TYPE => Type::Path(PathType(syntax)).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        match self {
            Type::Path(path_type) => &path_type.0,
        }
    }
}

pub struct PathType<'db>(RedNode<'db>);

impl<'db> Node<'db> for PathType<'db> {
    fn cast(db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind(db) {
            PATH_TYPE => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for PathType<'db> {}

fn child<'db, N: Node<'db>>(db: &'db dyn Database, parent: &RedNode<'db>) -> Option<N> {
    parent.children(db).find_map(|syntax| N::cast(db, syntax))
}
