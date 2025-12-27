//! Typed AST wrappers over the raw syntax tree.

use salsa::Database;

use crate::SyntaxKind::*;
use crate::{Red, RedNode, RedToken};

/// Typed wrapper around a syntax node.
pub trait Node<'db>: Sized {
    /// Attempts to cast a raw syntax node into this typed wrapper.
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self>;

    /// Returns the underlying syntax node.
    fn syntax(&self) -> &RedNode<'db>;
}

/// Nodes that expose a name child.
pub trait HasName<'db>: Node<'db> {
    /// Returns the name child, if present.
    fn name(&self, db: &'db dyn Database) -> Option<Name<'db>> {
        child(db, self.syntax())
    }
}

/// Root module node.
pub struct Module<'db>(pub RedNode<'db>);

impl<'db> Module<'db> {
    /// Wraps a root syntax node as a module.
    pub fn new(root: RedNode<'db>) -> Self {
        Self(root)
    }

    /// Iterates items contained in the module.
    pub fn items(&self, db: &'db dyn Database) -> impl Iterator<Item = Item<'db>> + '_ {
        self.0.children().filter_map(move |syntax| Item::cast(db, syntax))
    }
}

impl<'db> Node<'db> for Module<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        (syntax.kind() == MODULE).then_some(Self(syntax))
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

/// Top-level items.
pub enum Item<'db> {
    Function(Function<'db>),
}

impl<'db> Node<'db> for Item<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
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

/// Function definition node.
pub struct Function<'db>(RedNode<'db>);

impl<'db> Function<'db> {
    /// Returns the parameter list.
    pub fn params(&self, db: &'db dyn Database) -> Option<Params<'db>> {
        child(db, &self.0)
    }

    /// Returns the return type, if any.
    pub fn ret_type(&self, db: &'db dyn Database) -> Option<RetType<'db>> {
        child(db, self.syntax())
    }

    /// Returns the function body.
    pub fn body(&self, db: &'db dyn Database) -> Option<Block<'db>> {
        child(db, &self.0)
    }
}

impl<'db> Node<'db> for Function<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            FN => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

/// Return type node.
pub struct RetType<'db> {
    pub(crate) syntax: RedNode<'db>,
}

impl<'db> RetType<'db> {
    /// Returns the type node.
    #[inline]
    pub fn ty(&self, db: &'db dyn Database) -> Option<Type<'db>> {
        child(db, &self.syntax)
    }
}

impl<'db> Node<'db> for RetType<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            RETURN_TYPE => Some(Self { syntax }),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.syntax
    }
}

impl<'db> HasName<'db> for Function<'db> {}

/// Parameter list node.
pub struct Params<'db>(RedNode<'db>);

impl<'db> Params<'db> {
    /// Iterates parameters in the list.
    pub fn iter(&self, db: &'db dyn Database) -> impl Iterator<Item = Param<'db>> + '_ {
        self.0.children().filter_map(move |syntax| Param::cast(db, syntax))
    }
}

impl<'db> Node<'db> for Params<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            PARAM_LIST => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

/// Function parameter node.
pub struct Param<'db>(RedNode<'db>);

impl<'db> Param<'db> {
    /// Returns the parameter name.
    pub fn name(&self, db: &'db dyn Database) -> Name<'db> {
        child(db, self.syntax()).unwrap()
    }

    /// Returns the parameter type, if any.
    pub fn ty(&self, db: &'db dyn Database) -> Option<Type<'db>> {
        child(db, self.syntax())
    }
}

impl<'db> Node<'db> for Param<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            PARAM => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

/// Block node containing statements and an optional tail expression.
pub struct Block<'db>(RedNode<'db>);

impl<'db> Block<'db> {
    /// Iterates statements in the block.
    pub fn stmts(&self, db: &'db dyn Database) -> impl Iterator<Item = Stmt<'db>> + '_ {
        self.0.children().filter_map(move |syntax| Stmt::cast(db, syntax))
    }

    /// Returns the tail expression, if any.
    pub fn tail_expr(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        child(db, self.syntax())
    }
}

impl<'db> Node<'db> for Block<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            STMT_LIST => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

/// Statement node.
pub enum Stmt<'db> {
    Val(Val<'db>),
    Expr(ExprStmt<'db>),
}

impl<'db> Node<'db> for Stmt<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
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

/// Value statement node.
pub struct Val<'db>(RedNode<'db>);

impl<'db> Val<'db> {
    /// Returns the optional type annotation.
    pub fn ty(&self, db: &'db dyn Database) -> Option<Type<'db>> {
        child(db, self.syntax())
    }

    /// Returns the initializer expression.
    pub fn expr(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        child(db, self.syntax())
    }
}

impl<'db> Node<'db> for Val<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            VAL_STMT => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for Val<'db> {}

/// Expression statement node.
pub struct ExprStmt<'db>(RedNode<'db>);

impl<'db> ExprStmt<'db> {
    /// Returns the inner expression.
    pub fn expr(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        child(db, self.syntax())
    }

    /// Returns the trailing semicolon token, if any.
    pub fn semi(&self, _db: &'db dyn Database) -> Option<RedToken<'_>> {
        self.syntax()
            .children_with_tokens()
            .filter_map(Red::into_token)
            .find(|token| token.kind() == SEMICOLON)
    }
}

impl<'db> Node<'db> for ExprStmt<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            EXPR_STMT => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

/// Expression node.
pub enum Expr<'db> {
    Path(Path<'db>),
    Literal(Literal<'db>),
    Tuple(TupleExpr<'db>),
    Binary(Binary<'db>),
    Postfix(Postfix<'db>),
    Prefix(Prefix<'db>),
    If(IfExpr<'db>),
    Closure(Closure<'db>),
    Call(CallExpr<'db>),
}

impl<'db> Node<'db> for Expr<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            PATH_EXPR => Expr::Path(Path(syntax)).into(),
            LITERAL => Expr::Literal(Literal(syntax)).into(),
            BINARY_EXPR => Expr::Binary(Binary(syntax)).into(),
            POSTFIX_EXPR => Expr::Postfix(Postfix(syntax)).into(),
            PREFIX_EXPR => Expr::Prefix(Prefix(syntax)).into(),
            IF_EXPR => Expr::If(IfExpr(syntax)).into(),
            CLOSURE_EXPR => Expr::Closure(Closure(syntax)).into(),
            CALL_EXPR => Expr::Call(CallExpr(syntax)).into(),
            TUPLE_EXPR => Expr::Tuple(TupleExpr(syntax)).into(),
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
            Expr::Tuple(tuple_expr) => tuple_expr.syntax(),
        }
    }
}

/// Path expression node.
pub struct Path<'db>(RedNode<'db>);

impl<'db> Node<'db> for Path<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            PATH_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for Path<'db> {}

/// Literal expression node.
pub struct Literal<'db>(RedNode<'db>);

impl<'db> Literal<'db> {
    /// Returns the literal kind derived from the first token.
    pub fn kind(&self, _db: &'db dyn Database) -> LiteralKind<'db> {
        let token = first_non_trivia_token(&self.0).unwrap();

        match token.kind() {
            INT_NUMBER => LiteralKind::Int(token),
            FLOAT_NUMBER => LiteralKind::Float(token),
            STRING => LiteralKind::String(token),
            CHAR => LiteralKind::Char(token),
            kind @ (TRUE_KW | FALSE_KW) => LiteralKind::Bool(kind == TRUE_KW),
            _ => unreachable!(),
        }
    }
}

/// Tuple expression node.
pub struct TupleExpr<'db>(RedNode<'db>);

impl<'db> TupleExpr<'db> {
    /// Iterates tuple items.
    pub fn exprs(&self, db: &'db dyn Database) -> impl Iterator<Item = Expr<'db>> + '_ {
        self.0.children().filter_map(move |syntax| Expr::cast(db, syntax))
    }
}

impl<'db> Node<'db> for TupleExpr<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            TUPLE_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

/// Binary expression node.
pub struct Binary<'db>(RedNode<'db>);

impl<'db> Binary<'db> {
    /// Returns the left-hand side.
    pub fn lhs(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0.children().next().and_then(|syntax| Expr::cast(db, syntax))
    }

    /// Returns the operator text, if any.
    pub fn op(&self, _db: &'db dyn Database) -> Option<&'db str> {
        first_non_trivia_token(&self.0).map(|syntax| syntax.text_trimmed())
    }

    /// Returns the right-hand side.
    pub fn rhs(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0.children().nth(1).and_then(|syntax| Expr::cast(db, syntax))
    }
}

/// Postfix expression node.
pub struct Postfix<'db>(RedNode<'db>);

impl<'db> Postfix<'db> {
    /// Returns the inner expression.
    pub fn expr(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0.children().next().and_then(|syntax| Expr::cast(db, syntax))
    }

    /// Returns the operator text, if any.
    pub fn op(&self, _db: &'db dyn Database) -> Option<&'db str> {
        first_non_trivia_token(&self.0).map(|syntax| syntax.text_trimmed())
    }
}

/// Prefix expression node.
pub struct Prefix<'db>(RedNode<'db>);

impl<'db> Prefix<'db> {
    /// Returns the operator text, if any.
    pub fn op(&self, _db: &'db dyn Database) -> Option<&'db str> {
        first_non_trivia_token(&self.0).map(|syntax| syntax.text_trimmed())
    }

    /// Returns the inner expression.
    pub fn expr(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        self.0.children().next().and_then(|syntax| Expr::cast(db, syntax))
    }
}

/// If expression node.
pub struct IfExpr<'db>(RedNode<'db>);

impl<'db> IfExpr<'db> {
    /// Returns the condition expression.
    pub fn condition(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        child(db, self.syntax())
    }

    /// Returns the then branch block.
    pub fn then_branch(&self, db: &'db dyn Database) -> Option<Block<'db>> {
        self.syntax().children().nth(1).and_then(|syntax| Block::cast(db, syntax))
    }

    /// Returns the else branch block, if any.
    pub fn else_branch(&self, db: &'db dyn Database) -> Option<Block<'db>> {
        self.syntax().children().nth(2).and_then(|syntax| Block::cast(db, syntax))
    }
}

/// Closure expression node.
pub struct Closure<'db>(RedNode<'db>);

impl<'db> Closure<'db> {
    /// Returns the parameter list.
    pub fn params(&self, db: &'db dyn Database) -> Option<Params<'db>> {
        child(db, &self.0)
    }

    /// Returns the closure body.
    pub fn body(&self, db: &'db dyn Database) -> Block<'db> {
        child(db, self.syntax()).unwrap()
    }
}

impl<'db> Node<'db> for Closure<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            CLOSURE_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

/// Call expression node.
pub struct CallExpr<'db>(RedNode<'db>);

impl<'db> CallExpr<'db> {
    /// Returns the callee expression.
    pub fn callee(&self, db: &'db dyn Database) -> Option<Expr<'db>> {
        child(db, self.syntax())
    }

    /// Returns the argument list, if any.
    pub fn arg_list(&self, db: &'db dyn Database) -> Option<ArgList<'db>> {
        child(db, self.syntax())
    }
}

impl<'db> Node<'db> for CallExpr<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            CALL_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

/// Argument list node.
pub struct ArgList<'db> {
    syntax: RedNode<'db>,
}

impl<'db> Node<'db> for ArgList<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            ARG_LIST => Some(Self { syntax }),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.syntax
    }
}

impl<'db> ArgList<'db> {
    /// Iterates argument expressions.
    pub fn args(&self, db: &'db dyn Database) -> impl Iterator<Item = Expr<'db>> + '_ {
        self.syntax.children().filter_map(move |syntax| Expr::cast(db, syntax))
    }
}

impl<'db> Node<'db> for IfExpr<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            IF_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

/// Concrete literal variants.
pub enum LiteralKind<'db> {
    Bool(bool),
    Int(RedToken<'db>),
    Float(RedToken<'db>),
    String(RedToken<'db>),
    Char(RedToken<'db>),
}

/// Identifier node.
pub struct Name<'db>(RedNode<'db>);

impl<'db> Name<'db> {
    /// Returns the identifier text.
    pub fn as_str(&self, _db: &'db dyn Database) -> &'db str {
        first_non_trivia_token(self.syntax()).unwrap().text_trimmed()
    }
}

impl<'db> Node<'db> for Name<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
            IDENT | NAME_REF => Self(syntax).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &RedNode<'db> {
        &self.0
    }
}

/// Type node.
pub enum Type<'db> {
    Path(PathType<'db>),
}

impl<'db> Node<'db> for Type<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
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

/// Path type node.
pub struct PathType<'db>(RedNode<'db>);

impl<'db> Node<'db> for PathType<'db> {
    fn cast(_db: &'db dyn Database, syntax: RedNode<'db>) -> Option<Self> {
        match syntax.kind() {
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
    parent.children().find_map(|syntax| N::cast(db, syntax))
}

fn first_non_trivia_token<'db>(node: &RedNode<'db>) -> Option<RedToken<'db>> {
    node.children_with_tokens().find_map(|child| {
        let token = child.into_token()?;
        if token.is_trivia() { None } else { Some(token) }
    })
}
