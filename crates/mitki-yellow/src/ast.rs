//! Typed AST wrappers over the raw syntax tree.

use text_size::TextRange;

use crate::SyntaxKind::*;
use crate::{SyntaxElement, SyntaxNode, SyntaxToken};

/// Typed wrapper around a syntax node.
pub trait Node<'db>: Sized {
    /// Attempts to cast a raw syntax node into this typed wrapper.
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self>;

    /// Returns the underlying syntax node.
    fn syntax(&self) -> &SyntaxNode<'db>;
}

/// Nodes that expose a name child.
pub trait HasName<'db>: Node<'db> {
    /// Returns the name child, if present.
    fn name(&self) -> Option<Name<'db>> {
        child(self.syntax())
    }
}

/// Root module node.
pub struct Module<'db>(pub SyntaxNode<'db>);

impl<'db> Module<'db> {
    /// Wraps a root syntax node as a module.
    pub fn new(root: SyntaxNode<'db>) -> Self {
        Self(root)
    }

    /// Iterates items contained in the module.
    pub fn items(&self) -> impl Iterator<Item = Item<'db>> + '_ {
        self.0.children().filter_map(Item::cast)
    }
}

impl<'db> Node<'db> for Module<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        (syntax.kind() == MODULE).then_some(Self(syntax))
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Top-level items.
pub enum Item<'db> {
    Function(Function<'db>),
    Struct(StructDef<'db>),
    Enum(EnumDef<'db>),
}

impl<'db> Node<'db> for Item<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            FN => Item::Function(Function(syntax)).into(),
            STRUCT_DEF => Item::Struct(StructDef(syntax)).into(),
            ENUM_DEF => Item::Enum(EnumDef(syntax)).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        match self {
            Item::Function(function) => function.syntax(),
            Item::Struct(s) => s.syntax(),
            Item::Enum(e) => e.syntax(),
        }
    }
}

/// Function definition node.
pub struct Function<'db>(SyntaxNode<'db>);

impl<'db> Function<'db> {
    /// Iterates type parameters in the generic parameter list.
    pub fn type_params(&self) -> impl Iterator<Item = TypeParam<'db>> + '_ {
        self.0.children().filter_map(TypeParam::cast)
    }

    /// Returns the parameter list.
    pub fn params(&self) -> Option<Params<'db>> {
        child(&self.0)
    }

    /// Returns the return type, if any.
    pub fn ret_type(&self) -> Option<RetType<'db>> {
        child(self.syntax())
    }

    /// Returns the function body.
    pub fn body(&self) -> Option<Block<'db>> {
        child(&self.0)
    }
}

impl<'db> Node<'db> for Function<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            FN => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Return type node.
pub struct RetType<'db> {
    pub(crate) syntax: SyntaxNode<'db>,
}

impl<'db> RetType<'db> {
    /// Returns the type node.
    #[inline]
    pub fn ty(&self) -> Option<Type<'db>> {
        child(&self.syntax)
    }
}

impl<'db> Node<'db> for RetType<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            RETURN_TYPE => Some(Self { syntax }),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.syntax
    }
}

impl<'db> HasName<'db> for Function<'db> {}

/// Type parameter node in a generic parameter list.
pub struct TypeParam<'db>(SyntaxNode<'db>);

impl<'db> TypeParam<'db> {
    pub fn as_str(&self) -> &'db str {
        first_non_trivia_token(&self.0).map_or("", |t| t.text_trimmed())
    }
}

impl<'db> Node<'db> for TypeParam<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            TYPE_PARAM => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Parameter list node.
pub struct Params<'db>(SyntaxNode<'db>);

impl<'db> Params<'db> {
    /// Iterates parameters in the list.
    pub fn iter(&self) -> impl Iterator<Item = Param<'db>> + '_ {
        self.0.children().filter_map(Param::cast)
    }
}

impl<'db> Node<'db> for Params<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            PARAM_LIST => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Function parameter node.
pub struct Param<'db>(SyntaxNode<'db>);

impl<'db> Param<'db> {
    /// Returns the parameter name.
    pub fn name(&self) -> Name<'db> {
        child(self.syntax()).unwrap()
    }

    /// Returns the parameter type, if any.
    pub fn ty(&self) -> Option<Type<'db>> {
        child(self.syntax())
    }
}

impl<'db> Node<'db> for Param<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            PARAM => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Block node containing statements and an optional tail expression.
pub struct Block<'db>(SyntaxNode<'db>);

impl<'db> Block<'db> {
    /// Iterates statements in the block.
    pub fn stmts(&self) -> impl Iterator<Item = Stmt<'db>> + '_ {
        self.0.children().filter_map(Stmt::cast)
    }

    /// Returns the tail expression, if any.
    pub fn tail_expr(&self) -> Option<Expr<'db>> {
        child(self.syntax())
    }
}

impl<'db> Node<'db> for Block<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            STMT_LIST => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Statement node.
pub enum Stmt<'db> {
    Val(Val<'db>),
    Expr(ExprStmt<'db>),
}

impl<'db> Node<'db> for Stmt<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            VAL_STMT => Stmt::Val(Val(syntax)).into(),
            EXPR_STMT => Stmt::Expr(ExprStmt(syntax)).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        match self {
            Stmt::Val(val) => val.syntax(),
            Stmt::Expr(expr) => expr.syntax(),
        }
    }
}

/// Value statement node.
pub struct Val<'db>(SyntaxNode<'db>);

impl<'db> Val<'db> {
    /// Returns the optional type annotation.
    pub fn ty(&self) -> Option<Type<'db>> {
        child(self.syntax())
    }

    /// Returns the initializer expression.
    pub fn expr(&self) -> Option<Expr<'db>> {
        child(self.syntax())
    }
}

impl<'db> Node<'db> for Val<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            VAL_STMT => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for Val<'db> {}

/// Expression statement node.
pub struct ExprStmt<'db>(SyntaxNode<'db>);

impl<'db> ExprStmt<'db> {
    /// Returns the inner expression.
    pub fn expr(&self) -> Option<Expr<'db>> {
        child(self.syntax())
    }

    /// Returns the trailing semicolon token, if any.
    pub fn semi(&self) -> Option<SyntaxToken<'_>> {
        self.syntax()
            .children_with_tokens()
            .filter_map(SyntaxElement::into_token)
            .find(|token| token.kind() == SEMICOLON)
    }
}

impl<'db> Node<'db> for ExprStmt<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            EXPR_STMT => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Expression node.
pub enum Expr<'db> {
    Path(Path<'db>),
    Field(FieldExpr<'db>),
    Literal(Literal<'db>),
    Tuple(TupleExpr<'db>),
    BinOpSeq(BinOpSeq<'db>),
    Postfix(Postfix<'db>),
    Prefix(Prefix<'db>),
    If(IfExpr<'db>),
    Closure(Closure<'db>),
    Call(CallExpr<'db>),
    Struct(StructExpr<'db>),
}

impl<'db> Node<'db> for Expr<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            PATH_EXPR => Expr::Path(Path(syntax)).into(),
            FIELD_EXPR => Expr::Field(FieldExpr(syntax)).into(),
            LITERAL => Expr::Literal(Literal(syntax)).into(),
            BIN_OP_SEQ => Expr::BinOpSeq(BinOpSeq(syntax)).into(),
            POSTFIX_EXPR => Expr::Postfix(Postfix(syntax)).into(),
            PREFIX_EXPR => Expr::Prefix(Prefix(syntax)).into(),
            IF_EXPR => Expr::If(IfExpr(syntax)).into(),
            CLOSURE_EXPR => Expr::Closure(Closure(syntax)).into(),
            CALL_EXPR => Expr::Call(CallExpr(syntax)).into(),
            TUPLE_EXPR => Expr::Tuple(TupleExpr(syntax)).into(),
            STRUCT_EXPR => Expr::Struct(StructExpr(syntax)).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        match self {
            Expr::Path(path) => path.syntax(),
            Expr::Field(field) => field.syntax(),
            Expr::Literal(literal) => &literal.0,
            Expr::BinOpSeq(seq) => &seq.0,
            Expr::Postfix(postfix) => &postfix.0,
            Expr::Prefix(prefix) => &prefix.0,
            Expr::If(if_) => if_.syntax(),
            Expr::Closure(closure) => closure.syntax(),
            Expr::Call(call) => call.syntax(),
            Expr::Tuple(tuple_expr) => tuple_expr.syntax(),
            Expr::Struct(struct_expr) => struct_expr.syntax(),
        }
    }
}

/// Path expression node.
pub struct Path<'db>(SyntaxNode<'db>);

impl<'db> Node<'db> for Path<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            PATH_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for Path<'db> {}

/// Field access expression node (`base.field`).
pub struct FieldExpr<'db>(SyntaxNode<'db>);

impl<'db> FieldExpr<'db> {
    /// Returns the base expression.
    pub fn expr(&self) -> Option<Expr<'db>> {
        child(self.syntax())
    }
}

impl<'db> Node<'db> for FieldExpr<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            FIELD_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for FieldExpr<'db> {}

/// Literal expression node.
pub struct Literal<'db>(SyntaxNode<'db>);

impl<'db> Literal<'db> {
    /// Returns the literal kind derived from the first token.
    pub fn kind(&self) -> LiteralKind<'db> {
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
pub struct TupleExpr<'db>(SyntaxNode<'db>);

impl<'db> TupleExpr<'db> {
    /// Iterates tuple items.
    pub fn exprs(&self) -> impl Iterator<Item = Expr<'db>> + '_ {
        self.0.children().filter_map(Expr::cast)
    }
}

impl<'db> Node<'db> for TupleExpr<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            TUPLE_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// A flat sequence of interleaved expressions and binary operators.
pub struct BinOpSeq<'db>(SyntaxNode<'db>);

impl<'db> BinOpSeq<'db> {
    /// Iterates through elements (expressions and tokens) in the sequence.
    pub fn elements(&self) -> impl Iterator<Item = SyntaxElement<'db>> {
        self.0.children_with_tokens()
    }
}

impl<'db> Node<'db> for BinOpSeq<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            BIN_OP_SEQ => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Postfix expression node.
pub struct Postfix<'db>(SyntaxNode<'db>);

impl<'db> Postfix<'db> {
    /// Returns the inner expression.
    pub fn expr(&self) -> Option<Expr<'db>> {
        self.0.children().next().and_then(Expr::cast)
    }

    /// Returns the operator text, if any.
    pub fn op(&self) -> Option<&'db str> {
        first_non_trivia_token(&self.0).map(|syntax| syntax.text_trimmed())
    }
}

/// Prefix expression node.
pub struct Prefix<'db>(SyntaxNode<'db>);

impl<'db> Prefix<'db> {
    /// Returns the operator text, if any.
    pub fn op(&self) -> Option<&'db str> {
        first_non_trivia_token(&self.0).map(|syntax| syntax.text_trimmed())
    }

    /// Returns the inner expression.
    pub fn expr(&self) -> Option<Expr<'db>> {
        self.0.children().next().and_then(Expr::cast)
    }
}

/// If expression node.
pub struct IfExpr<'db>(SyntaxNode<'db>);

impl<'db> IfExpr<'db> {
    /// Returns the condition expression.
    pub fn condition(&self) -> Option<Expr<'db>> {
        child(self.syntax())
    }

    /// Returns the then branch block.
    pub fn then_branch(&self) -> Option<Block<'db>> {
        self.syntax().children().nth(1).and_then(Block::cast)
    }

    /// Returns the else branch block, if any.
    pub fn else_branch(&self) -> Option<Block<'db>> {
        self.syntax().children().nth(2).and_then(Block::cast)
    }
}

/// Closure expression node.
pub struct Closure<'db>(SyntaxNode<'db>);

impl<'db> Closure<'db> {
    /// Returns the parameter list.
    pub fn params(&self) -> Option<Params<'db>> {
        child(&self.0)
    }

    /// Returns the closure body.
    pub fn body(&self) -> Block<'db> {
        child(self.syntax()).unwrap()
    }
}

impl<'db> Node<'db> for Closure<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            CLOSURE_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Call expression node.
pub struct CallExpr<'db>(SyntaxNode<'db>);

impl<'db> CallExpr<'db> {
    /// Returns the callee expression.
    pub fn callee(&self) -> Option<Expr<'db>> {
        child(self.syntax())
    }

    /// Returns the argument list, if any.
    pub fn arg_list(&self) -> Option<ArgList<'db>> {
        child(self.syntax())
    }
}

impl<'db> Node<'db> for CallExpr<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            CALL_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Argument list node.
pub struct ArgList<'db> {
    syntax: SyntaxNode<'db>,
}

impl<'db> Node<'db> for ArgList<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            ARG_LIST => Some(Self { syntax }),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.syntax
    }
}

impl<'db> ArgList<'db> {
    /// Iterates argument expressions.
    pub fn args(&self) -> impl Iterator<Item = Expr<'db>> + '_ {
        self.syntax.children().filter_map(Expr::cast)
    }
}

impl<'db> Node<'db> for IfExpr<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            IF_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Concrete literal variants.
pub enum LiteralKind<'db> {
    Bool(bool),
    Int(SyntaxToken<'db>),
    Float(SyntaxToken<'db>),
    String(SyntaxToken<'db>),
    Char(SyntaxToken<'db>),
}

/// Identifier node.
pub struct Name<'db>(SyntaxNode<'db>);

impl<'db> Name<'db> {
    /// Returns the name token range.
    pub fn text_range(&self) -> TextRange {
        first_non_trivia_token(self.syntax())
            .map_or_else(|| self.syntax().text_range(), |token| token.trimmed_range())
    }

    /// Returns the identifier text.
    pub fn as_str(&self) -> &'db str {
        first_non_trivia_token(self.syntax()).map_or("", |token| token.text_trimmed())
    }
}

impl<'db> Node<'db> for Name<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            IDENT | NAME_REF => Self(syntax).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Type node.
pub enum Type<'db> {
    Path(PathType<'db>),
    Tuple(TupleType<'db>),
    Function(FunctionType<'db>),
    Union(UnionType<'db>),
    Inter(InterType<'db>),
    Record(RecordType<'db>),
}

impl<'db> Node<'db> for Type<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            PATH_TYPE => Type::Path(PathType(syntax)).into(),
            TUPLE_TYPE => Type::Tuple(TupleType(syntax)).into(),
            FUNCTION_TYPE => Type::Function(FunctionType(syntax)).into(),
            UNION_TYPE => Type::Union(UnionType(syntax)).into(),
            INTER_TYPE => Type::Inter(InterType(syntax)).into(),
            RECORD_TYPE => Type::Record(RecordType(syntax)).into(),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        match self {
            Type::Path(path_type) => &path_type.0,
            Type::Tuple(tuple_type) => &tuple_type.0,
            Type::Function(function_type) => &function_type.0,
            Type::Union(union_type) => &union_type.0,
            Type::Inter(inter_type) => &inter_type.0,
            Type::Record(record_type) => &record_type.0,
        }
    }
}

/// Path type node.
pub struct PathType<'db>(SyntaxNode<'db>);

impl<'db> Node<'db> for PathType<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            PATH_TYPE => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for PathType<'db> {}

/// Tuple type node.
pub struct TupleType<'db>(SyntaxNode<'db>);

impl<'db> TupleType<'db> {
    /// Iterates types in the tuple type.
    pub fn types(&self) -> impl Iterator<Item = Type<'db>> + '_ {
        self.0.children().filter_map(Type::cast)
    }
}

impl<'db> Node<'db> for TupleType<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            TUPLE_TYPE => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Function type node (`fun(T1, ...) -> Out`).
pub struct FunctionType<'db>(SyntaxNode<'db>);

impl<'db> FunctionType<'db> {
    pub fn inputs(&self) -> Option<TupleType<'db>> {
        child(&self.0)
    }

    pub fn output(&self) -> Option<Type<'db>> {
        self.0.children().nth(1).and_then(Type::cast)
    }
}

impl<'db> Node<'db> for FunctionType<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            FUNCTION_TYPE => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Union type node (`A | B`).
pub struct UnionType<'db>(SyntaxNode<'db>);

impl<'db> UnionType<'db> {
    pub fn lhs(&self) -> Option<Type<'db>> {
        child(&self.0)
    }

    pub fn rhs(&self) -> Option<Type<'db>> {
        self.0.children().nth(1).and_then(Type::cast)
    }
}

impl<'db> Node<'db> for UnionType<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            UNION_TYPE => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Intersection type node (`A & B`).
pub struct InterType<'db>(SyntaxNode<'db>);

impl<'db> InterType<'db> {
    pub fn lhs(&self) -> Option<Type<'db>> {
        child(&self.0)
    }

    pub fn rhs(&self) -> Option<Type<'db>> {
        self.0.children().nth(1).and_then(Type::cast)
    }
}

impl<'db> Node<'db> for InterType<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            INTER_TYPE => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Structural record type (`{ field: Ty, ... }`).
pub struct RecordType<'db>(SyntaxNode<'db>);

impl<'db> RecordType<'db> {
    pub fn fields(&self) -> impl Iterator<Item = StructField<'db>> + '_ {
        self.0.children().filter_map(StructField::cast)
    }
}

impl<'db> Node<'db> for RecordType<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            RECORD_TYPE => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Returns the first child node matching `N`.
fn child<'db, N: Node<'db>>(parent: &SyntaxNode<'db>) -> Option<N> {
    parent.children().find_map(N::cast)
}

/// Returns the first non-trivia token among the node's children.
fn first_non_trivia_token<'db>(node: &SyntaxNode<'db>) -> Option<SyntaxToken<'db>> {
    node.children_with_tokens().find_map(|child| {
        let token = child.into_token()?;
        if token.is_trivia() { None } else { Some(token) }
    })
}

/// Struct definition node.
pub struct StructDef<'db>(SyntaxNode<'db>);

impl<'db> StructDef<'db> {
    pub fn type_params(&self) -> impl Iterator<Item = TypeParam<'db>> + '_ {
        self.0.children().filter_map(TypeParam::cast)
    }

    pub fn field_list(&self) -> Option<StructFieldList<'db>> {
        child(&self.0)
    }
}

impl<'db> Node<'db> for StructDef<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            STRUCT_DEF => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for StructDef<'db> {}

/// Struct field list node.
pub struct StructFieldList<'db>(SyntaxNode<'db>);

impl<'db> StructFieldList<'db> {
    pub fn fields(&self) -> impl Iterator<Item = StructField<'db>> + '_ {
        self.0.children().filter_map(StructField::cast)
    }
}

impl<'db> Node<'db> for StructFieldList<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            STRUCT_FIELD_LIST => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Struct field node.
pub struct StructField<'db>(SyntaxNode<'db>);

impl<'db> StructField<'db> {
    pub fn ty(&self) -> Option<Type<'db>> {
        child(&self.0)
    }
}

impl<'db> Node<'db> for StructField<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            STRUCT_FIELD => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for StructField<'db> {}

/// Enum definition node.
pub struct EnumDef<'db>(SyntaxNode<'db>);

impl<'db> EnumDef<'db> {
    pub fn type_params(&self) -> impl Iterator<Item = TypeParam<'db>> + '_ {
        self.0.children().filter_map(TypeParam::cast)
    }

    pub fn variant_list(&self) -> Option<EnumVariantList<'db>> {
        child(&self.0)
    }
}

impl<'db> Node<'db> for EnumDef<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            ENUM_DEF => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for EnumDef<'db> {}

/// Enum variant list node.
pub struct EnumVariantList<'db>(SyntaxNode<'db>);

impl<'db> EnumVariantList<'db> {
    pub fn variants(&self) -> impl Iterator<Item = EnumVariant<'db>> + '_ {
        self.0.children().filter_map(EnumVariant::cast)
    }
}

impl<'db> Node<'db> for EnumVariantList<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            ENUM_VARIANT_LIST => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Enum variant node.
pub struct EnumVariant<'db>(SyntaxNode<'db>);

impl<'db> EnumVariant<'db> {
    pub fn field_types(&self) -> Option<TupleType<'db>> {
        child(&self.0)
    }
}

impl<'db> Node<'db> for EnumVariant<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            ENUM_VARIANT => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for EnumVariant<'db> {}

/// Struct expression node (e.g. `Point { x: 1, y: 2 }`).
pub struct StructExpr<'db>(SyntaxNode<'db>);

impl<'db> StructExpr<'db> {
    pub fn path(&self) -> Option<Path<'db>> {
        child(&self.0)
    }

    pub fn field_list(&self) -> Option<StructExprFieldList<'db>> {
        child(&self.0)
    }
}

impl<'db> Node<'db> for StructExpr<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            STRUCT_EXPR => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Struct expression field list node.
pub struct StructExprFieldList<'db>(SyntaxNode<'db>);

impl<'db> StructExprFieldList<'db> {
    pub fn fields(&self) -> impl Iterator<Item = StructExprField<'db>> + '_ {
        self.0.children().filter_map(StructExprField::cast)
    }
}

impl<'db> Node<'db> for StructExprFieldList<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            STRUCT_EXPR_FIELD_LIST => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

/// Struct expression field node.
pub struct StructExprField<'db>(SyntaxNode<'db>);

impl<'db> StructExprField<'db> {
    pub fn expr(&self) -> Option<Expr<'db>> {
        child(&self.0)
    }
}

impl<'db> Node<'db> for StructExprField<'db> {
    fn cast(syntax: SyntaxNode<'db>) -> Option<Self> {
        match syntax.kind() {
            STRUCT_EXPR_FIELD => Some(Self(syntax)),
            _ => None,
        }
    }

    fn syntax(&self) -> &SyntaxNode<'db> {
        &self.0
    }
}

impl<'db> HasName<'db> for StructExprField<'db> {}
