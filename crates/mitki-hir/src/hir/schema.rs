use mitki_hir_macros::define_hir;

define_hir! {
    categories { Expr, Stmt, Ty }

    nodes {
        Name: BindingInLhs(sym: Symbol) => Expr;
        TypePath: BindingInLhs(sym: Symbol) => Ty;
        TypeTuple: ListRange(items: Ty) => Ty;
        TypeFunction: Direct2(inputs: Ty, output: Ty) => Ty;
        TypeUnion: Direct2(lhs: Ty, rhs: Ty) => Ty;
        TypeInter: Direct2(lhs: Ty, rhs: Ty) => Ty;
        TypeRecord: ListRange(fields: Ty) => Ty;
        TypeField: Direct2(name: Name <- Symbol, ty: Ty) => Ty;

        True: ZeroZero() => Expr;
        False: ZeroZero() => Expr;
        Error: ZeroZero() => Expr;

        Int: BindingInLhs(sym: Option<Symbol>) => Expr;
        Float: BindingInLhs(sym: Option<Symbol>) => Expr;
        String: BindingInLhs(sym: Option<Symbol>) => Expr;
        Char: BindingInLhs(sym: Option<Symbol>) => Expr;

        Tuple: ListRange(items: Expr) => Expr;
        Call: CallRange(callee: Expr, args: Expr) => Expr;
        Field: Direct2(expr: Expr, name: Expr) => Expr;

        Binary: TripleLane(lhs: Expr, op: Expr, rhs: Expr) => Expr;
        If: TripleLane(cond: Expr, then_branch: Expr, else_branch: Expr) => Expr;
        LocalVar: TripleLane(name: Name, ty: Ty, initializer: Expr) => Stmt;

        Block: BlockWithTail(stmts: Stmt, tail: Expr) => Expr;
        Closure: BlockWithTail(params: Param, body: Expr) => Expr;

        Param: Direct2(name: Name <- Symbol, ty: Ty) => _;
        Prefix: Direct2(op: Expr, expr: Expr) => Expr;
        Postfix: Direct2(expr: Expr, op: Expr) => Expr;
        StructExpr: ListRange(items: Expr) => Expr;
    }
}

impl From<ExprId> for StmtId {
    fn from(value: ExprId) -> StmtId {
        StmtId::from_raw(value.raw())
    }
}
