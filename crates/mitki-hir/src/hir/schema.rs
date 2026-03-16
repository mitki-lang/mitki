use mitki_hir_macros::define_hir;

use super::NodeStore;

define_hir! {
    categories { Expr, Stmt, Ty, Pat }

    nodes {
        Name: BindingInLhs(sym: Symbol) => Expr;
        TypePath: BindingInLhs(sym: Symbol) => Ty;
        TypeApply: Direct2(path: Ty, args: Ty) => Ty;
        TypeArray: Direct2(item: Ty, unused: Ty) => Ty;
        TypeTuple: ListRange(items: Ty) => Ty;
        TypeFunction: Direct2(inputs: Ty, output: Ty) => Ty;
        TypeUnion: Direct2(lhs: Ty, rhs: Ty) => Ty;
        TypeInter: Direct2(lhs: Ty, rhs: Ty) => Ty;
        TypeRecord: ListRange(fields: Ty) => Ty;
        TypeField: Direct2(name: Name <- Symbol, ty: Ty) => Ty;
        TypePtrConst: Direct2(item: Ty, unused: Ty) => Ty;
        TypePtrMut: Direct2(item: Ty, unused: Ty) => Ty;

        True: ZeroZero() => Expr;
        False: ZeroZero() => Expr;
        Error: ZeroZero() => Expr;

        Int: BindingInLhs(sym: Option<Symbol>) => Expr;
        Float: BindingInLhs(sym: Option<Symbol>) => Expr;
        String: BindingInLhs(sym: Option<Symbol>) => Expr;
        Char: BindingInLhs(sym: Option<Symbol>) => Expr;

        Tuple: ListRange(items: Expr) => Expr;
        Array: ListRange(items: Expr) => Expr;
        ArrayRepeat: Direct2(value: Expr, len: Expr) => Expr;
        Call: CallRange(callee: Expr, args: Expr) => Expr;
        Field: Direct2(expr: Expr, name: Expr) => Expr;

        Binary: TripleLane(lhs: Expr, op: Expr, rhs: Expr) => Expr;
        If: TripleLane(cond: Expr, then_branch: Expr, else_branch: Expr) => Expr;
        Match: CallRange(scrutinee: Expr, arms: MatchArm) => Expr;
        LoopExpr: Direct2(body: Expr, unused: Expr) => Expr;
        UnsafeBlock: Direct2(body: Expr, unused: Expr) => Expr;
        BreakExpr: ZeroZero() => Expr;
        ContinueExpr: ZeroZero() => Expr;
        LocalVar: TripleLane(pattern: Pat, ty: Ty, initializer: Expr) => Stmt;
        AssignStmt: Direct2(target: Expr, value: Expr) => Stmt;
        ReturnStmt: Direct2(value: Expr, unused: Expr) => Stmt;

        Block: BlockWithTail(stmts: Stmt, tail: Expr) => Expr;
        Closure: BlockWithTail(params: Param, body: Expr) => Expr;

        MatchArm: Direct2(pattern: Pat, expr: Expr) => _;
        Param: Direct2(pattern: Pat, ty: Ty) => _;
        Prefix: Direct2(op: Expr, expr: Expr) => Expr;
        Postfix: Direct2(expr: Expr, op: Expr) => Expr;
        StructExpr: ListRange(items: Expr) => Expr;

        PatBinding: Direct2(name: Name <- Symbol, unused: Pat) => Pat;
        PatWildcard: ZeroZero() => Pat;
        PatTrue: ZeroZero() => Pat;
        PatFalse: ZeroZero() => Pat;
        PatInt: BindingInLhs(sym: Option<Symbol>) => Pat;
        PatFloat: BindingInLhs(sym: Option<Symbol>) => Pat;
        PatString: BindingInLhs(sym: Option<Symbol>) => Pat;
        PatChar: BindingInLhs(sym: Option<Symbol>) => Pat;
        PatTyped: Direct2(pattern: Pat, ty: Ty) => Pat;
        PatParen: Direct2(pattern: Pat, unused: Pat) => Pat;
        PatTuple: ListRange(items: Pat) => Pat;
        PatVariant: CallRange(path: Expr, args: Pat) => Pat;
        PatStructField: Direct2(name: Name <- Symbol, pat: Pat) => _;
        PatStruct: CallRange(path: Expr, fields: PatStructField) => Pat;
    }
}

impl From<ExprId> for StmtId {
    fn from(value: ExprId) -> StmtId {
        StmtId::from_raw(value.raw())
    }
}

impl StmtId {
    pub fn node_id(self) -> ExprId {
        ExprId::from_raw(self.raw())
    }
}

impl<'db> NodeStore<'db> {
    pub fn pattern_binding_names(&self, pattern: PatId) -> Vec<NameId> {
        if pattern == PatId::ZERO {
            return Vec::new();
        }

        let mut names = Vec::new();
        self.collect_pattern_binding_names(pattern, &mut names);
        names
    }

    fn collect_pattern_binding_names(&self, pattern: PatId, names: &mut Vec<NameId>) {
        if pattern == PatId::ZERO {
            return;
        }

        match self.node_kind(pattern) {
            NodeKind::PatBinding => {
                let (name, _) = self.pat_binding(self.as_pat_binding(pattern).expect("binding"));
                names.push(name);
            }
            NodeKind::PatTyped => {
                let (inner, _) = self.pat_typed(self.as_pat_typed(pattern).expect("typed"));
                if inner != PatId::ZERO {
                    self.collect_pattern_binding_names(inner, names);
                }
            }
            NodeKind::PatParen => {
                let (inner, _) = self.pat_paren(self.as_pat_paren(pattern).expect("paren"));
                if inner != PatId::ZERO {
                    self.collect_pattern_binding_names(inner, names);
                }
            }
            NodeKind::PatTuple => {
                for item in self.pat_tuple(self.as_pat_tuple(pattern).expect("tuple")).iter() {
                    if item != PatId::ZERO {
                        self.collect_pattern_binding_names(item, names);
                    }
                }
            }
            NodeKind::PatVariant => {
                let (_, items) = self.pat_variant(self.as_pat_variant(pattern).expect("variant"));
                for item in items.iter() {
                    if item != PatId::ZERO {
                        self.collect_pattern_binding_names(item, names);
                    }
                }
            }
            NodeKind::PatStruct => {
                let (_, fields) = self.pat_struct(self.as_pat_struct(pattern).expect("struct"));
                for field in fields.iter() {
                    let (name, pat) =
                        self.pat_struct_field(self.as_pat_struct_field(field).expect("field"));
                    if pat != PatId::ZERO {
                        self.collect_pattern_binding_names(pat, names);
                    } else {
                        names.push(name);
                    }
                }
            }
            NodeKind::PatWildcard
            | NodeKind::PatTrue
            | NodeKind::PatFalse
            | NodeKind::PatInt
            | NodeKind::PatFloat
            | NodeKind::PatString
            | NodeKind::PatChar => {}
            _ => {}
        }
    }
}
