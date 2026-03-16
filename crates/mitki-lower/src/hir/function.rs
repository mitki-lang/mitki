use mitki_hir::hir::{ExprId, Function, NameId, ParamId, PatId, StmtId, TyId, WasmLinkage};
use mitki_span::{IntoSymbol as _, Symbol};
use mitki_yellow::ast::{self, HasName as _, Node as _};
use mitki_yellow::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxNodePtr};
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Database;

use super::FunctionWithSourceMap;

fn operator_precedence(op: &str) -> u8 {
    match op {
        "||" => 1,
        "&&" => 2,
        "==" | "!=" => 3,
        "<" | ">" | "<=" | ">=" => 4,
        "+" | "-" => 5,
        "*" | "/" | "%" => 6,
        _ => 0,
    }
}

#[derive(Default, PartialEq, Eq, salsa::Update)]
pub struct FunctionSourceMap {
    node_map: FxHashMap<SyntaxNodePtr, ExprId>,
    node_map_back: FxHashMap<ExprId, SyntaxNodePtr>,
    pat_map: FxHashMap<SyntaxNodePtr, PatId>,
    pat_map_back: FxHashMap<PatId, SyntaxNodePtr>,
    type_map: FxHashMap<SyntaxNodePtr, TyId>,
    type_map_back: FxHashMap<TyId, SyntaxNodePtr>,
    mutable_bindings: FxHashSet<NameId>,
}

impl FunctionSourceMap {
    pub fn syntax_expr(&self, syntax: &SyntaxNode) -> Option<ExprId> {
        self.node_map.get(&SyntaxNodePtr::new(syntax)).copied()
    }

    pub fn try_node_syntax(&self, node: ExprId) -> Option<SyntaxNodePtr> {
        self.node_map_back.get(&node).copied()
    }

    #[track_caller]
    pub fn node_syntax(&self, node: ExprId) -> SyntaxNodePtr {
        self.node_map_back[&node]
    }

    pub fn syntax_pat(&self, syntax: &SyntaxNode) -> Option<PatId> {
        self.pat_map.get(&SyntaxNodePtr::new(syntax)).copied()
    }

    pub fn try_pat_syntax(&self, pat: PatId) -> Option<SyntaxNodePtr> {
        self.pat_map_back.get(&pat).copied()
    }

    #[track_caller]
    pub fn pat_syntax(&self, pat: PatId) -> SyntaxNodePtr {
        self.pat_map_back[&pat]
    }

    pub fn syntax_type(&self, syntax: &SyntaxNode) -> Option<TyId> {
        self.type_map.get(&SyntaxNodePtr::new(syntax)).copied()
    }

    pub fn try_type_syntax(&self, ty: TyId) -> Option<SyntaxNodePtr> {
        self.type_map_back.get(&ty).copied()
    }

    #[track_caller]
    pub fn type_syntax(&self, ty: TyId) -> SyntaxNodePtr {
        self.type_map_back[&ty]
    }

    pub fn is_mutable_binding(&self, name: NameId) -> bool {
        self.mutable_bindings.contains(&name)
    }
}

pub(crate) struct FunctionBuilder<'db> {
    db: &'db dyn Database,
    function: Function<'db>,
    source_map: FunctionSourceMap,
}

impl<'db> FunctionBuilder<'db> {
    pub(crate) fn new(db: &'db dyn Database) -> Self {
        Self { db, function: Function::default(), source_map: FunctionSourceMap::default() }
    }

    pub(super) fn build(mut self, node: &ast::Function<'db>) -> FunctionWithSourceMap<'db> {
        let type_params: Vec<_> =
            node.type_params().map(|tp| tp.as_str().into_symbol(self.db)).collect();
        let params = self.build_params(node.params());
        let ret_type = self.build_ty(node.ret_type().and_then(|ret_type| ret_type.ty()));
        let body = self.build_block(node.body());
        let linkage = function_linkage(self.db, node);
        let comptime = node.is_comptime();
        let unsafe_ = node.is_unsafe();

        self.function.set_type_params(type_params);
        self.function.set_params(params);
        self.function.set_ret_type(ret_type);
        self.function.set_body(body);
        self.function.set_linkage(linkage);
        self.function.set_comptime(comptime);
        self.function.set_unsafe(unsafe_);

        FunctionWithSourceMap::new(self.db, self.function, self.source_map)
    }

    pub(super) fn build_destructor(
        mut self,
        owner_name: Symbol<'db>,
        type_params: Vec<Symbol<'db>>,
        node: &ast::DestructorDef<'db>,
    ) -> FunctionWithSourceMap<'db> {
        let self_ty = self.synthetic_owner_ty(owner_name, &type_params);
        let params = self.build_destructor_params(node.params(), self_ty);
        let ret_type = self.function.node_store_mut().alloc_type_tuple(Vec::new()).into();
        let body = self.build_block(node.body());

        self.function.set_type_params(type_params);
        self.function.set_params(params);
        self.function.set_ret_type(ret_type);
        self.function.set_body(body);
        self.function.set_linkage(WasmLinkage::Internal);
        self.function.set_comptime(false);
        self.function.set_unsafe(false);

        FunctionWithSourceMap::new(self.db, self.function, self.source_map)
    }

    fn build_params(&mut self, params: Option<ast::Params<'db>>) -> Vec<ParamId> {
        let Some(params) = params else {
            return Vec::new();
        };

        params
            .iter()
            .map(|param| {
                let pattern = self.build_pattern(param.pattern());
                let ty = self.build_ty(param.ty());
                if param.is_mutable() {
                    for name in self.function.node_store().pattern_binding_names(pattern) {
                        self.source_map.mutable_bindings.insert(name);
                    }
                }
                self.function.node_store_mut().alloc_param(pattern, ty)
            })
            .collect()
    }

    fn build_destructor_params(
        &mut self,
        params: Option<ast::Params<'db>>,
        self_ty: TyId,
    ) -> Vec<ParamId> {
        let Some(params) = params else {
            return Vec::new();
        };

        params
            .iter()
            .enumerate()
            .map(|(index, param)| {
                let pattern = self.build_pattern(param.pattern());
                let ty = if index == 0 { self_ty } else { self.build_ty(param.ty()) };
                if param.is_mutable() {
                    for name in self.function.node_store().pattern_binding_names(pattern) {
                        self.source_map.mutable_bindings.insert(name);
                    }
                }
                self.function.node_store_mut().alloc_param(pattern, ty)
            })
            .collect()
    }

    fn synthetic_owner_ty(&mut self, owner_name: Symbol<'db>, type_params: &[Symbol<'db>]) -> TyId {
        let path: TyId = self.function.node_store_mut().alloc_type_ref(owner_name).into();
        if type_params.is_empty() {
            return path;
        }

        let args = type_params
            .iter()
            .map(|&param| self.function.node_store_mut().alloc_type_ref(param).into())
            .collect::<Vec<_>>();
        let args = self.function.node_store_mut().alloc_type_tuple(args).into();
        self.function.node_store_mut().alloc_type_apply(path, args).into()
    }

    fn build_block(&mut self, block: Option<ast::Block<'db>>) -> ExprId {
        let Some(block) = block else {
            return ExprId::ZERO;
        };

        let mut stmts = Vec::new();
        let mut tail = ExprId::ZERO;
        let children = block.syntax().children().collect::<Vec<_>>();
        let last_index = children.len().saturating_sub(1);

        for (index, child) in children.into_iter().enumerate() {
            if let Some(stmt) = ast::Stmt::cast(child) {
                stmts.push(self.build_stmt(&stmt));
                continue;
            }
            let Some(expr) = ast::Expr::cast(child) else {
                continue;
            };
            if index == last_index {
                tail = self.build_expr(Some(expr));
            } else {
                stmts.push(self.build_expr(Some(expr)).into());
            }
        }

        let node = self.function.node_store_mut().alloc_block(stmts, tail);
        let expr = node.into();
        self.alloc_ptr(expr, block.syntax());
        expr
    }

    fn build_stmt(&mut self, stmt: &ast::Stmt<'db>) -> StmtId {
        match &stmt {
            ast::Stmt::Val(val) => {
                let pattern = self.build_pattern(val.pattern());
                let ty = self.build_ty(val.ty());
                let initializer =
                    val.expr().map_or(ExprId::ZERO, |expr| self.build_expr(Some(expr)));
                if val.is_mutable() {
                    for name in self.function.node_store().pattern_binding_names(pattern) {
                        self.source_map.mutable_bindings.insert(name);
                    }
                }
                self.function.node_store_mut().alloc_local_var(pattern, ty, initializer).into()
            }
            ast::Stmt::Assign(assign_stmt) => {
                let target = self.build_expr(assign_stmt.target());
                let value = self.build_expr(assign_stmt.expr());
                self.function.node_store_mut().alloc_assign_stmt(target, value).into()
            }
            ast::Stmt::Return(return_stmt) => {
                let value =
                    return_stmt.expr().map_or(ExprId::ZERO, |expr| self.build_expr(Some(expr)));
                self.function.node_store_mut().alloc_return_stmt(value, ExprId::ZERO).into()
            }
            ast::Stmt::Expr(stmt) => self.build_expr(stmt.expr()).into(),
        }
    }

    fn alloc_ptr(&mut self, node: ExprId, syntax: &SyntaxNode) {
        let ptr = SyntaxNodePtr::new(syntax);
        self.source_map.node_map.insert(ptr, node);
        self.source_map.node_map_back.insert(node, ptr);
    }

    fn alloc_pat_ptr(&mut self, pat: PatId, syntax: &SyntaxNode) {
        let ptr = SyntaxNodePtr::new(syntax);
        self.source_map.pat_map.insert(ptr, pat);
        self.source_map.pat_map_back.insert(pat, ptr);
    }

    fn alloc_type_ptr(&mut self, ty: TyId, syntax: &SyntaxNode) {
        let ptr = SyntaxNodePtr::new(syntax);
        self.source_map.type_map.insert(ptr, ty);
        self.source_map.type_map_back.insert(ty, ptr);
    }

    fn build_expr(&mut self, expr: Option<ast::Expr<'db>>) -> ExprId {
        let Some(expr) = expr else {
            return self.function.node_store_mut().alloc_error().into();
        };

        let node: ExprId = match &expr {
            ast::Expr::Path(path) => {
                let path = syntax_non_trivia_text(path.syntax()).into_symbol(self.db);
                self.function.node_store_mut().alloc_name(path).into()
            }
            ast::Expr::Field(field_expr) => {
                let expr =
                    field_expr.expr().map_or(ExprId::ZERO, |base| self.build_expr(Some(base)));
                let field_name = field_expr.name();
                let field_name_sym =
                    field_name.as_ref().map_or("", |name| name.as_str()).into_symbol(self.db);
                let field_name_id: ExprId =
                    self.function.node_store_mut().alloc_name(field_name_sym).into();

                match field_name {
                    Some(name) => self.alloc_ptr(field_name_id, name.syntax()),
                    None => self.alloc_ptr(field_name_id, field_expr.syntax()),
                }

                self.function.node_store_mut().alloc_field(expr, field_name_id).into()
            }
            ast::Expr::Literal(literal) => self.build_literal(literal),
            ast::Expr::Paren(paren) => self.build_expr(paren.expr()),
            ast::Expr::BinOpSeq(seq) => self.build_bin_op_seq(seq),
            ast::Expr::Postfix(postfix) => {
                let expr = self.build_expr(postfix.expr());
                let op_sym = postfix.op().map(|op| op.into_symbol(self.db));
                let op = op_sym.map_or(ExprId::ZERO, |sym| {
                    self.function.node_store_mut().alloc_name(sym).into()
                });
                self.function.node_store_mut().alloc_postfix(expr, op).into()
            }
            ast::Expr::Prefix(prefix) => {
                let op_sym = prefix.op().map(|op| op.into_symbol(self.db));
                let op = op_sym.map_or(ExprId::ZERO, |sym| {
                    self.function.node_store_mut().alloc_name(sym).into()
                });
                let expr = self.build_expr(prefix.expr());
                self.function.node_store_mut().alloc_prefix(op, expr).into()
            }
            ast::Expr::Loop(loop_expr) => {
                let body = self.build_block(loop_expr.body());
                self.function.node_store_mut().alloc_loop_expr(body, ExprId::ZERO).into()
            }
            ast::Expr::Break(_) => self.function.node_store_mut().alloc_break_expr().into(),
            ast::Expr::Continue(_) => self.function.node_store_mut().alloc_continue_expr().into(),
            ast::Expr::If(if_expr) => self.build_if_expr(if_expr),
            ast::Expr::Match(match_expr) => {
                let scrutinee = self.build_expr(match_expr.scrutinee());
                let arms = match_expr
                    .arms()
                    .map(|arm| {
                        let pattern = self.build_pattern(arm.pattern());
                        let expr = self.build_expr(arm.expr());
                        self.function.node_store_mut().alloc_match_arm(pattern, expr)
                    })
                    .collect::<Vec<_>>();
                self.function.node_store_mut().alloc_match(scrutinee, arms).into()
            }
            ast::Expr::Unsafe(unsafe_expr) => {
                let body = self.build_block(unsafe_expr.body());
                self.function.node_store_mut().alloc_unsafe_block(body, ExprId::ZERO).into()
            }
            ast::Expr::Closure(closure) => {
                let params = self.build_params(closure.params());
                let body = self.build_block(Some(closure.body()));
                self.function.node_store_mut().alloc_closure(params, body).into()
            }
            ast::Expr::Call(call_expr) => {
                let callee = self.build_expr(call_expr.callee());
                let arg_list = call_expr.arg_list().unwrap();
                let args =
                    arg_list.args().map(|arg| self.build_expr(arg.into())).collect::<Vec<_>>();

                self.function.node_store_mut().alloc_call(callee, args).into()
            }
            ast::Expr::Tuple(tuple_expr) => {
                let exprs =
                    tuple_expr.exprs().map(|expr| self.build_expr(expr.into())).collect::<Vec<_>>();
                self.function.node_store_mut().alloc_tuple(exprs).into()
            }
            ast::Expr::Array(array_expr) => {
                let exprs =
                    array_expr.exprs().map(|expr| self.build_expr(Some(expr))).collect::<Vec<_>>();
                let is_repeat = array_expr
                    .syntax()
                    .children_with_tokens()
                    .filter_map(SyntaxElement::into_token)
                    .any(|token| !token.is_trivia() && token.kind() == SyntaxKind::SEMICOLON);
                if is_repeat && exprs.len() == 2 {
                    self.function.node_store_mut().alloc_array_repeat(exprs[0], exprs[1]).into()
                } else {
                    self.function.node_store_mut().alloc_array(exprs).into()
                }
            }
            ast::Expr::Struct(struct_expr) => {
                let mut items: Vec<ExprId> = Vec::new();

                if let Some(path) = struct_expr.path() {
                    let name_sym = path.name().map_or("", |n| n.as_str()).into_symbol(self.db);
                    let name_id: ExprId =
                        self.function.node_store_mut().alloc_name(name_sym).into();
                    items.push(name_id);
                }

                if let Some(field_list) = struct_expr.field_list() {
                    for field in field_list.fields() {
                        let field_name_sym =
                            field.name().map_or("", |n| n.as_str()).into_symbol(self.db);
                        let field_name: ExprId =
                            self.function.node_store_mut().alloc_name(field_name_sym).into();
                        let field_expr = if let Some(expr) = field.expr() {
                            self.build_expr(Some(expr))
                        } else {
                            self.function.node_store_mut().alloc_name(field_name_sym).into()
                        };
                        items.push(field_name);
                        items.push(field_expr);
                    }
                }

                self.function.node_store_mut().alloc_struct_expr(items).into()
            }
        };

        self.alloc_ptr(node, expr.syntax());

        node
    }

    fn build_if_expr(&mut self, if_expr: &ast::IfExpr<'db>) -> ExprId {
        let cond = self.build_expr(if_expr.condition());
        let then_branch = self.build_block(if_expr.then_block());
        let else_branch = if let Some(else_if) = if_expr.else_if() {
            self.build_if_expr(&else_if)
        } else {
            self.build_block(if_expr.else_block())
        };
        self.function.node_store_mut().alloc_if(cond, then_branch, else_branch).into()
    }

    fn build_pattern(&mut self, pattern: Option<ast::Pattern<'db>>) -> PatId {
        let Some(pattern) = pattern else {
            return PatId::ZERO;
        };

        let pat = match &pattern {
            ast::Pattern::Binding(binding) => {
                let name_sym = binding.name().map_or("", |name| name.as_str()).into_symbol(self.db);
                let pat = self.function.node_store_mut().alloc_pat_binding(name_sym, PatId::ZERO);
                let (name, _) = self.function.node_store_mut().pat_binding(pat);
                if let Some(name_syntax) = binding.name() {
                    self.alloc_ptr(name.into(), name_syntax.syntax());
                }
                pat.into()
            }
            ast::Pattern::Wildcard(_) => self.function.node_store_mut().alloc_pat_wildcard().into(),
            ast::Pattern::Literal(literal) => self.build_literal_pattern(literal),
            ast::Pattern::Typed(typed) => {
                let inner = self.build_pattern(typed.pattern());
                let ty = self.build_ty(typed.ty());
                self.function.node_store_mut().alloc_pat_typed(inner, ty).into()
            }
            ast::Pattern::Paren(paren) => {
                let inner = self.build_pattern(paren.pattern());
                self.function.node_store_mut().alloc_pat_paren(inner, PatId::ZERO).into()
            }
            ast::Pattern::Tuple(tuple) => {
                let items: Vec<_> =
                    tuple.patterns().map(|item| self.build_pattern(Some(item))).collect();
                self.function.node_store_mut().alloc_pat_tuple(items).into()
            }
            ast::Pattern::Variant(variant) => {
                let path = variant
                    .path()
                    .as_ref()
                    .map_or(ExprId::ZERO, |path| self.build_field_pattern_path(path));
                let args = variant
                    .patterns()
                    .map(|item| self.build_pattern(Some(item)))
                    .collect::<Vec<_>>();
                self.function.node_store_mut().alloc_pat_variant(path, args).into()
            }
            ast::Pattern::Struct(struct_pattern) => {
                let path = struct_pattern
                    .path()
                    .as_ref()
                    .map_or(ExprId::ZERO, |path| self.build_path_pattern_path(path));
                let fields = struct_pattern
                    .fields()
                    .map(|field| {
                        let name_sym =
                            field.name().map_or("", |name| name.as_str()).into_symbol(self.db);
                        let pat = self.build_pattern(field.pattern());
                        let field_id =
                            self.function.node_store_mut().alloc_pat_struct_field(name_sym, pat);
                        let (name, _) = self.function.node_store_mut().pat_struct_field(field_id);
                        if let Some(name_syntax) = field.name() {
                            self.alloc_ptr(name.into(), name_syntax.syntax());
                        }
                        field_id
                    })
                    .collect::<Vec<_>>();
                self.function.node_store_mut().alloc_pat_struct(path, fields).into()
            }
        };

        self.alloc_pat_ptr(pat, pattern.syntax());
        pat
    }

    fn build_literal_pattern(&mut self, literal: &ast::LiteralPattern<'db>) -> PatId {
        match literal.kind() {
            ast::LiteralKind::Bool(true) => self.function.node_store_mut().alloc_pat_true().into(),
            ast::LiteralKind::Bool(false) => {
                self.function.node_store_mut().alloc_pat_false().into()
            }
            ast::LiteralKind::Int(token) => self
                .function
                .node_store_mut()
                .alloc_pat_int(Some(token.text_trimmed().into_symbol(self.db)))
                .into(),
            ast::LiteralKind::Float(token) => self
                .function
                .node_store_mut()
                .alloc_pat_float(Some(token.text_trimmed().into_symbol(self.db)))
                .into(),
            ast::LiteralKind::String(token) => self
                .function
                .node_store_mut()
                .alloc_pat_string(Some(token.text_trimmed().into_symbol(self.db)))
                .into(),
            ast::LiteralKind::Char(token) => self
                .function
                .node_store_mut()
                .alloc_pat_char(Some(token.text_trimmed().into_symbol(self.db)))
                .into(),
        }
    }

    fn build_path_pattern_path(&mut self, path: &ast::PathPattern<'db>) -> ExprId {
        let name_sym = path.name().map_or("", |name| name.as_str()).into_symbol(self.db);
        let expr: ExprId = self.function.node_store_mut().alloc_name(name_sym).into();
        if let Some(name) = path.name() {
            self.alloc_ptr(expr, name.syntax());
        }
        self.alloc_ptr(expr, path.syntax());
        expr
    }

    fn build_field_pattern_path(&mut self, path: &ast::FieldPattern<'db>) -> ExprId {
        let base =
            path.base().as_ref().map_or(ExprId::ZERO, |base| self.build_path_pattern_path(base));
        let field_name = path.name();
        let field_name_sym =
            field_name.as_ref().map_or("", |name| name.as_str()).into_symbol(self.db);
        let field_name_id: ExprId =
            self.function.node_store_mut().alloc_name(field_name_sym).into();

        match field_name {
            Some(name) => self.alloc_ptr(field_name_id, name.syntax()),
            None => self.alloc_ptr(field_name_id, path.syntax()),
        }

        let expr: ExprId = self.function.node_store_mut().alloc_field(base, field_name_id).into();
        self.alloc_ptr(expr, path.syntax());
        expr
    }

    fn build_bin_op_seq(&mut self, seq: &ast::BinOpSeq<'db>) -> ExprId {
        let mut operands: Vec<ExprId> = Vec::new();
        let mut operators: Vec<&'db str> = Vec::new();

        for element in seq.elements() {
            match element {
                SyntaxElement::Node(node) => {
                    let expr = ast::Expr::cast(node);
                    operands.push(self.build_expr(expr));
                }
                SyntaxElement::Token(token) => {
                    if !token.is_trivia() {
                        operators.push(token.text_trimmed());
                    }
                }
            }
        }

        self.pratt_parse(&operands, &operators, 0, operands.len())
    }

    fn pratt_parse(
        &mut self,
        operands: &[ExprId],
        operators: &[&'db str],
        start: usize,
        end: usize,
    ) -> ExprId {
        if end - start == 1 {
            return operands[start];
        }

        // Find the lowest-precedence operator (rightmost among ties for left-assoc).
        let mut min_prec = u8::MAX;
        let mut split = start;
        for (i, &op) in operators.iter().enumerate().take(end - 1).skip(start) {
            let prec = operator_precedence(op);
            if prec <= min_prec {
                min_prec = prec;
                split = i;
            }
        }

        let lhs = self.pratt_parse(operands, operators, start, split + 1);
        let op_sym = operators[split].into_symbol(self.db);
        let op: ExprId = self.function.node_store_mut().alloc_name(op_sym).into();
        let rhs = self.pratt_parse(operands, operators, split + 1, end);
        self.function.node_store_mut().alloc_binary(lhs, op, rhs).into()
    }

    fn build_literal(&mut self, literal: &ast::Literal<'db>) -> ExprId {
        let db = self.db;
        match literal.kind() {
            ast::LiteralKind::Bool(true) => self.function.node_store_mut().alloc_true().into(),
            ast::LiteralKind::Bool(false) => self.function.node_store_mut().alloc_false().into(),
            ast::LiteralKind::Int(token) => self
                .function
                .node_store_mut()
                .alloc_int(Some(token.text_trimmed().into_symbol(db)))
                .into(),
            ast::LiteralKind::Float(token) => self
                .function
                .node_store_mut()
                .alloc_float(Some(token.text_trimmed().into_symbol(db)))
                .into(),
            ast::LiteralKind::String(token) => self
                .function
                .node_store_mut()
                .alloc_string(Some(token.text_trimmed().into_symbol(db)))
                .into(),
            ast::LiteralKind::Char(token) => self
                .function
                .node_store_mut()
                .alloc_char(Some(token.text_trimmed().into_symbol(db)))
                .into(),
        }
    }

    fn build_ty(&mut self, ty: Option<ast::Type<'_>>) -> TyId {
        ty.map_or(TyId::ZERO, |ty| {
            let syntax = ty.syntax();
            let ty_id = match &ty {
                ast::Type::Path(path) => {
                    let path_ref: TyId = self
                        .function
                        .node_store_mut()
                        .alloc_type_ref(path.path_text().into_symbol(self.db))
                        .into();
                    let type_args = path
                        .type_args()
                        .into_iter()
                        .map(|ty| self.build_ty(Some(ty)))
                        .collect::<Vec<_>>();
                    if type_args.is_empty() {
                        path_ref
                    } else {
                        let args =
                            self.function.node_store_mut().alloc_type_tuple(type_args).into();
                        self.function.node_store_mut().alloc_type_apply(path_ref, args).into()
                    }
                }
                ast::Type::Array(array_type) => {
                    let item = self.build_ty(array_type.item());
                    self.function.node_store_mut().alloc_type_array(item, TyId::ZERO).into()
                }
                ast::Type::Tuple(tuple_type) => {
                    let items: Vec<TyId> =
                        tuple_type.types().map(|t| self.build_ty(Some(t))).collect();
                    self.function.node_store_mut().alloc_type_tuple(items).into()
                }
                ast::Type::Function(function_type) => {
                    let inputs = self.build_ty(function_type.inputs().map(ast::Type::Tuple));
                    let output = self.build_ty(function_type.output());
                    self.function.node_store_mut().alloc_type_function(inputs, output).into()
                }
                ast::Type::Union(union_type) => {
                    let lhs = self.build_ty(union_type.lhs());
                    let rhs = self.build_ty(union_type.rhs());
                    self.function.node_store_mut().alloc_type_union(lhs, rhs).into()
                }
                ast::Type::Inter(inter_type) => {
                    let lhs = self.build_ty(inter_type.lhs());
                    let rhs = self.build_ty(inter_type.rhs());
                    self.function.node_store_mut().alloc_type_inter(lhs, rhs).into()
                }
                ast::Type::Record(record_type) => {
                    let fields: Vec<TyId> = record_type
                        .fields()
                        .filter_map(|field| {
                            let name = field.name()?;
                            let ty = self.build_ty(field.ty());
                            let name = name.as_str().into_symbol(self.db);
                            Some(self.function.node_store_mut().alloc_type_field(name, ty).into())
                        })
                        .collect();
                    self.function.node_store_mut().alloc_type_record(fields).into()
                }
                ast::Type::Pointer(pointer_type) => {
                    let item = self.build_ty(pointer_type.pointee());
                    if pointer_type.is_mut() {
                        self.function.node_store_mut().alloc_type_ptr_mut(item, TyId::ZERO).into()
                    } else {
                        self.function.node_store_mut().alloc_type_ptr_const(item, TyId::ZERO).into()
                    }
                }
            };
            self.alloc_type_ptr(ty_id, syntax);
            ty_id
        })
    }
}

fn syntax_non_trivia_text(syntax: &SyntaxNode<'_>) -> String {
    let mut text = String::new();
    collect_non_trivia_text(syntax, &mut text);
    text
}

fn collect_non_trivia_text(syntax: &SyntaxNode<'_>, text: &mut String) {
    for child in syntax.children_with_tokens() {
        match child {
            SyntaxElement::Token(token) if !token.is_trivia() => {
                text.push_str(token.text_trimmed())
            }
            SyntaxElement::Node(node) => collect_non_trivia_text(&node, text),
            SyntaxElement::Token(_) => {}
        }
    }
}

fn function_linkage<'db>(db: &'db dyn Database, node: &ast::Function<'db>) -> WasmLinkage<'db> {
    if let Some(module) = node.import_module() {
        if node.is_unsafe() {
            return WasmLinkage::RawImport { module: module.into_symbol(db) };
        }
        return WasmLinkage::Import { module: module.into_symbol(db) };
    }

    if node.is_exported() {
        return WasmLinkage::Export;
    }

    if node.name().is_some_and(|name| name.as_str() == "main") {
        return WasmLinkage::ImplicitMainExport;
    }

    WasmLinkage::Internal
}
