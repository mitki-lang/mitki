use mitki_hir::hir::{ExprId, Function, ParamId, StmtId, TyId};
use mitki_span::IntoSymbol as _;
use mitki_yellow::ast::{self, HasName as _, Node as _};
use mitki_yellow::{SyntaxElement, SyntaxNode, SyntaxNodePtr};
use rustc_hash::FxHashMap;
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
    type_map: FxHashMap<SyntaxNodePtr, TyId>,
    type_map_back: FxHashMap<TyId, SyntaxNodePtr>,
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

        self.function.set_type_params(type_params);
        self.function.set_params(params);
        self.function.set_ret_type(ret_type);
        self.function.set_body(body);

        FunctionWithSourceMap::new(self.db, self.function, self.source_map)
    }

    fn build_params(&mut self, params: Option<ast::Params<'db>>) -> Vec<ParamId> {
        let Some(params) = params else {
            return Vec::new();
        };

        params
            .iter()
            .map(|param| {
                let name_sym = param.name().as_str().into_symbol(self.db);
                let ty = self.build_ty(param.ty());
                let param_id = self.function.node_store_mut().alloc_param(name_sym, ty);
                let (name_id, _) = self.function.node_store_mut().param(param_id);
                self.alloc_ptr(name_id.into(), param.name().syntax());
                param_id
            })
            .collect()
    }

    fn build_block(&mut self, block: Option<ast::Block<'db>>) -> ExprId {
        let Some(block) = block else {
            return ExprId::ZERO;
        };

        let stmts: Vec<StmtId> = block.stmts().map(|stmt| self.build_stmt(&stmt)).collect();
        let tail = block.tail_expr().map_or(ExprId::ZERO, |tail| self.build_expr(tail.into()));

        let node = self.function.node_store_mut().alloc_block(stmts, tail);
        let expr = node.into();
        self.alloc_ptr(expr, block.syntax());
        expr
    }

    fn build_stmt(&mut self, stmt: &ast::Stmt<'db>) -> StmtId {
        let db = self.db;
        match &stmt {
            ast::Stmt::Val(val) => {
                let name = val.name().map_or("", |name| name.as_str()).into_symbol(db);
                let ty = self.build_ty(val.ty());
                let initializer =
                    val.expr().map_or(ExprId::ZERO, |expr| self.build_expr(Some(expr)));

                let name = self.function.node_store_mut().alloc_name(name);
                let node = self.function.node_store_mut().alloc_local_var(name, ty, initializer);

                match val.name() {
                    Some(a) => self.alloc_ptr(name.into(), a.syntax()),
                    None => self.alloc_ptr(name.into(), val.syntax()),
                }

                node.into()
            }
            ast::Stmt::Expr(stmt) => self.build_expr(stmt.expr()).into(),
        }
    }

    fn alloc_ptr(&mut self, node: ExprId, syntax: &SyntaxNode) {
        let ptr = SyntaxNodePtr::new(syntax);
        self.source_map.node_map.insert(ptr, node);
        self.source_map.node_map_back.insert(node, ptr);
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
                let path = path.name().unwrap().as_str().into_symbol(self.db);
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
            ast::Expr::If(if_expr) => {
                let cond = self.build_expr(if_expr.condition());
                let then_branch = self.build_block(if_expr.then_branch());
                let else_branch = self.build_block(if_expr.else_branch());
                self.function.node_store_mut().alloc_if(cond, then_branch, else_branch).into()
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
                        let field_expr = self.build_expr(field.expr());
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
                    let path = path
                        .syntax()
                        .children_with_tokens()
                        .find_map(|child| {
                            let token = child.into_token()?;
                            if token.is_trivia() { None } else { Some(token) }
                        })
                        .expect("path should have at least one token")
                        .text_trimmed();
                    let path = path.into_symbol(self.db);
                    self.function.node_store_mut().alloc_type_ref(path).into()
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
            };
            self.alloc_type_ptr(ty_id, syntax);
            ty_id
        })
    }
}
