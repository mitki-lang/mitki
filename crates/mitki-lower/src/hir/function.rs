use mitki_hir::hir::{ExprId, Function, ParamId, StmtId, TyId};
use mitki_span::IntoSymbol as _;
use mitki_yellow::ast::{self, HasName as _, Node as _};
use mitki_yellow::{SyntaxNode, SyntaxNodePtr};
use rustc_hash::FxHashMap;
use salsa::Database;

use super::FunctionWithSourceMap;

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

    #[track_caller]
    pub fn node_syntax(&self, node: ExprId) -> SyntaxNodePtr {
        self.node_map_back[&node]
    }

    pub fn syntax_type(&self, syntax: &SyntaxNode) -> Option<TyId> {
        self.type_map.get(&SyntaxNodePtr::new(syntax)).copied()
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
        let params = self.build_params(node.params(self.db));
        let ret_type =
            self.build_ty(node.ret_type(self.db).and_then(|ret_type| ret_type.ty(self.db)));
        let body = self.build_block(node.body(self.db));

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
            .iter(self.db)
            .map(|param| {
                let name_sym = param.name(self.db).as_str(self.db).into_symbol(self.db);
                let ty = self.build_ty(param.ty(self.db));
                let param_id = self.function.node_store_mut().alloc_param(name_sym, ty);
                let (name_id, _) = self.function.node_store_mut().param(param_id);
                self.alloc_ptr(name_id.into(), param.name(self.db).syntax());
                param_id
            })
            .collect()
    }

    fn build_block(&mut self, block: Option<ast::Block<'db>>) -> ExprId {
        let Some(block) = block else {
            return ExprId::ZERO;
        };

        let stmts: Vec<StmtId> = block.stmts(self.db).map(|stmt| self.build_stmt(&stmt)).collect();
        let tail =
            block.tail_expr(self.db).map_or(ExprId::ZERO, |tail| self.build_expr(tail.into()));

        let node = self.function.node_store_mut().alloc_block(stmts, tail);
        let expr = node.into();
        self.alloc_ptr(expr, block.syntax());
        expr
    }

    fn build_stmt(&mut self, stmt: &ast::Stmt<'db>) -> StmtId {
        let db = self.db;
        match &stmt {
            ast::Stmt::Val(val) => {
                let name = val.name(db).map_or("", |name| name.as_str(db)).into_symbol(db);
                let ty = self.build_ty(val.ty(db));
                let initializer = self.build_expr(val.expr(db));

                let name = self.function.node_store_mut().alloc_name(name);
                let node = self.function.node_store_mut().alloc_local_var(name, ty, initializer);

                if let Some(a) = val.name(db) {
                    self.alloc_ptr(name.into(), a.syntax());
                }

                node.into()
            }
            ast::Stmt::Expr(stmt) => self.build_expr(stmt.expr(db)).into(),
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

        let db = self.db;
        let node: ExprId = match &expr {
            ast::Expr::Path(path) => {
                let path = path.name(db).unwrap().as_str(db).into_symbol(self.db);
                self.function.node_store_mut().alloc_name(path).into()
            }
            ast::Expr::Literal(literal) => self.build_literal(literal),
            ast::Expr::Binary(binary) => {
                let lhs = self.build_expr(binary.lhs(db));
                let op_sym = binary.op(db).map(|op| op.into_symbol(self.db));
                let op = op_sym.map_or(ExprId::ZERO, |sym| {
                    self.function.node_store_mut().alloc_name(sym).into()
                });
                let rhs = self.build_expr(binary.rhs(db));
                self.function.node_store_mut().alloc_binary(lhs, op, rhs).into()
            }
            ast::Expr::Postfix(postfix) => {
                let expr = self.build_expr(postfix.expr(db));
                let op_sym = postfix.op(db).map(|op| op.into_symbol(self.db));
                let op = op_sym.map_or(ExprId::ZERO, |sym| {
                    self.function.node_store_mut().alloc_name(sym).into()
                });
                self.function.node_store_mut().alloc_postfix(expr, op).into()
            }
            ast::Expr::Prefix(prefix) => {
                let op_sym = prefix.op(db).map(|op| op.into_symbol(self.db));
                let op = op_sym.map_or(ExprId::ZERO, |sym| {
                    self.function.node_store_mut().alloc_name(sym).into()
                });
                let expr = self.build_expr(prefix.expr(db));
                self.function.node_store_mut().alloc_prefix(op, expr).into()
            }
            ast::Expr::If(if_expr) => {
                let cond = self.build_expr(if_expr.condition(db));
                let then_branch = self.build_block(if_expr.then_branch(db));
                let else_branch = self.build_block(if_expr.else_branch(db));
                self.function.node_store_mut().alloc_if(cond, then_branch, else_branch).into()
            }
            ast::Expr::Closure(closure) => {
                let params = self.build_params(closure.params(db));
                let body = self.build_block(Some(closure.body(db)));
                self.function.node_store_mut().alloc_closure(params, body).into()
            }
            ast::Expr::Call(call_expr) => {
                let callee = self.build_expr(call_expr.callee(db));
                let arg_list = call_expr.arg_list(db).unwrap();
                let args =
                    arg_list.args(db).map(|arg| self.build_expr(arg.into())).collect::<Vec<_>>();

                self.function.node_store_mut().alloc_call(callee, args).into()
            }
            ast::Expr::Tuple(tuple_expr) => {
                let exprs = tuple_expr
                    .exprs(db)
                    .map(|expr| self.build_expr(expr.into()))
                    .collect::<Vec<_>>();
                self.function.node_store_mut().alloc_tuple(exprs).into()
            }
        };

        self.alloc_ptr(node, expr.syntax());

        node
    }

    fn build_literal(&mut self, literal: &ast::Literal<'db>) -> ExprId {
        let db = self.db;
        match literal.kind(db) {
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
            };
            self.alloc_type_ptr(ty_id, syntax);
            ty_id
        })
    }
}
