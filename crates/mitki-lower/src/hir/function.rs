use mitki_hir::hir::{Function, NodeId, NodeKind};
use mitki_span::IntoSymbol as _;
use mitki_yellow::ast::{self, HasName as _, Node as _};
use mitki_yellow::{SyntaxNode, SyntaxNodePtr};
use rustc_hash::FxHashMap;
use salsa::Database;

use super::FunctionWithSourceMap;

#[derive(Default, PartialEq, Eq, salsa::Update)]
pub struct FunctionSourceMap {
    node_map: FxHashMap<SyntaxNodePtr, NodeId>,
    node_map_back: FxHashMap<NodeId, SyntaxNodePtr>,
}

impl FunctionSourceMap {
    pub fn syntax_expr(&self, syntax: &SyntaxNode) -> Option<NodeId> {
        self.node_map.get(&SyntaxNodePtr::new(syntax)).copied()
    }

    #[track_caller]
    pub fn node_syntax(&self, node: &NodeId) -> SyntaxNodePtr {
        self.node_map_back[node]
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

    fn build_params(&mut self, params: Option<ast::Params<'db>>) -> Vec<NodeId> {
        let Some(params) = params else {
            return Vec::new();
        };

        params
            .iter(self.db)
            .map(|param| {
                let name = self
                    .function
                    .node_store_mut()
                    .alloc_name(param.name(self.db).as_str(self.db).into_symbol(self.db));

                self.alloc_ptr(name, param.name(self.db).syntax());

                name
            })
            .collect()
    }

    fn build_block(&mut self, block: Option<ast::Block<'db>>) -> NodeId {
        let Some(block) = block else {
            return NodeId::ZERO;
        };

        let stmts = block.stmts(self.db).map(|stmt| self.build_stmt(&stmt)).collect();
        let tail =
            block.tail_expr(self.db).map_or(NodeId::ZERO, |tail| self.build_expr(tail.into()));

        let node = self.function.node_store_mut().alloc_block(stmts, tail);
        self.alloc_ptr(node, block.syntax());
        node
    }

    fn build_stmt(&mut self, stmt: &ast::Stmt<'db>) -> NodeId {
        let db = self.db;
        match &stmt {
            ast::Stmt::Val(val) => {
                let name = val.name(db).map_or("", |name| name.as_str(db)).into_symbol(db);
                let ty = self.build_ty(val.ty(db));
                let initializer = self.build_expr(val.expr(db));

                let name = self.function.node_store_mut().alloc_name(name);
                let node = self.function.node_store_mut().alloc_local_var(name, ty, initializer);

                if let Some(a) = val.name(db) {
                    self.alloc_ptr(name, a.syntax());
                }

                self.alloc_ptr(node, stmt.syntax());

                node
            }
            ast::Stmt::Expr(stmt) => self.build_expr(stmt.expr(db)),
        }
    }

    fn alloc_ptr(&mut self, node: NodeId, syntax: &SyntaxNode) {
        let ptr = SyntaxNodePtr::new(syntax);
        self.source_map.node_map.insert(ptr, node);
        self.source_map.node_map_back.insert(node, ptr);
    }

    fn build_expr(&mut self, expr: Option<ast::Expr<'db>>) -> NodeId {
        let Some(expr) = expr else {
            return self.function.node_store_mut().alloc_error();
        };

        let db = self.db;
        let node = match &expr {
            ast::Expr::Path(path) => {
                let path = path.name(db).unwrap().as_str(db).into_symbol(self.db);
                self.function.node_store_mut().alloc_name(path)
            }
            ast::Expr::Literal(literal) => self.build_literal(literal),
            ast::Expr::Binary(binary) => {
                let lhs = self.build_expr(binary.lhs(db));
                let op_sym = binary.op(db).map(|op| op.into_symbol(self.db));
                let op = op_sym
                    .map_or(NodeId::ZERO, |sym| self.function.node_store_mut().alloc_binding(sym));
                let rhs = self.build_expr(binary.rhs(db));
                self.function.node_store_mut().alloc_binary(lhs, op, rhs)
            }
            ast::Expr::Postfix(postfix) => {
                let expr = self.build_expr(postfix.expr(db));
                let op_sym = postfix.op(db).map(|op| op.into_symbol(self.db));
                let op = op_sym
                    .map_or(NodeId::ZERO, |sym| self.function.node_store_mut().alloc_binding(sym));
                self.function.node_store_mut().alloc_postfix(expr, op)
            }
            ast::Expr::Prefix(prefix) => {
                let op_sym = prefix.op(db).map(|op| op.into_symbol(self.db));
                let op = op_sym
                    .map_or(NodeId::ZERO, |sym| self.function.node_store_mut().alloc_binding(sym));
                let expr = self.build_expr(prefix.expr(db));
                self.function.node_store_mut().alloc_prefix(op, expr)
            }
            ast::Expr::If(if_expr) => {
                let cond = self.build_expr(if_expr.condition(db));
                let then_branch = self.build_block(if_expr.then_branch(db));
                let else_branch = self.build_block(if_expr.else_branch(db));
                self.function.node_store_mut().alloc_if(cond, then_branch, else_branch)
            }
            ast::Expr::Closure(closure) => {
                let params = self.build_params(closure.params(db));
                let body = self.build_block(Some(closure.body(db)));
                self.function.node_store_mut().alloc_closure(params, body)
            }
            ast::Expr::Call(call_expr) => {
                let callee = self.build_expr(call_expr.callee(db));
                let arg_list = call_expr.arg_list(db).unwrap();
                let args =
                    arg_list.args(db).map(|arg| self.build_expr(arg.into())).collect::<Vec<_>>();

                self.function.node_store_mut().alloc_call(callee, args)
            }
            ast::Expr::Tuple(tuple_expr) => {
                let exprs = tuple_expr.exprs(db).map(|expr| self.build_expr(expr.into())).collect();
                self.function.node_store_mut().alloc_tuple(exprs)
            }
        };

        self.alloc_ptr(node, expr.syntax());

        node
    }

    fn build_literal(&mut self, literal: &ast::Literal<'db>) -> NodeId {
        let db = self.db;
        let (kind, value) = match literal.kind(db) {
            ast::LiteralKind::Bool(true) => (NodeKind::True, None),
            ast::LiteralKind::Bool(false) => (NodeKind::False, None),
            ast::LiteralKind::Int(token) => {
                (NodeKind::Int, Some(token.text_trimmed().into_symbol(db)))
            }
            ast::LiteralKind::Float(token) => {
                (NodeKind::Float, Some(token.text_trimmed().into_symbol(db)))
            }
            ast::LiteralKind::String(token) => {
                (NodeKind::String, Some(token.text_trimmed().into_symbol(db)))
            }
            ast::LiteralKind::Char(token) => {
                (NodeKind::Char, Some(token.text_trimmed().into_symbol(db)))
            }
        };

        self.function.node_store_mut().alloc_literal(kind, value)
    }

    fn build_ty(&mut self, ty: Option<ast::Type<'_>>) -> NodeId {
        ty.map_or(NodeId::ZERO, |ty| match ty {
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
                self.function.node_store_mut().alloc_type_ref(path)
            }
        })
    }
}
