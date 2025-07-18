use mitki_span::{IntoSymbol as _, Symbol};
use mitki_yellow::ast::{self, HasName as _, Node as _};
use mitki_yellow::{RedNode, RedNodePtr};
use rustc_hash::FxHashMap;
use salsa::Database;

use super::FunctionWithSourceMap;
use super::syntax::{
    BinaryExpr, IfExpr, LocalVar, NodeId, NodeKind, NodeStore, PostfixExpr, PrefixExpr,
};

#[derive(Default, Debug, PartialEq, Eq, salsa::Update)]
pub struct Function<'db> {
    node_store: NodeStore<'db>,

    params: Vec<NodeId>,
    body: NodeId,
    ret_type: NodeId,
}

#[derive(Default, PartialEq, Eq, salsa::Update)]
pub struct FunctionSourceMap {
    node_map: FxHashMap<RedNodePtr, NodeId>,
    node_map_back: FxHashMap<NodeId, RedNodePtr>,
}

impl FunctionSourceMap {
    pub(crate) fn syntax_expr(&self, db: &dyn Database, syntax: &RedNode) -> Option<NodeId> {
        self.node_map.get(&RedNodePtr::new(db, syntax)).copied()
    }

    #[track_caller]
    pub fn node_syntax(&self, node: &NodeId) -> RedNodePtr {
        self.node_map_back[node]
    }
}

impl<'db> Function<'db> {
    pub fn params(&self) -> &[NodeId] {
        &self.params
    }

    pub fn ret_type(&self) -> NodeId {
        self.ret_type
    }

    pub(crate) fn body(&self) -> NodeId {
        self.body
    }

    #[track_caller]
    pub fn node_kind(&self, node: NodeId) -> NodeKind {
        self.node_store.node_kind(node)
    }

    pub(crate) fn block_stmts(&self, block: NodeId) -> (&[NodeId], NodeId) {
        self.node_store.block_stmts(block)
    }

    #[track_caller]
    pub fn binding_symbol(&self, binding: NodeId) -> Symbol<'db> {
        self.node_store.symbol(binding)
    }

    #[track_caller]
    pub fn local_var(&self, node: NodeId) -> LocalVar {
        self.node_store.local_var(node)
    }

    #[track_caller]
    pub fn binary(&self, node: NodeId) -> BinaryExpr {
        self.node_store.binary(node)
    }

    #[track_caller]
    pub fn postfix(&self, node: NodeId) -> PostfixExpr {
        self.node_store.postfix(node)
    }

    #[track_caller]
    pub fn prefix(&self, node: NodeId) -> PrefixExpr {
        self.node_store.prefix(node)
    }

    pub fn tuple(&self, node: NodeId) -> &[NodeId] {
        self.node_store.tuple(node)
    }

    #[track_caller]
    pub fn if_expr(&self, node: NodeId) -> IfExpr {
        self.node_store.if_expr(node)
    }

    #[track_caller]
    pub fn closure_parts(&self, node: NodeId) -> (&[NodeId], NodeId) {
        self.node_store.closure_parts(node)
    }

    #[track_caller]
    pub(crate) fn name(&self, node: NodeId) -> Symbol<'db> {
        self.node_store.name(node)
    }

    #[track_caller]
    pub(crate) fn call(&self, node: NodeId) -> (NodeId, &[NodeId]) {
        self.node_store.call(node)
    }
}

pub(crate) struct FunctionBuilder<'db> {
    db: &'db dyn Database,
    function: Function<'db>,
    source_map: FunctionSourceMap,
}

impl<'db> std::ops::Deref for FunctionBuilder<'db> {
    type Target = Function<'db>;

    fn deref(&self) -> &Self::Target {
        &self.function
    }
}

impl std::ops::DerefMut for FunctionBuilder<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.function
    }
}

impl<'db> FunctionBuilder<'db> {
    pub(crate) fn new(db: &'db dyn Database) -> Self {
        Self { db, function: Function::default(), source_map: FunctionSourceMap::default() }
    }

    pub(super) fn build(mut self, node: &ast::Function<'db>) -> FunctionWithSourceMap<'db> {
        self.function.params = self.build_params(node.params(self.db));
        self.function.ret_type =
            self.build_ty(node.ret_type(self.db).and_then(|ret_type| ret_type.ty(self.db)));
        self.function.body = self.build_block(node.body(self.db));
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
                    .node_store
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

        let node = self.node_store.alloc_block(stmts, tail);
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

                let name = self.node_store.alloc_name(name);
                let node = self.node_store.alloc_local_var(name, ty, initializer);

                if let Some(a) = val.name(db) {
                    self.alloc_ptr(name, a.syntax());
                }

                self.alloc_ptr(node, stmt.syntax());

                node
            }
            ast::Stmt::Expr(stmt) => self.build_expr(stmt.expr(db)),
        }
    }

    fn alloc_ptr(&mut self, node: NodeId, syntax: &RedNode) {
        let ptr = RedNodePtr::new(self.db, syntax);
        self.source_map.node_map.insert(ptr, node);
        self.source_map.node_map_back.insert(node, ptr);
    }

    fn build_expr(&mut self, expr: Option<ast::Expr<'db>>) -> NodeId {
        let Some(expr) = expr else {
            return self.node_store.alloc_error();
        };

        let db = self.db;
        let node = match &expr {
            ast::Expr::Path(path) => {
                let path = path.name(db).unwrap().as_str(db).into_symbol(self.db);
                self.node_store.alloc_name(path)
            }
            ast::Expr::Literal(literal) => self.node_store.alloc_literal(db, literal),
            ast::Expr::Binary(binary) => {
                let lhs = self.build_expr(binary.lhs(db));
                let op_sym = binary.op(db).map(|op| op.into_symbol(self.db));
                let op = op_sym.map_or(NodeId::ZERO, |sym| self.node_store.alloc_binding(sym));
                let rhs = self.build_expr(binary.rhs(db));
                self.node_store.alloc_binary(lhs, op, rhs)
            }
            ast::Expr::Postfix(postfix) => {
                let expr = self.build_expr(postfix.expr(db));
                let op_sym = postfix.op(db).map(|op| op.into_symbol(self.db));
                let op = op_sym.map_or(NodeId::ZERO, |sym| self.node_store.alloc_binding(sym));
                self.node_store.alloc_postfix(expr, op)
            }
            ast::Expr::Prefix(prefix) => {
                let op_sym = prefix.op(db).map(|op| op.into_symbol(self.db));
                let op = op_sym.map_or(NodeId::ZERO, |sym| self.node_store.alloc_binding(sym));
                let expr = self.build_expr(prefix.expr(db));
                self.node_store.alloc_prefix(op, expr)
            }
            ast::Expr::If(if_expr) => {
                let cond = self.build_expr(if_expr.condition(db));
                let then_branch = self.build_block(if_expr.then_branch(db));
                let else_branch = self.build_block(if_expr.else_branch(db));
                self.node_store.alloc_if(cond, then_branch, else_branch)
            }
            ast::Expr::Closure(closure) => {
                let params = self.build_params(closure.params(db));
                let body = self.build_block(Some(closure.body(db)));
                self.node_store.alloc_closure(params, body)
            }
            ast::Expr::Call(call_expr) => {
                let callee = self.build_expr(call_expr.callee(db));
                let arg_list = call_expr.arg_list(db).unwrap();
                let args =
                    arg_list.args(db).map(|arg| self.build_expr(arg.into())).collect::<Vec<_>>();

                self.node_store.alloc_call(callee, args)
            }
            ast::Expr::Tuple(tuple_expr) => {
                let exprs = tuple_expr.exprs(db).map(|expr| self.build_expr(expr.into())).collect();
                self.node_store.alloc_tuple(exprs)
            }
        };

        self.alloc_ptr(node, expr.syntax());

        node
    }

    fn build_ty(&mut self, ty: Option<ast::Type<'_>>) -> NodeId {
        ty.map_or(NodeId::ZERO, |ty| match ty {
            ast::Type::Path(path) => {
                let path = path
                    .syntax()
                    .children_with_tokens(self.db)
                    .next()
                    .unwrap()
                    .into_token()
                    .unwrap()
                    .green()
                    .text_trimmed(self.db);
                let path = path.into_symbol(self.db);
                self.function.node_store.alloc_type_ref(path)
            }
        })
    }
}
