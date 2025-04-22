use std::sync::Arc;

use mitki_span::{IntoSymbol as _, Symbol};
use mitki_yellow::ast::{self, HasName as _, Node as _};
use mitki_yellow::{RedNode, RedNodePtr};
use rustc_hash::FxHashMap;
use salsa::Database;

use super::syntax::{LocalVar, NodeId, NodeKind, NodeStore};

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
    pub(crate) fn name(&self, node: NodeId) -> Symbol<'db> {
        self.node_store.name(node)
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

    pub(super) fn build(
        mut self,
        node: &ast::Function<'db>,
    ) -> (Arc<Function<'db>>, FunctionSourceMap) {
        self.function.params = self.build_params(node.params(self.db));
        self.function.ret_type =
            self.build_ty(node.ret_type(self.db).and_then(|ret_type| ret_type.ty(self.db)));
        self.function.body = self.build_block(node.body(self.db));
        (self.function.into(), self.source_map)
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
            ast::Expr::Binary(_binary) => self.node_store.alloc_error(),
            ast::Expr::Postfix(_postfix) => self.node_store.alloc_error(),
            ast::Expr::Prefix(_prefix) => self.node_store.alloc_error(),
            ast::Expr::If(_if_expr) => self.node_store.alloc_error(),
            ast::Expr::Closure(_closure) => self.node_store.alloc_error(),
            ast::Expr::Call(call_expr) => {
                let callee = self.build_expr(call_expr.callee(db));
                let args = vec![];
                self.node_store.alloc_call(callee, args)
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
