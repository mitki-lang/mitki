use mitki_span::Symbol;
use mitki_yellow::ast::{self, HasName as _, Node as _};
use mitki_yellow::{RedNode, RedNodePtr};
use rustc_hash::FxHashMap;
use salsa::Database;

use super::syntax::{Hir, LocalVar, NodeId, NodeKind};
use crate::ToSymbol;

#[derive(Default, Debug)]
pub struct Function<'db> {
    hir: Hir<'db>,

    params: Vec<NodeId>,
    body: NodeId,

    node_map: FxHashMap<RedNodePtr, NodeId>,
    node_map_back: FxHashMap<NodeId, RedNodePtr>,
}

impl<'db> Function<'db> {
    pub fn params(&self) -> &[NodeId] {
        &self.params
    }

    pub(crate) fn body(&self) -> NodeId {
        self.body
    }

    pub(crate) fn node_kind(&self, node: NodeId) -> NodeKind {
        self.hir.node_kind(node)
    }

    pub(crate) fn block_stmts(&self, block: NodeId) -> &[NodeId] {
        self.hir.block_stmts(block)
    }

    pub(crate) fn binding_symbol(&self, binding: NodeId) -> Symbol<'db> {
        self.hir.symbol(binding)
    }

    pub(crate) fn syntax_expr(&self, db: &dyn Database, syntax: &RedNode) -> Option<NodeId> {
        self.node_map.get(&RedNodePtr::new(db, syntax)).copied()
    }

    pub fn binding_syntax(&self, binding: &NodeId) -> RedNodePtr {
        self.node_map_back[binding]
    }

    pub(crate) fn local_var(&self, node: NodeId) -> LocalVar {
        self.hir.local_var(node)
    }
}

pub(crate) struct FunctionBuilder<'db> {
    db: &'db dyn Database,
    function: Function<'db>,
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
        Self { db, function: Function::default() }
    }

    pub(super) fn build(mut self, node: &ast::Function<'db>) -> Function<'db> {
        self.function.params = self.build_params(node.params(self.db));
        self.function.body = self.build_block(node.body(self.db));
        self.function
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
                    .hir
                    .alloc_binding(Symbol::new(self.db, param.name(self.db).as_str(self.db)));

                let ptr = RedNodePtr::new(self.db, param.name(self.db).syntax());
                self.node_map.insert(ptr, name);
                self.node_map_back.insert(name, ptr);

                name
            })
            .collect()
    }

    fn build_block(&mut self, block: Option<ast::Block<'db>>) -> NodeId {
        let Some(block) = block else {
            return NodeId::ZERO;
        };

        let stmts = block.stmts(self.db).map(|stmt| self.build_stmt(stmt)).collect();
        self.hir.alloc_block(stmts)
    }

    fn build_stmt(&mut self, stmt: ast::Stmt<'db>) -> NodeId {
        match stmt {
            ast::Stmt::Val(val) => {
                let name = val.to_symbol(self.db);

                let ty = val.ty(self.db).map_or(NodeId::ZERO, |ty| match ty {
                    ast::Type::Path(_) => NodeId::ZERO,
                });

                let initializer = self.build_expr(val.expr(self.db));
                let name = self.hir.alloc_binding(name);

                if let Some(ptr) = val.name(self.db) {
                    let ptr = RedNodePtr::new(self.db, ptr.syntax());
                    self.node_map.insert(ptr, name);
                    self.node_map_back.insert(name, ptr);
                }

                self.hir.alloc_local_var(name, ty, initializer)
            }
            ast::Stmt::Expr(stmt) => self.build_expr(stmt.expr(self.db)),
        }
    }

    fn build_expr(&mut self, node: Option<ast::Expr<'db>>) -> NodeId {
        let Some(node) = node else {
            return NodeId::ZERO;
        };

        let db = self.db;
        let expr = match &node {
            ast::Expr::Path(path) => self.hir.alloc_name(path.to_symbol(db)),
            ast::Expr::Literal(literal) => self.hir.alloc_literal(db, literal),
            ast::Expr::Binary(_binary) => todo!(),
            ast::Expr::Postfix(_postfix) => todo!(),
            ast::Expr::Prefix(_prefix) => todo!(),
            ast::Expr::If(_if_expr) => todo!(),
            ast::Expr::Closure(_closure) => {
                todo!()
            }
            ast::Expr::Call(call_expr) => {
                let callee = self.build_expr(call_expr.callee(db));
                let args = vec![];
                self.hir.alloc_call(callee, args)
            }
        };

        let ptr = RedNodePtr::new(db, node.syntax());

        self.node_map.insert(ptr, expr);
        self.node_map_back.insert(expr, ptr);

        expr
    }
}
