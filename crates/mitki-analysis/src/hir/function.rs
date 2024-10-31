use la_arena::Arena;
use mitki_span::Symbol;
use mitki_yellow::RedNodePtr;
use mitki_yellow::ast::{self, HasName as _, Node as _};
use rustc_hash::FxHashMap;
use salsa::Database;

use super::syntax::{Binding, Block, Expr, ExprData, Stmt};

#[derive(Default, Debug)]
pub(crate) struct Function<'db> {
    body: Block<'db>,

    exprs: Arena<ExprData<'db>>,
    expr_map: FxHashMap<RedNodePtr, Expr<'db>>,
    expr_map_back: FxHashMap<Expr<'db>, RedNodePtr>,

    bindings: Arena<Symbol<'db>>,
    binding_map: FxHashMap<RedNodePtr, Binding<'db>>,
    binding_map_back: FxHashMap<Binding<'db>, RedNodePtr>,
}

impl<'db> Function<'db> {
    pub(crate) fn body(&self) -> &Block<'db> {
        &self.body
    }

    pub(crate) fn exprs(&self) -> &Arena<ExprData<'db>> {
        &self.exprs
    }

    pub(crate) fn bindings(&self) -> &Arena<Symbol<'db>> {
        &self.bindings
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

    pub(super) fn build(mut self, node: &ast::Function) -> Function<'db> {
        self.build_block(node.body(self.db));
        self.function
    }

    fn build_block(&mut self, block: Option<ast::Block>) -> Block<'db> {
        let Some(block) = block else {
            return Block::default();
        };

        let mut stmts: Vec<Stmt<'_>> =
            block.stmts(self.db).map(|stmt| self.build_stmt(stmt)).collect();

        let tail = match &stmts[..] {
            [.., Stmt::Expr { expr, has_semi: false }] => {
                let tail = Some(*expr);
                stmts.pop();
                tail
            }
            _ => None,
        };

        Block { stmts, tail }
    }

    fn build_stmt(&mut self, stmt: ast::Stmt<'_>) -> Stmt<'db> {
        match stmt {
            ast::Stmt::Val(val) => {
                let name = val.name(self.db).map_or("", |name| name.as_str(self.db));
                let name = Symbol::new(self.db, name);
                let initializer = self.build_expr(val.expr(self.db));

                let ptr = RedNodePtr::new(self.db, val.name(self.db).unwrap().syntax());

                let name = self.bindings.alloc(name);
                self.binding_map.insert(ptr, name);
                self.binding_map_back.insert(name, ptr);

                Stmt::Val { name, initializer }
            }
            ast::Stmt::Expr(expr) => Stmt::Expr {
                expr: self.build_expr(expr.expr(self.db)),
                has_semi: expr.semi(self.db).is_some(),
            },
        }
    }

    fn build_expr(&mut self, node: Option<ast::Expr>) -> Expr<'db> {
        let Some(node) = node else {
            return self.function.exprs.alloc(ExprData::Missing);
        };

        let expr = match &node {
            ast::Expr::Path(path) => {
                let name = path.name(self.db).map_or("", |name| name.as_str(self.db));
                ExprData::Path(Symbol::new(self.db, name))
            }
            ast::Expr::Literal(_literal) => ExprData::Missing,
            ast::Expr::Binary(_binary) => ExprData::Missing,
            ast::Expr::Postfix(_postfix) => ExprData::Missing,
            ast::Expr::Prefix(_prefix) => ExprData::Missing,
            ast::Expr::If(if_expr) => {
                let condition = self.build_expr(if_expr.condition(self.db));
                let then_branch = self.build_block(if_expr.then_branch(self.db));
                let else_branch = self.build_block(if_expr.else_branch(self.db));
                ExprData::If { condition, then_branch, else_branch }
            }
        };

        let expr = self.exprs.alloc(expr);
        let ptr = RedNodePtr::new(self.db, node.syntax());

        self.expr_map.insert(ptr, expr);
        self.expr_map_back.insert(expr, ptr);

        expr
    }
}
