use la_arena::Arena;
use mitki_span::Symbol;
use mitki_yellow::ast::{self, HasName as _, Node as _};
use mitki_yellow::{RedNode, RedNodePtr, RedToken};
use rustc_hash::FxHashMap;
use salsa::Database;

use super::syntax::{Binding, Block, Expr, ExprData, Stmt, Ty};
use crate::ToSymbol;

#[derive(Default, Debug)]
pub struct Function<'db> {
    body: Block<'db>,

    exprs: Arena<ExprData<'db>>,
    expr_map: FxHashMap<RedNodePtr, Expr<'db>>,
    expr_map_back: FxHashMap<Expr<'db>, RedNodePtr>,

    bindings: Arena<Symbol<'db>>,
    binding_map: FxHashMap<RedNodePtr, Binding<'db>>,
    pub binding_map_back: FxHashMap<Binding<'db>, RedNodePtr>,
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

    pub(crate) fn syntax_expr(&self, db: &dyn Database, syntax: &RedNode) -> Option<Expr> {
        self.expr_map.get(&RedNodePtr::new(db, syntax)).copied()
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

    #[expect(clippy::needless_pass_by_value)]
    pub(super) fn build(mut self, node: ast::Function<'db>) -> Function<'db> {
        self.function.body = self.build_block(node.body(self.db));
        self.function
    }

    fn build_block(&mut self, block: Option<ast::Block<'db>>) -> Block<'db> {
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

    fn build_stmt(&mut self, stmt: ast::Stmt<'db>) -> Stmt<'db> {
        match stmt {
            ast::Stmt::Val(val) => {
                let name = val.to_symbol(self.db);

                let ty = val.ty(self.db).map(|ty| match ty {
                    ast::Type::Path(path_type) => Ty::Path(path_type.to_symbol(self.db)),
                });

                let initializer = self.build_expr(val.expr(self.db));
                let name = self.bindings.alloc(name);

                if let Some(ptr) = val.name(self.db) {
                    let ptr = RedNodePtr::new(self.db, ptr.syntax());
                    self.binding_map.insert(ptr, name);
                    self.binding_map_back.insert(name, ptr);
                }

                Stmt::Val { name, ty, initializer }
            }
            ast::Stmt::Expr(expr) => Stmt::Expr {
                expr: self.build_expr(expr.expr(self.db)),
                has_semi: expr.semi(self.db).is_some(),
            },
        }
    }

    fn build_expr(&mut self, node: Option<ast::Expr<'db>>) -> Expr<'db> {
        let Some(node) = node else {
            return self.function.exprs.alloc(ExprData::Missing);
        };

        let expr = match &node {
            ast::Expr::Path(path) => ExprData::Path(path.to_symbol(self.db)),
            ast::Expr::Literal(literal) => match literal.kind(self.db) {
                ast::LiteralKind::Bool(value) => ExprData::Bool(value),
                ast::LiteralKind::Int(token) => ExprData::Int(self.token_text(&token)),
                ast::LiteralKind::Float(token) => ExprData::Float(self.token_text(&token)),
            },
            ast::Expr::Binary(_binary) => ExprData::Missing,
            ast::Expr::Postfix(_postfix) => ExprData::Missing,
            ast::Expr::Prefix(_prefix) => ExprData::Missing,
            ast::Expr::If(if_expr) => ExprData::If {
                condition: self.build_expr(if_expr.condition(self.db)),
                then_branch: self.build_block(if_expr.then_branch(self.db)),
                else_branch: if_expr
                    .else_branch(self.db)
                    .map(|else_branch| self.build_block(else_branch.into())),
            },
            ast::Expr::Closure(closure) => {
                ExprData::Closure { body: self.build_block(closure.body(self.db).into()) }
            }
        };

        let expr = self.exprs.alloc(expr);
        let ptr = RedNodePtr::new(self.db, node.syntax());

        self.expr_map.insert(ptr, expr);
        self.expr_map_back.insert(expr, ptr);

        expr
    }

    fn token_text(&self, red_data: &RedToken<'db>) -> Symbol<'db> {
        Symbol::new(self.db, red_data.green().text_trimmed(self.db))
    }
}
