use la_arena::{Arena, ArenaMap, Idx, IdxRange, RawIdx};
use mitki_span::Symbol;
use salsa::Database;

use crate::hir::{Binding, Block, Expr, ExprData, Function, HasBody, Stmt};
use crate::item::scope::FunctionLocation;

pub(crate) trait HasExprScopes<'db> {
    fn expr_scopes(self, db: &'db dyn Database) -> &'db ExprScopes<'db>;
}

#[salsa::tracked]
impl<'db> HasExprScopes<'db> for FunctionLocation<'db> {
    #[salsa::tracked(return_ref, no_eq)]
    fn expr_scopes(self, db: &'db dyn Database) -> ExprScopes<'db> {
        ExprScopesBuilder::new(self.body(db)).build()
    }
}

#[derive(Debug, Default)]
pub(crate) struct ExprScopes<'db> {
    pub scopes: Arena<ScopeData<'db>>,
    pub scope_entries: Arena<ScopeEntry<'db>>,
    pub scope_by_expr: ArenaMap<Expr<'db>, Scope<'db>>,
}

impl<'db> ExprScopes<'db> {
    pub(crate) fn chain(&self, scope: Option<Scope<'db>>) -> impl Iterator<Item = Scope<'db>> + '_ {
        std::iter::successors(scope, move |&scope| self.scopes[scope].parent)
    }

    #[allow(dead_code)]
    pub(crate) fn entries(&self, scope: Scope<'db>) -> &[ScopeEntry<'db>] {
        &self.scope_entries[self.scopes[scope].entries.clone()]
    }

    pub(crate) fn scope_for(&self, expr: Expr<'db>) -> Option<Scope<'db>> {
        self.scope_by_expr.get(expr).copied()
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct ScopeEntry<'db> {
    pub(crate) name: Symbol<'db>,
    pub(crate) binding: Binding<'db>,
}

pub(crate) type Scope<'db> = Idx<ScopeData<'db>>;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ScopeData<'db> {
    parent: Option<Scope<'db>>,
    entries: IdxRange<ScopeEntry<'db>>,
}

pub(crate) struct ExprScopesBuilder<'db> {
    function: &'db Function<'db>,
    scopes: ExprScopes<'db>,
}

fn empty_entries<'db>(idx: usize) -> IdxRange<ScopeEntry<'db>> {
    IdxRange::new(Idx::from_raw(RawIdx::from(idx as u32))..Idx::from_raw(RawIdx::from(idx as u32)))
}

impl<'db> ExprScopesBuilder<'db> {
    pub(crate) fn new(body: &'db Function<'db>) -> Self {
        Self { function: body, scopes: ExprScopes::default() }
    }

    fn root_scope(&mut self) -> Scope<'db> {
        self.scopes.scopes.alloc(ScopeData {
            parent: None,
            entries: empty_entries(self.scopes.scope_entries.len()),
        })
    }

    fn scope(&mut self, parent: Scope<'db>) -> Scope<'db> {
        self.scopes.scopes.alloc(ScopeData {
            parent: Some(parent),
            entries: empty_entries(self.scopes.scope_entries.len()),
        })
    }

    fn build_block(&mut self, block: &Block<'db>, scope: Scope<'db>) {
        let mut scope = self.scope(scope);

        for stmt in &block.stmts {
            match *stmt {
                Stmt::Val { name, ty: _, initializer } => {
                    self.build_expr_scopes(initializer, scope);
                    scope = self.scope(scope);

                    let symbol = self.function.bindings()[name];
                    let entry =
                        self.scopes.scope_entries.alloc(ScopeEntry { name: symbol, binding: name });
                    self.scopes.scopes[scope].entries =
                        IdxRange::new_inclusive(self.scopes.scopes[scope].entries.start()..=entry);
                }
                Stmt::Expr { expr, has_semi: _ } => {
                    self.build_expr_scopes(expr, scope);
                }
            }
        }

        if let Some(tail) = block.tail {
            self.build_expr_scopes(tail, scope);
        }
    }

    fn build_expr_scopes(&mut self, expr: Expr<'db>, scope: Scope<'db>) {
        self.scopes.scope_by_expr.insert(expr, scope);

        match &self.function.exprs()[expr] {
            ExprData::If { condition, then_branch, else_branch } => {
                self.build_expr_scopes(*condition, scope);
                self.build_block(then_branch, scope);
                if let Some(else_branch) = else_branch {
                    self.build_block(else_branch, scope);
                }
            }
            ExprData::Missing => {}
            _ => {}
        }
    }

    fn build(mut self) -> ExprScopes<'db> {
        let scope = self.root_scope();
        self.build_block(self.function.body(), scope);
        self.scopes
    }
}
