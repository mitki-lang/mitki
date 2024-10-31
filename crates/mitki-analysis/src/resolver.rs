mod scope;

use salsa::Database;
use scope::{ExprScopes, HasExprScopes, Scope};

use crate::hir::Expr;
use crate::item::scope::FunctionLocation;

#[derive(Debug, Clone)]
pub(crate) struct Resolver<'db> {
    expr_scopes: &'db ExprScopes<'db>,
    scopes: Vec<Scope<'db>>,
}

impl<'db> Resolver<'db> {
    pub(crate) fn new(db: &'db dyn Database, function: FunctionLocation<'db>) -> Self {
        Self { expr_scopes: function.expr_scopes(db), scopes: Vec::new() }
    }

    pub(crate) fn scopes(&self) -> impl ExactSizeIterator<Item = Scope<'db>> + '_ {
        self.scopes.iter().rev().copied()
    }

    pub(crate) fn scopes_for_expr(&mut self, expr: Expr<'db>) -> Guard {
        let start = self.scopes.len();

        let innermost_scope = self.scopes().next();
        let scope_for_expr = self.expr_scopes.scope_for(expr);

        if let Some(scope) = innermost_scope {
            let scopes = self.expr_scopes.chain(scope_for_expr).take_while(|&it| it != scope);
            self.scopes.extend(scopes);
        } else {
            let scopes = self.expr_scopes.chain(scope_for_expr);
            self.scopes.extend(scopes);
        }

        self.scopes[start..].reverse();

        Guard(start)
    }

    pub(crate) fn reset(&mut self, Guard(start): Guard) {
        self.scopes.truncate(start);
    }
}

pub(crate) struct Guard(usize);
