pub(crate) mod scope;

use mitki_span::Symbol;
use salsa::Database;
use scope::{ExprScopes, HasExprScopes as _, Scope};

use crate::hir::NodeId;
use crate::item::scope::{FunctionLocation, HasItemScope as _, ItemScope};

#[derive(Debug, Clone)]
pub struct Resolver<'db> {
    item_scope: &'db ItemScope<'db>,
    expr_scopes: &'db ExprScopes<'db>,
    scopes: Vec<Scope<'db>>,
}

impl<'db> Resolver<'db> {
    pub(crate) fn new(db: &'db dyn Database, function: FunctionLocation<'db>) -> Self {
        let file = function.file(db);

        Self {
            item_scope: file.item_scope(db),
            expr_scopes: function.expr_scopes(db),
            scopes: Vec::new(),
        }
    }

    pub(crate) fn scopes(&self) -> impl ExactSizeIterator<Item = Scope<'db>> + '_ {
        self.scopes.iter().rev().copied()
    }

    #[expect(dead_code)]
    pub(crate) fn scopes_for_expr(&mut self, expr: NodeId) -> Guard {
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

    #[expect(dead_code)]
    pub(crate) fn reset(&mut self, Guard(start): Guard) {
        self.scopes.truncate(start);
    }

    pub fn resolve_path(&self, path: Symbol<'db>) -> Option<PathResolution> {
        for scope in self.scopes() {
            if let Some(entry) =
                self.expr_scopes.entries(scope).iter().find(|entry| entry.name == path)
            {
                return PathResolution::Local(entry.binding).into();
            }
        }

        if let Some(item) = self.item_scope.get(&path) {
            return PathResolution::Function(item).into();
        }

        None
    }

    pub(crate) fn for_scope(
        item_scope: &'db ItemScope<'db>,
        expr_scopes: &'db ExprScopes<'db>,
        scope: Option<Scope<'db>>,
    ) -> Resolver<'db> {
        let mut scopes: Vec<_> = expr_scopes.chain(scope).collect::<Vec<_>>().into_iter().collect();
        scopes.reverse();

        Resolver { item_scope, scopes, expr_scopes }
    }
}

pub(crate) struct Guard(usize);

pub enum PathResolution<'db> {
    Local(NodeId),
    Function(FunctionLocation<'db>),
}
