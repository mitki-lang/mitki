use mitki_hir::hir::{ExprId, NameId, TyId};
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::item::scope::{FunctionLocation, HasItemScope as _, ItemScope};
use mitki_span::{IntoSymbol as _, Symbol};
use rustc_hash::FxHashMap;
use salsa::Database;

use crate::scope::{ExprScopes, HasExprScopes as _, Scope};

#[salsa::tracked(returns(ref))]
fn builtin_scope(db: &dyn Database) -> FxHashMap<Symbol<'_>, Ty<'_>> {
    FxHashMap::from_iter([
        ("bool".into_symbol(db), Ty::new(db, TyKind::Bool)),
        ("char".into_symbol(db), Ty::new(db, TyKind::Char)),
        ("float".into_symbol(db), Ty::new(db, TyKind::Float)),
        ("int".into_symbol(db), Ty::new(db, TyKind::Int)),
        ("str".into_symbol(db), Ty::new(db, TyKind::String)),
    ])
}

#[derive(Debug, Clone)]
pub struct Resolver<'db> {
    item_scope: &'db ItemScope<'db>,
    expr_scopes: &'db ExprScopes<'db>,
    scopes: Vec<Scope<'db>>,
    builtin_scope: &'db FxHashMap<Symbol<'db>, Ty<'db>>,
}

impl<'db> Resolver<'db> {
    pub fn new(db: &'db dyn Database, function: FunctionLocation<'db>) -> Self {
        let file = function.file(db);

        Self {
            item_scope: file.item_scope(db),
            expr_scopes: function.expr_scopes(db),
            scopes: Vec::new(),
            builtin_scope: builtin_scope(db),
        }
    }

    fn scopes(&self) -> impl ExactSizeIterator<Item = Scope<'db>> + '_ {
        self.scopes.iter().rev().copied()
    }

    pub fn scopes_for_node(&mut self, node: ExprId) -> Guard {
        let start = self.scopes.len();

        let innermost_scope = self.scopes().next();
        let scope_for_expr = self.expr_scopes.scope_for(node);

        let scopes = self.expr_scopes.chain(scope_for_expr);
        if let Some(scope) = innermost_scope {
            self.scopes.extend(scopes.take_while(|&it| it != scope));
        } else {
            self.scopes.extend(scopes);
        }

        self.scopes[start..].reverse();

        Guard(start)
    }

    pub fn scopes_for_type(&mut self, ty: TyId) -> Guard {
        let start = self.scopes.len();

        let innermost_scope = self.scopes().next();
        let scope_for_ty = self.expr_scopes.scope_for_ty(ty);

        let scopes = self.expr_scopes.chain(scope_for_ty);
        if let Some(scope) = innermost_scope {
            self.scopes.extend(scopes.take_while(|&it| it != scope));
        } else {
            self.scopes.extend(scopes);
        }

        self.scopes[start..].reverse();

        Guard(start)
    }

    pub fn reset(&mut self, Guard(start): Guard) {
        self.scopes.truncate(start);
    }

    pub fn resolve_path(&self, path: Symbol<'db>) -> Option<Resolution<'db>> {
        for scope in self.scopes() {
            if let Some(entry) =
                self.expr_scopes.entries(scope).iter().find(|entry| entry.name == path)
            {
                return Resolution::Local(entry.binding).into();
            }
        }

        if let Some(item) = self.item_scope.get(&path) {
            return Resolution::Function(item).into();
        }

        if let Some(&ty) = self.builtin_scope.get(&path) {
            return Resolution::Type(ty).into();
        }

        None
    }

    pub fn for_scope(
        db: &'db dyn Database,
        item_scope: &'db ItemScope<'db>,
        expr_scopes: &'db ExprScopes<'db>,
        scope: Option<Scope<'db>>,
    ) -> Self {
        let mut scopes: Vec<_> = expr_scopes.chain(scope).collect::<Vec<_>>().into_iter().collect();
        scopes.reverse();

        Resolver { item_scope, scopes, expr_scopes, builtin_scope: builtin_scope(db) }
    }
}

pub struct Guard(usize);

#[derive(Debug)]
pub enum Resolution<'db> {
    Local(NameId),
    Function(FunctionLocation<'db>),
    Type(Ty<'db>),
}
