use std::sync::Arc;

use mitki_hir::hir::{ExprId, NameId, TyId};
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::item::scope::{FunctionLocation, HasItemScope as _, ItemScope, ItemScopeDb};
use mitki_span::{IntoSymbol as _, Symbol};
use rustc_hash::FxHashMap;

use crate::scope::{ExprScopes, HasExprScopes as _, Scope};

pub trait ResolverDb: ItemScopeDb + crate::scope::ExprScopesDb + HasBuiltinScopeQuery {}

impl<T> ResolverDb for T where T: ItemScopeDb + crate::scope::ExprScopesDb + HasBuiltinScopeQuery {}

type BuiltinScopeMap = FxHashMap<Symbol, Ty>;

#[picante::tracked]
pub async fn builtin_scope<DB: mitki_hir::ty::TypeDatabase>(
    db: &DB,
) -> picante::PicanteResult<Arc<BuiltinScopeMap>> {
    let scope = FxHashMap::from_iter([
        ("bool".into_symbol(db), Ty::new(db, TyKind::Bool)),
        ("char".into_symbol(db), Ty::new(db, TyKind::Char)),
        ("float".into_symbol(db), Ty::new(db, TyKind::Float)),
        ("int".into_symbol(db), Ty::new(db, TyKind::Int)),
        ("str".into_symbol(db), Ty::new(db, TyKind::String)),
    ]);
    Ok(Arc::new(scope))
}

pub async fn builtin_scope_for<DB>(db: &DB) -> Arc<FxHashMap<Symbol, Ty>>
where
    DB: ResolverDb,
{
    builtin_scope(db).await.expect("failed to compute builtin scope")
}

pub struct Resolver<'db, DB>
where
    DB: ResolverDb,
{
    db: &'db DB,
    item_scope: Arc<ItemScope>,
    expr_scopes: Arc<ExprScopes>,
    scopes: Vec<Scope>,
    builtin_scope: Arc<BuiltinScopeMap>,
}

impl<'db, DB> Resolver<'db, DB>
where
    DB: ResolverDb,
{
    pub async fn new(db: &'db DB, function: FunctionLocation) -> Self {
        let file = function.file(db);

        Self {
            db,
            item_scope: file.item_scope(db).await,
            expr_scopes: function.expr_scopes(db).await,
            scopes: Vec::new(),
            builtin_scope: builtin_scope(db).await.expect("failed to compute builtin scope"),
        }
    }

    fn scopes(&self) -> impl ExactSizeIterator<Item = Scope> + '_ {
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

    pub fn resolve_path(&self, path: Symbol) -> Option<Resolution> {
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

        if let Some(ty) = self.item_scope.get_type(&path) {
            return Resolution::Type(ty).into();
        }

        if let Some(&ty) = self.builtin_scope.get(&path) {
            return Resolution::Type(ty).into();
        }

        None
    }

    pub fn for_scope(
        db: &'db DB,
        item_scope: Arc<ItemScope>,
        expr_scopes: Arc<ExprScopes>,
        builtin_scope: Arc<BuiltinScopeMap>,
        scope: Option<Scope>,
    ) -> Self {
        let mut scopes: Vec<_> = expr_scopes.chain(scope).collect::<Vec<_>>().into_iter().collect();
        scopes.reverse();

        Resolver { db, item_scope, scopes, expr_scopes, builtin_scope }
    }

    pub fn resolve_enum_variant(&self, variant: Symbol) -> Option<Ty> {
        let mut resolved = None;

        for (_, &ty) in self.item_scope.types() {
            let TyKind::Enum { variants, .. } = ty.kind(self.db) else {
                continue;
            };

            if variants.iter().any(|(name, _)| *name == variant) {
                if resolved.is_some() {
                    return None;
                }
                resolved = Some(ty);
            }
        }

        resolved
    }
}

pub struct Guard(usize);

#[derive(Debug)]
pub enum Resolution {
    Local(NameId),
    Function(FunctionLocation),
    Type(Ty),
}
