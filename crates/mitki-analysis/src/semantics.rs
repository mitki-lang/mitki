use mitki_inputs::File;
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::{Declaration, FunctionLocationData, HasItemScope as _, ItemScopeDb};
use mitki_resolve::scope::HasExprScopes as _;
use mitki_resolve::{Resolver, ResolverDb};
use mitki_yellow::ast::Node as _;
use mitki_yellow::{SyntaxNode, SyntaxNodePtr, ast};
use rustc_hash::FxHashMap;

pub struct Semantics {
    source_map: SourceMap,
}

impl Semantics {
    pub async fn new<DB>(db: &DB, file: File) -> Self
    where
        DB: ItemScopeDb,
    {
        let mut source_map = SourceMap { functions: FxHashMap::default() };
        let item_scope = file.item_scope(db).await;

        for &declaration in item_scope.declarations() {
            match declaration {
                Declaration::Function(func) => {
                    let ptr = func.source_ptr(db);
                    source_map.functions.insert(ptr, func);
                }
                Declaration::Struct(_) | Declaration::Enum(_) => {}
            }
        }

        Self { source_map }
    }

    pub fn function(&self, function: &SyntaxNode) -> FunctionLocationData {
        self.source_map.functions[&SyntaxNodePtr::new(function)]
    }

    pub async fn resolver<'db, DB>(
        &self,
        db: &'db DB,
        location: FunctionLocationData,
        current_node: &SyntaxNode<'_>,
    ) -> Resolver<'db, DB>
    where
        DB: ResolverDb,
    {
        let hir = location.hir_function(db).await;
        let source_map = hir.source_map();
        let scopes = location.expr_scopes(db).await;
        let scope = current_node
            .ancestors()
            .filter_map(ast::Expr::cast)
            .find_map(|expr| source_map.syntax_expr(expr.syntax()))
            .and_then(|expr| scopes.scope_by_node(expr.into()));

        Resolver::for_scope(
            db,
            location.file(db).item_scope(db).await,
            scopes,
            mitki_resolve::builtin_scope_for(db).await,
            scope,
        )
    }
}

struct SourceMap {
    functions: FxHashMap<SyntaxNodePtr, FunctionLocationData>,
}
