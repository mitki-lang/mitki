use mitki_inputs::File;
use mitki_lower::ast_map::HasAstMap as _;
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::{Declaration, FunctionLocation, HasItemScope as _};
use mitki_lower::item::tree::HasItemTree as _;
use mitki_resolve::Resolver;
use mitki_resolve::scope::HasExprScopes as _;
use mitki_yellow::ast::Node as _;
use mitki_yellow::{SyntaxNode, SyntaxNodePtr, ast};
use rustc_hash::FxHashMap;
use salsa::Database;

pub struct Semantics<'db> {
    source_map: SourceMap<'db>,
}

impl<'db> Semantics<'db> {
    pub fn new(db: &'db dyn Database, file: File) -> Self {
        let mut source_map = SourceMap { functions: FxHashMap::default() };
        let item_tree = file.item_tree(db);
        let item_scope = file.item_scope(db);
        let ast_map = file.ast_map(db);

        for &declaration in item_scope.declarations() {
            match declaration {
                Declaration::Function(func) => {
                    let id = item_tree[func.index(db)].id;
                    let &ptr = ast_map.find_node(id);
                    source_map.functions.insert(ptr, func);
                }
            }
        }

        Self { source_map }
    }

    pub fn function(&self, _db: &'db dyn Database, function: &SyntaxNode) -> FunctionLocation<'db> {
        self.source_map.functions[&SyntaxNodePtr::new(function)]
    }

    pub fn resolver(
        &self,
        db: &'db dyn Database,
        location: FunctionLocation<'db>,
        current_node: &SyntaxNode,
    ) -> Resolver<'db> {
        let source_map = location.hir_function(db).source_map(db);
        let scopes = location.expr_scopes(db);
        let scope = current_node
            .ancestors()
            .filter_map(|syntax| ast::Expr::cast(db, syntax))
            .find_map(|expr| source_map.syntax_expr(expr.syntax()))
            .and_then(|expr| scopes.scope_by_node(expr));

        Resolver::for_scope(db, location.file(db).item_scope(db), scopes, scope)
    }
}

struct SourceMap<'db> {
    functions: FxHashMap<SyntaxNodePtr, FunctionLocation<'db>>,
}
