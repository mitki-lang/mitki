use mitki_inputs::File;
use mitki_yellow::ast::Node as _;
use mitki_yellow::{RedNode, RedNodePtr, ast};
use rustc_hash::FxHashMap;
use salsa::Database;

use crate::ast_map::HasAstMap as _;
use crate::hir::HasFunction as _;
use crate::item::scope::{Declaration, FunctionLocation, HasItemScope as _};
use crate::item::tree::HasItemTree as _;
use crate::resolver::Resolver;
use crate::resolver::scope::HasExprScopes as _;

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

    pub fn function(&self, db: &'db dyn Database, function: &RedNode) -> FunctionLocation {
        self.source_map.functions[&RedNodePtr::new(db, function)]
    }

    pub fn resolver(
        &self,
        db: &'db dyn Database,
        location: FunctionLocation<'db>,
        current_node: &RedNode,
    ) -> Resolver<'db> {
        let source_map = location.hir_function(db).source_map(db);
        let scopes = location.expr_scopes(db);
        let scope = current_node
            .ancestors()
            .filter_map(|syntax| ast::Expr::cast(db, syntax))
            .find_map(|expr| source_map.syntax_expr(db, expr.syntax()))
            .and_then(|expr| scopes.scope_by_node.get(&expr))
            .copied();

        Resolver::for_scope(location.file(db).item_scope(db), scopes, scope)
    }
}

struct SourceMap<'db> {
    functions: FxHashMap<RedNodePtr, FunctionLocation<'db>>,
}
