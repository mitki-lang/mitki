mod function;
mod syntax;

use std::sync::Arc;

pub(crate) use function::{Function, FunctionSourceMap};
pub(crate) use syntax::{NodeId, NodeKind};

pub trait HasFunction<'db> {
    fn hir_with_source_map(
        self,
        db: &'db dyn salsa::Database,
    ) -> &'db (Arc<Function<'db>>, FunctionSourceMap);
    fn hir(self, db: &'db dyn salsa::Database) -> Arc<Function<'db>>;
    fn hir_source_map(self, db: &'db dyn salsa::Database) -> &'db FunctionSourceMap;
}

#[salsa::tracked]
impl<'db> HasFunction<'db> for crate::item::scope::FunctionLocation<'db> {
    #[salsa::tracked(return_ref, no_eq)]
    fn hir_with_source_map(
        self,
        db: &'db dyn salsa::Database,
    ) -> (Arc<Function<'db>>, FunctionSourceMap) {
        function::FunctionBuilder::new(db).build(&self.source(db))
    }

    #[salsa::tracked]
    fn hir(self, db: &'db dyn salsa::Database) -> Arc<Function<'db>> {
        self.hir_with_source_map(db).0.clone()
    }

    fn hir_source_map(self, db: &'db dyn salsa::Database) -> &'db FunctionSourceMap {
        &self.hir_with_source_map(db).1
    }
}
