mod function;
mod syntax;

pub(crate) use function::{Function, FunctionSourceMap};
pub(crate) use syntax::{NodeId, NodeKind, NodeStore};

#[salsa::tracked]
pub struct FunctionWithSourceMap<'db> {
    #[tracked]
    #[returns(ref)]
    pub function: Function<'db>,
    #[tracked]
    #[no_eq]
    #[returns(ref)]
    pub source_map: FunctionSourceMap,
}

pub trait HasFunction<'db> {
    fn hir_function(self, db: &'db dyn salsa::Database) -> FunctionWithSourceMap<'db>;
}

#[salsa::tracked]
impl<'db> HasFunction<'db> for crate::item::scope::FunctionLocation<'db> {
    #[salsa::tracked]
    fn hir_function(self, db: &'db dyn salsa::Database) -> FunctionWithSourceMap<'db> {
        function::FunctionBuilder::new(db).build(&self.source(db))
    }
}
