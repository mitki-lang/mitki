mod function;
pub use function::FunctionSourceMap;
use mitki_hir::hir::Function;
use mitki_span::IntoSymbol as _;
use mitki_yellow::ast::HasName as _;

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

#[salsa::tracked]
impl<'db> HasFunction<'db> for crate::item::scope::StructDestructorLocation<'db> {
    #[salsa::tracked]
    fn hir_function(self, db: &'db dyn salsa::Database) -> FunctionWithSourceMap<'db> {
        let parent = self.parent(db);
        let source = parent.source(db);
        let type_params =
            source.type_params().map(|tp| tp.as_str().into_symbol(db)).collect::<Vec<_>>();
        let owner_name = source.name().expect("struct destructor owner should have a name");
        function::FunctionBuilder::new(db).build_destructor(
            owner_name.as_str().into_symbol(db),
            type_params,
            &self.source(db),
        )
    }
}

#[salsa::tracked]
impl<'db> HasFunction<'db> for crate::item::scope::EnumDestructorLocation<'db> {
    #[salsa::tracked]
    fn hir_function(self, db: &'db dyn salsa::Database) -> FunctionWithSourceMap<'db> {
        let parent = self.parent(db);
        let source = parent.source(db);
        let type_params =
            source.type_params().map(|tp| tp.as_str().into_symbol(db)).collect::<Vec<_>>();
        let owner_name = source.name().expect("enum destructor owner should have a name");
        function::FunctionBuilder::new(db).build_destructor(
            owner_name.as_str().into_symbol(db),
            type_params,
            &self.source(db),
        )
    }
}
