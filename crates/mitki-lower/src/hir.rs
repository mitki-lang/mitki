mod function;
use std::future::Future;
use std::sync::Arc;

pub use function::FunctionSourceMap;
use mitki_hir::hir::Function;

#[derive(Debug, PartialEq, Eq, facet::Facet)]
pub struct FunctionWithSourceMap {
    #[facet(opaque)]
    function: Function,
    #[facet(opaque)]
    source_map: FunctionSourceMap,
}

impl FunctionWithSourceMap {
    pub fn new(function: Function, source_map: FunctionSourceMap) -> Self {
        Self { function, source_map }
    }

    pub fn function(&self) -> &Function {
        &self.function
    }

    pub fn source_map(&self) -> &FunctionSourceMap {
        &self.source_map
    }
}

pub trait HirFunctionDb: crate::item::scope::SignatureDb + HasHirFunctionQuery {}

impl<T> HirFunctionDb for T where T: crate::item::scope::SignatureDb + HasHirFunctionQuery {}

#[rustfmt::skip]
#[picante::tracked]
pub async fn hir_function<DB: mitki_hir::ty::TypeDatabase + mitki_parse::HasParseQuery>(
    db: &DB,
    location: crate::item::scope::FunctionLocation,
) -> picante::PicanteResult<Arc<FunctionWithSourceMap>> {
    use mitki_yellow::ast::{self, Node as _};

    let file = location.file(db);
    let parsed = mitki_parse::parse(db, file).await?;
    let syntax = location.source_ptr(db).to_node(&parsed.syntax_node());
    let function = ast::Function::cast(syntax).unwrap();
    let hir = function::FunctionBuilder::new(db).build(&function);
    Ok(Arc::new(hir))
}

pub trait HasFunction {
    fn hir_function<DB>(self, db: &DB) -> impl Future<Output = Arc<FunctionWithSourceMap>> + Send
    where
        DB: HirFunctionDb + Sync;
}

impl HasFunction for crate::item::scope::FunctionLocation {
    #[allow(clippy::manual_async_fn)]
    fn hir_function<DB>(self, db: &DB) -> impl Future<Output = Arc<FunctionWithSourceMap>> + Send
    where
        DB: HirFunctionDb + Sync,
    {
        async move { hir_function(db, self).await.expect("failed to compute hir function") }
    }
}
