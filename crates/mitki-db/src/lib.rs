use std::future::Future;
use std::sync::LazyLock;

pub use mitki_analysis::check_file;
pub use mitki_errors::{Diagnostic, Level};

#[picante::db(
    inputs(mitki_inputs::SourceFile),
    interned(mitki_span::SymbolData, mitki_hir::ty::TyData),
    tracked(mitki_parse::parse_file, mitki_lower::item::scope::item_scope_cached),
    db_trait(Database)
)]
pub struct RootDatabase {}

impl Default for RootDatabase {
    fn default() -> Self {
        Self::new()
    }
}

static EXECUTOR: LazyLock<tokio::runtime::Runtime> = LazyLock::new(|| {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(1)
        .enable_all()
        .build()
        .expect("failed to build parse runtime")
});

impl mitki_parse::ParseExecutor for RootDatabase {
    fn block_on<F>(&self, future: F) -> F::Output
    where
        F: Future,
    {
        EXECUTOR.block_on(future)
    }
}
