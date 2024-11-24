mod function;
mod syntax;

pub(crate) use function::Function;
pub(crate) use syntax::{Binding, Block, Expr, ExprData, Stmt};

pub trait HasFunction<'db> {
    fn hir_function(self, db: &'db dyn salsa::Database) -> &'db Function<'db>;
}

#[salsa::tracked]
impl<'db> HasFunction<'db> for crate::item::scope::FunctionLocation<'db> {
    #[salsa::tracked(return_ref, no_eq)]
    fn hir_function(self, db: &'db dyn salsa::Database) -> Function<'db> {
        function::FunctionBuilder::new(db).build(self.source(db))
    }
}
