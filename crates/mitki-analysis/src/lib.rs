pub mod ast_map;
pub mod hir;
pub mod infer;
pub mod item;
pub mod resolver;
pub mod semantics;
pub mod ty;

pub use semantics::Semantics;

trait ToSymbol<'db>: mitki_yellow::ast::HasName<'db> {
    fn to_symbol(&self, db: &'db dyn salsa::Database) -> mitki_span::Symbol<'db> {
        mitki_span::Symbol::new(db, self.name(db).map_or("", |name| name.as_str(db)))
    }
}

impl<'db, T> ToSymbol<'db> for T where T: mitki_yellow::ast::HasName<'db> {}

#[salsa::tracked]
pub fn check_file(db: &dyn salsa::Database, file: mitki_inputs::File) {
    use infer::Inferable as _;
    use item::scope::HasItemScope as _;

    for declaration in file.item_scope(db).declarations() {
        declaration.infer(db);
    }
}
