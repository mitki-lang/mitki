mod arena;
pub mod ast_map;
pub mod hir;
pub mod infer;
pub mod item;
pub mod resolver;
pub mod semantics;
pub mod ty;

use item::scope::Declaration;
pub use semantics::Semantics;

#[salsa::tracked]
pub fn check_file(db: &dyn salsa::Database, file: mitki_inputs::File) {
    use infer::Inferable as _;
    use item::scope::HasItemScope as _;

    for declaration in file.item_scope(db).declarations() {
        match declaration {
            Declaration::Function(func) => _ = func.infer(db),
        }
    }
}
